unit Async.Comments;

// ----------------------------------
// Application logic, business layer.
// Can be referenced by anyone.
// Cannot hold references to View.
// ----------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    System.Diagnostics,
    System.Win.ComObj,
    System.SyncObjs,
    System.Threading,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.ComCtrls,
    Vcl.Dialogs,
    Data.Win.ADODB,
    Data.DB,
    Handler.Sql,
    Unity.Interposer,
    Unity.Statics,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;

type


    IComments = interface(IInterface)
    ['{1B3127BB-EC78-4177-A286-C138E02709D3}']
        procedure EditDailyComment(Fields: TDailyCommentFields);
        procedure EditGeneralComment(Fields: TGeneralCommentFields);
    end;


    TComments = class(TInterfacedObject, IComments)
    {$TYPEINFO ON}
    private
        procedure FInsertDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields);
        procedure FUpdateDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; Condition: string);
        procedure FInsertGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields);
        procedure FUpdateGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields; Condition: string);
    public
        procedure EditDailyComment(Fields: TDailyCommentFields);
        procedure EditGeneralComment(Fields: TGeneralCommentFields);
    end;


implementation


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Account,
    Handler.Database,
    Unity.Settings,
    Sync.Documents,
    DbModel,
    AgeView,
    Transactions;


// ------------------------------------
// Perform SQL "insert into" command
// ------------------------------------

procedure TComments.FInsertDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields);
begin

    if Fields.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add('0');
    end;

    if Fields.EmailReminder then
    begin
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add('0');
    end;

    if Fields.EmailAutoStat then
    begin
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add('0');
    end;

    if Fields.EmailManuStat then
    begin
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add('0');
    end;

    if Fields.CallEvent then
    begin
        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add('1');
        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(Fields.CallDuration.ToString());
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add('0');
        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add('0');
    end;

    DailyText.Columns.Add(TDailyComment.FixedComment);
    DailyText.Values.Add(Fields.Comment);

    if (DailyText.InsertInto(TDailyComment.DailyComment, True)) and (DailyText.RowsAffected > 0) then
    begin

        if Fields.UpdateGrid then TThread.Synchronize(nil, procedure
        begin
            ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
        end);

        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"DailyComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot post daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
// Perform SQL "update" command
// ------------------------------------

procedure TComments.FUpdateDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; Condition: string);
begin

    if Fields.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add(IntToStr(StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.Email].Value), 0)));
    end;

    // Call event and call duration always comes together
    if Fields.CallEvent then
    begin

        var LCallEvent: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.CallEvent].Value), 0);
        Inc(LCallEvent);

        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add(LCallEvent.ToString());

        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(Fields.CallDuration.ToString());

    end;

    if Fields.EmailReminder then
    begin
        var LEmailReminder: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailReminder].Value), 0);
        Inc(LEmailReminder);
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add(LEmailReminder.ToString());
    end;

    if Fields.EmailAutoStat then
    begin
        var LEmailAutoStat: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailAutoStat].Value), 0);
        Inc(LEmailAutoStat);
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add(LEmailAutoStat.ToString());
    end;

    if Fields.EmailManuStat then
    begin
        var LEmailManuStat: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailManuStat].Value), 0);
        Inc(LEmailManuStat);
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add(LEmailManuStat.ToString());
    end;

    if not(Fields.Comment = '') then
    begin
        DailyText.Columns.Add(TDailyComment.FixedComment);
        DailyText.Values.Add(Fields.Comment);
    end;

    if (DailyText.UpdateRecord(TDailyComment.DailyComment, True, Condition)) and (DailyText.RowsAffected > 0) then
    begin

        if Fields.UpdateGrid then TThread.Synchronize(nil, procedure
        begin
            ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
        end);

        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"DailyComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.EditDailyComment(Fields: TDailyCommentFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DailyText: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            var Condition:    string:=TDailyComment.Cuid + TSql.EQUAL + QuotedStr(Fields.CUID) + TSql._AND + TDailyComment.AgeDate + TSql.EQUAL + QuotedStr(MainForm.AgeDateSel);
            var DataCheckSum: string:=Fields.CUID + StringReplace(MainForm.AgeDateSel, '-', '', [rfReplaceAll]);

            DailyText.CustFilter:=TSql.WHERE + Condition;
            DailyText.OpenTable(TDailyComment.DailyComment);

            if not (DailyText.DataSet.RecordCount = 0) then
            begin

                // ------------------------
                // Update exisiting comment
                // ------------------------

                // Allow to extend comment by adding to existing wording a new comment line
                if Fields.ExtendComment then
                    Fields.Comment:=DailyText.DataSet.Fields[TDailyComment.FixedComment].Value + TChars.CRLF + Fields.Comment;

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(MainForm.WinUserName));

                FUpdateDailyComment(DailyText, Fields, Condition);

            end
            else
            begin

                // -----------------
                // Insert new record
                // -----------------

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.GroupId);
                DailyText.Values.Add(MainForm.GroupIdSel);

                DailyText.Columns.Add(TDailyComment.Cuid);
                DailyText.Values.Add(Fields.CUID);

                DailyText.Columns.Add(TDailyComment.AgeDate);
                DailyText.Values.Add(MainForm.AgeDateSel);

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(MainForm.WinUserName));

                DailyText.Columns.Add(TDailyComment.DataCheckSum);
                DailyText.Values.Add(DataCheckSum);

                FInsertDailyComment(DailyText, Fields);

            end;

        finally
            DailyText.Free;
        end;

    end);

    NewTask.Start();

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.FInsertGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields);
begin

    if not(Fields.FixedComment = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add(Fields.FixedComment);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add('');
    end;

    if not(Fields.FollowUp = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add(Fields.FollowUp);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add('');
    end;

    if not(Fields.Free1 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add(Fields.Free1);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add('');
    end;

    if not(Fields.Free2 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add(Fields.Free2);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add('');
    end;

    if not(Fields.Free3 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add(Fields.Free3);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add('');
    end;

    if (GenText.InsertInto(TGeneralComment.GeneralComment, True)) and (GenText.RowsAffected > 0) then
    begin
        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"GeneralComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update general comment into database.' +  TChars.CRLF + 'Error message received: ' + GenText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.FUpdateGeneralComment(var GenText: TDataTables ;var Fields: TGeneralCommentFields; Condition: string);
begin

    if not(Fields.FixedComment = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add(Fields.FixedComment);
    end;

    if not(Fields.FollowUp = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add(Fields.FollowUp);
    end;

    if not(Fields.Free1 = TUnknown.Null) then
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add(Fields.Free1);
    end;

    if not(Fields.Free2 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add(Fields.Free2);
    end;

    if not(Fields.Free3 = TUnknown.Null) then
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add(Fields.Free3);
    end;

    if (GenText.UpdateRecord(TGeneralComment.GeneralComment, True, Condition)) and (GenText.RowsAffected > 0) then
    begin
        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"GeneralComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update general comment into database.' +  TChars.CRLF + 'Error message received: ' + GenText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.EditGeneralComment(Fields: TGeneralCommentFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var GenText: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            var Condition: string:=TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(Fields.CUID);
            GenText.CustFilter:=TSql.WHERE + Condition;
            GenText.OpenTable(TGeneralComment.GeneralComment);

            if not (GenText.DataSet.RecordCount = 0) then
            begin

                // -----------------
                // Update record
                // -----------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(MainForm.WinUserName));

                FUpdateGeneralComment(GenText, Fields, Condition);

            end
            else
            begin

                // -----------------
                // Insert new record
                // -----------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Cuid);
                GenText.Values.Add(Fields.CUID);

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(MainForm.WinUserName));

                FInsertGeneralComment(GenText, Fields);

            end;

        finally
            GenText.Free;
        end;

    end);

    NewTask.Start();

end;


end.

