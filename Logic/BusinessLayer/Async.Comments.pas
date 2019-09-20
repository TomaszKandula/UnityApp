unit Async.Comments;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

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
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;

type


    // --------------------
    // Callback signatures.
    // --------------------

    TEditDailyComment   = procedure(LastError: TLastError) of object;
    TEditGeneralComment = procedure(LastError: TLastError) of object;


    IComments = interface(IInterface)
    ['{1B3127BB-EC78-4177-A286-C138E02709D3}']
        procedure EditDailyComment(Fields: TDailyCommentFields; Callback: TEditDailyComment);
        procedure EditGeneralComment(Fields: TGeneralCommentFields; Callback: TEditGeneralComment);
    end;


    TComments = class(TInterfacedObject, IComments)
    {$TYPEINFO ON}
    private
        procedure FInsertDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; var LastError: TLastError);
        procedure FUpdateDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; Condition: string; var LastError: TLastError);
        procedure FInsertGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields; var LastError: TLastError);
        procedure FUpdateGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields; Condition: string; var LastError: TLastError);
    public
        procedure EditDailyComment(Fields: TDailyCommentFields; Callback: TEditDailyComment);
        procedure EditGeneralComment(Fields: TGeneralCommentFields; Callback: TEditGeneralComment);
    end;


implementation


uses
    Handler.Account,
    Handler.Database,
    Unity.Sql,
    Unity.Chars,
    Unity.Helpers,
    Unity.Unknown,
    Unity.Settings,
    Unity.Messaging,
    Unity.EventLogger,
    Unity.SessionService,
    Sync.Documents,
    DbModel,
    Transactions;


// ------------------------------------
// Perform SQL "insert into" command
// ------------------------------------

procedure TComments.FInsertDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; var LastError: TLastError);
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
        LastError.IsSucceeded:=True;
        LastError.ErrorMessage:='"DailyComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.';
        if Fields.EventLog then ThreadFileLog.Log(LastError.ErrorMessage);
    end
    else
    begin
        LastError.IsSucceeded:=False;
        LastError.ErrorMessage:='Cannot post daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.';
        ThreadFileLog.Log('Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
    end;

end;


// ------------------------------------
// Perform SQL "update" command
// ------------------------------------

procedure TComments.FUpdateDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; Condition: string; var LastError: TLastError);
begin

    if Fields.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add(IntToStr(StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.Email].Value), 0)));
    end;

    // Call event and call duration always comes together
    if Fields.CallEvent then
    begin

        var LCallEvent: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.CallEvent].Value), 0);
        Inc(LCallEvent);

        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add(LCallEvent.ToString());

        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(Fields.CallDuration.ToString());

    end;

    if Fields.EmailReminder then
    begin
        var LEmailReminder: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailReminder].Value), 0);
        Inc(LEmailReminder);
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add(LEmailReminder.ToString());
    end;

    if Fields.EmailAutoStat then
    begin
        var LEmailAutoStat: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailAutoStat].Value), 0);
        Inc(LEmailAutoStat);
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add(LEmailAutoStat.ToString());
    end;

    if Fields.EmailManuStat then
    begin
        var LEmailManuStat: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailManuStat].Value), 0);
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
        LastError.IsSucceeded:=True;
        LastError.ErrorMessage:='"DailyComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.';
        if Fields.EventLog then ThreadFileLog.Log(LastError.ErrorMessage);
    end
    else
    begin
        LastError.IsSucceeded:=False;
        LastError.ErrorMessage:='Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.';
        ThreadFileLog.Log('Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.EditDailyComment(Fields: TDailyCommentFields; Callback: TEditDailyComment);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var DailyText: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            var Condition:    string:=TDailyComment.Cuid + TSql.EQUAL + QuotedStr(Fields.CUID) + TSql._AND + TDailyComment.AgeDate + TSql.EQUAL + QuotedStr(Fields.AgeDateSel);
            var DataCheckSum: string:=Fields.CUID + StringReplace(Fields.AgeDateSel, '-', '', [rfReplaceAll]);

            DailyText.CustFilter:=TSql.WHERE + Condition;
            DailyText.OpenTable(TDailyComment.DailyComment);

            if not (DailyText.DataSet.RecordCount = 0) then
            begin

                // -------------------------
                // Update exisiting comment.
                // -------------------------

                // Allow to extend comment by adding to existing wording a new comment line
                if Fields.ExtendComment then
                    Fields.Comment:=DailyText.DataSet.Fields[TDailyComment.FixedComment].Value + TChars.CRLF + Fields.Comment;

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(SessionService.SessionUser));

                FUpdateDailyComment(DailyText, Fields, Condition, LastError);

            end
            else
            begin

                // ------------------
                // Insert new record.
                // ------------------

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.GroupId);
                DailyText.Values.Add(Fields.GroupIdSel);

                DailyText.Columns.Add(TDailyComment.Cuid);
                DailyText.Values.Add(Fields.CUID);

                DailyText.Columns.Add(TDailyComment.AgeDate);
                DailyText.Values.Add(Fields.AgeDateSel);

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(SessionService.SessionUser));

                DailyText.Columns.Add(TDailyComment.DataCheckSum);
                DailyText.Values.Add(DataCheckSum);

                FInsertDailyComment(DailyText, Fields, LastError);

            end;

        finally
            DailyText.Free;
        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
        end);

    end);

    NewTask.Start();

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.FInsertGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields; var LastError: TLastError);
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
        LastError.IsSucceeded:=True;
        LastError.ErrorMessage:='"GeneralComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.';
        if Fields.EventLog then ThreadFileLog.Log('"GeneralComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        LastError.IsSucceeded:=False;
        LastError.ErrorMessage:='Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.';
        ThreadFileLog.Log('Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.FUpdateGeneralComment(var GenText: TDataTables ;var Fields: TGeneralCommentFields; Condition: string; var LastError: TLastError);
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
        LastError.IsSucceeded:=True;
        LastError.ErrorMessage:='"GeneralComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.';
        if Fields.EventLog then ThreadFileLog.Log('"GeneralComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        LastError.IsSucceeded:=False;
        LastError.ErrorMessage:='Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.';
        ThreadFileLog.Log('Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TComments.EditGeneralComment(Fields: TGeneralCommentFields; Callback: TEditGeneralComment);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var GenText: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            var Condition: string:=TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(Fields.CUID);
            GenText.CustFilter:=TSql.WHERE + Condition;
            GenText.OpenTable(TGeneralComment.GeneralComment);

            if not (GenText.DataSet.RecordCount = 0) then
            begin

                // --------------
                // Update record.
                // --------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(SessionService.SessionUser));

                FUpdateGeneralComment(GenText, Fields, Condition, LastError);

            end
            else
            begin

                // ------------------
                // Insert new record.
                // ------------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Cuid);
                GenText.Values.Add(Fields.CUID);

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(SessionService.SessionUser));

                FInsertGeneralComment(GenText, Fields, LastError);

            end;

        finally
            GenText.Free;
        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
        end);

    end);

    NewTask.Start();

end;


end.

