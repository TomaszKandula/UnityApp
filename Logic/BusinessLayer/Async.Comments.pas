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


    /// <summary>
    /// Callback signature (delegate) for updating (insert/update actions) daily comment.
    /// </summary>
    TEditDailyComment = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for updating (insert/update actions) general comment.
    /// </summary>
    TEditGeneralComment = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for getting results for daily comments list.
    /// </summary>
    TGetDailyComments = procedure(ReturnedGrid: TStringGrid; CallResponse: TCallResponse) of object;


    IComments = interface(IInterface)
    ['{1B3127BB-EC78-4177-A286-C138E02709D3}']

        /// <summary>
        /// Allow to async. update daily comment (either insert or update). Requires to pass database table fields as payload.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure EditDailyComment(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);

        /// <summary>
        /// Allow to async. update general comment (either insert or update). Requires to pass database table fields as payload.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure EditGeneralComment(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);

        /// <summary>
        /// Allow to async. retrive general comment for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetGeneralCommentAwaited(CUID: string): string;

        /// <summary>
        /// Allow to async. retrieve daily comments for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDailyCommentsAwaited(CUID: string): TStringGrid;

    end;


    TComments = class(TInterfacedObject, IComments)
    {$TYPEINFO ON}
    private
        procedure FInsertDailyComment(var DailyText: TDataTables; var PayLoad: TDailyCommentFields; var CallResponse: TCallResponse);
        procedure FUpdateDailyComment(var DailyText: TDataTables; var PayLoad: TDailyCommentFields; Condition: string; var CallResponse: TCallResponse);
        procedure FInsertGeneralComment(var GenText: TDataTables; var PayLoad: TGeneralCommentFields; var CallResponse: TCallResponse);
        procedure FUpdateGeneralComment(var GenText: TDataTables; var PayLoad: TGeneralCommentFields; Condition: string; var CallResponse: TCallResponse);
    public

        /// <summary>
        /// Allow to async. update daily comment (either insert or update). Requires to pass database table fields as payload.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// Note: this method defines callback as nil be default.
        /// </remarks>
        procedure EditDailyComment(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);

        /// <summary>
        /// Allow to async. update general comment (either insert or update). Requires to pass database table fields as payload.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// Note: this method defines callback as nil be default.
        /// </remarks>
        procedure EditGeneralComment(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);

        /// <summary>
        /// Allow to async. retrive general comment for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetGeneralCommentAwaited(CUID: string): string;

        /// <summary>
        /// Allow to async. retrieve daily comments for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDailyCommentsAwaited(CUID: string): TStringGrid;

    end;


implementation


uses
    Handler.Database{Legacy},
    Unity.Sql,
    Unity.Chars,
    Unity.Helpers,
    Unity.Unknown,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Sync.Documents,
    DbModel{Legacy};


procedure TComments.FInsertDailyComment(var DailyText: TDataTables; var PayLoad: TDailyCommentFields; var CallResponse: TCallResponse);
begin

    if PayLoad.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add('0');
    end;

    if PayLoad.EmailReminder then
    begin
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add('0');
    end;

    if PayLoad.EmailAutoStat then
    begin
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add('0');
    end;

    if PayLoad.EmailManuStat then
    begin
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add('0');
    end;

    if PayLoad.CallEvent then
    begin
        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add('1');
        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(PayLoad.CallDuration.ToString());
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add('0');
        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add('0');
    end;

    DailyText.Columns.Add(TDailyComment.FixedComment);
    DailyText.Values.Add(PayLoad.Comment);

    if (DailyText.InsertInto(TDailyComment.DailyComment, True)) and (DailyText.RowsAffected > 0) then
    begin
        CallResponse.IsSucceeded:=True;
        CallResponse.LastMessage:='"DailyComment" table has been posted (CUID: ' + PayLoad.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.';
        if PayLoad.EventLog then ThreadFileLog.Log(CallResponse.LastMessage);
    end
    else
    begin
        CallResponse.IsSucceeded:=False;
        CallResponse.LastMessage:='Cannot post daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.';
        ThreadFileLog.Log('Cannot update daily comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
    end;

end;


procedure TComments.FUpdateDailyComment(var DailyText: TDataTables; var PayLoad: TDailyCommentFields; Condition: string; var CallResponse: TCallResponse);
begin

    if PayLoad.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add(IntToStr(StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.Email].Value), 0)));
    end;

    // Call event and call duration always comes together
    if PayLoad.CallEvent then
    begin

        var LCallEvent: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.CallEvent].Value), 0);
        Inc(LCallEvent);

        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add(LCallEvent.ToString());

        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(PayLoad.CallDuration.ToString());

    end;

    if PayLoad.EmailReminder then
    begin
        var LEmailReminder: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailReminder].Value), 0);
        Inc(LEmailReminder);
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add(LEmailReminder.ToString());
    end;

    if PayLoad.EmailAutoStat then
    begin
        var LEmailAutoStat: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailAutoStat].Value), 0);
        Inc(LEmailAutoStat);
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add(LEmailAutoStat.ToString());
    end;

    if PayLoad.EmailManuStat then
    begin
        var LEmailManuStat: integer:=StrToIntDef(THelpers.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailManuStat].Value), 0);
        Inc(LEmailManuStat);
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add(LEmailManuStat.ToString());
    end;

    if not(PayLoad.Comment = '') then
    begin
        DailyText.Columns.Add(TDailyComment.FixedComment);
        DailyText.Values.Add(PayLoad.Comment);
    end;

    if (DailyText.UpdateRecord(TDailyComment.DailyComment, True, Condition)) and (DailyText.RowsAffected > 0) then
    begin
        CallResponse.IsSucceeded:=True;
        CallResponse.LastMessage:='"DailyComment" table has been updated (CUID: ' + PayLoad.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.';
        if PayLoad.EventLog then ThreadFileLog.Log(CallResponse.LastMessage);
    end
    else
    begin
        CallResponse.IsSucceeded:=False;
        CallResponse.LastMessage:='Cannot update daily comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.';
        ThreadFileLog.Log('Cannot update daily comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
    end;

end;


procedure TComments.EditDailyComment(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var DailyText: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            var Condition:    string:=TDailyComment.Cuid + TSql.EQUAL + QuotedStr(PayLoad.CUID) + TSql._AND + TDailyComment.AgeDate + TSql.EQUAL + QuotedStr(PayLoad.AgeDateSel);
            var DataCheckSum: string:=PayLoad.CUID + StringReplace(PayLoad.AgeDateSel, '-', '', [rfReplaceAll]);

            DailyText.CustFilter:=TSql.WHERE + Condition;
            DailyText.OpenTable(TDailyComment.DailyComment);

            if not (DailyText.DataSet.RecordCount = 0) then
            begin

                // -------------------------
                // Update exisiting comment.
                // -------------------------

                // Allow to extend comment by adding to existing wording a new comment line
                if PayLoad.ExtendComment then
                    PayLoad.Comment:=DailyText.DataSet.Fields[TDailyComment.FixedComment].Value + TChars.CRLF + PayLoad.Comment;

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(SessionService.SessionData.AliasName));

                FUpdateDailyComment(DailyText, PayLoad, Condition, CallResponse);

            end
            else
            begin

                // ------------------
                // Insert new record.
                // ------------------

                DailyText.CleanUp();

                DailyText.Columns.Add(TDailyComment.GroupId);
                DailyText.Values.Add(PayLoad.GroupIdSel);

                DailyText.Columns.Add(TDailyComment.Cuid);
                DailyText.Values.Add(PayLoad.CUID);

                DailyText.Columns.Add(TDailyComment.AgeDate);
                DailyText.Values.Add(PayLoad.AgeDateSel);

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(SessionService.SessionData.AliasName));

                DailyText.Columns.Add(TDailyComment.DataCheckSum);
                DailyText.Values.Add(DataCheckSum);

                FInsertDailyComment(DailyText, PayLoad, CallResponse);

            end;

        finally
            DailyText.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


procedure TComments.FInsertGeneralComment(var GenText: TDataTables; var PayLoad: TGeneralCommentFields; var CallResponse: TCallResponse);
begin

    if not(PayLoad.FixedComment = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add(PayLoad.FixedComment);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add('');
    end;

    if not(PayLoad.FollowUp = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add(PayLoad.FollowUp);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add('');
    end;

    if not(PayLoad.Free1 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add(PayLoad.Free1);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add('');
    end;

    if not(PayLoad.Free2 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add(PayLoad.Free2);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add('');
    end;

    if not(PayLoad.Free3 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add(PayLoad.Free3);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add('');
    end;

    if (GenText.InsertInto(TGeneralComment.GeneralComment, True)) and (GenText.RowsAffected > 0) then
    begin
        CallResponse.IsSucceeded:=True;
        CallResponse.LastMessage:='"GeneralComment" table has been posted (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.';
        if PayLoad.EventLog then ThreadFileLog.Log('"GeneralComment" table has been posted (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        CallResponse.IsSucceeded:=False;
        CallResponse.LastMessage:='Cannot update general comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.';
        ThreadFileLog.Log('Cannot update general comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
    end;

end;


procedure TComments.FUpdateGeneralComment(var GenText: TDataTables; var PayLoad: TGeneralCommentFields; Condition: string; var CallResponse: TCallResponse);
begin

    if not(PayLoad.FixedComment = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add(PayLoad.FixedComment);
    end;

    if not(PayLoad.FollowUp = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add(PayLoad.FollowUp);
    end;

    if not(PayLoad.Free1 = TUnknown.Null) then
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add(PayLoad.Free1);
    end;

    if not(PayLoad.Free2 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add(PayLoad.Free2);
    end;

    if not(PayLoad.Free3 = TUnknown.Null) then
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add(PayLoad.Free3);
    end;

    if (GenText.UpdateRecord(TGeneralComment.GeneralComment, True, Condition)) and (GenText.RowsAffected > 0) then
    begin
        CallResponse.IsSucceeded:=True;
        CallResponse.LastMessage:='"GeneralComment" table has been updated (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.';
        if PayLoad.EventLog then ThreadFileLog.Log('"GeneralComment" table has been updated (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        CallResponse.IsSucceeded:=False;
        CallResponse.LastMessage:='Cannot update general comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.';
        ThreadFileLog.Log('Cannot update general comment (CUID: ' + PayLoad.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
    end;

end;


procedure TComments.EditGeneralComment(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var GenText: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            var Condition: string:=TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(PayLoad.CUID);
            GenText.CustFilter:=TSql.WHERE + Condition;
            GenText.OpenTable(TGeneralComment.GeneralComment);

            if not (GenText.DataSet.RecordCount = 0) then
            begin

                // --------------
                // Update record.
                // --------------

                GenText.CleanUp();

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(SessionService.SessionData.AliasName));

                FUpdateGeneralComment(GenText, PayLoad, Condition, CallResponse);

            end
            else
            begin

                // ------------------
                // Insert new record.
                // ------------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Cuid);
                GenText.Values.Add(PayLoad.CUID);

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(SessionService.SessionData.AliasName));

                FInsertGeneralComment(GenText, PayLoad, CallResponse);

            end;

        finally
            GenText.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TComments.GetGeneralCommentAwaited(CUID: string): string;
begin

    var NewResult: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var GenText: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                GenText.CustFilter:=TSql.WHERE + TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(CUID);
                GenText.OpenTable(TGeneralComment.GeneralComment);

                if not (GenText.DataSet.EOF) then
                    NewResult:=THelpers.OleGetStr(GenText.DataSet.Fields[TGeneralComment.FixedComment].Value);

            except
                on E: Exception do
                    ThreadFileLog.Log('[GetGeneralCommentAwaited]: ' + E.Message);

            end;

        finally
            GenText.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    {If under ARC / do not manually release it}
    Result:=NewResult;

end;


function TComments.GetDailyCommentsAwaited(CUID: string): TStringGrid;
begin

    var NewResult: TStringGrid:=TStringGrid.Create(nil);

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DailyText: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                DailyText.Columns.Add(TDailyComment.AgeDate);
                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Columns.Add(TDailyComment.FixedComment);

                DailyText.CustFilter:=TSql.WHERE + TDailyComment.Cuid + TSql.EQUAL + QuotedStr(CUID);
                DailyText.OpenTable(TDailyComment.DailyComment);
                DailyText.DataSet.Sort:=TDailyComment.Stamp + TSql.DESC;

                if not (DailyText.DataSet.EOF) then DailyText.SqlToGrid(NewResult, DailyText.DataSet, False, True);

            except
                on E: Exception do
                    ThreadFileLog.Log('[GetDailyCommentsAwaited]: ' + E.Message);

            end;

        finally
            DailyText.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    {If under ARC / do not manually release it}
    Result:=NewResult;

end;


end.

