unit Async.Comments;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
    Unity.Grid,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for updating/inserting data for daily comment.
    /// </summary>
    TEditDailyComment = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for updating/inserting data for general comment.
    /// </summary>
    TEditGeneralComment = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results from daily comments list request.
    /// </summary>
    TGetDailyComments = procedure(Comments: TArray<TDailyCommentFields>; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results for general comment single dataset.
    /// </summary>
    TGetGeneralComments = procedure(Comments: TGeneralCommentFields; CallResponse: TCallResponse) of object;


    IComments = interface(IInterface)
    ['{1B3127BB-EC78-4177-A286-C138E02709D3}']
        /// <summary>
        /// Allow to async. update daily comment (either insert or update). Requires to pass database table fields as payload with comment Id
        /// parameter for update. If comment Id is not supplied (assumes id = 0), then POST method is called. Please note that only non-existing
        /// comment can be added for given customer number, age date and company code.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure EditDailyCommentAsync(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);
        /// <summary>
        /// Allow to async. update general comment (either insert or update). Requires to pass database table fields as payload.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure EditGeneralCommentAsync(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);
        /// <summary>
        /// Allow to async. check ig daily comment exists for given company code, customer number and user alias and age date.
        /// Note: there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckGeneralCommentAwaited(SourceDBName: string; CustNumber: integer; var CommentExists: TCommentExists): TCallResponse;
        /// <summary>
        /// Allow to async. retrive general comment for given company code, customer number and user alias.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure GetGeneralCommentAsync(SourceDBName: string; CustNumber: integer; UserAlias: string; Callback: TGetGeneralComments);
        /// <summary>
        /// Allow to async. check ig daily comment exists for given company code, customer number and user alias and age date.
        /// Note: there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckDailyCommentAwaited(SourceDBName: string; CustNumber: integer; AgeDate: string; var CommentExists: TCommentExists): TCallResponse;
        /// <summary>
        /// Allow to async. retrieve daily comments for given company code, customer number and user alias.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure GetDailyCommentsAsync(SourceDBName: string; CustNumber: integer; UserAlias: string; Callback: TGetDailyComments);
        /// <summary>
        /// Allow to async. update daily comment for given company code, customer number and age date. Unlike EditDailyComment, it will not perform insertion
        /// if comment does not exists. Note that there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function UpdateDailyCommentAwaited(SourceDBName: string; CustNumber: integer; AgeDate: string): TCallResponse;
    end;


    TComments = class(TInterfacedObject, IComments)
    {$TYPEINFO ON}
    public
        /// <summary>
        /// Allow to async. update daily comment (either insert or update). Requires to pass database table fields as payload and comment id
        /// parameter for update. If comment id is not supplied (assumes id = 0), then POST method is called. Please note that only non-existing
        /// comment can be added for given customer number, age date and company code.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// Note: this method defines callback as nil be default.
        /// </remarks>
        procedure EditDailyCommentAsync(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);
        /// <summary>
        /// Allow to async. update general comment (either insert or update). Requires to pass database table fields as payload.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// Note: this method defines callback as nil be default.
        /// </remarks>
        procedure EditGeneralCommentAsync(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);
        /// <summary>
        /// Allow to async. check ig daily comment exists for given company code, customer number and user alias and age date.
        /// Note: there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckGeneralCommentAwaited(SourceDBName: string; CustNumber: integer; var CommentExists: TCommentExists): TCallResponse;
        /// <summary>
        /// Allow to async. retrive general comment for given company code, customer number and user alias.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure GetGeneralCommentAsync(SourceDBName: string; CustNumber: integer; UserAlias: string; Callback: TGetGeneralComments);
        /// <summary>
        /// Allow to async. check ig daily comment exists for given company code, customer number and user alias and age date.
        /// Note: there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckDailyCommentAwaited(SourceDBName: string; CustNumber: integer; AgeDate: string; var CommentExists: TCommentExists): TCallResponse;
        /// <summary>
        /// Allow to async. retrieve daily comments for given company code, customer number and user alias.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure GetDailyCommentsAsync(SourceDBName: string; CustNumber: integer; UserAlias: string; Callback: TGetDailyComments);
        /// <summary>
        /// Allow to async. update daily comment for given company code, customer number and age date. Unlike EditDailyComment, it will not perform insertion
        /// if comment does not exists. Note that there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function UpdateDailyCommentAwaited(SourceDBName: string; CustNumber: integer; AgeDate: string): TCallResponse;
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Settings,
    Unity.RestWrapper,
    Unity.Helpers,
    Unity.EventLogger,
    Unity.SessionService,
    Api.UserGeneralComment,
    Api.UserGeneralCommentAdd,
    Api.UserGeneralCommentUpdate,
    Api.UserGeneralCommentAdded,
    Api.UserGeneralCommentCheck,
    Api.UserGeneralCommentUpdated,
    Api.UserDailyCommentsList,
    Api.UserDailyCommentAdd,
    Api.UserDailyCommentAdded,
    Api.UserDailyCommentUpdate,
    Api.UserDailyCommentUpdated,
    Api.UserDailyCommentCheck;


procedure TComments.EditDailyCommentAsync(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'dailycommentaries/'
            + PayLoad.SourceDBName
            + '/'
            + PayLoad.CustomerNumber.ToString()
            + '/comment/'
            + PayLoad.UserAlias
            + '/';

        if PayLoad.CommentId > 0 then
        begin

            Restful.RequestMethod:=TRESTRequestMethod.rmPATCH;
            ThreadFileLog.Log('[EditGeneralComment]: Executing PATCH ' + Restful.ClientBaseURL);

            var UserDailyCommentUpdate:=TUserDailyCommentUpdate.Create();
            try

                UserDailyCommentUpdate.CallEvent           :=PayLoad.CallEvent;
                UserDailyCommentUpdate.CallDuration        :=PayLoad.CallDuration;
                UserDailyCommentUpdate.FixedStatementsSent :=PayLoad.FixedStatementsSent;
                UserDailyCommentUpdate.CustomStatementsSent:=PayLoad.CustomStatementsSent;
                UserDailyCommentUpdate.FixedRemindersSent  :=PayLoad.FixedRemindersSent;
                UserDailyCommentUpdate.CustomRemindersSent :=PayLoad.CustomRemindersSent;
                UserDailyCommentUpdate.UserComment         :=PayLoad.UserComment;
                UserDailyCommentUpdate.CommentId           :=PayLoad.CommentId;

                Restful.CustomBody:=TJson.ObjectToJsonString(UserDailyCommentUpdate);

            finally
                UserDailyCommentUpdate.Free();
            end;

        end
        else
        begin

            Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
            ThreadFileLog.Log('[EditGeneralComment]: Executing POST ' + Restful.ClientBaseURL);

            var UserDailyCommentAdd:=TUserDailyCommentAdd.Create();
            try

                UserDailyCommentAdd.CallEvent           :=PayLoad.CallEvent;
                UserDailyCommentAdd.CallDuration        :=PayLoad.CallDuration;
                UserDailyCommentAdd.FixedStatementsSent :=PayLoad.FixedStatementsSent;
                UserDailyCommentAdd.CustomStatementsSent:=PayLoad.CustomStatementsSent;
                UserDailyCommentAdd.FixedRemindersSent  :=PayLoad.FixedRemindersSent;
                UserDailyCommentAdd.CustomRemindersSent :=PayLoad.CustomRemindersSent;
                UserDailyCommentAdd.AgeDate             :=PayLoad.AgeDate;
                UserDailyCommentAdd.UserComment         :=PayLoad.UserComment;

                Restful.CustomBody:=TJson.ObjectToJsonString(UserDailyCommentAdd);

            finally
                UserDailyCommentAdd.Free();
            end;

        end;

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                if not PayLoad.CommentId = 0 then
                begin

                    var UserDailyCommentUpdated:=TJson.JsonToObject<TUserDailyCommentUpdated>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserDailyCommentUpdated.IsSucceeded;
                        CallResponse.LastMessage:=UserDailyCommentUpdated.Error.ErrorDesc;
                        CallResponse.ErrorCode  :=UserDailyCommentUpdated.Error.ErrorCode;
                    finally
                        UserDailyCommentUpdated.Free();
                    end;

                end
                else
                begin

                    var UserDailyCommentAdded:=TJson.JsonToObject<TUserDailyCommentAdded>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserDailyCommentAdded.IsSucceeded;
                        CallResponse.LastMessage:=UserDailyCommentAdded.Error.ErrorDesc;
                        CallResponse.ErrorCode  :=UserDailyCommentAdded.Error.ErrorCode;
                    finally
                        UserDailyCommentAdded.Free();
                    end;

                end;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                ThreadFileLog.Log('[EditGeneralComment]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[EditGeneralComment]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[EditGeneralComment]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[EditGeneralComment]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[EditGeneralComment]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


procedure TComments.EditGeneralCommentAsync(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);
begin

    var QueryData: TCommentExists;
    var CallResponse:=CheckGeneralCommentAwaited(
        PayLoad.SourceDBName,
        PayLoad.CustomerNumber,
        QueryData
    );

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'generalcommentaries/'
            + PayLoad.SourceDBName
            + '/'
            + PayLoad.CustomerNumber.ToString()
            + '/comment/'
            + PayLoad.UserAlias
            + '/';

        if QueryData.DoesCommentExists then
        begin

            Restful.RequestMethod:=TRESTRequestMethod.rmPATCH;
            ThreadFileLog.Log('[EditGeneralComment]: Executing PATCH ' + Restful.ClientBaseURL);

            var UserGeneralCommentUpdate:=TUserGeneralCommentUpdate.Create();
            try

                if not String.IsNullOrEmpty(PayLoad.FollowUp)    then UserGeneralCommentUpdate.FollowUp   :=PayLoad.FollowUp;
                if not String.IsNullOrEmpty(PayLoad.Free1)       then UserGeneralCommentUpdate.Free1      :=PayLoad.Free1;
                if not String.IsNullOrEmpty(PayLoad.Free1)       then UserGeneralCommentUpdate.Free2      :=PayLoad.Free2;
                if not String.IsNullOrEmpty(PayLoad.Free1)       then UserGeneralCommentUpdate.Free3      :=PayLoad.Free3;
                if not String.IsNullOrEmpty(PayLoad.UserComment) then UserGeneralCommentUpdate.UserComment:=PayLoad.UserComment;

                UserGeneralCommentUpdate.CommentId:=QueryData.CommentId;
                Restful.CustomBody:=TJson.ObjectToJsonString(UserGeneralCommentUpdate);

            finally
                UserGeneralCommentUpdate.Free();
            end;

        end
        else
        begin

            Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
            ThreadFileLog.Log('[EditGeneralComment]: Executing POST ' + Restful.ClientBaseURL);

            var UserGeneralCommentAdd:=TUserGeneralCommentAdd.Create();
            try

                if not String.IsNullOrEmpty(PayLoad.FollowUp) then UserGeneralCommentAdd.FollowUp:=PayLoad.FollowUp;
                if not String.IsNullOrEmpty(PayLoad.Free1)    then UserGeneralCommentAdd.Free1   :=PayLoad.Free1;
                if not String.IsNullOrEmpty(PayLoad.Free1)    then UserGeneralCommentAdd.Free2   :=PayLoad.Free2;
                if not String.IsNullOrEmpty(PayLoad.Free1)    then UserGeneralCommentAdd.Free3   :=PayLoad.Free3;

                UserGeneralCommentAdd.UserComment:=PayLoad.UserComment;
                Restful.CustomBody:=TJson.ObjectToJsonString(UserGeneralCommentAdd);

            finally
                UserGeneralCommentAdd.Free();
            end;

        end;

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                if QueryData.DoesCommentExists then
                begin

                    var UserGeneralCommentUpdated:=TJson.JsonToObject<TUserGeneralCommentUpdated>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserGeneralCommentUpdated.IsSucceeded;
                        CallResponse.LastMessage:=UserGeneralCommentUpdated.Error.ErrorDesc;
                        CallResponse.ErrorCode  :=UserGeneralCommentUpdated.Error.ErrorCode;
                    finally
                        UserGeneralCommentUpdated.Free();
                    end;

                end
                else
                begin

                    var UserGeneralCommentAdded:=TJson.JsonToObject<TUserGeneralCommentAdded>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserGeneralCommentAdded.IsSucceeded;
                        CallResponse.LastMessage:=UserGeneralCommentAdded.Error.ErrorDesc;
                        CallResponse.ErrorCode  :=UserGeneralCommentAdded.Error.ErrorCode;
                    finally
                        UserGeneralCommentAdded.Free();
                    end;

                end;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                ThreadFileLog.Log('[EditGeneralComment]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[EditGeneralComment]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[EditGeneralComment]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[EditGeneralComment]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[EditGeneralComment]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TComments.CheckGeneralCommentAwaited(SourceDBName: string; CustNumber: integer; var CommentExists: TCommentExists): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var DoesCommentExists: TCommentExists;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'generalcommentaries/'
            + SourceDBName
            + '/'
            + CustNumber.ToString()
            + '/check/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[CheckGeneralCommentAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserGeneralCommentCheck:=TJson.JsonToObject<TUserGeneralCommentCheck>(Restful.Content);
                try
                    DoesCommentExists.DoesCommentExists:=UserGeneralCommentCheck.DoesCommentExists;
                    DoesCommentExists.UserComment:=UserGeneralCommentCheck.UserComment;
                    DoesCommentExists.CommentId:=UserGeneralCommentCheck.CommentId;
                    CallResponse.IsSucceeded:=UserGeneralCommentCheck.IsSucceeded;
                    CallResponse.LastMessage:=UserGeneralCommentCheck.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=UserGeneralCommentCheck.Error.ErrorCode;
                finally
                    UserGeneralCommentCheck.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[CheckDailyCommentAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[CheckDailyCommentAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[CheckDailyCommentAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckDailyCommentAwaited]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    CommentExists:=DoesCommentExists;
    Result:=CallResponse;

end;


procedure TComments.GetGeneralCommentAsync(SourceDBName: string; CustNumber: integer; UserAlias: string; Callback: TGetGeneralComments);
begin

    var CallResponse: TCallResponse;
    var TempComments: TGeneralCommentFields;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'generalcommentaries/'
            + SourceDBName
            + '/'
            + CustNumber.ToString()
            + '/comment/'
            + UserAlias
            + '/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetGeneralCommentAsync]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserGeneralComment:=TJson.JsonToObject<TUserGeneralComment>(Restful.Content);
                try

                    TempComments.CommentId  :=UserGeneralComment.CommentId;
                    TempComments.FollowUp   :=UserGeneralComment.FollowUp;
                    TempComments.Free1      :=UserGeneralComment.Free1;
                    TempComments.Free2      :=UserGeneralComment.Free2;
                    TempComments.Free3      :=UserGeneralComment.Free3;
                    TempComments.UserComment:=UserGeneralComment.UserComment;

                    CallResponse.IsSucceeded:=UserGeneralComment.IsSucceeded;
                    CallResponse.LastMessage:=UserGeneralComment.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=UserGeneralComment.Error.ErrorCode;
                    ThreadFileLog.Log('[GetGeneralCommentAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserGeneralComment.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetGeneralCommentAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetGeneralCommentAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetGeneralCommentAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetGeneralCommentAsync]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(TempComments, CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TComments.CheckDailyCommentAwaited(SourceDBName: string; CustNumber: integer; AgeDate: string; var CommentExists: TCommentExists): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var DoesCommentExists: TCommentExists;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'dailycommentaries/'
            + SourceDBName
            + '/'
            + CustNumber.ToString()
            + '/check/'
            + AgeDate
            + '/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[CheckDailyCommentAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserDailyCommentCheck:=TJson.JsonToObject<TUserDailyCommentCheck>(Restful.Content);
                try
                    DoesCommentExists.DoesCommentExists:=UserDailyCommentCheck.DoesCommentExists;
                    DoesCommentExists.UserComment:=UserDailyCommentCheck.UserComment;
                    DoesCommentExists.CommentId:=UserDailyCommentCheck.CommentId;
                    CallResponse.IsSucceeded:=UserDailyCommentCheck.IsSucceeded;
                    CallResponse.LastMessage:=UserDailyCommentCheck.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=UserDailyCommentCheck.Error.ErrorCode;
                finally
                    UserDailyCommentCheck.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[CheckDailyCommentAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[CheckDailyCommentAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[CheckDailyCommentAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckDailyCommentAwaited]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    CommentExists:=DoesCommentExists;
    Result:=CallResponse;

end;


procedure TComments.GetDailyCommentsAsync(SourceDBName: string; CustNumber: integer; UserAlias: string; Callback: TGetDailyComments);
begin

    var CallResponse: TCallResponse;
    var TempComments:=TArray<TDailyCommentFields>.Create();

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'dailycommentaries/'
            + SourceDBName
            + '/'
            + CustNumber.ToString()
            + '/comments/'
            + UserAlias
            + '/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetDailyCommentsAsync]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserDailyCommentsList:=TJson.JsonToObject<TUserDailyCommentsList>(Restful.Content);
                try

                    SetLength(TempComments, Length(UserDailyCommentsList.CommentId));

                    for var iCNT:=0 to Length(UserDailyCommentsList.CommentId) - 1 do
                    begin
                        TempComments[iCNT].CommentId           :=UserDailyCommentsList.CommentId[iCNT];
                        TempComments[iCNT].SourceDBName        :=UserDailyCommentsList.SourceDBName[iCNT];
                        TempComments[iCNT].CustomerNumber      :=UserDailyCommentsList.CustomerNumber[iCNT];
                        TempComments[iCNT].AgeDate             :=UserDailyCommentsList.AgeDate[iCNT];
                        TempComments[iCNT].CallEvent           :=UserDailyCommentsList.CallEvent[iCNT];
                        TempComments[iCNT].CallDuration        :=UserDailyCommentsList.CallDuration[iCNT];
                        TempComments[iCNT].FixedStatementsSent :=UserDailyCommentsList.FixedStatementsSent[iCNT];
                        TempComments[iCNT].CustomStatementsSent:=UserDailyCommentsList.CustomStatementsSent[iCNT];
                        TempComments[iCNT].FixedRemindersSent  :=UserDailyCommentsList.FixedRemindersSent[iCNT];
                        TempComments[iCNT].CustomRemindersSent :=UserDailyCommentsList.CustomRemindersSent[iCNT];
                        TempComments[iCNT].UserComment         :=UserDailyCommentsList.UserComment[iCNT];
                        TempComments[iCNT].UserAlias           :=UserDailyCommentsList.UserAlias[iCNT];
                        TempComments[iCNT].EntryDateTime       :=UserDailyCommentsList.EntryDateTime[iCNT];
                    end;

                    CallResponse.IsSucceeded:=UserDailyCommentsList.IsSucceeded;
                    CallResponse.LastMessage:=UserDailyCommentsList.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=UserDailyCommentsList.Error.ErrorCode;
                    ThreadFileLog.Log('[GetDailyCommentsAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserDailyCommentsList.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetDailyCommentsAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetDailyCommentsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetDailyCommentsAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetDailyCommentsAsync]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(TempComments, CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TComments.UpdateDailyCommentAwaited(SourceDBName: string; CustNumber: integer; AgeDate: string): TCallResponse;
begin

    var CallResponse: TCallResponse;
    try

        var CommentExists: TCommentExists;
        CheckDailyCommentAwaited(
            SourceDBName,
            CustNumber,
            AgeDate,
            CommentExists
        );

        var LocalPayLoad: TDailyCommentFields;
        var ExtendedComment: string;

        if not String.IsNullOrWhiteSpace(CommentExists.UserComment) then
            ExtendedComment:=CommentExists.UserComment + #13#10 + 'New communication has been sent.'
        else
            ExtendedComment:='New communication has been sent.';

        LocalPayLoad.CommentId           :=CommentExists.CommentId;
        LocalPayLoad.SourceDBName        :=SourceDBName;
        LocalPayLoad.CustomerNumber      :=CustNumber;
        LocalPayLoad.AgeDate             :=AgeDate;
        LocalPayLoad.CallEvent           :=0;
        LocalPayLoad.CallDuration        :=0;
        LocalPayLoad.FixedStatementsSent :=0;
        LocalPayLoad.CustomStatementsSent:=0;
        LocalPayLoad.FixedRemindersSent  :=0;
        LocalPayLoad.CustomRemindersSent :=0;
        LocalPayLoad.UserComment         :=ExtendedComment;
        LocalPayLoad.UserAlias           :=SessionService.SessionData.AliasName;

        EditDailyCommentAsync(LocalPayLoad);
        CallResponse.IsSucceeded:=True;

    except
        on E: Exception do
        begin
            CallResponse.IsSucceeded:=False;
            CallResponse.LastMessage:='[UpdateDailyCommentAwaited]: Cannot execute the request. Description: ' + E.Message;
            ThreadFileLog.Log(CallResponse.LastMessage);
        end;

    end;

    Result:=CallResponse;

end;


end.

