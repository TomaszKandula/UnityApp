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
    /// Callback signature for updating (insert/update actions) daily comment.
    /// </summary>
    TEditDailyComment = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for updating (insert/update actions) general comment.
    /// </summary>
    TEditGeneralComment = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results for daily comments list.
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
        /// Allow to async. retrive general comment for given company code, customer number and user alias. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetGeneralCommentAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string; var Output: TGeneralCommentFields): TCallResponse;

        /// <summary>
        /// Allow to async. retrieve daily comments for given company code, customer number and user alias. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDailyCommentsAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string): TCallResponse;

    end;


    TComments = class(TInterfacedObject, IComments)
    {$TYPEINFO ON}
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
        /// Allow to async. retrive general comment for given company code, customer number and user alias. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetGeneralCommentAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string; var Output: TGeneralCommentFields): TCallResponse;

        /// <summary>
        /// Allow to async. retrieve daily comments for given company code, customer number and user alias. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDailyCommentsAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string): TCallResponse;

    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.RestWrapper,
    Api.UserGeneralComment,
    Api.UserGeneralCommentAdd,
    Api.UserGeneralCommentUpdate,
    Api.UserGeneralCommentAdded,
    Api.UserGeneralCommentUpdated;


procedure TComments.EditDailyComment(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);
begin

end;


procedure TComments.EditGeneralComment(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);
begin

    var QueryData: TGeneralCommentFields;
    var CallResponse:=GetGeneralCommentAwaited(
        PayLoad.CompanyCode.ToInteger(),
        PayLoad.CustomerNumber.ToInteger(),
        PayLoad.UserAlias,
        QueryData
    );

    var CapturedCommentId:=QueryData.CommentId;

    var ShouldUpdate: boolean;
    case CallResponse.ErrorNumber of

        0:    ShouldUpdate:=True;
        1016: ShouldUpdate:=False;

    end;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl
            + 'generalcommentaries/'
            + PayLoad.CompanyCode
            + '/'
            + PayLoad.CustomerNumber
            + '/comment/'
            + PayLoad.UserAlias
            + '/';

        if ShouldUpdate then
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

                UserGeneralCommentUpdate.CommentId:=CapturedCommentId;
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

                if ShouldUpdate then
                begin

                    var UserGeneralCommentUpdated:=TJson.JsonToObject<TUserGeneralCommentUpdated>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserGeneralCommentUpdated.IsSucceeded;
                        CallResponse.LastMessage:=UserGeneralCommentUpdated.Error.ErrorDesc;
                        CallResponse.ErrorNumber:=UserGeneralCommentUpdated.Error.ErrorNum;
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
                        CallResponse.ErrorNumber:=UserGeneralCommentAdded.Error.ErrorNum;
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


function TComments.GetGeneralCommentAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string; var Output: TGeneralCommentFields): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var TempComments: TGeneralCommentFields;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl
            + 'generalcommentaries/'
            + CompanyCode.ToString()
            + '/'
            + CustNumber.ToString()
            + '/comment/'
            + UserAlias
            + '/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetGeneralCommentAwaited]: Executing GET ' + Restful.ClientBaseURL);

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
                    CallResponse.ErrorNumber:=UserGeneralComment.Error.ErrorNum;
                    ThreadFileLog.Log('[GetGeneralCommentAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserGeneralComment.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetGeneralCommentAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetGeneralCommentAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetGeneralCommentAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetGeneralCommentAwaited]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Output:=TempComments;
    Result:=CallResponse;

end;


function TComments.GetDailyCommentsAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    Result:=CallResponse;

end;


end.

