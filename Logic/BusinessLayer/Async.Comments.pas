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
    Unity.Records,
    Api.UserGeneralComment;


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
        function GetGeneralCommentAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string; var Output: TGeneralCommentFields): TCallResponse;

        /// <summary>
        /// Allow to async. retrieve daily comments for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDailyCommentsAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string): TStringGrid;

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
        /// Allow to async. retrive general comment for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetGeneralCommentAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string; var Output: TGeneralCommentFields): TCallResponse;

        /// <summary>
        /// Allow to async. retrieve daily comments for given customer (via CUID number). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDailyCommentsAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string): TStringGrid;

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
    Api.ErrorHandler;


procedure TComments.EditDailyComment(PayLoad: TDailyCommentFields; Callback: TEditDailyComment = nil);
begin

end;


procedure TComments.EditGeneralComment(PayLoad: TGeneralCommentFields; Callback: TEditGeneralComment = nil);
begin

end;


function TComments.GetGeneralCommentAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string; var Output: TGeneralCommentFields): TCallResponse;
begin

    var CallResponse: TCallResponse;
//    var TempOutput:=TUserGeneralComment.Create();
//    try

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

                    Output:=TJson.JsonToObject<TUserGeneralComment>(Restful.Content);
                    CallResponse.IsSucceeded:=Output.IsSucceeded;
                    ThreadFileLog.Log('[GetGeneralCommentAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

//                    var UserGeneralComment: TUserGeneralComment:=TJson.JsonToObject<TUserGeneralComment>(Restful.Content);
//                    try
//
//                        if not Assigned(TempOutput.Error) then
//                            TempOutput.Error:=TErrorHandler.Create();
//
//                        TempOutput.FollowUp       :=UserGeneralComment.FollowUp;
//                        TempOutput.Free1          :=UserGeneralComment.Free1;
//                        TempOutput.Free2          :=UserGeneralComment.Free2;
//                        TempOutput.Free3          :=UserGeneralComment.Free3;
//                        TempOutput.UserComment    :=UserGeneralComment.UserComment;
//                        TempOutput.IsSucceeded    :=UserGeneralComment.IsSucceeded;
//                        TempOutput.Error.ErrorDesc:=UserGeneralComment.Error.ErrorDesc;
//                        TempOutput.Error.ErrorNum :=UserGeneralComment.Error.ErrorNum;
//
//                        CallResponse.IsSucceeded:=UserGeneralComment.IsSucceeded;
//                        ThreadFileLog.Log('[GetGeneralCommentAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
//
//                    finally
//                        UserGeneralComment.Free();
//                    end;

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

//        if not Assigned(Output.Error) then
//            Output.Error:=TErrorHandler.Create();
//
//        Output.FollowUp       :=TempOutput.FollowUp;
//        Output.Free1          :=TempOutput.Free1;
//        Output.Free2          :=TempOutput.Free2;
//        Output.Free3          :=TempOutput.Free3;
//        Output.UserComment    :=TempOutput.UserComment;
//        Output.IsSucceeded    :=TempOutput.IsSucceeded;
//        Output.Error.ErrorDesc:=TempOutput.Error.ErrorDesc;
//        Output.Error.ErrorNum :=TempOutput.Error.ErrorNum;

//    finally
//        TempOutput.Free();
        Result:=CallResponse;
//    end;

end;


function TComments.GetDailyCommentsAwaited(CompanyCode: integer; CustNumber: integer; UserAlias: string): TStringGrid;
begin

    Result:=nil;

end;


end.

