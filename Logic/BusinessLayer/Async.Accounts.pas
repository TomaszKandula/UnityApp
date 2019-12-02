unit Async.Accounts;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Unity.Records;


type


    IAccounts = interface(IInterface)
    ['{4BA4CF2E-B8BD-4029-B358-93D1A344DAF3}']

        /// <summary>
        /// Allow to initiate new user session by loggin user data in the database via Unity API.
        /// This user session entry is later used by other service that uses Active Directory.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function InitiateAwaited(SessionId: string; AliasName: string): TCallResponse;

        /// <summary>
        /// Allow to check if user has been validated by Active Directory. We relay on assigned session token.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckAwaited(SessionId: string): TCallResponse;

    end;


    TAccounts = class(TInterfacedObject, IAccounts)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Allow to initiate new user session by loggin user data in the database via Unity API.
        /// This user session entry is later used by other service that uses Active Directory.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function InitiateAwaited(SessionId: string; AliasName: string): TCallResponse;

        /// <summary>
        /// Allow to check if user has been validated by Active Directory. We relay on assigned session token.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckAwaited(SessionId: string): TCallResponse;

    end;


implementation


uses
    System.SysUtils,
    System.Classes,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.SessionService,
    Handler.Rest,
    Api.CheckSessionResponse,
    Api.NewSessionResponse,
    Api.PostCheckSession,
    Api.PostNewSession;


function TAccounts.InitiateAwaited(SessionId: string; AliasName: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/initiate/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;

        try

            var PostNewSession:=TPostNewSession.Create();
            PostNewSession.SessionToken:=SessionId;
            PostNewSession.AliasName:=AliasName;
            Restful.CustomBody:=TJson.ObjectToJsonString(PostNewSession);

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var NewSessionResponse: TNewSessionResponse:=TJson.JsonToObject<TNewSessionResponse>(Restful.Content);

                CallResponse.IsSucceeded:=NewSessionResponse.IsSucceeded;
                CallResponse.LastMessage:=NewSessionResponse.Error.ErrorDesc;
                CallResponse.ErrorNumber:=NewSessionResponse.Error.ErrorNum;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[InitiateAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[InitiateAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[InitiateAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[InitiateAwaited]: Cannot execute the request. Description: ' + E.Message;
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAccounts.CheckAwaited(SessionId: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/check/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;

        try

            var PostCheckSession:=TPostCheckSession.Create();
            PostCheckSession.SessionToken:=SessionId;
            Restful.CustomBody:=TJson.ObjectToJsonString(PostCheckSession);

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var CheckSessionResponse: TCheckSessionResponse:=TJson.JsonToObject<TCheckSessionResponse>(Restful.Content);

                CallResponse.IsSucceeded:=CheckSessionResponse.IsValidated;
                CallResponse.LastMessage:=CheckSessionResponse.Error.ErrorDesc;
                CallResponse.ErrorNumber:=CheckSessionResponse.Error.ErrorNum;

                if CallResponse.IsSucceeded then
                begin

                    SessionService.UpdateUserData(
                        CheckSessionResponse.UserId,
                        CheckSessionResponse.Department,
                        CheckSessionResponse.AliasName,
                        CheckSessionResponse.DisplayName,
                        CheckSessionResponse.EmailAddress
                    );

                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[CheckAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[CheckAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[CheckAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckAwaited]: Cannot execute the request. Description: ' + E.Message;
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


end.

