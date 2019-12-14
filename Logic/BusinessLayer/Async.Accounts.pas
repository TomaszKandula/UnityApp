unit Async.Accounts;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
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
        function InitiateSessionAwaited(SessionId: string; AliasName: string): TCallResponse;

        /// <summary>
        /// Allow to check if user has been validated by Active Directory. We relay on assigned session token.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckSessionAwaited(SessionId: string): TCallResponse;

        /// <summary>
        /// Allow to load async. list of company codes assigned to the current user. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetUserCompanyListAwaited(var CompanyList: TArray<TArray<string>>): TCallResponse;

        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;

        /// <summary>
        /// Allow to write async. user choice of comapny codes to load. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserCompanyListAwaited(UserSelection: TList<integer>): TCallResponse;

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
        function InitiateSessionAwaited(SessionId: string; AliasName: string): TCallResponse;

        /// <summary>
        /// Allow to check if user has been validated by Active Directory. We relay on assigned session token.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckSessionAwaited(SessionId: string): TCallResponse;

        /// <summary>
        /// Allow to load async. list of company codes assigned to the current user. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetUserCompanyListAwaited(var CompanyList: TArray<TArray<string>>): TCallResponse;

        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;

        /// <summary>
        /// Allow to write async. user choice of comapny codes to load. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserCompanyListAwaited(UserSelection: TList<integer>): TCallResponse;

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
    Api.UserSessionAdd,
    Api.UserSessionAdded,
    Api.UserSessionChecked,
    Api.UserCompanyList,
    Api.UserCompanySelection,
    Api.UserCompaniesUpdated,
    Api.UserSessionLogs,
    Api.UserSessionLogsSaved;


function TAccounts.InitiateSessionAwaited(SessionId: string; AliasName: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/initiate/' + SessionId;
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[InitiateAwaited]: Executing POST ' + Restful.ClientBaseURL);

        var UserSessionAdd:=TUserSessionAdd.Create();
        var UserSessionAdded: TUserSessionAdded;
        try

            UserSessionAdd.AliasName:=AliasName;
            Restful.CustomBody:=TJson.ObjectToJsonString(UserSessionAdd);
            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    UserSessionAdded:=TJson.JsonToObject<TUserSessionAdded>(Restful.Content);

                    CallResponse.IsSucceeded:=UserSessionAdded.IsSucceeded;
                    CallResponse.LastMessage:=UserSessionAdded.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=UserSessionAdded.Error.ErrorNum;

                    ThreadFileLog.Log('[InitiateAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

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
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[InitiateAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            UserSessionAdded.Free();
            UserSessionAdd.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAccounts.CheckSessionAwaited(SessionId: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/check/' + SessionId;
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[CheckAwaited]: Executing GET ' + Restful.ClientBaseURL);

        var UserSessionChecked: TUserSessionChecked;
        try

            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    UserSessionChecked:=TJson.JsonToObject<TUserSessionChecked>(Restful.Content);

                    CallResponse.IsSucceeded:=UserSessionChecked.IsValidated;
                    CallResponse.LastMessage:=UserSessionChecked.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=UserSessionChecked.Error.ErrorNum;

                    if CallResponse.IsSucceeded then
                    begin

                        SessionService.UpdateUserData(
                            UserSessionChecked.UserId,
                            UserSessionChecked.Department,
                            UserSessionChecked.AliasName,
                            UserSessionChecked.DisplayName,
                            UserSessionChecked.EmailAddress
                        );

                    end;

                    ThreadFileLog.Log('[CheckAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

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
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[CheckAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            UserSessionChecked.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAccounts.GetUserCompanyListAwaited(var CompanyList: TArray<TArray<string>>): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var TempUserCompanyList: TArray<TArray<string>>;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/' + SessionService.SessionData.UnityUserId.ToString() + '/companies/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetUserCompanyListAwaited]: Executing GET ' + Restful.ClientBaseURL);

        var UserCompanyList: TUserCompanyList;
        try

            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    UserCompanyList:=TJson.JsonToObject<TUserCompanyList>(Restful.Content);
                    var ItemCount:=Length(UserCompanyList.Companies);
                    SetLength(TempUserCompanyList, ItemCount, 2);

                    for var iCNT:=0 to ItemCount - 1 do
                    begin
                        TempUserCompanyList[iCNT, 0]:=UserCompanyList.Companies[iCNT];
                        TempUserCompanyList[iCNT, 1]:=UserCompanyList.IsSelected[iCNT].ToString();
                    end;

                    CallResponse.IsSucceeded:=True;
                    ThreadFileLog.Log('[GetUserCompanyListAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                end
                else
                begin

                    if not String.IsNullOrEmpty(Restful.ExecuteError) then
                        CallResponse.LastMessage:='[GetUserCompanyListAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                    else
                        if String.IsNullOrEmpty(Restful.Content) then
                            CallResponse.LastMessage:='[GetUserCompanyListAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[GetUserCompanyListAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetUserCompanyListAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            UserCompanyList.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    TArrayUtils<TArray<string>>.Move(TempUserCompanyList, CompanyList);
    Result:=CallResponse;

end;


function TAccounts.SaveUserLogsAwaited(): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/' + SessionService.SessionData.UnityUserId.ToString() + '/logs/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[SaveUserLogsAwaited]: Executing POST ' + Restful.ClientBaseURL);
        ThreadFileLog.Log('[SaveUserLogsAwaited]: Application shutdown.');

        var UserSessionLogs:=TUserSessionLogs.Create();
        var UserSessionLogsSaved: TUserSessionLogsSaved;
        try

            var StrEventLog:='Session signature: ' + SessionService.SessionId + '<br>' + THelpers.ListToString(ThreadFileLog.SessionEventLines, '<br>');

            UserSessionLogs.UserAlias  :=SessionService.SessionData.AliasName.ToUpper();
            UserSessionLogs.AppEventLog:=StrEventLog;
            UserSessionLogs.AppName    :='Unity Platform';

            Restful.CustomBody:=TJson.ObjectToJsonString(UserSessionLogs);
            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    UserSessionLogsSaved:=TJson.JsonToObject<TUserSessionLogsSaved>(Restful.Content);

                    CallResponse.IsSucceeded:=UserSessionLogsSaved.IsSucceeded;
                    CallResponse.LastMessage:=UserSessionLogsSaved.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=UserSessionLogsSaved.Error.ErrorNum;

                    ThreadFileLog.Log('[SaveUserLogsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                    ThreadFileLog.Log('[SaveUserLogsAwaited]: Application shutdown.');

                end
                else
                begin

                    if not String.IsNullOrEmpty(Restful.ExecuteError) then
                        CallResponse.LastMessage:='[SaveUserLogsAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                    else
                        if String.IsNullOrEmpty(Restful.Content) then
                            CallResponse.LastMessage:='[SaveUserLogsAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[SaveUserLogsAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[SaveUserLogsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            UserSessionLogsSaved.Free();
            UserSessionLogs.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAccounts.SaveUserCompanyListAwaited(UserSelection: TList<integer>): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/' + SessionService.SessionData.UnityUserId.ToString() + '/companies/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPATCH;
        ThreadFileLog.Log('[SaveUserCompanyListAwaited]: Executing PATCH ' + Restful.ClientBaseURL);

        var UserCompanySelection:=TUserCompanySelection.Create();
        try

            UserCompanySelection.SelectedCoCodes:=UserSelection.ToArray();
            Restful.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    var UserCompaniesUpdated:=TJson.JsonToObject<TUserCompaniesUpdated>(Restful.Content);

                    CallResponse.IsSucceeded:=UserCompaniesUpdated.IsSucceeded;
                    UserCompaniesUpdated.Free();
                    ThreadFileLog.Log('[SaveUserCompanyListAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                end
                else
                begin

                    if not String.IsNullOrEmpty(Restful.ExecuteError) then
                        CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                    else
                        if String.IsNullOrEmpty(Restful.Content) then
                            CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            UserCompanySelection.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


end.

