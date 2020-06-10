unit Async.Accounts;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
    Unity.Types,
    Unity.Records;


type


    IUsers = interface(IInterface)
    ['{4BA4CF2E-B8BD-4029-B358-93D1A344DAF3}']
        /// <summary>
        /// Allow to load async. list of company codes assigned to the current user.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetUserCompanyListAsync(Callback: TGetUserCompanyList);
        /// <summary>
        /// Allow to write async. user choice of comapny codes to load.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure SetUserCompanyListAsync(UserSelection: TList<string>; Callback: TSetUserCompanyList);
        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;
        /// <summary>
        /// Allows to get the user permissions. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetUserPermissionsAwaited(): TCallResponse;
    end;


    ISessions = interface(IInterface)
    ['{4BA4CF2E-B8BD-4029-B358-93D1A344DAF3}']
        /// <summary>
        //  Allow to request access token in exchange of client id and client secrets. It is necessary for further communication.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function RequestAccessTokenAwaited(var AccessToken: string): TCallResponse;
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
    end;


    IRatings = interface(IInterface)
    ['{4BA4CF2E-B8BD-4029-B358-93D1A344DAF3}']
        /// <summary>
        /// Allow to load async. user rating with optional comment. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure LoadRatingAsync(Callback: TLoadRating);
        /// <summary>
        /// Allow to write async. user rating with optional comment.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure SubmitRatingAsync(Rating: TRating; Callback: TSubmitRating);
        /// <summary>
        /// Allow to re-write async. user rating with optional comment.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure UpdateRatingAsync(Rating: TRating; Callback: TUpdateRating);
    end;


    /// <remarks>
    /// Use this base class to provide template for common functionality for Users, Sessions and Ratings.
    /// </remarks>
    TBaseAccounts = class(TInterfacedObject)
    public
        constructor Create();
        destructor  Destroy(); override;
    end;


    TUsers = class(TBaseAccounts, IUsers)
    public
        constructor Create();
        destructor  Destroy(); override;
        procedure   GetUserCompanyListAsync(Callback: TGetUserCompanyList);
        procedure   SetUserCompanyListAsync(UserSelection: TList<string>; Callback: TSetUserCompanyList);
        function    SaveUserLogsAwaited(): TCallResponse;
        function    GetUserPermissionsAwaited(): TCallResponse;
    end;


    TSessions = class(TBaseAccounts, ISessions)
    public
        constructor Create();
        destructor  Destroy(); override;
        function    RequestAccessTokenAwaited(var AccessToken: string): TCallResponse;
        function    InitiateSessionAwaited(SessionId: string; AliasName: string): TCallResponse;
        function    CheckSessionAwaited(SessionId: string): TCallResponse;
    end;


    TRatings = class(TBaseAccounts, IRatings)
    public
        constructor Create();
        destructor  Destroy(); override;
        procedure   LoadRatingAsync(Callback: TLoadRating);
        procedure   SubmitRatingAsync(Rating: TRating; Callback: TSubmitRating);
        procedure   UpdateRatingAsync(Rating: TRating; Callback: TUpdateRating);
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.Service,
    Api.UserSessionAdd,
    Api.UserSessionAdded,
    Api.UserSessionChecked,
    Api.UserCompanyList,
    Api.UserCompanySelection,
    Api.UserCompaniesUpdated,
    Api.UserSessionLogs,
    Api.UserSessionLogsSaved,
    Api.UserRating,
    Api.UserRatingAdd,
    Api.UserRatingAdded,
    Api.UserRatingUpdate,
    Api.UserRatingUpdated,
    Api.TokenGranted,
    Api.UserPermissions,
    Api.UserPermissionList;


constructor TBaseAccounts.Create();
begin
end;


destructor TBaseAccounts.Destroy();
begin
    inherited;
end;


constructor TUsers.Create();
begin
    inherited;
end;


destructor TUsers.Destroy();
begin
    inherited;
end;


procedure TUsers.GetUserCompanyListAsync(Callback: TGetUserCompanyList);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + Service.SessionData.UnityUserId.ToString()
            + '/companies/';

        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetUserCompanyListAsync]: Executing GET ' + Rest.ClientBaseURL);

        var UserCompanyList: TUserCompanyList;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                UserCompanyList:=TJson.JsonToObject<TUserCompanyList>(Rest.Content);
                Service.Logger.Log('[GetUserCompanyListAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    UserCompanyList.Error.ErrorDesc:='[GetUserCompanyListAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        UserCompanyList.Error.ErrorDesc:='[GetUserCompanyListAsync]: Invalid server response. Please contact IT Support.'
                    else
                        UserCompanyList.Error.ErrorDesc:='[GetUserCompanyListAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(UserCompanyList.Error.ErrorDesc);

            end;

        except on
            E: Exception do
            begin
                UserCompanyList.Error.ErrorDesc:='[GetUserCompanyListAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(UserCompanyList.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(UserCompanyList);
            if Assigned(UserCompanyList) then UserCompanyList.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TUsers.SetUserCompanyListAsync(UserSelection: TList<string>; Callback: TSetUserCompanyList);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + Service.SessionData.UnityUserId.ToString()
            + '/companies/';

        Rest.RequestMethod:=TRESTRequestMethod.rmPATCH;
        Service.Logger.Log('[SaveUserCompanyListAwaited]: Executing PATCH ' + Rest.ClientBaseURL);

        var UserCompanySelection:=TUserCompanySelection.Create();
        try
            UserCompanySelection.SelectedCoCodes:=UserSelection.ToArray();
            Rest.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
        finally
            UserCompanySelection.Free();
        end;

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var UserCompaniesUpdated:=TJson.JsonToObject<TUserCompaniesUpdated>(Rest.Content);
                try
                    CallResponse.IsSucceeded:=UserCompaniesUpdated.IsSucceeded;
                    Service.Logger.Log('[SaveUserCompanyListAwaited]: Returned status code is ' + Rest.StatusCode.ToString());
                finally
                    UserCompaniesUpdated.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SaveUserCompanyListAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TUsers.SaveUserLogsAwaited(): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + Service.SessionData.UnityUserId.ToString()
            + '/logs/';

        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[SaveUserLogsAwaited]: Executing POST ' + Rest.ClientBaseURL);

        var UserSessionLogs:=TUserSessionLogs.Create();
        try

            var StrEventLog:='Session signature: ' + Service.SessionId + '<br>' + THelpers.ListToString(Service.Logger.SessionEventLines, '<br>');

            UserSessionLogs.UserAlias  :=Service.SessionData.AliasName.ToUpper();
            UserSessionLogs.AppEventLog:=StrEventLog;
            UserSessionLogs.AppName    :='Unity Platform';

            Rest.CustomBody:=TJson.ObjectToJsonString(UserSessionLogs);
            try

                if (Rest.Execute) and (Rest.StatusCode = 200) then
                begin

                    var UserSessionLogsSaved:=TJson.JsonToObject<TUserSessionLogsSaved>(Rest.Content);
                    try
                        CallResponse.IsSucceeded:=UserSessionLogsSaved.IsSucceeded;
                        CallResponse.LastMessage:=UserSessionLogsSaved.Error.ErrorDesc;
                        CallResponse.ErrorCode  :=UserSessionLogsSaved.Error.ErrorCode;
                        Service.Logger.Log('[SaveUserLogsAwaited]: Returned status code is ' + Rest.StatusCode.ToString());
                        Service.Logger.Log('[SaveUserLogsAwaited]: Application shutdown.');
                    finally
                        UserSessionLogsSaved.Free();
                    end;

                end
                else
                begin

                    if not String.IsNullOrEmpty(Rest.ExecuteError) then
                        CallResponse.LastMessage:='[SaveUserLogsAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                    else
                        if String.IsNullOrEmpty(Rest.Content) then
                            CallResponse.LastMessage:='[SaveUserLogsAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[SaveUserLogsAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    Service.Logger.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[SaveUserLogsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    Service.Logger.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            UserSessionLogs.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TUsers.GetUserPermissionsAwaited(): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/permissions/'
            + Service.SessionData.UnityUserId.ToString();

        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetUserPermissionsAwaited]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var UserPermissionList:=TJson.JsonToObject<TUserPermissionList>(Rest.Content);
                try

                    Service.UpdateUserPermissions(UserPermissionList.UserPermissions);

                    CallResponse.IsSucceeded :=UserPermissionList.IsSucceeded;
                    CallResponse.ErrorCode   :=UserPermissionList.Error.ErrorCode;
                    CallResponse.LastMessage :=UserPermissionList.Error.ErrorDesc;
                    CallResponse.ReturnedCode:=Rest.StatusCode;

                    Service.Logger.Log('[GetUserPermissionsAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    UserPermissionList.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetUserPermissionsAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetUserPermissionsAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetUserPermissionsAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetUserPermissionsAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


constructor TSessions.Create();
begin
    inherited;
end;


destructor TSessions.Destroy();
begin
    inherited;
end;


function TSessions.RequestAccessTokenAwaited(var AccessToken: string): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var NewAccessToken: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
        Rest.AccessToken:=String.Empty;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_LOGIN_URI') + 'oauth/authorize/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[RequestAccessTokenAwaited]: Executing POST ' + Rest.ClientBaseURL);

        Rest.AddParameter('GrantType',    Service.Settings.GetStringValue('AUTHORIZATION', 'GRANT_TYPE'));
        Rest.AddParameter('ClientId',     Service.Settings.GetStringValue('AUTHORIZATION', 'CLIENT_ID'));
        Rest.AddParameter('ClientSecret', Service.Settings.GetStringValue('AUTHORIZATION', 'CLIENT_SECRET'));

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var TokenGranted:=TJson.JsonToObject<TTokenGranted>(Rest.Content);
                try
                    NewAccessToken:=TokenGranted.AccessToken;
                    CallResponse.IsSucceeded:=TokenGranted.IsSucceeded;
                    CallResponse.LastMessage:=TokenGranted.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=TokenGranted.Error.ErrorCode;
                    Service.Logger.Log('[RequestAccessTokenAwaited]: Returned status code is ' + Rest.StatusCode.ToString());
                finally
                    TokenGranted.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[RequestAccessTokenAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[RequestAccessTokenAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[RequestAccessTokenAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
                E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[RequestAccessTokenAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    AccessToken:=NewAccessToken;
    Result:=CallResponse;

end;


function TSessions.InitiateSessionAwaited(SessionId: string; AliasName: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'accounts/initiate/' + SessionId;
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[InitiateSessionAwaited]: Executing POST ' + Rest.ClientBaseURL);

        var UserSessionAdd:=TUserSessionAdd.Create();
        try
            UserSessionAdd.AliasName:=AliasName;
            Rest.CustomBody:=TJson.ObjectToJsonString(UserSessionAdd);
        finally
            UserSessionAdd.Free();
        end;

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var UserSessionAdded: TUserSessionAdded:=TJson.JsonToObject<TUserSessionAdded>(Rest.Content);
                try
                    CallResponse.IsSucceeded:=UserSessionAdded.IsSucceeded;
                    CallResponse.LastMessage:=UserSessionAdded.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=UserSessionAdded.Error.ErrorCode;
                    Service.Logger.Log('[InitiateSessionAwaited]: Returned status code is ' + Rest.StatusCode.ToString());
                finally
                    UserSessionAdded.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[InitiateSessionAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[InitiateSessionAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[InitiateSessionAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[InitiateSessionAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TSessions.CheckSessionAwaited(SessionId: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'accounts/check/' + SessionId;
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[CheckSessionAwaited]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var UserSessionChecked: TUserSessionChecked:=TJson.JsonToObject<TUserSessionChecked>(Rest.Content);
                try

                    CallResponse.IsSucceeded:=UserSessionChecked.IsValidated;
                    CallResponse.LastMessage:=UserSessionChecked.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=UserSessionChecked.Error.ErrorCode;

                    if CallResponse.IsSucceeded then
                    begin

                        Service.UpdateUserData(
                            UserSessionChecked.UserId,
                            UserSessionChecked.Department,
                            UserSessionChecked.AliasName,
                            UserSessionChecked.DisplayName,
                            UserSessionChecked.EmailAddress
                        );

                    end;

                    Service.Logger.Log('[CheckSessionAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    UserSessionChecked.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[CheckSessionAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[CheckSessionAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[CheckSessionAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckSessionAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


constructor TRatings.Create();
begin
    inherited;
end;


destructor TRatings.Destroy();
begin
    inherited;
end;


procedure TRatings.LoadRatingAsync(Callback: TLoadRating);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + Service.SessionData.UnityUserId.ToString()
            + '/rating/';

        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[LoadRatingAwaited]: Executing GET ' + Rest.ClientBaseURL);

        var UserRating: TUserRating;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                UserRating:=TJson.JsonToObject<TUserRating>(Rest.Content);
                Service.Logger.Log('[LoadRatingAwaited]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    UserRating.Error.ErrorDesc:='[LoadRatingAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        UserRating.Error.ErrorDesc:='[LoadRatingAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        UserRating.Error.ErrorDesc:='[LoadRatingAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(UserRating.Error.ErrorDesc);

            end;

        except on
            E: Exception do
            begin
                UserRating.Error.ErrorDesc:='[LoadRatingAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(UserRating.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(UserRating);
            if Assigned(UserRating) then UserRating.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TRatings.SubmitRatingAsync(Rating: TRating; Callback: TSubmitRating);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + Service.SessionData.UnityUserId.ToString()
            + '/rating/';

        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[SubmitRatingAsync]: Executing POST ' + Rest.ClientBaseURL);

        var UserRatingAdd:=TUserRatingAdd.Create();
        try
            UserRatingAdd.UserRating:=Rating.UserRating;
            UserRatingAdd.Comment:=Rating.UserComment;
            Rest.CustomBody:=TJson.ObjectToJsonString(UserRatingAdd);
        finally
            UserRatingAdd.Free();
        end;

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var UserRatingAdded:=TJson.JsonToObject<TUserRatingAdded>(Rest.Content);
                try

                    CallResponse.IsSucceeded :=UserRatingAdded.IsSucceeded;
                    CallResponse.ErrorCode   :=UserRatingAdded.Error.ErrorCode;
                    CallResponse.LastMessage :=UserRatingAdded.Error.ErrorDesc;
                    CallResponse.ReturnedCode:=Rest.StatusCode;

                    Service.Logger.Log('[SubmitRatingAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    UserRatingAdded.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[SubmitRatingAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[SubmitRatingAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SubmitRatingAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SubmitRatingAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


procedure TRatings.UpdateRatingAsync(Rating: TRating; Callback: TUpdateRating);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + Service.SessionData.UnityUserId.ToString()
            + '/rating/';

        Rest.RequestMethod:=TRESTRequestMethod.rmPATCH;
        Service.Logger.Log('[UpdateRatingAsync]: Executing PATCH ' + Rest.ClientBaseURL);

        var UserRatingUpdate:=TUserRatingUpdate.Create();
        try
            UserRatingUpdate.UserRating:=Rating.UserRating;
            UserRatingUpdate.Comment   :=Rating.UserComment;
            Rest.CustomBody         :=TJson.ObjectToJsonString(UserRatingUpdate);
        finally
            UserRatingUpdate.Free();
        end;

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var UserRatingUpdated:=TJson.JsonToObject<TUserRatingUpdated>(Rest.Content);
                try

                    CallResponse.IsSucceeded :=UserRatingUpdated.IsSucceeded;
                    CallResponse.ErrorCode   :=UserRatingUpdated.Error.ErrorCode;
                    CallResponse.LastMessage :=UserRatingUpdated.Error.ErrorDesc;
                    CallResponse.ReturnedCode:=Rest.StatusCode;

                    Service.Logger.Log('[UpdateRatingAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    UserRatingUpdated.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[UpdateRatingAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[UpdateRatingAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[UpdateRatingAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[UpdateRatingAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

