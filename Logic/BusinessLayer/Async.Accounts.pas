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


    /// <summary>
    /// Callback signature for inserting new user rating.
    /// </summary>
    TSubmitRating = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for updating existing user rating.
    /// </summary>
    TUpdateRating = procedure(CallResponse: TCallResponse) of object;


    IAccounts = interface(IInterface)
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
        function SaveUserCompanyListAwaited(UserSelection: TList<string>): TCallResponse;
        /// <summary>
        /// Allow to load async. user rating with optional comment. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function LoadRatingAwaited(var Rating: TRating): TCallResponse;
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


    TAccounts = class(TInterfacedObject, IAccounts)
    {$TYPEINFO ON}
    public
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
        function SaveUserCompanyListAwaited(UserSelection: TList<string>): TCallResponse;
        /// <summary>
        /// Allow to load async. user rating with optional comment. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function LoadRatingAwaited(var Rating: TRating): TCallResponse;
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


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.Settings,
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
    Api.UserSessionLogsSaved,
    Api.UserRating,
    Api.UserRatingAdd,
    Api.UserRatingAdded,
    Api.UserRatingUpdate,
    Api.UserRatingUpdated,
    Api.TokenGranted;


function TAccounts.RequestAccessTokenAwaited(var AccessToken: string): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var NewAccessToken: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(String.Empty, TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_LOGIN_URI') + 'oauth/authorize/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[RequestAccessTokenAwaited]: Executing POST ' + Restful.ClientBaseURL);

        Restful.AddParameter('GrantType',    Settings.GetStringValue('AUTHORIZATION', 'GRANT_TYPE'));
        Restful.AddParameter('ClientId',     Settings.GetStringValue('AUTHORIZATION', 'CLIENT_ID'));
        Restful.AddParameter('ClientSecret', Settings.GetStringValue('AUTHORIZATION', 'CLIENT_SECRET'));

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var TokenGranted:=TJson.JsonToObject<TTokenGranted>(Restful.Content);
                try
                    NewAccessToken:=TokenGranted.AccessToken;
                    CallResponse.IsSucceeded:=TokenGranted.IsSucceeded;
                    CallResponse.LastMessage:=TokenGranted.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=TokenGranted.Error.ErrorNum;
                    ThreadFileLog.Log('[RequestAccessTokenAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                finally
                    TokenGranted.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[RequestAccessTokenAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[RequestAccessTokenAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[RequestAccessTokenAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
                E: Exception do
            begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[RequestAccessTokenAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    AccessToken:=NewAccessToken;
    Result:=CallResponse;

end;


function TAccounts.InitiateSessionAwaited(SessionId: string; AliasName: string): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'accounts/initiate/' + SessionId;
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[InitiateAwaited]: Executing POST ' + Restful.ClientBaseURL);

        var UserSessionAdd:=TUserSessionAdd.Create();
        try

            UserSessionAdd.AliasName:=AliasName;
            Restful.CustomBody:=TJson.ObjectToJsonString(UserSessionAdd);
            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    var UserSessionAdded: TUserSessionAdded:=TJson.JsonToObject<TUserSessionAdded>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserSessionAdded.IsSucceeded;
                        CallResponse.LastMessage:=UserSessionAdded.Error.ErrorDesc;
                        CallResponse.ErrorNumber:=UserSessionAdded.Error.ErrorNum;
                        ThreadFileLog.Log('[InitiateAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                    finally
                        UserSessionAdded.Free();
                    end;

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

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'accounts/check/' + SessionId;
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[CheckAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserSessionChecked: TUserSessionChecked:=TJson.JsonToObject<TUserSessionChecked>(Restful.Content);
                try

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

                finally
                    UserSessionChecked.Free();
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

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + SessionService.SessionData.UnityUserId.ToString()
            + '/companies/';

        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetUserCompanyListAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin


                var UserCompanyList:=TJson.JsonToObject<TUserCompanyList>(Restful.Content);
                try

                    var ItemCount:=Length(UserCompanyList.Companies);
                    SetLength(TempUserCompanyList, ItemCount, 2);

                    for var iCNT:=0 to ItemCount - 1 do
                    begin
                        TempUserCompanyList[iCNT, 0]:=UserCompanyList.Companies[iCNT];
                        TempUserCompanyList[iCNT, 1]:=UserCompanyList.IsSelected[iCNT].ToString();
                    end;

                    CallResponse.IsSucceeded:=True;
                    ThreadFileLog.Log('[GetUserCompanyListAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserCompanyList.Free();
                end;

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

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + SessionService.SessionData.UnityUserId.ToString()
            + '/logs/';

        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[SaveUserLogsAwaited]: Executing POST ' + Restful.ClientBaseURL);
        ThreadFileLog.Log('[SaveUserLogsAwaited]: Application shutdown.');

        var UserSessionLogs:=TUserSessionLogs.Create();
        try

            var StrEventLog:='Session signature: ' + SessionService.SessionId + '<br>' + THelpers.ListToString(ThreadFileLog.SessionEventLines, '<br>');

            UserSessionLogs.UserAlias  :=SessionService.SessionData.AliasName.ToUpper();
            UserSessionLogs.AppEventLog:=StrEventLog;
            UserSessionLogs.AppName    :='Unity Platform';

            Restful.CustomBody:=TJson.ObjectToJsonString(UserSessionLogs);
            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    var UserSessionLogsSaved:=TJson.JsonToObject<TUserSessionLogsSaved>(Restful.Content);
                    try
                        CallResponse.IsSucceeded:=UserSessionLogsSaved.IsSucceeded;
                        CallResponse.LastMessage:=UserSessionLogsSaved.Error.ErrorDesc;
                        CallResponse.ErrorNumber:=UserSessionLogsSaved.Error.ErrorNum;
                        ThreadFileLog.Log('[SaveUserLogsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                        ThreadFileLog.Log('[SaveUserLogsAwaited]: Application shutdown.');
                    finally
                        UserSessionLogsSaved.Free();
                    end;

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
            UserSessionLogs.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAccounts.SaveUserCompanyListAwaited(UserSelection: TList<string>): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + SessionService.SessionData.UnityUserId.ToString()
            + '/companies/';

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
                    try
                        CallResponse.IsSucceeded:=UserCompaniesUpdated.IsSucceeded;
                        ThreadFileLog.Log('[SaveUserCompanyListAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                    finally
                        UserCompaniesUpdated.Free();
                    end;

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


function TAccounts.LoadRatingAwaited(var Rating: TRating): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var TempRating: TRating;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + SessionService.SessionData.UnityUserId.ToString()
            + '/rating/';

        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[LoadRatingAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserRating:=TJson.JsonToObject<TUserRating>(Restful.Content);
                try

                    TempRating.UserRating:=UserRating.Rating;
                    TempRating.UserComment:=UserRating.Comment;

                    CallResponse.IsSucceeded :=UserRating.IsSucceeded;
                    CallResponse.ErrorNumber :=UserRating.Error.ErrorNum;
                    CallResponse.LastMessage :=UserRating.Error.ErrorDesc;
                    CallResponse.ReturnedCode:=Restful.StatusCode;

                    ThreadFileLog.Log('[LoadRatingAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserRating.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[LoadRatingAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[LoadRatingAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[LoadRatingAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[LoadRatingAwaited]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Rating:=TempRating;
    Result:=CallResponse;

end;


procedure TAccounts.SubmitRatingAsync(Rating: TRating; Callback: TSubmitRating);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + SessionService.SessionData.UnityUserId.ToString()
            + '/rating/';

        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[SubmitRatingAsync]: Executing POST ' + Restful.ClientBaseURL);

        var UserRatingAdd:=TUserRatingAdd.Create();
        try
            UserRatingAdd.UserRating:=Rating.UserRating;
            UserRatingAdd.Comment   :=Rating.UserComment;
            Restful.CustomBody      :=TJson.ObjectToJsonString(UserRatingAdd);
        finally
            UserRatingAdd.Free();
        end;

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserRatingAdded:=TJson.JsonToObject<TUserRatingAdded>(Restful.Content);
                try

                    CallResponse.IsSucceeded :=UserRatingAdded.IsSucceeded;
                    CallResponse.ErrorNumber :=UserRatingAdded.Error.ErrorNum;
                    CallResponse.LastMessage :=UserRatingAdded.Error.ErrorDesc;
                    CallResponse.ReturnedCode:=Restful.StatusCode;

                    ThreadFileLog.Log('[SubmitRatingAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserRatingAdded.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[SubmitRatingAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[SubmitRatingAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SubmitRatingAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SubmitRatingAsync]: Cannot execute the request. Description: ' + E.Message;
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


procedure TAccounts.UpdateRatingAsync(Rating: TRating; Callback: TUpdateRating);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'accounts/'
            + SessionService.SessionData.UnityUserId.ToString()
            + '/rating/';

        Restful.RequestMethod:=TRESTRequestMethod.rmPATCH;
        ThreadFileLog.Log('[UpdateRatingAsync]: Executing PATCH ' + Restful.ClientBaseURL);

        var UserRatingUpdate:=TUserRatingUpdate.Create();
        try
            UserRatingUpdate.UserRating:=Rating.UserRating;
            UserRatingUpdate.Comment   :=Rating.UserComment;
            Restful.CustomBody         :=TJson.ObjectToJsonString(UserRatingUpdate);
        finally
            UserRatingUpdate.Free();
        end;

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserRatingUpdated:=TJson.JsonToObject<TUserRatingUpdated>(Restful.Content);
                try

                    CallResponse.IsSucceeded :=UserRatingUpdated.IsSucceeded;
                    CallResponse.ErrorNumber :=UserRatingUpdated.Error.ErrorNum;
                    CallResponse.LastMessage :=UserRatingUpdated.Error.ErrorDesc;
                    CallResponse.ReturnedCode:=Restful.StatusCode;

                    ThreadFileLog.Log('[UpdateRatingAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    UserRatingUpdated.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[UpdateRatingAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[UpdateRatingAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[UpdateRatingAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[UpdateRatingAsync]: Cannot execute the request. Description: ' + E.Message;
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


end.

