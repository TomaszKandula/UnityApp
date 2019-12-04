unit Async.Accounts;

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
    Data.Win.ADODB,
    Data.DB,
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


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
        function GetUserCompanyListAwaited(var UserCompanyList: TALists): TCallResponse;

        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetUserSortingOptionsAwaited(var SortingOptions: TStringList);

        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;

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
        function GetUserCompanyListAwaited(var UserCompanyList: TALists): TCallResponse;

        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetUserSortingOptionsAwaited(var SortingOptions: TStringList);

        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;

    end;


implementation


uses
    Handler.Database{Legacy}, //remove
    Handler.Sql{Legacy}, //remove
    Unity.Sql{Legacy}, //remove
    Unity.Helpers,
    Unity.Settings,
    Unity.StatusBar,
    Unity.Chars,
    Unity.Common,
    Sync.Documents,
    Bcrypt,
    REST.Types,
    REST.Json,
    Unity.DateTimeFormats,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.RestWrapper,
    Api.UserSessionAdd,
    Api.UserSessionAdded,
    Api.UserSessionChecked,
    Api.UserCompanyList,
    DbModel{Legacy}; //remove


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
        try

            UserSessionAdd.AliasName:=AliasName;
            Restful.CustomBody:=TJson.ObjectToJsonString(UserSessionAdd);
            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    var UserSessionAdded: TUserSessionAdded:=TJson.JsonToObject<TUserSessionAdded>(Restful.Content);

                    CallResponse.IsSucceeded:=UserSessionAdded.IsSucceeded;
                    CallResponse.LastMessage:=UserSessionAdded.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=UserSessionAdded.Error.ErrorNum;

                    UserSessionAdded.Free();
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

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserSessionChecked: TUserSessionChecked:=TJson.JsonToObject<TUserSessionChecked>(Restful.Content);

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

                UserSessionChecked.Free();
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

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAccounts.GetUserCompanyListAwaited(var UserCompanyList: TALists): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var TempUserCompanyList: TALists;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'accounts/' + SessionService.SessionData.UnityUserId.ToString() + '/companies/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetUserCompanyListAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var UserCompanyList: TUserCompanyList:=TJson.JsonToObject<TUserCompanyList>(Restful.Content);
                var ItemCount:=Length(UserCompanyList.Companies);
                SetLength(TempUserCompanyList, ItemCount, 2);

                for var iCNT:=0 to ItemCount - 1 do
                begin
                    TempUserCompanyList[iCNT, 0]:=UserCompanyList.Companies[iCNT];
                    TempUserCompanyList[iCNT, 1]:=UserCompanyList.IsSelected[iCNT].ToString();
                end;

                CallResponse.IsSucceeded:=True;
                UserCompanyList.Free();
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

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    UserCompanyList:=TempUserCompanyList;
    SetLength(UserCompanyList, Length(TempUserCompanyList), 2);
    TempUserCompanyList:=nil;

    Result:=CallResponse;

end;


procedure TAccounts.GetUserSortingOptionsAwaited(var SortingOptions: TStringList); // replace with rest
begin

    var TempList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            var StringGrid:=TStringGrid.Create(nil);
            try

                DataTables.StrSQL:='select ModeDesc from Customer.SortingOptions';
                DataTables.SqlToGrid(StringGrid, DataTables.ExecSQL, False, False);

                for var iCNT:=1{Skip header} to StringGrid.RowCount - 1 do
                    TempList.Add(StringGrid.Cells[1{ModeDesc}, iCNT]);

            finally
                DataTables.Free();
                StringGrid.Free();
            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        SortingOptions.AddStrings(TempList);

    finally
        TempList.Free();
    end;

end;


function TAccounts.SaveUserLogsAwaited(): TCallResponse; // replace with rest
begin

    var NewCallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                var Today: string:=FormatDateTime(TDateTimeFormats.DateTimeFormat, Now);

                DataTables.Columns.Add(TUnityEventLogs.UserAlias);
                DataTables.Columns.Add(TUnityEventLogs.DateTimeStamp);
                DataTables.Columns.Add(TUnityEventLogs.AppEventLog);
                DataTables.Columns.Add(TUnityEventLogs.AppName);
                DataTables.Values.Add(SessionService.SessionData.AliasName.ToUpper);
                DataTables.Values.Add(Today);
                DataTables.Values.Add(THelpers.LoadFileToStr(ThreadFileLog.LogFileName));
                DataTables.Values.Add('Unity Platform');
                DataTables.InsertInto(TUnityEventLogs.UnityEventLogs, True);

                NewCallResponse.IsSucceeded:=True;

            except
                on E: Exception do
                begin
                    NewCallResponse.IsSucceeded:=False;
                    NewCallResponse.LastMessage:='[SaveUserLogsAwaited]: ' + E.Message;
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewCallResponse;

end;


end.

