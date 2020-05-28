unit Async.Utilities;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Threading,
    System.Generics.Collections,
    Unity.Grid,
    Unity.Types,
    Unity.Records,
    Api.CustomerSnapshotEx;


type


    IUtilities = interface(IInterface)
    ['{0B054CF4-86F7-4770-957B-3026BE491B5A}']
        /// <summary>
        /// Allow to async. check provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);
        /// <summary>
        /// Allow to async. setup newly provided local administrator password that works only for given program installed on local machine.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);
        /// <summary>
        /// Allow to async. check latest available release version (hosted in the Software Centre). If the current software version is lower
        /// than available for download, then we will prompt the user and exit the application.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckReleaseAwaited(var AClientInfo: TClientInfo): TCallResponse;
        /// <summary>
        /// Allows to recalculate async. aging summary and risk class based on provided age grid.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure RecalcAgeViewSummaryAsync(Source: TStringGrid; RiskClassGroup: TRiskClassGroup; Callback: TRecalcAgeViewSummary);
        /// <summary>
        /// Allows to load async. all the BI reports details, so the user can select and open desired report in a separate view.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetBiReportsAsync(Callback: TGetBiReports);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and extend them.
    /// </remarks>
    TUtilities = class(TInterfacedObject, IUtilities)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword); virtual;
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword); virtual;
        function CheckReleaseAwaited(var AClientInfo: TClientInfo): TCallResponse; virtual;
        procedure RecalcAgeViewSummaryAsync(Source: TStringGrid; RiskClassGroup: TRiskClassGroup; Callback: TRecalcAgeViewSummary); virtual;
        procedure GetBiReportsAsync(Callback: TGetBiReports); virtual;
    end;


implementation


uses
    System.SysUtils,
    System.Classes,
    System.Win.ComObj,
    System.Variants,
    REST.Types,
    REST.Json,
    Unity.Enums,
    Unity.Constants,
    Unity.Helpers,
    Unity.Service,
    Api.ReturnClientInfo,
    Api.ReturnReportList,
    Api.ReportListFields,
    Bcrypt;


constructor TUtilities.Create();
begin
end;


destructor TUtilities.Destroy();
begin
    inherited;
end;


procedure TUtilities.CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var ReHashed: boolean;
            var Hash: string:=Service.Settings.GetStringValue(TConfigSections.PasswordSection, 'HASH', '');

            if Hash = '' then
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckGivenPassword]: Missing hash value, check settings file. Please contact IT Support.';
                Service.Logger.Log(CallResponse.LastMessage);
            end
            else
            begin

                CallResponse.IsSucceeded:=TBcrypt.CheckPassword(Password, Hash, ReHashed);

                if CallResponse.IsSucceeded then
                begin
                    CallResponse.LastMessage:='[CheckGivenPassword]: Administrator password has been validaded.';
                    Service.Logger.Log(CallResponse.LastMessage);
                end
                else
                begin
                    CallResponse.LastMessage:='[CheckGivenPassword]: Incorrect password, please re-type it and try again';
                    Service.Logger.Log(CallResponse.LastMessage);
                end;

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckGivenPassword]: Cannot execute. Error has been thrown: ' + E.Message;
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


procedure TUtilities.SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            if (CurrentPassword = String.Empty) or (NewPassword = String.Empty) then
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='Please provide current password and new password.';
                Service.Logger.Log('[SetNewPassword]: User have not provided current password/new password.');
            end
            else
            begin

                // -----------------------
                // Check current password.
                // -----------------------
                var ReHashed: boolean;
                var Hash: string:=Service.Settings.GetStringValue(TConfigSections.PasswordSection, 'HASH', '');

                if TBcrypt.CheckPassword(CurrentPassword, Hash, ReHashed) then
                begin

                    // ------------------------------
                    // Setup newly provided password.
                    // ------------------------------
                    var HashPasswd: string:=TBcrypt.HashPassword(NewPassword);

                    Service.Settings.SetStringValue(TConfigSections.PasswordSection, 'HASH', HashPasswd);
                    Service.Settings.Encode(TAppFiles.Configuration);

                    CallResponse.IsSucceeded:=True;
                    Service.Logger.Log('[SetNewPassword]: New administrator password has been setup.');

                end
                else
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='Incorrect password, please re-type it and try again.';
                    Service.Logger.Log('[SetNewPassword]: provided current password is incorrect.');
                end;

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SetNewPassword]: Cannot execute. Error has been thrown: ' + E.Message;
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


function TUtilities.CheckReleaseAwaited(var AClientInfo: TClientInfo): TCallResponse;
begin

    var LCallResponse: TCallResponse;
    var LClientInfo:   TClientInfo;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'application/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[CheckReleaseAwaited]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnClientInfo:=TJson.JsonToObject<TReturnClientInfo>(Rest.Content);
                try

                    LClientInfo.Version:=ReturnClientInfo.Version;
                    LClientInfo.Date   :=ReturnClientInfo.Date;
                    LClientInfo.Status :=ReturnClientInfo.Status;

                    LCallResponse.IsSucceeded:=ReturnClientInfo.IsSucceeded;
                    LCallResponse.ErrorCode  :=ReturnClientInfo.Error.ErrorCode;
                    LCallResponse.LastMessage:=ReturnClientInfo.Error.ErrorDesc;

                    Service.Logger.Log('[CheckReleaseAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnClientInfo.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    LCallResponse.LastMessage:='[CheckReleaseAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        LCallResponse.LastMessage:='[CheckReleaseAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        LCallResponse.LastMessage:='[CheckReleaseAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                LCallResponse.ReturnedCode:=Rest.StatusCode;
                LCallResponse.IsSucceeded:=False;
                Service.Logger.Log(LCallResponse.LastMessage);

            end;

        except
            on E: Exception do
            begin
                LCallResponse.IsSucceeded:=False;
                LCallResponse.LastMessage:='[CheckReleaseAwaited]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(LCallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    AClientInfo:=LClientInfo;
    Result:=LCallResponse;

end;


procedure TUtilities.RecalcAgeViewSummaryAsync(Source: TStringGrid; RiskClassGroup: TRiskClassGroup; Callback: TRecalcAgeViewSummary);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LCallResponse: TCallResponse;
        var PayLoad: TAgingPayLoad;
        try
            THelpers.ComputeAgeSummary(Source, PayLoad);
            THelpers.ComputeRiskClass(Source, PayLoad, RiskClassGroup);
            LCallResponse.IsSucceeded:=True;
        except
            on E: Exception do
            begin
                LCallResponse.IsSucceeded:=False;
                LCallResponse.LastMessage:='[RecalcAgeViewSummaryAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(LCallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(LCallResponse, PayLoad);
        end);

    end);

    NewTask.Start();

end;


procedure TUtilities.GetBiReportsAsync(Callback: TGetBiReports);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'reports/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetBiReportsAsync]: Executing GET ' + Rest.ClientBaseURL);

        var ReturnReportList: TReturnReportList;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                ReturnReportList:=TJson.JsonToObject<TReturnReportList>(Rest.Content);
                Service.Logger.Log('[GetBiReportsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    ReturnReportList.Error.ErrorDesc:='[GetBiReportsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        ReturnReportList.Error.ErrorDesc:='[GetBiReportsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        ReturnReportList.Error.ErrorDesc:='[GetBiReportsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(ReturnReportList.Error.ErrorDesc);

            end;

        except
            on E: Exception do
            begin
                ReturnReportList.Error.ErrorDesc:='[GetBiReportsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(ReturnReportList.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnReportList);
            if Assigned(ReturnReportList) then ReturnReportList.Free();
        end);

    end);

    NewTask.Start();

end;


end.

