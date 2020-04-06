unit Async.Debtors;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
    Unity.Types,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


    IDebtors = interface(IInterface)
    ['{194FE2BE-386E-499E-93FB-0299DA53A70A}']
        /// <summary>
        /// Allow to check async. for latest generated snapshots. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function CheckSnapshotsAwaited(CheckDate: string; var ReceivedTime: string; var ReceivedStatus: string): TCallResponse;
        /// <summary>
        /// Allow to read async. current age report from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ScanSnapshotsAsync(SnapshotsUpdate: string; Callback: TScanSnapshots);
        /// <summary>
        /// Allow to read async. current age report from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetAgingReportAsync(SelectedCompanies: TList<string>; Callback: TGetAgingReport);
        /// <summary>
        /// Allow to read async. current age report from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadAgeViewAsync(SelectedCompanies: TList<string>; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView);
        /// <summary>
        /// Allow to map data between grids. It replaces the target column data for appropiate data in source grid based on given parameters.
        /// It writes data to target grid when synchronized with main thread. Thus, it is not necessary to lock (hold painting) visual component.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure MapTableAsync(Grid: TStringGrid; Source: TStringGrid; IsPrefixRequired: boolean;
            const ColTargetName: integer; const ColSourceId: integer; const ColTargetCoCode: integer;
            const ColSourceDbName: integer; const ColSourceDesc: integer);
        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustSortingOptionsAwaited(var SortingOptions: TStringList): TCallResponse;
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or extend them.
    /// </remarks>
    TDebtors = class(TInterfacedObject, IDebtors)
    strict private
        function FComparableDbName(InputString: string; IsPrefixRequired: boolean): string;
    public
        constructor Create();
        destructor Destroy(); override;
        function CheckSnapshotsAwaited(CheckDate: string; var ReceivedTime: string; var ReceivedStatus: string): TCallResponse; virtual;
        procedure ScanSnapshotsAsync(SnapshotsUpdate: string; Callback: TScanSnapshots); virtual;
        procedure GetAgingReportAsync(SelectedCompanies: TList<string>; Callback: TGetAgingReport); virtual;
        procedure ReadAgeViewAsync(SelectedCompanies: TList<string>; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView); virtual;
        procedure MapTableAsync(Grid: TStringGrid; Source: TStringGrid; IsPrefixRequired: boolean;
            const ColTargetName: integer; const ColSourceId: integer; const ColTargetCoCode: integer;
            const ColSourceDbName: integer; const ColSourceDesc: integer); virtual;
        function GetCustSortingOptionsAwaited(var SortingOptions: TStringList): TCallResponse; virtual;
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.Sorting,
    Unity.Service,
    Unity.Constants,
    Api.CustSortingOptions,
    Api.UserCustSnapshotList,
    Api.ReturnCustSnapshots,
    Api.CustomerSnapshot,
    Api.ReturnCustomerReport,
    Api.LatestAzureJobStatus;


constructor TDebtors.Create();
begin
end;


destructor TDebtors.Destroy();
begin
    inherited;
end;


function TDebtors.CheckSnapshotsAwaited(CheckDate: string; var ReceivedTime: string; var ReceivedStatus: string): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var GetDateTime:  string;
    var GetStatus:    string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'automation/azurejobs/' + CheckDate + '/';
        Rest.AddParameter('type','snapshots');
        Rest.AddParameter('mode','generation');

        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[CheckSnapshotsAwaited]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var LatestAzureJobStatus:=TJson.JsonToObject<TLatestAzureJobStatus>(Rest.Content);
                try

                    GetDateTime:=LatestAzureJobStatus.JobDateTime;
                    GetStatus:=LatestAzureJobStatus.StatusCode;
                    CallResponse.LastMessage:=LatestAzureJobStatus.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=LatestAzureJobStatus.Error.ErrorCode;

                    CallResponse.IsSucceeded:=True;
                    Service.Logger.Log('[CheckSnapshotsAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    LatestAzureJobStatus.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[CheckSnapshotsAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[CheckSnapshotsAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[CheckSnapshotsAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckSnapshotsAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    Result:=CallResponse;
    ReceivedTime  :=GetDateTime;
    ReceivedStatus:=GetStatus;

end;


procedure TDebtors.ScanSnapshotsAsync(SnapshotsUpdate: string; Callback: TScanSnapshots);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ReceivedStatus:   string;
        var ReceivedDateTime: string;
        var ReadDateTime:     string;
        var CallResponse:     TCallResponse;
        var CanGetAging:=False;
        try

            var SnapshotsResponse: TCallResponse;
            var CurrentDate:=DateToStr(Now);

            SnapshotsResponse:=CheckSnapshotsAwaited(CurrentDate, ReceivedDateTime, ReceivedStatus);
            ReadDateTime:=THelpers.FormatDateTime(ReceivedDateTime, TCalendar.DateTime);

            if ( StrToDateTime(SnapshotsUpdate) < StrToDateTime(ReadDateTime) )
                and ( ReceivedStatus = 'Completed' ) then CanGetAging:=True;

            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ScanSnapshotsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CanGetAging, ReadDateTime, CallResponse);
        end);

    end);

    NewTask.Start;

end;


procedure TDebtors.GetAgingReportAsync(SelectedCompanies: TList<string>; Callback: TGetAgingReport);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var Grid:=TStringGrid.Create(nil);

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'snapshots/customers/report/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[GetAgingReportAsync]: Executing POST ' + Rest.ClientBaseURL);

        var UserCustSnapshotList:=TUserCustSnapshotList.Create();
        try
            UserCustSnapshotList.SelectedCoCodes:=SelectedCompanies.ToArray();
            UserCustSnapshotList.SortMode:=String.Empty;
            Rest.CustomBody:=TJson.ObjectToJsonString(UserCustSnapshotList);
        finally
            UserCustSnapshotList.Free();
        end;

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnCustomerReport:=TJson.JsonToObject<TReturnCustomerReport>(Rest.Content);
                try

                    var RowCount:=Length(ReturnCustomerReport.CustomerSnapshot);
                    Grid.RowCount:=RowCount;
                    Grid.ColCount:=29;

                    Grid.Cells[0, 0]:='';
                    Grid.Cells[1, 0]:=TCustomerSnapshot._CustomerName;
                    Grid.Cells[2, 0]:=TCustomerSnapshot._CustomerNumber;
                    Grid.Cells[3, 0]:=TCustomerSnapshot._SourceDbName;
                    Grid.Cells[4, 0]:=TCustomerSnapshot._Overdue;
                    Grid.Cells[5, 0]:=TCustomerSnapshot._NotDue;
                    Grid.Cells[6, 0]:=TCustomerSnapshot._Range1;
                    Grid.Cells[7, 0]:=TCustomerSnapshot._Range2;
                    Grid.Cells[8, 0]:=TCustomerSnapshot._Range3;
                    Grid.Cells[9, 0]:=TCustomerSnapshot._Range4;
                    Grid.Cells[10,0]:=TCustomerSnapshot._Range5;
                    Grid.Cells[11,0]:=TCustomerSnapshot._Range6;
                    Grid.Cells[12,0]:=TCustomerSnapshot._Total;
                    Grid.Cells[13,0]:=TCustomerSnapshot._CreditLimit;
                    Grid.Cells[14,0]:=TCustomerSnapshot._CreditBalance;
                    Grid.Cells[15,0]:=TCustomerSnapshot._LedgerIso;
                    Grid.Cells[16,0]:=TCustomerSnapshot._FollowUp;
                    Grid.Cells[17,0]:=TCustomerSnapshot._Free1;
                    Grid.Cells[18,0]:=TCustomerSnapshot._Free2;
                    Grid.Cells[19,0]:=TCustomerSnapshot._Free3;
                    Grid.Cells[20,0]:=TCustomerSnapshot._PersonResponsible;
                    Grid.Cells[21,0]:=TCustomerSnapshot._SalesResponsible;
                    Grid.Cells[22,0]:=TCustomerSnapshot._CustomerGroup;
                    Grid.Cells[23,0]:=TCustomerSnapshot._AccountType;
                    Grid.Cells[24,0]:=TCustomerSnapshot._PaymentTerms;
                    Grid.Cells[25,0]:=TCustomerSnapshot._Inf4;
                    Grid.Cells[26,0]:=TCustomerSnapshot._Group3;
                    Grid.Cells[27,0]:=TCustomerSnapshot._GeneralComment;
                    Grid.Cells[28,0]:=TCustomerSnapshot._DailyComment;

                    for var Index:=1 to RowCount - 1 do
                    begin
                        Grid.Cells[1, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].CustomerName;
                        Grid.Cells[2, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].CustomerNumber.ToString();
                        Grid.Cells[3, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].SourceDbName;
                        Grid.Cells[4, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Overdue.ToString();
                        Grid.Cells[5, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].NotDue.ToString();
                        Grid.Cells[6, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Range1.ToString();
                        Grid.Cells[7, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Range2.ToString();
                        Grid.Cells[8, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Range3.ToString();
                        Grid.Cells[9, Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Range4.ToString();
                        Grid.Cells[10,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Range5.ToString();
                        Grid.Cells[11,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Range6.ToString();
                        Grid.Cells[12,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Total.ToString();
                        Grid.Cells[13,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].CreditLimit.ToString();
                        Grid.Cells[14,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].CreditBalance.ToString();
                        Grid.Cells[15,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].LedgerIso;
                        Grid.Cells[16,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].FollowUp;
                        Grid.Cells[17,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Free1;
                        Grid.Cells[18,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Free2;
                        Grid.Cells[19,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Free3;
                        Grid.Cells[20,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].PersonResponsible;
                        Grid.Cells[21,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].SalesResponsible;
                        Grid.Cells[22,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].CustomerGroup;
                        Grid.Cells[23,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].AccountType;
                        Grid.Cells[24,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].PaymentTerms.ToString();
                        Grid.Cells[25,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Inf4;
                        Grid.Cells[26,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].Group3.ToString();
                        Grid.Cells[27,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].GeneralComment;
                        Grid.Cells[28,Index]:=ReturnCustomerReport.CustomerSnapshot[Index].DailyComment;
                    end;

                    Service.Logger.Log('[GetAgingReportAsync]: Returned status code is ' + Rest.StatusCode.ToString());
                    Service.Logger.Log('[GetAgingReportAsync]: Rows processed: '
                        + ReturnCustomerReport.Meta.RowsAffected.ToString()
                        + ' within '
                        + ReturnCustomerReport.Meta.ProcessingTimeSpan
                        + ' seconds.'
                    );

                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    CallResponse.IsSucceeded:=True;

                finally
                    ReturnCustomerReport.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetAgingReportAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetAgingReportAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetAgingReportAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetAgingReportAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(Grid, CallResponse);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TDebtors.ReadAgeViewAsync(SelectedCompanies: TList<string>; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var PayLoad: TAgingPayLoad;
        var CallResponse: TCallResponse;
        var Grid:=TStringGrid.Create(nil);

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'snapshots/customers/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[ReadAgeViewAsync]: Executing POST ' + Rest.ClientBaseURL);

        var UserCustSnapshotList:=TUserCustSnapshotList.Create();
        try
            UserCustSnapshotList.SelectedCoCodes:=SelectedCompanies.ToArray();
            UserCustSnapshotList.SortMode:=SortMode;
            Rest.CustomBody:=TJson.ObjectToJsonString(UserCustSnapshotList);
        finally
            UserCustSnapshotList.Free();
        end;

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnCustSnapshots:=TJson.JsonToObject<TReturnCustSnapshots>(Rest.Content);
                try

                    var RowCount:=Length(ReturnCustSnapshots.SourceDbName);
                    Grid.RowCount:=RowCount;
                    Grid.ColCount:=28;

                    Grid.Cells[0, 0]:='';
                    Grid.Cells[1, 0]:=ReturnCustSnapshots._CustomerName;
                    Grid.Cells[2, 0]:=ReturnCustSnapshots._CustomerNumber;
                    Grid.Cells[3, 0]:=ReturnCustSnapshots._FollowUp;
                    Grid.Cells[4, 0]:=ReturnCustSnapshots._Overdue;
                    Grid.Cells[5, 0]:=ReturnCustSnapshots._NotDue;
                    Grid.Cells[6, 0]:=ReturnCustSnapshots._Range1;
                    Grid.Cells[7, 0]:=ReturnCustSnapshots._Range2;
                    Grid.Cells[8, 0]:=ReturnCustSnapshots._Range3;
                    Grid.Cells[9, 0]:=ReturnCustSnapshots._Range4;
                    Grid.Cells[10,0]:=ReturnCustSnapshots._Range5;
                    Grid.Cells[11,0]:=ReturnCustSnapshots._Range6;
                    Grid.Cells[12,0]:=ReturnCustSnapshots._Total;
                    Grid.Cells[13,0]:=ReturnCustSnapshots._SourceDbName;
                    Grid.Cells[14,0]:=ReturnCustSnapshots._Free1;
                    Grid.Cells[15,0]:=ReturnCustSnapshots._PersonResponsible;
                    Grid.Cells[16,0]:=ReturnCustSnapshots._SalesResponsible;
                    Grid.Cells[17,0]:=ReturnCustSnapshots._PaymentTerms;
                    Grid.Cells[18,0]:=ReturnCustSnapshots._Free2;
                    Grid.Cells[19,0]:=ReturnCustSnapshots._Free3;
                    Grid.Cells[20,0]:=ReturnCustSnapshots._CreditLimit;
                    Grid.Cells[21,0]:=ReturnCustSnapshots._CreditBalance;
                    Grid.Cells[22,0]:='Risk Class'; // Autogenerated, setup only header
                    Grid.Cells[23,0]:=ReturnCustSnapshots._LedgerIso;
                    Grid.Cells[24,0]:=ReturnCustSnapshots._CustomerGroup;
                    Grid.Cells[25,0]:=ReturnCustSnapshots._AccountType;
                    Grid.Cells[26,0]:=ReturnCustSnapshots._Inf4;
                    Grid.Cells[27,0]:=ReturnCustSnapshots._Group3;

                    for var Index:=1 to RowCount do
                    begin
                        Grid.Cells[1, Index]:=ReturnCustSnapshots.CustomerName[Index - 1];
                        Grid.Cells[2, Index]:=ReturnCustSnapshots.CustomerNumber[Index - 1].ToString();
                        Grid.Cells[3, Index]:=ReturnCustSnapshots.FollowUp[Index - 1];
                        Grid.Cells[4, Index]:=ReturnCustSnapshots.Overdue[Index - 1].ToString();
                        Grid.Cells[5, Index]:=ReturnCustSnapshots.NotDue[Index - 1].ToString();
                        Grid.Cells[6, Index]:=ReturnCustSnapshots.Range1[Index - 1].ToString();
                        Grid.Cells[7, Index]:=ReturnCustSnapshots.Range2[Index - 1].ToString();
                        Grid.Cells[8, Index]:=ReturnCustSnapshots.Range3[Index - 1].ToString();
                        Grid.Cells[9, Index]:=ReturnCustSnapshots.Range4[Index - 1].ToString();
                        Grid.Cells[10,Index]:=ReturnCustSnapshots.Range5[Index - 1].ToString();
                        Grid.Cells[11,Index]:=ReturnCustSnapshots.Range6[Index - 1].ToString();
                        Grid.Cells[12,Index]:=ReturnCustSnapshots.Total[Index - 1].ToString();
                        Grid.Cells[13,Index]:=ReturnCustSnapshots.SourceDbName[Index - 1];
                        Grid.Cells[14,Index]:=ReturnCustSnapshots.Free1[Index - 1];
                        Grid.Cells[15,Index]:=ReturnCustSnapshots.PersonResponsible[Index - 1];
                        Grid.Cells[16,Index]:=ReturnCustSnapshots.SalesResponsible[Index - 1];
                        Grid.Cells[17,Index]:=ReturnCustSnapshots.PaymentTerms[Index - 1].ToString();
                        Grid.Cells[18,Index]:=ReturnCustSnapshots.Free2[Index - 1];
                        Grid.Cells[19,Index]:=ReturnCustSnapshots.Free3[Index - 1];
                        Grid.Cells[20,Index]:=ReturnCustSnapshots.CreditLimit[Index - 1].ToString();
                        Grid.Cells[21,Index]:=ReturnCustSnapshots.CreditBalance[Index - 1].ToString();
                        Grid.Cells[23,Index]:=ReturnCustSnapshots.LedgerIso[Index - 1];
                        Grid.Cells[24,Index]:=ReturnCustSnapshots.CustomerGroup[Index - 1];
                        Grid.Cells[25,Index]:=ReturnCustSnapshots.AccountType[Index - 1];
                        Grid.Cells[26,Index]:=ReturnCustSnapshots.Inf4[Index - 1];
                        Grid.Cells[27,Index]:=ReturnCustSnapshots.Group3[Index - 1];
                    end;

                    PayLoad.AgeDate:=ReturnCustSnapshots.AgeDate;

                    THelpers.ComputeAgeSummary(Grid, PayLoad);
                    Service.Logger.Log('[ReadAgeViewAsync]: Aging summary has been calculated.');

                    THelpers.ComputeRiskClass(Grid, PayLoad, RiskClassGroup);
                    Service.Logger.Log('[ReadAgeViewAsync]: Risk class data has been calculated.');

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[ReadAgeViewAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnCustSnapshots.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[ReadAgeViewAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[ReadAgeViewAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[ReadAgeViewAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ReadAgeViewAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(Grid, PayLoad, CallResponse);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TDebtors.MapTableAsync(Grid: TStringGrid; Source: TStringGrid; IsPrefixRequired: boolean;
    const ColTargetName: integer; const ColSourceId: integer; const ColTargetCoCode: integer;
    const ColSourceDbName: integer; const ColSourceDesc: integer);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        for var GridIndex:=1 to Grid.RowCount - 1 do
        begin

            var TargetName:=Grid.Cells[ColTargetName, GridIndex];
            var TargetCoCode:=Grid.Cells[ColTargetCoCode, GridIndex];

            for var SourceIndex:=1 to Source.RowCount - 1 do
            begin

                var SourceName:=Source.Cells[ColSourceId, SourceIndex];
                var SourceDbName:=Source.Cells[ColSourceDbName, SourceIndex];

                if (TargetName = SourceName) and (FComparableDbName(TargetCoCode, IsPrefixRequired) = SourceDbName)
                then
                begin

                    TThread.Synchronize(nil, procedure
                    begin
                        Grid.Cells[ColTargetName, GridIndex]:=Source.Cells[ColSourceDesc, SourceIndex];
                    end);

                    Break;

                end;

            end;

        end;

    end);

    NewTask.Start();

end;


function TDebtors.GetCustSortingOptionsAwaited(var SortingOptions: TStringList): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var TempList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var Rest:=Service.InvokeRest();
			Rest.AccessToken:=Service.AccessToken;
            Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

            Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'snapshots/options/';
            Rest.RequestMethod:=TRESTRequestMethod.rmGET;
            Service.Logger.Log('[GetUserSortingOptionsAwaited]: Executing GET ' + Rest.ClientBaseURL);

            try

                if (Rest.Execute) and (Rest.StatusCode = 200) then
                begin

                    var CustSortingOptions: TCustSortingOptions:=TJson.JsonToObject<TCustSortingOptions>(Rest.Content);

                    for var Index:=0 to Length(CustSortingOptions.SortingOptions) - 1 do
                        TempList.Add(CustSortingOptions.SortingOptions[Index]);

                    CallResponse.IsSucceeded:=True;
                    CustSortingOptions.Free();
                    Service.Logger.Log('[GetUserSortingOptionsAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                end
                else
                begin

                    if not String.IsNullOrEmpty(Rest.ExecuteError) then
                        CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                    else
                        if String.IsNullOrEmpty(Rest.Content) then
                            CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    Service.Logger.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    Service.Logger.Log(CallResponse.LastMessage);
                end;

            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        SortingOptions.AddStrings(TempList);
        Result:=CallResponse;

    finally
        TempList.Free();
    end;

end;


function TDebtors.FComparableDbName(InputString: string; IsPrefixRequired: boolean): string;
begin

    case IsPrefixRequired of
        True:  Result:=THelpers.GetSourceDBName(InputString, 'F');
        False: Result:=InputString;
    end;

end;


end.

