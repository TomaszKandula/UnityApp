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
        procedure SetDefaultHeader(var SourceGrid: TStringGrid);
    public
        constructor Create();
        destructor Destroy(); override;
        function CheckSnapshotsAwaited(CheckDate: string; var ReceivedTime: string; var ReceivedStatus: string): TCallResponse; virtual;
        procedure ScanSnapshotsAsync(SnapshotsUpdate: string; Callback: TScanSnapshots); virtual;
        procedure GetAgingReportAsync(SelectedCompanies: TList<string>; Callback: TGetAgingReport); virtual;
        procedure ReadAgeViewAsync(SelectedCompanies: TList<string>; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView); virtual;
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
    Api.CustomerSnapshot,
    Api.CustomerSnapshotEx,
    Api.ReturnCustomerSnapshots,
    Api.ReturnCustomerReport,
    Api.LatestAzureJobStatus,
    Layout.AgeViewModel;


constructor TDebtors.Create();
begin
end;


destructor TDebtors.Destroy();
begin
    inherited;
end;


procedure TDebtors.SetDefaultHeader(var SourceGrid: TStringGrid);
begin
    SourceGrid.Cells[0, 0]:='';
    SourceGrid.Cells[1, 0]:=TCustomerSnapshot._CustomerName;
    SourceGrid.Cells[2, 0]:=TCustomerSnapshot._CustomerNumber;
    SourceGrid.Cells[3, 0]:=TCustomerSnapshot._FollowUp;
    SourceGrid.Cells[4, 0]:=TCustomerSnapshot._Overdue;
    SourceGrid.Cells[5, 0]:=TCustomerSnapshot._NotDue;
    SourceGrid.Cells[6, 0]:=TCustomerSnapshot._Range1;
    SourceGrid.Cells[7, 0]:=TCustomerSnapshot._Range2;
    SourceGrid.Cells[8, 0]:=TCustomerSnapshot._Range3;
    SourceGrid.Cells[9, 0]:=TCustomerSnapshot._Range4;
    SourceGrid.Cells[10,0]:=TCustomerSnapshot._Range5;
    SourceGrid.Cells[11,0]:=TCustomerSnapshot._Range6;
    SourceGrid.Cells[12,0]:=TCustomerSnapshot._Total;
    SourceGrid.Cells[13,0]:=TCustomerSnapshot._SourceDbName;
    SourceGrid.Cells[14,0]:=TCustomerSnapshot._Free1;
    SourceGrid.Cells[15,0]:=TCustomerSnapshot._PersonResponsible;
    SourceGrid.Cells[16,0]:=TCustomerSnapshot._SalesResponsible;
    SourceGrid.Cells[17,0]:=TCustomerSnapshot._PaymentTerms;
    SourceGrid.Cells[18,0]:=TCustomerSnapshot._Free2;
    SourceGrid.Cells[19,0]:=TCustomerSnapshot._Free3;
    SourceGrid.Cells[20,0]:=TCustomerSnapshot._CreditLimit;
    SourceGrid.Cells[21,0]:=TCustomerSnapshot._CreditBalance;
    SourceGrid.Cells[22,0]:='Risk Class'; // Autogenerated, setup only header
    SourceGrid.Cells[23,0]:=TCustomerSnapshot._LedgerIso;
    SourceGrid.Cells[24,0]:=TCustomerSnapshot._CustomerGroup;
    SourceGrid.Cells[25,0]:=TCustomerSnapshot._AccountType;
    SourceGrid.Cells[26,0]:=TCustomerSnapshot._Inf4;
    SourceGrid.Cells[27,0]:=TCustomerSnapshot._Group3;
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

                    var RowCount:=Length(ReturnCustomerReport.CustomerSnapshotEx);
                    Grid.RowCount:=RowCount + 1; // Add header
                    Grid.ColCount:=29;

                    Grid.Cells[0, 0]:='';
                    Grid.Cells[1, 0]:=TCustomerSnapshotEx._CustomerName;
                    Grid.Cells[2, 0]:=TCustomerSnapshotEx._CustomerNumber;
                    Grid.Cells[3, 0]:=TCustomerSnapshotEx._SourceDbName;
                    Grid.Cells[4, 0]:=TCustomerSnapshotEx._Overdue;
                    Grid.Cells[5, 0]:=TCustomerSnapshotEx._NotDue;
                    Grid.Cells[6, 0]:=TCustomerSnapshotEx._Range1;
                    Grid.Cells[7, 0]:=TCustomerSnapshotEx._Range2;
                    Grid.Cells[8, 0]:=TCustomerSnapshotEx._Range3;
                    Grid.Cells[9, 0]:=TCustomerSnapshotEx._Range4;
                    Grid.Cells[10,0]:=TCustomerSnapshotEx._Range5;
                    Grid.Cells[11,0]:=TCustomerSnapshotEx._Range6;
                    Grid.Cells[12,0]:=TCustomerSnapshotEx._Total;
                    Grid.Cells[13,0]:=TCustomerSnapshotEx._CreditLimit;
                    Grid.Cells[14,0]:=TCustomerSnapshotEx._CreditBalance;
                    Grid.Cells[15,0]:=TCustomerSnapshotEx._LedgerIso;
                    Grid.Cells[16,0]:=TCustomerSnapshotEx._FollowUp;
                    Grid.Cells[17,0]:=TCustomerSnapshotEx._Free1;
                    Grid.Cells[18,0]:=TCustomerSnapshotEx._Free2;
                    Grid.Cells[19,0]:=TCustomerSnapshotEx._Free3;
                    Grid.Cells[20,0]:=TCustomerSnapshotEx._PersonResponsible;
                    Grid.Cells[21,0]:=TCustomerSnapshotEx._SalesResponsible;
                    Grid.Cells[22,0]:=TCustomerSnapshotEx._CustomerGroup;
                    Grid.Cells[23,0]:=TCustomerSnapshotEx._AccountType;
                    Grid.Cells[24,0]:=TCustomerSnapshotEx._PaymentTerms;
                    Grid.Cells[25,0]:=TCustomerSnapshotEx._Inf4;
                    Grid.Cells[26,0]:=TCustomerSnapshotEx._Group3;
                    Grid.Cells[27,0]:=TCustomerSnapshotEx._GeneralComment;
                    Grid.Cells[28,0]:=TCustomerSnapshotEx._DailyComment;

                    for var Index:=1 to RowCount do
                    begin
                        Grid.Cells[1, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].CustomerName;
                        Grid.Cells[2, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].CustomerNumber.ToString();
                        Grid.Cells[3, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].SourceDbName;
                        Grid.Cells[4, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Overdue.ToString();
                        Grid.Cells[5, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].NotDue.ToString();
                        Grid.Cells[6, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Range1.ToString();
                        Grid.Cells[7, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Range2.ToString();
                        Grid.Cells[8, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Range3.ToString();
                        Grid.Cells[9, Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Range4.ToString();
                        Grid.Cells[10,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Range5.ToString();
                        Grid.Cells[11,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Range6.ToString();
                        Grid.Cells[12,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Total.ToString();
                        Grid.Cells[13,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].CreditLimit.ToString();
                        Grid.Cells[14,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].CreditBalance.ToString();
                        Grid.Cells[15,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].LedgerIso;
                        Grid.Cells[16,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].FollowUp;
                        Grid.Cells[17,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Free1;
                        Grid.Cells[18,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Free2;
                        Grid.Cells[19,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Free3;
                        Grid.Cells[20,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].PersonResponsible;
                        Grid.Cells[21,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].SalesResponsible;
                        Grid.Cells[22,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].CustomerGroup;
                        Grid.Cells[23,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].AccountType;
                        Grid.Cells[24,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].PaymentTerms;
                        Grid.Cells[25,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Inf4;
                        Grid.Cells[26,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].Group3.ToString();
                        Grid.Cells[27,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].GeneralComment;
                        Grid.Cells[28,Index]:=ReturnCustomerReport.CustomerSnapshotEx[Index - 1].DailyComment;
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

                var ReturnCustomerSnapshots:=TJson.JsonToObject<TReturnCustomerSnapshots>(Rest.Content);
                try

                    var RowCount:=Length(ReturnCustomerSnapshots.CustomerSnapshot);
                    Grid.RowCount:=RowCount + 1; // Add header
                    Grid.ColCount:=28;

                    if not FileExists(Service.Settings.DirLayouts + TCommon.AgeViewLayout) then
                    begin
                        SetDefaultHeader(Grid);
                        Grid.SetColWidth(10, 20, 400);
                    end
                    else
                    begin

                        var LLayoutColumns: TLayoutColumns;
                        try

                            var LResponse:=Service.Mediator.Utility.LoadAgeLayoutSync(
                                Service.Settings.DirLayouts + TCommon.AgeViewLayout,
                                LLayoutColumns
                            );

                            if not LResponse.IsSucceeded then
                            begin
                                SetDefaultHeader(Grid);
                                Grid.SetColWidth(10, 20, 400);
                            end
                            else
                            begin

                                Grid.ColCount:=Length(LLayoutColumns.Columns);

                                for var Index:=0 to Grid.ColCount - 1 do
                                begin
                                    var ColumnNumber:=LLayoutColumns.Columns[Index].Number;
                                    Grid.Cells[ColumnNumber, 0] :=LLayoutColumns.Columns[Index].Name;
                                    Grid.ColWidths[ColumnNumber]:=LLayoutColumns.Columns[Index].Width;
                                end;

                            end;

                        finally
                            LLayoutColumns.Free();
                        end;

                    end;

                    for var Index:=1 to RowCount do
                    begin
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._CustomerName), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].CustomerName;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._CustomerNumber), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].CustomerNumber.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._FollowUp), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].FollowUp;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Overdue), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Overdue.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._NotDue), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].NotDue.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Range1), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Range1.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Range2), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Range2.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Range3), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Range3.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Range4), Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Range4.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Range5),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Range5.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Range6),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Range6.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Total),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Total.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._SourceDbName),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].SourceDbName;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Free1),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Free1;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._PersonResponsible),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].PersonResponsible;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._SalesResponsible),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].SalesResponsible;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._PaymentTerms),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].PaymentTerms;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Free2),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Free2;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Free3),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Free3;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._CreditLimit),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].CreditLimit.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._CreditBalance),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].CreditBalance.ToString();
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._LedgerIso),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].LedgerIso;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._CustomerGroup),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].CustomerGroup;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._AccountType),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].AccountType;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Inf4),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Inf4;
                        Grid.Cells[Grid.GetCol(TCustomerSnapshot._Group3),Index]:=ReturnCustomerSnapshots.CustomerSnapshot[Index - 1].Group3.ToString();
                    end;

                    PayLoad.AgeDate:=ReturnCustomerSnapshots.AgeDate;

                    THelpers.ComputeAgeSummary(Grid, PayLoad);
                    Service.Logger.Log('[ReadAgeViewAsync]: Aging summary has been calculated.');

                    THelpers.ComputeRiskClass(Grid, PayLoad, RiskClassGroup);
                    Service.Logger.Log('[ReadAgeViewAsync]: Risk class data has been calculated.');

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[ReadAgeViewAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnCustomerSnapshots.Free();
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


end.

