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
        /// Allow to read async. current age report from SQL database. Notification is always executed in main thread
        /// as long as callback is provided.
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
    /// and override the methods or and extend them.
    /// </remarks>
    TDebtors = class(TInterfacedObject, IDebtors)
    strict private
        function  FComparableDbName(InputString: string; IsPrefixRequired: boolean): string;
        procedure FComputeAgeSummary(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad);
        procedure FComputeRiskClass(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad; RiskClassGroup: TRiskClassGroup);
    public
        constructor Create();
        destructor Destroy(); override;
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
    Api.ReturnCustSnapshots;


constructor TDebtors.Create();
begin
end;


destructor TDebtors.Destroy();
begin
    inherited;
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

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        Grid.Cells[1, iCNT]:=ReturnCustSnapshots.CustomerName[iCNT - 1];
                        Grid.Cells[2, iCNT]:=ReturnCustSnapshots.CustomerNumber[iCNT - 1].ToString();
                        Grid.Cells[3, iCNT]:=ReturnCustSnapshots.FollowUp[iCNT - 1];
                        Grid.Cells[4, iCNT]:=ReturnCustSnapshots.Overdue[iCNT - 1].ToString();
                        Grid.Cells[5, iCNT]:=ReturnCustSnapshots.NotDue[iCNT - 1].ToString();
                        Grid.Cells[6, iCNT]:=ReturnCustSnapshots.Range1[iCNT - 1].ToString();
                        Grid.Cells[7, iCNT]:=ReturnCustSnapshots.Range2[iCNT - 1].ToString();
                        Grid.Cells[8, iCNT]:=ReturnCustSnapshots.Range3[iCNT - 1].ToString();
                        Grid.Cells[9, iCNT]:=ReturnCustSnapshots.Range4[iCNT - 1].ToString();
                        Grid.Cells[10,iCNT]:=ReturnCustSnapshots.Range5[iCNT - 1].ToString();
                        Grid.Cells[11,iCNT]:=ReturnCustSnapshots.Range6[iCNT - 1].ToString();
                        Grid.Cells[12,iCNT]:=ReturnCustSnapshots.Total[iCNT - 1].ToString();
                        Grid.Cells[13,iCNT]:=ReturnCustSnapshots.SourceDbName[iCNT - 1];
                        Grid.Cells[14,iCNT]:=ReturnCustSnapshots.Free1[iCNT - 1];
                        Grid.Cells[15,iCNT]:=ReturnCustSnapshots.PersonResponsible[iCNT - 1];
                        Grid.Cells[16,iCNT]:=ReturnCustSnapshots.SalesResponsible[iCNT - 1];
                        Grid.Cells[17,iCNT]:=ReturnCustSnapshots.PaymentTerms[iCNT - 1].ToString();
                        Grid.Cells[18,iCNT]:=ReturnCustSnapshots.Free2[iCNT - 1];
                        Grid.Cells[19,iCNT]:=ReturnCustSnapshots.Free3[iCNT - 1];
                        Grid.Cells[20,iCNT]:=ReturnCustSnapshots.CreditLimit[iCNT - 1].ToString();
                        Grid.Cells[21,iCNT]:=ReturnCustSnapshots.CreditBalance[iCNT - 1].ToString();
                        Grid.Cells[23,iCNT]:=ReturnCustSnapshots.LedgerIso[iCNT - 1];
                        Grid.Cells[24,iCNT]:=ReturnCustSnapshots.CustomerGroup[iCNT - 1];
                        Grid.Cells[25,iCNT]:=ReturnCustSnapshots.AccountType[iCNT - 1];
                        Grid.Cells[26,iCNT]:=ReturnCustSnapshots.Inf4[iCNT - 1];
                        Grid.Cells[27,iCNT]:=ReturnCustSnapshots.Group3[iCNT - 1];
                    end;

                    PayLoad.AgeDate:=ReturnCustSnapshots.AgeDate;

                    FComputeAgeSummary(Grid, PayLoad);
                    Service.Logger.Log('[ReadAgeViewAsync]: Aging summary has been calculated.');

                    FComputeRiskClass(Grid, PayLoad, RiskClassGroup);
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

        for var iCNT:=1 to Grid.RowCount - 1 do
        begin

            for var jCNT:=1 to Source.RowCount - 1 do
            begin

                var TargetName:=Grid.Cells[ColTargetName, iCNT];
                var SourceName:=Source.Cells[ColSourceId, jCNT];

                var TargetCoCode:=Grid.Cells[ColTargetCoCode, iCNT];
                var SourceDbName:=Source.Cells[ColSourceDbName, jCNT];

                if not SourceDbName.Contains('F') then SourceDbName:=THelpers.GetSourceDBName(SourceDbName, 'F');

                if (TargetName = SourceName) and (FComparableDbName(TargetCoCode, IsPrefixRequired) = SourceDbName)
                then
                begin

                    TThread.Synchronize(nil, procedure
                    begin
                        Grid.Cells[ColTargetName, iCNT]:=Source.Cells[ColSourceDesc, jCNT];
                    end);

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

                    for var iCNT:=0 to Length(CustSortingOptions.SortingOptions) - 1 do
                        TempList.Add(CustSortingOptions.SortingOptions[iCNT]);

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


procedure TDebtors.FComputeAgeSummary(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad);
begin

    for var iCNT:=1 to Grid.RowCount - 1 do //if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then
    begin

        AgingPayLoad.ANotDue:=AgingPayLoad.ANotDue + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._NotDue), iCNT], 0);
        AgingPayLoad.ARange1:=AgingPayLoad.ARange1 + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Range1), iCNT], 0);
        AgingPayLoad.ARange2:=AgingPayLoad.ARange2 + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Range2), iCNT], 0);
        AgingPayLoad.ARange3:=AgingPayLoad.ARange3 + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Range3), iCNT], 0);
        AgingPayLoad.ARange4:=AgingPayLoad.ARange4 + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Range4), iCNT], 0);
        AgingPayLoad.ARange5:=AgingPayLoad.ARange5 + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Range5), iCNT], 0);
        AgingPayLoad.ARange6:=AgingPayLoad.ARange6 + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Range6), iCNT], 0);

        AgingPayLoad.Balance:=AgingPayLoad.Balance + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Total), iCNT], 0);
        AgingPayLoad.Limits:=AgingPayLoad.Limits + StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._CreditLimit), iCNT], 0);

        if StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._CreditBalance), iCNT], 0) < 0 then
        begin
            inc(AgingPayLoad.Exceeders);
            AgingPayLoad.TotalExceed:=AgingPayLoad.TotalExceed + Abs(StrToFloatDef(Grid.Cells[Grid.GetCol(TReturnCustSnapshots._CreditBalance), iCNT], 0));
        end;

        inc(AgingPayLoad.CustAll);

    end;

end;


procedure TDebtors.FComputeRiskClass(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad; RiskClassGroup: TRiskClassGroup);
begin

    if AgingPayLoad.Balance = 0 then Exit();

    var TotalPerItem: TArray<double>;
    var ListPosition: TArray<integer>;
    var Count: double:=0;
    var Rows: integer:=0;

    AgingPayLoad.RCA:=AgingPayLoad.Balance * RiskClassGroup.Class_A;
    AgingPayLoad.RCB:=AgingPayLoad.Balance * RiskClassGroup.Class_B;
    AgingPayLoad.RCC:=AgingPayLoad.Balance * RiskClassGroup.Class_C;

    // Move totals and its positions into array
    for var iCNT:=1 to Grid.RowCount do
    //if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then
    begin
        SetLength(ListPosition, Rows + 1);
        SetLength(TotalPerItem, Rows + 1);
        ListPosition[Rows]:=iCNT;
        TotalPerItem[Rows]:=StrToFloatDef((Grid.Cells[Grid.GetCol(TReturnCustSnapshots._Total), iCNT]), 0);
        inc(Rows);
    end;

    // Sort via total value
    TSorting.QuickSort(TotalPerItem, ListPosition, Low(TotalPerItem), High(TotalPerItem), False);

    // Compute and display RCA
    for var iCNT:=Low(ListPosition) to High(ListPosition) do
    begin

        Count:=Count + TotalPerItem[iCNT];

        // Risk Class 'A'
        if Count <= AgingPayLoad.RCA then
        begin
            Grid.Cells[Grid.GetCol('Risk Class'), ListPosition[iCNT]]:='A';
            inc(AgingPayLoad.RCAcount);
        end;

        // Risk Class 'B'
        if (Count > AgingPayLoad.RCA) and (Count <= AgingPayLoad.RCA + AgingPayLoad.RCB) then
        begin
            Grid.Cells[Grid.GetCol('Risk Class'), ListPosition[iCNT]]:='B';
            inc(AgingPayLoad.RCBcount);
        end;

        // Risk Class 'C'
        if Count > AgingPayLoad.RCA + AgingPayLoad.RCB then
        begin
            Grid.Cells[Grid.GetCol('Risk Class'), ListPosition[iCNT]]:='C';
            inc(AgingPayLoad.RCCcount);
        end;

    end;

end;


end.

