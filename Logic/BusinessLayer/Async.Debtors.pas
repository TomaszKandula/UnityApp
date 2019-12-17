unit Async.Debtors;

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
    Vcl.Graphics,
    Vcl.ComCtrls,
    Vcl.Dialogs,
    Data.Win.ADODB,
    Data.DB,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for reading current age report from SQL database.
    /// </summary>
    TReadAgeView = procedure(ReturnedData: TStringGrid; PayLoad: TAgingPayLoad; CallResponse: TCallResponse) of object;


    IDebtors = interface(IInterface)
    ['{194FE2BE-386E-499E-93FB-0299DA53A70A}']

        /// <summary>
        /// Allow to read async. current age report from SQL database. Notification is always executed in main thread
        /// as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadAgeViewAsync(SelectedCoCodes: string; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView);

        /// <summary>
        /// Allow to map data between grids. It replaces the target column data for appropiate data in source grid based on given parameters.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure MapTableAwaited(Grid: TStringGrid; Source: TStringGrid; IsPrefixRequired: boolean;
            const ColTargetPersonResp: integer; const ColSourceId: integer; const ColTargetCoCode: integer;
            const ColSourceDbName: integer; const ColSourceDesc: integer);

        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustSortingOptionsAwaited(var SortingOptions: TStringList): TCallResponse;

    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    strict private
        function  FComparableDbName(InputString: string; IsPrefixRequired: boolean): string;
        procedure FComputeAgeSummary(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad);
        procedure FComputeRiskClass(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad; RiskClassGroup: TRiskClassGroup);
    public

        /// <summary>
        /// Allow to read async. current age report from database for given CoCodes and sorting option.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadAgeViewAsync(SelectedCoCodes: string; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView);

        /// <summary>
        /// Allow to map data between grids. It replaces the target column data for appropiate data in source grid based on given parameters.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure MapTableAwaited(Grid: TStringGrid; Source: TStringGrid; IsPrefixRequired: boolean;
            const ColTargetPersonResp: integer; const ColSourceId: integer; const ColTargetCoCode: integer;
            const ColSourceDbName: integer; const ColSourceDesc: integer);

        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustSortingOptionsAwaited(var SortingOptions: TStringList): TCallResponse;

    end;


implementation


uses
    System.StrUtils,
    REST.Types,
    REST.Json,
    Handler.Database{legacy},
    Handler.Sql{legacy},
    DbModel{legacy},
    Unity.Helpers,
    Unity.Settings,
    Unity.StatusBar,
    Unity.Sorting,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Sql{Legacy},
    Unity.RestWrapper,
    Api.CustSortingOptions,
    Sync.Documents;


procedure TDebtors.ReadAgeViewAsync(SelectedCoCodes: string; SortMode: string; RiskClassGroup: TRiskClassGroup; Callback: TReadAgeView);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var PayLoad: TAgingPayLoad;
        var CallResponse: TCallResponse;
        var Grid: TStringGrid:=TStringGrid.Create(nil);
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            CallResponse.IsSucceeded:=True;
            try

                var StrCol: string;
                var CheckColumns:=Grid.LoadLayout(
                    StrCol,
                    TConfigSections.ColumnWidthName,
                    TConfigSections.ColumnOrderName,
                    TConfigSections.ColumnNames,
                    TConfigSections.ColumnPrefix
                );

                if not CheckColumns then
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[ReadAgeViewAsync]: Cannot load columns. Please contact IT support.';
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end
                else
                begin

                    DataTables.CmdType:=cmdText;
                    DataTables.StrSQL:='exec Customer.AgeViewReportAlt2 ' + StrCol.QuotedString + ',' + SelectedCoCodes.QuotedString + ',' + SortMode.QuotedString;
                    DataTables.SqlToGrid(Grid, DataTables.ExecSQL, False, False);
                    ThreadFileLog.Log('[ReadAgeViewAsync]: SQL statement applied "' + DataTables.StrSQL + '".');

                    FComputeAgeSummary(Grid, PayLoad);
                    ThreadFileLog.Log('[ReadAgeViewAsync]: Aging summary has been calculated.');

                    FComputeRiskClass(Grid, PayLoad, RiskClassGroup);
                    ThreadFileLog.Log('[ReadAgeViewAsync]: Risk class data has been calculated.');

                end;

            except
                on E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[ReadAgeViewAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            DataTables.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(Grid, PayLoad, CallResponse);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TDebtors.MapTableAwaited(Grid: TStringGrid; Source: TStringGrid; IsPrefixRequired: boolean;
    const ColTargetPersonResp: integer; const ColSourceId: integer; const ColTargetCoCode: integer;
    const ColSourceDbName: integer; const ColSourceDesc: integer);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        for var iCNT:=1 to Grid.RowCount - 1 do
        begin

            for var jCNT:=1 to Source.RowCount - 1 do
            begin

                if (Grid.Cells[ColTargetPersonResp, iCNT] = Source.Cells[ColSourceId, jCNT]) and
                    (FComparableDbName(Grid.Cells[ColTargetCoCode, iCNT], IsPrefixRequired) = Source.Cells[ColSourceDbName, jCNT])
                then
                    Grid.Cells[ColTargetPersonResp, iCNT]:=Source.Cells[ColSourceDesc, jCNT];

            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

end;


function TDebtors.GetCustSortingOptionsAwaited(var SortingOptions: TStringList): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var TempList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
            Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'snapshots/options/';
            Restful.RequestMethod:=TRESTRequestMethod.rmGET;
            ThreadFileLog.Log('[GetUserSortingOptionsAwaited]: Executing GET ' + Restful.ClientBaseURL);

            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    var CustSortingOptions: TCustSortingOptions:=TJson.JsonToObject<TCustSortingOptions>(Restful.Content);

                    for var iCNT:=0 to Length(CustSortingOptions.SortingOptions) - 1 do
                        TempList.Add(CustSortingOptions.SortingOptions[iCNT]);

                    CallResponse.IsSucceeded:=True;
                    CustSortingOptions.Free();
                    ThreadFileLog.Log('[GetUserSortingOptionsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                end
                else
                begin

                    if not String.IsNullOrEmpty(Restful.ExecuteError) then
                        CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                    else
                        if String.IsNullOrEmpty(Restful.Content) then
                            CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetUserSortingOptionsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
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

    for var iCNT:=1 to Grid.RowCount - 1 do {if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then}
    begin

        AgingPayLoad.ANotDue:=AgingPayLoad.ANotDue + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fNotDue), iCNT], 0);
        AgingPayLoad.ARange1:=AgingPayLoad.ARange1 + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fRange1), iCNT], 0);
        AgingPayLoad.ARange2:=AgingPayLoad.ARange2 + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fRange2), iCNT], 0);
        AgingPayLoad.ARange3:=AgingPayLoad.ARange3 + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fRange3), iCNT], 0);
        AgingPayLoad.ARange4:=AgingPayLoad.ARange4 + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fRange4), iCNT], 0);
        AgingPayLoad.ARange5:=AgingPayLoad.ARange5 + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fRange5), iCNT], 0);
        AgingPayLoad.ARange6:=AgingPayLoad.ARange6 + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fRange6), iCNT], 0);

        AgingPayLoad.Balance:=AgingPayLoad.Balance + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fTotal), iCNT], 0);
        AgingPayLoad.Limits:=AgingPayLoad.Limits + StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fCreditLimit), iCNT], 0);

        if StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fCreditBalance), iCNT], 0) < 0 then
        begin
            inc(AgingPayLoad.Exceeders);
            AgingPayLoad.TotalExceed:=AgingPayLoad.TotalExceed + Abs(StrToFloatDef(Grid.Cells[Grid.GetCol(TSnapshots.fCreditBalance), iCNT], 0));
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
    {if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then}
    begin
        SetLength(ListPosition, Rows + 1);
        SetLength(TotalPerItem, Rows + 1);
        ListPosition[Rows]:=iCNT;
        TotalPerItem[Rows]:=StrToFloatDef((Grid.Cells[Grid.GetCol(TSnapshots.fTotal), iCNT]), 0);
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
            Grid.Cells[Grid.GetCol(TSnapshots.fRiskClass), ListPosition[iCNT]]:='A';
            inc(AgingPayLoad.RCAcount);
        end;

        // Risk Class 'B'
        if (Count > AgingPayLoad.RCA) and (Count <= AgingPayLoad.RCA + AgingPayLoad.RCB) then
        begin
            Grid.Cells[Grid.GetCol(TSnapshots.fRiskClass), ListPosition[iCNT]]:='B';
            inc(AgingPayLoad.RCBcount);
        end;

        // Risk Class 'C'
        if Count > AgingPayLoad.RCA + AgingPayLoad.RCB then
        begin
            Grid.Cells[Grid.GetCol(TSnapshots.fRiskClass), ListPosition[iCNT]]:='C';
            inc(AgingPayLoad.RCCcount);
        end;

    end;

end;


end.

