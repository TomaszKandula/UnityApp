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
    Unity.Records,
    Unity.Arrays;


type


    /// <summary>
    /// Callback signature (delegate) for reading current age report from SQL database.
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

    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    strict private
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

    end;


implementation


uses
    System.StrUtils,
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


procedure TDebtors.FComputeAgeSummary(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad);
begin

    for var iCNT:=1 to Grid.RowCount - 1 do {if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then}
    begin

        AgingPayLoad.ANotDue:=AgingPayLoad.ANotDue + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fNotDue, 1, 1), iCNT], 0);
        AgingPayLoad.ARange1:=AgingPayLoad.ARange1 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange1, 1, 1), iCNT], 0);
        AgingPayLoad.ARange2:=AgingPayLoad.ARange2 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange2, 1, 1), iCNT], 0);
        AgingPayLoad.ARange3:=AgingPayLoad.ARange3 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange3, 1, 1), iCNT], 0);
        AgingPayLoad.ARange4:=AgingPayLoad.ARange4 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange4, 1, 1), iCNT], 0);
        AgingPayLoad.ARange5:=AgingPayLoad.ARange5 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange5, 1, 1), iCNT], 0);
        AgingPayLoad.ARange6:=AgingPayLoad.ARange6 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange6, 1, 1), iCNT], 0);

        AgingPayLoad.Balance:=AgingPayLoad.Balance + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fTotal, 1, 1), iCNT], 0);
        AgingPayLoad.Limits:=AgingPayLoad.Limits + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCreditLimit, 1, 1), iCNT], 0);

        if StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCreditBalance, 1, 1), iCNT], 0) < 0 then
        begin
            inc(AgingPayLoad.Exceeders);
            AgingPayLoad.TotalExceed:=AgingPayLoad.TotalExceed + Abs(StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCreditBalance, 1, 1), iCNT], 0));
        end;

        inc(AgingPayLoad.CustAll);

    end;

end;


procedure TDebtors.FComputeRiskClass(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad; RiskClassGroup: TRiskClassGroup);
begin

    if AgingPayLoad.Balance = 0 then Exit();

    var TotalPerItem: Unity.Arrays.TADoubles;
    var ListPosition: Unity.Arrays.TAIntigers;
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
        TotalPerItem[Rows]:=StrToFloatDef((Grid.Cells[Grid.ReturnColumn(TSnapshots.fTotal, 1, 1), iCNT]), 0);
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
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fRiskClass, 1, 1), ListPosition[iCNT]]:='A';
            inc(AgingPayLoad.RCAcount);
        end;

        // Risk Class 'B'
        if (Count > AgingPayLoad.RCA) and (Count <= AgingPayLoad.RCA + AgingPayLoad.RCB) then
        begin
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fRiskClass, 1, 1), ListPosition[iCNT]]:='B';
            inc(AgingPayLoad.RCBcount);
        end;

        // Risk Class 'C'
        if Count > AgingPayLoad.RCA + AgingPayLoad.RCB then
        begin
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fRiskClass, 1, 1), ListPosition[iCNT]]:='C';
            inc(AgingPayLoad.RCCcount);
        end;

    end;

end;


end.

