unit Async.OpenItems;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.StrUtils,
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


    // --------------------
    // Callback signatures.
    // --------------------

    TScanOpenItemsAsync = procedure(LastError: TLastError) of object;
    TReadOpenItemsAsync = procedure(LastError: TLastError) of object;


    IOpenItems = interface(IInterface)
    ['{CD6AC138-D2A4-4C6B-A3F1-07F904BA44B1}']
        procedure ScanOpenItemsAsync();
        procedure ReadOpenItemsAsync(ActionMode: TLoading);
    end;


    TOpenItems = class(TInterfacedObject, IOpenItems)
    {$TYPEINFO ON}
    private
        var FDestGrid:    TStringGrid;
        var FSettingGrid: TStringGrid;
        function FGetDateTime(Return: TCalendar): string;
        function FGetStatus(DateTime: string): string;
        function FLoadToGrid: boolean;
    public
        procedure ScanOpenItemsAsync();
        procedure ReadOpenItemsAsync(ActionMode: TLoading);
    end;


implementation


uses
    System.Variants,
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Sql,
    Handler.Database,
    DbModel,
    Unity.Settings,
    Unity.Helpers,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Handler.Account,
    Sync.Documents,
    Async.Debtors,
    Unity.Sql,
    Unity.Chars,
    Unity.DateTimeFormats,
    Unity.Unknown;


// ------------------------------------
// Load aging report for main view
// *Change when SQL is replaced by API
// ------------------------------------

procedure TOpenItems.ScanOpenItemsAsync();
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CanMakeAge: boolean:=False;
//        var Transactions: TTransactions:=TTransactions.Create(SessionService.FDbConnect);
//        try
//
//            try
//
//                var ReadDateTime: string:=FGetDateTime(DateTime);
//                var ReadStatus:   string:=FGetStatus(ReadDateTime);
//
//                if ( StrToDateTime(MainForm.FOpenItemsUpdate) < StrToDateTime(ReadDateTime) ) and ( ReadStatus = 'Completed' ) then
//                begin
//
//                    // Switch off all of the timers
//                    MainForm.SwitchTimers(TurnedOff);
//
//                    // Refresh open items and make new aging view
//                    MainForm.FOpenItemsUpdate:=ReadDateTime;
//                    MainForm.FOpenItemsStatus:='';
//                    CanMakeAge:=True;
//
//                end;
//
//            except
//                on E: Exception do
//                    ThreadFileLog.Log('Cannot execute [OpenItemsScanner]. Error has been thrown: ' + E.Message);
//            end;
//
//        finally
//            Transactions.Free;
//        end;

        if CanMakeAge then ReadOpenItemsAsync(TLoading.CallMakeAge);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load open items into TStringGrid
// *Change when SQL is replaced by API
// ------------------------------------

procedure TOpenItems.ReadOpenItemsAsync(ActionMode: TLoading);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        //var OpenItems: TTransactions:=TTransactions.Create(SessionService.FDbConnect);
        //var StopWatch: TStopWatch:=TStopWatch.StartNew;
        try

            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Downloading, MainForm);
            try

                FDestGrid   :=MainForm.sgOpenItems;  // !!
                FSettingGrid:=MainForm.sgCompanyData; // !!
                FDestGrid.Freeze(True);

                // Sync with GUI
                TThread.Synchronize(nil, MainForm.ClearOpenItemsSummary);

                // Async
                FLoadToGrid;
                MainForm.UpdateOpenItemsSummary(MainForm.sgOpenItems); // !!!

            except
                on E: Exception do
                    ThreadFileLog.Log('Cannot execute [ReadOpenItems]. Error has been thorwn: ' + E.Message);
            end;

        finally

//            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
//            var THDSec:  extended:=THDMili / 1000;

//            ThreadFileLog.Log('Open Items loading thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
//            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

            // Release VCL and set auto column width
//            TThread.Synchronize(nil, procedure
//            begin
//                DestGrid.SetColWidth(10, 20, 400);
//            end);

            FDestGrid.Freeze(False);

        end;

//        // Make age view from open items and send to SQL Server
//        if ActionMode = CallMakeAge then
//        begin
//
//            MainForm.cbDump.Checked:=False;
//
//            var Debtors: IDebtors:=TDebtors.Create();
//            Debtors.MakeAgeViewAsync(MainForm.FOSAmount, MainForm.MakeAgeViewAsync_Callback);
//
//        end;

    end);

    NewTask.Start;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Get date and time from SSISMaster table.
/// </summary>

function TOpenItems.FGetDateTime(Return: TCalendar): string;
begin

    var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        DataTables.CleanUp;

        // Get latest date and time
        DataTables.Columns.Add
        (
            TSql.MAX +
                DataTables.BracketStr(TSSISMaster.StartDateTime, TBrackets.Round) +
            TSql._AS +
                QuotedStr(TSSISMaster.StartDateTime)
        );

        // Open column with function applied
        DataTables.OpenTable(TSSISMaster.SSISMaster);

        // Examine received data
        if (not (DataTables.DataSet = nil)) and (DataTables.DataSet.RecordCount = 1) then
        begin

            var Value: string:=VarToStr(DataTables.DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);

            if Value <> '' then
            begin

                case Return of

                    TCalendar.TimeOnly: Result:=FormatDateTime(TDateTimeFormats.TimeFormat, VarToDateTime(Value));
                    TCalendar.DateOnly: Result:=FormatDateTime(TDateTimeFormats.DateFormat, VarToDateTime(Value));
                    TCalendar.DateTime: Result:=FormatDateTime(TDateTimeFormats.DateTimeFormat, VarToDateTime(Value));

                end;

            end;

        end;

    finally
        DataTables.Free();
    end;

end;


/// <summary>
/// Get status code from SSIS Master table for given datetime.
/// </summary>

function TOpenItems.FGetStatus(DateTime: string): string;
begin

    var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        DataTables.CleanUp();
        DataTables.Columns.Add(TSSISMaster.StatusCode);
        DataTables.CustFilter:=TSql.WHERE + TSSISMaster.StartDateTime + TSql.EQUAL + QuotedStr(DateTime);
        DataTables.OpenTable(TSSISMaster.SSISMaster);

        if (not (DataTables.DataSet = nil)) and (DataTables.DataSet.RecordCount = 1) then
            Result:=VarToStr(DataTables.DataSet.Fields.Item[TSSISMaster.StatusCode].Value);

    finally
        DataTables.Free();
    end;


end;


/// <summary>
/// Load open items from database table into string grid.
/// </summary>

function TOpenItems.FLoadToGrid: boolean;
begin

    // Parameters for SQL stored procedure
    var Settings:  ISettings:=TSettings.Create;
    var CutOff:    string:='71150';
    var INF4:      string:='cards';
    var Agents:    string;
    var Divisions: string;

    var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        /// <remarks>
        /// SettingGrid has fixed dimensions.
        /// </remarks>

        for var iCNT: integer:=0 to 3 do
        begin

            /// <remarks>
            /// To stack companies all agent information must be the same
            /// thus set agent per last found the same principle applies for division.
            /// </remarks>

            if FSettingGrid.Cells[iCNT, 3] = 'OFF' then
                Agents:='OFF';

            if FSettingGrid.Cells[iCNT, 3] = 'ON' then
                Agents:='ON';

            if FSettingGrid.Cells[iCNT, 2] = 'OFF' then
                Divisions:='OFF';

            if FSettingGrid.Cells[iCNT, 2] = 'ON'  then
                Divisions:='ON';

        end;

        /// <remarks>
        /// Do not use "cmdstoredproc" to execute stored procedure with adodb
        /// use ordinary "cmdtext" with "exec" statement just like you would
        /// use it in Microsoft SQL Management Studio. Alternatively, use
        /// firedac from embarcadero instead of adodb as it is more robust library.
        /// </remarks>

        DataTables.CmdType:=cmdText;
        DataTables.StrSQL:=TSql.EXECUTE + ''                                          + TChars.SPACE +
                  QuotedStr(FGetDateTime(DateOnly))                                   + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(FSettingGrid.Cells[0, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(FSettingGrid.Cells[1, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(FSettingGrid.Cells[2, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(FSettingGrid.Cells[3, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(CutOff)                                                   + TChars.COMMA +
                  QuotedStr(Agents)                                                   + TChars.COMMA +
                  QuotedStr(Divisions)                                                + TChars.COMMA +
                  QuotedStr(INF4);

        Result:=DataTables.SqlToGrid(FDestGrid, DataTables.ExecSQL, False, True);

        // Sort via CUID
        FDestGrid.MSort(FDestGrid.ReturnColumn(DbModel.TOpenitems.Cuid, 1 , 1), 2, True);

    finally
        DataTables.Free();
    end;

end;


end.

