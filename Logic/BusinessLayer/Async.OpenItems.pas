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


    /// <summary>
    /// Callback signature (delegate) for scanning SSIS master table to check if open items have been updated.
    /// </summary>
    TScanOpenItems = procedure(CanMakeAge: boolean; ReadDateTime: string; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for reading open items. Payload returned contains summary data from loaded invoices.
    /// </summary>
    TReadOpenItems = procedure(ActionMode: TLoading; OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse) of object;


    IOpenItems = interface(IInterface)
    ['{CD6AC138-D2A4-4C6B-A3F1-07F904BA44B1}']

        /// <summary>
        /// Returns latest open items date and time of SSIS data extract (query SSIS master database table).
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDateTimeAwaited(Return: TCalendar): string;

        /// <summary>
        /// Returns status code from SSIS master table for given date and time.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetStatusAwaited(DateTime: string): string;

        /// <summary>
        /// Allow to async. check SSIS master table to check if open items have been updated.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);

        /// <summary>
        /// Allow to async. load current open items from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadOpenItemsAsync(ActionMode: TLoading; OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid; Callback: TReadOpenItems);

    end;


    TOpenItems = class(TInterfacedObject, IOpenItems)
    {$TYPEINFO ON}
    private
        function FLoadToGrid(OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid): boolean;
        procedure FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
    public

        /// <summary>
        /// Returns latest open items date and time of SSIS data extract (query SSIS master database table).
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDateTimeAwaited(Return: TCalendar): string;

        /// <summary>
        /// Returns status code from SSIS master table for given date and time.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetStatusAwaited(DateTime: string): string;

        /// <summary>
        /// Allow to async. check SSIS master table to check if open items have been updated.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);

        /// <summary>
        /// Allow to async. load current open items from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadOpenItemsAsync(ActionMode: TLoading; OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid; Callback: TReadOpenItems);

    end;


implementation


uses
    System.Variants,
    Handler.Account{legacy},
    Handler.Sql{legacy},
    Handler.Database{legacy},
    DbModel{legacy},
    Unity.Settings,
    Unity.Helpers,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Sql,
    Unity.Chars,
    Unity.DateTimeFormats,
    Unity.Unknown,
    Sync.Documents,
    Async.Debtors;


procedure TOpenItems.ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ReadStatus: string;
        var ReadDateTime: string;
        var CallResponse: TCallResponse;
        var CanMakeAge: boolean:=False;

        try

            ReadDateTime:=GetDateTimeAwaited(DateTime);
            ReadStatus:=GetStatusAwaited(ReadDateTime);

            if ( StrToDateTime(OpenItemsUpdate) < StrToDateTime(ReadDateTime) )
                and ( ReadStatus = 'Completed' ) then CanMakeAge:=True;

            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ScanOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CanMakeAge, ReadDateTime, CallResponse);
        end);

    end);

    NewTask.Start;

end;


procedure TOpenItems.ReadOpenItemsAsync(ActionMode: TLoading; OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid; Callback: TReadOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var OpenItemsData: TOpenItemsPayLoad;

        try

            FLoadToGrid(OpenItemsGrid, SettingsGrid);
            FCalculateOpenItems(OpenItemsGrid, OpenItemsData);
            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ReadOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ActionMode, OpenItemsData, CallResponse);
        end);

    end);

    NewTask.Start;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


function TOpenItems.GetDateTimeAwaited(Return: TCalendar): string;
begin

    var NewResult: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            DataTables.CleanUp;

            // -------------------------
            // Get latest date and time.
            // -------------------------

            DataTables.Columns.Add
            (
                TSql.MAX +
                    DataTables.BracketStr(TSSISMaster.StartDateTime, TBrackets.Round) +
                TSql._AS +
                    QuotedStr(TSSISMaster.StartDateTime)
            );

            DataTables.OpenTable(TSSISMaster.SSISMaster);

            // ----------------------
            // Examine received data.
            // ----------------------

            if (not (DataTables.DataSet = nil)) and (DataTables.DataSet.RecordCount = 1) then
            begin

                var Value: string:=VarToStr(DataTables.DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);

                if Value <> '' then
                begin

                    case Return of

                        TCalendar.TimeOnly: NewResult:=FormatDateTime(TDateTimeFormats.TimeFormat, VarToDateTime(Value));
                        TCalendar.DateOnly: NewResult:=FormatDateTime(TDateTimeFormats.DateFormat, VarToDateTime(Value));
                        TCalendar.DateTime: NewResult:=FormatDateTime(TDateTimeFormats.DateTimeFormat, VarToDateTime(Value));

                    end;

                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewResult;

end;


function TOpenItems.GetStatusAwaited(DateTime: string): string;
begin

    var NewResult: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            DataTables.CleanUp();
            DataTables.Columns.Add(TSSISMaster.StatusCode);
            DataTables.CustFilter:=TSql.WHERE + TSSISMaster.StartDateTime + TSql.EQUAL + QuotedStr(DateTime);
            DataTables.OpenTable(TSSISMaster.SSISMaster);

            if (not (DataTables.DataSet = nil)) and (DataTables.DataSet.RecordCount = 1) then
                NewResult:=VarToStr(DataTables.DataSet.Fields.Item[TSSISMaster.StatusCode].Value);

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewResult;

end;


function TOpenItems.FLoadToGrid(OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid): boolean;
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

            if SettingsGrid.Cells[iCNT, 3] = 'OFF' then
                Agents:='OFF';

            if SettingsGrid.Cells[iCNT, 3] = 'ON' then
                Agents:='ON';

            if SettingsGrid.Cells[iCNT, 2] = 'OFF' then
                Divisions:='OFF';

            if SettingsGrid.Cells[iCNT, 2] = 'ON'  then
                Divisions:='ON';

        end;

        /// <remarks>
        /// Do not use "cmdstoredproc" to execute stored procedure with adodb
        /// use ordinary "cmdtext" with "exec" statement just like you would
        /// use it in Microsoft SQL Management Studio. Alternatively, use
        /// firedac from embarcadero instead of adodb as it is more robust library.
        /// </remarks>

        DataTables.CmdType:=cmdText;
        DataTables.StrSQL:=TSql.EXECUTE + 'Customer.QueryOpenItemsAlt'                + TChars.SPACE +
                  QuotedStr(GetDateTimeAwaited(DateOnly))                             + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(SettingsGrid.Cells[0, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(SettingsGrid.Cells[1, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(SettingsGrid.Cells[2, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(THelpers.ConvertCoCode(SettingsGrid.Cells[3, 0], 'F', 0)) + TChars.COMMA +
                  QuotedStr(CutOff)                                                   + TChars.COMMA +
                  QuotedStr(Agents)                                                   + TChars.COMMA +
                  QuotedStr(Divisions)                                                + TChars.COMMA +
                  QuotedStr(INF4);

        // ----------------------------------------------------
        // Move received data to locked visual component async.
        // ----------------------------------------------------

        Result:=DataTables.SqlToGrid(OpenItemsGrid, DataTables.ExecSQL, False, True);

    finally
        DataTables.Free();
    end;

end;


procedure TOpenItems.FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
begin

    var Settings: ISettings:=TSettings.Create;
    var VoucherNumber: string:=Settings.GetStringValue(TConfigSections.Unallocated, 'VOUCHER_NUM', '0');

    var VoTpCol   :=InputGrid.ReturnColumn(DbModel.TOpenitems.VoTp, 1, 1);
    var OpenAmCol :=InputGrid.ReturnColumn(DbModel.TOpenitems.OpenAm, 1, 1);
    var PmtStatCol:=InputGrid.ReturnColumn(DbModel.TOpenitems.PmtStat, 1, 1);

    for var iCNT: integer:=1 to InputGrid.RowCount - 1 do
    begin

        // -------------------------------
        // Get actual invoice open amount.
        // -------------------------------

        var InvoiceAmt: double:=StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);

        // -------------------------
        // Aggregate invoice amount.
        // -------------------------

        OutputData.OsAmount:=OutputData.OsAmount + InvoiceAmt;

        // --------------------------------------------------------
        // Depends on invoice type defined in the general settings.
        // --------------------------------------------------------

        if THelpers.IsVoType(InputGrid.Cells[VoTpCol, iCNT]) = True then inc(OutputData.NumOfInvoices);

        // ----------------------------------------------
        // Count all overdue invoices and thiers amounts.
        // ----------------------------------------------

        if (StrToIntDef(InputGrid.Cells[PmtStatCol, iCNT], 0) < 0) and (THelpers.IsVoType(InputGrid.Cells[VoTpCol, iCNT]) = True) then
        begin
            inc(OutputData.OverdueItems);
            OutputData.OvdAmount:=OutputData.OvdAmount + StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);
        end;

        // ---------------------------------------------------------
        // For unallocated payments we take into consideration
        // negative amounts and voucher that indicate bank postings.
        // ---------------------------------------------------------

        if (StrToFloat(InputGrid.Cells[OpenAmCol, iCNT]) < 0) and (InputGrid.Cells[VoTpCol, iCNT] = VoucherNumber) then
            OutputData.UnallocatedAmt:=OutputData.UnallocatedAmt + StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);

    end;

    OutputData.UnallocatedAmt:=Abs(OutputData.UnallocatedAmt);
    OutputData.TotalItems:=InputGrid.RowCount - 1;

end;


end.

