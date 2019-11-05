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

    TScanOpenItems = procedure(CanMakeAge: boolean; ReadDateTime: string; LastError: TLastError) of object;
    TReadOpenItems = procedure(ActionMode: TLoading; OpenItemsData: TOpenItemsPayLoad; LastError: TLastError) of object;


    IOpenItems = interface(IInterface)
    ['{CD6AC138-D2A4-4C6B-A3F1-07F904BA44B1}']
        function GetDateTime(Return: TCalendar): string;
        function GetStatus(DateTime: string): string;
        procedure ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);
        procedure ReadOpenItemsAsync(ActionMode: TLoading; OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid; Callback: TReadOpenItems);
    end;


    TOpenItems = class(TInterfacedObject, IOpenItems)
    {$TYPEINFO ON}
    private
        function FLoadToGrid(OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid): boolean;
        procedure FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
    public
        function GetDateTime(Return: TCalendar): string;
        function GetStatus(DateTime: string): string;
        procedure ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);
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


// ------------------------------------
// Load aging report for main view
// *Change when SQL is replaced by API.
// ------------------------------------

procedure TOpenItems.ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ReadStatus: string;
        var ReadDateTime: string;
        var LastError: TLastError;
        var CanMakeAge: boolean:=False;

        try

            ReadDateTime:=GetDateTime(DateTime);
            ReadStatus:=GetStatus(ReadDateTime);

            if ( StrToDateTime(OpenItemsUpdate) < StrToDateTime(ReadDateTime) )
                and ( ReadStatus = 'Completed' ) then CanMakeAge:=True;

            LastError.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[ScanOpenItemsAsync] Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(CanMakeAge, ReadDateTime, LastError);
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load open items into TStringGrid
// *Change when SQL is replaced by API.
// ------------------------------------

procedure TOpenItems.ReadOpenItemsAsync(ActionMode: TLoading; OpenItemsGrid: TStringGrid; SettingsGrid: TStringGrid; Callback: TReadOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var OpenItemsData: TOpenItemsPayLoad;

        try

            FLoadToGrid(OpenItemsGrid, SettingsGrid);
            FCalculateOpenItems(OpenItemsGrid, OpenItemsData);
            LastError.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[ReadOpenItemsAsync] Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(ActionMode, OpenItemsData, LastError);
        end);

    end);

    NewTask.Start;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


function TOpenItems.GetDateTime(Return: TCalendar): string;
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


function TOpenItems.GetStatus(DateTime: string): string;
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
                  QuotedStr(GetDateTime(DateOnly))                                    + TChars.COMMA +
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

