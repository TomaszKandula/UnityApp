
{$I .\Include\Header.inc}

unit Transactions;

interface

uses
    InterposerClasses, Model, SQL, Variants, StrUtils, SysUtils, StdCtrls, Classes, Windows, Messages, ADODB;

type

    /// <summary>
    ///     Responsible for loading open items and displaying summary in Open Items tab sheet.
    /// </summary>

    TTransactions = class(TDataTables)
    {$TYPEINFO ON}
    public
        var DestGrid    :  TStringGrid;
        var SettingGrid :  TStringGrid;
    published
        function  GetDateTime(Return: integer): string;
        function  GetStatus(DateTime: string): string;
        function  LoadToGrid: boolean;
        function  IsVoType(VoType: string): boolean;
        procedure ClearSummary;
        procedure UpdateSummary;
    end;


implementation


uses
    Main, Settings;


// ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS CLASS //


/// <summary>
///     Get date and time from SSISMaster table.
/// </summary>

function TTransactions.GetDateTime(Return: integer): string;
var
    Value:  string;
begin

    CleanUp;

    // Get latest date and time
    Columns.Add(
        MAX +
            BracketStr(TSSISMaster.StartDateTime, brRound) +
        _AS +
            QuotedStr(TSSISMaster.StartDateTime)
    );

    // Open column with function applied
    OpenTable(TSSISMaster.SSISMaster);

    // Examine received data
    if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
    begin
        Value:=VarToStr(DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);
        if Value <> '' then
        begin
            if Return = gdTimeOnly then Result:=FormatDateTime(gdTimeFormat,     VarToDateTime(Value));
            if Return = gdDateOnly then Result:=FormatDateTime(gdDateFormat,     VarToDateTime(Value));
            if Return = gdDateTime then Result:=FormatDateTime(gdDateTimeFormat, VarToDateTime(Value));
        end;

    end;

end;

/// <summary>
///     Get status code from SSIS Master table for given datetime.
/// </summary>

function TTransactions.GetStatus(DateTime: string): string;
begin

    CleanUp;
    Columns.Add(TSSISMaster.StatusCode);
    CustFilter:=WHERE + TSSISMaster.StartDateTime + EQUAL + QuotedStr(DateTime);
    OpenTable(TSSISMaster.SSISMaster);

    if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
        Result:=VarToStr(DataSet.Fields.Item[TSSISMaster.StatusCode].Value);

end;

/// <summary>
///     Load open items from database table into string grid.
/// </summary>

function TTransactions.LoadToGrid: boolean;
var
    Settings:    ISettings;
    CutOff:      string;
    INF4:        string;
    Agents:      string;
    Divisions:   string;
    iCNT:        integer;
begin
    Settings:=TSettings.Create;
    // Parameters for SQL stored procedure
    CutOff:=IntToStr(Settings.GetIntegerValue(OpenItemsData, 'NRCUTOFFNUM', 0));
    INF4:=Settings.GetStringValue(OpenItemsData, 'TXCUTOFFTXT', '');

    // Agent ON/OFF

    /// <remarks>
    ///     SettingGrid has fixed dimensions.
    /// </remarks>

    for iCNT:=0 to 3 do
    begin

        /// <remarks>
        ///     To stack companies all agent information must be the same
        ///     thus set agent per last found the same principle applies for division.
        /// </remarks>

        if SettingGrid.Cells[iCNT, 3] = 'OFF' then
            Agents:='OFF';

        if SettingGrid.Cells[iCNT, 3] = 'ON' then
            Agents:='ON';

        if SettingGrid.Cells[iCNT, 2] = 'OFF' then
            Divisions:='OFF';

        if SettingGrid.Cells[iCNT, 2] = 'ON'  then
            Divisions:='ON';

    end;

    /// <remarks>
    ///     Do not use "cmdstoredproc" to execute stored procedure with adodb
    ///     use ordinary "cmdtext" with "exec" statement just like you would
    ///     use it in microsoft management studio. Alternatively, use firedac
    ///     from embarcadero instead of adodb as it is more robust library.
    /// </remarks>

    CmdType:=cmdText;
    StrSQL:=EXECUTE + QueryOpenItems                                           + SPACE +
              QuotedStr(GetDateTime(gdDateOnly))                               + COMMA +
              QuotedStr(MainForm.ConvertName(SettingGrid.Cells[0, 0], 'F', 0)) + COMMA +
              QuotedStr(MainForm.ConvertName(SettingGrid.Cells[1, 0], 'F', 0)) + COMMA +
              QuotedStr(MainForm.ConvertName(SettingGrid.Cells[2, 0], 'F', 0)) + COMMA +
              QuotedStr(MainForm.ConvertName(SettingGrid.Cells[3, 0], 'F', 0)) + COMMA +
              QuotedStr(CutOff)                                                + COMMA +
              QuotedStr(Agents)                                                + COMMA +
              QuotedStr(Divisions)                                             + COMMA +
              QuotedStr(INF4);

    Result:=SqlToGrid(DestGrid, ExecSQL, False, True);

    // Sort via CUID
    DestGrid.MSort(DestGrid.ReturnColumn(TOpenitems.CUID, 1 , 1), 2, True);  //to be removed - to be done on SQL server

end;

/// <summary>
///     Look for voucher type in settings file where we define such.
/// </summary>

function TTransactions.IsVoType(VoType: string): boolean;
var
    Settings:  ISettings;
    tsVAL:     TStringList;
    iCNT :     integer;
begin

    Result  :=False;
    tsVAL   :=TStringList.Create;
    Settings:=TSettings.Create;

    try
        Settings.GetSectionValues(InvoiceTypes, tsVAL);
        for iCNT:=0 to tsVAL.Count - 1 do
            if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
            begin
                Result:=True;
                break;
            end;
    finally
        tsVAL.Free;
    end;

end;

/// <summary>
///     Clear all summary details.
/// </summary>

procedure TTransactions.ClearSummary;
begin
    MainForm.OSAmount                :=0;
    MainForm.tcOpenItems.Caption     :='0';
    MainForm.tcOverdue.Caption       :='0';
    MainForm.tcInvoices.Caption      :='0';
    MainForm.tcOSAmt.Caption         :='0';
    MainForm.tcUNamt.Caption         :='0';
    MainForm.tcOvdAmt.Caption        :='0';
    MainForm.tcKPIoverdue.Caption    :='0';
    MainForm.tcKPIunallocated.Caption:='0';
end;

/// <summary>
///     Display updated summary of open items for the user.
/// </summary>

procedure TTransactions.UpdateSummary;
var
    Settings        : ISettings;
    iCNT            : integer;
    nInvoices       : integer;
    Overdue         : integer;
    VoucherNumber   : string;
    // Amounts
    OverdueAmt      : double;
    UNamt           : double;
    KPIOverdue      : double;
    KPIUnalloc      : double;
    // O/S invoices only
    InvoiceAmt      : double;
begin

    nInvoices :=0;
    Overdue   :=0;
    OverdueAmt:=0;
    UNamt     :=0;
    KPIOverdue:=0;
    KPIUnalloc:=0;

    Settings:=TSettings.Create;
    VoucherNumber:=Settings.GetStringValue(Unallocated, 'VOUCHER_NUM', '0');

    // Compute
    for iCNT:=1 to DestGrid.RowCount - 1 do
    begin
        // Get actual invoice open amount
        InvoiceAmt:=StrToFloatDef(DestGrid.Cells[5, iCNT], 0);

        // Aggregate invoice amount
        MainForm.OSAmount:=MainForm.OSAmount + InvoiceAmt;

        { DEPENDS ON INVOICE TYPE DEFINED IN THE GENERAL SETTINGS }
        if IsVoType(DestGrid.Cells[3, iCNT]) = True then inc(nInvoices);

        // Count all overdue invoices and thiers amounts
        if (StrToIntDef(DestGrid.Cells[33, iCNT], 0) < 0) and (IsVoType(DestGrid.Cells[3, iCNT]) = True) then
        begin
            inc(Overdue);
            OverdueAmt:=OverdueAmt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0);
        end;

        // Unallocated payments

        /// <remarks>
        ///     We take into consideration negative amounts and voucher that indicate bank postings.
        /// </remarks>

        if (StrToFloat(DestGrid.Cells[5, iCNT]) < 0) and (DestGrid.Cells[3, iCNT] = VoucherNumber) then
            UNamt:=UNamt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0);

    end;

    { GET TOTAL SUM OF KPI TARGETS FOR ALL LOADED COMPANY CODES }
    CleanUp;
    Columns.Add(
        SUM +
            BracketStr(TCompanyData.KPI_OVERDUE_TARGET, brRound) +
        _AS +
            QuotedStr(TCompanyData.KPI_OVERDUE_TARGET)
    );

    Columns.Add(
        SUM +
            BracketStr(TCompanyData.KPI_UNALLOCATED_TARGET, brRound) +
            _AS +
            QuotedStr(TCompanyData.KPI_UNALLOCATED_TARGET)
    );

    CustFilter:=WHERE +
                    TCompanyData.CO_CODE +
                EQUAL +
                    QuotedStr(SettingGrid.Cells[0, 0]) +
                _OR  +
                    TCompanyData.CO_CODE +
                EQUAL +
                    QuotedStr(SettingGrid.Cells[1, 0]) +
                _OR  +
                    TCompanyData.CO_CODE +
                EQUAL +
                    QuotedStr(SettingGrid.Cells[2, 0]) +
                _OR  +
                    TCompanyData.CO_CODE +
                EQUAL +
                    QuotedStr(SettingGrid.Cells[3, 0]);

    OpenTable(TCompanyData.CompanyData);
    if DataSet.RecordCount = 1 then
    begin
        KPIOverdue:=StrToFloatDef(MainForm.OleGetStr(DataSet.Fields[TCompanyData.KPI_OVERDUE_TARGET].Value), 0);
        KPIUnalloc:=StrToFloatDef(MainForm.OleGetStr(DataSet.Fields[TCompanyData.KPI_UNALLOCATED_TARGET].Value), 0);
    end;

    { DISPLAY }
    MainForm.tcOpenItems.Caption     :=FormatFloat('### ###',  DestGrid.RowCount - 1);
    MainForm.tcInvoices.Caption      :=FormatFloat('### ###',  nInvoices);
    MainForm.tcOverdue.Caption       :=FormatFloat('### ###',  Overdue);
    MainForm.tcOSAmt.Caption         :=FormatFloat('#,##0.00', MainForm.OSAmount);
    MainForm.tcOvdAmt.Caption        :=FormatFloat('#,##0.00', OverdueAmt);
    MainForm.tcUNAmt.Caption         :=FormatFloat('#,##0.00', abs(UNamt));
    MainForm.tcKPIoverdue.Caption    :=FormatFloat('#,##0.00', KPIOverdue);
    MainForm.tcKPIUnallocated.Caption:=FormatFloat('#,##0.00', KPIUnalloc);

end;


end.
