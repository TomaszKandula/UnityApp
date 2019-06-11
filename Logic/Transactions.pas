unit Transactions;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.Variants,
    System.StrUtils,
    System.SysUtils,
    System.Classes,
    Vcl.StdCtrls,
    Data.Win.ADODB,
    DbModel,
    SqlHandler,
    InterposerClasses,
    Helpers;

type


    TTransactions = class(TDataTables)
    {$TYPEINFO ON}
    public
        var DestGrid:    TStringGrid;
        var SettingGrid: TStringGrid;
        function  GetDateTime(Return: TEnums.TCalendar): string;
        function  GetStatus(DateTime: string): string;
        function  LoadToGrid: boolean;
        function  IsVoType(VoType: string): boolean;
        procedure ClearSummary;
        procedure UpdateSummary;
    end;


implementation


uses
    Main,
    Settings;


// ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS CLASS //


/// <summary>
/// Get date and time from SSISMaster table.
/// </summary>

function TTransactions.GetDateTime(Return: TEnums.TCalendar): string;
begin

    CleanUp;

    // Get latest date and time
    Columns.Add(
        TSql.MAX +
            BracketStr(TSSISMaster.StartDateTime, TEnums.TBrackets.Round) +
        TSql._AS +
            QuotedStr(TSSISMaster.StartDateTime)
    );

    // Open column with function applied
    OpenTable(TSSISMaster.SSISMaster);

    // Examine received data
    if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
    begin

        var Value: string:=VarToStr(DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);
        if Value <> '' then
        begin

            case Return of

              TEnums.TCalendar.TimeOnly: Result:=FormatDateTime(TDateTimeFormats.TimeFormat, VarToDateTime(Value));
              TEnums.TCalendar.DateOnly: Result:=FormatDateTime(TDateTimeFormats.DateFormat, VarToDateTime(Value));
              TEnums.TCalendar.DateTime: Result:=FormatDateTime(TDateTimeFormats.DateTimeFormat, VarToDateTime(Value));

            end;

        end;

    end;

end;


/// <summary>
/// Get status code from SSIS Master table for given datetime.
/// </summary>

function TTransactions.GetStatus(DateTime: string): string;
begin

    CleanUp;
    Columns.Add(TSSISMaster.StatusCode);
    CustFilter:=TSql.WHERE + TSSISMaster.StartDateTime + TSql.EQUAL + QuotedStr(DateTime);
    OpenTable(TSSISMaster.SSISMaster);

    if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
        Result:=VarToStr(DataSet.Fields.Item[TSSISMaster.StatusCode].Value);

end;


/// <summary>
/// Load open items from database table into string grid.
/// </summary>

function TTransactions.LoadToGrid: boolean;
begin

    // Parameters for SQL stored procedure
    var Settings:  ISettings:=TSettings.Create;
    var CutOff:    string:=IntToStr(Settings.GetIntegerValue(TConfigSections.OpenItemsData, 'NRCUTOFFNUM', 0));
    var INF4:      string:=Settings.GetStringValue(TConfigSections.OpenItemsData, 'TXCUTOFFTXT', '');
    var Agents:    string;
    var Divisions: string;

    /// <remarks>
    /// SettingGrid has fixed dimensions.
    /// </remarks>

    for var iCNT: integer:=0 to 3 do
    begin

        /// <remarks>
        /// To stack companies all agent information must be the same
        /// thus set agent per last found the same principle applies for division.
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
    /// Do not use "cmdstoredproc" to execute stored procedure with adodb
    /// use ordinary "cmdtext" with "exec" statement just like you would
    /// use it in Microsoft Management Studio. Alternatively, use firedac
    /// from embarcadero instead of adodb as it is more robust library.
    /// </remarks>

    CmdType:=cmdText;
    StrSQL:=TSql.EXECUTE + QueryOpenItems                                        + TChars.SPACE +
              QuotedStr(GetDateTime(DateOnly))                                   + TChars.COMMA +
              QuotedStr(MainForm.ConvertCoCode(SettingGrid.Cells[0, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(MainForm.ConvertCoCode(SettingGrid.Cells[1, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(MainForm.ConvertCoCode(SettingGrid.Cells[2, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(MainForm.ConvertCoCode(SettingGrid.Cells[3, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(CutOff)                                                  + TChars.COMMA +
              QuotedStr(Agents)                                                  + TChars.COMMA +
              QuotedStr(Divisions)                                               + TChars.COMMA +
              QuotedStr(INF4);

    Result:=SqlToGrid(DestGrid, ExecSQL, False, True);

    // Sort via CUID
    DestGrid.MSort(DestGrid.ReturnColumn(TOpenitems.Cuid, 1 , 1), 2, True);  //to be removed - to be done on SQL server

end;


/// <summary>
/// Look for voucher type in settings file where we define such.
/// </summary>

function TTransactions.IsVoType(VoType: string): boolean;
begin

    Result:=False;

    var Settings: ISettings:=TSettings.Create;
    var tsVAL: TStringList:=TStringList.Create;
    try

        Settings.GetSectionValues(TConfigSections.InvoiceTypes, tsVAL);

        for var iCNT: integer:=0 to tsVAL.Count - 1 do
            if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
            begin
                Result:=True;
                break;
            end;

    finally
        tsVAL.Free;
    end;

end;


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
/// Display updated summary of open items for the user.
/// </summary>

procedure TTransactions.UpdateSummary;
begin

    var nInvoices:  integer:=0;
    var Overdue:    integer:=0;
    var OverdueAmt: double:=0;
    var UNamt:      double:=0;
    var KPIOverdue: double:=0;
    var KPIUnalloc: double:=0;

    var Settings: ISettings:=TSettings.Create;
    var VoucherNumber: string:=Settings.GetStringValue(TConfigSections.Unallocated, 'VOUCHER_NUM', '0');

    // Compute
    for var iCNT: integer:=1 to DestGrid.RowCount - 1 do
    begin
        // Get actual invoice open amount
        var InvoiceAmt: double:=StrToFloatDef(DestGrid.Cells[5, iCNT], 0);

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
        /// We take into consideration negative amounts and voucher that indicate bank postings.
        /// </remarks>

        if (StrToFloat(DestGrid.Cells[5, iCNT]) < 0) and (DestGrid.Cells[3, iCNT] = VoucherNumber) then
            UNamt:=UNamt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0);

    end;

    // Get total sum of KPI targets for all loaded company codes
    CleanUp;
    Columns.Add(
        TSql.SUM +
            BracketStr(TCompanyData.KpiOverdueTarget, Round) +
        TSql._AS +
            QuotedStr(TCompanyData.KpiOverdueTarget)
    );

    Columns.Add(
        TSql.SUM +
            BracketStr(TCompanyData.KpiUnallocatedTarget, Round) +
            TSql._AS +
            QuotedStr(TCompanyData.KpiUnallocatedTarget)
    );

    CustFilter:=TSql.WHERE +
                    TCompanyData.CoCode +
                TSql.EQUAL +
                    QuotedStr(SettingGrid.Cells[0, 0]) +
                TSql._OR  +
                    TCompanyData.CoCode +
                TSql.EQUAL +
                    QuotedStr(SettingGrid.Cells[1, 0]) +
                TSql._OR  +
                    TCompanyData.CoCode +
                TSql.EQUAL +
                    QuotedStr(SettingGrid.Cells[2, 0]) +
                TSql._OR  +
                    TCompanyData.CoCode +
                TSql.EQUAL +
                    QuotedStr(SettingGrid.Cells[3, 0]);

    OpenTable(TCompanyData.CompanyData);
    if DataSet.RecordCount = 1 then
    begin
        KPIOverdue:=StrToFloatDef(MainForm.OleGetStr(DataSet.Fields[TCompanyData.KpiOverdueTarget].Value), 0);
        KPIUnalloc:=StrToFloatDef(MainForm.OleGetStr(DataSet.Fields[TCompanyData.KpiUnallocatedTarget].Value), 0);
    end;

    // Display
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
