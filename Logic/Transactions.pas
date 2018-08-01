{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Transactions;

interface

uses
  Main, Model, SQL, Variants, StrUtils, SysUtils, StdCtrls, Classes, Windows, Messages, ADODB;

{ ------------------------------------------------------------- ! OPEN ITEMS CLASS ! ------------------------------------------------------------------------ }
type
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
  Settings;

{ ############################################################## ! OPEN ITEMS CLASS ! ####################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------- GET DATE AND TIME FROM "SSISMASTER" }
function TTransactions.GetDateTime(Return: integer): string;
var
  Value:  string;
begin
  CleanUp;
  { GET LATEST DATE AND TIME }
  Columns.Add(
               MAX +
                 BracketStr(TSSISMaster.StartDateTime, brRound) +
               _AS +
                 QuotedStr(TSSISMaster.StartDateTime)
             );
  { OPEN COLUMN WITH FUNCTION APPLIED }
  OpenTable(TblSSISMaster);
  { EXAMINE RECEIVED DATA }
  if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
  begin
    Value:=VarToStr(DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);
    { WE GOT THE DATE AND TIME }
    if Value <> '' then
    begin
      if Return = gdTimeOnly then Result:=FormatDateTime(gdTimeFormat,     VarToDateTime(Value));
      if Return = gdDateOnly then Result:=FormatDateTime(gdDateFormat,     VarToDateTime(Value));
      if Return = gdDateTime then Result:=FormatDateTime(gdDateTimeFormat, VarToDateTime(Value));
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------- GET STATUS FROM "SSISMASTER" FOR GIVEN DATETIME }
function TTransactions.GetStatus(DateTime: string): string;
begin
  CleanUp;
  Columns.Add(TSSISMaster.StatusCode);
  CustFilter:=WHERE + TSSISMaster.StartDateTime + EQUAL + QuotedStr(DateTime);
  OpenTable(TblSSISMaster);
  if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then Result:=VarToStr(DataSet.Fields.Item[TSSISMaster.StatusCode].Value);
end;

{ ----------------------------------------------------------------------------------------------------------------------------- LOAD OPEN ITEMS FROM DATABASE }
function TTransactions.LoadToGrid: boolean;
var
  AppSettings: TSettings;
  CutOff:      string;
  INF4:        string;
  Agents:      string;
  Divisions:   string;
  iCNT:        integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  AppSettings:=TSettings.Create;
  try
    { PARAMETERS FOR SQL PROCEDURE }
    CutOff :=IntToStr(AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFNUM', 0));
    INF4   :=AppSettings.TMIG.ReadString(OpenItemsData, 'TXCUTOFFTXT', '');
  finally
    AppSettings.Free;
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------------- AGENT ON/OFF }

  (* WARNING! "SettingGrid" HAS FIXED DIMENSIONS *)

  for iCNT:=0 to 3 do
  begin
    { TO STACK COMPANIES ALL AGENT   }
    { INFORMATION MUST BE THE SAME   }
    { THUS SET AGENT PER LAST FOUND  }
    { THE SAME PRINCIPLE APPLIES FOR }
    { DIVISION                       }
    if SettingGrid.Cells[iCNT, 3] = 'OFF' then Agents   :='OFF';
    if SettingGrid.Cells[iCNT, 3] = 'ON'  then Agents   :='ON';
    if SettingGrid.Cells[iCNT, 2] = 'OFF' then Divisions:='OFF';
    if SettingGrid.Cells[iCNT, 2] = 'ON'  then Divisions:='ON';
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------- EXECUTE STORED SQL }

  (* WARNING! DO NOT USE "cmdStoredProc" TO EXECUTE STORED PROCEDURE WITH ADODB *)
  (*          USE ORDINARY "cmdText" WITH "EXEC" STATEMENT JUST LIKE YOU WOULD  *)
  (*          USE IT IN MICROSOFT MANAGEMENT STUDIO. ALTERNATIVELY, USE FIREDAC *)
  (*          FROM EMBARCADERO INSTEAD OF ADODB AS IT IS MORE ROBUST LIBRARY    *)

  CmdType:=cmdText;
  StrSQL:=EXECUTE + QueryOpenItems                                + SPACE +
          QuotedStr(GetDateTime(gdDateOnly))                      + COMMA +
          QuotedStr(MainForm.ConvertName(SettingGrid.Cells[0, 0], 'F', 0)) + COMMA +
          QuotedStr(MainForm.ConvertName(SettingGrid.Cells[1, 0], 'F', 0)) + COMMA +
          QuotedStr(MainForm.ConvertName(SettingGrid.Cells[2, 0], 'F', 0)) + COMMA +
          QuotedStr(MainForm.ConvertName(SettingGrid.Cells[3, 0], 'F', 0)) + COMMA +
          QuotedStr(CutOff)                                       + COMMA +
          QuotedStr(Agents)                                       + COMMA +
          QuotedStr(Divisions)                                    + COMMA +
          QuotedStr(INF4);
  Result:=SqlToGrid(DestGrid, ExecSQL, False, True);
  { ------------------------------------------------------------------------------------------------------------------------------------------- SORT VIA CUID }
  DestGrid.MSort(DestGrid.ReturnColumn(TOpenitems.CUID, 1 ,1), 2, True);  //to be removed - to be done on SQL server
end;

{ ------------------------------------------------------------------------------------------------------------------------- LOOK FOR VOUCHER TYPE IN SETTINGS }
function TTransactions.IsVoType(VoType: string): boolean;
var
  AppSettings: TSettings;
  tsVAL:       TStringList;
  iCNT :       integer;
begin
  Result:=False;
  tsVAL :=TStringList.Create;
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.ReadSectionValues(InvoiceTypes, tsVAL);
    for iCNT:=0 to tsVAL.Count - 1 do
      if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
      begin
        Result:=True;
        break;
      end;
  finally
    AppSettings.Free;
    tsVAL.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------- CLEAR ALL SUMMARY DETAILS }
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

{ ---------------------------------------------------------------------------------------------------------------------- DISPLAY UPDATED SUMMARY FOR THE USER }
procedure TTransactions.UpdateSummary;  //REFACTOR!!!

  (* COMMON VARIABLES *)

  var
    { COUNTERS }
    iCNT            : integer;
    nInvoices       : integer;
    Overdue         : integer;
    { AMOUNTS }
    OverdueAmt      : double;
    UNamt           : double;
    KPIOverdue      : double;
    KPIUnalloc      : double;
    { AMOUNT OF OUTSTANDING INVOICES ONLY }
    InvoiceAmt      : double;
    { VOUCHER NUMBER }
    VoucherNumber   : string;

  (* NESTED METHODS *)

  { INITIALIZE ALL LOCAL VARIABLES }
  procedure VarInitialize;
  begin
    nInvoices :=0;
    Overdue   :=0;
    OverdueAmt:=0;
    UNamt     :=0;
    InvoiceAmt:=0;
    KPIOverdue:=0;
    KPIUnalloc:=0;
  end;

  { GET VOUCHER NUMBER FROM SETTINGS }
  function GetVoucherNumber: string;
  var
    AppSettings: TSettings;
  begin
    Result:='0';
    AppSettings:=TSettings.Create;
    try
      Result:=AppSettings.TMIG.ReadString(Unallocated, 'VOUCHER_NUM', '');
    finally
      AppSettings.Free;
    end;
  end;

begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  VarInitialize;
  VoucherNumber:=GetVoucherNumber;
  { ------------------------------------------------------------------------------------------------------------------------------------------------- COMPUTE }
  for iCNT:=1 to DestGrid.RowCount - 1 do
  begin
    { GET ACTUAL INVOICE OPEN AMOUNT }
    InvoiceAmt:=StrToFloatDef(DestGrid.Cells[5, iCNT], 0);
    { AGGREGATE INVOICE AMOUNT }
    MainForm.OSAmount:=MainForm.OSAmount + InvoiceAmt;
    { DEPENDS ON INVOICE TYPE DEFINED IN THE GENERAL SETTINGS }
    if IsVoType(DestGrid.Cells[3, iCNT]) = True then inc(nInvoices);
    { ------------------------------------------------------------------------------------------------------------- COUNT ALL OVERDUE INVOICES AND ITS AMOUNT }
    if (StrToIntDef(DestGrid.Cells[33, iCNT], 0) < 0) and (IsVoType(DestGrid.Cells[3, iCNT]) = True) then
    begin
      inc(Overdue);
      OverdueAmt:=OverdueAmt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0);
    end;
    { ---------------------------------------------------------------------------------------------------------------------------------- UNALLOCATED PAYMENTS }
    { WE TAKE INTO CONSIDERATION NEGATIVE AMOUNTS }
    { AND VOUCHER THAT INDICATE BANK POSTINGS     }
    if (StrToFloat(DestGrid.Cells[5, iCNT]) < 0) and (DestGrid.Cells[3, iCNT] = VoucherNumber) then
      UNamt:=UNamt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0);
  end;
  { GET TOTAL SUM OF KPI TARGETS FOR ALL LOADED COMPANY CODES }
  CleanUp;
  Columns.Add(
               SUM +
                 BracketStr(TCompany.KPI_OVERDUE_TARGET, brRound) +
               _AS +
                 QuotedStr(TCompany.KPI_OVERDUE_TARGET)
             );
  Columns.Add(
               SUM +
                 BracketStr(TCompany.KPI_UNALLOCATED_TARGET, brRound) +
               _AS +
                 QuotedStr(TCompany.KPI_UNALLOCATED_TARGET)
             );
  CustFilter:=WHERE +
                TCompany.CO_CODE +
              EQUAL +
                QuotedStr(SettingGrid.Cells[0, 0]) +
              _OR  +
                TCompany.CO_CODE +
              EQUAL +
                QuotedStr(SettingGrid.Cells[1, 0]) +
              _OR  +
                TCompany.CO_CODE +
              EQUAL +
                QuotedStr(SettingGrid.Cells[2, 0]) +
              _OR  +
                TCompany.CO_CODE +
              EQUAL +
                QuotedStr(SettingGrid.Cells[3, 0]);
  OpenTable(TblCompany);
  if DataSet.RecordCount = 1 then
  begin
    KPIOverdue:=StrToFloatDef(MainForm.OleGetStr(DataSet.Fields[TCompany.KPI_OVERDUE_TARGET].Value), 0);
    KPIUnalloc:=StrToFloatDef(MainForm.OleGetStr(DataSet.Fields[TCompany.KPI_UNALLOCATED_TARGET].Value), 0);
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
