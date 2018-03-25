{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Transactions;

interface

uses
  Main, Model, Variants, StrUtils, SysUtils, StdCtrls, Classes, Windows, Messages, ADODB;

{ ------------------------------------------------------------- ! OPEN ITEMS CLASS ! ------------------------------------------------------------------------ }
type
  TTransactions = class(TDataTables)
  {$TYPEINFO ON}
  public
    var DestGrid    :  TStringGrid;
    var SettingGrid :  TStringGrid;
  published
    function  GetDateTime(Return: integer): string;
    function  LoadToGrid: boolean;
    function  IsVoType(VoType: string): boolean;
    procedure ClearSummary;
    procedure UpdateSummary;
  end;

implementation

uses
  Settings;

{ ############################################################## ! OPEN ITEMS CLASS ! ####################################################################### }

{ --------------------------------------------------------------------------------------------------------------------------------- GET CURRENT DATE AND TIME }
function TTransactions.GetDateTime(Return: integer): string;
var
  Value:       string;
begin
  { GET LATEST DATE AND TIME }
  Columns.Add(
               MAX +
                 BracketStr(TOpenitems.ExtractDateStamp, brRound) +
               _AS +
                 QuotedStr(TOpenitems.ExtractDateStamp)
             );
  { OPEN COLUMN WITH FUNCTION APPLIED }
  OpenTable(TblOpenitems);
  ExecSQL;
  { EXAMINE RECEIVED DATA }
  if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
  begin
    Value:=VarToStr(DataSet.Fields.Item[TOpenitems.ExtractDateStamp].Value);
    { WE GOT THE DATE AND TIME }
    if Value <> '' then
    begin
      if Return = gdTimeOnly then Result:=FormatDateTime(gdTimeFormat,     StrToDateTime(Value));
      if Return = gdDateOnly then Result:=FormatDateTime(gdDateFormat,     StrToDateTime(Value));
      if Return = gdDateTime then Result:=FormatDateTime(gdDateTimeFormat, StrToDateTime(Value));
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------- LOAD OPEN ITEMS FROM DATABASE }
function TTransactions.LoadToGrid: boolean;
var
  AppSettings: TSettings;
  CutOff:      string;
  INF4:        string;
  Agents:      string;
  SortPos:     integer;
  iCNT:        integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  AppSettings:=TSettings.Create;
  try
    { PARAMETERS FOR SQL PROCEDURE }
    CutOff :=IntToStr(AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFNUM', 0));
    INF4   :=AppSettings.TMIG.ReadString(OpenItemsData, 'TXCUTOFFTXT', '');
    { FOR STRING GRID SORTING }
    SortPos:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'SORTPOS', 0);
  finally
    AppSettings.Free;
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------------- AGENT ON/OFF }

  (* WARNING! "SettingGrid" HAS FIXED DIMENSIONS *)

  for iCNT:=0 to 3 do
  begin
    { TO STACK COMPANIES ALL AGENT  }
    { INFORMATION MUST BE THE SAME  }
    { THUS SET AGENT PER LAST FOUND }
    if SettingGrid.Cells[iCNT, 3] = 'OFF' then Agents:='OFF';
    if SettingGrid.Cells[iCNT, 3] = 'ON'  then Agents:='ON';
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
          QuotedStr(INF4);
  ExecSQL;
  Result:=SqlToGrid(DestGrid, ExecSQL, False, False);
  { ----------------------------------------------------------------------------------------------------------------------------------------- SORT VIA CUID }
  DestGrid.MSort(SortPos, 2, True);
end;

{ --------------------------------------------------------------------------------------------------------------------------------- CLEAR ALL SUMMARY DETAILS }
procedure TTransactions.ClearSummary;
begin
  MainForm.OSAmount                :=0;
  MainForm.tcOpenItems.Caption     :='0';
  MainForm.tcOverdue.Caption       :='0';
  MainForm.tcInvoices.Caption      :='0';
  MainForm.tcDecAmt.Caption        :='0';
  MainForm.tcDisAmt.Caption        :='0';
  MainForm.tcRecovery.Caption      :='0';
  MainForm.tcOSAmt.Caption         :='0';
  MainForm.tcUNamt.Caption         :='0';
  MainForm.tcOvdAmt.Caption        :='0';
  MainForm.tcOverdueRatio.Caption  :='0';
  MainForm.tcKPIoverdue.Caption    :='0';
  MainForm.tcKPIunallocated.Caption:='0';
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

{ ---------------------------------------------------------------------------------------------------------------------- DISPLAY UPDATED SUMMARY FOR THE USER }
procedure TTransactions.UpdateSummary;

  (* COMMON VARIABLES *)

  var
    { COUNTERS }
    iCNT            : integer;
    nInvoices       : integer;
    Overdue         : integer;
    { AMOUNTS }
    OverdueAmt      : double;
    UNamt           : double;
    { REFLECTS TIME VALUE OF MONEY: DISCOUNTED AMOUNT, DECREASE IN AMOUNT AND RECOVERY AMOUNT }
    cDiscountedAmt  : double;
    cDecreaseAmt    : double;
    cRecoveryAmt    : double;
    DiscountedAmt   : double;
    DecreaseAmt     : double;
    RecoveryAmt     : double;
    { INTEREST RATE FOR GIVEN COMPANY CODE }
    InterestRate    : double;
    { AMOUNT OF OUTSTANDING INVOICES ONLY }
    InvoiceAmt      : double;
    { PAYMENT STATUS }
    TimeDiff        : integer;
    { VOUCHER NUMBER }
    VoucherNumber   : string;

  (* NESTED METHODS *)

  { INITIALIZE ALL LOCAL VARIABLES }
  procedure VarInitialize;
  begin
    nInvoices     :=0; Overdue     :=0; OverdueAmt  :=0; UNamt:=0;
    cDiscountedAmt:=0; cDecreaseAmt:=0; cRecoveryAmt:=0;
    DiscountedAmt :=0; DecreaseAmt :=0; RecoveryAmt :=0;
    InterestRate  :=0; InvoiceAmt  :=0; TimeDiff    :=0;
  end;

  { GET INTEREST RATE FOR GIVEN CO CODE }
  function GetInterestRate(CoCode: string): double;
  var
    iCNT: integer;
  begin
    Result:=0;
    CoCode:=IntToStr((StrToInt(StringReplace(CoCode, 'F', '0', [rfReplaceAll]))));
    { NOTE OF CAUTION: SETTING GRID ON "TRANSACTIONS" HAS FIXED DIMENSION }
    {                  INTEREST RATE IS IN SECOND ROW                     }
    for iCNT:=0 to 3 do
    begin
      if SettingGrid.Cells[iCNT, 0] = CoCode then
      begin
        Result:=StrToFloatDef(SettingGrid.Cells[iCNT, 2], 0);
        Break;
      end;
    end;
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
    { GET PAYMENT STATUS }
    TimeDiff:=StrToIntDef(DestGrid.Cells[33, iCNT], 0);
    { GET ACTUAL INVOICE OPEN AMOUNT }
    InvoiceAmt:=StrToFloatDef(DestGrid.Cells[5, iCNT], 0);
    { AGGREGATE INVOICE AMOUNT }
    MainForm.OSAmount:=MainForm.OSAmount + InvoiceAmt;
    { GET INTEREST RATE }
    InterestRate:=GetInterestRate(DestGrid.Cells[1, iCNT]);
    { COMPUTE INVOICE DISCOUNTING }
    if (TimeDiff < 0) and (InterestRate > 0) and (InvoiceAmt > 0) and (IsVoType(DestGrid.Cells[3, iCNT]) = True) then
    begin
      { CALCULATE FOR GIVEN INVOICE }
      DiscountedAmt:=InvoiceAmt / ( 1 + ( (InterestRate / 365) * ABS(TimeDiff)) );
      DecreaseAmt  :=InvoiceAmt - DiscountedAmt;
      RecoveryAmt  :=InvoiceAmt + DecreaseAmt;
      { WRITE TO STRING GRID }
      DestGrid.Cells[34, iCNT]:=FormatFloat('###0.00', DiscountedAmt);
      DestGrid.Cells[35, iCNT]:=FormatFloat('###0.00', DecreaseAmt);
      DestGrid.Cells[36, iCNT]:=FormatFloat('###0.00', RecoveryAmt);
      { AGGREGATE FOR SUMMARY }
      cDiscountedAmt:=cDiscountedAmt + DiscountedAmt;
      cDecreaseAmt  :=cDecreaseAmt   + DecreaseAmt;
      cRecoveryAmt  :=cRecoveryAmt   + RecoveryAmt;
    end;
    { DEPENDS ON INVOICE TYPE DEFINED IN THE GENERAL SETTINGS }
    if IsVoType(DestGrid.Cells[3, iCNT]) = True then inc(nInvoices);
    { ------------------------------------------------------------------------------------------------------------- COUNT ALL OVERDUE INVOICES AND ITS AMOUNT }
    if (StrToIntDef(DestGrid.Cells[33, iCNT], 0) < 0) and (IsVoType(DestGrid.Cells[3, iCNT]) = True) then
    begin
      inc(Overdue);
      OverdueAmt:=OverdueAmt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0)
    end;
    { ------------------------------------------------------------------------------------------------------------------------------ UNALLOCATED PAYMENTS }
    { WE TAKE INTO CONSIDERATION NEGATIVE AMOUNTS }
    { AND VOUCHER THAT INDICATE BANK POSTINGS     }
    if (StrToFloat(DestGrid.Cells[5, iCNT]) < 0) and (DestGrid.Cells[3, iCNT] = VoucherNumber) then
      UNamt:=UNamt + StrToFloatDef(DestGrid.Cells[5, iCNT], 0);
  end;
  { DISPLAY }
  MainForm.tcOpenItems.Caption     :=FormatFloat('### ###',  DestGrid.RowCount - 1);
  MainForm.tcInvoices.Caption      :=FormatFloat('### ###',  nInvoices);
  MainForm.tcOverdue.Caption       :=FormatFloat('### ###',  Overdue);
  MainForm.tcOverdueRatio.Caption  :=FormatFloat('0.00',     (( (Overdue / nInvoices) * 100 ))) + '%';
  MainForm.tcDisAmt.Caption        :=FormatFloat('#,##0.00', cDiscountedAmt);
  MainForm.tcDecAmt.Caption        :=FormatFloat('#,##0.00', cDecreaseAmt);
  MainForm.tcRecovery.Caption      :=FormatFloat('#,##0.00', cRecoveryAmt);
  MainForm.tcOSAmt.Caption         :=FormatFloat('#,##0.00', MainForm.OSAmount);
  MainForm.tcOvdAmt.Caption        :=FormatFloat('#,##0.00', OverdueAmt);
  MainForm.tcUNAmt.Caption         :=FormatFloat('#,##0.00', abs(UNamt));
end;

end.
