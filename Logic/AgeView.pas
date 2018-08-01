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
unit AgeView;

interface

uses
  Main, Model, SQL, ADODB, StrUtils, SysUtils, Variants, Messages, Windows, Classes, Graphics;

{ ------------------------------------------------------------- ! AGE VIEW CLASS ! -------------------------------------------------------------------------- }
type
  TAgeView = class(TDataTables)
  {$TYPEINFO ON}
  public
    { GENERATEG AGING REPORT }
    var ArrAgeView : TLists;
    { TOTAS }
    var CustAll    : integer;
    var CallsAll   : integer;
    var EmailsAll  : integer;
    { AMOUNTS }
    var ANotDue    : extended;
    var ARange1    : extended;
    var ARange2    : extended;
    var ARange3    : extended;
    var ARange4    : extended;
    var ARange5    : extended;
    var ARange6    : extended;
    var Balance    : extended;
    var Limits     : extended;
    var Exceeders  : integer;
    var TotalExceed: extended;
    var RCA        : extended;
    var RCB        : extended;
    var RCC        : extended;
    var RCAcount   : cardinal;
    var RCBcount   : cardinal;
    var RCCcount   : cardinal;
    var Class_A    : double;
    var Class_B    : double;
    var Class_C    : double;
    { SELECTION }
    var GroupID   : string;
    var AgeDate   : string;
  published
    constructor Create(Connector: TADOConnection); overload;
    destructor  Destroy; override;
    procedure   Read(var Grid: TStringGrid; Mode: integer);
    procedure   ComputeAgeSummary(Grid: TStringGrid);
    procedure   ComputeAndShowRCA(Grid: TStringGrid);
    procedure   ClearSummary;
    procedure   UpdateSummary;
    procedure   GetDetails(var Grid: TStringGrid);
    procedure   MapGroup3(var Grid: TStringGrid; Source: TStringGrid);
    function    GetData(Code: string; Table: string; Entity: string): string;
    procedure   AgeViewMode(var Grid: TStringGrid; ModeBySection: string);
    procedure   QuickSortExt(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
    procedure   Make(OSAmount: double);
    procedure   Write(DestTable: string; SourceArray: TLists);
    procedure   ExportToCSV(FileName: string; SourceArray: TLists);
  end;

implementation

uses
  Settings;

{ ############################################################## ! AGE VIEW CLASS ! ######################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TAgeView.Create(Connector: TADOConnection);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    if FormatSettings.DecimalSeparator = ',' then
    begin
      Class_A:=StrToFloat(AppSet.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', '0,80'));
      Class_B:=StrToFloat(AppSet.TMIG.ReadString(RiskClassDetails, 'CLASS_B_MAX', '0,15'));
      Class_C:=StrToFloat(AppSet.TMIG.ReadString(RiskClassDetails, 'CLASS_C_MAX', '0,05'));
    end;
    if FormatSettings.DecimalSeparator = '.' then
    begin
      Class_A:=StrToFloat(StringReplace(AppSet.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', '0,80'), ',', '.', [rfReplaceAll]));
      Class_B:=StrToFloat(StringReplace(AppSet.TMIG.ReadString(RiskClassDetails, 'CLASS_B_MAX', '0,15'), ',', '.', [rfReplaceAll]));
      Class_C:=StrToFloat(StringReplace(AppSet.TMIG.ReadString(RiskClassDetails, 'CLASS_C_MAX', '0,05'), ',', '.', [rfReplaceAll]));
    end;
  finally
    AppSet.Free;
  end;
  inherited;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TAgeView.Destroy;
begin
  ArrAgeView:=nil;
  inherited Destroy;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ READ }
procedure TAgeView.Read(var Grid: TStringGrid; Mode: integer);
var
  StrCol: string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Grid.Freeze(True);
  { --------------------------------------------------------------------------------------------- READ GRID LAYOUT TO BE PASSED AS PARAMETER TO SQL STATEMENT }
  Grid.LoadLayout(StrCol, ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);
  CmdType:=cmdText;
  StrSQL:=EXECUTE + AgeViewReport + SPACE + QuotedStr(StrCol) + COMMA + QuotedStr(GroupID) + COMMA + QuotedStr(AgeDate) + COMMA + QuotedStr(IntToStr(Mode));
  SqlToGrid(Grid, ExecSQL, False, False);
  LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: SQL statement applied [' + StrSQL + '].');
  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  Grid.Freeze(False);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- COMPUTE AGING SUMMARY }
procedure TAgeView.ComputeAgeSummary(Grid: TStringGrid);
var
  iCNT: integer;
begin
  for iCNT:=1 to Grid.RowCount - 1 do
  begin
    if Grid.RowHeights[iCNT] <> sgRowHidden then
    begin
      { NOT DUE AND RANGES }
      ANotDue:=ANotDue + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fNOT_DUE, 1, 1), iCNT], 0);
      ARange1:=ARange1 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRANGE1,  1, 1), iCNT], 0);
      ARange2:=ARange2 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRANGE2,  1, 1), iCNT], 0);
      ARange3:=ARange3 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRANGE3,  1, 1), iCNT], 0);
      ARange4:=ARange4 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRANGE4,  1, 1), iCNT], 0);
      ARange5:=ARange5 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRANGE5,  1, 1), iCNT], 0);
      ARange6:=ARange6 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRANGE6,  1, 1), iCNT], 0);
      { TOTAL AMOUNT | LEDGER BALANCE }
      Balance:=Balance + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fTOTAL, 1, 1), iCNT], 0);
      { GRANTED LIMITS | SUM OF ALL LIMITS }
      Limits:=Limits + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCREDIT_LIMIT, 1, 1), iCNT], 0);
      { EXCEEDERS }
      if StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fEXCEEDED_AMOUNT, 1, 1), iCNT], 0) < 0 then
      begin
        { COUNT EXCEEDERS }
        inc(Exceeders);
        { SUM ALL EXCEEDERS AMOUNT }
        TotalExceed:=TotalExceed + Abs(StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fEXCEEDED_AMOUNT, 1, 1), iCNT], 0));
      end;
      { COUNT ITEMS }
      inc(CustAll);
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------- COMPUTE AND ASSIGN RISK CLASS }
procedure TAgeView.ComputeAndShowRCA(Grid: TStringGrid);
var
  Rows        :  integer;
  iCNT        :  integer;
  Count       :  double;
  TotalPerItem:  array of double;
  ListPosition:  array of integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  if Balance = 0 then Exit;
  Count:=0;
  Rows :=0;
  RCA:=Balance * Class_A;
  RCB:=Balance * Class_B;
  RCC:=Balance * Class_C;
  { ---------------------------------------------------------------------------------------------------------------- MOVE TOTALS AND ITS POSITIONS INTO ARRAY }
  for iCNT:=1 to Grid.RowCount do
  begin
    if Grid.RowHeights[iCNT] <> sgRowHidden then
    begin
      SetLength(ListPosition, Rows + 1);
      SetLength(TotalPerItem, Rows + 1);
      ListPosition[Rows]:=iCNT;
      TotalPerItem[Rows]:=StrToFloatDef((Grid.Cells[Grid.ReturnColumn(TSnapshots.fTOTAL, 1, 1), iCNT]), 0);
      inc(Rows);
    end;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------ SORT VIA TOTAL VALUE }
  QuickSortExt(TotalPerItem, ListPosition, Low(TotalPerItem), High(TotalPerItem), False);
  { ------------------------------------------------------------------------------------------------------------------------------------ COMPUTE AND SHOW RCA }
  for iCNT:=Low(ListPosition) to High(ListPosition) - 1 do
  begin
    Count:=Count + TotalPerItem[iCNT];
    { ASSIGN RISK CLASS 'A' }
    if Count <= RCA then
    begin
      Grid.Cells[Grid.ReturnColumn(TSnapshots.fRISK_CLASS, 1, 1), ListPosition[iCNT]]:='A';
      inc(RCAcount);
    end;
    { ASSIGN RISK CLASS 'B' }
    if (Count > RCA) and (Count <= RCA + RCB) then
    begin
      Grid.Cells[Grid.ReturnColumn(TSnapshots.fRISK_CLASS, 1, 1), ListPosition[iCNT]]:='B';
      inc(RCBcount);
    end;
    { ASSIGN RISK CLASS 'C' }
    if Count > RCA + RCB then
    begin
      Grid.Cells[Grid.ReturnColumn(TSnapshots.fRISK_CLASS, 1, 1), ListPosition[iCNT]]:='C';
      inc(RCCcount);
    end;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- CLEAR SUMMARY }
procedure TAgeView.ClearSummary;  //to mainform
begin
  { TOP }
  MainForm.tcCOCODE1.Caption    :='n/a';
  MainForm.tcCOCODE2.Caption    :='n/a';
  MainForm.tcCOCODE3.Caption    :='n/a';
  MainForm.tcCOCODE4.Caption    :='n/a';
  MainForm.tcCURRENCY.Caption   :='n/a';
  MainForm.tcTOTAL.Caption      :='0';
  { TRADE RECEIVABLES SUMMARY }
  MainForm.valND.Caption        :='0';
  MainForm.valR1.Caption        :='0';
  MainForm.valR2.Caption        :='0';
  MainForm.valR3.Caption        :='0';
  MainForm.valR4.Caption        :='0';
  MainForm.valR5.Caption        :='0';
  MainForm.valR6.Caption        :='0';
  MainForm.custRISKA.Caption    :='0';
  MainForm.custRISKB.Caption    :='0';
  MainForm.custRISKC.Caption    :='0';
  MainForm.valTAMT.Caption      :='0';
  { PERCENTAGE }
  MainForm.procND.Caption       :='0';
  MainForm.procR1.Caption       :='0';
  MainForm.procR2.Caption       :='0';
  MainForm.procR3.Caption       :='0';
  MainForm.procR4.Caption       :='0';
  MainForm.procR5.Caption       :='0';
  MainForm.procR6.Caption       :='0';
  { EXCEEDERS }
  MainForm.valEXCEEDERS.Caption :='0';
  MainForm.valTEXCEES.Caption   :='0';
  MainForm.valTLIMITS.Caption   :='0';
  { NOT DUE | PAST DUE | DEFAULTED }
  MainForm.valTND.Caption       :='0';
  MainForm.valPASTDUE.Caption   :='0';
  MainForm.valDEFAULTED.Caption :='0';
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- FORMAT AND DISPLAY }
procedure TAgeView.UpdateSummary;   //to mainform
begin
  { TOP | AGE VIEW DETAILS }
  MainForm.tcTOTAL.Caption   :=IntToStr(CustAll);
  { BOTTOM | TRADE RECEIVABLES SUMMARY }
  { VALUES }
  MainForm.valND.Caption     :=FormatFloat('#,##0.00', ANotDue);
  MainForm.valR1.Caption     :=FormatFloat('#,##0.00', ARange1);
  MainForm.valR2.Caption     :=FormatFloat('#,##0.00', ARange2);
  MainForm.valR3.Caption     :=FormatFloat('#,##0.00', ARange3);
  MainForm.valR4.Caption     :=FormatFloat('#,##0.00', ARange4);
  MainForm.valR5.Caption     :=FormatFloat('#,##0.00', ARange5);
  MainForm.valR6.Caption     :=FormatFloat('#,##0.00', ARange6);
  MainForm.valTAMT.Caption   :=FormatFloat('#,##0.00', Balance);
  { PERCENTAGE }
  if not (Balance = 0) then
  begin
    MainForm.procND.Caption    :=FormatFloat('0.00', ( (ANotDue / Balance) * 100 )) + '%';
    MainForm.procR1.Caption    :=FormatFloat('0.00', ( (ARange1 / Balance) * 100 )) + '%';
    MainForm.procR2.Caption    :=FormatFloat('0.00', ( (ARange2 / Balance) * 100 )) + '%';
    MainForm.procR3.Caption    :=FormatFloat('0.00', ( (ARange3 / Balance) * 100 )) + '%';
    MainForm.procR4.Caption    :=FormatFloat('0.00', ( (ARange4 / Balance) * 100 )) + '%';
    MainForm.procR5.Caption    :=FormatFloat('0.00', ( (ARange5 / Balance) * 100 )) + '%';
    MainForm.procR6.Caption    :=FormatFloat('0.00', ( (ARange6 / Balance) * 100 )) + '%';
    MainForm.procTAMT.Caption  :=FormatFloat('0.00', ( ( (ANotDue / Balance) +
                                                         (ARange1 / Balance) +
                                                         (ARange2 / Balance) +
                                                         (ARange3 / Balance) +
                                                         (ARange4 / Balance) +
                                                         (ARange5 / Balance) +
                                                         (ARange6 / Balance) ) * 100 ) ) + '%';
  end;
  { RICK CLASSES }
  MainForm.valRISKA.Caption:=FormatFloat('#,##0.00', RCA);
  MainForm.valRISKB.Caption:=FormatFloat('#,##0.00', RCB);
  MainForm.valRISKC.Caption:=FormatFloat('#,##0.00', RCC);
  MainForm.custRISKA.Caption:=IntToStr(RCAcount) + ' customers';
  MainForm.custRISKB.Caption:=IntToStr(RCBcount) + ' customers';
  MainForm.custRISKC.Caption:=IntToStr(RCCcount) + ' customers';
  { BOTTOM | EXCEEDERS }
  MainForm.valEXCEEDERS.Caption :=IntToStr(Exceeders);
  MainForm.valTEXCEES.Caption   :=FormatFloat('#,##0.00', TotalExceed);
  MainForm.valTLIMITS.Caption   :=FormatFloat('#,##0.00', Limits);
  { BOTTOM | NOT DUE | PAST DUE | DEFAULTED }
  MainForm.valTND.Caption       :=MainForm.valND.Caption;
  MainForm.valPASTDUE.Caption   :=FormatFloat('#,##0.00', (ARange1 + ARange2 + ARange3));
  MainForm.valDEFAULTED.Caption :=FormatFloat('#,##0.00', (ARange4 + ARange5 + ARange6));
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- RETURN BASIC DETAILS }
procedure TAgeView.GetDetails(var Grid: TStringGrid);  //to mainform
var
  SL:    TStringList;
  iCNT:  integer;
begin
  { CLEAR GRID }
  Grid.ClearAll(4, 0, 0, False);
  { GET CO CODES FROM SELECTED GROUP }
  MainForm.tcCOCODE1.Caption:=MainForm.GetCoCode(1, MainForm.GroupIdSel);
  MainForm.tcCOCODE2.Caption:=MainForm.GetCoCode(2, MainForm.GroupIdSel);
  MainForm.tcCOCODE3.Caption:=MainForm.GetCoCode(3, MainForm.GroupIdSel);
  MainForm.tcCOCODE4.Caption:=MainForm.GetCoCode(4, MainForm.GroupIdSel);
  if MainForm.tcCOCODE1.Caption = '0' then MainForm.tcCOCODE1.Font.Color:=clWhite else MainForm.tcCOCODE1.Font.Color:=clBlack;
  if MainForm.tcCOCODE2.Caption = '0' then MainForm.tcCOCODE2.Font.Color:=clWhite else MainForm.tcCOCODE2.Font.Color:=clBlack;
  if MainForm.tcCOCODE3.Caption = '0' then MainForm.tcCOCODE3.Font.Color:=clWhite else MainForm.tcCOCODE3.Font.Color:=clBlack;
  if MainForm.tcCOCODE4.Caption = '0' then MainForm.tcCOCODE4.Font.Color:=clWhite else MainForm.tcCOCODE4.Font.Color:=clBlack;
  Grid.Cells[0, 0]:=MainForm.tcCOCODE1.Caption; MainForm.Find(0);
  Grid.Cells[1, 0]:=MainForm.tcCOCODE2.Caption; MainForm.Find(1);
  Grid.Cells[2, 0]:=MainForm.tcCOCODE3.Caption; MainForm.Find(2);
  Grid.Cells[3, 0]:=MainForm.tcCOCODE4.Caption; MainForm.Find(3);
  { GET CURRENCY CODES }

  (* NOTE: THERE SHOULD BE ALWAYS THE SAME CURRENCY CODE FOR ALL STACKED COMPANIES *)

  SL:=TStringList.Create;
  try
    SL.Clear;
    SL.Sorted:=True;
    SL.Duplicates:=dupIgnore;
    for iCNT:=1 to MainForm.sgAgeView.RowCount - 1 do
      SL.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fLEDGER_ISO, 1, 1), iCNT]);
  finally
    MainForm.tcCURRENCY.Caption:=SL.Text;
    SL.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- MAP GROUP3 COLUMN }

(* NOTE: DO NOT RUN IT UNTILL GROUP3 GRID IS POPULATED *)

procedure TAgeView.MapGroup3(var Grid: TStringGrid; Source: TStringGrid);
var
  iCNT:  integer;
  jCNT:  integer;
begin
  for iCNT:=1 to Grid.RowCount - 1 do
    for jCNT:=1 to Source.RowCount - 1 do
    if
      (
        Grid.Cells[Grid.ReturnColumn(TSnapshots.fGROUP3, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TGroup3.ERP_CODE, 1, 1), jCNT]
      )
    and
      (
        Grid.Cells[Grid.ReturnColumn(TSnapshots.fCO_CODE, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TGroup3.COCODE, 1, 1), jCNT]
      )
    then
      Grid.Cells[Grid.ReturnColumn(TSnapshots.fGROUP3, 1, 1), iCNT]:=Source.Cells[Source.ReturnColumn(TGroup3.DESCRIPTION, 1, 1), jCNT]
end;

{ ------------------------------------------------------------------------------------------------------------------------- FIND MATCH DATA IN GENERAL TABLES }
function TAgeView.GetData(Code: string; Table: string; Entity: string): string;  //move to another module
var
  Field:  string;
begin
  Result:=unUnassigned;
  if (Code = ' ') or (Code = '') or (Entity = ' ') or (Entity = '') then Exit;
  try
    { "GROUP3" TABLE }
    if Table = TblGroup3 then
    begin
      Field:=TGroup3.DESCRIPTION;
      CleanUp;
      Columns.Add(Field);
      CustFilter:=WHERE + TGroup3.ERP_CODE + EQUAL + QuotedStr(Code) + _AND + TGroup3.COCODE + EQUAL + QuotedStr(Entity);
      OpenTable(Table);
    end;
    { "PAID INFO" TABLE | INDEPENDENT FROM ENTITY CODE }
    if Table = TblPaidinfo then
    begin
      Field:=TPaidinfo.DESCRIPTION;
      CleanUp;
      Columns.Add(Field);
      CustFilter:=WHERE + TPaidInfo.ERP_CODE + EQUAL + QuotedStr(Code);
      OpenTable(Table);
    end;
    { "PAYMENT TERMS" TABLE }
    if Table = TblPmtterms then
    begin
      Field:=TPmtterms.DESCRIPTION;
      CleanUp;
      Columns.Add(Field);
      CustFilter:=WHERE + TPmtterms.ERP_CODE + EQUAL + QuotedStr(Code) + _AND + TPmtterms.COCODE + EQUAL + QuotedStr(Entity);
      OpenTable(Table);
    end;
    { "PERSONS" TABLE }
    if Table = TblPerson then
    begin
      Field:=TPerson.DESCRIPTION;
      CleanUp;
      Columns.Add(Field);
      CustFilter:=WHERE + TPerson.ERP_CODE + EQUAL + QuotedStr(Code) + _AND + TPerson.COCODE + EQUAL + QuotedStr(Entity);
      OpenTable(Table);
    end;
    if DataSet.RecordCount = 1 then Result:=MainForm.OleGetStr(DataSet.Fields[Field].Value);
  except
    Result:='';
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------- SETUP PROPER COLUMNS VISIBILITY }
procedure TAgeView.AgeViewMode(var Grid: TStringGrid; ModeBySection: string);
var
  AppSettings:  TSettings;
  iCNT:         integer;
begin
  AppSettings:=TSettings.Create;
  try
    for iCNT:=0 to Grid.ColCount - 2 do
      if AppSettings.TMIG.ReadString(ModeBySection, MainForm.FindKey(AppSettings.TMIG, ModeBySection, iCNT), 'True') = 'False' then
        Grid.ColWidths[Grid.ReturnColumn(MainForm.FindKey(AppSettings.TMIG, ModeBySection, iCNT), 1, 1)]:=-1
          else
            Grid.ColWidths[Grid.ReturnColumn(MainForm.FindKey(AppSettings.TMIG, ModeBySection, iCNT), 1, 1)]:=100;
  finally
    AppSettings.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ QUICK SORT }
procedure TAgeView.QuickSortExt(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
{ "A" VARIABLE HOLDS NUMERICAL DATA TO BE SORTED. "L" VARIABLE IS "ASSOCIATED" COLUMN WITH ORIGINAL LIST POSITION. THE SECOND ASSOCIATED COLUMN FOLLOWS }
{ "A" COLUMN, BUT IT IS NOT SORTED. IT ALLOWS TO ASSIGN SORTED VALUES BACK TO ORIGINAL LIST POSITION AFTER COMPUTATION IS DONE. THIS IS TO BE USED WHEN }
{ SORTING IS NECESSARY BEFORE APPLAYING COMPUTATION AND AFTER WHICH WE MUST PUT VALUES BACK TO ITS ORIGINAL POSITIONS.                                  }
var
  Lo:     integer;
  Hi:     integer;
  Pivot:  double;
  T1:     double;   { FOR SORTING COLUMN    }
  T2:     integer;  { FOR ASSOCIATED COLUMN }
begin
   Lo:=iLo;
   Hi:=iHi;
   Pivot:=A[(Lo + Hi) div 2];
   repeat
     { ASCENDING }
     if ASC then
     begin
       while A[Lo] < Pivot do Inc(Lo);
       while A[Hi] > Pivot do Dec(Hi);
     end;
     { DESCENDING }
     if not ASC then
     begin
       while A[Lo] > Pivot do Inc(Lo);
       while A[Hi] < Pivot do Dec(Hi);
     end;
     { MOVING POSITIONS }
     if Lo <= Hi then
     begin
       T1:=A[Lo];
       T2:=L[Lo];
       { SORTING COLUMN }
       A[Lo]:= A[Hi];
       A[Hi]:= T1;
       { ASSOCIATED COLUMN }
       L[Lo]:= L[Hi];
       L[Hi]:= T2;
       { MOVE NEXT }
       Inc(Lo);
       Dec(Hi);
     end;
   until Lo > Hi;
   if Hi > iLo then QuickSortExt(A, L, iLo, Hi, ASC);
   if Lo < iHi then QuickSortExt(A, L, Lo, iHi, ASC);
end;

(* ********************************************************* ! PRE-PARE AGE VIEW ! ****************************************************************************

NOTES:
------

D - DESTINATION
C - CALCULATED 'ON THE FLY'

WARNING!

IF NUMBERS ARE PROVIDED WITH NON-ENGLISH FORMAT '100000,00', THEN WE MUST REPLACE IT BY POINT DECIMAL SEPARATOR TO SAFELY SEND TO THE SQL SERVER.
SUCH REPLACEMENT CAN BE OMITTED IF SOURCE IS ALREADY PRESENTED WITH DECIMAL POINT SEPARATOR.

OPEN ITEMS OWNLOADED FROM SOURCE FILE | AGE VIEW MADE FROM OPEN ITEMS      | COLUMN     | WORKER ARRAY
--------------------------------------|------------------------------------|------------|---------------
COLUMN NUMBER   | FIELD NAME          | ASSIGN NUMBER   | FIELD NAME       | ASSIGNMENT | COLUMN NUMBER
----------------|---------------------|-----------------|------------------|------------|---------------
 0              | ID                  | DO NOT USE      | ID               |            | DO NOT USE
 1              | SourceDBName        | -               | GROUP_ID         |            | 0
 2              | CustNo              | -               | AGE_DATE         |            | 1
 3              | VoTp                | -               | SNAPSHOT_DT      |            | 2
 4              | OpenCurAm           | 6               | CUSTOMER_NAME    | D          | 3
 5              | OpenAm              | 2               | CUSTOMER_NUMBER  | D          | 4
 6              | Nm                  | 15              | COUNTRY_CODE     | D          | 5
 7              | ISO                 | -               | NOT_DUE          | C          | 6
 8              | CurAm               | -               | RANGE1           | C          | 7
 9              | Am                  | -               | RANGE2           | C          | 8
 10             | InvoNo              | -               | RANGE3           | C          | 9
 11             | DueDt               | -               | RANGE4           | C          | 10
 12             | Inf4                | -               | RANGE5           | C          | 11
 13             | Inf7                | -               | RANGE6           | C          | 12
 14             | CrLmt               | -               | OVERDUE          | C          | 13
 15             | Ctry                | -               | TOTAL            | C          | 14
 16             | CPmtTrm             | 14              | CREDIT_LIMIT     | D          | 15
 17             | PdSts               | -               | EXCEEDED_AMOUNT  | C          | 16
 18             | Agent               | 16              | PAYMENT_TERMS    | D          | 17
 19             | Ctrl                | 18              | AGENT            | D          | 18
 20             | Ad1                 | 27              | DIVISION         | D          | 19
 21             | Ad2                 | 1               | CO_CODE          | D          | 20
 22             | Ad3                 | -               | LEDGER_ISO       | C          | 21
 23             | Pno                 | 12              | INF4             | D          | 22
 24             | PArea               | 13              | INF7             | D          | 23
 25             | GenAcNo             | 30              | PERSON           | D          | 24
 26             | ValDt               | 28              | GROUP3           | D          | 25
 27             | R1  (division)      | -               | RISK_CLASS       | C          | 26
 28             | Gr3                 | -               | CUID             | D          | 27
 29             | Txt                 | -               | -                |            |
 30             | R8  (person)        | 37              | -                |            |
 31             | DirDeb              | -               | -                |            |
 32             | AddTxt              | -               | -                |            |
 33             | PmtStat (calc)      | -               | -                |            |
 34             | CUID    (calc)      | -               | -                |            |

************************************************************************************************************************************************************ *)

{ -------------------------------------------------------------------------------------------------------------------------------------------- GENERATE AGING }
procedure TAgeView.Make(OSAmount: double);  // REFACTOR TO REMOVE TIGHT COUPLING !!!

(* COMMON VARIABLES AND CONSTANTS *)

var
  { COUNTERS }
  iCNT:      integer;                   { NUMBER OF ROWS OF OPEN ITEMS 'STRINGGRID'      }
  jCNT:      integer;                   { NUMBER OF COLUMNS OF OPEN ITEMS 'STRINGGRID'   }
  { ROW COUNTERS FO AGING VIEW }
  avRow:     integer;                   { TOTAL ROWS IN AGE VIEW                         }
  exRow:     integer;                   { ROW COUNTER WHEN FILLING AGE VIEW WITH NUMBERS }
  { FIXED DATA PER ROW }
  DatTim:    string;                    { CURRENT DATE AND TIME                          }
  CutOff:    TDateTime;                 { CUT-OFF DATE, USUALLY TODAY - 1 OR - 3         }
  { BUCKET RANGES - LOWER BOUND }
  R1lo:      integer;                   { EXAMPLE: 1   }
  R2lo:      integer;                   { EXAMPLE: 8   }
  R3lo:      integer;                   { EXAMPLE: 31  }
  R4lo:      integer;                   { EXAMPLE: 61  }
  R5lo:      integer;                   { EXAMPLE: 91  }
  R6lo:      integer;                   { EXAMPLE: 121 }
  { BUCKET RANGES - UPPER BOUND }
  R1hi:      integer;                   { EXAMPLE: 7   }
  R2hi:      integer;                   { EXAMPLE: 30  }
  R3hi:      integer;                   { EXAMPLE: 60  }
  R4hi:      integer;                   { EXAMPLE: 90  }
  R5hi:      integer;                   { EXAMPLE: 120 }
  R6hi:      integer;                   { EXAMPLE: oo  }
  { BIAS }
  bias:      integer;
  { DISCOUNTED AMOUNT }
  DiscAmnt:  double;
  { RISK CLASS }
  MyWallet:  array of double;           { CONTAIN WALLET SHARE VALUES  }
  MyList:    array of integer;          { CONTAIN ASSOCIATED POSITIONS }
  RcHi:      double;                    { UPPER BOUND                  }
  RcLo:      double;                    { LOWER BOUND                  }
  Count:     double;                    { SUMS WALLET SHARE            }
  { SETTINGS }
  AppSettings: TSettings;

const
  { WARNING! BELOW MUST BE ALIGNED WITH OPEN ITEMS SOURCE AND DESTINATION TABLE IN DATABASE FOR AGE VIEW }
  oiCol: array[0..12] of integer = (6, 2, 15, 14, 16, 18, 27, 1,  12, 13, 30, 28, 34);     { DEFINES SOURCE COLUMNS TO BE TRANSFERRED TO AGE VIEW 'AS IS' }
  avCol: array[0..12] of integer = (3, 4, 5,  15, 17, 18, 19, 20, 22, 23, 24, 25, 27);     { DEFINES DESTINATION COLUMNS IN AGE VIEW ARRAY                }
  rnCol: array[0..7 ] of integer = (6, 7, 8,  9,  10, 11, 12, 13);                         { DEFINES BUCKET COLUMNS: NOT DUE, RANGE1..6, OVERDUE          }
  rfCol: array[0..11] of integer = (6, 7, 8,  9,  10, 11, 12, 13, 14, 15, 16, 26);         { DEFINES ALL COLUMNS WITH VALUES, TO BE REPLACED WITH '.'     }

(* NESTED METHODS *)

function bucket(pmtstat: integer; bias: integer): integer;
begin
  { 'PMTSTAT' IS COUNTED BETWEEN DUE DATE AND CURRENT DATE, THUS IT MUST BE CORRECTED FOR CUTOFF DATE. }
  { EXAMPLE BEING:                                                                                     }
  {   ON MONDAY WE MAKE AGING FOR FRIDAY, THRERFORE BIAS EQUALS 3 AND CUT-OFF IS 'CDATETIME' - 3.      }
  {   NON-MONDAY GOES WITH BIAS = 1, THUS I.E. WEDNESDAY - 1.                                          }
  {   HOWEVER, OPEN ITEM COLUMN 'PAYMENT STATUS' CONTAINS WITH CALCULATION:                            }
  {     'PMTSTAT' = DUE DATE - TODAY, THUS:                                                            }
  {     'PMTSTAT' = 2017-10-10 - 2017-10-23 = -13 (OVERDUE).                                           }
  {   CUT-OFF DATE IS 2017-10-23 (MONDAY) - 3 = 2017-10-20 (FRIDAY).                                   }
  {   THIS IS WHY WE COMPUTE 'PMTSTAT' = -13 + 3 = -10 (BETWEEN 8 AND 30).                             }
  pmtstat:=pmtstat + bias;
  Result:=0;
  { NOT DUE }
  if pmtstat >=0 then
  begin
    Result:=rnCol[0];
    exit;
  end;
  { OVERDUE RANGES 1..6 }
  if (abs(pmtstat) >= R1lo) and (abs(pmtstat) <= R1hi)   then result:=rnCol[1];
  if (abs(pmtstat) >= R2lo) and (abs(pmtstat) <= R2hi)   then result:=rnCol[2];
  if (abs(pmtstat) >= R3lo) and (abs(pmtstat) <= R3hi)   then result:=rnCol[3];
  if (abs(pmtstat) >= R4lo) and (abs(pmtstat) <= R4hi)   then result:=rnCol[4];
  if (abs(pmtstat) >= R5lo) and (abs(pmtstat) <= R5hi)   then result:=rnCol[5];
  if (abs(pmtstat) >= R6lo) {and (abs(pmtstat) <= R6hi)} then result:=rnCol[6];  { USE MORE THAN AS WE CAN HAVE ABOVE 365 DAYS }
end;

{ FILL ARRAY WITH ZEROES }
procedure AgeViewZeroFields(WhatRow: integer);
begin
  ArrAgeView[WhatRow, rnCol[0]]:='0';  { NOT DUE }
  ArrAgeView[WhatRow, rnCol[1]]:='0';  { RANGE 1 }
  ArrAgeView[WhatRow, rnCol[2]]:='0';  { RANGE 2 }
  ArrAgeView[WhatRow, rnCol[3]]:='0';  { RANGE 3 }
  ArrAgeView[WhatRow, rnCol[4]]:='0';  { RANGE 4 }
  ArrAgeView[WhatRow, rnCol[5]]:='0';  { RANGE 5 }
  ArrAgeView[WhatRow, rnCol[6]]:='0';  { RANGE 6 }
  ArrAgeView[WhatRow, rnCol[7]]:='0';  { OVERDUE }
end;

(* MAIN BLOCK *)

begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  RcLo :=0;
  RcHi :=0;
  avRow:=0;
  SetLength(ArrAgeView, 1, 28);  { MAKE 1 ROW AND 1..28 COLUMNS }
  { PUT ZERO FIELDS }
  AgeViewZeroFields(0);
  { ------------------------------------------------------------------------------------------------------------------------------------------  DATE AND TIME }
  DatTim:=DateToStr(Now) + ' ' + TimeToStr(Now);
  if SysUtils.DayOfWeek(Now) = 2 then bias:=3 else bias:=1;
  CutOff:=Now - bias;
  { -------------------------------------------------------------------------------------------------- REMOVE DUPLICATES AND MAKE AGE VIEW WITHOUT AGE VALUES }
  { WARNING! IT REQUIRES OPEN ITEMS "STRING GRID" TO BE SORTED BY    }
  {          SUPPORTED COLUMN "CUID", WE ASSUME THAT IS ALREADY DONE }
  {          AND WE START WITH "ONE" BECAUSE ZERO POINTS TO HEADERS  }
  {          IN "STRING GRID"                                        }
  for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
    { GO THROUGH THE ROWS AND POPULATE WHEN FIND THAT THE ROW BELOW IS DIFFERENT THAN CURRENT }
    if (MainForm.sgOpenItems.Cells[34, iCNT] <> MainForm.sgOpenItems.Cells[34, iCNT + 1]) then
    begin
      { ---------------------------------------------------------------------------------------------------------------------------------- FIXED DATA PER ROW }
      ArrAgeView[avRow, 0]:=GroupID;
      ArrAgeView[avRow, 1]:=DateToStr(CutOff);
      ArrAgeView[avRow, 2]:=DatTim;
      ArrAgeView[avRow, 26]:=' ';  { DUMMY FIELD AFTER EMBEDDED RISK CLASS }
      { ------------------------------------------------------------------------------------------------------------------------ RE-WRITE REST OF THE COLUMNS }
      for jCNT:=0 to high(oiCol) do ArrAgeView[avRow, avCol[jCNT]]:=MainForm.sgOpenItems.Cells[oiCol[jCNT], iCNT];
      { ---------------------------------------------------------------------------------------------------------------------------------------- ALTERNATIONS }
      { REMOVE "TAB" CHARACTER IF FOUND IN CUSTOMER NAME }
      ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], TAB, '', [rfReplaceAll]);
      { REPLACE SINGLE QUOTES TO DOUBLE QUOTES }
      ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], SingleQuote, DoubleQuote, [rfReplaceAll]);
      { REMOVE FROM CO CODE "F" PREFIX }
      ArrAgeView[avRow, 20]:=IntToStr((StrToInt(StringReplace(ArrAgeView[avRow, 20], 'F', '0', [rfReplaceAll]))));
      { LEDGER ISO }
      if MainForm.DetailsGrid.Cells[0, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.DetailsGrid.Cells[0, 1];
      if MainForm.DetailsGrid.Cells[1, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.DetailsGrid.Cells[1, 1];
      if MainForm.DetailsGrid.Cells[2, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.DetailsGrid.Cells[2, 1];
      if MainForm.DetailsGrid.Cells[3, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.DetailsGrid.Cells[3, 1];
      { -------------------------------------------------------------------------------------------------------------------------------------------- COUNTERS }
      { MOVE COUNTER }
      inc(avRow);
      { EXPAND ARRAY BY ONE EMPTY ROW }
      SetLength(ArrAgeView, avRow + 1, 28);
      { ZERO FIELDS }
      AgeViewZeroFields(avRow);
    end;

  { ---------------------------------------------------------------------------------------------------------------------------- RANGES AND RISK CLASS BOUNDS }
  AppSettings:=TSettings.Create;
  try
    { LOWER BOUNDS }
    R1lo:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE1A', 0);
    R2lo:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE2A', 0);
    R3lo:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE3A', 0);
    R4lo:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE4A', 0);
    R5lo:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE5A', 0);
    R6lo:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE6A', 0);
    { UPPER BOUNDS }
    R1hi:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE1B', 0);
    R2hi:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE2B', 0);
    R3hi:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE3B', 0);
    R4hi:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE4B', 0);
    R5hi:=AppSettings.TMIG.ReadInteger(AgingRanges, 'RANGE5B', 0);
  finally
    AppSettings.Free;
  end;

  { ------------------------------------------------------------------------------------------------------------------------------------------------ POPULATE }
  { LOOP VIA CUID COLUMN IN AGE VIEW [27] }
  for exRow:=0 to avRow - 1 do
  begin
    DiscAmnt:=0;
    { LOOP VIA CUID COLUMN IN OPEN ITEMS [34] }
    for iCNT:=0 to MainForm.sgOpenItems.RowCount - 1 do
    begin
      { COMPARE AND EXECUTE IF THE SAME }
      if MainForm.sgOpenItems.Cells[34, iCNT] = ArrAgeView[exRow, 27] then
      begin
        { SUM ITEMS: NOT DUE, RANGE1..6 }
        ArrAgeView[exRow,
                    bucket(
                            StrToInt(MainForm.sgOpenItems.Cells[33, iCNT]),
                            bias
                          )
                  ]:=FloatToStr(
                                 StrToFloat(ArrAgeView[exRow, bucket(StrToInt(MainForm.sgOpenItems.Cells[33, iCNT]), bias)]) +
                                 StrToFloat(MainForm.sgOpenItems.Cells[5, iCNT])
                               );
        { SUM ITEMS: DISCOUNTED AMOUNT [34] | TECHNICAL VARIABLE }
        DiscAmnt:=DiscAmnt + StrToFloat(MainForm.sgOpenItems.Cells[34, iCNT]);
      end;
    end;
    { CALCULATE TOTAL AMOUNT [14] }
    ArrAgeView[exRow, 14]:=FloatToStr(
                        StrToFloat(ArrAgeView[exRow, rnCol[0]]) +
                        StrToFloat(ArrAgeView[exRow, rnCol[1]]) +
                        StrToFloat(ArrAgeView[exRow, rnCol[2]]) +
                        StrToFloat(ArrAgeView[exRow, rnCol[3]]) +
                        StrToFloat(ArrAgeView[exRow, rnCol[4]]) +
                        StrToFloat(ArrAgeView[exRow, rnCol[5]]) +
                        StrToFloat(ArrAgeView[exRow, rnCol[6]])
                        );
    { TOTAL OVERDUE [13] = TOTAL AMOUNT [14] - NOT DUE [6] }
    ArrAgeView[exRow, 13]:=FloatToStr(
                                       StrToFloat(ArrAgeView[exRow, 14]) -
                                       StrToFloat(ArrAgeView[exRow,  6])
                                     );
    { EXCEEDED AMOUNT [16] = CREDIT LIMIT [15] - TOTAL AMOUNT [14] }
    ArrAgeView[exRow, 16]:=FloatToStr(StrToFloat(ArrAgeView[exRow, 15]) - StrToFloat(ArrAgeView[exRow, 14]));

  end;
  { --------------------------------------------------------------------------------------------------------------------------------------- DECIMAL SEPARATOR }
  if FormatSettings.DecimalSeparator = ',' then
    for exRow:=0 to avRow - 1 do
      for jCNT:=0 to high(rfCol) do
        { REPLACE ',' TO '.' FOR ALL VALUES [6..14] AND [25..26] }
        ArrAgeView[exRow, rfCol[jCNT]]:=StringReplace(ArrAgeView[exRow, rfCol[jCNT]], ',', '.', [rfReplaceAll]);
end;

{ -------------------------------------------------------------------------------------------------------------------------- TRANSFER 'AGEVIEW' TO SQL SERVER }
procedure TAgeView.Write(DestTable: string; SourceArray: TLists);
var
  Transaction:  string;
  DeleteData:   string;
  Condition:    string;
begin
  { ASSIGN COLUMNS | ADD ALL BUT ID COLUMN }
  Columns.Add(TSnapshots.GROUP_ID);
  Columns.Add(TSnapshots.AGE_DATE);
  Columns.Add(TSnapshots.SNAPSHOT_DT);
  Columns.Add(TSnapshots.CUSTOMER_NAME);
  Columns.Add(TSnapshots.CUSTOMER_NUMBER);
  Columns.Add(TSnapshots.COUNTRY_CODE);
  Columns.Add(TSnapshots.NOT_DUE);
  Columns.Add(TSnapshots.RANGE1);
  Columns.Add(TSnapshots.RANGE2);
  Columns.Add(TSnapshots.RANGE3);
  Columns.Add(TSnapshots.RANGE4);
  Columns.Add(TSnapshots.RANGE5);
  Columns.Add(TSnapshots.RANGE6);
  Columns.Add(TSnapshots.OVERDUE);
  Columns.Add(TSnapshots.TOTAL);
  Columns.Add(TSnapshots.CREDIT_LIMIT);
  Columns.Add(TSnapshots.EXCEEDED_AMOUNT);
  Columns.Add(TSnapshots.PAYMENT_TERMS);
  Columns.Add(TSnapshots.AGENT);
  Columns.Add(TSnapshots.DIVISION);
  Columns.Add(TSnapshots.CO_CODE);
  Columns.Add(TSnapshots.LEDGER_ISO);
  Columns.Add(TSnapshots.INF4);
  Columns.Add(TSnapshots.INF7);
  Columns.Add(TSnapshots.PERSON);
  Columns.Add(TSnapshots.GROUP3);
  Columns.Add(TSnapshots.RISK_CLASS);
  Columns.Add(TSnapshots.CUID);

  { DELETE STATEMENT | REMOVE OLD DATA }
  DeleteData:=DELETE_FROM + DestTable;
  Condition:=TSnapshots.GROUP_ID + EQUAL + QuotedStr(SourceArray[0, 0]) + _AND + TSnapshots.AGE_DATE + EQUAL + QuotedStr(LeftStr(SourceArray[0, 1], 10));
(*
  { INSERT STATEMENT | INSERT NEW DATA }
  Transaction:=TransactSnap;
  Transaction:=StringReplace(Transaction, '{DestTable}',   DestTable,   [rfReplaceAll]);
  Transaction:=StringReplace(Transaction, '{Condition}',   Condition,   [rfReplaceAll]);
  Transaction:=StringReplace(Transaction, '{DeleteData}',  DeleteData,  [rfReplaceAll]);
  Transaction:=StringReplace(
        Transaction,
        '{InsertNew}',
        ArrayToSql(SourceArray, DestTable, ColumnsToList(Columns, enQuotesOff)),
        [rfReplaceAll]
  );
*)
  { EXECUTE }
  StrSQL:=Transaction;
  try
    MainForm.ExecMessage(False, mcStatusBar, stSQLupdate);
    ExecSQL;
  except
    on E: Exception do
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot send to server. Error has been thrown: ' + E.Message);
  end;
  LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Age View transferred to Microsoft SQL Server. Rows affected: ' + IntToStr(High(SourceArray)) + '.');
end;

{ ----------------------------------------------------------------------------------------------------------------------------- EXPORT ARRAY DATA TO CSV FILE }
procedure TAgeView.ExportToCSV(FileName: string; SourceArray: TLists);
var
  iCNT:    integer;
  jCNT:    integer;
  SL:      TStringList;
  TempStr: string;
begin
  SL:=TStringList.Create;
  SL.Clear;
  try
    for iCNT:=0 to High(SourceArray) - 1 do
    begin
      for jCNT:=0 to High(SourceArray[1]) do TempStr:=TempStr + SourceArray[iCNT, jCNT] + ';';
      SL.Add(TempStr);
      TempStr:='';
    end;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

end.
