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
unit AgeView;

interface

uses
  Main;

{ ------------------------------------------------------------- ! AGE VIEW CLASS ! -------------------------------------------------------------------------- }
type                                                   (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TAgeView = class(TObject)
  {$TYPEINFO ON}
  public
    (* VARIABLES UPDATED BY WORKER THREAD AND USED BY MAIN THREAD *)
    ArrAgeView   : TStrArray;
    CustAll      : integer;
    CallsAll     : integer;
    EmailsAll    : integer;
    NotDue       : extended;
    Range1       : extended;
    Range2       : extended;
    Range3       : extended;
    Range4       : extended;
    Range5       : extended;
    Range6       : extended;
    Balance      : extended;
    Limits       : extended;
    Exceeders    : integer;
    TotalExceed  : extended;
    RCA          : extended;
    RCB          : extended;
    RCC          : extended;
  published
    (* RUN IN WORKER THREAD ONLY *)
    procedure   Read(GroupID: string; AgeDate: TDateTime; idThd: integer);                     //refactor!!
    procedure   Make(GroupID: string; OSAmount: double; idThd: integer);                       //ok
    procedure   Write(DestTable: string; idThd: integer);                                      //refactor!!
    procedure   Details(GroupID: string; AgeDate: TDateTime; idThd: integer);                  //refactor!!
    function    MapData(AgeGrid: TStringGrid; WhichCol: string; tblMap: TStringGrid): string;  //refactor!!
    function    GetCoCode(ListPos: integer; CoPos: integer; Mode: integer): string;
  end;

implementation

{ ############################################################## ! AGE VIEW CLASS ! ######################################################################### }

(* ******************************************************* ! AGE VIEW FOR GIVEN GROUP ID ! ********************************************************************

IMPORTANT NOTE:
---------------

DATA SNAPSHOT FOR AGING CONTAINS WITH 0..28 COLUMNS. HOWEVER, FOR USER 'AGING VIEW' WE SHOW ONLY 0..25 COLUMNS.

DATA SNAPSHOT   | NR | AGE VIEW COLUMN | NOTE
----------------|----|-----------------|-------------------------------------------------------------------------------
ID              | 0  |                 | TECHNICAL COLUMN (DO NOT QUERY)
GROUP_ID        | 1  |                 | TECHNICAL COLUMN (DO NOT QUERY)
AGE_DATE        | 2  |                 | TECHNICAL COLUMN (DO NOT QUERY)
SNAPSHOT_DT     | 3  |                 | TECHNICAL COLUMN (DO NOT QUERY)
CUSTOMER_NAME   | 4  |  2              | RENAME AS PER GENERAL SETTINGS
CUSTOMER_NUMBER | 5  |  1              | RENAME AS PER GENERAL SETTINGS
COUNTRY_CODE    | 6  |  16             | RENAME AS PER GENERAL SETTINGS
NOT_DUE         | 7  |  3              | RENAME AS PER GENERAL SETTINGS
RANGE1          | 8  |  4              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE2          | 9  |  5              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE3          | 10 |  6              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE4          | 11 |  7              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE5          | 12 |  8              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
TOTAL           | 13 |  9              | RENAME AS PER GENERAL SETTINGS
CREDIT_LIMIT    | 14 |  10             | RENAME AS PER GENERAL SETTINGS
EXCEEDED_AMOUNT | 15 |  11             | RENAME AS PER GENERAL SETTINGS
PAYMENT_TERMS   | 16 |  17             | RENAME AS PER GENERAL SETTINGS
AGENT           | 17 |  14             | RENAME AS PER GENERAL SETTINGS
DIVISION        | 18 |  15             | RENAME AS PER GENERAL SETTINGS
CO_CODE         | 19 |  13             | RENAME AS PER GENERAL SETTINGS
LEDGER_ISO      | 20 |  12             | RENAME AS PER GENERAL SETTINGS
INF4            | 21 |  18             | RENAME AS PER GENERAL SETTINGS
INF7            | 22 |  19             | RENAME AS PER GENERAL SETTINGS
PERSON          | 23 |  20             | RENAME AS PER GENERAL SETTINGS
GROUP3          | 24 |  21             | RENAME AS PER GENERAL SETTINGS
RISK_CLASS      | 25 |  22             | RENAME AS PER GENERAL SETTINGS
QUALITY_IDX     | 26 |  23             | RENAME AS PER GENERAL SETTINGS, TECHNICAL COLUMN, CAN BE HIDDEN
WALLET_SHARE    | 27 |  24             | RENAME AS PER GENERAL SETTINGS, TECHNICAL COLUMN, CAN BE HIDDEN
CUID            | 28 |  25             | UNIQUE CUSTOMER ID ACROSS ALL ITEMS IN THE TABLE PER GIVEN DAY, IT CAN BE RENAMED AND CAN BE HIDDEN

************************************************************************************************************************************************************ *)

procedure TAgeView.Read(GroupID: string; AgeDate: TDateTime; idThd: integer);  (* ASYNC *)
var
  { COUNTER }
  iCNT:          integer;
  jCNT:          integer;
  { SQL }
  StrSQL:        string;
  StrCol:        string;
  Query:         TADOQuery;
  DataBase:      TDataBase;
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------- DISABLE DRAWING }
  MainForm.sgAgeView.Freeze(True);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  { TOTALS }
  AgeView.CustAll    :=0;
  AgeView.CallsAll   :=0; { <== QUERY SEPARATE TABLE THAT HOLDS EMAILS AND CALLS FOR SPECIFIC DATE AND GROUP NUMBER }
  AgeView.EmailsAll  :=0; { <== QUERY SEPARATE TABLE THAT HOLDS EMAILS AND CALLS FOR SPECIFIC DATE AND GROUP NUMBER }
  { AMOUNTS }
  AgeView.NotDue     :=0;
  AgeView.Range1     :=0;
  AgeView.Range2     :=0;
  AgeView.Range3     :=0;
  AgeView.Range4     :=0;
  AgeView.Range5     :=0;
  AgeView.Range6     :=0;
  AgeView.Balance    :=0;
  AgeView.Limits     :=0;
  AgeView.Exceeders  :=0;
  AgeView.TotalExceed:=0;
  AgeView.RCA        :=0;
  AgeView.RCB        :=0;
  AgeView.RCC        :=0;
  { ------------------------------------------------------------------------------------------------------------------------------------ BUILD SQL EXPRESSION }
  { READ GRID LAYOUT, HOLDS COLUMN ORDER TO BE PASSED AS PARAMETER TO SQL EXPRESSION }
  MainForm.sgAgeView.LoadLayout(StrCol, ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);

  (* DISPLAY COLUMNS FROM TABLE THAT HOLDS AGE DETAILS AND FOLLOWUP COLUMN *)
  (* WARNING! WE ALWAYS USE LEFT JOIN ON TWO TABLES                        *)

  StrSQL:='SELECT '                                 +
             StrCol                                 +
          'FROM '                                   +
            'tbl_snapshots '                        +
          'LEFT JOIN '                              +
          '  tbl_general '                          +
          'ON '                                     +
          '  tbl_snapshots.cuid = tbl_general.cuid '+
          'WHERE '                                  +
            'tbl_snapshots.GROUP_ID = '             + QuotedStr(GroupID) + ' ' +
            'AND tbl_snapshots.AGE_DATE =  '        + QuotedStr(DateToStr(AgeDate)) + ' ' +
          'ORDER BY '                               +
            '(CASE WHEN tbl_general.FOLLOWUP IS NULL THEN 1 ELSE 0 END) ASC, ' +
            'tbl_snapshots.RANGE6 DESC, ' +
            'tbl_snapshots.RANGE5 DESC, ' +
            'tbl_snapshots.RANGE4 DESC, ' +
            'tbl_snapshots.RANGE3 DESC, ' +
            'tbl_snapshots.RANGE2 DESC, ' +
            'tbl_snapshots.RANGE1 DESC; ' ;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- NEW QUERY }
  { QUERY WITH GIVEN SQL EXPRESSION AND PARAMETERS }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=MainForm.ADOConnect;
  try
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    { EXECUTE SQL }
    Query.Open;
    { CLEAR 'STRINGGRID' }
    for iCNT:=1 to MainForm.sgAgeView.RowCount do for jCNT:=1 to MainForm.sgAgeView.ColCount do MainForm.sgAgeView.Cells[jCNT, iCNT]:='';
    { ASSIGN COLUMNS [FIELDS] AND ROWS [RECORDS] }
    iCNT:=1;
    MainForm.sgAgeView.RowCount:=Query.RecordCount + 1;
    MainForm.sgAgeView.ColCount:=Query.Recordset.Fields.Count + 1;
    { MOVE DATA FROM RECORDSET TO 'STRINGGRID' }
    while not Query.Recordset.EOF do
    begin
      for jCNT:=1 to Query.Recordset.Fields.Count do MainForm.sgAgeView.Cells[jCNT, iCNT]:=VarToStr(Query.Recordset.Fields[jCNT - 1].Value);
      Query.Recordset.MoveNext;
      inc(iCNT);
    end;
  finally
    Query.Close;
    Query.Free;
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: SQL statement applied [' + StrSQL + '].');
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: SQL statement parameters [uParam1 = ' + GroupID + '], [uParam2 = ' + DateToStr(AgeDate) + '].');
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- CALCULATE VALUES }
  for iCNT:=1 to MainForm.sgAgeView.RowCount - 1 do
  begin
    { NOT DUE & RANGE1..5 }
    AgeView.NotDue:=AgeView.NotDue + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('NOT DUE', 1, 1), iCNT], 0);
    AgeView.Range1:=AgeView.Range1 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('1 - 7',  1, 1), iCNT], 0);
    AgeView.Range2:=AgeView.Range2 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('8 - 30',  1, 1), iCNT], 0);
    AgeView.Range3:=AgeView.Range3 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('31 - 60',  1, 1), iCNT], 0);
    AgeView.Range4:=AgeView.Range4 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('61 - 90',  1, 1), iCNT], 0);
    AgeView.Range5:=AgeView.Range5 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('91 - 120',  1, 1), iCNT], 0);
    AgeView.Range6:=AgeView.Range6 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('121 - oo',  1, 1), iCNT], 0);
    { TOTAL AMOUNT | LEDGER BALANCE }
    AgeView.Balance:=AgeView.Balance + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    { GRANTED LIMITS | SUM OF ALL LIMITS }
    AgeView.Limits:=AgeView.Limits + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CREDIT LIMIT', 1, 1), iCNT], 0);
    { EXCEEDERS }
    if StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('EXCEEDED AMOUNT', 1, 1), iCNT], 0) < 0 then
    begin
      { COUNT EXCEEDERS }
      inc(AgeView.Exceeders);
      { SUM ALL EXCEEDERS AMOUNT }
      AgeView.TotalExceed:=AgeView.TotalExceed + Abs(StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('EXCEEDED AMOUNT', 1, 1), iCNT], 0));
    end;
    { SUM ALL ITEMS FOR RISK CLASSES }
    if MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('RISK CLASS', 1, 1), iCNT] = 'A' then AgeView.RCA:=AgeView.RCA + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    if MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('RISK CLASS', 1, 1), iCNT] = 'B' then AgeView.RCB:=AgeView.RCB + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    if MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('RISK CLASS', 1, 1), iCNT] = 'C' then AgeView.RCC:=AgeView.RCC + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    { COUNT ITEMS }
    inc(AgeView.CustAll);
  end;
  MainForm.sgAgeView.DefaultRowHeight:=17;
  DataBase:=TDataBase.Create(False);
  if UpperCase(MainForm.AccessMode) = 'BASIC' then MainForm.Action_BasicViewClick(self);
  if UpperCase(MainForm.AccessMode) = 'FULL'  then MainForm.Action_FullViewClick(self);
  FreeAndNil(DataBase);
  { ------------------------------------------------------------------------------------------------------------------------------------------ ENABLE DRAWING }
  MainForm.sgAgeView.Freeze(False);
end;

(* ********************************************************* ! PRE-PARE AGE VIEW ! ****************************************************************************

NOTES:
------

S - SOURCE TO BE MOVE 'AS IS'
D - DESTINATION
C - CALCULATED 'ON THE FLY'

WARNING!

IF NUMBERS ARE PROVIDED WITH NON-ENGLISH FORMAT '100000,00', THEN WE MUST REPLACE IT BY DOT DECIMAL SEPARATOR TO SENT SQL QUERY.
SUCH REPLACEMENT CAN BE OMITTED IF SOURCE IS ALREADY PRESENTED WITH DECIMAL POINT SEPARATOR.

OPEN ITEMS OWNLOADED FROM SOURCE FILE | AGE VIEW MADE FROM OPEN ITEMS      | COLUMN     | WORKER ARRAY
--------------------------------------|------------------------------------|------------|---------------
COLUMN NUMBER   | FIELD NAME          | COLUMN NUMBER   | FIELD NAME       | ASSIGNMENT | COLUMN NUMBER
----------------|---------------------|-----------------|------------------|------------|---------------
 0              | LP                  |  0              | ID               |            | -
 1            S | Co Code             |  1              | GROUP_ID         |            | 0
 2            S | Cust. Number        |  2              | AGE_DATE         |            | 1
 3              | Voucher Type        |  3              | SNAPSHOT_DT      |            | 2
 4              | Voucher Date        |  4              | CUSTOMER_NAME    | D          | 3
 5              | O/S in currency     |  5              | CUSTOMER_NUMBER  | D          | 4
 6              | O/S amount          |  6              | COUNTRY_CODE     | D          | 5
 7            S | Cust. Name          |  7              | NOT_DUE          | C          | 6
 8              | Currency            |  8              | RANGE1           | C          | 7
 9              | Amount in currency  |  9              | RANGE2           | C          | 8
 10             | Amount              |  10             | RANGE3           | C          | 9
 11             | Invoice Number      |  11             | RANGE4           | C          | 10
 12             | Due Date            |  12             | RANGE5           | C          | 11
 13           S | INF4                |  13             | RANGE6           | C          | 12
 14           S | INF7                |  14             | OVERDUE          | C          | 13
 15           S | Credit Limit        |  15             | TOTAL            | C          | 14
 16           S | Country Code        |  16  NULLABLE   | CREDIT_LIMIT     | D          | 15
 17           S | Payment Terms       |  17  NULLABLE   | EXCEEDED_AMOUNT  | C          | 16
 18             | Paid Info           |  18  NULLABLE   | PAYMENT_TERMS    | D          | 17
 19           S | Agent               |  19             | AGENT            | D          | 18
 20             | Control Status      |  20             | DIVISION         | D          | 19
 21             | Address 1           |  21  NULLABLE   | CO_CODE          | D          | 20
 22             | Address 2           |  22  NULLABLE   | LEDGER_ISO       | C          | 21
 23             | Address 3           |  23  NULLABLE   | INF4             | D          | 22
 24             | Postal Number       |  24  NULLABLE   | INF7             | D          | 23
 25             | Postal Area         |  25             | PERSON           | D          | 24
 26             | GL Account          |  26             | GROUP3           | D          | 25
 27             | Value Date          |  27  NULLABLE   | RISK_CLASS       | C          | 26
 28           S | Division            |  28  NULLABLE   | QUALITY_IDX      | C          | 27
 29           S | Group 3             |  29             | WALET_SHARE      | C          | 28
 30             | Text                | -               | CUID             | D          | 29   6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 26, 27, 28
 31           S | Persons             | -               | -                |            |
 32             | Direct Debit        | -               | -                |            |
 33             | AddTxt              | -               | -                |            |
 34             | Payment Status      | -               | -                |            |
 35             | Discounted Amount   | -               | -                |            |
 36             | Decrease in Value   | -               | -                |            |
 37             | Recover Value       | -               | -                |            |
 38           S | CUID                | -               | -                |            |

************************************************************************************************************************************************************ *)

procedure TAgeView.Make(GroupID: string; OSAmount: double; idThd: integer);  (* ASYNC *)
{ ------------------------------------------------------ ! MAIN VARIABLES AND CONSTANTS ! ------------------------------------------------------------------- }
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
  R6hi:      integer;                   { EXAMPLE: OO  }
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
  oiCol: array[0..12] of integer = (1,  2, 7, 13, 14, 15, 16, 17, 19, 28, 29, 31, 38);    { DEFINES SOURCE COLUMNS TO BE TRANSFERRED TO AGE VIEW 'AS IS' }
  avCol: array[0..12] of integer = (20, 4, 3, 22, 23, 15,  5, 17, 18, 19, 25, 24, 29);    { DEFINES DESTINATION COLUMNS IN AGE VIEW ARRAY                }
  rnCol: array[0..7]  of integer = (6, 7, 8, 9, 10, 11, 12, 13);                          { DEFINES BUCKET COLUMNS: NOT DUE, RANGE1..6, OVERDUE          }
  rfCol: array[0..13] of integer = (6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 26, 27, 28);  { DEFINES ALL COLUMNS WITH VALUES, TO BE REPLACED WITH '.'     }
{ ------------------------------------------------------------- ! INNER FUNCTION ! -------------------------------------------------------------------------- }
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
{ ------------------------------------------------------------------------------------------------------------------------------------ FILL ARRAY WITH ZEROES }
procedure AgeViewZeroFields(WhatRow: integer);
begin
  AgeView.ArrAgeView[WhatRow, rnCol[0]]:='0';  { NOT DUE }
  AgeView.ArrAgeView[WhatRow, rnCol[1]]:='0';  { RANGE 1 }
  AgeView.ArrAgeView[WhatRow, rnCol[2]]:='0';  { RANGE 2 }
  AgeView.ArrAgeView[WhatRow, rnCol[3]]:='0';  { RANGE 3 }
  AgeView.ArrAgeView[WhatRow, rnCol[4]]:='0';  { RANGE 4 }
  AgeView.ArrAgeView[WhatRow, rnCol[5]]:='0';  { RANGE 5 }
  AgeView.ArrAgeView[WhatRow, rnCol[6]]:='0';  { RANGE 6 }
  AgeView.ArrAgeView[WhatRow, rnCol[7]]:='0';  { OVERDUE }
end;
{ ------------------------------------------------------------------------------------------------------------------------------------------------ QUICK SORT }
procedure QuickSortR(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
{ 'A' VARIABLE HOLDS NUMERICAL DATA TO BE SORTED. 'L' VARIABLE IS ASSOCIATED COLUMN WITH ORIGINAL LIST POSITION. THE SECOND ASSOCIATED COLUMN FOLLOWS   }
{ 'A' COLUMN, BUT IT IS NOT SORTED. IT ALLOWS TO ASSIGN SORTED VALUES BACK TO ORIGINAL LIST POSITION AFTER COMPUTATION IS DONE. THIS IS TO BE USED WHEN }
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
   if Hi > iLo then QuickSortR(A, L, iLo, Hi, ASC);
   if Lo < iHi then QuickSortR(A, L, Lo, iHi, ASC);
end;
{ --------------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------- }
begin
  AppSettings:=TSettings.Create;
  { ----------------------------------------------------------------------------------------------------------------------------------------- DISPLAY MESSAGE }
  PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Generating age view...')));
  LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Generating age view...');
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  RcLo :=0;
  RcHi :=0;
  avRow:=0;
  SetLength(AgeView.ArrAgeView, 1, 30);  { MAKE 1 ROW AND 1..30 COLUMNS }
  { PUT ZERO FIELDS }
  AgeViewZeroFields(0);
  { ------------------------------------------------------------------------------------------------------------------------------------------  DATE AND TIME }
  DatTim:=DateToStr(Now) + ' ' + TimeToStr(Now);
  if SysUtils.DayOfWeek(Now) = 2 then bias:=3 else bias:=1;
  CutOff:=Now - bias;
  { -------------------------------------------------------------------------------------------------- REMOVE DUPLICATES AND MAKE AGE VIEW WITHOUT AGE VALUES }
  { WARNING! IT REQUIRES OPEN ITEMS 'STRINGGRID' TO BE SORTED BY    }
  {          SUPPORTED COLUMN 'UID', WE ASSUME THAT IS ALREADY DONE }
  {          AND WE START WITH "ONE" BECAUSE ZERO POINTS TO HEADERS }
  {          IN 'STRINGGRID'                                        }
  for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
    { GO THROUGH THE ROWS AND POPULATE WHEN FIND THAT THE ROW BELOW IS DIFFERENT THAN CURRENT }
    if (MainForm.sgOpenItems.Cells[38, iCNT] <> MainForm.sgOpenItems.Cells[38, iCNT + 1]) then
    begin
      { ---------------------------------------------------------------------------------------------------------------------------------- FIXED DATA PER ROW }
      AgeView.ArrAgeView[avRow, 0]:=GroupID;
      AgeView.ArrAgeView[avRow, 1]:=DateToStr(CutOff);
      AgeView.ArrAgeView[avRow, 2]:=DatTim;
      { ------------------------------------------------------------------------------------------------------------------------ RE-WRITE REST OF THE COLUMNS }
      for jCNT:=0 to high(oiCol) do AgeView.ArrAgeView[avRow, avCol[jCNT]]:=MainForm.sgOpenItems.Cells[oiCol[jCNT], iCNT];
      { ---------------------------------------------------------------------------------------------------------------------------------------- ALTERNATIONS }
      { REMOVE 'TAB' CHARACTER IF FOUND IN CUSTOMER NAME }
      AgeView.ArrAgeView[avRow, 3]:=StringReplace(AgeView.ArrAgeView[avRow, 3], #9, '', [rfReplaceAll]);
      { REPLACE SINGLE QUOTES TO DOUBLE QUOTES }
      AgeView.ArrAgeView[avRow, 3]:=StringReplace(AgeView.ArrAgeView[avRow, 3], '''', '''''', [rfReplaceAll]);
      { REMOVE FROM CO CODE 'F' PREFIX }
      AgeView.ArrAgeView[avRow, 20]:=OpenItems.ConvertName(MidStr(AgeView.ArrAgeView[avRow, 20], 2, 5), '', 2);
      { LEDGER ISO }
      if MainForm.COC1.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR1.Text;
      if MainForm.COC2.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR2.Text;
      if MainForm.COC3.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR3.Text;
      if MainForm.COC4.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR4.Text;
      { -------------------------------------------------------------------------------------------------------------------------------------------- COUNTERS }
      { MOVE COUNTER }
      inc(avRow);
      { EXPAND ARRAY BY ONE EMPTY ROW }
      SetLength(AgeView.ArrAgeView, avRow + 1, 30);
      { ZERO FIELDS }
      AgeViewZeroFields(avRow);
    end;
  { ------------------------------------------------------------------------------------------------------------------ CALCULATE VALUES FOR GIVEN AGE BUCKETS }
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
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
  { ------------------------------------------------------------------------------------------------------------------------------------------------ POPULATE }
  { LOOP VIA CUID COLUMN IN AGE VIEW [29] }
  for exRow:=0 to avRow - 1 do
  begin
    DiscAmnt:=0;
    { LOOP VIA CUID COLUMN IN OPEN ITEMS [38] }
    for iCNT:=0 to MainForm.sgOpenItems.RowCount - 1 do
    begin
      { COMPARE AND EXECUTE IF THE SAME }
      if MainForm.sgOpenItems.Cells[38, iCNT] = AgeView.ArrAgeView[exRow, 29] then
      begin
        { SUM ITEMS: NOT DUE, RANGE1..5 }
        AgeView.ArrAgeView[exRow, bucket(StrToInt(MainForm.sgOpenItems.Cells[34, iCNT]), bias)]:=FloatToStr(
                                                                           StrToFloat(AgeView.ArrAgeView[exRow, bucket(StrToInt(MainForm.sgOpenItems.Cells[34, iCNT]), bias)]) +
                                                                           StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT])
                                                                           );
        { SUM ITEMS: DISCOUNTED AMOUNT [35] | TECHNICAL VARIABLE }
        DiscAmnt:=DiscAmnt + StrToFloat(MainForm.sgOpenItems.Cells[35, iCNT]);
      end;
    end;
    { CALCULATE TOTAL AMOUNT [14] }
    AgeView.ArrAgeView[exRow, 14]:=FloatToStr(
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[0]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[1]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[2]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[3]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[4]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[5]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[6]])
                        );
    { TOTAL OVERDUE [13] = TOTAL AMOUNT [14] - NOT DUE [6] }
    AgeView.ArrAgeView[exRow, 13]:=FloatToStr(
                        StrToFloat(AgeView.ArrAgeView[exRow, 14]) -
                        StrToFloat(AgeView.ArrAgeView[exRow,  6])
                        );
    { EXCEEDED AMOUNT [16] = CREDIT LIMIT [15] - TOTAL AMOUNT [14] }
    AgeView.ArrAgeView[exRow, 16]:=FloatToStr(StrToFloat(AgeView.ArrAgeView[exRow, 15]) - StrToFloat(AgeView.ArrAgeView[exRow, 14]));
    { WALLET SHARE [28] | TECHNICAL COLUMN }
    if OSAmount <> 0 then AgeView.ArrAgeView[exRow, 28]:=FloatToStrF(( (StrToFloat(AgeView.ArrAgeView[exRow, 14]) / OSAmount) * 1), ffFixed, 4, 4)
      else
        AgeView.ArrAgeView[exRow, 28]:='0';
    { CALCULATE QUALITY INDEX [27] }
    if OSAmount <> 0 then AgeView.ArrAgeView[exRow, 27]:=FloatToStrF(( 1 - (DiscAmnt / OSAmount) ), ffFixed, 6, 6)
      else
        AgeView.ArrAgeView[exRow, 27]:='0';
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------- RISK CLASS BOUNDS }
  if FormatSettings.DecimalSeparator = ',' then
  begin
    RcLo:=StrToFloat(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', '0,80'));     { EXAMPLE: 80% }
    RcHi:=StrToFloat(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', '0,80')) +    { EXAMPLE: 95% }
          StrToFloat(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_B_MAX', '0,15'));
  end;
  if FormatSettings.DecimalSeparator = '.' then
  begin
    RcLo:=StrToFloat(StringReplace(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', '0,80'), ',', '.', [rfReplaceAll]));
    RcHi:=StrToFloat(StringReplace(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', '0,80'), ',', '.', [rfReplaceAll])) +
          StrToFloat(StringReplace(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_B_MAX', '0,15'), ',', '.', [rfReplaceAll]));
  end;
  { DISPOSE SETTINGS OBJECT }
  FreeAndNil(AppSettings);
  { --------------------------------------------------------------------------------------------------------------------------------- RISK CLASS CALCULATIONS }
  SetLength(MyWallet, avRow);
  SetLength(MyList,   avRow);
  Count:=0;
  { MOVE REQUIRED ITEMS TO ARRAYS }
  for iCNT:=0 to avRow - 1 do
  begin
    MyList[iCNT]  :=iCNT;                                     { ORIGINAL LP  }
    MyWallet[iCNT]:=StrToFloat(AgeView.ArrAgeView[iCNT, 28]); { WALLET SHARE }
  end;
  { SORT DESCENDING VIA WALLET SHARTE }
  QuickSortR(MyWallet, MyList, Low(MyWallet), High(MyWallet), False);
  { CALCULATE AND ASSIGN }
  for iCNT:=0 to avRow - 1 do
  begin
    Count:=Count + MyWallet[iCNT];
    { ASSIGN RISK CLASS 'A' }
    if Count <= RcLo then   AgeView.ArrAgeView[MyList[iCNT], 26]:='A';
    { ASSIGN RISK CLASS 'B' }
    if (Count > RcLo)  and
       (Count <= RcHi) then AgeView.ArrAgeView[MyList[iCNT], 26]:='B';
    { ASSIGN RISK CLASS 'C' }
    if Count > RcHi then    AgeView.ArrAgeView[MyList[iCNT], 26]:='C';
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------- DECIMAL SEPARATOR }
  if FormatSettings.DecimalSeparator = ',' then
    for exRow:=0 to avRow - 1 do
      for jCNT:=0 to high(rfCol) do
        { REPLACE ',' TO '.' FOR ALL VALUES [6..14] AND [25..26] }
        AgeView.ArrAgeView[exRow, rfCol[jCNT]]:=StringReplace(AgeView.ArrAgeView[exRow, rfCol[jCNT]], ',', '.', [rfReplaceAll]);
end;

{ -------------------------------------------------------------------------------------------------------------------------- TRANSFER 'AGEVIEW' TO SQL SERVER }
procedure TAgeView.Write(DestTable: string; idThd: integer);  (* ASYNC *)  //refactor!!
const
  AllColumns = 'GROUP_ID,AGE_DATE,SNAPSHOT_DT,CUSTOMER_NAME,CUSTOMER_NUMBER,COUNTRY_CODE,NOT_DUE,RANGE1,RANGE2,RANGE3,RANGE4,RANGE5,RANGE6,OVERDUE,TOTAL,'+
               'CREDIT_LIMIT,EXCEEDED_AMOUNT,PAYMENT_TERMS,AGENT,DIVISION,CO_CODE,LEDGER_ISO,INF4,INF7,PERSON,GROUP3,RISK_CLASS,QUALITY_IDX,WALLET_SHARE,CUID';
var
  MSSQL:        TMSSQL;
  Transaction:  string;
  DeleteData:   string;
begin
  { INITIALIZE }
  MSSQL:=TMSSQL.Create(MainForm.ADOConnect);
  DeleteData:='DELETE FROM ' + DestTable + ' WHERE GROUP_ID = ' + QuotedStr(AgeView.ArrAgeView[0, 0]) + ' AND AGE_DATE = ' + QuotedStr(LeftStr(AgeView.ArrAgeView[0, 1], 10));
  try
    { BUILD AND EXECUTE }
    Transaction:=MSSQL.ArrayToSql(AgeView.ArrAgeView, DestTable, AllColumns);
    Transaction:='BEGIN TRANSACTION'                                              + #13#10 +
                 'SELECT TOP 1 * FROM ' + DestTable + ' WITH (TABLOCK, HOLDLOCK)' + #13#10 +
                 DeleteData                                                       + #13#10 +
                 Transaction                                                      + #13#10 +
                 'COMMIT TRANSACTION';
    { ASSIGN AND EXECUTE }
    MSSQL.StrSQL:=Transaction;
    try
      MSSQL.ExecSQL;
    except
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot send to server. Error has been thrown: ' + IntToStr(High(AgeView.ArrAgeView)) + '.');
    end;
  finally
    MSSQL.Free;
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Age View transferred to Microsoft SQL Server. Rows affected: ' + IntToStr(High(AgeView.ArrAgeView)) + '.');
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------- RETURN CO CODE AND CUREENCY }
procedure TAgeView.Details(GroupID: string; AgeDate: TDateTime; idThd: integer);  (* SYNCHRONIZE *)  // REFACTOR !!!!
var
  MSSQL :  TMSSQL;
  StrSQL:  string;
  RS    :  _Recordset;
begin
  StrSQL:='SELECT DISTINCT '             +
            'tbl_snapshots.CO_CODE,'     +
            'tbl_snapshots.LEDGER_ISO,'  +
            'tbl_company.INTEREST_RATE,' +
            'tbl_company.AGENTS '        +
          'FROM '                        +
            'tbl_snapshots '             +
          'LEFT JOIN '                   +
            'tbl_company '               +
          'ON '                          +
            'tbl_snapshots.CO_CODE = tbl_company.CO_CODE ' +
          'WHERE'                        +
            ' GROUP_ID = '               + QuotedStr(GroupID) +
            ' AND AGE_DATE = '           + QuotedStr(DateToStr(AgeDate));
  MSSQL:=TMSSQL.Create(MainForm.ADOConnect);
  try
    MSSQL.StrSQL:=StrSQL;
    RS:=MSSQL.ExecSQL;
    if (RS.RecordCount > 0) and (RS.RecordCount < 5)  then
    begin
      { 1ST SET }
      RS.MoveFirst;
      MainForm.COC1.Text:=RS.Fields.Item['CO_CODE'      ].Value;
      MainForm.CUR1.Text:=RS.Fields.Item['LEDGER_ISO'   ].Value;
      MainForm.INT1.Text:=RS.Fields.Item['INTEREST_RATE'].Value;
      MainForm.AGT1.Text:=RS.Fields.Item['AGENTS'       ].Value;
      { 2ND SET }
      RS.MoveNext;
      if (RS.EOF = False) and (RS.BOF = False) then
      begin
        MainForm.COC2.Text:=RS.Fields.Item['CO_CODE'      ].Value;
        MainForm.CUR2.Text:=RS.Fields.Item['LEDGER_ISO'   ].Value;
        MainForm.INT2.Text:=RS.Fields.Item['INTEREST_RATE'].Value;
        MainForm.AGT2.Text:=RS.Fields.Item['AGENTS'       ].Value;
        RS.MoveNext;
      end;
      { 3RD SET }
      if (RS.EOF = False) and (RS.BOF = False) then
      begin
        MainForm.COC3.Text:=RS.Fields.Item['CO_CODE'      ].Value;
        MainForm.CUR3.Text:=RS.Fields.Item['LEDGER_ISO'   ].Value;
        MainForm.INT3.Text:=RS.Fields.Item['INTEREST_RATE'].Value;
        MainForm.AGT3.Text:=RS.Fields.Item['AGENTS'       ].Value;
        RS.MoveNext;
      end;
      { 4TH SET }
      if (RS.EOF = False) and (RS.BOF = False) then
      begin
        MainForm.COC4.Text:=RS.Fields.Item['CO_CODE'      ].Value;
        MainForm.CUR4.Text:=RS.Fields.Item['LEDGER_ISO'   ].Value;
        MainForm.INT4.Text:=RS.Fields.Item['INTEREST_RATE'].Value;
        MainForm.AGT4.Text:=RS.Fields.Item['AGENTS'       ].Value;
      end;
    end;
  finally
    RS:=nil;
    MSSQL.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------- DISPLAY CO CODE AND CURRENCY }
  MainForm.tcCOCODE.Caption   :='';
  MainForm.tcCURRENCY.Caption :='';
  { CO CODE }
  if MainForm.COC1.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC1.Text + ' ';
  if MainForm.COC2.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC2.Text + ' ';
  if MainForm.COC3.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC3.Text + ' ';
  if MainForm.COC4.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC4.Text + ' ';
  { CURRENCY }
  if MainForm.CUR1.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR1.Text + ' ';
  if MainForm.CUR2.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR2.Text + ' ';
  if MainForm.CUR3.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR3.Text + ' ';
  if MainForm.CUR4.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR4.Text + ' ';
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- LOOK FOR DATA }
function TAgeView.MapData(AgeGrid: TStringGrid; WhichCol: string; tblMap: TStringGrid): string;
var
  iCNT:  integer;
  jCNT:  integer;
begin
  Result:='unassigned';
  { FIND GIVEN COLUMN }
  for jCNT:=1 to AgeGrid.ColCount - 1 do
    if WhichCol = AgeGrid.Cells[jCNT, 0] then Break;
  { FIND DATA }
  for iCNT:=1 to tblMap.RowCount - 1 do
  begin
    if AgeGrid.Cells[jCNT, AgeGrid.Row] = tblMap.Cells[2, iCNT] then
    begin
      if (tblMap.Cells[3, iCNT] = '') or (tblMap.Cells[3, iCNT] = ' ') then Result:='unassigned' else Result:=tblMap.Cells[3, iCNT];
      Break;
    end;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------- UAC | RETURN SPECIFIC 'COCOE' FROM THE LIST }
function TAgeView.GetCoCode(ListPos: integer; CoPos: integer; Mode: integer): string;
{ WARNING! GROUP ID FORMAT: SERIES OF 4 GROUPS OF 5 DIGITS, I.E.: '020470034000043' MUST BE READ AS FOLLOWS: }
{   1. 1ST CO CODE: 02047 (2047)                                                                             }
{   2. 2ND CO CODE: 00340 (340)                                                                              }
{   3. 3RD CO CODE: 00043 (43)                                                                               }
{   4. 4TH CO CODE: 00000 (0)                                                                                }
{ PARAMETERS:                                                                                                }
{   1. 'LISTPOS' = POSITION OF THE GROUP HOLING 'COCODES'                                                    }
{   2. 'COPOS'   = NUMBER OF THE 'COCODE' TO BE RETURNED                                                     }
{   3. 'MODE'    = 0 (COCODE) OR 1 (GROUP NAME) OR 2 (GROUP ID)                                              }
begin
  { VALIDATE INPUT DATA }
  if ( ListPos > High(MainForm.ArrGroupList) ) or (CoPos > 4) then Exit;
  { EXTRACT | 'COCODE' FROM GROUP ID }
  if Mode = 0 then
  begin
    if CoPos = 1 then Result:=IntToStr(StrToInt(MidStr(MainForm.ArrGroupList[ListPos, Mode], 1,  5)));
    if CoPos = 2 then Result:=IntToStr(StrToInt(MidStr(MainForm.ArrGroupList[ListPos, Mode], 6,  5)));
    if CoPos = 3 then Result:=IntToStr(StrToInt(MidStr(MainForm.ArrGroupList[ListPos, Mode], 11, 5)));
    if CoPos = 4 then Result:=IntToStr(StrToInt(MidStr(MainForm.ArrGroupList[ListPos, Mode], 16, 5)));
  end;
  { EXTRACT | GROUP NAME }
  if Mode = 1 then Result:=MainForm.ArrGroupList[ListPos, Mode];
  { EXTRACT | FULL GROUP ID }
  if Mode = 2 then Result:=MainForm.ArrGroupList[ListPos, 0];
end;

end.
