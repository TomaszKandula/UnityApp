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


    // --------------------
    // Callback signatures.
    // --------------------

    TMakeAgeViewAsync = procedure(LastError: TLastError) of object;
    TReadAgeViewAsync = procedure(ActionMode: TLoading; ReturnedData: TStringGrid; LastError: TLastError) of object;


    IDebtors = interface(IInterface)
    ['{194FE2BE-386E-499E-93FB-0299DA53A70A}']
        procedure MakeAgeViewSQLAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure MakeAgeViewCSVAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    private
        function FMakeAgeView(OSAmount: double; GroupID: string; SourceGrid: TStringGrid): TALists;
        procedure FWriteAgeView(DestTable: string; GroupID: string; SourceArray: TALists);
    public
        procedure MakeAgeViewSQLAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure MakeAgeViewCSVAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
    end;


implementation


uses
    System.StrUtils,
    Handler.Database,
    Handler.Account,
    Handler.Sql,
    DbModel,
    Transactions,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.Sorting,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Sql,
    Sync.Documents,
    Async.OpenItems;


// ------------------------------------
// Make aging report for main view.
// *Remove when SQL is replaced by API.
// ------------------------------------

procedure TDebtors.MakeAgeViewSQLAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; Callback: TMakeAgeViewAsync);
begin

     var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        LastError.IsSucceeded:=True;
        try

            FWriteAgeView(
                TSnapshots.Snapshots,
                GroupID,
                FMakeAgeView(OpenAmount, GroupID, SourceGrid)
            );

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[MakeAgeViewSQLAsync] Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Make aging report to CSV file.
// *Remove when SQL is replaced by API.
// ------------------------------------

procedure TDebtors.MakeAgeViewCSVAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; Callback: TMakeAgeViewAsync);
begin

    //...

end;


// ------------------------------------
// Load aging report for main view.
// *Change when SQL is replaced by API.
// ------------------------------------

procedure TDebtors.ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var Grid: TStringGrid:=TStringGrid.Create(nil);
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            LastError.IsSucceeded:=True;
            try

                var StrCol: string;
                var CheckColumns:=Grid.LoadLayout(StrCol, TConfigSections.ColumnWidthName, TConfigSections.ColumnOrderName, TConfigSections.ColumnNames, TConfigSections.ColumnPrefix);

                if not CheckColumns then
                begin
                    LastError.IsSucceeded:=False;
                    LastError.ErrorMessage:='[ReadAgeViewAsync] Cannot load columns. Please contact IT support.';
                    ThreadFileLog.Log('[ReadAgeViewAsync] Cannot load columns. Please contact IT support.');
                end
                else
                begin

                    DataTables.CmdType:=cmdText;
                    DataTables.StrSQL:=
                        TSql.EXECUTE             +
                        'Customer.AgeViewReport' +
                        TChars.SPACE             +
                        QuotedStr(StrCol)        +
                        TChars.COMMA             +
                        QuotedStr(GroupIdSel)    +
                        TChars.COMMA             +
                        QuotedStr(AgeDateSel)    +
                        TChars.COMMA             +
                        QuotedStr(SortMode.ToString);

                    DataTables.SqlToGrid(Grid, DataTables.ExecSQL, False, False);
                    ThreadFileLog.Log('SQL statement applied [' + DataTables.StrSQL + '].');

                end;

            except
                on E: Exception do
                begin
                    LastError.IsSucceeded:=False;
                    LastError.ErrorMessage:='[ReadAgeViewAsync] Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log('[ReadAgeViewAsync] Cannot execute. Error has been thrown: ' + E.Message);
                end;

            end;

        finally
            DataTables.Free;
        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(ActionMode, Grid, LastError);
        end);

    end);

    NewTask.Start;

end;


(* ************************************************************************************************************************************************************

NOTES:
------

D - DESTINATION
C - CALCULATED 'ON THE FLY'

WARNING!

IF NUMBERS ARE PROVIDED WITH NON-ENGLISH FORMAT '100000,00', THEN WE MUST REPLACE IT BY POINT DECIMAL SEPARATOR TO SAFELY SEND TO THE SQL SERVER.
SUCH REPLACEMENT CAN BE OMITTED IF SOURCE IS ALREADY PRESENTED WITH DECIMAL POINT SEPARATOR.

OPEN ITEMS LOADED FROM SOURCE FILE    | AGE VIEW MADE FROM OPEN ITEMS      | COLUMN     | WORKER ARRAY
--------------------------------------|------------------------------------|------------|---------------
COLUMN NUMBER   | FIELD NAME          | ASSIGN NUMBER   | FIELD NAME       | ASSIGNMENT | COLUMN NUMBER
----------------|---------------------|-----------------|------------------|------------|---------------
 0              | ID                  | DO NOT USE      | ID               |            | DO NOT USE
 1              | SourceDBName        | -               | GROUP_ID         |            | 0
 2              | CustNo              | -               | AGE_DATE         |            | 1
 3              | VoTp                | -               | SNAPSHOT_DT      |            | 2
 4              | OpenCurAm           | -               | CUSTOMER_NAME    | D          | 3
 5              | OpenAm              | -               | CUSTOMER_NUMBER  | D          | 4
 6              | Nm                  | -               | COUNTRY_CODE     | D          | 5
 7              | ISO                 | -               | NOT_DUE          | C          | 6
 8              | CurAm               | -               | RANGE1           | C          | 7
 9              | Am                  | -               | RANGE2           | C          | 8
 10             | InvoNo              | -               | RANGE3           | C          | 9
 11             | DueDt               | -               | RANGE4           | C          | 10
 12             | Inf4                | -               | RANGE5           | C          | 11
 13             | Inf7                | -               | RANGE6           | C          | 12
 14             | CrLmt               | -               | OVERDUE          | C          | 13
 15             | Ctry                | -               | TOTAL            | C          | 14
 16             | CPmtTrm             | -               | CREDIT_LIMIT     | D          | 15
 17             | PdSts               | -               | EXCEEDED_AMOUNT  | C          | 16
 18             | Agent               | -               | PAYMENT_TERMS    | D          | 17
 19             | Ctrl                | -               | AGENT            | D          | 18
 20             | Ad1                 | -               | DIVISION         | D          | 19
 21             | Ad2                 | -               | CO_CODE          | D          | 20
 22             | Ad3                 | -               | LEDGER_ISO       | C          | 21
 23             | Pno                 | -               | INF4             | D          | 22
 24             | PArea               | -               | INF7             | D          | 23
 25             | GenAcNo             | -               | PERSON           | D          | 24
 26             | ValDt               | -               | GROUP3           | D          | 25
 27             | R1  (division)      | -               | RISK_CLASS       | C          | 26
 28             | Gr3                 | -               | CUID             | D          | 27
 29             | Txt                 | -               | SalesResponsible | D          | 28
 30             | R8  (person)        | -               | CustomerGroup    | D          | 29
 31             | DirDeb              | -               | PersonResponsible| D          | 30
 32             | AddTxt              | -               | AccountType      | D          | 31
 33             | PmtStat (calc)      | -               | -                |            |
 34             | CUID    (calc)      | -               | -                |            |
 35             | SalesResponsible    | -               | -                |            |
 36             | CustomerGroup       | -               | -                |            |
 37             | PersonResponsible   | -               | -                |            |
 38             | AccountType         | -               | -                |            |

************************************************************************************************************************************************************ *)

{ -------------------------------------------------------------------------------------------------------------------------------------------- GENERATE AGING }
function TDebtors.FMakeAgeView(OSAmount: double; GroupID: string; SourceGrid: TStringGrid): TALists;

    var

        ArrAgeView: TALists;

        // ----------------------
        // Internal row counters.
        // ----------------------

        iCNT:      integer;   // Number of rows of open items 'StringGrid'
        jCNT:      integer;   // Number of columns of open items 'StringGrid'
        avRow:     integer;   // Total rows in age view
        exRow:     integer;   // Row counter when filling age view with numbers

        // -------------------
        // Fixed data per row.
        // -------------------

        DatTim:    string;    // Current date and time
        CutOff:    TDateTime; // Cut-ff date, usually -1 OR -3

        // ----------------------------
        // Bucket ranges - lower bound.
        // ----------------------------

        R1lo:      integer;
        R2lo:      integer;
        R3lo:      integer;
        R4lo:      integer;
        R5lo:      integer;
        R6lo:      integer;

        // ----------------------------
        // Bucket ranges - upper bound.
        // ----------------------------

        R1hi:      integer;
        R2hi:      integer;
        R3hi:      integer;
        R4hi:      integer;
        R5hi:      integer;
        {R6hi:     integer;}

        bias:      integer;
        DiscAmnt:  double;

        // SETTINGS
        Settings:  ISettings;

    // WARNING! BELOW MUST BE ALIGNED WITH OPEN ITEMS SOURCE AND DESTINATION TABLE IN DATABASE FOR AGE VIEW
    const

        // DEFINES SOURCE COLUMNS TO BE TRANSFERRED TO AGE VIEW 'AS IS'
        oiCol: array[0..16] of integer = (6, 2, 15, 14, 16, 18, 27, 1,  12, 13, 30, 28, 34, 35, 36, 37, 38);

        // DEFINES DESTINATION COLUMNS IN AGE VIEW ARRAY
        avCol: array[0..16] of integer = (3, 4, 5, 15, 17, 18, 19, 20, 22, 23, 24, 25, 27, 28, 29, 30, 31);

        // DEFINES BUCKET COLUMNS (OUTPUT): NOT DUE, RANGE1..6, OVERDUE
        rnCol: array[0..7 ] of integer = (6, 7, 8, 9, 10, 11, 12, 13);

        // DEFINES ALL COLUMNS WITH VALUES, TO BE REPLACED WITH '.'
        rfCol: array[0..11] of integer = (6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 26);

    function bucket(pmtstat: integer; bias: integer): integer;
    begin

        // --------------------------------------------------------------------------------------------------
        // 'PMTSTAT' IS COUNTED BETWEEN DUE DATE AND CURRENT DATE, THUS IT MUST BE CORRECTED FOR CUTOFF DATE.
        // EXAMPLE BEING:
        //   ON MONDAY WE MAKE AGING FOR FRIDAY, THRERFORE BIAS EQUALS 3 AND CUT-OFF IS 'CDATETIME' - 3.
        //   NON-MONDAY GOES WITH BIAS = 1, THUS I.E. WEDNESDAY - 1.
        //   HOWEVER, OPEN ITEM COLUMN 'PAYMENT STATUS' CONTAINS WITH CALCULATION:
        //     'PMTSTAT' = DUE DATE - TODAY, THUS:
        //     'PMTSTAT' = 2017-10-10 - 2017-10-23 = -13 (OVERDUE).
        //   CUT-OFF DATE IS 2017-10-23 (MONDAY) - 3 = 2017-10-20 (FRIDAY).
        //   THIS IS WHY WE COMPUTE 'PMTSTAT' = -13 + 3 = -10 (BETWEEN 8 AND 30).
        // --------------------------------------------------------------------------------------------------

        pmtstat:=pmtstat + bias;
        Result:=0;

        // ---------------
        // Not due values.
        // ---------------

        if pmtstat >=0 then
        begin
            Result:=rnCol[0];
            exit;
        end;

        // --------------------
        // Overdue ranges 1..6.
        // --------------------

        if (abs(pmtstat) >= R1lo) and (abs(pmtstat) <= R1hi)   then result:=rnCol[1];
        if (abs(pmtstat) >= R2lo) and (abs(pmtstat) <= R2hi)   then result:=rnCol[2];
        if (abs(pmtstat) >= R3lo) and (abs(pmtstat) <= R3hi)   then result:=rnCol[3];
        if (abs(pmtstat) >= R4lo) and (abs(pmtstat) <= R4hi)   then result:=rnCol[4];
        if (abs(pmtstat) >= R5lo) and (abs(pmtstat) <= R5hi)   then result:=rnCol[5];
        if (abs(pmtstat) >= R6lo) {and (abs(pmtstat) <= R6hi)} then result:=rnCol[6];  // Use "more than" as we can have above 365 days

    end;

    // -----------------------
    // Fill array with zeroes.
    // -----------------------

    procedure AgeViewZeroFields(WhatRow: integer);
    begin
        ArrAgeView[WhatRow, rnCol[0]]:='0';  // NOT DUE
        ArrAgeView[WhatRow, rnCol[1]]:='0';  // RANGE 1
        ArrAgeView[WhatRow, rnCol[2]]:='0';  // RANGE 2
        ArrAgeView[WhatRow, rnCol[3]]:='0';  // RANGE 3
        ArrAgeView[WhatRow, rnCol[4]]:='0';  // RANGE 4
        ArrAgeView[WhatRow, rnCol[5]]:='0';  // RANGE 5
        ArrAgeView[WhatRow, rnCol[6]]:='0';  // RANGE 6
        ArrAgeView[WhatRow, rnCol[7]]:='0';  // OVERDUE
    end;

(* MAIN BLOCK *)

begin

    // -----------
    // Initialize.
    // -----------

    avRow:=0;
    SetLength(ArrAgeView, 1, 32);  // make 1 row and 1..32 columns
    AgeViewZeroFields(0);

    // --------------
    // Date and time.
    // --------------

    DatTim:=DateToStr(Now) + ' ' + TimeToStr(Now);
    if System.SysUtils.DayOfWeek(Now) = 2 then bias:=3 else bias:=1;
    CutOff:=Now - bias;

    // ----------------------------------------------------------------
    // WARNING! IT REQUIRES OPEN ITEMS "STRING GRID" TO BE SORTED BY
    //          SUPPORTED COLUMN "CUID", WE ASSUME THAT IS ALREADY DONE
    //          AND WE START WITH "ONE" BECAUSE ZERO POINTS TO HEADERS
    //          IN "STRING GRID".
    // ----------------------------------------------------------------

    for iCNT:=1 to SourceGrid.RowCount - 1 do
    begin

        { GO THROUGH THE ROWS AND POPULATE WHEN FIND THAT THE ROW BELOW IS DIFFERENT THAN CURRENT }
        if (SourceGrid.Cells[34, iCNT] <> SourceGrid.Cells[34, iCNT + 1]) then
        begin

            { FIXED DATA PER ROW }
            ArrAgeView[avRow, 0]:=GroupID;
            ArrAgeView[avRow, 1]:=DateToStr(CutOff);
            ArrAgeView[avRow, 2]:=DatTim;
            ArrAgeView[avRow, 26]:=' ';  { DUMMY FIELD AFTER DEPRICATED EMBEDDED RISK CLASS }

            { RE-WRITE REST OF THE COLUMNS }
            for jCNT:=0 to high(oiCol) do
                ArrAgeView[avRow, avCol[jCNT]]:=SourceGrid.Cells[oiCol[jCNT], iCNT];

            { ALTERNATIONS }

            { REMOVE "TAB" CHARACTER IF FOUND IN CUSTOMER NAME }
            ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], TChars.TAB, '', [rfReplaceAll]);

            { REPLACE SINGLE QUOTES TO DOUBLE QUOTES }
            ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], TChars.SingleQuote, TChars.DoubleQuote, [rfReplaceAll]);

            { REMOVE FROM CO CODE "F" PREFIX }
            ArrAgeView[avRow, 20]:=IntToStr((StrToInt(StringReplace(ArrAgeView[avRow, 20], 'F', '0', [rfReplaceAll]))));

            { LEDGER ISO }
            //if MainForm.sgCompanyData.Cells[0, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.sgCompanyData.Cells[0, 1];
            //if MainForm.sgCompanyData.Cells[1, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.sgCompanyData.Cells[1, 1];
            //if MainForm.sgCompanyData.Cells[2, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.sgCompanyData.Cells[2, 1];
            //if MainForm.sgCompanyData.Cells[3, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=MainForm.sgCompanyData.Cells[3, 1];

            { COUNTERS }
            { MOVE COUNTER }
            inc(avRow);

            { EXPAND ARRAY BY ONE EMPTY ROW }
            SetLength(ArrAgeView, avRow + 1, 32);

            { ZERO FIELDS }
            AgeViewZeroFields(avRow);

        end;
    end;

    { RANGES AND RISK CLASS BOUNDS }
    Settings:=TSettings.Create;

    { LOWER BOUNDS }
    R1lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE1A', 0);
    R2lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE2A', 0);
    R3lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE3A', 0);
    R4lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE4A', 0);
    R5lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE5A', 0);
    R6lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE6A', 0);

    { UPPER BOUNDS }
    R1hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE1B', 0);
    R2hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE2B', 0);
    R3hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE3B', 0);
    R4hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE4B', 0);
    R5hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE5B', 0);

    { POPULATE }

    { LOOP VIA CUID COLUMN IN AGE VIEW [27] }
    for exRow:=0 to avRow - 1 do
    begin

        DiscAmnt:=0;

        { LOOP VIA CUID COLUMN IN OPEN ITEMS [34] }
        for iCNT:=0 to SourceGrid.RowCount - 1 do
        begin

            { COMPARE AND EXECUTE IF THE SAME }
            if SourceGrid.Cells[34, iCNT] = ArrAgeView[exRow, 27] then
            begin

                { SUM ITEMS: NOT DUE, RANGE1..6 }
                ArrAgeView[exRow,
                    bucket(
                            StrToInt(SourceGrid.Cells[33, iCNT]),
                            bias
                    )
                ]:=FloatToStr(
                    StrToFloat(ArrAgeView[exRow, bucket(StrToInt(SourceGrid.Cells[33, iCNT]), bias)]) +
                    StrToFloat(SourceGrid.Cells[5, iCNT])
                );

                { SUM ITEMS: DISCOUNTED AMOUNT [34] | TECHNICAL VARIABLE }
                DiscAmnt:=DiscAmnt + StrToFloat(SourceGrid.Cells[34, iCNT]);

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

    { DECIMAL SEPARATOR }
    if FormatSettings.DecimalSeparator = ',' then
        for exRow:=0 to avRow - 1 do
            for jCNT:=0 to high(rfCol) do
                { REPLACE ',' TO '.' FOR ALL VALUES [6..14] AND [25..26] }
                ArrAgeView[exRow, rfCol[jCNT]]:=StringReplace(ArrAgeView[exRow, rfCol[jCNT]], ',', '.', [rfReplaceAll]);

    Result:=ArrAgeView;

end;

{ -------------------------------------------------------------------------------------------------------------------------- TRANSFER 'AGEVIEW' TO SQL SERVER }
procedure TDebtors.FWriteAgeView(DestTable: string; GroupID: string; SourceArray: TALists);
var
    Transaction: string;
    DeleteData:  string;
    Condition:   string;
begin

    var DataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        try

            { ASSIGN COLUMNS | ADD ALL BUT ID COLUMN }
            DataTables.Columns.Add(TSnapshots.GroupId);
            DataTables.Columns.Add(TSnapshots.AgeDate);
            DataTables.Columns.Add(TSnapshots.SnapshotDt);
            DataTables.Columns.Add(TSnapshots.CustomerName);
            DataTables.Columns.Add(TSnapshots.CustomerNumber);
            DataTables.Columns.Add(TSnapshots.CountryCode);
            DataTables.Columns.Add(TSnapshots.NotDue);
            DataTables.Columns.Add(TSnapshots.Range1);
            DataTables.Columns.Add(TSnapshots.Range2);
            DataTables.Columns.Add(TSnapshots.Range3);
            DataTables.Columns.Add(TSnapshots.Range4);
            DataTables.Columns.Add(TSnapshots.Range5);
            DataTables.Columns.Add(TSnapshots.Range6);
            DataTables.Columns.Add(TSnapshots.Overdue);
            DataTables.Columns.Add(TSnapshots.Total);
            DataTables.Columns.Add(TSnapshots.CreditLimit);
            DataTables.Columns.Add(TSnapshots.ExceededAmount);
            DataTables.Columns.Add(TSnapshots.PaymentTerms);
            DataTables.Columns.Add(TSnapshots.Agent);
            DataTables.Columns.Add(TSnapshots.Division);
            DataTables.Columns.Add(TSnapshots.CoCode);
            DataTables.Columns.Add(TSnapshots.LedgerIso);
            DataTables.Columns.Add(TSnapshots.Inf4);
            DataTables.Columns.Add(TSnapshots.Inf7);
            DataTables.Columns.Add(TSnapshots.Person);
            DataTables.Columns.Add(TSnapshots.Group3);
            DataTables.Columns.Add(TSnapshots.RiskClass);
            DataTables.Columns.Add(TSnapshots.Cuid);
            DataTables.Columns.Add(TSnapshots.SalesResponsible);
            DataTables.Columns.Add(TSnapshots.CustomerGroup);
            DataTables.Columns.Add(TSnapshots.PersonResponsible);
            DataTables.Columns.Add(TSnapshots.AccountType);

            { DELETE STATEMENT | REMOVE OLD DATA }
            DeleteData:=TSql.DELETE_FROM + DestTable;
            Condition:=TSnapshots.GroupId + TSql.EQUAL + QuotedStr(SourceArray[0, 0]) + TSql._AND + TSnapshots.AgeDate + TSql.EQUAL + QuotedStr(LeftStr(SourceArray[0, 1], 10));

            { INSERT STATEMENT | INSERT NEW DATA }
            Transaction:=DataTables.TransactTemp;
            Transaction:=StringReplace(Transaction, '{CommonSelect}', DataTables.CommonSelect,  [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{CommonDelete}', DataTables.CommonDelete,  [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{DestTable}',    DestTable,     [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{Condition}',    Condition,     [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{DeleteData}',   DeleteData,    [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{SimpleInput}',  TChars.SPACE, [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{SWITCH}',       'OFF',         [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{Begin}',        TChars.SPACE, [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{End}',          TChars.SPACE, [rfReplaceAll]);
            Transaction:=StringReplace(
                Transaction,
                '{ComplexInput}',
                DataTables.ToSqlInsert(SourceArray, nil, DestTable, DataTables.ColumnsToList(DataTables.Columns, TQuotes.Disabled)),
                [rfReplaceAll]
            );

            { EXECUTE }
            DataTables.StrSQL:=Transaction;

            //THelpers.ExecMessage(False, TMessaging.TWParams.StatusBar, TStatusBar.SQLupdate, MainForm);
            DataTables.ExecSQL;

        except
            on E: Exception do
                ThreadFileLog.Log('Cannot send to server. Error has been thrown: ' + E.Message);
        end;

        ThreadFileLog.Log('Age View transferred to Microsoft SQL Server. Rows affected: ' + DataTables.RowsAffected.ToString + '.');

    finally
        DataTables.Free();
    end;

end;


end.

