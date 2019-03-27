program UnityFunctions;


{$APPTYPE CONSOLE}


{$R *.res}


uses
    System.Classes,
    System.SysUtils,
    System.StrUtils,
    System.DateUtils,
    Winapi.Windows,
    Vcl.Forms,
    Vcl.Grids,
    ADODB,
    Database in 'Functions\Database.pas',
    Model in 'Functions\Model.pas',
    SQL in 'Functions\SQL.pas',
    Arrays in 'Functions\Arrays.pas',
    CustomTypes in 'Functions\CustomTypes.pas';


{$I .\Functions\Common.inc}


var
    ArrAgeView:  TLists;
    AdoConn:     TADOConnection;
    DataBase:    TDataBase;
    CheckResult: integer;


procedure DebugMsg(const Msg: String);
begin
    OutputDebugString(PChar(Msg));
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


// ------------------------------------------------------------------------------------------------------------------------------------------ GENERATE AGING //


procedure Make(OSAmount: double; OpenItems: TStringGrid);

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

    { WARNING! BELOW MUST BE ALIGNED WITH OPEN ITEMS SOURCE AND DESTINATION TABLE IN DATABASE FOR AGE VIEW }
    const
        { DEFINES SOURCE COLUMNS TO BE TRANSFERRED TO AGE VIEW 'AS IS' }
        oiCol: array[0..16] of integer = (6, 2, 15, 14, 16, 18, 27, 1,  12, 13, 30, 28, 34, 35, 36, 37, 38);

        { DEFINES DESTINATION COLUMNS IN AGE VIEW ARRAY }
        avCol: array[0..16] of integer = (3, 4, 5,  15, 17, 18, 19, 20, 22, 23, 24, 25, 27, 28, 29, 30, 31);

        { DEFINES BUCKET COLUMNS (OUTPUT): NOT DUE, RANGE1..6, OVERDUE }
        rnCol: array[0..7 ] of integer = (6, 7, 8,  9,  10, 11, 12, 13);

        { DEFINES ALL COLUMNS WITH VALUES, TO BE REPLACED WITH '.' }
        rfCol: array[0..11] of integer = (6, 7, 8,  9,  10, 11, 12, 13, 14, 15, 16, 26);

    (* NESTED METHODS *)

    function bucket(pmtstat: integer; bias: integer): integer;
    begin

        /// <remarks>
        /// 'PMTSTAT' IS COUNTED BETWEEN DUE DATE AND CURRENT DATE, THUS IT MUST BE CORRECTED FOR CUTOFF DATE.
        /// EXAMPLE BEING:
        ///   ON MONDAY WE MAKE AGING FOR FRIDAY, THRERFORE BIAS EQUALS 3 AND CUT-OFF IS 'CDATETIME' - 3.
        ///   NON-MONDAY GOES WITH BIAS = 1, THUS I.E. WEDNESDAY - 1.
        ///   HOWEVER, OPEN ITEM COLUMN 'PAYMENT STATUS' CONTAINS WITH CALCULATION:
        ///     'PMTSTAT' = DUE DATE - TODAY, THUS:
        ///     'PMTSTAT' = 2017-10-10 - 2017-10-23 = -13 (OVERDUE).
        ///   CUT-OFF DATE IS 2017-10-23 (MONDAY) - 3 = 2017-10-20 (FRIDAY).
        ///   THIS IS WHY WE COMPUTE 'PMTSTAT' = -13 + 3 = -10 (BETWEEN 8 AND 30).
        /// </remarks>

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

    case DayOfWeek(Now) of

        DaySaturday:
        begin
            Write('[Unity]: It is Saturday, time to relax and hang with your friends. Aging generator is quitting the task...');
            Exit;
        end;

        DaySunday:
        begin
            Write('[Unity]: It is Sunday, time to relax and hang with your friends. Aging generator is quitting the task...');
            Exit;
        end;

    end;

    Write('[Unity]: Generating aging report... ');

    // INITIALIZE
    RcLo :=0;
    RcHi :=0;
    avRow:=0;
    SetLength(ArrAgeView, 1, 32);  // MAKE 1 ROW AND 1..32 COLUMNS

    // PUT ZERO FIELDS
    AgeViewZeroFields(0);

    // DATE AND TIME
    DatTim:=DateToStr(Now) + ' ' + TimeToStr(Now);
    if DayOfWeek(Now) = 2 then
        bias:=3
            else
                bias:=1;
    CutOff:=Now - bias;

    // REMOVE DUPLICATES AND MAKE AGE VIEW WITHOUT AGE VALUES

    /// <remarks>
    /// IT REQUIRES OPEN ITEMS "STRING GRID" TO BE SORTED BY
    /// SUPPORTED COLUMN "CUID", WE ASSUME THAT IS ALREADY DONE
    /// AND WE START WITH "ONE" BECAUSE "ZERO" IS THE HEADER ROW/COLUMN
    /// IN "STRING GRID"
    /// </remarks>

    for iCNT:=1 to OpenItems.RowCount - 1 do
    begin

        { GO THROUGH THE ROWS AND POPULATE WHEN FIND THAT THE ROW BELOW IS DIFFERENT THAN CURRENT }
        if (OpenItems.Cells[34, iCNT] <> OpenItems.Cells[34, iCNT + 1]) then
        begin

            { FIXED DATA PER ROW }
            ArrAgeView[avRow, 0]:='';
            ArrAgeView[avRow, 1]:=DateToStr(CutOff);
            ArrAgeView[avRow, 2]:=DatTim;
            ArrAgeView[avRow, 26]:=' ';  { DUMMY FIELD AFTER DEPRICATED EMBEDDED RISK CLASS }

            { RE-WRITE REST OF THE COLUMNS }
            for jCNT:=0 to high(oiCol) do
                ArrAgeView[avRow, avCol[jCNT]]:=OpenItems.Cells[oiCol[jCNT], iCNT];

            { ALTERNATIONS }

            { REMOVE "TAB" CHARACTER IF FOUND IN CUSTOMER NAME }
            ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], TAB, '', [rfReplaceAll]);

            { REPLACE SINGLE QUOTES TO DOUBLE QUOTES }
            ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], SingleQuote, DoubleQuote, [rfReplaceAll]);

            { REMOVE FROM CO CODE "F" PREFIX }
            ArrAgeView[avRow, 20]:=IntToStr((StrToInt(StringReplace(ArrAgeView[avRow, 20], 'F', '0', [rfReplaceAll]))));

            { LEDGER ISO }
            ArrAgeView[avRow, 21]:='EUR';

            { COUNTERS }
            { MOVE COUNTER }
            inc(avRow);

            { EXPAND ARRAY BY ONE EMPTY ROW }
            SetLength(ArrAgeView, avRow + 1, 32);

            { ZERO FIELDS }
            AgeViewZeroFields(avRow);

        end;
    end;

    { LOWER BOUNDS }
    R1lo:=1;
    R2lo:=8;
    R3lo:=31;
    R4lo:=61;
    R5lo:=91;
    R6lo:=121;

    { UPPER BOUNDS }
    R1hi:=7;
    R2hi:=30;
    R3hi:=60;
    R4hi:=90;
    R5hi:=120;

    { POPULATE }

    { LOOP VIA CUID COLUMN IN AGE VIEW [27] }
    for exRow:=0 to avRow - 1 do
    begin

        DiscAmnt:=0;

        { LOOP VIA CUID COLUMN IN OPEN ITEMS [34] }
        for iCNT:=0 to OpenItems.RowCount - 1 do
        begin

            { COMPARE AND EXECUTE IF THE SAME }
            if OpenItems.Cells[34, iCNT] = ArrAgeView[exRow, 27] then
            begin

                { SUM ITEMS: NOT DUE, RANGE1..6 }
                ArrAgeView[exRow,
                    bucket(
                            StrToInt(OpenItems.Cells[33, iCNT]),
                            bias
                    )
                ]:=FloatToStr(
                    StrToFloatDef(ArrAgeView[exRow, bucket(StrToInt(OpenItems.Cells[33, iCNT]), bias)], 0) +
                    StrToFloatDef(OpenItems.Cells[5, iCNT], 0)
                );

                { SUM ITEMS: DISCOUNTED AMOUNT [34] | TECHNICAL VARIABLE }
                DiscAmnt:=DiscAmnt + StrToFloat(OpenItems.Cells[34, iCNT]);

            end;

        end;

        { CALCULATE TOTAL AMOUNT [14] }
        ArrAgeView[exRow, 14]:=FloatToStr(
            StrToFloatDef(ArrAgeView[exRow, rnCol[0]], 0) +
            StrToFloatDef(ArrAgeView[exRow, rnCol[1]], 0) +
            StrToFloatDef(ArrAgeView[exRow, rnCol[2]], 0) +
            StrToFloatDef(ArrAgeView[exRow, rnCol[3]], 0) +
            StrToFloatDef(ArrAgeView[exRow, rnCol[4]], 0) +
            StrToFloatDef(ArrAgeView[exRow, rnCol[5]], 0) +
            StrToFloatDef(ArrAgeView[exRow, rnCol[6]], 0)
        );

        { TOTAL OVERDUE [13] = TOTAL AMOUNT [14] - NOT DUE [6] }
        ArrAgeView[exRow, 13]:=FloatToStr(
            StrToFloatDef(ArrAgeView[exRow, 14], 0) -
            StrToFloatDef(ArrAgeView[exRow,  6], 0)
        );

        { EXCEEDED AMOUNT [16] = CREDIT LIMIT [15] - TOTAL AMOUNT [14] }
        ArrAgeView[exRow, 16]:=FloatToStr(StrToFloatDef(ArrAgeView[exRow, 15], 0) - StrToFloatDef(ArrAgeView[exRow, 14], 0));

    end;

    { DECIMAL SEPARATOR }
    if FormatSettings.DecimalSeparator = ',' then
        for exRow:=0 to avRow - 1 do
            for jCNT:=0 to high(rfCol) do
                { REPLACE ',' TO '.' FOR ALL VALUES [6..14] AND [25..26] }
                ArrAgeView[exRow, rfCol[jCNT]]:=StringReplace(ArrAgeView[exRow, rfCol[jCNT]], ',', '.', [rfReplaceAll]);

    Writeln('done (' + exRow.ToString + ' items processed).');

end;

// ---

procedure WriteToDb(DestTable: string; SourceArray: TLists);
var
    Transaction: string;
    Tables:      TDataTables;
begin

    Tables:=TDataTables.Create(AdoConn);
    try

        { ASSIGN COLUMNS | ADD ALL BUT ID COLUMN }
        Tables.Columns.Add(TSnapshots.GroupId);
        Tables.Columns.Add(TSnapshots.AgeDate);
        Tables.Columns.Add(TSnapshots.SnapshotDt);
        Tables.Columns.Add(TSnapshots.CustomerName);
        Tables.Columns.Add(TSnapshots.CustomerNumber);
        Tables.Columns.Add(TSnapshots.CountryCode);
        Tables.Columns.Add(TSnapshots.NotDue);
        Tables.Columns.Add(TSnapshots.Range1);
        Tables.Columns.Add(TSnapshots.Range2);
        Tables.Columns.Add(TSnapshots.Range3);
        Tables.Columns.Add(TSnapshots.Range4);
        Tables.Columns.Add(TSnapshots.Range5);
        Tables.Columns.Add(TSnapshots.Range6);
        Tables.Columns.Add(TSnapshots.Overdue);
        Tables.Columns.Add(TSnapshots.Total);
        Tables.Columns.Add(TSnapshots.CreditLimit);
        Tables.Columns.Add(TSnapshots.ExceededAmount);
        Tables.Columns.Add(TSnapshots.PaymentTerms);
        Tables.Columns.Add(TSnapshots.Agent);
        Tables.Columns.Add(TSnapshots.Division);
        Tables.Columns.Add(TSnapshots.CoCode);
        Tables.Columns.Add(TSnapshots.LedgerIso);
        Tables.Columns.Add(TSnapshots.Inf4);
        Tables.Columns.Add(TSnapshots.Inf7);
        Tables.Columns.Add(TSnapshots.Person);
        Tables.Columns.Add(TSnapshots.Group3);
        Tables.Columns.Add(TSnapshots.RiskClass);
        Tables.Columns.Add(TSnapshots.Cuid);
        Tables.Columns.Add(TSnapshots.SalesResponsible);
        Tables.Columns.Add(TSnapshots.CustomerGroup);
        Tables.Columns.Add(TSnapshots.PersonResponsible);
        Tables.Columns.Add(TSnapshots.AccountType);

        { INSERT STATEMENT | INSERT NEW DATA }
        Transaction:=TransactTemp;
        Transaction:=StringReplace(Transaction, '{CommonSelect}', SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{CommonDelete}', SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{DestTable}',    SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{Condition}',    SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{DeleteData}',   SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{SimpleInput}',  SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{SWITCH}',       'OFF',        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{Begin}',        SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(Transaction, '{End}',          SPACE,        [rfReplaceAll]);
        Transaction:=StringReplace(
            Transaction,
            '{ComplexInput}',
            Tables.ToSqlInsert(SourceArray, nil, DestTable, Tables.ColumnsToList(Tables.Columns, enQuotesOff)),
            [rfReplaceAll]
        );

        Tables.StrSQL:=Transaction;
        Tables.ExecSQL;

    finally
        Tables.Free;
    end;

end;


// ---------------------------------------------------------------------------------------------------------------------------------------------- MAIN BLOCK //


var
    Tables:      TDataTables;
    CoCodeList:  TStringGrid;
    OpenItems:   TStringGrid;
    iCNT:        integer;
    jCNT:        integer;
    Today:       TDate;
    OSAmount:    double;
    Processed:   integer;
    RegSettings: TFormatSettings;
begin

    /// <summary>
    /// Setup formats to user local settings.
    /// </summary>

    {$WARN SYMBOL_PLATFORM OFF} { Windows only }
    RegSettings:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}

    RegSettings.CurrencyDecimals    :=4;
    RegSettings.DateSeparator       :='-';
    RegSettings.ShortDateFormat     :='yyyy-mm-dd';
    RegSettings.LongDateFormat      :='yyyy-mm-dd';
    RegSettings.TimeSeparator       :=':';
    RegSettings.TimeAMString        :='AM';
    RegSettings.TimePMString        :='PM';
    RegSettings.ShortTimeFormat     :='hh:mm tt';
    RegSettings.LongTimeFormat      :='hh:mm:ss';
    FormatSettings                  :=RegSettings;
    Application.UpdateFormatSettings:=False;

    DataBase:=TDataBase.Create;
    AdoConn :=TADOConnection.Create(nil);
    try
        try

            // Display version
            Writeln('Unity for Debt Management - Aging Generator');
            Writeln('===========================================');
            Writeln('');

            // Get the database connection
            DataBase.InitializeConnection(AdoConn);
            Writeln('[Unity]: Initializing (' + Database.MsgConnStr + ').');
            Writeln('[Unity]: Initialization status: ' + Database.MsgInitStatus);

            CheckResult:=DataBase.Check;
            Writeln('[Unity]: ' + Database.MsgConnCheck);

            if CheckResult > 0 then
            begin
                Readln;
                Exit;
            end;

            Today:=Now;
            Processed:=0;
            OpenItems:=TStringGrid.Create(nil);
            CoCodeList:=TStringGrid.Create(nil);
            Tables:=TDataTables.Create(AdoConn);
            try

                // Get company list
                Write('[Unity]: Getting company list... ');

                Tables.StrSQL:='EXEC Customer.GetCompanyList ' + QuotedStr(DateToStr(Today));
                Tables.SqlToGrid(CoCodeList, Tables.ExecSQL, False, True);

                if CoCodeList.RowCount = 0 then
                begin
                    Writeln('[Unity]: Cannot get company list, execution has been stopped.');
                    Readln;
                    Exit;
                end
                else
                begin
                    Writeln('done.');
                end;

                // Iterate via all companies
                // We start with 1 because 0 is header
                for iCNT:=1 to CoCodeList.RowCount - 1 do
                begin

                    // Screen update
                    Write('[Unity]: Loading open items for "' + CoCodeList.Cells[2, iCNT] + '"... ');

                    // Load open items
                    Tables.StrSQL:=
                        'EXEC Customer.QueryOpenItemsAlt '   +
                        QuotedStr(DateToStr(Today))          + ',' +
                        QuotedStr(CoCodeList.Cells[1, iCNT]) + ',' +
                        QuotedStr('0')                       + ',' +
                        QuotedStr('0')                       + ',' +
                        QuotedStr('0')                       + ',' +
                        QuotedStr('71150')                   + ',' +
                        QuotedStr('ON')                      + ',' +
                        QuotedStr('ON')                      + ',' +
                        QuotedStr('cards');

                    // Count total amount and make aging
                    if Tables.SqlToGrid(OpenItems, Tables.ExecSQL, False, True) then
                    begin

                        OSAmount:=0;

                        // Get total outstanding [OpenAm: 5]
                        for jCNT:=1 to OpenItems.RowCount - 1 do
                        begin
                            OSAmount:=OSAmount + StrToFloat(OpenItems.Cells[5, jCNT]);
                        end;

                        // Screen update
                        Writeln((OpenItems.RowCount - 1).ToString + ' items loaded (outstanding amount = ' + FormatFloat('#,##0.00', OSAmount) + ').');

                        // Make aging
                        Make(OSAmount, OpenItems);

                        // Write to db...
                        Writeln('[Unity]: Writing to database...');
                        WriteToDb('Customer.SnapshotsAggregateTest', ArrAgeView);

                        Inc(Processed);

                    end
                    else
                    begin
                        Writeln('no data to load, company skipped.');
                    end;

                end;

            finally
                Tables.Free;
                OpenItems.Free;
                CoCodeList.Free;
            end;

            Writeln('[Unity]: ' + Processed.ToString + ' companies has been processed.');

        except
            on E: Exception do
                Writeln(E.ClassName, ': ', E.Message);
        end;

    finally
        AdoConn.Free;
        DataBase.Free;
    end;

end.

