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
        procedure MakeAgeViewSQLAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure MakeAgeViewCSVAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    private
        function FMakeAgeView(OSAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid): TALists;
        procedure FWriteAgeView(DestTable: string; GroupID: string; SourceArray: TALists);
    public
        procedure MakeAgeViewSQLAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid; Callback: TMakeAgeViewAsync);
        procedure MakeAgeViewCSVAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid; Callback: TMakeAgeViewAsync);
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

procedure TDebtors.MakeAgeViewSQLAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid; Callback: TMakeAgeViewAsync);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        LastError.IsSucceeded:=True;
        try

            FWriteAgeView(
                TSnapshots.Snapshots,
                GroupID,
                FMakeAgeView(OpenAmount, GroupID, SourceGrid, CompanyData)
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

procedure TDebtors.MakeAgeViewCSVAsync(OpenAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid; Callback: TMakeAgeViewAsync);
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
            DataTables.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(ActionMode, Grid, LastError);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------ GENERATE AGING //
function TDebtors.FMakeAgeView(OSAmount: double; GroupID: string; SourceGrid: TStringGrid; CompanyData: TStringGrid): TALists;

    var

        bias:       integer;
        DiscAmnt:   double;
        Settings:   ISettings;
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

    const

        // --------------------------------------------------------
        // Warning! Below must be aligned with open items source
        //          and destination table in database for age view.
        //          Please refer to the manual.
        // --------------------------------------------------------

        // Defines source columns to be transferred to age view 'as is'
        oiCol: array[0..16] of integer = (6, 2, 15, 14, 16, 18, 27, 1,  12, 13, 30, 28, 34, 35, 36, 37, 38);

        // Defines destination columns in age view array
        avCol: array[0..16] of integer = (3, 4, 5, 15, 17, 18, 19, 20, 22, 23, 24, 25, 27, 28, 29, 30, 31);

        // Defines bucket columns (output): not due, range1..6, overdue
        rnCol: array[0..7 ] of integer = (6, 7, 8, 9, 10, 11, 12, 13);

        // Defines all columns with values, to be replaced with dot.
        rfCol: array[0..11] of integer = (6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 26);

    function bucket(pmtstat: integer; bias: integer): integer;
    begin

        // --------------------------------------------------------------------------------------------------
        // 'Pmtstat' is counted between due date and current date, thus it must be corrected for cutoff date.
        // example being:
        //   On monday we make aging for Friday, threrfore bias equals 3 and cut-off is 'cdatetime' - 3.
        //   non-monday goes with bias = 1, thus i.e. Wednesday - 1.
        //   However, open item column 'payment status' contains with calculation:
        //     'pmtstat' = Due date - Today, thus:
        //     'pmtstat' = 2017-10-10 - 2017-10-23 = -13 (overdue).
        //   Cut-off date is 2017-10-23 (Monday) - 3 = 2017-10-20 (Friday).
        //   This is why we compute 'pmtstat' = -13 + 3 = -10 (between 8 and 30).
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
        if (abs(pmtstat) >= R6lo) {and (abs(pmtstat) <= R6hi)} then result:=rnCol[6];

    end;

    // -----------------------
    // Fill array with zeroes.
    // -----------------------

    procedure AgeViewZeroFields(WhatRow: integer);
    begin
        ArrAgeView[WhatRow, rnCol[0]]:='0';  // Not due
        ArrAgeView[WhatRow, rnCol[1]]:='0';  // Range 1
        ArrAgeView[WhatRow, rnCol[2]]:='0';  // Range 2
        ArrAgeView[WhatRow, rnCol[3]]:='0';  // Range 3
        ArrAgeView[WhatRow, rnCol[4]]:='0';  // Range 4
        ArrAgeView[WhatRow, rnCol[5]]:='0';  // Range 5
        ArrAgeView[WhatRow, rnCol[6]]:='0';  // Range 6
        ArrAgeView[WhatRow, rnCol[7]]:='0';  // Overdue
    end;

begin

    avRow:=0;
    SetLength(ArrAgeView, 1, 32);
    AgeViewZeroFields(0);

    DatTim:=DateToStr(Now) + ' ' + TimeToStr(Now);
    if System.SysUtils.DayOfWeek(Now) = 2 then bias:=3 else bias:=1;
    CutOff:=Now - bias;

    // --------------------------------------------------------------
    // Warning! It requires open items StringGrid to be sorted by
    //          CUID value, we assume that this is already done
    //          and we start with "one" because zero points to header
    //          in StringGrid.
    // --------------------------------------------------------------

    for iCNT:=1 to SourceGrid.RowCount - 1 do
    begin

        // Go through the rows and populate when find that the row below is different than current
        if (SourceGrid.Cells[34, iCNT] <> SourceGrid.Cells[34, iCNT + 1]) then
        begin

            ArrAgeView[avRow, 0]:=GroupID;
            ArrAgeView[avRow, 1]:=DateToStr(CutOff);
            ArrAgeView[avRow, 2]:=DatTim;
            ArrAgeView[avRow, 26]:=' ';

            for jCNT:=0 to high(oiCol) do
                ArrAgeView[avRow, avCol[jCNT]]:=SourceGrid.Cells[oiCol[jCNT], iCNT];

            ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], TChars.TAB, '', [rfReplaceAll]);
            ArrAgeView[avRow, 3]:=StringReplace(ArrAgeView[avRow, 3], TChars.SingleQuote, TChars.DoubleQuote, [rfReplaceAll]);
            ArrAgeView[avRow, 20]:=IntToStr((StrToInt(StringReplace(ArrAgeView[avRow, 20], 'F', '0', [rfReplaceAll]))));

            // Ledger ISO
            if CompanyData.Cells[0, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=CompanyData.Cells[0, 1];
            if CompanyData.Cells[1, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=CompanyData.Cells[1, 1];
            if CompanyData.Cells[2, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=CompanyData.Cells[2, 1];
            if CompanyData.Cells[3, 0] = ArrAgeView[avRow, 20] then ArrAgeView[avRow, 21]:=CompanyData.Cells[3, 1];

            inc(avRow);
            SetLength(ArrAgeView, avRow + 1, 32);
            AgeViewZeroFields(avRow);

        end;
    end;

    Settings:=TSettings.Create();

    // Get risk class lower bunds
    R1lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE1A', 0);
    R2lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE2A', 0);
    R3lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE3A', 0);
    R4lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE4A', 0);
    R5lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE5A', 0);
    R6lo:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE6A', 0);

    // Get risk class upper bounds
    R1hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE1B', 0);
    R2hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE2B', 0);
    R3hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE3B', 0);
    R4hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE4B', 0);
    R5hi:=Settings.GetIntegerValue(TConfigSections.AgingRanges, 'RANGE5B', 0);

    // Loop via CUID in age view [27]
    for exRow:=0 to avRow - 1 do
    begin

        DiscAmnt:=0;

        // Loop via cuid column in open items [34]
        for iCNT:=0 to SourceGrid.RowCount - 1 do
        begin

            // Compare and execute if the same
            if SourceGrid.Cells[34, iCNT] = ArrAgeView[exRow, 27] then
            begin

                // Sum items: not due, range1..6
                ArrAgeView[exRow,
                    bucket(
                            StrToInt(SourceGrid.Cells[33, iCNT]),
                            bias
                    )
                ]:=FloatToStr(
                    StrToFloat(ArrAgeView[exRow, bucket(StrToInt(SourceGrid.Cells[33, iCNT]), bias)]) +
                    StrToFloat(SourceGrid.Cells[5, iCNT])
                );

                // Sum items: discounted amount [34] | technical variable
                DiscAmnt:=DiscAmnt + StrToFloat(SourceGrid.Cells[34, iCNT]);

            end;

        end;

        // Calculate total amount [14]
        ArrAgeView[exRow, 14]:=FloatToStr(
            StrToFloat(ArrAgeView[exRow, rnCol[0]]) +
            StrToFloat(ArrAgeView[exRow, rnCol[1]]) +
            StrToFloat(ArrAgeView[exRow, rnCol[2]]) +
            StrToFloat(ArrAgeView[exRow, rnCol[3]]) +
            StrToFloat(ArrAgeView[exRow, rnCol[4]]) +
            StrToFloat(ArrAgeView[exRow, rnCol[5]]) +
            StrToFloat(ArrAgeView[exRow, rnCol[6]])
        );

        // Total overdue [13] = total amount [14] - not due [6]
        ArrAgeView[exRow, 13]:=FloatToStr(
            StrToFloat(ArrAgeView[exRow, 14]) -
            StrToFloat(ArrAgeView[exRow,  6])
        );

        // Exceeded amount [16] = credit limit [15] - total amount [14]
        ArrAgeView[exRow, 16]:=FloatToStr(StrToFloat(ArrAgeView[exRow, 15]) - StrToFloat(ArrAgeView[exRow, 14]));

    end;

    // Decimal separator
    if FormatSettings.DecimalSeparator = ',' then
        for exRow:=0 to avRow - 1 do
            for jCNT:=0 to high(rfCol) do
                // Replace ',' to '.' for all values [6..14] and [25..26]
                ArrAgeView[exRow, rfCol[jCNT]]:=StringReplace(ArrAgeView[exRow, rfCol[jCNT]], ',', '.', [rfReplaceAll]);

    // ------------------
    // Return and forget.
    // ------------------

    Result:=ArrAgeView;
    SetLength(ArrAgeView, 0);

end;

// ------------------------------------------------------------------------------------------------------------------------ TRANSFER 'AGEVIEW' TO SQL SERVER //
procedure TDebtors.FWriteAgeView(DestTable: string; GroupID: string; SourceArray: TALists);
var
    Transaction: string;
    DeleteData:  string;
    Condition:   string;
begin

    var DataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        try

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

            // Delete statement (to remove old data)
            DeleteData:=TSql.DELETE_FROM + DestTable;
            Condition:=TSnapshots.GroupId + TSql.EQUAL + QuotedStr(SourceArray[0, 0]) + TSql._AND + TSnapshots.AgeDate + TSql.EQUAL + QuotedStr(LeftStr(SourceArray[0, 1], 10));

            // Insert statement for new data
            Transaction:=DataTables.TransactTemp;
            Transaction:=StringReplace(Transaction, '{CommonSelect}', DataTables.CommonSelect, [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{CommonDelete}', DataTables.CommonDelete, [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{DestTable}',    DestTable,     [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{Condition}',    Condition,     [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{DeleteData}',   DeleteData,    [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{SimpleInput}',  TChars.SPACE,  [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{SWITCH}',       'OFF',         [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{Begin}',        TChars.SPACE,  [rfReplaceAll]);
            Transaction:=StringReplace(Transaction, '{End}',          TChars.SPACE,  [rfReplaceAll]);
            Transaction:=StringReplace(
                Transaction,
                '{ComplexInput}',
                DataTables.ToSqlInsert(SourceArray, nil, DestTable, DataTables.ColumnsToList(DataTables.Columns, TQuotes.Disabled)),
                [rfReplaceAll]
            );

            DataTables.StrSQL:=Transaction;
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

