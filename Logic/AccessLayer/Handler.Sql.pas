unit Handler.Sql;

// ----------------------------------------
// Application logic, access layers.
// Can be referenced by anyone.
// Cannot hold references to View.
// ----------------------------------------

interface


uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    System.StrUtils,
    System.Variants,
    Vcl.StdCtrls,
    Data.Win.ADODB,
    Unity.Grid,
    Unity.Arrays,
    Unity.Enums;


    // legacy code - to be removed after REST is implemented, do not modify


type


    TMSSQL = class
    {$TYPEINFO ON}
    private
        var FParamList:    TALists;
        var FStrSQL:       string;
        var FADOCon:       TADOConnection;
        var FCmdType:      TCommandType;
        var FRowsAffected: integer;
        var FLastErrorMsg: string;
    public
        property StrSQL:       string         read FStrSQL        write FStrSQL;
        property CmdType:      TCommandType   read FCmdType       write FCmdType;
        property ParamList:    TALists        read FParamList;
        property ADOCon:       TADOConnection read FADOCon;
        property RowsAffected: integer        read FRowsAffected;
        property LastErrorMsg: string         read FLastErrorMsg;
        constructor Create(Connector: TADOConnection);
        destructor  Destroy; override;
        procedure   ClearSQL;
        function    CleanStr(Text: string; Quoted: boolean): string;
        function    ExecSQL: _Recordset;
        function    ToSqlInsert(Table: TALists; Grid: TStringGrid; tblName: string; tblColumns: string; HeaderPresent: boolean = True{Option}): string;
        function    SqlToGrid(var Grid: TStringGrid; RS: _Recordset; AutoNoCol: boolean; Headers: boolean): boolean;
        function    SqlToSimpleList(var List: TComboBox; RS: _Recordset): boolean;
    const
        schCustomer       = 'Customer';
        schErp            = 'Erp';
        schCommon         = 'Common';
        AgeViewExport     = 'Customer.AgeViewExport';
        AgeViewReport     = 'Customer.AgeViewReport';
        QueryOpenItems    = 'Customer.QueryOpenItems';
        TrackerList       = 'Customer.TrackerList';
        UpsertFreeColumns = 'Customer.UpsertFreeColumns';
        CommonSelect      = 'IF EXISTS (SELECT 1 FROM {DestTable} WITH (UPDLOCK) WHERE {Condition})';
        CommonDelete      = '{DeleteData} WHERE {Condition}';
        TransactTemp      = 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE' + #13#10 +
                            'BEGIN TRANSACTION                           ' + #13#10 +
                            '    SET XACT_ABORT ON                       ' + #13#10 +
                            '    SET NOCOUNT {SWITCH}                    ' + #13#10 +
                            '    {CommonSelect}                          ' + #13#10 +
                            '    {Begin}                                 ' + #13#10 +
                            '    {CommonDelete}                          ' + #13#10 +
                            '    {SimpleInput}                           ' + #13#10 +
                            '    {ComplexInput}                          ' + #13#10 +
                            '    {End}                                   ' + #13#10 +
                            'COMMIT TRANSACTION                          ';
    end;


    /// <summary>
    /// Basic CRUD operation handler for database tables.
    /// </summary>

    TDataTables = class(TMSSQL)
    {$TYPEINFO ON}
    private
        var FConnStr:     string;
        var FidThd:       integer;
        var FCustFilter:  string;
    public
        var DataSet:    _Recordset;
        var Columns:    TStringList;
        var Values:     TStringList;
        var Conditions: TStringList;
        property idThd:       integer  read FidThd      write FidThd;
        property CustFilter:  string   read FCustFilter write FCustFilter;
        property ConnStr:     string   read FConnStr;
        constructor Create(Connector: TADOConnection); overload;
        destructor  Destroy; override;
        function    BracketStr(Expression: string; BracketType: TBrackets): string;
        function    ColumnsToList(Holder: TStringList; Quoted: TQuotes): string;
        procedure   CleanUp;
        function    OpenTable(TableName: string): boolean;
        function    InsertInto(TableName: string; IsExplicit: boolean; ExtSourceGrid: TStringGrid = nil{Option}; ExtSourceArray: TALists = nil{Option}; HeaderPresent: boolean = True{Option}): boolean;
        function    UpdateRecord(TableName: string; IsExplicit: boolean; SingleCondition: string = ''{Option}): boolean;
        function    DeleteRecord(TableName: string; KeyName: string; KeyValue: string; IsExplicit: boolean): boolean;
    end;


implementation


uses
    Unity.Sql,
    Unity.Chars,
    View.Main;


// MS SQL -------------------------------------------------------------------------------------------------------------------------------------------------- //


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TMSSQL.Create(Connector: TADOConnection);
begin
    FStrSQL :='';
    FADOCon :=Connector;
    FCmdType:=cmdText;
    SetLength(FParamList, 1, 2);
end;


destructor TMSSQL.Destroy;
begin
    inherited;
end;


// ------------------------------------------------------------------------------------------------------------------------------------- SQL HANDLER METHODS //


/// <summary>
/// Clear last SQL command with rows affected and error (if any).
/// </summary>

procedure TMSSQL.ClearSQL;
begin
    FStrSQL:='';
    FLastErrorMsg:='';
    FRowsAffected:=0;
end;


/// <summary>
/// Remove characters that may negatively affect SQL expression.
/// </summary>

function TMSSQL.CleanStr(Text: string; Quoted: boolean): string;
begin
    Result:='';
    Text:=StringReplace(Text, TChars.TAB,   TChars.SPACE, [rfReplaceAll]);
    Text:=StringReplace(Text, TChars.QUOTE, TChars.SPACE, [rfReplaceAll]);
    Text:=StringReplace(Text, TChars.CRLF,  TChars.SPACE, [rfReplaceAll]);
    if Quoted then Result:=QuotedStr(Text) else Result:=Text;
end;


/// <summary>
/// Execute dml, ddl, tcl and dcl statements.
/// </summary>
/// <remarks>
/// Inserting records using 'values' keyword has limit up to 1000 records.
/// to insert more than 1000 records with single query statement, we can
/// split query into smaller chunks and execute "insert into [] values ()"
/// as many times as needed. Alternatively, instead of 'values' keyword we
/// may use 'union all' and 'select' statement to obtain ability to send
/// as many rows as we want, this is the preferred method.
/// </remarks>

function TMSSQL.ExecSQL: _Recordset;
begin

    Result:=nil;

    if not (Length(StrSQL)) > 0 then Exit;

    var Query: TADOCommand:=TADOCommand.Create(nil);
    try
        Query.Connection:=ADOCon;
        try
            Query.CommandType:=CmdType;
            Query.CommandText:=StrSQL;
            FRowsAffected:=0;
            Result:=Query.Execute(FRowsAffected, 0);
        except
            on E: Exception do
            begin
                FLastErrorMsg:=E.Message;
                Result:=nil;
            end;
        end;
    finally
        Query.Free;
    end;

end;


/// <summary>
/// Transfer grid/array data to insert into statement. Note: if StringGrid is used, it assumes that header and list position exists,
/// and therefore starts from "1, 1" instead of "0, 0" position. If no header and list position column is used, then use HeaderPresent
//  flag set to false.
/// </summary>
/// <remarks>
/// This function build sql insert into expression with 'select' and 'union' keywords and therefore 1000 record limit does not apply here.
//  column name must not use quotes and names must be delaminated by comma. It uses multi-dimensional array or StringGrid type.
/// </remarks>

function TMSSQL.ToSqlInsert(Table: TALists; Grid: TStringGrid; tblName: string; tblColumns: string; HeaderPresent: boolean = True{Option}): string;
begin

    Result:='';

    var mRows: integer:=0;
    var mCols: integer:=0;
    var sRows: integer:=0;
    var sCols: integer:=0;

    // Table and Grid cannot be nil or be provided at the same time
    // We require only one of them
    if (Table = nil)  and (Grid = nil)  then Exit;
    if (Table <> nil) and (Grid <> nil) then Exit;

    if Table <> nil then
    begin
        mRows:=High(Table) - 1;
        mCols:=High(Table[1]);
        sRows:=0;
        sCols:=0;
    end;

    if Grid <> nil then
    begin

        // Set number of columns
        mCols:=Grid.ColCount - 1;
        mRows:=Grid.RowCount - 1;

        // Skipt first row in StringGrid (table header)
        if HeaderPresent then
        begin
            sRows:=1;
            sCols:=1;
        end
        else
        begin
            sRows:=0;
            sCols:=0;
        end;

    end;

    var Line:  string;
    var Lines: string;
    var Clean: string;
    var Lead:  string:=TSql.INSERT + TChars.Space + tblName + ' ( ' + tblColumns + ' ) ' + TChars.CrLf;

    for var iCNT: integer:=sRows to mRows do
    begin
        Line:=TSql.SELECT + TChars.SPACE;

        for var jCNT: integer:=sCols to mCols do
        begin

            // Clear data from characters that may negatively affect SQL execution
            if Table <> nil then Clean:=CleanStr(Table[iCNT, jCNT], True);
            if Grid  <> nil then Clean:=CleanStr(Grid.Cells[jCNT, iCNT], True);

            if (jCNT <> mCols) then Line:=Line + Clean + TChars.COMMA;

            if Table <> nil then
            begin
                if (jCNT =  mCols) and (iCNT <> mRows) then Line:=Line + QuotedStr(Table[iCNT, jCNT]) + TChars.SPACE + TSql.UNION + TChars.CRLF;
                if (jCNT =  mCols) and (iCNT =  mRows) then Line:=Line + QuotedStr(Table[iCNT, jCNT]) + TChars.CRLF;
            end;

            if Grid <> nil then
            begin
                if (jCNT =  mCols) and (iCNT <> mRows) then Line:=Line + QuotedStr(Grid.Cells[jCNT, iCNT]) + TChars.SPACE + TSql.UNION + TChars.CRLF;
                if (jCNT =  mCols) and (iCNT =  mRows) then Line:=Line + QuotedStr(Grid.Cells[jCNT, iCNT]) + TChars.CRLF;
            end;

        end;

        Lines:=Lines + Line;
        Line:='';

    end;

    // Output SQL expression without transaction template
    Result:=(Lead + Lines);

end;


/// <summary>
/// Move recordset content to string grid with headers.
/// </summary>

function TMSSQL.SqlToGrid(var Grid: TStringGrid; RS: _Recordset; AutoNoCol: boolean; Headers: boolean): boolean;
begin

    Result:=False;
    var iCNT: integer:=0;

    // Exit condition
    if RS = nil then Exit;
    if (RS.EOF) or (RS.BOF) then Exit;
    if RS.Status <> 0 then Exit;

    try
        Grid.FixedCols:=1;
        Grid.FixedRows:=1;
        Grid.ColCount:=RS.Fields.Count + 1;
        Grid.RowCount:=RS.RecordCount  + 1;

        while not RS.EOF do
        begin
            for var jCNT: integer:=1 to RS.Fields.Count do
            begin
                // Headers
                if (iCNT = 0) and (Headers) then Grid.Cells[jCNT, iCNT]:=VarToStr(RS.Fields[jCNT - 1].Name);
                // Data
                Grid.Cells[jCNT, iCNT + 1]:=VarToStr(RS.Fields[jCNT - 1].Value);
            end;

            RS.MoveNext;
            Inc(iCNT);
            // Lp for fixed column
            if (AutoNoCol) and (iCNT > 0) then Grid.Cells[0, iCNT]:=IntToStr(iCNT);

        end;

        Result:=True;
    except
        Result:=False;

    end;

end;


/// <summary>
/// Move one column to TComboBox component with no header.
/// </summary>

function TMSSQL.SqlToSimpleList(var List: TComboBox; RS: _Recordset): boolean;
begin

    Result:=False;

    // Exit conditons
    if not (RS.RecordCount > 0) then Exit;
    if RS.Fields.Count > 1      then Exit;

    try
        List.Clear;
        while not RS.EOF do
        begin
            List.Items.Add(RS.Fields[0].Value);
            RS.MoveNext;
        end;

        Result:=True;

    except
        Result:=False;
    end;

end;


// DATA TABLES --------------------------------------------------------------------------------------------------------------------------------------------- //


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TDataTables.Create(Connector: TADOConnection);
begin
    FidThd     :=0;
    FCustFilter:='';
    DataSet    :=nil;
    Columns    :=TStringList.Create;
    Values     :=TStringList.Create;
    Conditions :=TStringList.Create;
    inherited;
end;


destructor TDataTables.Destroy;
begin
    Columns.Free;
    Values.Free;
    Conditions.Free;
    DataSet:=nil;
    inherited Destroy;
end;


/// <summary>
/// Helper method to surround string with brackets.
/// </summary>

function TDataTables.BracketStr(Expression: string; BracketType: TBrackets): string;
begin
    Result:='';

    case BracketType of
        TBrackets.Round:  Result:='(' + Expression + ')';
        TBrackets.Square: Result:='[' + Expression + ']';
        TBrackets.Curly:  Result:='{' + Expression + '}';
    end;

end;


/// <summary>
/// Transpose columns to rows.
/// </summary>

function TDataTables.ColumnsToList(Holder: TStringList; Quoted: TQuotes): string;
begin
    Result:=TSql.ALL;

    { PERFORM }
    if (Holder.Text <> '') and (Holder.Count > 0) then
    begin
        Result:='';

        for var iCNT: integer:=0 to Holder.Count - 1 do if Result = '' then
        begin

            case Quoted of
                TQuotes.Disabled: Result:=Holder.Strings[iCNT];
                TQuotes.Enabled:  Result:=QuotedStr(Holder.Strings[iCNT]);
            end;

        end
        else
        begin

            case Quoted of
                TQuotes.Disabled: Result:=Result + TChars.COMMA + Holder.Strings[iCNT];
                TQuotes.Enabled:  Result:=Result + TChars.COMMA + QuotedStr(Holder.Strings[iCNT]);
            end;

        end;

    end;

end;


procedure TDataTables.CleanUp;
begin
    Columns.Clear;
    Values.Clear;
    Conditions.Clear;
end;


/// <summary>
/// Open table to recordset.
/// </summary>

function TDataTables.OpenTable(TableName: string): boolean;
begin

    Result:=True;

    try
        if CustFilter =  '' then
            FStrSQL:=TSql.SELECT + ColumnsToList(Columns, TQuotes.Disabled) + TSql.FROM + TableName;

        if CustFilter <> '' then

            /// <remarks>
            /// Note: it requires "WHERE" clause.
            /// </remarks>

            FStrSQL:=TSql.SELECT + ColumnsToList(Columns, TQuotes.Disabled) + TSql.FROM + TableName + CustFilter;

        DataSet:=ExecSQL;
    except
        on E: Exception do
        begin
            MainForm.FAppEvents.Log(MainForm.EventLogPath, 'SQL: [OpenTable] Error occured: ' + E.Message);
            Result:=False;
        end;
    end;

end;


/// <summary>
/// Insert row(s) into given table.
/// </summary>
/// <param name="IsExplicit">
/// False - sends to SQL Server bare SQL statement(s) which imposes
///         autocommit after each statement and involves no rollback
///         in case of default.
/// True - sends to SQL Server statement(s) with begin and end of the
///        transaction, it allows rollback in case of default.
/// </param>

function TDataTables.InsertInto(TableName: string; IsExplicit: boolean; ExtSourceGrid: TStringGrid = nil; ExtSourceArray: TALists = nil; HeaderPresent: boolean = True): boolean;
begin

    Result:=False;
    try

        if not(string.IsNullOrEmpty(Columns.Text)) then
        begin

            if (not(string.IsNullOrEmpty(Values.Text))) and ( (ExtSourceGrid = nil) and (Pointer(ExtSourceArray) = nil) ) then
            begin
                FStrSQL:=TSql.INSERT +
                           TableName + TChars.SPACE + BracketStr(ColumnsToList(Columns, TQuotes.Disabled), Round) +
                         TSql.VAL +
                           BracketStr(ColumnsToList(Values, TQuotes.Enabled), Round);
            end;

            if (ExtSourceGrid = nil)  and (ExtSourceArray <> nil) then FStrSQL:=ToSqlInsert(ExtSourceArray, nil, TableName, ColumnsToList(Columns, TQuotes.Disabled), HeaderPresent);
            if (ExtSourceGrid <> nil) and (ExtSourceArray = nil)  then FStrSQL:=ToSqlInsert(nil, ExtSourceGrid, TableName, ColumnsToList(Columns, TQuotes.Disabled), HeaderPresent);

            if IsExplicit then
            begin
                var Transact: string;
                Transact:=TransactTemp;
                Transact:=StringReplace(Transact, '{SWITCH}',       'OFF',        [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{SimpleInput}',  FStrSQL,      [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{CommonDelete}', TChars.SPACE, [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{CommonSelect}', TChars.SPACE, [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{ComplexInput}', TChars.SPACE, [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{Begin}',        TChars.SPACE, [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{End}',          TChars.SPACE, [rfReplaceAll]);
                FStrSQL:=Transact;
            end;

            ExecSQL;
            if string.IsNullOrEmpty(LastErrorMsg) then Result:=True
                else
                    MainForm.FAppEvents.Log(MainForm.EventLogPath, 'SQL: [InsertInto] Error occured: ' + LastErrorMsg);

        end;

    except
        on E: Exception do
        begin
            MainForm.FAppEvents.Log(MainForm.EventLogPath, 'SQL: [InsertInto] Error occured: ' + E.Message);
            Result:=False;
        end;
    end;

end;


/// <summary>
/// Perform update on given columns.
/// </summary>
/// <param name="IsExplicit">
/// False - sends to SQL Server bare SQL statement(s) which imposes
///         autocommit after each statement and involves no rollback
///         in case of default.
/// True - sends to SQL Server statement(s) with begin and end of the
///        transaction, it allows rollback in case of default.
/// </param>

function TDataTables.UpdateRecord(TableName: string; IsExplicit: boolean; SingleCondition: string = '' {Optional}): boolean;
begin

    Result:=False;

    if (string.IsNullOrEmpty(Columns.Text))    or  (string.IsNullOrEmpty(Values.Text))     then Exit;
    if (string.IsNullOrEmpty(SingleCondition)) and (string.IsNullOrEmpty(Conditions.Text)) then Exit;

    // Execute update for each column
    try

        var Temp:     string;
        var Lock:     string;
        var Begins:   string;
        var Ends:     string;
        var AddComma: string;

        if not(SingleCondition.Length) > 0 then
        begin

            /// <remarks>
            /// Generates single UPDATE statement for each defined column separately, eg.:
            ///     UPDATE Customer.DailyComment SET Stamp = '2018-07-31 22:52:29' WHERE Cuid = '870768307200' AND AgeDate = '2018-07-30';
            ///     UPDATE Customer.DailyComment SET FixedComment = 'test 55' WHERE Cuid = '870768307200' AND AgeDate = '2018-07-30';
            /// It is possible to have different columns, values and conditions. Performs well if number of updates are low.
            /// WARNING: This one will not lock table rows during the update!
            /// </remarks>

            for var iCNT: integer:=0 to Columns.Count - 1 do
            begin
                Temp:=Temp + (
                                TSql._UPDATE +
                                    TableName +
                                TSql._SET +
                                    Columns.Strings[iCNT] +
                                TSql.EQUAL +
                                    QuotedStr(Values.Strings[iCNT]) +
                                TSql.WHERE +
                                    Conditions.Strings[iCNT] + ';'
                             ) + TChars.CRLF;
            end;

        end
        else
        begin

            /// <remarks>
            /// Generates UPDATE statement with multiple columns and values, but with common condition, eg.:
            ///     UPDATE Customer.DailyComment
            ///	    SET
		    ///         Stamp = '2018-07-31 22:52:29',
	    	///         UserAlias = 'TOMEK',
    		///         FixedComment = 'test 55'
            ///     WHERE Cuid = '870768307200' AND AgeDate = '2018-07-30';
            /// It is possible to update one row with many columns and values at the time. Performs well with large number of columns.
            /// </remarks>

            Begins:=TSql._UPDATE + TableName + TChars.LF + TSql._SET + TChars.LF;
            Ends:=TSql.WHERE + SingleCondition;
            AddComma:=TChars.COMMA;

            for var iCNT: integer:=0 to (Columns.Count - 1) do
            begin
                if iCNT = (Columns.Count - 1) then AddComma:=TChars.SPACE;
                Temp:=Temp + (
                                Columns.Strings[iCNT] +
                                TSql.EQUAL +
                                QuotedStr(Values.Strings[iCNT]) +
                                AddComma +
                                TChars.LF
                             );
            end;

            Temp:=Begins + Temp + Ends;
            Lock:=CommonSelect;
            Lock:=StringReplace(Lock, '{DestTable}', TableName,       [rfReplaceAll]);
            Lock:=StringReplace(Lock, '{Condition}', SingleCondition, [rfReplaceAll]);
            Begins:=TSql._BEGIN;
            Ends:=TSql._END

        end;

        FStrSQL:=Temp;

        if IsExplicit then
        begin
            var Transact: string;
            Transact:=TransactTemp;
            Transact:=StringReplace(Transact, '{SWITCH}',       'OFF',        [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonSelect}', Lock,         [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{SimpleInput}',  FStrSQL,      [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonDelete}', TChars.SPACE, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{ComplexInput}', TChars.SPACE, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{Begin}',        Begins,       [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{End}',          Ends,         [rfReplaceAll]);
            FStrSQL :=Transact;
        end;

        ExecSQL;
        if string.IsNullOrEmpty(FLastErrorMsg) then Result:=True;

    except
        on E: Exception do
        begin
            MainForm.FAppEvents.Log(MainForm.EventLogPath, 'SQL: [InsertInto] Error occured: ' + E.Message);
            Result:=False;
        end;
    end;

end;


/// <summary>
/// Delete only single record from the given table.
/// </summary>

function TDataTables.DeleteRecord(TableName: string; KeyName: string; KeyValue: string; IsExplicit: boolean): boolean;
begin

    Result:=False;

    if (string.IsNullOrEmpty(TableName))
        or (string.IsNullOrEmpty(KeyValue))
        or (string.IsNullOrEmpty(KeyName))
            then Exit;

    try
        FStrSQL:=TSql.DELETE_FROM + TableName + TSql.WHERE + KeyName + TSql.EQUAL + QuotedStr(KeyValue);

        if IsExplicit then
        begin
            var Transact: string;
            Transact:=TransactTemp;
            Transact:=StringReplace(Transact, '{SWITCH}',       'ON',         [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonDelete}', FStrSQL,      [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonSelect}', TChars.SPACE, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{SimpleInput}',  TChars.SPACE, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{ComplexInput}', TChars.SPACE, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{Begin}',        TChars.SPACE, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{End}',          TChars.SPACE, [rfReplaceAll]);
            FStrSQL :=Transact;
        end;

        ExecSQL;
        if string.IsNullOrEmpty(FLastErrorMsg) then Result:=True;

    except
        on E: Exception do
        begin
            MainForm.FAppEvents.Log(MainForm.EventLogPath, 'SQL: [DeleteRecord] Error occured: ' + E.Message);
            Result:=False;
        end;
    end;

end;


end.

