
{$I \Include\Header.inc}

unit SQL;

interface

uses
    Main, SysUtils, Windows, Classes, ADODB, StrUtils, Variants, StdCtrls;

type

    /// <summary>
    ///     Basic class for SQL handling, execute and generate SQL expressions.
    /// </summary>

    TMSSQL = class
    {$TYPEINFO ON}
    public
        var FParamList  : TLists;
        var StrSQL      : string;
        var ADOCon      : TADOConnection;
        var CmdType     : TCommandType;
        var RowsAffected: integer;
        var LastErrorMsg: string;
    published
        constructor Create(Connector: TADOConnection);
        destructor  Destroy; override;
        procedure   ClearSQL;
        function    CleanStr(Text: string; Quoted: boolean): string;
        function    ExecSQL: _Recordset;

        function    GridToSql(Grid: TStringGrid; tblName: string; tblColumns: string; sRow: integer; sCol: integer): string;
        function    ArrayToSql(Table: TLists; tblName: string; tblColumns: string): string;

        function    SqlToGrid(var Grid: TStringGrid; RS: _Recordset; AutoNoCol: boolean; Headers: boolean): boolean;
        function    SqlToSimpleList(var List: TComboBox; RS: _Recordset): boolean;
    end;

    /// <summary>
    ///     Basic CRUD operation handler for database tables.
    /// </summary>

    TDataTables = class(TMSSQL)
    {$TYPEINFO ON}
    private
        var pConnStr :  string;
    public
        var idThd          :  integer;
        var CustFilter     :  string;
        var DataSet        :  _Recordset;
        var Columns        :  TStringList;
        var Values         :  TStringList;
        var Conditions     :  TStringList;
        property   ConnStr : string read pConnStr;
    published
        constructor Create(Connector: TADOConnection); overload;
        destructor  Destroy; override;
        function    BracketStr(Expression: string; BracketType: integer): string;
        function    ColumnsToList(Holder: TStringList; Quoted: integer): string;
        procedure   CleanUp;
        function    OpenTable(TableName: string): boolean;
        function    InsertInto(TableName: string; TransactionType: integer): boolean;
        function    UpdateRecord(TableName: string; TransactionType: integer; SingleCondition: string = '' {=OPTIONAL} ): boolean;
        function    DeleteRecord(TableName: string; KeyName: string; KeyValue: string; TransactionType: integer): boolean;
    end;


implementation


// MS SQL -------------------------------------------------------------------------------------------------------------------------------------------------- //


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TMSSQL.Create(Connector: TADOConnection);
begin
    StrSQL :='';
    ADOCon :=Connector;
    CmdType:=cmdText;
    SetLength(FParamList, 1, 2);
end;

destructor TMSSQL.Destroy;
begin
    (* EMPTY *)
    inherited;
end;


// ------------------------------------------------------------------------------------------------------------------------------------- SQL HANDLER METHODS //


/// <summary>
///     Clear last SQL command with rows affected and error (if any).
/// </summary>

procedure TMSSQL.ClearSQL;
begin
    StrSQL:='';
    LastErrorMsg:='';
    RowsAffected:=0;
end;

/// <summary>
///     Remove characters that may negatively affect SQL expression.
/// </summary>

function TMSSQL.CleanStr(Text: string; Quoted: boolean): string;
begin
    Result:='';
    Text:=StringReplace(Text, TAB,   SPACE, [rfReplaceAll]);
    Text:=StringReplace(Text, QUOTE, SPACE, [rfReplaceAll]);
    Text:=StringReplace(Text, CRLF,  SPACE, [rfReplaceAll]);
    if Quoted then Result:=QuotedStr(Text) else Result:=Text;
end;

/// <summary>
///     Execute dml, ddl, tcl and dcl statements.
/// </summary>
/// <remarks>
///     Inserting records using 'values' keyword has limit up to 1000 records.
///     to insert more than 1000 records with single query statement, we can
///     split query into smaller chunks and execute "insert into [] values ()"
///     as many times as needed. Alternatively, instead of 'values' keyword we
///     may use 'union all' and 'select' statement.
/// </remarks>

function TMSSQL.ExecSQL: _Recordset;
var
    Query:  TADOCommand;
begin
    Result:=nil;

    if not (Length(StrSQL)) > 0 then Exit;

    Query:=TADOCommand.Create(nil);
    Query.Connection:=ADOCon;

    try
        try
            Query.CommandType:=CmdType;
            Query.CommandText:=StrSQL;
            RowsAffected:=0;
            Result:=Query.Execute(RowsAffected, 0);
        except
            on E: Exception do
            begin
                LastErrorMsg:=E.Message;
                Result:=nil;
            end;
        end;
    finally
        Query.Free;
    end;

end;

/// <summary>
///     Transfer grid data to insert into statement.
/// </summary>
/// <remarks>
///     This function build sql insert into expression with 'select' and 'union' keywords
///     and therefore 1000 record limit does not apply here.
//      column name must not use quotes and names must be delaminated by comma.
//      using multi-dimensional array
/// </remarks>

function TMSSQL.ArrayToSql(Table: TLists; tblName: string; tblColumns: string): string;
var
    iCNT    : integer;
    jCNT    : integer;
    LEAD    : string;
    LINE    : string;
    LINES   : string;
    mRows   : integer;
    mCols   : integer;
    Clean   : string;
    Transact: string;
begin

    Result:='';
    LEAD:=INSERT + SPACE + tblName + ' ( ' + tblColumns + ' ) ' + CRLF;
    mRows:=High(Table) - 1;
    mCols:=High(Table[1]);

    for iCNT:=0 to mRows do
    begin
        LINE:=SELECT + SPACE;

        for jCNT:=0 to mCols do
        begin

            // Clear data from characters that may injured SQL execution
            Clean:=CleanStr(Table[iCNT, jCNT], True);

            if (jCNT <> mCols) then LINE:=LINE + Clean + COMMA;
            if (jCNT =  mCols) and (iCNT <> mRows) then LINE:=LINE + QuotedStr(Table[iCNT, jCNT]) + SPACE + UNION + CRLF;
            if (jCNT =  mCols) and (iCNT =  mRows) then LINE:=LINE + QuotedStr(Table[iCNT, jCNT]) + CRLF;

        end;

        LINES:=LINES + LINE;
        LINE:='';

    end;

    // Output SQL expression (no transaction)
    Result:=(LEAD + LINES);

end;

function TMSSQL.GridToSql(Grid: TStringGrid; tblName: string; tblColumns: string; sRow: integer; sCol: integer): string;         /// REMOVE THAT ONE AND UPDATE ABOVE APPROPIATELY!!!
var
    iCNT    : integer;
    jCNT    : integer;
    LEAD    : string;
    LINE    : string;
    LINES   : string;
    mRows   : integer;
    mCols   : integer;
    Clean   : string;
    Transact: string;
begin

    Result:='';
    LEAD:=INSERT + SPACE + tblName + ' ( ' + tblColumns + ' ) ' + CRLF;
    mRows:=Grid.RowCount - 1;
    mCols:=Grid.ColCount - 1;

    for iCNT:=sRow to mRows do
    begin
        LINE:=SELECT + SPACE;

        for jCNT:=sCol to mCols do
        begin

            Clean:=CleanStr(Grid.Cells[jCNT, iCNT], True);

            if (jCNT <> mCols) then LINE:=LINE + Clean + COMMA;
            if (jCNT =  mCols) and (iCNT <> mRows) then LINE:=LINE + QuotedStr(Grid.Cells[jCNT, iCNT]) + SPACE + UNION + CRLF;
            if (jCNT =  mCols) and (iCNT =  mRows) then LINE:=LINE + QuotedStr(Grid.Cells[jCNT, iCNT]) + CRLF;
        end;

        LINES:=LINES + LINE;
        LINE:='';
    end;

    // Output SQL expression
    Result:=(LEAD + LINES);

end;

/// <summary>
///     Move recordset content to string grid with headers.
/// </summary>

function TMSSQL.SqlToGrid(var Grid: TStringGrid; RS: _Recordset; AutoNoCol: boolean; Headers: boolean): boolean;
var
    iCNT:  integer;
    jCNT:  integer;
begin
    Result:=False;
    iCNT:=0;

    // Exit condition
    if RS = nil then Exit;
    if (RS.EOF) or (RS.BOF) then Exit;
    if RS.Status <> adOpenAll then Exit;

    try
        Grid.FixedCols:=1;
        Grid.FixedRows:=1;
        Grid.ColCount:=RS.Fields.Count + 1;
        Grid.RowCount:=RS.RecordCount  + 1;

        while not RS.EOF do
        begin
            for jCNT:=1 to RS.Fields.Count do
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
///     Move one column to TComboBox component with no header.
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


/// <summary>
///     Initialize class with three string lists holding columns, vaues and conditions (for WHERE clause etc.).
/// </summary>

constructor TDataTables.Create(Connector: TADOConnection);
begin
    idThd     :=0;
    CustFilter:='';
    DataSet   :=nil;
    Columns   :=TStringList.Create;
    Values    :=TStringList.Create;
    Conditions:=TStringList.Create;
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
///     Helper method to surround string with brackets.
/// </summary>

function TDataTables.BracketStr(Expression: string; BracketType: integer): string;
begin
    Result:='';
    if BracketType = brRound  then Result:='(' + Expression + ')';
    if BracketType = brSquare then Result:='[' + Expression + ']';
    if BracketType = brCurly  then Result:='{' + Expression + '}';
end;

/// <summary>
///     Transpose columns to rows.
/// </summary>

function TDataTables.ColumnsToList(Holder: TStringList; Quoted: integer): string;
var
    iCNT:   integer;
begin
    Result:=ALL;

    { PERFORM }
    if (Holder.Text <> '') and (Holder.Count > 0) then
    begin
        Result:='';

        for iCNT:=0 to Holder.Count - 1 do
        if Result = '' then
        begin
            if Quoted = enQuotesOff then Result:=Holder.Strings[iCNT];
            if Quoted = enQuotesOn  then Result:=QuotedStr(Holder.Strings[iCNT]);
        end
        else
        begin
            if Quoted = enQuotesOff then Result:=Result + COMMA + Holder.Strings[iCNT];
            if Quoted = enQuotesOn  then Result:=Result + COMMA + QuotedStr(Holder.Strings[iCNT]);
        end;

    end;

end;

/// <summary>
///     Clear all the list.
/// </summary>

procedure TDataTables.CleanUp;
begin
    Columns.Clear;
    Values.Clear;
    Conditions.Clear;
end;

/// <summary>
///     Open table to recordset.
/// </summary>

function TDataTables.OpenTable(TableName: string): boolean;
begin

    Result:=True;

    try
        if CustFilter =  '' then
            StrSQL:=SELECT + ColumnsToList(Columns, enQuotesOff) + FROM + TableName;

        if CustFilter <> '' then

            /// <remarks>
            ///     Require "WHERE" clause.
            /// </remarks>

            StrSQL:=SELECT + ColumnsToList(Columns, enQuotesOff) + FROM + TableName + CustFilter;

        DataSet:=ExecSQL;
    except
        Result:=False;
    end;

end;

/// <summary>
///     Insert single row into table.
/// </summary>
/// <param name="TransactionType">
///     Integer, use flags:
///     ttImplicit - sends to SQL Server bare SQL statement(s) which imposes
///                  autocommit after each statement and involves no rollback
///                  in case of default.
///     ttExplicit - sends to SQL Server statement(s) with begin and end of the
///                  transaction, it allows rollback in case of default.
/// </param>

function TDataTables.InsertInto(TableName: string; TransactionType: integer): boolean;
var
    Transact: string;
begin

    Result:=False;

    try

        if (Values.Text <> '') and (Columns.Text <> '') then
        begin
            StrSQL:=INSERT +
                      TableName + SPACE + BracketStr(ColumnsToList(Columns, enQuotesOff), brRound) +
                    VAL +
                      BracketStr(ColumnsToList(Values, enQuotesOn), brRound);

            if TransactionType = ttExplicit then
            begin
                Transact:=TransactTemp;
                Transact:=StringReplace(Transact, '{SWITCH}',       'OFF',  [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{SimpleInput}',  StrSQL, [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{CommonDelete}', SPACE,  [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{CommonSelect}', SPACE,  [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{ComplexInput}', SPACE,  [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{Begin}',        SPACE,  [rfReplaceAll]);
                Transact:=StringReplace(Transact, '{End}',          SPACE,  [rfReplaceAll]);
                StrSQL  :=Transact;
            end;

            ExecSQL;
            if string.IsNullOrEmpty(LastErrorMsg) then Result:=True;

        end;

    except
        Result:=False;
    end;

end;

/// <summary>
///     Perform update on given columns.
/// </summary>
/// <param name="TransactionType">
///     Integer, use flags:
///     ttImplicit - sends to SQL Server bare SQL statement(s) which imposes
///                  autocommit after each statement and involves no rollback
///                  in case of default.
///     ttExplicit - sends to SQL Server statement(s) with begin and end of the
///                  transaction, it allows rollback in case of default.
/// </param>

function TDataTables.UpdateRecord(TableName: string; TransactionType: integer; SingleCondition: string = '' {=OPTIONAL}): boolean;
var
    iCNT:     integer;
    Temp:     string;
    Transact: string;
    Begins:   string;
    Ends:     string;
    Lock:     string;
    AddComma: string;
begin

    Result:=False;

    if (string.IsNullOrEmpty(Columns.Text)) or (string.IsNullOrEmpty(Values.Text)) then Exit;
    if (string.IsNullOrEmpty(SingleCondition)) and (string.IsNullOrEmpty(Conditions.Text)) then Exit;

    // Execute update for each column
    try

        if not(SingleCondition.Length) > 0 then
        begin

            /// <remarks>
            ///     Generates single UPDATE statement for each defined column separately, eg.:
            ///         UPDATE Customer.DailyComment SET Stamp = '2018-07-31 22:52:29' WHERE Cuid = '870768307200' AND AgeDate = '2018-07-30';
            ///         UPDATE Customer.DailyComment SET FixedComment = 'test 55' WHERE Cuid = '870768307200' AND AgeDate = '2018-07-30';
            ///     It is possible to have different columns, values and conditions. Perform well if number of updates are low.
            ///     WARNING: This one will not lock table rows during the update!
            /// </remarks>

            for iCNT:=0 to Columns.Count - 1 do
            begin
                Temp:=Temp + (
                                _UPDATE +
                                    TableName +
                                _SET +
                                    Columns.Strings[iCNT] +
                                EQUAL +
                                    QuotedStr(Values.Strings[iCNT]) +
                                WHERE +
                                    Conditions.Strings[iCNT] + ';'
                             ) + CRLF;
            end;

        end
        else

        /// <remarks>
        ///     Generates UPDATE statement with multiple columns and values, but with common condition, eg.:
        ///             UPDATE Customer.DailyComment
        ///	            SET
		///                 Stamp = '2018-07-31 22:52:29',
		///                 UserAlias = 'TOMEK',
		///                 FixedComment = 'test 55'
        ///             WHERE Cuid = '870768307200' AND AgeDate = '2018-07-30';
        ///     It is possible to update one row with many columns and values at the time. It performs well with large number of columns.
        /// </remarks>

        begin

            Begins:=_UPDATE + TableName + LF + _SET + LF;
            Ends:=WHERE + SingleCondition;
            AddComma:=COMMA;

            for iCNT:=0 to (Columns.Count - 1) do
            begin
                if iCNT = (Columns.Count - 1) then AddComma:=SPACE;
                Temp:=Temp + (
                                Columns.Strings[iCNT] +
                                EQUAL +
                                QuotedStr(Values.Strings[iCNT]) +
                                AddComma +
                                LF
                             );
            end;

            Temp:=Begins + Temp + Ends;
            Lock:=CommonSelect;
            Lock:=StringReplace(Lock, '{DestTable}', TableName,       [rfReplaceAll]);
            Lock:=StringReplace(Lock, '{Condition}', SingleCondition, [rfReplaceAll]);
            Begins:=_BEGIN;
            Ends:=_END
        end;

        StrSQL:=Temp;

        if TransactionType = ttExplicit then
        begin
            Transact:=TransactTemp;
            Transact:=StringReplace(Transact, '{SWITCH}',       'OFF',  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonSelect}', Lock,   [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{SimpleInput}',  StrSQL, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonDelete}', SPACE,  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{ComplexInput}', SPACE,  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{Begin}',        Begins, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{End}',          Ends,   [rfReplaceAll]);
            StrSQL  :=Transact;
        end;

        ExecSQL;
        if string.IsNullOrEmpty(LastErrorMsg) then Result:=True;

    except
        Result:=False;
    end;

end;

/// <summary>
///     Delete only single record from the given table.
/// </summary>

function TDataTables.DeleteRecord(TableName: string; KeyName: string; KeyValue: string; TransactionType: integer): boolean;
var
    Transact: string;
begin
    Result:=False;

    if (string.IsNullOrEmpty(TableName))
        or (string.IsNullOrEmpty(KeyValue))
        or (string.IsNullOrEmpty(KeyName))
            then Exit;

    try
        StrSQL:=DELETE_FROM + TableName + WHERE + KeyName + EQUAL + QuotedStr(KeyValue);

        if TransactionType = ttExplicit then
        begin
            Transact:=TransactTemp;
            Transact:=StringReplace(Transact, '{SWITCH}',       'ON',   [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonDelete}', StrSQL, [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{CommonSelect}', SPACE,  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{SimpleInput}',  SPACE,  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{ComplexInput}', SPACE,  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{Begin}',        SPACE,  [rfReplaceAll]);
            Transact:=StringReplace(Transact, '{End}',          SPACE,  [rfReplaceAll]);
            StrSQL  :=Transact;
        end;

        ExecSQL;
        if string.IsNullOrEmpty(LastErrorMsg) then Result:=True;

    except
        Result:=False;
    end;

end;

end.
