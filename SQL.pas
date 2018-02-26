{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit SQL;

interface

uses
  SysUtils, Windows, Classes, ADODB, StrUtils, Variants, Main;

{ --------------------------------------------------------------- ! MS SQL CLASS ! -------------------------------------------------------------------------- }
type
  TMSSQL = class                                          (* BASE CLASS FOR SQL HANDLING *)
  {$TYPEINFO ON}
  private
    var         pStrSQL      : string;
    var         pADOCon      : TADOConnection;
    const       CRLF         : string = #13#10;
    const       COMMA        : char   = #44;
    const       SPACE        : char   = #32;
    const       TAB          : char   = #9;
    const       QUOTE        : char   = #39;
    const       INSERT       : string = 'INSERT INTO';
    const       SELECT       : string = 'SELECT';
    const       UPDATE       : string = 'UPDATE';
    const       _SET         : string = 'SET';
    const       UNION        : string = 'UNION ALL';
  public
    property    StrSQL  : string         read pStrSQL write pStrSQL;
    property    ADOCon  : TADOConnection read pADOCon write pADOCon;
  published
    constructor Create(Connector: TADOConnection);
    procedure   ClearSQL;
    function    CleanStr(Text: string; Quoted: boolean): string;
    function    OpenSQL: _Recordset;
    function    ExecSQL: boolean;
    function    GridToSql(Grid: TStringGrid; tblName: string; tblColumns: string; sRow: integer; sCol: integer): string;
    function    SqlToGrid(var Grid: TStringGrid; RS: _Recordset; AutoNoCol: boolean): boolean;
    function    ArrayToSql(Table: TStrArray; tblName: string; tblColumns: string): string;
  end;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

{ ############################################################# ! BASE CLASS METHODS ! ###################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------- CREATE OBJECT AND SETUP EMPTY SQL }
constructor TMSSQL.Create(Connector: TADOConnection);
begin
  pStrSQL:='';
  pADOCon:=Connector;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- CLEAR SQL EXPRESSION }
procedure TMSSQL.ClearSQL;
begin
  StrSQL:='';
end;

{ ------------------------------------------------------------------------------------------------ REMOVE CHARACTERS THAT MAY NEGATIVELY AFFECT SQL EXECUTION }
function TMSSQL.CleanStr(Text: string; Quoted: boolean): string;
begin
  Result:='';
  Text:=StringReplace(Text, TAB,   SPACE, [rfReplaceAll]);
  Text:=StringReplace(Text, QUOTE, SPACE, [rfReplaceAll]);
  Text:=StringReplace(Text, CRLF,  SPACE, [rfReplaceAll]);
  if Quoted then Result:=QuotedStr(Text) else Result:=Text;
end;

{ ------------------------------------------------------------------------------------------------------- EXECUTE ONLY SELECT STATEMENTS AND RETURN RECORDSET }
function TMSSQL.OpenSQL: _Recordset;
var
  Query: TADOCommand;
begin
  { DEFAULT RETURN VALUE }
  Result:=nil;
  { ONLY 'SELECT' IS ALLOWED }
  if not (Length(StrSQL)) > 0 then Exit;
  if not (UpperCase(LeftStr(StrSQL, 6)) = 'SELECT') then Exit;
  { PROCESS GIVEN EXPRESSION }
  Query:=TADOCommand.Create(nil);
  Query.Connection:=ADOCon;
  try
    try
      Query.CommandText:=StrSQL;
      Result:=Query.Execute;
    except
      Result:=nil;
    end;
  finally
    Query.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ EXECUTE NON-SELECT STATEMENT }

(* IMPORTANT NOTE: INSERTING RECORDS USING 'VALUES' KEYWORD HAS LIMIT UP TO 1000 RECORDS  *)
(*                 TO INSERT MORE THAN 1000 RECORDS WITH SINGLE QUERY STATEMENT, WE CAN   *)
(*                 SPLIT QUERY INTO SMALLER CHUNKS AND EXECUTE "INSERT INTO [] VALUES ()" *)
(*                 AS MANY TIMEs AS NEEDED. ALTERNATIVELY, INSTEAD OF 'VALUES' KEYWORD WE *)
(*                 MAY USE 'UNION ALL' AND 'SELECT' STATEMENT.                            *)

function TMSSQL.ExecSQL: boolean;
var
  Query: TADOCommand;
begin
  { INITIALIZE }
  Result:=False;
  { ONLY 'NON-SELECT' IS ALLOWED }
  if (Length(StrSQL)) = 0 then Exit;
  if (UpperCase(LeftStr(StrSQL, 6)) = 'SELECT') then Exit;
  { PROCESS GIVEN EXPRESSION }
  Query:=TADOCommand.Create(nil);
  Query.Connection:=ADOCon;
  try
    try
      Query.CommandText:=StrSQL;
      Query.Execute;
      Result:=True;
    except
      Result:=False;
    end;
  finally
    Query.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------- TRANSFER GRID DATA TO INSERT INTO STATEMENT }

(* IMPORTANT NOTE: THIS FUNCTION BUILD SQL INSERT INTO EXPRESSION WITH 'SELECT' AND 'UNION' KEWORDS *)
(*                 AND THEREFORE 1000 RECORD LIMIT DOES NOT APPLY HERE.                             *)
(*                 COLUMN NAME MUST NOT USE QUOTES AND NAMES MUST BE DELIMINATED BY COMMA.          *)

(* USING MULTI-DIMENSIONAL ARRAY *)

function TMSSQL.ArrayToSql(Table: TStrArray; tblName: string; tblColumns: string): string;
var
  iCNT   : integer;
  jCNT   : integer;
  LEAD   : string;
  LINE   : string;
  LINES  : string;
  mRows  : integer;
  mCols  : integer;
  Clean  : string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:='';
  LEAD:=INSERT + SPACE + tblName + ' ( ' + tblColumns + ' ) ' + CRLF;
  mRows:=High(Table) - 1;
  mCols:=29;
  { ------------------------------------------------------------------------------------------------------------------------------------------------ MAKE SQL }
  for iCNT:=0 to mRows do
  begin
    LINE:=SELECT + SPACE;
    for jCNT:=0 to mCols do
    begin
      { ------------------------------------------------------------------------------------------- CLEAR DATA FROM CHARACTERS THAT MAY INJURED SQL EXECUTION }
      Clean:=CleanStr(Table[iCNT, jCNT], True);
      { -------------------------------------------------------------------------------------------------------------------------------------------- POPULATE }
      if (jCNT <> mCols) then LINE:=LINE + Clean + COMMA;
      if (jCNT =  mCols) and (iCNT <> mRows) then LINE:=LINE + QuotedStr(Table[iCNT, jCNT]) + SPACE + UNION + CRLF;
      if (jCNT =  mCols) and (iCNT =  mRows) then LINE:=LINE + QuotedStr(Table[iCNT, jCNT]) + CRLF;
    end;
    LINES:=LINES + LINE;
    LINE:='';
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- OUTPUT BUILT SQL }
  Result:=LEAD + LINES;
end;

(* USING STRING GRID *)

function TMSSQL.GridToSql(Grid: TStringGrid; tblName: string; tblColumns: string; sRow: integer; sCol: integer): string;
var
  iCNT   : integer;
  jCNT   : integer;
  LEAD   : string;
  LINE   : string;
  LINES  : string;
  mRows  : integer;
  mCols  : integer;
  Clean  : string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:='';
  LEAD:=INSERT + SPACE + tblName + ' ( ' + tblColumns + ' ) ' + CRLF;
  mRows:=Grid.RowCount - 1;
  mCols:=Grid.ColCount - 1;
  { ------------------------------------------------------------------------------------------------------------------------------------------------ MAKE SQL }
  for iCNT:=sRow to mRows do
  begin
    LINE:=SELECT + SPACE;
    for jCNT:=sCol to mCols do
    begin
      { ------------------------------------------------------------------------------------------- CLEAR DATA FROM CHARACTERS THAT MAY INJURED SQL EXECUTION }
      Clean:=CleanStr(Grid.Cells[jCNT, iCNT], True);
      { -------------------------------------------------------------------------------------------------------------------------------------------- POPULATE }
      if (jCNT <> mCols) then LINE:=LINE + Clean + COMMA;
      if (jCNT =  mCols) and (iCNT <> mRows) then LINE:=LINE + QuotedStr(Grid.Cells[jCNT, iCNT]) + SPACE + UNION + CRLF;
      if (jCNT =  mCols) and (iCNT =  mRows) then LINE:=LINE + QuotedStr(Grid.Cells[jCNT, iCNT]) + CRLF;
    end;
    LINES:=LINES + LINE;
    LINE:='';
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- OUTPUT BUILT SQL }
  Result:=LEAD + LINES;
end;

{ -------------------------------------------------------------------------------------------------------- MOVE RECORDSET CONTENT TO STRING GRID WITH HEADERS }
function TMSSQL.SqlToGrid(var Grid: TStringGrid; RS: _Recordset; AutoNoCol: boolean): boolean;
var
  iCNT:  integer;
  jCNT:  integer;
begin
  { INITIALIZE }
  Result:=False;
  iCNT:=0;
  { EXIT CONDITION }
  if not (RS.RecordCount > 0) then Exit;
  { PROCESS }
  try
    Grid.FixedCols:=1;
    Grid.FixedRows:=1;
    Grid.ColCount:=RS.Fields.Count + 1;
    Grid.RowCount:=RS.RecordCount  + 1;
    while not RS.EOF do
    begin
      for jCNT:=1 to RS.Fields.Count do
      begin
        if iCNT = 0 then Grid.Cells[jCNT, iCNT]:=VarToStr(RS.Fields[jCNT - 1].Name); (* HEADERS *)
        Grid.Cells[jCNT, iCNT + 1]:=VarToStr(RS.Fields[jCNT - 1].Value);             (* DATA    *)
      end;
      RS.MoveNext;
      Inc(iCNT);
      { LP FOR FIXED COLUMN }
      if (AutoNoCol) and (iCNT > 0) then Grid.Cells[0, iCNT]:=IntToStr(iCNT);
    end;
    Result:=True;
  except
    Result:=False;
  end;
end;

end.
