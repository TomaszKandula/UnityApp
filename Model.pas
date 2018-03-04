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
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Model;  //REFACTOR!! remove polymorphism, one table one class with properties as columns

interface

uses
  SysUtils, Windows, Classes, ADODB, StrUtils, Variants, Main;

{ ---------------------------------------------------------- ! DATABASE TABLES HANDLING ! ------------------------------------------------------------------- }
type
  TDataTables = class
  {$TYPEINFO ON}
  private
    pidThd      : integer;
    pManyLines  : boolean;
    pTableSelect: string;
    pDataSet    : _Recordset;
  public
    property idThd       : integer    read pidThd       write pidThd;
    property ManyLines   : boolean    read pManyLines   write pManyLines;
    property TableSelect : string     read pTableSelect write pTableSelect;
    property DataSet     : _Recordset read pDataSet     write pDataSet;
   published
    function Read   : boolean; virtual; abstract;
    function Write  : boolean; virtual; abstract;
  end;

{ --------------------------------------------------------------- ! TRACKER DETAILS ! ----------------------------------------------------------------------- }
type                                                         (* RUN IN WORKER THREAD ONLY *)
  TTracker = class(TDataTables)  //refactor!! split tbl_tracker and tbl_addressbook
  {$TYPEINFO ON}
  private
    { COLUMNS' NAMES }
    const nCUID          :  string = 'CUID';
    const nCOCODE        :  string = 'COCODE';
    const nBRANCH        :  string = 'BRANCH';
    const nEMAILS        :  string = 'EMAILS';
    const nESTATEMENTS   :  string = 'ESTATEMENTS';
    const nLEGALTO       :  string = 'LEGALTO';
    const nREMINDER1     :  string = 'REMINDER1';
    const nREMINDER2     :  string = 'REMINDER2';
    const nREMINDER3     :  string = 'REMINDER3';
    const nLEGALACTION   :  string = 'LEGALACTION';
    const nSEND_NOTE_FROM:  string = 'SEND_NOTE_FROM';
    { TBL_ADDRESSBOOK }
    var pCUID            :  string;
    var pEMAILS          :  string;
    var pESTATEMENTS     :  string;
    { TBL_COMPANY }
    var pCOCODE          :  string;
    var pBRANCH          :  string;
    var pLEGALTO         :  string;
    var pREMINDER1       :  string;
    var pREMINDER2       :  string;
    var pREMINDER3       :  string;
    var pLEGALACTION     :  string;
    var pSEND_NOTE_FROM  :  string;
  public
    property CUID          : string  read pCUID           write pCUID;
    property COCODE        : string  read pCOCODE         write pCOCODE;
    property BRANCH        : string  read pBRANCH         write pBRANCH;
    property EMAILS        : string  read pEMAILS         write pEMAILS;
    property ESTATEMENTS   : string  read pESTATEMENTS    write pESTATEMENTS;
    property LEGALTO       : string  read pLEGALTO        write pLEGALTO;
    property REMINDER1     : string  read pREMINDER1      write pREMINDER1;
    property REMINDER2     : string  read pREMINDER2      write pREMINDER2;
    property REMINDER3     : string  read pREMINDER3      write pREMINDER3;
    property LEGALACTION   : string  read pLEGALACTION    write pLEGALACTION;
    property SEND_NOTE_FROM: string  read pSEND_NOTE_FROM write pSEND_NOTE_FROM;
  published
    constructor Create;
    function Read : boolean; override;
    function Write: boolean; override;
  end;

{ ---------------------------------------------------------------- ! DAILY COMMENT ! ------------------------------------------------------------------------ }
type                                                        (* RUN IN WORKER THREAD ONLY *)
  TDaily = class(TDataTables)
  {$TYPEINFO ON}
  private
    { COLUMNS' NAMES }
    const nID          : string = 'ID';
    const nGROUP_ID    : string = 'GROUP_ID';
    const nCUID        : string = 'CUID';
    const nAGEDATE     : string = 'AGEDATE';
    const nSTAMP       : string = 'STAMP';
    const nUSER_ALIAS  : string = 'USER_ALIAS';
    const nEMAIL       : string = 'EMAIL';
    const nCALLEVENT   : string = 'CALLEVENT';
    const nCALLDURATION: string = 'CALLDURATION';
    const nFIXCOMMENT  : string = 'FIXCOMMENT';
    { PROPERTIES' VARIABLES }
    var pID            : string;
    var pGROUP_ID      : string;
    var pCUID          : string;
    var pAGEDATE       : string;
    var pSTAMP         : string;
    var pUSER_ALIAS    : string;
    var pEMAIL         : string;
    var pCALLEVENT     : string;
    var pCALLDURATION  : string;
    var pFIXCOMMENT    : string;
  public
    property ID          : string read pID           write pID;
    property GROUP_ID    : string read pGROUP_ID     write pGROUP_ID;
    property CUID        : string read pCUID         write pCUID;
    property AGEDATE     : string read pAGEDATE      write pAGEDATE;
    property STAMP       : string read pSTAMP        write pSTAMP;
    property USER_ALIAS  : string read pUSER_ALIAS   write pUSER_ALIAS;
    property EMAIL       : string read pEMAIL        write pEMAIL;
    property CALLEVENT   : string read pCALLEVENT    write pCALLEVENT;
    property CALLDURATION: string read pCALLDURATION write pCALLDURATION;
    property FIXCOMMENT  : string read pFIXCOMMENT   write pFIXCOMMENT;
  published
    constructor Create;
    function Read : boolean; override;
    function Write: boolean; override;
  end;

{ -------------------------------------------------------------- ! GENERAL COMMENT ! ------------------------------------------------------------------------ }
type                                                       (* RUN IN WORKER THREAD ONLY *)
  TGeneral = class(TDataTables)
  {$TYPEINFO ON}
  private
    { COLUMNS' NAMES }
    const nID          : string = 'ID';
    const nCUID        : string = 'CUID';
    const nSTAMP       : string = 'STAMP';
    const nUSER_ALIAS  : string = 'USER_ALIAS';
    const nFIXCOMMENT  : string = 'FIXCOMMENT';
    const nFOLLOWUP    : string = 'FOLLOWUP';
    { PROPERTIES' VARIABLES }
    var pID          : string;
    var pCUID        : string;
    var pSTAMP       : string;
    var pUSER_ALIAS  : string;
    var pFIXCOMMENT  : string;
    var pFOLLOWUP    : string;
  public
    property ID          : string read pID         write pID;
    property CUID        : string read pCUID       write pCUID;
    property STAMP       : string read pSTAMP      write pSTAMP;
    property USER_ALIAS  : string read pUSER_ALIAS write pUSER_ALIAS;
    property FIXCOMMENT  : string read pFIXCOMMENT write pFIXCOMMENT;
    property FOLLOWUP    : string read pFOLLOWUP   write pFOLLOWUP;
  published
    constructor Create;
    function Read : boolean; override;
    function Write: boolean; override;
  end;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  SQL;

{ ############################################################# ! TRACKER CLASSES ! ######################################################################### }

constructor TTracker.Create;
begin
  { DEFAULT VALUES }
  pidThd         :=0;
  pCUID          :='';
  pCOCODE        :='';
  pBRANCH        :='';
  pEMAILS        :='';
  pESTATEMENTS   :='';
  pLEGALTO       :='';
  pREMINDER1     :='';
  pREMINDER2     :='';
  pREMINDER3     :='';
  pLEGALACTION   :='';
  pSEND_NOTE_FROM:='';
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- READ FROM DATABASE }
function TTracker.Read: boolean;
var
  MSSQL: TMSSQL;
begin
  inherited;
  Result:=False;
  MSSQL:=TMSSQL.Create(Database.ADOConnect);
  try
    { PROCESS }
    if (pidThd <> 0) and (CUID <> '') and (COCODE <> '') then
    begin
      { TABLE SELECTION }
      if TableSelect = tbl_company     then MSSQL.StrSQL:='SELECT LEGALTO, REMINDER1, REMINDER2, REMINDER3, LEGALACTION, SEND_NOTE_FROM FROM tbl_company WHERE CO_CODE = ' + QuotedStr(COCODE) + ' AND BRANCH = ' + QuotedStr(Branch);
      if TableSelect = tbl_addressbook then MSSQL.StrSQL:='SELECT EMAILS, ESTATEMENTS FROM tbl_addressbook WHERE CUID = ' + QuotedStr(CUID);
      { EXECUTE }
      DataSet:=MSSQL.ExecSQL;
      if DataSet.RecordCount > 0 then Result:=True;
      if DataSet.RecordCount = 1 then
      begin
        { RETURN VALUES }
        if TableSelect = tbl_addressbook then
        begin
          EMAILS        :=VarToStr(DataSet.Fields.Item[nEMAILS        ].Value);
          ESTATEMENTS   :=VarToStr(DataSet.Fields.Item[nESTATEMENTS   ].Value);
        end;
        if TableSelect = tbl_company then
        begin
          LEGALTO       :=VarToStr(DataSet.Fields.Item[nLEGALTO       ].Value);
          REMINDER1     :=VarToStr(DataSet.Fields.Item[nREMINDER1     ].Value);
          REMINDER2     :=VarToStr(DataSet.Fields.Item[nREMINDER2     ].Value);
          REMINDER3     :=VarToStr(DataSet.Fields.Item[nREMINDER3     ].Value);
          LEGALACTION   :=VarToStr(DataSet.Fields.Item[nLEGALACTION   ].Value);
          SEND_NOTE_FROM:=VarToStr(DataSet.Fields.Item[nSEND_NOTE_FROM].Value);
        end;
        Result:=True;
      end;
    end;
  finally
    MSSQL.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- WRITE TO DATABASE }
function TTracker.Write: boolean;
begin
  inherited;
  Result:=False;

  { OPTIONAL }

end;

{ ############################################################## ! DATABASE TABLES ! ######################################################################## }

{ ################################################################ ! DAILY CLASS ! ########################################################################## }

{ -------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZATION }
constructor TDaily.Create;
begin
  { DEFAULT VALUES }
  pidThd       :=0;
  pManyLines   :=False;
  pDataSet     :=nil;
  pGROUP_ID    :=Database.ArrGroupList[MainForm.GroupListBox.ItemIndex, 0];
  pCUID        :='0';
  pSTAMP       :=DateTimeToStr(Now);
  pUSER_ALIAS  :=UpperCase(MainForm.CurrentUserName);
  pEMAIL       :='0';
  pCALLEVENT   :='0';
  pCALLDURATION:='0';
  pFIXCOMMENT  :=' ';
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- READ DAILY COMMENT }
function TDaily.Read: boolean;  (* RETURNS EITHER ONE OR MANY ROWS FOR GIVEN CUID AND AGE DATE *)
var
  MSSQL    :  TMSSQL;
  Condition:  string;
begin
  inherited;
  Result:=False;
  MSSQL :=TMSSQL.Create(Database.ADOConnect);
  try
    try
      { PROCEED IF IDTHD, CUID AND AGEDATE ARE ASSIGNED }
      if (idThd <> 0) and (CUID <> '') and (AGEDATE <> '' ) then
      begin
        { ALLOW TO QUERY MANY LINES }
        if not ManyLines then Condition:=' AND ' + nAGEDATE + ' = ' + QuotedStr(AGEDATE)
          else Condition:=';';
        { OPEN AND READ ALL COLUMNS }
        MSSQL.StrSQL:='SELECT '      +
                      nID            + ',' +
                      nGROUP_ID      + ',' +
                      nCUID          + ',' +
                      nAGEDATE       + ',' +
                      nSTAMP         + ',' +
                      nUSER_ALIAS    + ',' +
                      nEMAIL         + ',' +
                      nCALLEVENT     + ',' +
                      nCALLDURATION  + ',' +
                      nFIXCOMMENT    +
                      ' FROM tbl_daily WHERE ' + nCUID + ' = ' + QuotedStr(CUID) + Condition;
        DataSet:=MSSQL.ExecSQL;
        if DataSet.RecordCount > 0 then Result:=True;
        if DataSet.RecordCount = 1 then
        begin
          ID          :=VARTOSTR(DataSet.Fields.Item[nID          ].Value);
          GROUP_ID    :=VARTOSTR(DataSet.Fields.Item[nGROUP_ID    ].Value);
          CUID        :=VARTOSTR(DataSet.Fields.Item[nCUID        ].Value);
          AGEDATE     :=VARTOSTR(DataSet.Fields.Item[nAGEDATE     ].Value);
          STAMP       :=VARTOSTR(DataSet.Fields.Item[nSTAMP       ].Value);
          USER_ALIAS  :=VARTOSTR(DataSet.Fields.Item[nUSER_ALIAS  ].Value);
          EMAIL       :=VARTOSTR(DataSet.Fields.Item[nEMAIL       ].Value);
          CALLEVENT   :=VARTOSTR(DataSet.Fields.Item[nCALLEVENT   ].Value);
          CALLDURATION:=VARTOSTR(DataSet.Fields.Item[nCALLDURATION].Value);
          FIXCOMMENT  :=VARTOSTR(DataSet.Fields.Item[nFIXCOMMENT  ].Value);
          Result:=True;
          { DO NOT CLOSE DATASET }
        end;
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot read from "' + tbl_daily + '". Error has been thrown: ' + E.Message);
    end;
  finally
    MSSQL.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- WRITE DAILY COMMENT }
function TDaily.Write: boolean;  (* WRITES ONLY ONE ROW FOR GIVEN CUID AND AGE DATE *)
var
  MSSQL:  TMSSQL;
begin
  inherited;
  Result:=False;
  MSSQL :=TMSSQL.Create(Database.ADOConnect);
  try
    try
      { PROCEED IF IDTHD, CUID AND AGEDATE ARE ASSIGNED AND WE DO NOT HAVE QUERIED MANY LINES }
      if (idThd <> 0) and (CUID <> '') and (AGEDATE <> '' ) and (ManyLines = False) then
      begin
        MSSQL.StrSQL:='BEGIN TRANSACTION'                                                                  + #13#10 +
                      'IF EXISTS '                                                                         + #13#10 +
                      '('                                                                                  + #13#10 +
                      '  SELECT * FROM ' + tbl_daily                                                       + #13#10 +
                      '  WITH (UPDLOCK, SERIALIZABLE) '                                                    + #13#10 +
                      '  WHERE ' + nCUID    + ' = ' + QuotedStr(CUID)                                      + #13#10 +
                      '  AND '   + nAGEDATE + ' = ' + QuotedStr(AGEDATE)                                   + #13#10 +
                      ')'                                                                                  + #13#10 +
                      'BEGIN'                                                                              + #13#10 +
                      '  UPDATE '  + tbl_daily + ' SET ' + nSTAMP       + ' = ' + QuotedStr(STAMP)         + #13#10 +
                      '    WHERE ' + nCUID     + ' = ' + QuotedStr(CUID)                                   + #13#10 +
                      '    AND '   + nAGEDATE  + ' = ' + QuotedStr(AGEDATE)                                + #13#10 +
                      '  UPDATE '  + tbl_daily + ' SET ' + nUSER_ALIAS  + ' = ' + QuotedStr(USER_ALIAS)    + #13#10 +
                      '    WHERE ' + nCUID     + ' = ' + QuotedStr(CUID)                                   + #13#10 +
                      '    AND '   + nAGEDATE  + ' = ' + QuotedStr(AGEDATE)                                + #13#10 +
                      '  UPDATE '  + tbl_daily + ' SET ' + nEMAIL       + ' = ' + QuotedStr(EMAIL)         + #13#10 +
                      '    WHERE ' + nCUID     + ' = ' + QuotedStr(CUID)                                   + #13#10 +
                      '    AND '   + nAGEDATE  + ' = ' + QuotedStr(AGEDATE) + ' '                          + #13#10 +
                      '  UPDATE '  + tbl_daily + ' SET ' + nCALLEVENT    + ' = ' + QuotedStr(CALLEVENT)    + #13#10 +
                      '    WHERE ' + nCUID     + ' = ' + QuotedStr(CUID)                                   + #13#10 +
                      '    AND '   + nAGEDATE  + ' = ' + QuotedStr(AGEDATE)                                + #13#10 +
                      '  UPDATE '  + tbl_daily + ' SET ' + nCALLDURATION + ' = ' + QuotedStr(CALLDURATION) + #13#10 +
                      '    WHERE ' + nCUID     + ' = ' + QuotedStr(CUID)                                   + #13#10 +
                      '    AND '   + nAGEDATE  + ' = ' + QuotedStr(AGEDATE)                                + #13#10 +
                      '  UPDATE '  + tbl_daily + ' SET ' + nFIXCOMMENT   + ' = ' + QuotedStr(FIXCOMMENT)   + #13#10 +
                      '    WHERE ' + nCUID     + ' = ' + QuotedStr(CUID)                                   + #13#10 +
                      '    AND '   + nAGEDATE  + ' = ' + QuotedStr(AGEDATE) + ' '                          + #13#10 +
                      'END '                                                                               + #13#10 +
                      'ELSE '                                                                              + #13#10 +
                      'BEGIN '                                                                             + #13#10 +
                      'INSERT INTO ' + tbl_daily + ' '                                                     + #13#10 +
                      '(' + nGROUP_ID     + ','                                                            + #13#10 +
                            nCUID         + ','                                                            + #13#10 +
                            nAGEDATE      + ','                                                            + #13#10 +
                            nSTAMP        + ','                                                            + #13#10 +
                            nUSER_ALIAS   + ','                                                            + #13#10 +
                            nEMAIL        + ','                                                            + #13#10 +
                            nCALLEVENT    + ','                                                            + #13#10 +
                            nCALLDURATION + ','                                                            + #13#10 +
                            nFIXCOMMENT                                                                    + #13#10 +
                      ') '                                                                                 + #13#10 +
                      'VALUES '                                                                            + #13#10 +
                      '(' + QuotedStr(GROUP_ID)     + ','                                                  + #13#10 +
                            QuotedStr(CUID)         + ','                                                  + #13#10 +
                            QuotedStr(AGEDATE)      + ','                                                  + #13#10 +
                            QuotedStr(STAMP)        + ','                                                  + #13#10 +
                            QuotedStr(USER_ALIAS)   + ','                                                  + #13#10 +
                            QuotedStr(EMAIL)        + ','                                                  + #13#10 +
                            QuotedStr(CALLEVENT)    + ','                                                  + #13#10 +
                            QuotedStr(CALLDURATION) + ','                                                  + #13#10 +
                            QuotedStr(FIXCOMMENT)                                                          + #13#10 +
                      ')'                                                                                  + #13#10 +
                      'END '                                                                               + #13#10 +
                      'COMMIT TRANSACTION ';
        if not (MSSQL.ExecSQL = nil) then Result:=True;
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot write to "' + tbl_daily + '". Error has been thrown: ' + E.Message);
    end;
  finally
    MSSQL.Free;
  end;
end;

{ ############################################################### ! GENERAL CLASS ! ######################################################################### }

{ -------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZATION }
constructor TGeneral.Create;
begin
  { DEFAULT VALUES }
  pidThd     :=0;
  pCUID      :='0';
  pSTAMP     :=DateTimeToStr(Now);
  pUSER_ALIAS:=UpperCase(MainForm.CurrentUserName);
  pFIXCOMMENT:=' ';
  pFOLLOWUP  :=' ';
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- READ GENERAL COMMENT }
function TGeneral.Read: boolean;  (* RETURNS ONLY ONE ROW FOR GIVEN CUID AND AGE DATE *)
var
  MSSQL:  TMSSQL;
begin
  inherited;
  Result:=False;
  MSSQL :=TMSSQL.Create(Database.ADOConnect);
  try
    try
      { PROCEED IF IDTHD AND CUID ARE ASSIGNED }
      if (idThd <> 0) and (CUID <> '') then
      begin
        { OPEN AND READ ALL COLUMNS }
        MSSQL.StrSQL:='SELECT '      +
                      nID            + ' , ' +
                      nCUID          + ' , ' +
                      nSTAMP         + ' , ' +
                      nUSER_ALIAS    + ' , ' +
                      nFIXCOMMENT    + ' , ' +
                      nFOLLOWUP      +
                      ' FROM ' + tbl_general + ' WHERE ' + nCUID + ' = ' + QuotedStr(CUID);
        DataSet:=MSSQL.ExecSQL;
        if DataSet.RecordCount = 1 then
        begin
          ID        :=VARTOSTR(DataSet.Fields.Item[nID        ].Value);
          CUID      :=VARTOSTR(DataSet.Fields.Item[nCUID      ].Value);
          STAMP     :=VARTOSTR(DataSet.Fields.Item[nSTAMP     ].Value);
          USER_ALIAS:=VARTOSTR(DataSet.Fields.Item[nUSER_ALIAS].Value);
          FIXCOMMENT:=VARTOSTR(DataSet.Fields.Item[nFIXCOMMENT].Value);
          FOLLOWUP  :=VARTOSTR(DataSet.Fields.Item[nFOLLOWUP  ].Value);
          Result:=True;
          { DO NOT CLOSE DATASET }
        end;
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot read from "' + tbl_general + '". Error has been thrown: ' + E.Message);
    end;
  finally
    MSSQL.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- WRITE GENERAL COMMENT }
function TGeneral.Write: boolean;  (* WRITES ONLY ONE ROW FOR GIVEN CUID *)
var
  MSSQL:  TMSSQL;
begin
  inherited;
  Result:=False;
  MSSQL :=TMSSQL.Create(Database.ADOConnect);
  try
    try
      { PROCEED IF IDTHD, CUID AND AGEDATE ARE ASSIGNED }
      if (idThd <> 0) and (CUID <> '') then
      begin
        MSSQL.StrSQL:='BEGIN TRANSACTION'                                                                + #13#10 +
                      'IF EXISTS '                                                                       + #13#10 +
                      '('                                                                                + #13#10 +
                      '  SELECT * FROM ' + tbl_general                                                   + #13#10 +
                      '  WITH (UPDLOCK, SERIALIZABLE) '                                                  + #13#10 +
                      '  WHERE ' + nCUID + ' = ' + QuotedStr(CUID) + ' '                                 + #13#10 +
                      ')'                                                                                + #13#10 +
                      'BEGIN'                                                                            + #13#10 +
                      '  UPDATE '  + tbl_general + ' SET ' + nSTAMP      + ' = ' + QuotedStr(STAMP)      + #13#10 +
                      '    WHERE ' + nCUID       + ' = ' + QuotedStr(CUID)                               + #13#10 +
                      '  UPDATE '  + tbl_general + ' SET ' + nUSER_ALIAS + ' = ' + QuotedStr(USER_ALIAS) + #13#10 +
                      '    WHERE ' + nCUID       + ' = ' + QuotedStr(CUID)                               + #13#10 +
                      '  UPDATE '  + tbl_general + ' SET ' + nFIXCOMMENT + ' = ' + QuotedStr(FIXCOMMENT) + #13#10 +
                      '    WHERE ' + nCUID       + ' = ' + QuotedStr(CUID)                               + #13#10 +
                      '  UPDATE '  + tbl_general + ' SET ' + nFOLLOWUP   + ' = ' + QuotedStr(FOLLOWUP)   + #13#10 +
                      '    WHERE ' + nCUID       + ' = ' + QuotedStr(CUID)                               + #13#10 +
                      'END '                                                                             + #13#10 +
                      'ELSE '                                                                            + #13#10 +
                      'BEGIN '                                                                           + #13#10 +
                      'INSERT INTO ' + tbl_general + ' '                                                 + #13#10 +
                      '(' + nCUID       + ','                                                            + #13#10 +
                            nSTAMP      + ','                                                            + #13#10 +
                            nUSER_ALIAS + ','                                                            + #13#10 +
                            nFIXCOMMENT + ','                                                            + #13#10 +
                            nFOLLOWUP                                                                    + #13#10 +
                      ') '                                                                               + #13#10 +
                      'VALUES '                                                                          + #13#10 +
                      '(' + QuotedStr(CUID)       + ','                                                  + #13#10 +
                            QuotedStr(STAMP)      + ','                                                  + #13#10 +
                            QuotedStr(USER_ALIAS) + ','                                                  + #13#10 +
                            QuotedStr(FIXCOMMENT) + ','                                                  + #13#10 +
                            QuotedStr(FOLLOWUP)                                                          + #13#10 +
                      ') '                                                                               + #13#10 +
                      'END '                                                                             + #13#10 +
                      'COMMIT TRANSACTION';
        if not (MSSQL.ExecSQL = nil) then Result:=True;
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot read from "' + tbl_general + '". Error has been thrown: ' + E.Message);
    end;
  finally
    MSSQL.Free;
  end;
end;

end.
