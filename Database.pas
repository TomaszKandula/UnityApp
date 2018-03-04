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
unit Database;

interface

uses
  Main;

{ --------------------------------------------------------------- ! DATABASE CLASS ! ------------------------------------------------------------------------ }
type                                                  (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TDataBase = class
  {$TYPEINFO ON}
  private
    pdbProvider  : string;   { DATABASE PROVIDER                            }
    pdbConnStr   : string;   { DATABASE CONNECTION STRING                   }
    pLastError   : integer;  { LAST OCCURED ERROR                           }
    pAccessLevel : string;   { RO = READ ONLY | RW = READ/WRITE | AD = ALL  }
    pAccessMode  : string;   { BASIC = HIDE SPECIFIC DATA | FULL = SHOW ALL }
  public
    var         ADOConnect   : TADOConnection;
    var         ArrGroupList : array of array of string;
    property    dbProvider   : string  read pdbProvider  write pdbProvider;
    property    dbConnStr    : string  read pdbConnStr   write pdbConnStr;
    property    LastError    : integer read pLastError   write pLastError;
    property    AccessLevel  : string  read pAccessLevel write pAccessLevel;
    property    AccessMode   : string  read pAccessMode  write pAccessMode;
  published
    constructor Create;
    procedure   InitializeConnection(idThd: integer; ErrorShow: boolean);
    function    AssignConnString(Print: boolean): boolean;
    function    Check: boolean;
    function    UACinitialize: boolean;                                              { RUN IN MAIN THREAD ONLY }
    procedure   UACGroupReader(UACuser: string);                                     { RUN EITHER IN WORKER OR MAIN THREAD }
    procedure   UACAgeDates(StrGroupID: string);                                     { RUN EITHER IN WORKER OR MAIN THREAD }
    function    UACString(ListPos: integer; CoPos: integer; Mode: integer): string;  { RUN EITHER IN WORKER OR MAIN THREAD }
  end;

implementation

{ ############################################################## ! DATABASE CLASS ! ######################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TDataBase.Create;
begin
  (* INITIALIZE PRIVATE VARIABES *)
  ADOConnect  :=TADOConnection.Create(nil);
  pdbProvider :='';
  pdbConnStr  :='';
  pAccessLevel:='';
  pAccessMode :='';
  pLastError  :=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- CONNECT TO DATABASE }
(* WARNING! MUST BE EXECUTED BEFORE ANY CONNECTION ATTEMPTS *)
procedure TDataBase.InitializeConnection(idThd: integer; ErrorShow: boolean);

{ ------------------------------------------------------ ! COMMON VARIABLES AND CONSTANTS ! ----------------------------------------------------------------- }
const
  ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
  ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';
var
  AppSettings:  TSettings;
{ -------------------------------------------------------------- ! INNER BLOCK ! ---------------------------------------------------------------------------- }
procedure ErrorHandler(err_class: string; err_msg: string; should_quit: boolean; err_wnd: boolean);
begin
  LogText(MainForm.EventLogPath, ERR_LOGTEXT + '[' + err_class + '] ' + err_msg + ' (' + IntToStr(ExitCode) + ').');
  if err_wnd     then Application.MessageBox(PChar(ERR_MESSAGE), PChar(MainForm.CAPTION), MB_OK + MB_ICONWARNING);
  if should_quit then Application.Terminate;
end;

{ --------------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------- }
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  ADOConnect.Connected:=False;
  DataBase.AssignConnString(ErrorShow);
  AppSettings:=TSettings.Create;
  { --------------------------------------------------------------------------------------------------------------------- SETUP CONNECTION AND TRY TO CONNECT }
  try
    { SETUP CONNESTION STRING AND PROVIDER }
    ADOConnect.ConnectionString :=DataBase.dbConnStr;
    ADOConnect.Provider         :=DataBase.dbProvider;
    { SETTINGS }
    ADOConnect.ConnectionTimeout:=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'CONNECTION_TIMEOUT', 15);
    ADOCOnnect.CommandTimeout   :=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'COMMAND_TIMEOUT',    15);
    ADOConnect.ConnectOptions   :=coConnectUnspecified;
    ADOConnect.KeepConnection   :=True;
    ADOConnect.LoginPrompt      :=False;
    ADOConnect.Mode             :=cmReadWrite;
    { CURSOR SETTINGS }
    ADOConnect.CursorLocation   :=clUseClient;        (* https://docs.microsoft.com/en-us/sql/ado/guide/data/the-significance-of-cursor-location *)
    ADOConnect.IsolationLevel   :=ilCursorStability;  (* https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx                    *)
    { CONNECT TO GIVEN SERVER }
    try
      if (DataBase.Check) then
      begin
        ADOConnect.Connected:=True;
        MainForm.InvoiceScanTimer.Enabled:=True;
        MainForm.OILoader.Enabled        :=True;
        SendMessage(MainForm.Handle, WM_GETINFO, 14, LPARAM(PCHAR('NULL')));
      end else
      begin
        MainForm.InvoiceScanTimer.Enabled:=False;
        MainForm.OILoader.Enabled        :=False;
        SendMessage(MainForm.Handle, WM_GETINFO, 15, LPARAM(PCHAR('NULL')));
      end;
    except
      on E: Exception do ErrorHandler(E.ClassName, E.Message, False, ErrorShow);
    end;
  finally
    { CHECK SERVER CONNECTION ON REGULAR BASIS }
    if not (MainForm.InetTimer.Enabled) then
    begin
      MainForm.InetTimer.Interval:=AppSettings.TMIG.ReadInteger(TimersSettings, 'NET_CONNETCTION', 15000);
      MainForm.InetTimer.Enabled:=True;
    end;
    AppSettings.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------- READ CONNECTION STRING FROM SETTINGS FILE }
function TDataBase.AssignConnString(Print: boolean): boolean;
var
  AppSettings:  TSettings;
  dbConnNoPwd:  string;
begin
  Result:=True;
  AppSettings:=TSettings.Create;
  try
    try
      if AppSettings.TMIG.ReadString(DatabaseSetup,'ACTIVE','') = 'MSSQL' then
      begin
        DataBase.dbProvider:=AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPROVIDER', '');
        dbConnNoPwd:='Provider='         + DataBase.dbProvider
                   + ';Data Source='     + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLSERVER',        '')
                   + ','                 + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPORT',          '')
                   + ';Initial catalog=' + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLCATALOG',       '')
                   + ';User Id='         + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLUSERNAME',      '');
        DataBase.dbConnStr:=dbConnNoPwd + ';Password=' + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPASSWORD', '');
      end;
    except
      Result:=False;
    end;
  finally
    if (Print) then LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Connection string built [show_no_password] = ' + dbConnNoPwd);
    AppSettings.Free
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ CHECK CONNECTION WITH SERVER }
function TDataBase.Check: boolean;
var
  EO:        EOleException;
  ConCheck:  TADOConnection;
begin
  { INITIALIZE }
  Result:=True;
  LastError:=0;
  ConCheck:=TADOConnection.Create(nil);
  { ASSIGN PARAMETERS }
  ConCheck.ConnectionString :=DataBase.dbConnStr;
  ConCheck.Provider         :=DataBase.dbProvider;
  ConCheck.ConnectionTimeout:=2;
  ConCheck.CommandTimeout   :=2;
  ConCheck.KeepConnection   :=False;
  ConCheck.LoginPrompt      :=False;
  ConCheck.Mode             :=cmRead;
  { TRY TO CONNECT }
  try
    try
      ConCheck.Connected:=True;
      ConCheck.Open;
    except
      on E: Exception do
      begin
        Result:=False;
        if E is EOLEException then
        begin
          EO:=EOleException(E);
          DataBase.LastError:=EO.ErrorCode;
        end;
      end;
    end;
  finally
    if ConCheck.Connected then ConCheck.Close;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------- USER ACCOUNT CONTROL | INITIALIZE }
(* NOTE: DO NOT USE THIS METHOD BEFORE DATABASE CONNECTION IS ESTABLISHED *)
function TDatabase.UACinitialize: boolean;
var
  Query:       TADOQuery;
  StrSQL:      string;
  iCNT:        integer;
  jCNT:        integer;
begin
  Result:=False;
  { ------------------------------------------------------------------------------------------------------------------------ READ ACCESS LEVEL FOR GIVEN USER }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    StrSQL:='SELECT ACCESS_LEVEL, ACCESS_MODE FROM tbl_UAC WHERE USERNAME = ' + QuotedStr(UpperCase(MainForm.CurrentUserName));
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Open;
    { TABLE 'TBL_UAC' SHOULD ALWAYS HOLDS DISTINC USERNAMES }
    { THEREFORE WE HAVE ONLY ONE ROW PER USER               }
    if Query.RecordCount = 1 then
    begin
      Database.AccessLevel:=Query.Recordset.Fields[0].Value;
      Database.AccessMode :=Query.Recordset.Fields[1].Value;
      Result:=True;
    end;
    { IF USER IS NOT FOUND }
    if Query.RecordCount = 0 then
    begin
      SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PCHAR('Cannot find user name: ' + UpperCase(MainForm.CurrentUserName) + '. ' + APPNAME + ' will be closed. Please contact IT support.')));
      LogText(MainForm.EventLogPath, '[UAC READER]: Cannot find user name: ' + UpperCase(MainForm.CurrentUserName) + '. Application terminated.');
      Application.Terminate;
    end;
    { ACCESS LEVEL AND MODE TO EVENT LOG }
    if Database.AccessLevel = 'RO' then LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Level = read only given group(s).');
    if Database.AccessLevel = 'RW' then LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Level = read and write to given group(s).');
    if Database.AccessLevel = 'AD' then
    begin
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Level = access all areas.');
    end;
    if Database.AccessMode <> ''   then LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Mode = ' + LowerCase(Database.AccessMode) + '.');
  finally
    Query.Close;
    Query.Free;
  end;
  { ----------------------------------------------------------------------------------------------------------------------------- LOOP THROUGH THE COMPONENTS }
  for iCNT:=0 to MainForm.ComponentCount - 1 do
    if MainForm.Components[iCNT] is TEdit then
    begin
      { IF ACCESS LEVEL IS 'ADMINISTRATOR' THEN WE ENABLE EDIT COMPONENTS }
      if Database.AccessLevel = 'AD' then
      begin
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'COC' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'CUR' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'INT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'AGT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
      end else
      begin
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'COC' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'CUR' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'INT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'AGT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
      end;
    end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- UAC | READ GROUPS }
procedure TDatabase.UACGroupReader(UACuser: string);
{ READ ALL GROUP ID AND GROUP NAME INTO AN ARRAY, GROUP LIS BOX IS POPULATED FROM SUCH ARRAY ONLY WITH GROUP NAME }
{ CORRESPONDING GROUP ID FOR SELECTED GROUP NAME BY THE USER IS USED TO UPLOAD ALL DISTINCT AGE DATES AND FINALY  }
{ USED TO UPLOAD PROPER DATA SNAPSHOT RELATED TO GIVEN GROUP NAME.                                                }
{ THIS METHOD IS NOT CALLED IF USER IS NOT FOUND IN DATABASE.                                                     }
var
  StrSQL:       string;
  Query:        TADOQuery;
  iCNT:         integer;
begin
  { INITIALIZE }
  MainForm.GroupListBox.Enabled:=False;
  iCNT:=0;
  { BUILD SQL QUERY }
  StrSQL:='SELECT GROUP_ID, GROUP_NAME FROM tbl_groups WHERE FID = (SELECT ID FROM tbl_UAC WHERE USERNAME = ' + QuotedStr(UACuser) + ')';
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    { EXECUTE WITH GIVEN SQL AND DATABASE }
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    try
      Query.Open;
      SetLength(Database.ArrGroupList, 2, 2);
      if (Query.RecordCount > 0) then
      begin
        { MOVE TO AN ARRAY }
        Query.Recordset.MoveFirst;
        while not Query.Recordset.EOF do
        begin
          { READ FROM AND WRITE TO }
          Database.ArrGroupList[iCNT, 0]:=Query.Recordset.Fields['GROUP_ID'  ].Value;
          Database.ArrGroupList[iCNT, 1]:=Query.Recordset.Fields['GROUP_NAME'].Value;
          { MOVE COUNTERS }
          inc(iCNT);
          Query.Recordset.MoveNext;
          { EXPAND ARRAY BY ONE ROW }
          SetLength(Database.ArrGroupList, iCNT + 1, 2);
        end;
        { POPULATE LIST BOX }
        MainForm.GroupListBox.Clear;
        for iCNT:=0 to high(Database.ArrGroupList) - 1 do MainForm.GroupListBox.Items.Add(Database.ArrGroupList[iCNT, 1]);
        MainForm.GroupListBox.ItemIndex:=0;
        MainForm.GroupListBox.Enabled:=True;
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, '[UAC READER]: Query error. Exception thrown: ' + E.Message);
    end;
  finally
    { RELEASE FROM MEMORY }
    Query.Close;
    Query.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------- UAC | READ ALL DATES FOR GIVEN ID }
procedure TDatabase.UACAgeDates(StrGroupID: string);
var
  StrSQL:      string;
  Query:       TADOQuery;
begin
  { INITIALIZE }
  MainForm.GroupListDates.Clear;
  MainForm.GroupListDates.Enabled:=False;
  { BUILD SQL QUERY }
  StrSQL:='SELECT DISTINCT AGE_DATE FROM tbl_snapshots WHERE GROUP_ID = ' + QuotedStr(StrGroupID);
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    { EXECUTE WITH GIVEN SQL AND DATABASE }
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    try
      Query.Open;
      if (Query.RecordCount > 0) then
      begin
        Query.Recordset.MoveFirst;
        while not Query.Recordset.EOF do
        begin
          MainForm.GroupListDates.Items.Add(Query.Recordset.Fields['AGE_DATE'].Value);
          Query.Recordset.MoveNext;
        end;
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, '[UAC READER]: Query error. Exception thrown: ' + E.Message);
    end;
  finally
    MainForm.GroupListDates.ItemIndex:=MainForm.GroupListDates.Items.Count - 1;
    if Database.AccessLevel = 'AD' then MainForm.GroupListDates.Enabled:=True;
    { RELEASE FROM MEMORY }
    Query.Close;
    Query.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------- UAC | RETURN SPECIFIC 'COCOE' FROM THE LIST }
function TDatabase.UACString(ListPos: integer; CoPos: integer; Mode: integer): string;
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
  if ( ListPos > High(Database.ArrGroupList) ) or (CoPos > 4) then Exit;
  { EXTRACT | 'COCODE' FROM GROUP ID }
  if Mode = 0 then
  begin
    if CoPos = 1 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 1,  5)));
    if CoPos = 2 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 6,  5)));
    if CoPos = 3 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 11, 5)));
    if CoPos = 4 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 16, 5)));
  end;
  { EXTRACT | GROUP NAME }
  if Mode = 1 then Result:=Database.ArrGroupList[ListPos, Mode];
  { EXTRACT | FULL GROUP ID }
  if Mode = 2 then Result:=Database.ArrGroupList[ListPos, 0];
end;

end.
