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
unit Database;

interface

uses
  Main, Forms, Windows, Messages, Settings, ADODB, Classes, SysUtils, ComObj, StrUtils;

{ --------------------------------------------------------------- ! DATABASE CLASS ! ------------------------------------------------------------------------ }
type                                                  (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TDataBase = class
  {$TYPEINFO ON}
  private
    pdbProvider  : string;
    pdbConnStr   : string;
    pInterval    : integer;
    pCmdTimeout  : integer;
    pConTimeout  : integer;
  public
    property    dbProvider   : string  read pdbProvider  write pdbProvider;
    property    dbConnStr    : string  read pdbConnStr   write pdbConnStr;
    property    Interval     : integer read pInterval    write pInterval;
    property    CmdTimeout   : integer read pCmdTimeout  write pCmdTimeout;
    property    ConTimeout   : Integer read pConTimeout  write pConTimeout;
  published
    constructor Create(ShowConnStr: boolean);
    procedure   InitializeConnection(idThd: integer; ErrorShow: boolean; var ActiveConnection: TADOConnection);
    function    Check: integer;
  end;

implementation

{ ############################################################## ! DATABASE CLASS ! ######################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TDataBase.Create(ShowConnStr: boolean);
var
  AppSettings:  TSettings;
  dbConnNoPwd:  string;
begin
  AppSettings:=TSettings.Create;
  try
    if AppSettings.TMIG.ReadString(DatabaseSetup,'ACTIVE','') = 'MSSQL' then
    begin
      dbProvider :=AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPROVIDER', '');
      dbConnNoPwd:='Provider='         + dbProvider
                 + ';Data Source='     + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLSERVER',        '')
                 + ','                 + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPORT',          '')
                 + ';Initial catalog=' + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLCATALOG',       '')
                 + ';User Id='         + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLUSERNAME',      '');
      dbConnStr  :=dbConnNoPwd + ';Password=' + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPASSWORD', '');
      pInterval  :=AppSettings.TMIG.ReadInteger(TimersSettings, 'NET_CONNETCTION', 15000);
      pCmdTimeout:=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'COMMAND_TIMEOUT',    15);
      pConTimeout:=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'CONNECTION_TIMEOUT', 15);
    end;
  finally
    AppSettings.Free;
  end;
  if ShowConnStr then LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Connection string built [show_no_password] = ' + dbConnNoPwd);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- CONNECT TO DATABASE }

(* WARNING! MUST BE EXECUTED BEFORE ANY CONNECTION ATTEMPTS *)

procedure TDataBase.InitializeConnection(idThd: integer; ErrorShow: boolean; var ActiveConnection: TADOConnection);

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
  if not (ActiveConnection = nil) then ActiveConnection.Connected:=False;
  { --------------------------------------------------------------------------------------------------------------------- SETUP CONNECTION AND TRY TO CONNECT }
  try
    { CONNECTION SETTINGS }
    ActiveConnection.ConnectionString :=dbConnStr;
    ActiveConnection.Provider         :=dbProvider;
    ActiveConnection.ConnectionTimeout:=ConTimeout;
    ActiveConnection.CommandTimeout   :=CmdTimeout;
    ActiveConnection.ConnectOptions   :=coConnectUnspecified;
    ActiveConnection.KeepConnection   :=True;
    ActiveConnection.LoginPrompt      :=False;
    ActiveConnection.Mode             :=cmReadWrite;
    ActiveConnection.CursorLocation   :=clUseClient;        (* https://docs.microsoft.com/en-us/sql/ado/guide/data/the-significance-of-cursor-location *)
    ActiveConnection.IsolationLevel   :=ilCursorStability;  (* https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx                    *)
    { CONNECT TO GIVEN SERVER }
    try
      if Check = 0 then
      begin
        ActiveConnection.Connected       :=True;
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
    if not MainForm.InetTimer.Enabled then
    begin
      MainForm.InetTimer.Interval:=Interval;
      MainForm.InetTimer.Enabled:=True;
    end;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ CHECK CONNECTION WITH SERVER }
function TDataBase.Check: integer;
var
  EO:        EOleException;
  ConCheck:  TADOConnection;
begin
  { INITIALIZE }
  Result:=0;
  ConCheck:=TADOConnection.Create(nil);
  { ASSIGN PARAMETERS }
  ConCheck.ConnectionString :=dbConnStr;
  ConCheck.Provider         :=dbProvider;
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
        Result:=404;
        if E is EOLEException then
        begin
          EO:=EOleException(E);
          Result:=EO.ErrorCode;
        end;
      end;
    end;
  finally
    if ConCheck.Connected then ConCheck.Close;
  end;
end;

end.
