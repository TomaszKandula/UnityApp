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
type
  TDataBase = class                                    (* BASE CLASS FOR CONNECTION HANDLING *)
  {$TYPEINFO ON}
  public
    var DBProvider  : string;
    var DBConnStr   : string;
    var Interval    : integer;
    var CmdTimeout  : integer;
    var ConTimeout  : Integer;
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
      DBProvider :=AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPROVIDER', '');
      dbConnNoPwd:='Provider='         + DBProvider
                 + ';Data Source='     + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLSERVER',        '')
                 + ','                 + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPORT',          '')
                 + ';Initial catalog=' + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLCATALOG',       '')
                 + ';User Id='         + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLUSERNAME',      '');
      DBConnStr :=dbConnNoPwd + ';Password=' + AppSettings.TMIG.ReadString(DatabaseSetup, 'MSSQLPASSWORD', '');
      Interval  :=AppSettings.TMIG.ReadInteger(TimersSettings, 'NET_CONNETCTION', 5000);
      CmdTimeout:=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'COMMAND_TIMEOUT',    15);
      ConTimeout:=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'CONNECTION_TIMEOUT', 15);
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
    ActiveConnection.ConnectionString :=DBConnStr;
    ActiveConnection.Provider         :=DBProvider;
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
      ActiveConnection.Connected:=True;
    except
      on E: Exception do
        ErrorHandler(E.ClassName, E.Message, False, ErrorShow);
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
  StrSQL:    string;
begin
  { INITIALIZE }
  Result:=0;
  StrSQL:='SELECT 1';
  ConCheck:=TADOConnection.Create(nil);
  { ASSIGN PARAMETERS }
  ConCheck.ConnectionString :=DBConnStr;
  ConCheck.Provider         :=DBProvider;
  ConCheck.ConnectionTimeout:=ConTimeout;
  ConCheck.CommandTimeout   :=CmdTimeout;
  ConCheck.KeepConnection   :=False;
  ConCheck.LoginPrompt      :=False;
  ConCheck.Mode             :=cmRead;
  { TRY TO CONNECT }
  try
    try
      ConCheck.Connected:=True;
      ConCheck.Execute(StrSQL, cmdText);
    except
      on E: Exception do
      begin
        Result:=100;
        if E is EOLEException then
        begin
          EO:=EOleException(E);
          Result:=EO.ErrorCode;
        end;
      end;
    end;
  finally
    if ConCheck.Connected then
    begin
      MainForm.ExecMessage(False, conOK, strNULL);
      ConCheck.Close;
    end
    else
    begin
      MainForm.ExecMessage(False, conERROR, strNULL);
    end;
    ConCheck.Free;
  end;
end;

end.
