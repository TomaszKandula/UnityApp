
{$I .\Include\Header.inc}

unit Database;

interface

uses
    Main, Forms, Windows, Messages, Settings, ADODB, Classes, SysUtils, ComObj, StrUtils;

type

    /// <summary>
    ///     Base class for handling SQL Server database connection.
    /// </summary>

    TDataBase = class
        {$TYPEINFO ON}
        strict private
            var ODBC_Driver    : string;
            var OLEDB_Provider : string;
            var OLEDB_PSI      : string;
            var Common_MARS    : string;
            var Common_TSC     : string;
            var Common_Encrypt : string;
            var Common_Server  : string;
            var Common_Database: string;
            var Common_UserName: string;
            var Common_Password: string;
            var Interval       : integer;
        private
            var DBConnStr      : string;
            var CmdTimeout     : integer;
            var ConTimeout     : Integer;
        published
            /// <param name="ShowConnStr">
            ///     Boolean. Set to true if you want to display connection string in the event log.
            /// </param>
            constructor Create(ShowConnStr: boolean);
            procedure   InitializeConnection(idThd: integer; ErrorShow: boolean; var ActiveConnection: TADOConnection);
            function    Check: integer;
    end;

implementation


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TDataBase.Create(ShowConnStr: boolean);
var
    AppSettings: TSettings;
    dbConnNoPwd: string;
    WhichActive: string;
begin

    // Get all data
    AppSettings:=TSettings.Create;
    try
        WhichActive    :=AppSettings.TMIG.ReadString(DatabaseSetup,  'ACTIVE'         , '');
        ODBC_Driver    :=AppSettings.TMIG.ReadString(DatabaseSetup,  'ODBC_Driver'    , '');
        OLEDB_Provider :=AppSettings.TMIG.ReadString(DatabaseSetup,  'OLEDB_Provider' , '');
        OLEDB_PSI      :=AppSettings.TMIG.ReadString(DatabaseSetup,  'OLEDB_PSI'      , '');
        Common_MARS    :=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_MARS'    , '');
        Common_TSC     :=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_TSC'     , '');
        Common_Encrypt :=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_Encrypt' , '');
        Common_Server  :=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_Server'  , '');
        Common_Database:=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_Database', '');
        Common_UserName:=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_UserName', '');
        Common_Password:=AppSettings.TMIG.ReadString(DatabaseSetup,  'COMMON_Password', '');
        CmdTimeout     :=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'SERVER_CMD_TIMEOUT', 15);
        ConTimeout     :=AppSettings.TMIG.ReadInteger(DatabaseSetup, 'SERVER_CON_TIMEOUT', 15);
        Interval       :=AppSettings.TMIG.ReadInteger(TimersSettings,'NET_CONNETCTION'   , 5000);
    finally
        AppSettings.Free;
    end;

    // Set template string
    if WhichActive = dbODBC  then dbConnNoPwd:=ConStrODBC;
    if WhichActive = dbOLEDB then dbConnNoPwd:=ConStrOLEDB;

    // Replace tags for retrieved data (it ignores tags that are missing)
    // Passwordless connection string
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{ODBC_Driver}',      ODBC_Driver);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{OLEDB_Provider}',   OLEDB_Provider);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{OLEDB_PSI}',        OLEDB_PSI);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{Common_Server}',    Common_Server);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{Common_Database}',  Common_Database);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{Common_MARS}',      Common_MARS);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{Common_Encrypt}',   Common_Encrypt);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{Common_TSC}',       Common_TSC);
    dbConnNoPwd:=ReplaceStr(dbConnNoPwd, '{Common_UserName}',  Common_UserName);

    // Connection string with password
    DBConnStr:=ReplaceStr(dbConnNoPwd, '{Common_Password}', Common_Password);

    // Show connection string in event log
    if (ShowConnStr) and (DBConnStr <> '') then
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Connection string built [show_no_password] = ' + dbConnNoPwd);

end;


// ------------------------------------------------------------------------------------------------------------------------------------- CONNECT TO DATABASE //


/// <summary>
///     Initialize connection with database for given TADOConnection.
/// </summary>
/// <remarks>
///     This method must be executed before any connection attempts.
/// </remarks>
/// <param name="idThd">Integer. Current thread id of running process.</param>
/// <param name="ErrorShow">Boolean. Set to true if you want to display error message on connection failure.</param>
/// <param name="ActiveConnection">TADOConnection. Must be established a'priori and passed as a reference.</param>

procedure TDataBase.InitializeConnection(idThd: integer; ErrorShow: boolean; var ActiveConnection: TADOConnection);

    // --------------------------------------------------------------------------------------------------------------------------------------- NESTED METHOD //

    procedure ErrorHandler(err_class: string; err_msg: string; should_quit: boolean; err_wnd: boolean);
    begin

        LogText(MainForm.EventLogPath, ERR_LOGTEXT + '[' + err_class + '] ' + err_msg + ' (' + IntToStr(ExitCode) + ').');

        if err_wnd then
            Application.MessageBox(PChar(ERR_MESSAGE), PChar(MainForm.CAPTION), MB_OK + MB_ICONWARNING);

        if should_quit then
            Application.Terminate;

    end;

// ---------------------------------------------------------------------------------------------------------------------------------------------- MAIN BLOCK //

begin

    if not (ActiveConnection = nil)
        then ActiveConnection.Connected:=False;

    try

        // Connection settings
        ActiveConnection.ConnectionString :=DBConnStr;
        ActiveConnection.ConnectionTimeout:=ConTimeout;
        ActiveConnection.CommandTimeout   :=CmdTimeout;

        /// <remarks>
        ///     The connection is formed synchronously.
        /// </remarks>

        ActiveConnection.ConnectOptions   :=coConnectUnspecified;

        ActiveConnection.KeepConnection   :=True;
        ActiveConnection.LoginPrompt      :=False;
        ActiveConnection.Mode             :=cmReadWrite;

        /// <seealso cref="https://docs.microsoft.com/en-us/sql/ado/guide/data/the-significance-of-cursor-location"/>

        ActiveConnection.CursorLocation   :=clUseClient;

        /// <seealso cref="https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx"/>

        ActiveConnection.IsolationLevel   :=ilCursorStability;

        // Connect to given server
        try
            ActiveConnection.Connected:=True;
            LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Server connection has been established successfully.');
        except
            on E: Exception do
                ErrorHandler(E.ClassName, E.Message, False, ErrorShow);
        end;

    finally

        // Check server connection on regular basis
        if not MainForm.InetTimer.Enabled then
        begin
            MainForm.InetTimer.Interval:=Interval;
            MainForm.InetTimer.Enabled:=True;
        end;

    end;

end;

// ---------------------------------------------------------------------------------------------------------------------------------- CONNECTION WITH SERVER //

/// <summary>
///     Check if can connect and query against given database.
/// </summary>
/// <returns>Integer. Error code.</returns>

function TDataBase.Check: integer;
var
    EO:        EOleException;
    ConCheck:  TADOConnection;
    StrSQL:    string;
begin

    Result:=0;

    /// <remarks>
    ///     It is not enough to establish connection, we must check if query can be executed. Therefore, we use simplist query
    ///     to check if we can connect to the database and send/receive data.
    /// </remarks>

    StrSQL:='SELECT 1';
    ConCheck:=TADOConnection.Create(nil);

    // Assign parameters
    ConCheck.ConnectionString :=DBConnStr;
    ConCheck.ConnectionTimeout:=ConTimeout;
    ConCheck.CommandTimeout   :=CmdTimeout;
    ConCheck.KeepConnection   :=False;
    ConCheck.LoginPrompt      :=False;
    ConCheck.Mode             :=cmRead;

    // Try to connect
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
