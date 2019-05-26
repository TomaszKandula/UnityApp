unit DbHandler;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.Classes,
    System.SysUtils,
    System.StrUtils,
    System.Win.ComObj,
    Vcl.Forms,
    Data.Win.ADODB,
    Settings;


    // legacy code - to be removed after REST is implemented


type

    /// <summary>
    /// Base class for handling SQL Server database connection.
    /// </summary>

    TDataBase = class
    {$TYPEINFO ON}
    strict private
        var ODBC_Driver:     string;
        var OLEDB_Provider:  string;
        var OLEDB_PSI:       string;
        var Common_MARS:     string;
        var Common_TSC:      string;
        var Common_Encrypt:  string;
        var Common_Server:   string;
        var Common_Database: string;
        var Common_UserName: string;
        var Common_Password: string;
        var Interval:        integer;
    private
        var DBConnStr:  string;
        var CmdTimeout: integer;
        var ConTimeout: integer;
    published
        constructor Create(ShowConnStr: boolean);
        procedure   InitializeConnection(idThd: integer; ErrorShow: boolean; var ActiveConnection: TADOConnection);
        function    Check: integer;
    end;


implementation


uses
    Main,
    Helpers;


const
    ConStrOLEDB = 'Provider={OLEDB_Provider};'              +
                  'Data Source={Common_Server};'            +
                  'Initial catalog={Common_Database};'      +
                  'Persist Security Info={OLEDB_PSI};'      +
                  'MultipleActiveResultSets={Common_MARS};' +
                  'Encrypt={Common_Encrypt};'               +
                  'TrustServerCertificate={Common_TSC};'    +
                  'User Id={Common_UserName};'              +
                  'Password={Common_Password};';
    ConStrODBC  = 'Driver={ODBC_Driver};'                   +
                  'Server={Common_Server};'                 +
                  'Database={Common_Database};'             +
                  'MARS_Connection={Common_MARS};'          +
                  'Encrypt={Common_Encrypt};'               +
                  'TrustServerCertificate={Common_TSC};'    +
                  'Uid={Common_UserName};'                  +
                  'Pwd={Common_Password};';


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TDataBase.Create(ShowConnStr: boolean);
var
    Settings:    ISettings;
    dbConnNoPwd: string;
    WhichActive: string;
begin

    // Get all data
    Settings:=TSettings.Create;
    WhichActive    :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'ACTIVE'            , '');
    ODBC_Driver    :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'ODBC_Driver'       , '');
    OLEDB_Provider :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'OLEDB_Provider'    , '');
    OLEDB_PSI      :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'OLEDB_PSI'         , '');
    Common_MARS    :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_MARS'       , '');
    Common_TSC     :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_TSC'        , '');
    Common_Encrypt :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Encrypt'    , '');
    Common_Server  :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Server'     , '');
    Common_Database:=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Database'   , '');
    Common_UserName:=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_UserName'   , '');
    Common_Password:=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Password'   , '');
    CmdTimeout     :=Settings.GetIntegerValue(TConfigSections.DatabaseSetup, 'SERVER_CMD_TIMEOUT', 15);
    ConTimeout     :=Settings.GetIntegerValue(TConfigSections.DatabaseSetup, 'SERVER_CON_TIMEOUT', 15);
    Interval       :=Settings.GetIntegerValue(TConfigSections.TimersSettings,'NET_CONNETCTION'   , 5000);

    // Set template string
    if WhichActive = TADODB.dbODBC  then dbConnNoPwd:=ConStrODBC;
    if WhichActive = TADODB.dbOLEDB then dbConnNoPwd:=ConStrOLEDB;

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
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Connection string built [show_no_password] = ' + dbConnNoPwd);

end;


// ------------------------------------------------------------------------------------------------------------------------------------- CONNECT TO DATABASE //


/// <summary>
/// Initialize connection with database for given TADOConnection.
/// </summary>
/// <remarks>
/// This method must be executed before any connection attempts.
/// </remarks>
/// <param name="idThd">Integer. Current thread id of running process.</param>
/// <param name="ErrorShow">Boolean. Set to true if you want to display error message on connection failure.</param>
/// <param name="ActiveConnection">TADOConnection. Must be established a'priori and passed as a reference.</param>

procedure TDataBase.InitializeConnection(idThd: integer; ErrorShow: boolean; var ActiveConnection: TADOConnection);

    // --------------------------------------------------------------------------------------------------------------------------------------- NESTED METHOD //

    procedure ErrorHandler(err_class: string; err_msg: string; should_quit: boolean; err_wnd: boolean);
    begin

        MainForm.LogText.Log(MainForm.EventLogPath, TADODB.ERR_LOGTEXT + '[' + err_class + '] ' + err_msg + ' (' + IntToStr(ExitCode) + ').');

        if err_wnd then
            Application.MessageBox(PChar(TADODB.ERR_MESSAGE), PChar(MainForm.CAPTION), MB_OK + MB_ICONWARNING);

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
        /// The connection is formed synchronously.
        /// </remarks>

        ActiveConnection.ConnectOptions:=coConnectUnspecified;

        ActiveConnection.KeepConnection:=True;
        ActiveConnection.LoginPrompt   :=False;
        ActiveConnection.Mode          :=cmReadWrite;

        /// <seealso cref="https://docs.microsoft.com/en-us/sql/ado/guide/data/the-significance-of-cursor-location"/>

        ActiveConnection.CursorLocation:=clUseClient;

        /// <seealso cref="https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx"/>

        ActiveConnection.IsolationLevel:=ilCursorStability;

        // Connect to given server
        try
            ActiveConnection.Connected:=True;
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Server connection has been established successfully.');
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
/// Check if can connect and query against given database.
/// </summary>
/// <returns>Integer. Error code.</returns>

function TDataBase.Check: integer;
var
    EO:       EOleException;
    ConCheck: TADOConnection;
    StrSQL:   string;
begin

    Result:=0;

    /// <remarks>
    /// It is not enough to establish connection, we must check if query can be executed. Therefore, we use simplist query
    /// to check if we can connect to the database and send/receive data.
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
            MainForm.ExecMessage(False, TMessaging.conOK, TNaVariants.NULL);
            ConCheck.Close;
        end
        else
        begin
            MainForm.ExecMessage(False, TMessaging.conERROR, TNaVariants.NULL);
        end;

        ConCheck.Free;
    end;

end;


end.

