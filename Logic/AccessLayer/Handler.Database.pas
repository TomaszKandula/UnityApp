unit Handler.Database;

// ----------------------------------------
// Application logic, access layers.
// Can be referenced by anyone.
// Cannot hold references to View.
// ----------------------------------------

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
    Unity.Settings;


    // legacy code - to be removed after REST is implemented


type

    /// <summary>
    /// Base class for handling SQL Server database connection.
    /// </summary>

    TDataBase = class
    {$TYPEINFO ON}
    strict private
        var FODBC_Driver:     string;
        var FOLEDB_Provider:  string;
        var FOLEDB_PSI:       string;
        var FCommon_MARS:     string;
        var FCommon_TSC:      string;
        var FCommon_Encrypt:  string;
        var FCommon_Server:   string;
        var FCommon_Database: string;
        var FCommon_UserName: string;
        var FCommon_Password: string;
        var FInterval:        cardinal;
    private
        var FDbConnStr:  string;
        var FCmdTimeout: integer;
        var FConTimeout: integer;
    published
        constructor Create(ShowConnStr: boolean);
        property Interval: cardinal read FInterval;
        function InitializeConnection(ErrorShow: boolean; var ActiveConnection: TADOConnection): boolean;
        function Check: integer;
    end;


implementation


uses
    Unity.Unknown,
    Unity.Messaging,
    Unity.Helpers,
    Unity.AdoDb,
    View.Main;


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
begin

    var DbConnNoPwd: string;
    var WhichActive: string;

    // Get all data
    var Settings: ISettings:=TSettings.Create;
    WhichActive     :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'ACTIVE'            , '');
    FODBC_Driver    :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'ODBC_Driver'       , '');
    FOLEDB_Provider :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'OLEDB_Provider'    , '');
    FOLEDB_PSI      :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'OLEDB_PSI'         , '');
    FCommon_MARS    :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_MARS'       , '');
    FCommon_TSC     :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_TSC'        , '');
    FCommon_Encrypt :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Encrypt'    , '');
    FCommon_Server  :=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Server'     , '');
    FCommon_Database:=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Database'   , '');
    FCommon_UserName:=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_UserName'   , '');
    FCommon_Password:=Settings.GetStringValue(TConfigSections.DatabaseSetup,  'COMMON_Password'   , '');
    FCmdTimeout     :=Settings.GetIntegerValue(TConfigSections.DatabaseSetup, 'SERVER_CMD_TIMEOUT', 15);
    FConTimeout     :=Settings.GetIntegerValue(TConfigSections.DatabaseSetup, 'SERVER_CON_TIMEOUT', 15);
    FInterval       :=Settings.GetIntegerValue(TConfigSections.TimersSettings,'NET_CONNETCTION'   , 5000);

    // Set template string
    if WhichActive = TADODB.dbODBC  then DbConnNoPwd:=ConStrODBC;
    if WhichActive = TADODB.dbOLEDB then DbConnNoPwd:=ConStrOLEDB;

    // Replace tags for retrieved data (it ignores tags that are missing)
    // Passwordless connection string
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{ODBC_Driver}',      FODBC_Driver);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{OLEDB_Provider}',   FOLEDB_Provider);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{OLEDB_PSI}',        FOLEDB_PSI);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{Common_Server}',    FCommon_Server);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{Common_Database}',  FCommon_Database);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{Common_MARS}',      FCommon_MARS);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{Common_Encrypt}',   FCommon_Encrypt);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{Common_TSC}',       FCommon_TSC);
    DbConnNoPwd:=ReplaceStr(DbConnNoPwd, '{Common_UserName}',  FCommon_UserName);

    // Connection string with password
    FDbConnStr:=ReplaceStr(DbConnNoPwd, '{Common_Password}', FCommon_Password);

    // Show connection string in event log
    if (ShowConnStr) and (FDbConnStr <> '') then
        //MainForm.FAppEvents.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Connection string built [show_no_password] = ' + DbConnNoPwd);

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

function TDataBase.InitializeConnection(ErrorShow: boolean; var ActiveConnection: TADOConnection): boolean;

    procedure ErrorHandler(err_class: string; err_msg: string; should_quit: boolean; err_wnd: boolean);
    begin

        MainForm.FAppEvents.Log(MainForm.EventLogPath, TADODB.ERR_LOGTEXT + '[' + err_class + '] ' + err_msg + ' (' + IntToStr(ExitCode) + ').');

        if err_wnd then
            Application.MessageBox(PChar(TADODB.ERR_MESSAGE), PChar(MainForm.CAPTION), MB_OK + MB_ICONWARNING);

        if should_quit then
            Application.Terminate;

    end;

begin

    Result:=False;
    if not (ActiveConnection = nil) then ActiveConnection.Connected:=False;

    try

        // Connection settings
        ActiveConnection.ConnectionString :=FDbConnStr;
        ActiveConnection.ConnectionTimeout:=FConTimeout;
        ActiveConnection.CommandTimeout   :=FCmdTimeout;

        // The connection is formed synchronously.
        ActiveConnection.ConnectOptions:=coConnectUnspecified;

        ActiveConnection.KeepConnection:=True;
        ActiveConnection.LoginPrompt   :=False;
        ActiveConnection.Mode          :=cmReadWrite;

        /// <seealso cref="https://docs.microsoft.com/en-us/sql/ado/guide/data/the-significance-of-cursor-location"/>
        ActiveConnection.CursorLocation:=clUseClient;

        /// <seealso cref="https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx"/>
        ActiveConnection.IsolationLevel:=ilCursorStability;

        ActiveConnection.Connected:=True;
        Result:=True;

    except
        on E: Exception do
            ErrorHandler(E.ClassName, E.Message, False, ErrorShow);
    end;

end;

// ---------------------------------------------------------------------------------------------------------------------------------- CONNECTION WITH SERVER //

/// <summary>
/// Check if can connect and query against given database.
/// </summary>
/// <returns>Integer. Error code.</returns>

function TDataBase.Check: integer;
begin

    Result:=0;

    /// <remarks>
    /// It is not enough to establish connection, we must check if query can be executed. Therefore, we use simplest query
    /// to check if we can connect to the database and send/receive data.
    /// </remarks>

    var StrSQL: string:='SELECT 1';
    var ConCheck: TADOConnection:=TADOConnection.Create(nil);

    // Assign parameters
    ConCheck.ConnectionString :=FDbConnStr;
    ConCheck.ConnectionTimeout:=FConTimeout;
    ConCheck.CommandTimeout   :=FCmdTimeout;
    ConCheck.KeepConnection   :=False;
    ConCheck.LoginPrompt      :=False;
    ConCheck.Mode             :=cmRead;

    // Try to connect
    try
        var EO: EOleException;
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
            THelpers.ExecMessage(False, TMessaging.TWParams.ConnectionOk, TUnknown.NULL, MainForm);
            ConCheck.Close;
        end
        else
        begin
            THelpers.ExecMessage(False, TMessaging.TWParams.ConnectionError, TUnknown.NULL, MainForm);
        end;

        ConCheck.Free;
    end;

end;


end.

