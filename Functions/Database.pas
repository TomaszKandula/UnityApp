
{$I .\Include\Header.inc}

unit Database;


interface


uses
    ADODB,
    Classes,
    SysUtils,
    ComObj,
    StrUtils;

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
        var FDBConnStr:      string;
        var FCmdTimeout:     integer;
        var FConTimeout:     integer;
        var FMsgConnStr:     string;
        var FMsgInitStatus:  string;
        var FMsgConnCheck:   string;
    public
        property MsgConnStr:    string read FMsgConnStr;
        property MsgInitStatus: string read FMsgInitStatus;
        property MsgConnCheck:  string read FMsgConnCheck;
        constructor Create;
        procedure   InitializeConnection(var ActiveConnection: TADOConnection);
        function    Check: integer;
    end;


implementation


{$I .\Functions\Common.inc}


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TDataBase.Create;
var
    dbConnNoPwd: string;
begin

    // Set all data
    OLEDB_Provider :='SQLNCLI11';
    OLEDB_PSI      :='False';
    Common_MARS    :='No';
    Common_TSC     :='No';
    Common_Encrypt :='Yes';
    Common_Server  :='citeam.database.windows.net,1433';
    Common_Database:='1305_UnityDB';
    Common_UserName:='unityaccess';
    Common_Password:='Unity1984';
    FCmdTimeout    :=15;
    FConTimeout    :=15;
    Interval       :=5000;

    // Replace tags for retrieved data (it ignores tags that are missing)
    // Passwordless connection string
    dbConnNoPwd:=ConStrOLEDB;
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
    FDBConnStr:=ReplaceStr(dbConnNoPwd, '{Common_Password}', Common_Password);
    FMsgConnStr:='Connection string built [show_no_password] = ' + dbConnNoPwd;

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

procedure TDataBase.InitializeConnection(var ActiveConnection: TADOConnection);
begin

    if not (ActiveConnection = nil)
        then ActiveConnection.Connected:=False;

    try

        // Connection settings
        ActiveConnection.ConnectionString :=FDBConnStr;
        ActiveConnection.ConnectionTimeout:=FConTimeout;
        ActiveConnection.CommandTimeout   :=FCmdTimeout;

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
            FMsgInitStatus:='Server connection has been established successfully.';
        except
            on E: Exception do
                FMsgInitStatus:=E.ClassName + ': ' + E.Message + ' (' + IntToStr(ExitCode) + ').';
        end;

    finally
        //
    end;

end;

// ---------------------------------------------------------------------------------------------------------------------------------- CONNECTION WITH SERVER //

/// <summary>
/// Check if can connect and query against given database.
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
    /// It is not enough to establish connection, we must check if query can be executed. Therefore, we use simplist query
    /// to check if we can connect to the database and send/receive data.
    /// </remarks>

    StrSQL:='SELECT 1';
    ConCheck:=TADOConnection.Create(nil);

    // Assign parameters
    ConCheck.ConnectionString :=FDBConnStr;
    ConCheck.ConnectionTimeout:=FConTimeout;
    ConCheck.CommandTimeout   :=FCmdTimeout;
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
            FMsgConnCheck:='Connection is OK!';
            ConCheck.Close;
        end
        else
        begin
            FMsgConnCheck:='Connection has failed!';
        end;

        ConCheck.Free;
    end;

end;


end.

