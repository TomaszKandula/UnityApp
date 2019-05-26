unit Helpers;


interface


uses
    Messages;


type

    /// <summary>
    /// Defines different statement option.
    /// </summary>

    TInvoiceFilter = (ReminderOvd, ReminderNonOvd, ShowAllItems);

    /// <remarks>
    /// Reference to two dimensional string array.
    /// </remarks>

    TALists = array of array of string;

    /// <remarks>
    /// Reference to one dimensional string array.
    /// </remarks>

    TAStrings = array of string;

    /// <remarks>
    /// Reference to one dimensional integer array.
    /// </remarks>

    TAIntigers = array of integer;

    /// <remarks>
    /// This record definition allows to hold column numbers for given column name. This is necessary as column order may change.
    /// Normally we would use "ReturnColumn" extension method, but in case of multithreading, we must pre-set them before many threads
    /// use it at the same time (VCL components are not thread safe). Having record with fields simplify things.
    /// </remarks>

    TOpenItemsRefs = record
        Ad1Col:         integer;
        Ad2Col:         integer;
        Ad3Col:         integer;
        PnoCol:         integer;
        PAreaCol:       integer;
        CuidCol:        integer;
        OpenAmCol:      integer;
        PmtStatCol:     integer;
        CtrlCol:        integer;
        InvoNoCol:      integer;
        ValDtCol:       integer;
        DueDtCol:       integer;
        ISOCol:         integer;
        CurAmCol:       integer;
        OpenCurAmCol:   integer;
        Text:           integer;
    end;

    /// <remarks>
    /// This record define column numbers for given field, so we do not have to use each time "ReturnColumn".
    /// </remarks>

    TControlStatusRefs = record
        Id:             integer;
        Code:           integer;
        Text:           integer;
        Description:    integer;
    end;

    /// <summary>
    /// To be deleted (legacy code).
    /// </summary>

    TUserAccess = class abstract
        type TTypes = (AccessLevel, AccessMode, UserKeyId);
        const AccessFull  = 'FULL';
        const AccessBasic = 'BASIC';
        const Admin       = 'AD';
        const ReadWrite   = 'RW';
        const ReadOnly    = 'RO';
    end;

    /// <summary>
    /// For internal communication between threads and main thread (UI) we use defined numeric arguments.
    /// </summary>

    TMessaging = class abstract

        const BusyScreenOff = 28;
        const BusyScreenOn  = 29;
        const scAGEVIEW     = 0;
        const scOPENITEMS   = 1;
        const scADDRESSBOOK = 2;

        const BusyScreenMan = 30;
        const scShow        = 100;
        const scHide        = 101;

        const conOK         = 14;
        const conERROR      = 15;

        const mmMailerItem  = 26;
        const mmAwaitClose  = 27;

        const msInfo        = 1;
        const msWarn        = 2;
        const msError       = 3;
        const mcQuestion1   = 4;
        const mcQuestion2   = 5;

        const msStatusBar   = 10;

    end;

    /// <summary>
    ///
    /// </summary>

    TChars = class abstract
        const CrLf:        string = #13#10;
        const Cr:          char   = #13;
        const LF:          char   = #10;
        const BACKSPACE:   char   = #8;
        const TAB:         char   = #9;
        const ESC:         char   = #27;
        const SPACE:       char   = #32;
        const QUOTE:       char   = #39;
        const COMMA:       char   = #44;
        const POINT:       char   = #46;
        const SingleQuote: string = '''';
        const DoubleQuote: string = '''''';
    end;

    /// <summary>
    ///
    /// </summary>

    TSorting = class abstract
        const StringType: integer = 0;
        const IntType:    integer = 1;
        const FloatType:  integer = 2;
        const smRanges:   integer = 0; // !!!
        const smFollowUp: integer = 1; // !!!
        const smTotal:    integer = 2; // !!!
        const smOverdue:  integer = 3; // !!!
    end;

    /// <summary>
    ///
    /// </summary>

    TFiltering = class abstract
        type TColumns = (
            Inf7,
            Inf4,
            CoCode,
            Agent,
            Division,
            Follow,
            Group3,
            Free1,
            Free2,
            Free3,
            SalesResponsible,
            PersonResponsible,
            CustomerGroup,
            AccountType
        );
    end;

    /// <summary>
    /// Application common types and constants.
    /// </summary>

    TCommon = class abstract
        type  TUnityFiles = (AppConfig, LicData);
        type  TTimers     = (TurnedOn, TurnedOff);
        type  TMsgTypes   = (Info, Warn, Error, Question1, Question2);
    end;

    /// <summary>
    /// Predefine date and time formats.
    /// </summary>

    TDateTimeFormats = class abstract
        const TimeFormat:     string = 'hh:mm:ss';
        const DateFormat:     string = 'YYYY-MM-DD';
        const DateTimeFormat: string = 'YYYY-MM-DD hh:mm:ss';
        const NullDate:       TDateTime = 0;
    end;

    /// <summary>
    ///
    /// </summary>

    TNaVariants = class abstract
        const Null:       string = 'NULL';
        const Unassigned: string = 'Unassigned item.';
        const NA:         string = 'N/A';
        const NotFound:   string = 'Not found!';
    end;

    /// <summary>
    ///
    /// </summary>

    TUnityApp = class abstract
        const SELCOLOR = $00F2E4D7;
        const FONCOLOR = $006433C9;
        const CUSCOLOR = $00FFDBB7;
        const DecryptKey:   integer   = 429496;
        const APPCAPTION:   string    = 'Unity';
        const LyncCall:     string    = 'LyncCall.exe';
        const UnityReader:  string    = 'UnityReader.exe';
        const LicenceFile:  string    = 'Unity.lic';
        const GridImgFile:  string    = 'Unity.img';
        const ReleaseFile:  string    = 'Release.zip';
        const ManifestFile: string    = 'Unity.manifest';
        const CurrentMutex: PWideChar = 'UNITY_10235';
        const ConfigFile:   string    = 'Config.cfg';
    end;

    /// <summary>
    /// Predefined messages displayed during splash screen along with latency and task numbers.
    /// </summary>

    TSplashScreen = class abstract
        const DelayStd       = 10;
        const DelayErr       = 750;
        const AllTasks       = 37;
        const SettingUp      = 'getting settings..., please wait.';
        const Connecting     = 'connecting to MS SQL Server..., please wait.';
        const GettingUsers   = 'getting user access details..., please wait.';
        const GettingAgeing  = 'loading default age view..., please wait.';
        const GettingGeneral = 'loading General Tables..., please wait.';
        const MappingTables  = 'loading Map Tables..., please wait.';
        const Finishing      = 'finishing..., please wait.';
    end;

    /// <summary>
    /// Windows/WinApi related helpers including predefine windows message names (private and non-private).
    /// </summary>

    TWindows = class abstract
        type  TState = (Modal, Modeless);
        const DWMI = 'dwmapi.dll';
        const CS_DROPSHADOW = $00020000;
        const WM_GETINFO    = WM_USER + 120;
        const WM_EXTINFO    = WM_APP  + 150;
        const NOTIFY_FOR_THIS_SESSION = $0;
        const NOTIFY_FOR_ALL_SESSIONS = $1;
    end;

    /// <summary>
    /// List of external assemblies necessary for application.
    /// </summary>

    TSkypeAssemblies = class abstract
        const LyncControls:  string = 'Microsoft.Lync.Controls.dll';
        const LyncFramework: string = 'Microsoft.Lync.Controls.Framework.dll';
        const LyncModel:     string = 'Microsoft.Lync.Model.dll';
        const LyncUtils:     string = 'Microsoft.Lync.Utilities.dll';
        const OfficeUc:      string = 'Microsoft.Office.Uc.dll';
    end;

    /// <summary>
    ///
    /// </summary>

    TUChars = class abstract
        const CRLF:        string = #13#10;
        const CR:          char   = #13;
        const LF:          char   = #10;
        const BACKSPACE:   char   = #8;
        const TAB:         char   = #9;
        const ESC:         char   = #27;
        const SPACE:       char   = #32;
        const QUOTE:       char   = #39;
        const COMMA:       char   = #44;
        const POINT:       char   = #46;
        const SingleQuote: string = '''';
        const DoubleQuote: string = '''''';
    end;

    /// <summary>
    ///
    /// </summary>

    TSql = class abstract
        const INSERT       = ' INSERT INTO ';
        const VAL          = ' VALUES ';
        const SELECT       = ' SELECT ';
        const SELECT_DIS   = ' SELECT DISTINCT ';
        const DISTINCT     = ' DISTINCT ';
        const _UPDATE      = ' UPDATE ';
        const DELETE_FROM  = ' DELETE FROM ';
        const _SET         = ' SET ';
        const _BEGIN       = ' BEGIN ';
        const _END         = ' END ';
        const UNION        = ' UNION ALL ';
        const EQUAL        = ' = ';
        const FROM         = ' FROM ';
        const ALL          = ' * ';
        const WHERE        = ' WHERE ';
        const LEFT_JOIN    = ' LEFT JOIN ';
        const RIGHT_JOIN   = ' RIGHT JOIN ';
        const INNER_JOIN   = ' INNER JOIN ';
        const OUTER_JOIN   = ' OUTER JOIN ';
        const _ON          = ' ON ';
        const _OR          = ' OR ';
        const ORDER        = ' ORDER BY ';
        const ASC          = ' ASC ';
        const DESC         = ' DESC ';
        const _AND         = ' AND ';
        const _AS          = ' AS ';
        const MAX          = ' MAX ';
        const SUM          = ' SUM ';
        const LIKE         = ' LIKE ';
        const EXECUTE      = ' EXEC ';
        const MATCHCASE    = ' COLLATE SQL_Latin1_General_CP1_CS_AS ';
    end;

    /// <summary>
    ///
    /// </summary>

    TDelimiters = class abstract
        const Semicolon = ';';
        const Comma     = ',';
        const Pipe      = '|';
    end;

    /// <summary>
    ///
    /// </summary>

    TEmails = class abstract
        type TAuthTypes = (cdoAnonymous, cdoBasic, cdoNTLM);
        type TAuthUsing = (cdoNone, cdoSendUsingPickup, cdoSendUsingPort, cdoSendUsingExchange);
    end;

    /// <summary>
    ///
    /// </summary>

    TDocuments = class abstract
        type TType = (dcReminder, dcStatement, dcAuto, dcManual);
        type TMode = (maCustom, maDefined);
    end;

    /// <summary>
    ///
    /// </summary>

    TAnimation = class abstract
        type TState = (AnimationON, AnimationOFF);
    end;

    /// <summary>
    ///
    /// </summary>

    TEnums = class abstract
        type TBrackets   = (brRound, brSquare, brCurly);
        type TQuotes     = (Enabled, Disabled);
        type TLoading    = (thNullParameter, thCallOpenItems, thCallMakeAge);
        type TCalendar   = (cfDateToDB, cfGetDate, gdTimeOnly, gdDateOnly, gdDateTime);
        type TActionTask = (adOpenAll, adOpenForUser, adInsert, adUpdate, adExport, adImport, adCopy, adPaste, adCut, adESC, adDEL);
    end;

    /// <summary>
    ///
    /// </summary>

    TADODB = class abstract
        /// <seealso cref="https://msdn.microsoft.com/en-us/library/ee275540(v=bts.10).aspx"/>
        type  TFilters    = (adFilterNone, adFilterPendingRecords, adFilterAffectedRecords, adFilterFetchedRecords, adFilterConflictingRecords);
        const dbOLEDB     = 'OLEDB';
        const dbODBC      = 'ODBC';
        const ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
        const ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';
    end;

    /// <summary>
    ///
    /// </summary>

    TStatusBar = class abstract
        const Ready       = 'Ready';
        const Processing  = 'Processing...';
        const ExportXLS   = 'Exporting to Excel...';
        const ExportCSV   = 'Exporting to CSV file...';
        const ImportCSV   = 'Importing from CSV file...';
        const Generating  = 'Generating age view...';
        const Downloading = 'Downloading Open Items...';
        const Loading     = 'Loading Aging Report...';
        const SQLupdate   = 'Sending to SQL Server...';
    end;

    /// <summary>
    ///
    /// </summary>

    TQms = class abstract
        const Open     = 'OPEN';
        const Pending  = 'PENDING';
        const Rejected = 'REJECTED';
        const Solved   = 'SOLVED';
    end;

    /// <summary>
    ///
    /// </summary>

    TRiskClass = class abstract
        const FClassA = 16777215;
        const BClassA = 8421631;
        const FClassC = 16777215;
        const BClassC = 11316313;
        const FClassB = 8404992;
        const BClassB = 13434879;
        const RISK_CLASS_A = '0,80';
        const RISK_CLASS_B = '0,15';
        const RISK_CLASS_C = '0,05';
    end;

    /// <summary>
    ///
    /// </summary>

    TNCSI = class abstract
        const HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
        const HTTPREQUEST_SETCREDENTIALS_FOR_PROXY = 1;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW = 0;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM = 1;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH = 2;
        const MAX_CHECK_ATTEMPTS = 6;
        const ncsiWww:  string = 'http://www.msftncsi.com/';
        const ncsiFile: string = 'ncsi.txt';
        const ncsiGet:  string = 'GET';
        const ncsiHead: string = 'HEAD';
    end;


implementation


end.

