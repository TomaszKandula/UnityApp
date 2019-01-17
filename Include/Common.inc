
/// <summary>
///     Below we provide all the application constants, so the project files do not use meaningless numbers
///     or custom strings within the methods and arguments. For clarity, always put new constant under relevant section.
// </summary>

const


    // ASSEMBLIES ------------------------------------------------------------------------------------------------------------------------------------------ //


    // ------------------------------------------------------------------------------------------------------------------------- DESKTOP WINDOWS MANAGER API //

    DWMI:  string  = 'dwmapi.dll';
    CS_DROPSHADOW  = $00020000;

    // ----------------------------------------------------------------------------------------------------------------------------- LYNC/SKYPE FOR BUSINESS //

    DLL1:  string = 'Microsoft.Lync.Controls.dll';
    DLL2:  string = 'Microsoft.Lync.Controls.Framework.dll';
    DLL3:  string = 'Microsoft.Lync.Model.dll';
    DLL4:  string = 'Microsoft.Lync.Utilities.dll';
    DLL5:  string = 'Microsoft.Office.Uc.dll';


    // GENERAL --------------------------------------------------------------------------------------------------------------------------------------------- //


    // ------------------------------------------------------------------------------------------------------------------------------------ OPERATING SYSTEM //

    OSNumber = 0;
    OSName   = 1;

    // ------------------------------------------------------------------------------------------------------------------------------------ WINDOWS MESSAGES //

    WM_GETINFO              = WM_USER + 120;
    WM_EXTINFO              = WM_APP  + 150;
    NOTIFY_FOR_THIS_SESSION = $0;
    NOTIFY_FOR_ALL_SESSIONS = $1;

    // ----------------------------------------------------------------------------------------------------------------------------- WIN API | SHELL EXECUTE //

    seOpen    = 'open';
    seExplore = 'explore';
    seFind    = 'find';

    // ------------------------------------------------------------------------------------------------------------------------------- CHARACTERS DEFINITION //

    CRLF         : string = #13#10;
    CR           : char   = #13;
    LF           : char   = #10;
    BACKSPACE    : char   = #8;
    TAB          : char   = #9;
    ESC          : char   = #27;
    SPACE        : char   = #32;
    QUOTE        : char   = #39;
    COMMA        : char   = #44;
    POINT        : char   = #46;
    SingleQuote  : string = '''';
    DoubleQuote  : string = '''''';

    // -------------------------------------------------------------------------------------------------------------------------------------- INITIALIZATION //

    DelayStd    :  integer   = 10;                 // Splash screen: slown down a bit to display the message during startup
    DelayErr    :  integer   = 750;                // Splash screen: slown down significantly for user in case of error
    AllTasks    :  integer   = 37;                 // Total number of all tasks on startup
    SELCOLOR    :  dword     = $00F2E4D7;          // Application color #1
    FONCOLOR    :  dword     = $006433C9;          // Application color #2
    CUSCOLOR    :  dword     = $00FFDBB7;          // Application color #3
    APPCAPTION  :  string    = 'Unity';            // Use it for "Findwindow" method
    LyncCall    :  string    = 'LyncCall.exe';     // Lync/Skype API developed in VS2017/C#
    UnityReader :  string    = 'UnityReader.exe';  // Chromium implementation CEF3 and CEF4
    LicenceFile :  string    = 'Unity.lic';        // Licence file
    GridImgFile :  string    = 'Unity.img';        // Application custom image(s)
    ReleaseFile :  string    = 'Release.zip';      // Release package
    ManifestFile:  string    = 'Unity.manifest';   // Manifest file on the server
    CurrentMutex:  PWideChar = 'UNITY_10177';      // Mutex value, increase it with each release*

    // *Do not allow to run multiple instances of Unity due to lack of sessions

    // --------------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN //

    spFree   = 0;
    spShow   = 1;
    spSplash = 100;
    spUpdate = 200;

    // SETTINGS FILE --------------------------------------------------------------------------------------------------------------------------------------- //


    // --------------------------------------------------------------------------------------------------------------------------------------------- GENERAL //

    DecryptKey:  integer = 429496;        // Setting file key
    ConfigFile:  string  = 'Config.cfg';  // Application settings

    // --------------------------------------------------------------------------------------------------------------------------------------- SECTION NAMES //

    ApplicationDetails  = 'APPLICATION';
    PasswordSection     = 'PASSWORD';
    TabSheetsNames      = 'TABSHEETS_NAMES';
    RiskClassDetails    = 'RISK_CLASS_DETAILS';
    MailerNTLM          = 'MAILER_NTLM';
    MailerBASIC         = 'MAILER_BASIC';
    MailerSetup         = 'MAILER_SETUP';
    DatabaseSetup       = 'DATABASE_SETTINGS';
    OpenItemsData       = 'OPEN_ITEMS';
    AddressBookData     = 'ADDRESS_BOOK';
    GeneralTables       = 'GENERAL_TABLES';
    InvoiceTypes        = 'INVOICE_TYPES';
    TabSheetsCaps       = 'TABSHEETS_CAPTIONS';
    AgingRanges         = 'AGEVIEW_RANGES';
    AgingBasic          = 'AGEVIEW_BASIC';
    AgingFull           = 'AGEVIEW_FULL';
    Unallocated         = 'UNALLOCATED_DEFINITION';
    Layouts             = 'LAYOUTS';
    TimersSettings      = 'TIMERS_INTERVALS';
    FollowUpColors      = 'FOLLOWUPS_COLORS';
    ColumnPrefix        = 'COLUMN';
    ColumnWidthName     = 'COLUMNWIDTH';
    ColumnOrderName     = 'COLUMNORDER';
    ColumnNames         = 'COLUMNNAMES';


    // MODES ----------------------------------------------------------------------------------------------------------------------------------------------- //


    // ----------------------------------------------------------------------------------------------------------------------------------------- BUSY SCREEN //

    scBusyOn  = 28;
    scBusyOff = 29;
    scBusy    = 30;

    scShow = 'ON';
    scHide = 'OFF';

    scAGEVIEW     = 0;
    scOPENITEMS   = 1;
    scADDRESSBOOK = 2;

    // ------------------------------------------------------------------------------------------------------------------------------------------- FILE TYPE //

    AppConfig   :  integer   = 2;
    LicData     :  integer   = 3;

    // -------------------------------------------------------------------------------------------------------------------------------- EMAIL AUTHENTICATION //

    auNTLM  = 0;
    auBASIC = 1;

    // ---------------------------------------------------------------------------------------------------------------------------------------- EMAIL CDOSYS //

    cdoSendUsingPickup   = 1;
    cdoSendUsingPort     = 2;
    cdoSendUsingExchange = 3;
    cdoAnonymous         = 0;
    cdoBasic             = 1;
    cdoNTLM              = 2;

    // -------------------------------------------------------------------------------------------------------------------------------------- SETTINGS PANEL //

    spLock   = 0;
    spUnLock = 1;

    // ------------------------------------------------------------------------------------------------------------------------------------------ DELIMITERS //

    deSemicolon = ';';
    deComma     = ',';
    dePipe      = '|';

    // --------------------------------------------------------------------------------------------------------------------------------------- UNPACK MODES //

    LeaveAsIs = 0;
    DeleteOld = 1;

    // -------------------------------------------------------------------------------------------------------------------------------------- SORTING METHOD //

    sdtSTRING  = 0;
    sdtINTEGER = 1;
    sdtFLOAT   = 2;

    // --------------------------------------------------------------------------------------------------------------------------------------- FILTER WINDOW //

    fltINF7      = 0;
    fltINF4      = 1;
    fltCOCODE    = 2;
    fltAGENT     = 3;
    fltDIVISION  = 4;
    fltFOLLOWUP  = 5;
    fltGR3       = 6;
    fltFree1     = 7;
    fltFree2     = 8;
    fltIncrement = 10;
    fltDecrement = 11;
    fltFree3     = 12;
    fltSalesRep  = 13;
    fltPersonRep = 14;
    fltCustGroup = 15;
    fltAccType   = 16;

    // --------------------------------------------------------------------------------------------------------------------------------------------- BRACKET //

    brRound  = 0;
    brSquare = 1;
    brCurly  = 2;

    // ----------------------------------------------------------------------------------------------------------------------------- "COLUMN TO LIST" METHOD //

    enQuotesOn  = 0;
    enQuotesOff = 1;

    // ----------------------------------------------------------------------------------------------------------------------------------- GENERATE DOCUMENT //

    dcReminder  = 0;
    dcStatement = 1;
    dcAuto      = 2;
    dcManual    = 3;

    // -------------------------------------------------------------------------------------------------------------------------------------------- CALENDAR //

    cfDateToDB = 0;
    cfGetDate  = 1;

    // ------------------------------------------------------------------------------------------------------------------------ OPEN ITEMS AND AGING WORKERS //

    thNullParameter = 0;
    thCallOpenItems = 1;
    thCallMakeAge   = 2;

    // --------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK WORKER //

    adOpenAll     = 0;
    adOpenForUser = 1;
    adUpdate      = 2;
    adExport      = 3;
    adImport      = 4;
    adInsert      = 5;
    adCopy        = 100;
    adPaste       = 101;
    adCut         = 102;
    adESC         = 103;
    adDEL         = 104;

    // ----------------------------------------------------------------------------------------------------------------------------------------- GETDATETIME //

    gdTimeOnly = 0;
    gdDateOnly = 1;
    gdDateTime = 2;

    // ------------------------------------------------------------------------------------------------------------------------------------ SORTING AGE VIEW //

    smRanges   = 0;
    smFollowUp = 1;
    smTotal    = 2;
    smOverdue  = 3;

    // ------------------------------------------------------------------------------------------------------------------------------------------------- UAC //

    adAccessLevel = 0;
    adAccessMode  = 1;
    adUserKeyID   = 2;
    adAccessFull  = 'FULL';
    adAccessBasic = 'BASIC';

    // --------------------------------------------------------------------------------------------------------------------------------------- ACCESS LEVELS //

    acADMIN       = 'AD';
    acReadWrite   = 'RW';
    acReadOnly    = 'RO';

    // --------------------------------------------------------------------------------------------------------------------------------------------- ACTIONS //

    loadNext = 0;
    loadPrev = 1;

    // ----------------------------------------------------------------------------------------------------------------------------------- ACCOUNT STATEMENT //

    maCustom  = 0;
    maDefined = 1;

    // --------------------------------------------------------------------------------------------------------------------------------------- PICTURE COLOR //

    imGreyed = 0;
    imColour = 1;

    // ---------------------------------------------------------------------------------------------------------------------------------------- WINDOW STATE //

    stModal    = 0;
    stModeless = 1;

    // ------------------------------------------------------------------------------------------------------------------------------------------ UNASSIGNED //

    unUnassigned = 'Unassigned item.';
    unNA         = 'N/A';
    unNotFound   = 'Not found!';

    // ---------------------------------------------------------------------------------------------------------------------------------------------- TIMERS //

    tmEnabled   = 1;
    tmDisabled  = 0;
    tmTimersOn  = 2;
    tmTimersOff = 3;

    // --------------------------------------------------------------------------------------------------------------------------------------- GIF ANIMATION //

    AnimationON  = 0;
    AnimationOFF = 1;

    // --------------------------------------------------------------------------------------------------------------------------------- MESSAGE BOX WRAPPER //

    mcInfo       = 1;
    mcWarn       = 2;
    mcError      = 3;
    mcQuestion1  = 4;
    mcQuestion2  = 5;
    mcStatusBar  = 10;
    conOK        = 14;
    conERROR     = 15;
    mmMailerItem = 26;
    mmAwaitClose = 27;
    strNULL      = 'NULL';

    // ---------------------------------------------------------------------------------------------------------------------------- DATABASE DRIVER/PROVIDER //

    dbOLEDB = 'OLEDB';
    dbODBC  = 'ODBC';

    // -------------------------------------------------------------------------------------------------------------------------------- ADODB/ADO.NET FILTER //

    /// <seealso cref="https://msdn.microsoft.com/en-us/library/ee275540(v=bts.10).aspx"/>

    adFilterNone               = 0;
    adFilterPendingRecords     = 1;
    adFilterAffectedRecords    = 2;
    adFilterFetchedRecords     = 3;
    adFilterConflictingRecords = 5;


    // DATE & TIME ----------------------------------------------------------------------------------------------------------------------------------------- //


    // --------------------------------------------------------------------------------------------------------------------------------------- GLOBAL FORMAT //

    gdTimeFormat    : string  = 'hh:mm:ss';
    gdDateFormat    : string  = 'YYYY-MM-DD';
    gdDateTimeFormat: string  = 'YYYY-MM-DD hh:mm:ss';

    // --------------------------------------------------------------------------------------------------------------------------------------- NO DATE VALUE //

    NULLDATE : TDateTime = 0;


    // MESSAGES -------------------------------------------------------------------------------------------------------------------------------------------- //


    // ----------------------------------------------------------------------------------------------------------------------------------------------- ADODB //

    ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
    ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';

    // ------------------------------------------------------------------------------------------------------------------------------------------ STATUS BAR //

    stReady       = 'Ready';
    stProcessing  = 'Processing...';
    stExportXLS   = 'Exporting to Excel...';
    stExportCSV   = 'Exporting to CSV file...';
    stImportCSV   = 'Importing from CSV file...';
    stGenerating  = 'Generating age view...';
    stDownloading = 'Downloading Open Items...';
    stLoading     = 'Loading Aging Report...';
    stSQLupdate   = 'Sending to SQL Server...';

    // ----------------------------------------------------------------------------------------------------------------------------------- SPLASH INDICATORS //

    spSetting    = 'getting settings..., please wait.';
    spConnecting = 'connecting to MS SQL Server..., please wait.';
    spUserAccess = 'getting user access details..., please wait.';
    spSnapshots  = 'loading default age view..., please wait.';
    spGeneral    = 'loading General Tables..., please wait.';
    spMapping    = 'loading Map Tables..., please wait.';
    spFinishing  = 'finishing..., please wait.';

    // -------------------------------------------------------------------------------------------------------------------------------------- QUERY STATUSES //

    qrOPEN     = 'OPEN';
    qrPENDING  = 'PENDING';
    qrREJECTED = 'REJECTED';
    qrSOLVED   = 'SOLVED';

    // ------------------------------------------------------------------------------------------------------------------------------------------------- QMS //

    QmsFsc = 0;
    QmsLbu = 1;


    // COMPONENT RELATED ----------------------------------------------------------------------------------------------------------------------------------- //


    // ------------------------------------------------------------------------------------------------------------------------ AGE VIEW | RISK CLASS COLORS //

    FClassA = 16777215;
    BClassA = 8421631;
    FClassC = 16777215;
    BClassC = 11316313;
    FClassB = 8404992;
    BClassB = 13434879;

    // ---------------------------------------------------------------------------------------------------------------- AGE VIEW | RISK CLASS DEFAULT VALUES //

    RISK_CLASS_A = '0,80';
    RISK_CLASS_B = '0,15';
    RISK_CLASS_C = '0,05';

    // ---------------------------------------------------------------------------------------------------------------------------- STRING GRID | ROW HEIGHT //

    sgRowHeight = 19;
    sgRowHidden = -1;

    // ----------------------------------------------------------------------------------------------------------------------- STRING GRID | MICROSOFT EXCEL //

    xlWBATWorksheet = -4167;
    xlWARN_MESSAGE  = 'Invalid class string';


    // SQL SERVER DATABASE --------------------------------------------------------------------------------------------------------------------------------- //


    // ------------------------------------------------------------------------------------------------------------------------------------------------ NCSI //

    ncsiWww  : string = 'http://www.msftncsi.com/';
    ncsiFile : string = 'ncsi.txt';
    ncsiGet  : string = 'GET';
    ncsiHead : string = 'HEAD';

    HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
    HTTPREQUEST_SETCREDENTIALS_FOR_PROXY  = 1;

    WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW    = 0;
    WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM = 1;
    WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH   = 2;

    MAX_CHECK_ATTEMPTS = 6;

    // ---------------------------------------------------------------------------------------------------------------------------------------- SQL KEYWORDS //

    /// <remarks>
    /// Use additional spaces in strings for easy concatenating.
    /// </remarks>

    INSERT       : string = ' INSERT INTO ';
    VAL          : string = ' VALUES ';
    SELECT       : string = ' SELECT ';
    SELECT_DIS   : string = ' SELECT DISTINCT ';
    DISTINCT     : string = ' DISTINCT ';
    _UPDATE      : string = ' UPDATE ';
    DELETE_FROM  : string = ' DELETE FROM ';
    _SET         : string = ' SET ';
    _BEGIN       : string = ' BEGIN ';
    _END         : string = ' END ';
    UNION        : string = ' UNION ALL ';
    EQUAL        : string = ' = ';
    FROM         : string = ' FROM ';
    ALL          : string = ' * ';
    WHERE        : string = ' WHERE ';
    LEFT_JOIN    : string = ' LEFT JOIN ';
    RIGHT_JOIN   : string = ' RIGHT JOIN ';
    INNER_JOIN   : string = ' INNER JOIN ';
    OUTER_JOIN   : string = ' OUTER JOIN ';
    _ON          : string = ' ON ';
    _OR          : string = ' OR ';
    ORDER        : string = ' ORDER BY ';
    ASC          : string = ' ASC ';
    DESC         : string = ' DESC ';
    _AND         : string = ' AND ';
    _AS          : string = ' AS ';
    MAX          : string = ' MAX ';
    SUM          : string = ' SUM ';
    LIKE         : string = ' LIKE ';
    EXECUTE      : string = ' EXEC ';
    MATCHCASE    : string = ' COLLATE SQL_Latin1_General_CP1_CS_AS ';

    // -------------------------------------------------------------------------------------------------------------------------------- TRANSACTION TEMPLATE //

    CommonSelect = 'IF EXISTS (SELECT 1 FROM {DestTable} WITH (UPDLOCK) WHERE {Condition})';
    CommonDelete = '{DeleteData} WHERE {Condition}';
    TransactTemp = 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE' + #13#10 +
                   'BEGIN TRANSACTION                           ' + #13#10 +
                   '    SET XACT_ABORT ON                       ' + #13#10 +
                   '    SET NOCOUNT {SWITCH}                    ' + #13#10 +
                   '    {CommonSelect}                          ' + #13#10 +
                   '    {Begin}                                 ' + #13#10 +
                   '    {CommonDelete}                          ' + #13#10 +
                   '    {SimpleInput}                           ' + #13#10 +
                   '    {ComplexInput}                          ' + #13#10 +
                   '    {End}                                   ' + #13#10 +
                   'COMMIT TRANSACTION                          ';

    // -------------------------------------------------------------------------------------------------------------------------------- SQL TRANSACTION TYPE //

    ttImplicit = 0;
    ttExplicit = 1;

    // ----------------------------------------------------------------------------------------------------------------------------------- SSIS STATUS CODES //

    gsRunning   = 'Running';
    gsCompleted = 'Completed';

    // --------------------------------------------------------------------------------------------------------------------------------------------- SCHEMAS //

    schCustomer = 'Customer';
    schErp      = 'Erp';
    schCommon   = 'Common';

    // ----------------------------------------------------------------------------------------------------------------------------------- STORED PROCEDURES //

    AgeViewExport  = 'Customer.AgeViewExport';
    AgeViewReport  = 'Customer.AgeViewReport';
    QueryOpenItems = 'Customer.QueryOpenItems';
    TrackerList    = 'Customer.TrackerList';


    // TEMPLATES ------------------------------------------------------------------------------------------------------------------------------------------- //


    // ----------------------------------------------------------------------------------------------------------------------------------- EMAIL GENERATION //

    HTML_BR = '<br>';

    CommonHTMLTable ='<table class="data">'                   + #13#10 +
                     '<!-- HEADERS -->'                       + #13#10 +
                     '<tr>'                                   + #13#10 +
                     '  <!-- COLUMNS -->'                     + #13#10 +
                     '  <th class="col1">Invoice No.:</th>'   + #13#10 +
                     '  <th class="col2">Invoice Date:</th>'  + #13#10 +
                     '  <th class="col3">Due date:</th>'      + #13#10 +
                     '  <th class="col4">Currency:</th>'      + #13#10 +
                     '  <th class="col5-h">Amount:</th>'      + #13#10 +
                     '  <th class="col6-h">O/S Amount:</th>'  + #13#10 +
                     '  <th class="col7">Status:</th>'        + #13#10 +
                     '</tr>'                                  + #13#10 +
                     '  <!-- ROWS WITH DATA -->'              + #13#10 +
                     '{ROWS}'                                 + #13#10 +
                     '</table>';

    CommonHTMLRow ='<tr>'                                     + #13#10 +
                   '  <td class="col1">{INV_NUM}</td>'        + #13#10 +
                   '  <td class="col2">{INV_DAT}</td>'        + #13#10 +
                   '  <td class="col3">{DUE_DAT}</td>'        + #13#10 +
                   '  <td class="col4">{INV_CUR}</td>'        + #13#10 +
                   '  <td class="col5">{INV_AMT}</td>'        + #13#10 +
                   '  <td class="col6">{INV_OSA}</td>'        + #13#10 +
                   '  <td class="col7">{INV_CRL}</td>'        + #13#10 +
                   '</tr>'                                    + #13#10;


    // ----------------------------------------------------------------------------------------------------------------------------------- SERVER CONNECTION //


    /// <summary>
    ///     Connection string template for OLE DB providers.
    /// </summary>

    ConStrOLEDB: string = 'Provider={OLEDB_Provider};'              +
                          'Data Source={Common_Server};'            +
                          'Initial catalog={Common_Database};'      +
                          'Persist Security Info={OLEDB_PSI};'      +
                          'MultipleActiveResultSets={Common_MARS};' +
                          'Encrypt={Common_Encrypt};'               +
                          'TrustServerCertificate={Common_TSC};'    +
                          'User Id={Common_UserName};'              +
                          'Password={Common_Password};';

    /// <summary>
    ///     Connection string template for ODBC drivers.
    /// </summary>

    ConStrODBC: string = 'Driver={ODBC_Driver};'                 +
                         'Server={Common_Server};'               +
                         'Database={Common_Database};'           +
                         'MARS_Connection={Common_MARS};'        +
                         'Encrypt={Common_Encrypt};'             +
                         'TrustServerCertificate={Common_TSC};'  +
                         'Uid={Common_UserName};'                +
                         'Pwd={Common_Password};';


