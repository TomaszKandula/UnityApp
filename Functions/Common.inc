const

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

    deSemicolon = ';';
    deComma     = ',';
    dePipe      = '|';

    brRound  = 0;
    brSquare = 1;
    brCurly  = 2;

    enQuotesOn  = 0;
    enQuotesOff = 1;

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

    strNULL      = 'NULL';

    ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
    ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';

    RISK_CLASS_A = '0,80';
    RISK_CLASS_B = '0,15';
    RISK_CLASS_C = '0,05';

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

    ttImplicit = 0;
    ttExplicit = 1;

    gsRunning   = 'Running';
    gsCompleted = 'Completed';

    schCustomer = 'Customer';
    schErp      = 'Erp';
    schCommon   = 'Common';

    AgeViewExport     = 'Customer.AgeViewExport';
    AgeViewReport     = 'Customer.AgeViewReport';
    QueryOpenItems    = 'Customer.QueryOpenItems';
    TrackerList       = 'Customer.TrackerList';
    UpsertFreeColumns = 'Customer.UpsertFreeColumns';

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

    ConStrOLEDB: string = 'Provider={OLEDB_Provider};'              +
                          'Data Source={Common_Server};'            +
                          'Initial catalog={Common_Database};'      +
                          'Persist Security Info={OLEDB_PSI};'      +
                          'MultipleActiveResultSets={Common_MARS};' +
                          'Encrypt={Common_Encrypt};'               +
                          'TrustServerCertificate={Common_TSC};'    +
                          'User Id={Common_UserName};'              +
                          'Password={Common_Password};';

    ConStrODBC: string = 'Driver={ODBC_Driver};'                 +
                         'Server={Common_Server};'               +
                         'Database={Common_Database};'           +
                         'MARS_Connection={Common_MARS};'        +
                         'Encrypt={Common_Encrypt};'             +
                         'TrustServerCertificate={Common_TSC};'  +
                         'Uid={Common_UserName};'                +
                         'Pwd={Common_Password};';


