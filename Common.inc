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

(* DEPENDENCIES *)

{ E:\Projects\Unity Project\__Unity\__dependencies\Synapse Trunk;  }
{ E:\Projects\Unity Project\__Unity\__dependencies\Synopse Zip;    }
{ E:\Projects\Unity Project\__Unity\__dependencies\BCrypt;         }
{ E:\Projects\Unity Project\__Unity\__dependencies;                }

(* ALL APPLICATION CONSTANTS *)

const

  { WINDOWS MESSAGES | INTERNAL AND EXTERNAL }

  WM_GETINFO = WM_USER + 120;
  WM_EXTINFO = WM_APP  + 150;

  { WINDOWS MESSAGES | WINDOWS SESSIONS }

  NOTIFY_FOR_THIS_SESSION = $0;
  NOTIFY_FOR_ALL_SESSIONS = $1;

  { INITIALIZATION DETAILS }

  DelayStd:       integer   = 50;               { SLOW DOWN A BIT TO SHOW MESSAGE  }
  DelayErr:       integer   = 750;              { SLOW DOWN SIGNIFICANTLY FOR USER }
  AllTasks:       integer   = 10;               { TOTAL NUMBER OF ALL BASIC TASKS  }
  DecryptKey:     integer   = 429496;
  APPCAPTION:     string    = 'Unity';
  LyncCall:       string    = 'LyncCall.exe';
  ConfigFile:     string    = 'Config.cfg';
  LicenceFile:    string    = 'Unity.licx';
  GridImgFile:    string    = 'Unity.img';
  ReleaseFile:    string    = 'Release.pak';
  CurrentMutex:   PWideChar = 'UNITY_10009';  { WARNING! INCREASE IT WITH EACH NEW RELEASE }

  { SPLASH SCREEN }

  spFree = 0;
  spShow = 1;
  spSplash = 100;
  spUpdate = 200;

  { DESKTOP WINDOW MANAGER API }

  DWMI:           string  = 'dwmapi.dll';

  { MICROSOFT ASSEMBLIES FOR LYNC/SKYPE FOR BUSINESS }

  DLL1:           string = 'Microsoft.Lync.Controls.dll';
  DLL2:           string = 'Microsoft.Lync.Controls.Framework.dll';
  DLL3:           string = 'Microsoft.Lync.Model.dll';
  DLL4:           string = 'Microsoft.Lync.Utilities.dll';
  DLL5:           string = 'Microsoft.Office.Uc.dll';

  { EMAIL OPTION }

  auNTLM  = 0;
  auBASIC = 1;

  { EMAIL AUTHENTICATION ENUMS }

  cdoSendUsingPickup   = 1;
  cdoSendUsingPort     = 2;
  cdoSendUsingExchange = 3;
  cdoAnonymous         = 0;
  cdoBasic             = 1;
  cdoNTLM              = 2;

  { APPLICATION NAME }

  APPNAME       = 'Unity';

  { SETTING FILE DECRYPT KEY }

  FILEKEY       = 429496;

  { FIX COLORS }

  SELCOLOR      = $00FFDBB7;
  FONCOLOR      = $006433C9;

  { SETTINGS FILE ID }

  AppConfig     = 2;
  LicData       = 3;

  { UNPACK METHOD ENUMS }

  LeaveAsIs = 0;
  DeleteOld = 1;

  { SECTION NAMES IN SETTING FILE }

  ApplicationDetails  = 'APPLICATION';
  Password            = 'PASSWORD';
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
  VariousLayouts      = 'LAYOUTS';
  TimersSettings      = 'TIMERS_INTERVALS';
  FollowUpColors      = 'FOLLOWUPS_COLORS';
  ColumnPrefix        = 'COLUMN';
  ColumnWidthName     = 'COLUMNWIDTH';
  ColumnOrderName     = 'COLUMNORDER';
  ColumnNames         = 'COLUMNNAMES';

  { RISK CLASS | DEFAULT VALUES }

  RISK_CLASS_A = '0,80';
  RISK_CLASS_B = '0,15';
  RISK_CLASS_C = '0,05';

  { FIXED MESSAGES }

  ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
  ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';

  NULLDATE     : TDateTime = 0;

  { SPECIAL CHARACTERS }

  CRLF         : string = #13#10;
  CR           : char   = #13;
  LF           : char   = #10;
  TAB          : char   = #9;
  SPACE        : char   = #32;
  QUOTE        : char   = #39;
  COMMA        : char   = #44;
  POINT        : char   = #46;
  SingleQuote  : string = '''';
  DoubleQuote  : string = '''''';

  { BRACKET ENUMARATIONS }

  brRound      : integer = 0;
  brSquare     : integer = 1;
  brCurly      : integer = 2;

  { "COLUMN TO LIST" METHOD ENUMS }

  enQuotesOn  = 0;
  enQuotesOff = 1;

  { PICTURE COLOR }

  imGreyed    = 0;
  imColour    = 1;

  { MESSAGE CALL WRAPPER ENUMS }

  mcInfo      = 1;
  mcWarn      = 2;
  mcError     = 3;
  mcQuestion1 = 4;
  mcQuestion2 = 5;
  mcStatusBar = 10;
  conOK       = 14;
  conERROR    = 15;
  strNULL     = 'NULL';

  { WINDOW STATE }

  stModal    = 0;
  stModeless = 1;

  { GENERATE DOCUMENT | ENUMERATION }

  dcReminder  = 0;
  dcStatement = 1;

  { SQL KEYWORDS | ADD. SPACES FOR EASIER CONCATENATING }

  INSERT       : string = ' INSERT INTO ';
  VAL          : string = ' VALUES ';
  SELECT       : string = ' SELECT ';
  SELECT_DIS   : string = ' SELECT DISTINCT ';
  _UPDATE      : string = ' UPDATE ';
  DELETE_FROM  : string = ' DELETE FROM ';
  _SET         : string = ' SET ';
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
  ORDER        : string = ' ORDER BY ';
  ASC          : string = ' ASC ';
  DESC         : string = ' DESC ';
  _AND         : string = ' AND ';
  _AS          : string = ' AS ';
  MAX          : string = ' MAX ';
  EXECUTE      : string = ' EXEC ';

  { "OPEN ITEMS" AND "AGING" WORKERS ENUMERATIONS }

  thNullParameter = 0;
  thCallOpenItems = 1;
  thCallMakeAge   = 2;

  { "ADDRESS BOOK" WORKER ENUMERATION }

  adOpenAll     = 0;
  adOpenForUser = 1;
  adSaveNew     = 2;
  adExport      = 3;
  adImport      = 4;
  adCopy        = 100;
  adPaste       = 101;
  adCut         = 102;
  adESC         = 103;
  adDEL         = 104;

  { RETURN TYPES FOR "GETDATETIME" }

  gdTimeOnly = 0;
  gdDateOnly = 1;
  gdDateTime = 2;

  (* NOTE! WE USE ONLY "YYYY-MM-DD" AND "hh:mm:ss" *)

  gdTimeFormat    : string = 'hh:mm:ss';
  gdDateFormat    : string = 'YYYY-MM-DD';
  gdDateTimeFormat: string = 'YYYY-MM-DD hh:mm:ss';

  { UAC ENUMERATION }

  adAccessLevel = 0;
  adAccessMode  = 1;
  adUserKeyID   = 2;
  adAccessFull  = 'FULL';
  adAccessBasic = 'BASIC';

  { ACCESS LEVELS }

  acADMIN     = 'AD';
  acReadWrite = 'RW';
  acReadOnly  = 'RO';

  { STATUS MESSAGES }

  stReady       = 'Ready.';
  stProcessing  = 'Processing...';
  stExportXLS   = 'Exporting to Excel...';
  stExportCSV   = 'Exporting to CSV file...';
  stImportCSV   = 'Importing from CSV file...';
  stGenerating  = 'Generating age view...';
  stDownloading = 'Downloading Open Items...';
  stLoading     = 'Loading Aging Report...';
  stSQLupdate   = 'Sending to SQL Server...';

  { UNASSIGNED }

  unUnassigned = 'Unassigned item.';
  unNA         = 'N/A';
  unNotFound   = 'Not found!';

  { TIMERS ENUMS }

  tmEnabled    = 1;
  tmDisabled   = 0;
  tmTimersOn   = 2;
  tmTimersOff  = 3;

  { MICROSOFT EXCEL }

  xlWBATWorksheet = -4167;
  xlWARN_MESSAGE  = 'Invalid class string';

  { MAPPED TABLES FROM DATABASE | WE DO NOT USE SEPARATE ORM FRAMEWORK }

  TblDaily       = 'tbl_daily';
  TblGeneral     = 'tbl_general';
  TblCompany     = 'tbl_company';
  TblManagers    = 'tbl_managers';
  TblTeamleaders = 'tbl_teamleaders';
  TblInvoices    = 'tbl_invoices';
  TblTracker     = 'tbl_tracker';
  TblGroup3      = 'tbl_group3';
  TblPaidinfo    = 'tbl_paidinfo';
  TblPmtterms    = 'tbl_pmtterms';
  TblPerson      = 'tbl_person';
  TblUAC         = 'tbl_UAC';
  TblGroups      = 'tbl_groups';
  TblAddressbook = 'tbl_addressbook';
  TblOpenitems   = 'tbl_openitems';
  TblSnapshots   = 'tbl_snapshots';

  { STORED PROCEDURES }

  QueryOpenItems = 'QueryOpenItems';
  TrackerList    = 'TrackerList';
  AgeViewExport  = 'AgeViewExport';
  AgeViewReport  = 'AgeViewReport';
  AgeViewDetails = 'AgeViewDetails';

  { ADO FILTER ENUM, MSDN: https://msdn.microsoft.com/en-us/library/ee275540(v=bts.10).aspx }

  adFilterNone               = 0;
  adFilterPendingRecords     = 1;
  adFilterAffectedRecords    = 2;
  adFilterFetchedRecords     = 3;
  adFilterConflictingRecords = 5;


  { EMAIL GENERATION | TEMPLATES }

  CommonHTMLTable ='<table class="data">'                   +#13#10+
                   '<!-- HEADERS -->'                       +#13#10+
                   '<tr>'                                   +#13#10+
                   '  <!-- COLUMNS -->'                     +#13#10+
                   '  <th class="col1">Invoice No.:</th>'   +#13#10+
                   '  <th class="col2">Invoice Date:</th>'  +#13#10+
                   '  <th class="col3">Due date:</th>'      +#13#10+
                   '  <th class="col4">Currency:</th>'      +#13#10+
                   '  <th class="col5">Amount:</th>'        +#13#10+
                   '  <th class="col6">O/S Amount:</th>'    +#13#10+
                   '</tr>'                                  +#13#10+
                   '  <!-- ROWS WITH DATA -->'              +#13#10+
                   '{ROWS}'                                 +#13#10+
                   '</table>';

  CommonHTMLRow ='<tr>'                                     +#13#10+
                 '  <td class="col1">{INV_NUM}</td>'        +#13#10+
                 '  <td class="col2">{INV_DAT}</td>'        +#13#10+
                 '  <td class="col3">{DUE_DAT}</td>'        +#13#10+
                 '  <td class="col4">{INV_CUR}</td>'        +#13#10+
                 '  <td class="col5">{INV_AMT}</td>'        +#13#10+
                 '  <td class="col6">{INV_OSA}</td>'        +#13#10+
                 '</tr>'                                    +#13#10;


