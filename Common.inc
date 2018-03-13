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

(* ALL APPLICATION CONSTANTS *)

const

  { WINDOWS MESSAGES FOR EXTERNAL AND INTERNAL MESSAGES }

  WM_GETINFO = WM_USER + 120;
  WM_EXTINFO = WM_APP  + 150;

  { APPLICATION EXTERNAL ASSEMBLY | STATIC LINKING }

  EX_LIBRARY = 'Unitylib.dll';

  { INITIALIZATION DETAILS }

  DelayStd:       integer = 50;               { SLOW DOWN A BIT TO SHOW MESSAGE  }
  DelayErr:       integer = 750;              { SLOW DOWN SIGNIFICANTLY FOR USER }
  AllTasks:       integer = 15;               { TOTAL NUMBER OF ALL BASIC TASKS  }
  DecryptKey:     integer = 429496;
  APPCAPTION:     string  = 'Unity';
  LyncCall:       string  = 'LyncCall.exe';
  ConfigFile:     string  = 'config.cfg';
  UserFolder:     string  = 'users';
  LicenceFile:    string  = 'Unity.licx';

  { DESKTOP WINDOW MANAGER API }

  DWMI:           string  = 'dwmapi.dll';

  { SYNAPSE ASSEMBLIES }

  DLL1:           string  = 'libeay32.dll';
  DLL2:           string  = 'ssleay32.dll';
  DLL3:           string  = 'vsinit.dll';
  CRC32DLL1:      DWord   = 3118596872;       { CRC32 CHECK }
  CRC32DLL2:      DWord   = 3608491174;       { CRC32 CHECK }
  CRC32DLL3:      DWord   = 4084329229;       { CRC32 CHECK }

  { MICROSOFT ASSEMBLIES FOR LYNC/SKYPE FOR BUSINESS }

  DLL4:           string = 'Microsoft.Lync.Controls.dll';
  DLL5:           string = 'Microsoft.Lync.Controls.Framework.dll';
  DLL6:           string = 'Microsoft.Lync.Model.dll';
  DLL7:           string = 'Microsoft.Lync.Utilities.dll';
  DLL8:           string = 'Microsoft.Office.Uc.dll';

  { APPLICATION NAME }

  APPNAME             = 'Unity';

  { SETTING FILE DECRYPT KEY }

  FILEKEY             = 429496;

  { FIX COLORS }

  SELCOLOR            = $00FFDBB7;
  FONCOLOR            = $006433C9;

  { SETTINGS FILE ID }

  UserConfig          = 1;
  AppConfig           = 2;
  LicData             = 3;

  { SECTION NAMES IN SETTING FILE }

  ApplicationDetails  = 'APPLICATION';
  Password            = 'PASSWD';
  TabSheetsNames      = 'TABSHEETS_NAMES';
  RiskClassDetails    = 'RISK_CLASS_DETAILS';
  MailerCDOSYS        = 'MAILER_CDOSYS_NTLM';
  MailerSYNAPSE       = 'MAILER_SYNAPSE';
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

  { SQL KEYWORDS | ADD. SPACES FOR EASIER CONCATENATING }

  INSERT       : string = ' INSERT INTO ';
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
  stLoading     = 'Loading aging report...';

  { UNASSIGNED }

  unUnassigned = 'Unassigned item.';
  unNA         = 'N/A';

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
  AgeViewExport  = 'AgeViewExport';
  AgeViewReport  = 'AgeViewReport';
  AgeViewDetails = 'AgeViewDetails';

  { CURSOR TYPE ENUM, MSDN: https://docs.microsoft.com/en-us/sql/ado/reference/ado-api/cursortypeenum }

  adOpenDynamic     = 2;
  adOpenForwardOnly = 0;
  adOpenKeyset      = 1;
  adOpenStatic      = 3;
  adOpenUnspecified = -1;

  { COMMAND TYPE ENUM, MSDN: https://docs.microsoft.com/en-us/sql/ado/reference/ado-api/commandtypeenum }

  adCmdUnspecified = -1;
  adCmdText        = 1;
  adCmdTable       = 2;
  adCmdStoredProc	 = 4;
  adCmdUnknown     = 8;
  adCmdFile        = 256;
  adCmdTableDirect = 512;

  { LOCK TYPE ENUM, MSDN: https://docs.microsoft.com/en-us/sql/ado/reference/ado-api/locktypeenum }

  adLockBatchOptimistic = 4;
  adLockOptimistic      = 3;
  adLockPessimistic     = 2;
  adLockReadOnly        = 1;
  adLockUnspecified     = -1;

  { ADO FILTER ENUM, MSDN: https://msdn.microsoft.com/en-us/library/ee275540(v=bts.10).aspx }

  adFilterNone               = 0;
  adFilterPendingRecords     = 1;
  adFilterAffectedRecords    = 2;
  adFilterFetchedRecords     = 3;
  adFilterConflictingRecords = 5;


