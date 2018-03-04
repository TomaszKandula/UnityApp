{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
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

  { SPECIAL CHARACTERS }

  CRLF         : string = #13#10;
  COMMA        : char   = #44;
  SPACE        : char   = #32;
  TAB          : char   = #9;
  QUOTE        : char   = #39;

  { SQL MODEL CONSTANTS }

  INSERT       : string = 'INSERT INTO';
  SELECT       : string = 'SELECT';
  UPDATE       : string = 'UPDATE';
  _SET         : string = 'SET';
  UNION        : string = 'UNION ALL';

  { ALL MAPPED TABLES AND COLUMNS IN DATABASE }

  tbl_daily       = 'tbl_daily';

  tbl_general     = 'tbl_general';

  tbl_company     = 'tbl_company';

  tbl_managers    = 'tbl_managers';

  tbl_teamleaders = 'tbl_teamleaders';

  tbl_invoices    = 'tbl_invoices';

  tbl_tracker     = 'tbl_tracker';

  tbl_group3      = 'tbl_group3';

  tbl_paidinfo    = 'tbl_paidinfo';

  tbl_pmtterms    = 'tbl_pmtterms';

  tbl_person      = 'tbl_person';

  tbl_UAC         = 'tbl_UAC';

  tbl_groups      = 'tbl_groups';

  tbl_addressbook = 'tbl_addressbook';

  tbl_snapshots   = 'tbl_snapshots';


