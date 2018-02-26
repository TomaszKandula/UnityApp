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
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Main;  (* !!! CHANGE ALL 'VARTOSTR' TO 'OLEGETSTR' BEFORE RELEASE !!! *)      //rafactor!!!

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, Grids, ExtCtrls, StdCtrls, CheckLst, Buttons, PNGimage,
  DBGrids, AppEvnts, ShellAPI, INIFiles, StrUtils, ValEdit, DateUtils, Clipbrd, DB, ADODB, ActiveX, CDO_TLB, Diagnostics, GIFImg, Math, Wininet, ComObj,
  OleCtrls, SHDocVw,

  (* OWN LIBRARY *)

  Coder,

  (* ARARAT SYNAPSE FOR E-MAILER *)
  (* MODIFIED FROM ORIGINAL:     *)
  (*  'SMTPSEND'                 *)
  (*  'MIMEMESS'                 *)

  blcksock,
  smtpsend,
  pop3send,
  ssl_openssl,
  synautil,
  synacode,
  mimemess;

{ --------------------------------------------------------- ! COMMON APPLICATION CONSTANTS ! ---------------------------------------------------------------- }
const
  WM_GETINFO = WM_USER + 120;
  WM_EXTINFO = WM_APP  + 150;
  EX_LIBRARY = 'Unitylib.dll';

{ ---------------------------------------------------------- ! REFERENCE TO M.DIM. ARRAY ! ------------------------------------------------------------------ }
type
  TStrArray = array of array of string;

{ ----------------------------------------------------------- ! 'TSTRINGGRID' REFERENCE ! ------------------------------------------------------------------- }
type
  { WE USE IT TO DELETE SELECTED ROWS FROM STRING GRID COMPONENT }
  TAbstractGrid = class(Grids.TStringGrid);

{ ------------------------------------------------------------ ! INTERPOSER CLASSES ! ----------------------------------------------------------------------- }
                                                    (* EXTEND CURRENT COMPONENTS | MAIN THREAD *)

{ -------------------------------------------------------------- ! TSHAPE CLASS ! --------------------------------------------------------------------------- }
type
  TShape = class(ExtCtrls.TShape)
  protected
    procedure Paint; override;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
  published
    property  Caption;
    property  Font;
    procedure ShapeText(Left, Top: integer; StrText: string; Format: TFontStyles);
  public
    var CaptionLeft : integer;
    var CaptionTop  : integer;
  end;

{ -------------------------------------------------------------- ! TPANEL CLASS ! --------------------------------------------------------------------------- }
type
  TPanel = class(ExtCtrls.TPanel)
  protected
    procedure Paint; override;
  published
    procedure PanelBorders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);
  public
    var PenWidthTop   :  integer;
    var PenWidthBottom:  integer;
    var PenWidthLeft  :  integer;
    var PenWidthRight :  integer;
    var PenColorTop   :  TColor;
    var PenColorBottom:  TColor;
    var PenColorLeft  :  TColor;
    var PenColorRight :  TColor;
    var mcBrushColor  :  TColor;
  end;

{ ------------------------------------------------------------- ! TSTRINGGRID CLASS ! ----------------------------------------------------------------------- }
type
  TStringGrid = class(Grids.TStringGrid)
  published
    procedure CopyCutPaste(mode: integer);
    procedure DelEsc(mode: integer; pCol, pRow: integer);
    procedure ClearAll(dfRows: integer; FixedRows: integer; FixedCols: integer; ZeroCol: boolean);
    procedure DeleteRowFrom(FixedRow: integer; FixedCol: integer);
    procedure DrawSelected(ARow: integer; ACol: integer; State: TGridDrawState; Rect: TRect; FontColorSel: TColor; BrushColorSel: TColor; FontColor: TColor; BrushColor: TColor; Headers: boolean);
    procedure ColorValues(ARow: integer; ACol: integer; Rect: TRect; NegativeColor: TColor; PositiveColor: TColor);
    procedure SetColWidth(FirstDefault: integer; AddSpace: integer);
	  procedure MSort(const SortCol, datatype: integer; const ascending: boolean);
    procedure AutoThumbSize;
    procedure SaveLayout(ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string);
    function  LoadLayout(var StrCol: string; ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string): boolean;
    function  ReturnColumn(ColumnName: string; FixedCol: integer; FixedRow: integer): integer;
    function  ToExcel(ASheetName, AFileName: string; idThd: integer): Boolean;
    procedure Freeze(PaintWnd: boolean);
  public
    var SqlColumns : TStrArray;
  end;

{ --------------------------------------------------------- ! APPLICATION SETTING CLASS ! ------------------------------------------------------------------- }
type                                                              (* ANY THREAD *)
  TSettings = class { SINGLE INSTANCE }
  {$TYPEINFO ON}

  (* PRIVATE APPLICATION VARIABLES *)

  private
    var RegionalSettings       : TFormatSettings;
    var pAppDir                : string;
    var pLayoutDir             : string;
    var pLogFile               : string;
    var pConfigFile            : string;
    var pLogonFile             : string;
    var pWinUserName           : string;
  public

    (* PUBLIC APPLICATION VARIABLES *)

    var  TMIP                  : TMemIniFile;
    var  TMIG                  : TMemIniFile;

    (* PUBLIC READ ONLY APPLICATION PROPERTIES *)

    property AppDir            : string read pAppDir;
    property LayoutDir         : string read pLayoutDir;
    property LogFile           : string read pLogFile;
    property ConfigFile        : string read pConfigFile;
    property LogonFile         : string read pLogonFile;
    property WinUserName       : string read pWinUserName;

    (* APPLICATION CONSTANTS *)

    const  APPNAME             = 'Unity';
    const  LICFILE             = 'Unity.lic';
    const  FILEKEY             = 429496;
    const  SELCOLOR            = $00FFDBB7;
    const  FONCOLOR            = $006433C9;
    const  ApplicationDetails  = 'APPLICATION';
    const  Password            = 'PASSWD';
    const  TabSheetsNames      = 'TABSHEETS_NAMES';
    const  RiskClassDetails    = 'RISK_CLASS_DETAILS';
    const  MailerCDOSYS        = 'MAILER_CDOSYS_NTLM';
    const  MailerSYNAPSE       = 'MAILER_SYNAPSE';
    const  MailerSetup         = 'MAILER_SETUP';
    const  DatabaseSetup       = 'DATABASE_SETTINGS';
    const  OpenItemsData       = 'OPEN_ITEMS';
    const  AddressBookData     = 'ADDRESS_BOOK';
    const  GeneralTables       = 'GENERAL_TABLES';
    const  InvoiceTypes        = 'INVOICE_TYPES';
    const  TabSheetsCaps       = 'TABSHEETS_CAPTIONS';
    const  AgingRanges         = 'AGEVIEW_RANGES';
    const  AgingBasic          = 'AGEVIEW_BASIC';
    const  AgingFull           = 'AGEVIEW_FULL';
    const  Unallocated         = 'UNALLOCATED_DEFINITION';
    const  VariousLayouts      = 'LAYOUTS';
    const  TimersSettings      = 'TIMERS_INTERVALS';
    const  ColumnPrefix        = 'COLUMN';
    const  ColumnWidthName     = 'COLUMNWIDTH';
    const  ColumnOrderName     = 'COLUMNORDER';
    const  ColumnNames         = 'COLUMNNAMES';
  published

    (* INITIALIZATION *)

    constructor Create(Caption: string);
  end;

{ --------------------------------------------------------------- ! DATABASE CLASS ! ------------------------------------------------------------------------ }
type                                                  (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TDataBase = class
  {$TYPEINFO ON}
  private
    pdbProvider  : string;   { DATABASE PROVIDER                            }
    pdbConnStr   : string;   { DATABASE CONNECTION STRING                   }
    pLastError   : integer;  { LAST OCCURED ERROR CODE                      }
    pAccessLevel : string;   { RO = READ ONLY | RW = READ/WRITE | AD = ALL  }
    pAccessMode  : string;   { BASIC = HIDE SPECIFIC DATA | FULL = SHOW ALL }
  public
    var         ADOConnect   : TADOConnection;
    var         ArrGroupList : array of array of string;
    property    dbProvider   : string  read pdbProvider  write pdbProvider;
    property    dbConnStr    : string  read pdbConnStr   write pdbConnStr;
    property    LastError    : integer read pLastError   write pLastError;
    property    AccessLevel  : string  read pAccessLevel write pAccessLevel;
    property    AccessMode   : string  read pAccessMode  write pAccessMode;
  published
    constructor Create;
    procedure   InitializeConnection(idThd: integer; ErrorShow: boolean);
    function    AssignConnString(Print: boolean): boolean;
    function    Check: boolean;
    function    UACinitialize: boolean;                                              { RUN IN MAIN THREAD ONLY }
    procedure   UACGroupReader(UACuser: string);                                     { RUN EITHER IN WORKER OR MAIN THREAD }
    procedure   UACAgeDates(StrGroupID: string);                                     { RUN EITHER IN WORKER OR MAIN THREAD }
    function    UACString(ListPos: integer; CoPos: integer; Mode: integer): string;  { RUN EITHER IN WORKER OR MAIN THREAD }
  end;

{ -------------------------------------------------------------- ! MAILER CLASS ! --------------------------------------------------------------------------- }
type                                                      (* RUN IN WORKER THREAD ONLY *)
  TMailer = class { BASE CLASS }
  {$TYPEINFO ON}
  private
    pidThd     : integer;
    pXMailer   : string;
    pmFrom     : string;
    pmTo       : string;
    pmCc       : string;
    pmBcc      : string;
    pmRt       : string;
    pmSubject  : string;
    pmBody     : string;
    pmLogo     : string;
  public
    property    idThd       : integer read pidThd    write pidThd;
    property    XMailer     : string  read pXMailer  write pXMailer;
    property    MailFrom    : string  read pmFrom    write pmFrom;
    property    MailTo      : string  read pmTo      write pmTo;
    property    MailCc      : string  read pmCc      write pmCc;
    property    MailBcc     : string  read pmBcc     write pmBcc;
    property    MailRt      : string  read pmRt      write pmRt;
    property    MailSubject : string  read pmSubject write pmSubject;
    property    MailBody    : string  read pmBody    write pmBody;
    property    Logo        : string  read pmLogo    write pmLogo;
  published
    constructor Create;
    function    SendCDOSYS : boolean;
    function    SendSynapse: boolean;
    function    SendNow    : boolean;
  end;

{ ---------------------------------------------------------- ! INVOICE TRACKER CLASS ! ---------------------------------------------------------------------- }
type                                                       (* RUN IN WORKER THREAD ONLY *)
  TInvoiceTracker = class(TMailer)
  {$TYPEINFO ON}
  published
    constructor Create;
    procedure   Scanner(idThd: integer);
    procedure   Refresh(var SG: TStringGrid; Param: string);
  end;

{ ----------------------------------------------------------- ! ADDRESS BOOK CLASS ! ------------------------------------------------------------------------ }
type
  TAddressBook = class //refactor!!
  {$TYPEINFO ON}
  private
    pidThd      : integer;
    pDelimiter  : string;
    pUserName   : string;
  public
    property    idThd      : integer read pidThd     write pidThd;
    property    Delimiter  : string  read pDelimiter write pDelimiter;
    property    UserName   : string  read pUserName  write pUserName;
  published
    constructor Create(UseDelimiter: string; UserNameDef: string);
    function    Read     : boolean; //refactor!!
    function    Write    : boolean; //refactor!!
    function    ImportCSV: boolean;
    function    ExportCSV: boolean;
  end;

{ ------------------------------------------------------------- ! AGE VIEW CLASS ! -------------------------------------------------------------------------- }
type                                                   (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TAgeView = class(TObject)
  {$TYPEINFO ON}
  public
    (* VARIABLES UPDATED BY WORKER THREAD AND USED BY MAIN THREAD *)
    ArrAgeView   : TStrArray;
    CustAll      : integer;
    CallsAll     : integer;
    EmailsAll    : integer;
    NotDue       : extended;
    Range1       : extended;
    Range2       : extended;
    Range3       : extended;
    Range4       : extended;
    Range5       : extended;
    Range6       : extended;
    Balance      : extended;
    Limits       : extended;
    Exceeders    : integer;
    TotalExceed  : extended;
    RCA          : extended;
    RCB          : extended;
    RCC          : extended;
  published
    (* RUN IN WORKER THREAD ONLY *)
    procedure   Read(GroupID: string; AgeDate: TDateTime; idThd: integer);                     //refactor!!
    procedure   Make(GroupID: string; OSAmount: double; idThd: integer);                       //ok
    procedure   Write(DestTable: string; idThd: integer);                                      //refactor!!
    procedure   Details(GroupID: string; AgeDate: TDateTime; idThd: integer);                  //refactor!!
    function    MapData(AgeGrid: TStringGrid; WhichCol: string; tblMap: TStringGrid): string;  //refactor!!
  end;

{ ------------------------------------------------------------- ! OPEN ITEMS CLASS ! ------------------------------------------------------------------------ }
type                                                  (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TOpenItems = class(TObject)
  {$TYPEINFO ON}
  public
    (* VARIABLES UPDATED BY WORKER THREAD AND USED BY MAIN THREAD *)
    OpenItemsDir  : string;                      { PATH TO OPEN ITEMS DIRECTORY                  }
    OpenItemsFor  : string;                      { FORMAT OF FILE WITH OPEN ITEMS                }
    FileExist     : boolean;                     { INDICATE IF SOURCE FILE FOR OPEN ITEMS EXISTS }
    ArrOpenItems  : array of array of string;    { INFORMATION ON SOURCES OF OPEN ITEMS          }        // to settings?
    nInvoices     : integer;
    Overdue       : integer;
    OSamt         : double;
    UNamt         : double;
    OverdueAmt    : double;
    cDiscountedAmt: double;
    cDecreaseAmt  : double;
    cRecoveryAmt  : double;
    KPI_overdue   : double;
    KPI_unalloc   : double;
  published
    procedure   Load(idThd: integer);            { RUN IN WORKER THREAD ONLY }         //ok
    function    Scan(mode: integer): boolean;                                          //ok
    function    ConvertName(CoNumber: string; Prefix: string; mode: integer): string;  //ok
    function    ReturnKPI(SG: TStringGrid; StrCoCode: string; mode: integer): double;  //refactor!!
  end;

{ ----------------------------------------------------------------- ! MAIN CLASS ! -------------------------------------------------------------------------- }
type                                                            (* GUI | MAIN THREAD *)
  TMainForm = class(TForm)
    MyPages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet7: TTabSheet;
    Header1: TPanel;
    Footer1: TPanel;
    hShapeGen: TShape;
    Text01: TLabel;
    Text02: TLabel;
    Text06: TLabel;
    tcCOCODE: TLabel;
    tcCURRENCY: TLabel;
    tcTOTAL: TLabel;
    Cap01: TShape;
    hShapeSet: TShape;
    Cap02: TShape;
    hShapeAge: TShape;
    Cap05: TShape;
    Text09: TLabel;
    tR1: TLabel;
    tR2: TLabel;
    tR3: TLabel;
    tR4: TLabel;
    tR5: TLabel;
    Text08: TLabel;
    procND: TLabel;
    procR1: TLabel;
    procR2: TLabel;
    procR3: TLabel;
    procR4: TLabel;
    procR5: TLabel;
    valND: TLabel;
    valR1: TLabel;
    valR2: TLabel;
    valR3: TLabel;
    valR4: TLabel;
    valR5: TLabel;
    tTAMT: TLabel;
    valTAMT: TLabel;
    procTAMT: TLabel;
    Text10: TLabel;
    Text11: TLabel;
    Text12: TLabel;
    hShapeAct: TShape;
    Cap03: TShape;
    GridFillerTop1: TImage;
    hShapeTC: TShape;
    Cap06: TShape;
    Text17: TLabel;
    Text18: TLabel;
    Text19: TLabel;
    hShapeTR: TShape;
    Cap07: TShape;
    Text20: TLabel;
    Text21: TLabel;
    Text22: TLabel;
    valExceeders: TLabel;
    valTEXCEES: TLabel;
    valTLIMITS: TLabel;
    valTND: TLabel;
    valPASTDUE: TLabel;
    valDEFAULTED: TLabel;
    valRISKA: TLabel;
    valRISKB: TLabel;
    valRISKC: TLabel;
    GridFillerBottom: TImage;
    Shape1: TShape;
    StatBar_CAP1: TLabel;
    StatBar_TXT1: TLabel;
    StatBar_CAP2: TLabel;
    StatBar_TXT2: TLabel;
    TabSheet8: TTabSheet;
    PopupMenu: TPopupMenu;
    Action_HideApp: TMenuItem;
    Action_ShowApp: TMenuItem;
    Action_Close: TMenuItem;
    N1: TMenuItem;
    Action_Help: TMenuItem;
    Action_About: TMenuItem;
    N2: TMenuItem;
    Action_OnTop: TMenuItem;
    N3: TMenuItem;
    TabSheet6: TTabSheet;
    sgOpenItems: TStringGrid;
    Header3: TPanel;
    sgAddressBook: TStringGrid;
    Header8: TPanel;
    Edit_PASSWORD: TEdit;
    btnUnlock: TButton;
    Cap21: TShape;
    hShapeCred: TShape;
    Text32: TLabel;
    Text33: TLabel;
    hShapePass: TShape;
    Cap22: TShape;
    btnPassUpdate: TButton;
    Text37: TLabel;
    Text38: TLabel;
    Text39: TLabel;
    Edit_CurrPassWd: TEdit;
    Edit_NewPassWd: TEdit;
    Edit_ConfPassWd: TEdit;
    ShapeList: TShape;
    imgKeyAdd: TImage;
    imgKeyRemove: TImage;
    imgUpdateValues: TImage;
    Text41: TLabel;
    Text42: TLabel;
    Text43: TLabel;
    sgListValue: TStringGrid;
    sgListSection: TStringGrid;
    imgSectionAdd: TImage;
    imgSectionRemove: TImage;
    Text48: TLabel;
    Text49: TLabel;
    Cap23: TShape;
    Text44: TLabel;
    hShapeActionPAB: TShape;
    Cap13: TShape;
    imgOFF: TImage;
    GridFillerTop3: TImage;
    Header2: TPanel;
    hShapeActionsVOI: TShape;
    Cap10: TShape;
    hShapeSettingsVOI: TShape;
    Cap11: TShape;
    hShapeDetailsVOI: TShape;
    Cap12: TShape;
    GridFillerTop2: TImage;
    btnReload: TImage;
    Text54L1: TLabel;
    btnOpenAB: TImage;
    btnSave: TImage;
    btnClose: TImage;
    Text64: TLabel;
    Text66: TLabel;
    Text67: TLabel;
    Text63: TLabel;
    COC1: TEdit;
    Text55: TLabel;
    Text56: TLabel;
    Text57: TLabel;
    Text60: TLabel;
    Text59: TLabel;
    Text58: TLabel;
    tcOpenItems: TLabel;
    tcInvoices: TLabel;
    tcOSAmt: TLabel;
    tcOverdue: TLabel;
    tcDecAmt: TLabel;
    tcRecovery: TLabel;
    Text68: TLabel;
    btnImport: TImage;
    btnExport: TImage;
    Text69: TLabel;
    GridFillerTop8: TImage;
    Header4: TPanel;
    hShapeInfoAM: TShape;
    Cap43: TShape;
    GridFillerTop4: TImage;
    Header5: TPanel;
    ShapeContent5: TShape;
    Cap51: TShape;
    GridFillerTop5: TImage;
    Header6: TPanel;
    ShapeContent6: TShape;
    Cap61: TShape;
    GridFillerTop6: TImage;
    Header7: TPanel;
    hShapeInfoGT: TShape;
    Cap15: TShape;
    GridFillerTop7: TImage;
    Text54L2: TLabel;
    COC2: TEdit;
    COC3: TEdit;
    COC4: TEdit;
    MainShape2: TPanel;
    MainShape6: TPanel;
    AppFooter: TPanel;
    Shape2: TShape;
    Shape3: TShape;
    StatBar_CAP3: TLabel;
    StatBar_TXT3: TLabel;
    Shape4: TShape;
    StatBar_CAP4: TLabel;
    StatBar_TXT4: TLabel;
    Shape5: TShape;
    StatBar_CAP5: TLabel;
    StatBar_TXT5: TLabel;
    Shape7: TShape;
    CurrentTime: TTimer;
    UpTime: TTimer;
    txtInfo1: TLabel;
    txtInfo3: TLabel;
    txtInfo2: TLabel;
    Cap16: TShape;
    Cap17: TShape;
    Cap18: TShape;
    CSVExport: TSaveDialog;
    CSVImport: TOpenDialog;
    Text77: TLabel;
    tcDisAmt: TLabel;
    Cap19: TShape;
    Cap20: TShape;
    LeftPanel: TPanel;
    ContentPanel7: TPanel;
    RightPanel: TPanel;
    MidPanel: TPanel;
    ContentPanel6: TPanel;
    ContentPanel5: TPanel;
    ContentPanel1: TPanel;
    ContentPanel: TPanel;
    ContentPanel2: TPanel;
    MainPanel1: TPanel;
    BottomPanel1: TPanel;
    ContentPanel8: TPanel;
    InnerPanel8Left: TPanel;
    tcUNAmt: TLabel;
    Text70: TLabel;
    Text72: TLabel;
    Text79: TLabel;
    Text73: TLabel;
    CUR1: TEdit;
    CUR2: TEdit;
    CUR3: TEdit;
    CUR4: TEdit;
    INT1: TEdit;
    INT2: TEdit;
    INT3: TEdit;
    INT4: TEdit;
    tcOverdueRatio: TLabel;
    OILoader: TTimer;
    Text80: TLabel;
    Text81: TLabel;
    tcKPIoverdue: TLabel;
    tcKPIunalLocated: TLabel;
    Text82: TLabel;
    tcOvdAmt: TLabel;
    GroupListBox: TComboBox;
    GroupListDates: TComboBox;
    Text31: TLabel;
    Text34: TLabel;
    Text35: TLabel;
    procRISKA: TLabel;
    procRISKB: TLabel;
    procRISKC: TLabel;
    tcNumCalls: TLabel;
    tcNumEmails: TLabel;
    Text36: TLabel;
    GroupName: TLabel;
    btnMakeGroup: TImage;
    Text83L1: TLabel;
    Text83L2: TLabel;
    Label1: TLabel;
    AGT1: TEdit;
    AGT2: TEdit;
    AGT3: TEdit;
    AGT4: TEdit;
    PanelGroupName: TPanel;
    btnMakeGroupAge: TSpeedButton;
    EditGroupName: TLabeledEdit;
    ReloadCover: TImage;
    sgAgeView: TStringGrid;
    cbDump: TCheckBox;
    sgInvoiceTracker: TStringGrid;
    AgeViewPopup: TPopupMenu;
    Action_Tracker: TMenuItem;
    N4: TMenuItem;
    Action_PaymentTerm: TMenuItem;
    Action_Group3: TMenuItem;
    Action_Person: TMenuItem;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TrackerPopup: TPopupMenu;
    Action_Remove: TMenuItem;
    Action_ShowMy: TMenuItem;
    Action_ShowAll: TMenuItem;
    Action_LyncCall: TMenuItem;
    TrayIcon: TTrayIcon;
    InvoiceScanTimer: TTimer;
    Action_ShowRegistered: TMenuItem;
    N8: TMenuItem;
    InetTimer: TTimer;
    StatBar_TXT7: TLabel;
    InnerPanel8Right: TPanel;
    EventLog: TMemo;
    Cap27: TShape;
    EventLogTimer: TTimer;
    N9: TMenuItem;
    Action_FilterINF7: TMenuItem;
    UpdaterTimer: TTimer;
    N5: TMenuItem;
    N7: TMenuItem;
    N6: TMenuItem;
    Action_AddToBook: TMenuItem;
    Text50: TLabel;
    imgAllowEdit: TImage;
    SplitLine1: TBevel;
    sgCoCodes: TStringGrid;
    sgPaidInfo: TStringGrid;
    sgPerson: TStringGrid;
    sgGroup3: TStringGrid;
    sgPmtTerms: TStringGrid;
    Action_AutoColumnSize: TMenuItem;
    Action_INF4: TMenuItem;
    btnOpenGroup: TSpeedButton;
    EventReload: TImage;
    InnerPanelTop: TPanel;
    SplitLine2: TBevel;
    Action_Search: TMenuItem;
    BookPopup: TPopupMenu;
    Action_Copy: TMenuItem;
    Action_Paste: TMenuItem;
    Action_Cut: TMenuItem;
    Action_DelRow: TMenuItem;
    N10: TMenuItem;
    Action_AddRow: TMenuItem;
    N11: TMenuItem;
    Action_ShowAsIs: TMenuItem;
    Action_ShowMyEntries: TMenuItem;
    Action_ToExce: TMenuItem;
    N12: TMenuItem;
    Action_BasicView: TMenuItem;
    N13: TMenuItem;
    Action_FullView: TMenuItem;
    XLExport: TSaveDialog;
    tR6: TLabel;
    valR6: TLabel;
    procR6: TLabel;
    N14: TMenuItem;
    Action_SearchBook: TMenuItem;
    Action_Overdue: TMenuItem;
    N15: TMenuItem;
    TabSheet9: TTabSheet;
    WebBrowser: TWebBrowser;
    WebContainer: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Action_HideAppClick(Sender: TObject);
    procedure Action_ShowAppClick(Sender: TObject);
    procedure Action_HelpClick(Sender: TObject);
    procedure Action_AboutClick(Sender: TObject);
    procedure Action_OnTopClick(Sender: TObject);
    procedure btnUnlockClick(Sender: TObject);
    procedure TabSheet8Show(Sender: TObject);
    procedure btnPassUpdateClick(Sender: TObject);
    procedure imgKeyAddMouseEnter(Sender: TObject);
    procedure imgKeyAddMouseLeave(Sender: TObject);
    procedure imgKeyRemoveMouseEnter(Sender: TObject);
    procedure imgKeyRemoveMouseLeave(Sender: TObject);
    procedure imgUpdateValuesMouseEnter(Sender: TObject);
    procedure imgUpdateValuesMouseLeave(Sender: TObject);
    procedure imgSectionAddMouseEnter(Sender: TObject);
    procedure imgSectionAddMouseLeave(Sender: TObject);
    procedure imgSectionRemoveMouseEnter(Sender: TObject);
    procedure imgSectionRemoveMouseLeave(Sender: TObject);
    procedure sgListSectionSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure imgSectionRemoveClick(Sender: TObject);
    procedure imgKeyRemoveClick(Sender: TObject);
    procedure imgSectionAddClick(Sender: TObject);
    procedure imgKeyAddClick(Sender: TObject);
    procedure imgUpdateValuesClick(Sender: TObject);
    procedure sgAddressBookMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgAddressBookMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgOpenItemsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgOpenItemsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure btnReloadMouseEnter(Sender: TObject);
    procedure btnReloadMouseLeave(Sender: TObject);
    procedure btnOpenABMouseEnter(Sender: TObject);
    procedure btnOpenABMouseLeave(Sender: TObject);
    procedure btnSaveMouseEnter(Sender: TObject);
    procedure btnSaveMouseLeave(Sender: TObject);
    procedure btnCloseMouseEnter(Sender: TObject);
    procedure btnCloseMouseLeave(Sender: TObject);
    procedure btnImportMouseEnter(Sender: TObject);
    procedure btnImportMouseLeave(Sender: TObject);
    procedure btnExportMouseEnter(Sender: TObject);
    procedure btnExportMouseLeave(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure sgListSectionKeyPress(Sender: TObject; var Key: Char);
    procedure sgListValueKeyPress(Sender: TObject; var Key: Char);
    procedure sgListValueClick(Sender: TObject);
    procedure sgListSectionClick(Sender: TObject);
    procedure Edit_PASSWORDKeyPress(Sender: TObject; var Key: Char);
    procedure COC1KeyPress(Sender: TObject; var Key: Char);
    procedure COC2KeyPress(Sender: TObject; var Key: Char);
    procedure COC3KeyPress(Sender: TObject; var Key: Char);
    procedure CurrentTimeTimer(Sender: TObject);
    procedure UpTimeTimer(Sender: TObject);
    procedure sgListSectionMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgListSectionMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgListValueMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgListValueMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgListSectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenABClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure COC4KeyPress(Sender: TObject; var Key: Char);
    procedure TabSheet7Show(Sender: TObject);
    procedure TabSheet7Resize(Sender: TObject);
    procedure sgCoCodesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgCoCodesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPmtTermsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPmtTermsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgGroup3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgGroup3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPaidInfoMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPaidInfoMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPersonMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPersonMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure OILoaderTimer(Sender: TObject);
    procedure GroupListBoxSelect(Sender: TObject);
    procedure btnMakeGroupClick(Sender: TObject);
    procedure btnMakeGroupMouseEnter(Sender: TObject);
    procedure btnMakeGroupMouseLeave(Sender: TObject);
    procedure CUR1KeyPress(Sender: TObject; var Key: Char);
    procedure CUR2KeyPress(Sender: TObject; var Key: Char);
    procedure CUR3KeyPress(Sender: TObject; var Key: Char);
    procedure CUR4KeyPress(Sender: TObject; var Key: Char);
    procedure INT1KeyPress(Sender: TObject; var Key: Char);
    procedure INT2KeyPress(Sender: TObject; var Key: Char);
    procedure INT3KeyPress(Sender: TObject; var Key: Char);
    procedure INT4KeyPress(Sender: TObject; var Key: Char);
    procedure AGT1KeyPress(Sender: TObject; var Key: Char);
    procedure AGT2KeyPress(Sender: TObject; var Key: Char);
    procedure AGT3KeyPress(Sender: TObject; var Key: Char);
    procedure AGT4KeyPress(Sender: TObject; var Key: Char);
    procedure btnMakeGroupAgeClick(Sender: TObject);
    procedure EditGroupNameKeyPress(Sender: TObject; var Key: Char);
    procedure sgAgeViewMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgAgeViewMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgInvoiceTrackerMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgInvoiceTrackerMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure Action_TrackerClick(Sender: TObject);
    procedure Action_PaymentTermClick(Sender: TObject);
    procedure Action_Group3Click(Sender: TObject);
    procedure Action_PersonClick(Sender: TObject);
    procedure Action_RemoveClick(Sender: TObject);
    procedure Action_ShowMyClick(Sender: TObject);
    procedure Action_ShowAllClick(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
    procedure Action_LyncCallClick(Sender: TObject);
    procedure InvoiceScanTimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrayIconDblClick(Sender: TObject);
    procedure InetTimerTimer(Sender: TObject);
    procedure EventLogTimerTimer(Sender: TObject);
    procedure Action_CloseClick(Sender: TObject);
    procedure Action_FilterINF7Click(Sender: TObject);
    procedure sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure UpdaterTimerTimer(Sender: TObject);
    procedure sgAgeViewDblClick(Sender: TObject);
    procedure Action_ShowRegisteredClick(Sender: TObject);
    procedure sgInvoiceTrackerDblClick(Sender: TObject);
    procedure Action_AddToBookClick(Sender: TObject);
    procedure sgAddressBookDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgInvoiceTrackerDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgListSectionDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgListValueDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure imgAllowEditMouseEnter(Sender: TObject);
    procedure imgAllowEditMouseLeave(Sender: TObject);
    procedure imgAllowEditClick(Sender: TObject);
    procedure sgAddressBookDblClick(Sender: TObject);
    procedure sgAddressBookClick(Sender: TObject);
    procedure AgeViewPopupPopup(Sender: TObject);
    procedure sgCoCodesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgPaidInfoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgPmtTermsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgPersonDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgGroup3DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure Action_AutoColumnSizeClick(Sender: TObject);
    procedure Action_INF4Click(Sender: TObject);
    procedure btnOpenGroupClick(Sender: TObject);
    procedure sgCoCodesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgPaidInfoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgPmtTermsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgPersonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgGroup3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgInvoiceTrackerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgOpenItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EventReloadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Action_SearchClick(Sender: TObject);
    procedure Action_CutClick(Sender: TObject);
    procedure Action_CopyClick(Sender: TObject);
    procedure Action_PasteClick(Sender: TObject);
    procedure Action_DelRowClick(Sender: TObject);
    procedure BookPopupPopup(Sender: TObject);
    procedure Action_AddRowClick(Sender: TObject);
    procedure Action_ShowAsIsClick(Sender: TObject);
    procedure Action_ShowMyEntriesClick(Sender: TObject);
    procedure sgAddressBookSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Action_ToExceClick(Sender: TObject);
    procedure Action_BasicViewClick(Sender: TObject);
    procedure Action_FullViewClick(Sender: TObject);
    procedure sgAgeViewColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
    procedure Action_SearchBookClick(Sender: TObject);
    procedure Action_OverdueClick(Sender: TObject);
  public
    procedure DebugMsg(const Msg: String);

    function  OleGetStr(RecordsetField: variant): string;
    function  FindKey(INI: TMemIniFile; OpenedSection: string; KeyPosition: integer): string;

    function  WndCall(WinForm: TForm; Mode: integer): integer;
    function  MsgCall(WndType: integer; WndText: string): integer;

    procedure LockSettingsPanel;

  protected

    procedure WndProc(var msg: Messages.TMessage); override;

  end;

{ ----------------------------------------------------------------------------------------------------------------------------------- POINTERS TO DLL IMPORTS }

{   IF LIBRARY IS WRITTEN IN DELPHI, THEN DELPHI TYPES CAN BE USED AS USUAL, HOWEVER, IF LIBRARY IS WRITTEN IN 'C' OR ANY OTHER LANGUAGE, THEN                }
{   PLEASE USE PLAIN 'C' LANGUAGE TYPES ONLY, SO INSTEAD OF PASCAL 'STRING' TYPE, PLEASE USE 'PCHAR' TYPE, ETC., ALSO, IN CASE OF C# LANGUAGE,                }
{   PLEASE REFER TO MANUAL ON 'MAKING C# DLL LIBRARY FOR DELPHI USAGE'                                                                                        }

TLogText              = procedure(filename: string; text: string); stdcall;
TMergeSort            = procedure(Grid: TStringgrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall;
TGetOSVer             = function(mode: integer): string; stdcall;
TGetCurrentUserSid    = function: string stdcall;
TGetBuildInfoAsString = function: string stdcall;

{ ------------------------------------------------------------ ! STATIC DLL IMPORTS ! ----------------------------------------------------------------------- }

function  GetCurrentUserSid: string; stdcall; external EX_LIBRARY;
function  GetOSVer(mode: integer): string; stdcall; external EX_LIBRARY;
function  GetBuildInfoAsString: string; stdcall; external EX_LIBRARY;
procedure LogText(filename: string; text: string); stdcall; external EX_LIBRARY;
procedure MergeSort(grid: TStringgrid; var vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall; external EX_LIBRARY;

var
  MainForm        : TMainForm;       { MAIN FORM | MAIN THREAD | GUI     }
  Settings        : TSettings;       { KEEP ALL THE APPLICATION SETTINGS }
  DataBase        : TDataBase;       { ESTABLISH DATABASE CONNECTION     }

  AgeView         : TAgeView;
  OpenItems       : TOpenItems;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Filter, Tracker, Invoices, Actions, Calendar, About, Search, Worker, Model, SQL;

var
  { --------------------------------------------------------------- ! GENERAL ! ----------------------------------------------------------------------------- }

  AllowClose      :  boolean;        { SIGNAL FORCE CLOSE WHEN WINDOWS IS SHUTTING DOWN     }
  Error           :  integer;        { RETURNS ERROR CODE                                   }
  WndMode         :  string;         { WINDOW STATE: MAXIMIZED OR NORMAL                    }
  cDateTime       :  TDateTime;      { CURRENT DATE AND TIME | REFRESH TIME EACH SECOND     }
  StartTime       :  TDateTime;      { APPLICATION START TIME                               }

{$R *.dfm}

{ ---------------------------------------------------------------- ! DEBUGGING ! ---------------------------------------------------------------------------- }
procedure TMainForm.DebugMsg(const Msg: String);
begin
  OutputDebugString(PChar(Msg));
end;

(* ******************************************************** ! MESSAGES BETWEEN THREADS ! **********************************************************************

IMPORTANT NOTE:
---------------

TMESSAGE CARRY TWO PARAMETERS (AMONG OTHERS) THAT WE USE MOSTLY:
  WPARAM = DIGIT
  LPARAM = STRING

THE APPLICATION GUI CAN BE HANDLED BY THE MAIN THREAD ONLY. THUS, WORKER THREAD SHOULD NOT MESS UP WITH THE VCL'S. IF THE WORKER THREAD REQUIRE
ACCESS TO VISUAL COMPONENT TO UPDATE THE DATA (I.E. SHOWING PROGRESS BAR DURING ONGOING CALCULATION), THEN SYNCHRONISATION BETWEEN MAIN THREAD AND WORKER
THREAD IS NECESSARY. PLEASE NOTE THAT SYNCHRONIZED METHOD IS RUN IN THE MAIN THREAD, THEREFORE, FOR HEAVY DUTY TASK, IT WILL FREEZE APPLICATION UNTIL
THE CALCULATION IS DONE, IT IS STRONGLY ADVISED NOT TO SYNCHRONIZE HEAVY DUTY TASKS, BUT RATHER USE SYNCHRONIZATION TO UPDATE VCL WHILE THE COMPUTATION
IS DONE BY THE WORKER THREAD.

NOTED EXCEPTION:
----------------

IF VCL DRAWING IS DISABLED AND THERE IS NO COMPLEX CODE UNDERNEATH THE COMPONENT, BUT RATHER PROPERTIES, VARIABLES, THEN WORKER THREAD CAN DO ANYTHING
SAFELY WITH THE COMPONENT. ONCE THE WORK IS DONE, THE VCL DRAWING CAN BE ENABLED BACK AGAIN, AND THE MAIN THREAD CAN REPAINT VCL SAFELY, SHOWING UPDATED DATA
WITHIN THIS VCL.

TO PASS THE INFORMATION FROM THE WORKER THREAD TO THE MAIN THREAD, WE USE WINDOWS MESSAGES, AS BELOW.

WINDOWS API:
------------

  1. SEND MESSAGE - SYNCHRONOUS DELIVERY, WORKER THREAD MUST WAIT FOR MAIN THREAD.
  2. POST MESSAGE - ASYNCHRONOUS DELIVERY, RETURN RESULT IMMEDIATELY.

  USAGE:

  SENDMESSAGE(<FORM>.HANDLE, WM_GETINFO, <YOUR INTEGER>, LPARAM(PCHAR(<YOUR STRING>)));
  POSTMESSAGE(<FORM>.HANDLE, WM_GETINFO, <YOUR INTEGER>, LPARAM(PCHAR(<YOUR STRING>)));

  OR USE POINTER, THEN:

  VAR
    LOG: PSTRING;

  LOG^:='TEST';

  SENDMESSAGE(<CLASS>.HANDLE, WM_GETINFO, <YOUR INTEGER>, LPARAM(LOG));
  POSTMESSAGE(<CLASS>.HANDLE, WM_GETINFO, <YOUR INTEGER>, LPARAM(LOG));

  WARNING! DO NOT USE POSTMESSAGE TO CARRY LARGE CHUNK OF DATA.

************************************************************************************************************************************************************ *)

{ ----------------------------------------------------------------------------------------------------------------------- MESSAGE RECEIVER FOR WORKER THREADS }
procedure TMainForm.WndProc(var Msg: Messages.TMessage);
var
  DailyComment: TDaily;
begin
  inherited;
  { ------------------------------------------------------------------------------------------------ INTERNAL MESSAGES BETWEEN WORKER THREADS AND MAIN THREAD }
  if Msg.Msg = WM_GETINFO then
  begin
    { DEBUG LINE }
    DebugMsg('WM_GETINFO RECEIVED');
    { SHOW MESSEGAE WINDOW }
    if ( (Msg.WParam > 0) and (Msg.WParam <= 4) ) and (PChar(Msg.LParam) <> '') then MainForm.MsgCall(Msg.WParam, PChar(Msg.LParam));
    if (Msg.WParam >= 10) and (Msg.WParam <= 20) then
    begin
      { STATUS BAR CHANGES }
      if Msg.WParam = 10 then StatBar_TXT1.Caption:=PChar(Msg.LParam);
      { CONNECTION OK }
      if Msg.WParam = 14 then
      begin
        MainForm.StatBar_TXT7.Font.Style :=[];
        MainForm.StatBar_TXT7.Caption    :='Connected with Microsoft SQL Server.';
      end;
      { CONNECTION LOST }
      if Msg.WParam = 15 then
      begin
        MainForm.StatBar_TXT7.Font.Style :=[fsBold];
        MainForm.StatBar_TXT7.Caption    :='Connection lost, re-connecting...';
      end;
    end;
  end;
  { --------------------------------------------------------------------------------------------------------------- RECEIVE MESSAGE FROM EXTERNAL APPLICATION }
  if Msg.Msg = WM_EXTINFO then
  begin
    { IF WPARAM EQUALS '14' THEN WE EXPECT LPARAM TO RETURN }
    { PHONE CALL DURATION FROM 'LYNCCALL.EXE' APPLICATION   }
    if Msg.WParam = 14 then
      { DEBUG LINE }
      (* DebugMsg(IntToStr(Msg.LParam)); *)
      { LOG TIME IN SECONDS IN DATABASE }
(*
      if Msg.LParam > 0 then
      begin
        DailyComment:=TDaily.Create;
        try
          { ASSIGN CALLED CUSTOMER }
          DailyComment.idThd  :=MainThreadID;
          DailyComment.CUID   :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1), MainForm.sgAgeView.Row];
          DailyComment.AGEDATE:=MainForm.GroupListDates.Text;
          { READ DATA IF ANY }
          DailyComment.Read;
          { UPDATE AND SAVE }
          DailyComment.CALLEVENT   :=IntToStr(StrToInt(DailyComment.CALLEVENT) + 1);
          DailyComment.CALLDURATION:=IntToStr(Msg.LParam);
           DailyComment.Write;
        finally
          DailyComment.Free;
        end;
      end;
*)
  end;
  { --------------------------------------------------------------------------------------------------------------- CLOSE UNITY WHEN WINDOWS IS SHUTTING DOWN }

  (* *********************************************************************************************************************************************************

  WARNING!
  ========

  IF WE USE "ON CLOSE QUERY" AND PREVENT USER/WINDOWS FROM CLOSING APPLICATION BY CLICKING "X" (NEED USER ANSWER IF SHOULD BE CLOSED, ETC.), THEN WE SHOULD
  USE GLOBAL FLAG SET BY DEFAULT TO FALSE AND LISTENING TO WINDOWS MESSAGES. IF WINDOWS PASS "QUERY END SESSION" OR "END SESSION", THEN WE SHOULD SET THIS
  FLAG TO TRUE AND ALLOW TO CLOSE WHEN METHOD "ON CLOSE QUERY" IS CALLED. OTHERWISE, APPLICATION WILL NOT BE TO CLOSED (AWAITING USER RESPOND, ETC.)
  AND WINDOWS WILL DISPLAY NOTIFICATION THAT APPLICATION PREVENT WINDOWS FROM SHUTTING DOWN.

  ********************************************************************************************************************************************************** *)

  (* WINDOWS' QUERY FOR SHUTDOWN *)

  if Msg.Msg = WM_QUERYENDSESSION then
  begin
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' (WM_QUERYENDSESSION). Windows is going to be shut down.');
    AllowClose:=True;
    Msg.Result:=1;
  end;

  (* WINDOWS IS SHUTTING DOWN *)

  if Msg.Msg = WM_ENDSESSION then
  begin
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' (WM_ENDSESSION). Windows is shutting down...');
    AllowClose:=True;
  end;

end;

{ ####################################################### ! EXTENSION OF 'TSHAPE' CLASS ! ################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- FONT CHANGE }
procedure TShape.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- TEXT CHANGE }
procedure TShape.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

{ --------------------------------------------------------------------------------------------------------------------- PAINT PROCEDURE WITH TEXTOUT FUNCTION }
procedure TShape.Paint;
var
  R: TRect;
begin
  inherited;
  Canvas.Font.Assign(Font);
  R:=ClientRect;
  (* DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_VCENTER or DT_LEFT { DT_CENTER } or DT_SINGLELINE); *)
  TextOut(Canvas.Handle, CaptionLeft, CaptionTop, PChar(Caption), Length(Caption));
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ DRAW TEXT INSIDE SHAPE }
procedure TShape.ShapeText(Left, Top: integer; StrText: string; Format: TFontStyles);
begin
  { FIXED }
  Font.Name  :='Tahoma';
  Font.Size  :=10;
  Font.Color :=clGray;
  { NON-FIXED }
  Font.Style :=Format;
  Caption    :=StrText;
  CaptionLeft:=Left;
  CaptionTop :=Top;
end;

{ ####################################################### ! EXTENSION OF 'TPANEL' CLASS ! ################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------- EXTENDED PANEL COMPONENT }
procedure TPanel.Paint;
var
  R: TRect;
begin
  inherited;

  (* NONE OF THE GIVEN VARIABLES CAN BE BLACK *)

  if (mcBrushColor   <> $00000000) and
     (PenColorTop    <> $00000000) and
     (PenColorBottom <> $00000000) and
     (PenColorLeft   <> $00000000) and
     (PenColorRight  <> $00000000) then
    begin
      { GET DIMENSIONS }
      R:=ClientRect;
      { FILL BACKGROUND }
      Canvas.Brush.Color:=mcBrushColor;
      { TOP BORDER }
      Canvas.Pen.Width:=PenWidthTop;
      Canvas.Pen.Color:=PenColorTop;
      Canvas.MoveTo(1,           1);
      Canvas.LineTo(R.Right - 1, 1);
      { BOTTOM BORDER }
      Canvas.Pen.Width:=PenWidthBottom;
      Canvas.Pen.Color:=PenColorBottom;
      Canvas.MoveTo(1,           R.Bottom - 1);
      Canvas.LineTo(R.Right - 1, R.Bottom - 1);
      { LEFT BORDER }
      Canvas.Pen.Width:=PenWidthLeft;
      Canvas.Pen.Color:=PenColorLeft;
      Canvas.MoveTo(1,            1);
      Canvas.LineTo(1, R.Bottom - 1);
      { RIGHT BORDER }
      Canvas.Pen.Width:=PenWidthRight;
      Canvas.Pen.Color:=PenColorLeft;
      Canvas.MoveTo(R.Right - 1,            1);
      Canvas.LineTo(R.Right - 1, R.Bottom - 1);
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- DRAW TPANEL BORDERS }
procedure TPanel.PanelBorders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);
begin
  { TURN-OFF STYLES }
  BorderStyle   :=bsNone;
  { ASSIGN COLORS AND DRAW }
  mcBrushColor  :=FillColor;
  PenColorTop   :=TopColor;
  PenColorBottom:=BottomColor;
  PenColorLeft  :=LeftColor;
  PenColorRight :=RightColor;
end;

{ ##################################################### ! EXTENSION OF 'TSTRINGGRID' CLASS ! ################################################################ }

{ ---------------------------------------------------------------------------------------------------------------------- PASTE, CUT, COPY TO/FROM STRING GRID }
procedure TStringGrid.CopyCutPaste(mode: integer);
{ AVAILABLE MODES:                 }
{   0 = PASTE DATA TO STRING GRID  }
{   1 = COPY DATA FROM STRING GRID }
{   2 = CUT DATA FROM STRING GRID  }
const
  kTAB       = #9;
  kCR        = #13;
  kLF        = #10;
  kCRLF      = #13#10;
var
  Grect:       TGridRect;
  S:           string;
  CS:          string;
  F:           string;
  L:           integer;
  R:           integer;
  C:           integer;
  Sel:         TGridRect;
  Row, Col:    integer;
  TxtFromSel:  string;
  RowNum:      integer;
begin
  { ----------------------------------------------------------------------------------------------------------------------------- PASTE DATA INTO STRING GRID }
  if mode = 0 then
  begin
    GRect:=Selection;
    L    :=GRect.Left;
    R    :=GRect.Top;
    S    :=ClipBoard.AsText;
    R    :=R - 1;
    { GO 'BREAK_LINE' BY 'BREAK_LINE' }
    while Pos(kCR, S) > 0 do
    begin
      R :=R + 1;
      C :=L - 1;
      CS:=Copy(S, 1, Pos(kCR, S));
      while Pos(kTAB, CS) > 0 do
      begin
        C:=C + 1;
        if (C <= ColCount - 1) and (R <= RowCount - 1) then Cells[C, R]:=Copy(CS, 1, Pos(kTAB, CS) - 1);
        F:=Copy(CS, 1, Pos(kTAB, CS) - 1);
        Delete(CS,  1, Pos(kTAB, CS));
      end;
      if (C <= ColCount - 1) and (R <= RowCount - 1) then Cells[C + 1, R]:=Copy(CS, 1, Pos(kCR, CS) - 1);
      Delete(S, 1,Pos(kCR, S));
      if Copy(S, 1, 1) = kLF then Delete(S, 1, 1);
    end;
  end;
  { ----------------------------------------------------------------------------------------------------------------------- COPY OR CUT DATA FROM STRING GRID }
  if (mode = 1) or (mode = 2) then
  begin
    Sel:=Selection;
    TxtFromSel:='';
    RowNum:=Sel.Top;
    for Row:=Sel.Top to Sel.Bottom do
    begin
      if RowHeights[RowNum] <> -1 then
      begin
        for Col:=Sel.Left to Sel.Right do
        begin
          TxtFromSel:=TxtFromSel + Cells[Col, Row];
          if mode = 2 then Cells[Col, Row]:='';  { CUT DATA FROM STRING GRID }
          if Col < Sel.Right then TxtFromSel:=TxtFromSel + kTAB;
        end;
        if Row < Sel.Bottom then TxtFromSel:=TxtFromSel + kCRLF;
      end;
      inc(RowNum);
    end;
    ClipBoard.AsText:=TxtFromSel + kCRLF;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------- DELETE AND ESCAPE BEHAVIOUR FOR STRING GRID }
procedure TStringGrid.DelEsc(mode: integer; pCol, pRow: integer);
begin
  { MODE = 0; ESCAPE, QUIT EDIT MODE }
  { MODE = 1; DEL, DELETE CELL VALUE }
  if mode = 0 then EditorMode:=False;
  if mode = 1 then Cells[pCol, pRow]:='';
end;

{ -------------------------------------------------------------------------------------------------------------------------- CLEAR ALL CONTENT OF STRING GRID }
procedure TStringGrid.ClearAll(dfRows: integer; FixedRows: integer; FixedCols: integer; ZeroCol: boolean);
var
  iCNT:    integer;
  jCNT:    integer;
  OffSet:  integer;
begin
  if not Enabled then Exit;
  if ZeroCol then OffSet:=1 else OffSet:=0;
  for iCNT:=FixedRows to Self.RowCount do
    for jCNT:=(FixedCols - OffSet) to Self.ColCount do
      Cells[jCNT, iCNT]:='';
  RowCount:=dfRows;
end;

{ --------------------------------------------------------------------------------------------------------------- DELETE SELECTED ROW FROM GIVEN 'STRINGGRID' }
procedure TStringGrid.DeleteRowFrom(FixedRow: integer; FixedCol: integer);
var
  mySG:    TAbstractGrid;
  myRect:  TGridRect;
  sRow:    integer;
  iCNT:    integer;
  jCNT:    integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  { -------------------------------------------------------------------------------------------------------------------------------------- CHECK FOR LAST ROW }
  if RowCount > FixedRow + 1 then
  begin
    { REMEMBER SELECTED ROW }
    sRow         :=Row;
    { SELECTION }
    myRect.Left  :=1;
    myRect.Right :=Col;
    myRect.Top   :=Row;
    myRect.Bottom:=Row;
    { ATTACHED 'STRINGGRID' TO 'MYSTRINGGRID' }
    Selection :=myRect;
    { DO NOT FREE OR NIL IT ! }
    mySG:=TAbstractGrid(Self);
    mySG.DeleteRow(sRow);
    { KEEP SELECTION IN PLACE }
    if (sRow < (RowCount - 1)) then Row:=sRow;
  end
    else
      if RowCount = FixedRow + 1 then
        for iCNT:=FixedRow to RowCount do for jCNT:=FixedCol to ColCount do Cells[jCNT, iCNT]:='';
  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  with Self do
  begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    Repaint;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------- SELECTION COLOR IN 'TSTRINGGRID' }
procedure TStringGrid.DrawSelected(ARow: integer; ACol: integer; State: TGridDrawState; Rect: TRect; FontColorSel: TColor; BrushColorSel: TColor; FontColor: TColor; BrushColor: TColor; Headers: boolean);
var
  FixedColumn:  integer;
  FixedRow:     integer;
begin
  { HIGHLLIGHT ENTIRE ROW IF CELL IS SELECTED }
  if (State = [gdSelected]) or (State = [gdFocused]) then
  begin
    Canvas.Font.Color:=FontColorSel;
    Canvas.Brush.Color:=BrushColorSel
  end else begin
    Canvas.Font.Color:=FontColor;
    Canvas.Brush.Color:=BrushColor;
  end;
  { EXTEND DRAWING ON HEADERS IF FALSE }
  FixedColumn:=0;
  FixedRow   :=0;
  if not (Headers) then
  begin
    FixedColumn:= -1;
    FixedRow   := -1;
  end;
  { DRAW SELECTED ROW }
  if (ARow > FixedRow) and (ACol > FixedColumn) then
  begin
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, Cells[ACol, ARow]);
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- FONT COLORS FOR NUMBERS }
procedure TStringGrid.ColorValues(ARow: integer; ACol: integer; Rect: TRect; NegativeColor: TColor; PositiveColor: TColor);
var
  MyCell:  string;
begin
  MyCell:=Cells[ACol, ARow];
  { COLORS }
  if StrToFloatDef(MyCell, 0) < 0 then
    Canvas.Font.Color:=NegativeColor
      else
        Canvas.Font.Color:=PositiveColor;
  { MAKE RECTANGLE SMALLER ENOUGH SO IT WILL NOT OVERLAP GRID LINES }
  InflateRect(Rect, -2, -2);
  Canvas.TextRect(Rect, MyCell);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- AUTO COLUMN WIDTH }
procedure TStringGrid.SetColWidth(FirstDefault: integer; AddSpace: integer);
var
  tblArray:  array of integer;
  iCNT:      integer;
  jCNT:      integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  if Row > 0 then SetLength(tblArray, RowCount) else Exit;
  ColWidths[0]:=FirstDefault;
  { ---------------------------------------------------------------------------------------------------------------------------- ITERATE THROUGHT ALL COLUMNS }
  for jCNT:=1 to ColCount - 1 do
  begin
    { ----------------------------------------------------------------------------------------------------- ITERATE THROUGHT ALL ROWS INCLUDING ACTUAL HEADER }
    for iCNT:=0 to RowCount - 1 do tblArray[iCNT]:=Canvas.TextWidth(Cells[jCNT, iCNT]);
    { ---------------------------------------------------------------------------------------------------------------------------------- RETURN HIGHEST VALUE }
    if not (ColWidths[jCNT] = -1) then ColWidths[jCNT]:=MaxIntValue(tblArray) + AddSpace;  (* SKIP HIDDEN COLUMNS *)
  end;
  SetLength(tblArray, 1);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ MERGE SORT }
procedure TStringGrid.MSort(const SortCol: integer; const DataType: integer; const Ascending: boolean);
var
   iCNT:      integer;
   TempGrid:  TStringGrid;
   List:      array of integer;
begin
  TempGrid:=TStringGrid.create(nil);
  try
    TempGrid.RowCount :=RowCount;
    TempGrid.ColCount :=ColCount;
    TempGrid.FixedRows:=FixedRows;
    SetLength(List, RowCount - FixedRows);
    for iCNT:= FixedRows to RowCount - 1 do
    begin
      List[iCNT - FixedRows]:=iCNT;
      TempGrid.Rows[iCNT].Assign(Rows[iCNT]);
    end;
    MergeSort(Self, List, SortCol, DataType, Ascending);
    for iCNT:=0 to RowCount - FixedRows - 1 do
    begin
      Rows[iCNT + FixedRows].Assign(TempGrid.Rows[List[iCNT]]);
    end;
    Row:=FixedRows;
  finally
    TempGrid.Free;
  end;
  SetLength(List, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- AUTO THUMB SIZE }
procedure TStringGrid.AutoThumbSize;
var
  info:  TScrollInfo;
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------------- READ DATA }
  Fillchar(info, SizeOf(info), 0);
  { ----------------------------------------------------------------------------------------------------------------------------------------------- VERITICAL }
  with info do begin
    cbSize:=SizeOf(info);
    fmask :=SIF_ALL;
    GetScrollInfo(Self.Handle, SB_VERT, info);
    fMask :=fmask or SIF_PAGE;
    nPage :=Self.VisibleRowCount * (nmax - nmin) div Self.RowCount;
  end;
  SetScrollInfo(Self.Handle, SB_VERT, info, True);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- HORIZONTAL }
  with info do begin
    cbSize:=SizeOf(info);
    fMask :=SIF_ALL;
    GetScrollInfo(Self.Handle, SB_HORZ, info);
    fmask :=fmask or SIF_PAGE;
    nPage :=Self.VisibleColCount * (nmax - nmin) div Self.ColCount;
  end;
  SetScrollInfo(Self.Handle, SB_HORZ, info, True);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- LAYOUT | SAVE }
procedure TStringGrid.SaveLayout(ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string);
var
  iCNT:  integer;
begin

  { COLUMN WIDTH }
  for iCNT:=0 to Self.ColCount - 1 do
  begin
    if iCNT = 0 then Settings.TMIP.WriteInteger(ColWidthName, ColPrefix + IntToStr(iCNT), 10);
    if iCNT > 0 then Settings.TMIP.WriteInteger(ColWidthName, ColPrefix + IntToStr(iCNT), Self.ColWidths[iCNT]);
  end;

  { SQL COLUMN NAME }
  for iCNT:=0 to Self.ColCount - 1 do Settings.TMIP.WriteString(ColOrderName, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 0]);

  { COLUMN TITLE }
  for iCNT:=0 to Self.ColCount - 1 do
  begin
    if iCNT = 0 then Settings.TMIP.WriteString(ColNames, ColPrefix + IntToStr(iCNT), '');
    if iCNT > 0 then Settings.TMIP.WriteString(ColNames, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 1]);
  end;

end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- LAYOUT | LOAD }
function TStringGrid.LoadLayout(var StrCol: string; ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string): boolean;
var
  ColOrderSec:  TStringList;
  ColWidthSec:  TStringList;
  ColNamesSec:  TStringList;
  iCNT:         integer;
begin

  (* IMPORTANT NOTE *)

  (* 'COLORDERNAME' AND 'COLWIDTHNAME' PROVIDE WITH SECTION NAMES FOR COLUMN ORDER AND COLUMN WIDTH. *)
  (* BOTH SECTIONS MUST CONTAINS EQUAL NUMBER OF VALUE KEYS. EACH KEY CONTAIN COLUMN NAME USED BY    *)
  (* STRING GRID COMPONENT (AGE VIEW) THAT DISPLAYS DATA FROM SQL SERVER DATABASE, THUS COLUMN NAMES *)
  (* ARE USED TO BUILD SQL QUERY, THIS IS BECAUSE WE USE SQL EXPRESSIONS TO OBTAIN PROPER OUTPUT     *)
  (* AFTER FILTERING AND/OR SORTING ETC.                                                             *)

  { CHECK NUMBER OF KEYS IN GIVEN SECTION }
  Result:=False;
  ColWidthSec:=TStringList.Create;
  ColOrderSec:=TStringList.Create;
  ColNamesSec:=TStringList.Create;
  try
    Settings.TMIP.ReadSection(ColWidthName, ColWidthSec);
    Settings.TMIP.ReadSection(ColOrderName, ColOrderSec);
    Settings.TMIP.ReadSection(ColNames, ColNamesSec);
    if (ColWidthSec.Count = ColOrderSec.Count) and (ColWidthSec.Count = ColNamesSec.Count) then
    begin
      Self.ColCount:=ColWidthSec.Count;
      Result:=True;
    end;
  finally
    ColWidthSec.Free;
    ColOrderSec.Free;
    ColNamesSec.Free;
  end;

  { PROCESS ALL THE DATA FROM DECODED SETTINGS FILE }
  if Result = True then
  begin
    SetLength(Self.SqlColumns, Self.ColCount, 2);
    for iCNT:=0 to Self.ColCount - 1 do
    begin

      { SKIP FIRST COLUMN AS IT HOLDS EMPTY COLUMN }
      if iCNT > 0 then
      begin
        if iCNT < (Self.ColCount - 1) then
          StrCol:=StrCol + Settings.TMIP.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ','
            else
              StrCol:=StrCol + Settings.TMIP.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ' ';

        { STORE SQL "COLUMN NAMES" AND "USER FRIENDLY NAMES" INTO HELPER ARRAY }
        Self.SqlColumns[iCNT, 0]:=Settings.TMIP.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '');
        Self.SqlColumns[iCNT, 1]:=Settings.TMIP.ReadString(ColNames, ColPrefix + IntToStr(iCNT), '');

        { DISPLAY "USER FRIENDLY" COLUMN NAME }
        Self.Cells[iCNT, 0]  :=Settings.TMIP.ReadString(ColNames, ColPrefix + IntToStr(iCNT), '');
      end;

      { ASSIGN SAVED WIDTH }
      Self.ColWidths[iCNT]:=Settings.TMIP.ReadInteger(ColWidthName, ColPrefix + IntToStr(iCNT), 100);
    end;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------- RETURN COLUMN NUMBER FOR GIVEN HEADER }
function TStringGrid.ReturnColumn(ColumnName: string; FixedCol: integer; FixedRow: integer): integer;
var
  iCNT: integer;
begin
  result:=-100; { RETURN OUT OF BOUND VALUE }
  for iCNT:=FixedCol to ColCount - 1 do
    if Cells[iCNT, FixedRow - 1] = ColumnName then
    begin
      result:=iCNT;
      Exit;
    end;
end;

{ ------------------------------------------------------------------------------------------------------------------ EXPORT STRING GRID CONTENT TO EXCEL FILE }

(* ************************************************************************************************************************************************************

NOTE: USER MUST HAVE INSTALLED MICROSOFT EXCEL WITH VBA INSTALLED BEFORE EXPORTING STRING GRID TO EXCEL FILE.
      OTHERWISE ERROR MESSAGE "INVALID STRING CLASS" WILL OCCUR.

NOTE: THIS METHOD SHOULD BE RUN IN WORKER THREAD.

************************************************************************************************************************************************************ *)

function TStringGrid.ToExcel(ASheetName: string; AFileName: string; idThd: integer): boolean;
const
  xlWBATWorksheet = -4167;
  xlWARN_MESSAGE  = 'Invalid class string';
var
  Col:        integer;
  Row:        integer;
  RowOffset:  integer;     { OFFSET CANNOT BE < 1 }
  ColOffset:  integer;     { OFFSET CANNOT BE < 1 }
  XLApp:      OLEVariant;
  Sheet:      OLEVariant;
begin
  Result:=False;
  { READ AGE VIEW WITH COMMENT COLUMN AND GENERAL COLUMN | TO STRING GRID }


  { ------------------------------------------------------------------------------------------------------------------------------ INITIATE EXCEL APPLICATION }
  try
    XLApp:=CreateOleObject('Excel.Application');
    try
      XLApp.Visible:=False;
      XLApp.Caption:='Unity For Debt Management - Data Export';
      XLApp.DisplayAlerts:=False;
      XLApp.Workbooks.Add(xlWBatWorkSheet);
      Sheet:=XLApp.Workbooks[1].WorkSheets[1]; (* WARNING! OLDER DELPHI CODE INSIGHT MAY SHOW FALSE ERROR, IGNORE IT *)
      Sheet.Name:=ASheetName;
      ColOffset:=1;
      RowOffset:=1;
      { -------------------------------------------------------------------------------------------------------------------------- STRING GRID TO EXCEL SHEET }
      for Col:=0 to Self.ColCount - 1 do
        for Row:=0 to Self.RowCount - 1 do
          { WE OMITT FIRST STRING GRID COLUMN }
          Sheet.Cells[Row + RowOffset, Col + ColOffset]:=Self.Cells[Col + 1, Row];
      { ---------------------------------------------------------------------------------------------------------------------------------------- SAVE TO FILE }
      XLApp.Workbooks[1].SaveAs(AFileName); (* WARNING! OLDER DELPHI CODE INSIGHT MAY SHOW FALSE ERROR, IGNORE IT *)
      Result:=True;
      { ----------------------------------------------------------------------------------------------------------------------------------- CLOSE AND DISPOSE }
    finally
      if not VarIsEmpty(XLApp) then
      begin
        XLApp.DisplayAlerts:=False;
        XLApp.Quit;
        XLAPP:=Unassigned;
        Sheet:=Unassigned;
        { ------------------------------------------------------------------------------------------------------------------------------- SUCCESSFULL MESSAGE }
        if Result then
        begin
          LogText(Settings.AppDir + Settings.LogFile, 'Thread: [' + IntToStr(idThd) + ']: The data has been successfully transferred to Excel.');
          SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PCHAR('The data has been successfully transferred to Excel.')));
        end;
      end;
    end;
  { --------------------------------------------------------------------------------------------------------------------------------- ON INITIALIZATION ERROR }
  except
    on E: Exception do
      if E.Message = xlWARN_MESSAGE then
      { EXCEL NOT FOUND }
      begin
        LogText(Settings.AppDir + Settings.LogFile, 'Thread: [' + IntToStr(idThd) + ']: The data cannot be exported because Excel cannot be found.');
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported because Excel cannot be found.')));
      end else
      { GENERIC MESSAGE }
      begin
        LogText(Settings.AppDir + Settings.LogFile, 'Thread: [' + IntToStr(idThd) + ']: The data cannot be exported, error message thrown: ' + E.Message + '.');
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported. Please contact IT support.')));
      end;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------ TURN ON OR OFF STRING GRID CONTENT }
procedure TStringGrid.Freeze(PaintWnd: Boolean);
begin
  { TRUE | HIDE STRING GRID CONTENT }
  if (PaintWnd) then
  begin
    with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  end;
  { FALSE | SHOW STRING GRID CONTENT }
  if not (PaintWnd) then
  begin
    with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
    Self.Repaint;
  end;
end;

{ ############################################################# ! SETTINGS CLASS ! ########################################################################## }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TSettings.Create(Caption: string);
begin

  (* SETTINGS FILE CONTAINERS *)

  TMIP:=TMemIniFile.Create('');
  TMIG:=TMemIniFile.Create('');

  (* READ ONLY VARIABLES *)

  pAppDir     :=ExtractFileDir(Application.ExeName) + '\';
  pWinUserName:=Trim(LowerCase(GetEnvironmentVariable('username')));
  pLogFile    :='users\' + pWinUserName + '.log';
  pConfigFile :='config.cfg';
  pLogonFile  :='users\' + pWinUserName + '.cfg';

  (* CONFIG.CFG *)

  Decode(AppDir + ConfigFile, FILEKEY, True, Error, TMIG);   { DECODE CONFIG.CFG AND PUT INTO TMIG }
  if Error <> 0 then
  begin
    LogText(AppDir + LogFile, 'Cannot read master configuration file. Error code: ' + IntToStr(Error) + '. Application terminated.');
    Application.MessageBox(PChar('Cannot read master configuration file. Error code: ' + IntToStr(Error) + '. Please contact support.'),
                           PChar(Caption), MB_OK + MB_ICONSTOP);
    Application.Terminate;
  end;

  (* [LOGON].CFG *)

  Decode(AppDir + LogonFile, FILEKEY, True, Error, TMIP);   { DECODE <LOGON>.CFG AND PUT INTO TMIP }
  if Error <> 0 then
  begin
    LogText(AppDir + LogFile, 'Cannot read user configuration file. Error code: ' + IntToStr(Error) + '. Application terminated.');
    Application.MessageBox(PChar('Cannot read user configuration file. Error code: ' + IntToStr(Error) + '. Please contact support.'),
                           PChar(Caption), MB_OK + MB_ICONSTOP);
    Application.Terminate;
  end;

  (* PASSWORD *)

  if TMIG.ReadString(Password, 'VALUE', '') = '' then
  begin
    LogText(AppDir + LogFile, 'WARNING! Master password is not established.');
    Application.MessageBox(PChar('Master password is not established. Please contact IT support to reset it.'), PChar(Caption), MB_OK + MB_ICONWARNING);
  end;

  (* LAYOUTS *)

  pLayoutDir:=TMIG.ReadString(VariousLayouts, 'PATH', 'C:\');

  (* REGIONAL SETTINGS *)

  RegionalSettings:=TFormatSettings.Create;
  RegionalSettings.CurrencyDecimals :=4;
  RegionalSettings.DateSeparator    :='-';
  RegionalSettings.ShortDateFormat  :='yyyy-mm-dd';
  RegionalSettings.LongDateFormat   :='yyyy-mm-dd';
  RegionalSettings.TimeSeparator    :=':';
  RegionalSettings.TimeAMString     :='AM';
  RegionalSettings.TimePMString     :='PM';
  RegionalSettings.ShortTimeFormat  :='hh:mm:ss';
  RegionalSettings.LongTimeFormat   :='hh:mm:ss';
  FormatSettings:=RegionalSettings;
  Application.UpdateFormatSettings:=False;

end;

{ ############################################################## ! DATABASE CLASS ! ######################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TDataBase.Create;
begin
  (* INITIALIZE PRIVATE VARIABES *)
  ADOConnect  :=TADOConnection.Create(nil);
  pdbProvider :='';
  pdbConnStr  :='';
  pAccessLevel:='';
  pAccessMode :='';
  pLastError  :=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- CONNECT TO DATABASE }
(* WARNING! MUST BE EXECUTED BEFORE ANY CONNECTION ATTEMPTS *)
procedure TDataBase.InitializeConnection(idThd: integer; ErrorShow: boolean);
{ ------------------------------------------------------ ! COMMON VARIABLES AND CONSTANTS ! ----------------------------------------------------------------- }
const
  ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
  ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';
{ -------------------------------------------------------------- ! INNER BLOCK ! ---------------------------------------------------------------------------- }
procedure ErrorHandler(err_class: string; err_msg: string; should_quit: boolean; err_wnd: boolean);
begin
  LogText(Settings.AppDir + Settings.LogFile, ERR_LOGTEXT + '[' + err_class + '] ' + err_msg + ' (' + IntToStr(ExitCode) + ').');
  if err_wnd     then Application.MessageBox(PChar(ERR_MESSAGE), PChar(MainForm.CAPTION), MB_OK + MB_ICONWARNING);
  if should_quit then Application.Terminate;
end;
{ --------------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------- }
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  ADOConnect.Connected:=False;
  DataBase.AssignConnString(ErrorShow);
  { --------------------------------------------------------------------------------------------------------------------- SETUP CONNECTION AND TRY TO CONNECT }
  try
    { SETUP CONNESTION STRING AND PROVIDER }
    ADOConnect.ConnectionString :=DataBase.dbConnStr;
    ADOConnect.Provider         :=DataBase.dbProvider;
    { SETTINGS }
    ADOConnect.ConnectionTimeout:=Settings.TMIG.ReadInteger(Settings.DatabaseSetup, 'CONNECTION_TIMEOUT', 15);
    ADOCOnnect.CommandTimeout   :=Settings.TMIG.ReadInteger(Settings.DatabaseSetup, 'COMMAND_TIMEOUT',    15);
    ADOConnect.ConnectOptions   :=coConnectUnspecified;
    ADOConnect.KeepConnection   :=True;
    ADOConnect.LoginPrompt      :=False;
    ADOConnect.Mode             :=cmReadWrite;
    { CURSOR SETTINGS }
    ADOConnect.CursorLocation   :=clUseClient;        (* https://docs.microsoft.com/en-us/sql/ado/guide/data/the-significance-of-cursor-location *)
    ADOConnect.IsolationLevel   :=ilCursorStability;  (* https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx                    *)
    { CONNECT TO GIVEN SERVER }
    try
      if (DataBase.Check) then
      begin
        ADOConnect.Connected:=True;
        MainForm.InvoiceScanTimer.Enabled:=True;
        MainForm.OILoader.Enabled        :=True;
        SendMessage(MainForm.Handle, WM_GETINFO, 14, LPARAM(PCHAR('NULL')));
      end else
      begin
        MainForm.InvoiceScanTimer.Enabled:=False;
        MainForm.OILoader.Enabled        :=False;
        SendMessage(MainForm.Handle, WM_GETINFO, 15, LPARAM(PCHAR('NULL')));
      end;
    except
      on E: Exception do ErrorHandler(E.ClassName, E.Message, False, ErrorShow);
    end;
  finally
    { CHECK SERVER CONNECTION ON REGULAR BASIS }
    if not (MainForm.InetTimer.Enabled) then
    begin
      MainForm.InetTimer.Interval:=Settings.TMIG.ReadInteger(Settings.TimersSettings, 'NET_CONNETCTION', 15000);
      MainForm.InetTimer.Enabled:=True;
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------- READ CONNECTION STRING FROM SETTINGS FILE }
function TDataBase.AssignConnString(Print: boolean): boolean;
var
  dbConnNoPwd:  string;
begin
  Result:=True;
  try
    try
      if Settings.TMIG.ReadString(Settings.DatabaseSetup,'ACTIVE','') = 'MSSQL' then
      begin
        DataBase.dbProvider:=Settings.TMIG.ReadString(Settings.DatabaseSetup, 'MSSQLPROVIDER', '');
        dbConnNoPwd:='Provider='         + DataBase.dbProvider
                   + ';Data Source='     + Settings.TMIG.ReadString(Settings.DatabaseSetup, 'MSSQLSERVER',        '')
                   + ','                 + Settings.TMIG.ReadString(Settings.DatabaseSetup, 'MSSQLPORT',          '')
                   + ';Initial catalog=' + Settings.TMIG.ReadString(Settings.DatabaseSetup, 'MSSQLCATALOG',       '')
                   + ';User Id='         + Settings.TMIG.ReadString(Settings.DatabaseSetup, 'MSSQLUSERNAME',      '');
        DataBase.dbConnStr:=dbConnNoPwd + ';Password=' + Settings.TMIG.ReadString(Settings.DatabaseSetup, 'MSSQLPASSWORD', '');
      end;
    except
      Result:=False;
    end;
  finally
    if (Print) then LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: Connection string built [show_no_password] = ' + dbConnNoPwd);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ CHECK CONNECTION WITH SERVER }
function TDataBase.Check: boolean;
var
  EO:        EOleException;
  ConCheck:  TADOConnection;
begin
  { INITIALIZE }
  Result:=True;
  LastError:=0;
  ConCheck:=TADOConnection.Create(nil);
  { ASSIGN PARAMETERS }
  ConCheck.ConnectionString :=DataBase.dbConnStr;
  ConCheck.Provider         :=DataBase.dbProvider;
  ConCheck.ConnectionTimeout:=2;
  ConCheck.CommandTimeout   :=2;
  ConCheck.KeepConnection   :=False;
  ConCheck.LoginPrompt      :=False;
  ConCheck.Mode             :=cmRead;
  { TRY TO CONNECT }
  try
    try
      ConCheck.Connected:=True;
      ConCheck.Open;
    except
      on E: Exception do
      begin
        Result:=False;
        if E is EOLEException then
        begin
          EO:=EOleException(E);
          DataBase.LastError:=EO.ErrorCode;
        end;
      end;
    end;
  finally
    if ConCheck.Connected then ConCheck.Close;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------- USER ACCOUNT CONTROL | INITIALIZE }
(* NOTE: DO NOT USE THIS METHOD BEFORE DATABASE CONNECTION IS ESTABLISHED *)
function TDatabase.UACinitialize: boolean;
var
  Query:   TADOQuery;
  StrSQL:  string;
  iCNT:    integer;
  jCNT:    integer;
begin
  Result:=False;
  { ------------------------------------------------------------------------------------------------------------------------ READ ACCESS LEVEL FOR GIVEN USER }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    StrSQL:='SELECT ACCESS_LEVEL, ACCESS_MODE FROM tbl_UAC WHERE USERNAME = :uParam1';
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Parameters.ParamByName('uParam1').Value:=UpperCase(Settings.WinUserName);
    Query.Open;
    { TABLE 'TBL_UAC' SHOULD ALWAYS HOLDS DISTINC USERNAMES }
    { THEREFORE WE HAVE ONLY ONE ROW PER USER               }
    if Query.RecordCount = 1 then
    begin
      Database.AccessLevel:=Query.Recordset.Fields[0].Value;
      Database.AccessMode :=Query.Recordset.Fields[1].Value;
      Result:=True;
    end;
    { IF USER IS NOT FOUND }
    if Query.RecordCount = 0 then
    begin
      SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PCHAR('Cannot find user name: ' + UpperCase(Settings.WinUserName) + '. ' + Settings.AppName + ' will be closed. Please contact IT support.')));
      LogText(Settings.AppDir + Settings.LogFile, '[UAC READER]: Cannot find user name: ' + UpperCase(Settings.WinUserName) + '. Application terminated.');
      Exit;
    end;
  finally
    Query.Close;
    Query.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------- ACCESS LEVEL AND MODE TO EVENT LOG }
  if Database.AccessLevel = 'RO' then LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Level = read only given group(s).');
  if Database.AccessLevel = 'RW' then LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Level = read and write to given group(s).');
  if Database.AccessLevel = 'AD' then
  begin
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Level = access all areas.');
  end;
  if Database.AccessMode <> ''   then LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: User Access Mode = ' + LowerCase(Database.AccessMode) + '.');
  { ----------------------------------------------------------------------------------------------------------------------------- LOOP THROUGH THE COMPONENTS }
  for iCNT:=0 to MainForm.ComponentCount - 1 do
    if MainForm.Components[iCNT] is TEdit then
    begin
      { IF ACCESS LEVEL IS 'ADMINISTRATOR' THEN WE ENABLE EDIT COMPONENTS }
      if Database.AccessLevel = 'AD' then
      begin
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'COC' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'CUR' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'INT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'AGT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=True;
      end else
      begin
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'COC' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'CUR' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'INT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
        for jCNT:=1 to 4 do if MainForm.Components[iCNT].Name = 'AGT' + IntToStr(jCNT) then TEdit(MainForm.Components[iCNT]).Enabled:=False;
      end;
    end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- UAC | READ GROUPS }
procedure TDatabase.UACGroupReader(UACuser: string);
{ READ ALL GROUP ID AND GROUP NAME INTO AN ARRAY, GROUP LIS BOX IS POPULATED FROM SUCH ARRAY ONLY WITH GROUP NAME }
{ CORRESPONDING GROUP ID FOR SELECTED GROUP NAME BY THE USER IS USED TO UPLOAD ALL DISTINCT AGE DATES AND FINALY  }
{ USED TO UPLOAD PROPER DATA SNAPSHOT RELATED TO GIVEN GROUP NAME.                                                }
{ THIS METHOD IS NOT CALLED IF USER IS NOT FOUND IN DATABASE.                                                     }
var
  StrSQL:  string;
  Query:   TADOQuery;
  iCNT:    integer;
begin
  { INITIALIZE }
  MainForm.GroupListBox.Enabled:=False;
  iCNT:=0;
  { BUILD SQL QUERY }
  StrSQL:='SELECT GROUP_ID, GROUP_NAME FROM tbl_groups WHERE FID = (SELECT ID FROM tbl_UAC WHERE USERNAME = ' + QuotedStr(UACuser) + ')';
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    { EXECUTE WITH GIVEN SQL AND DATABASE }
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    try
      Query.Open;
      SetLength(Database.ArrGroupList, 2, 2);
      if (Query.RecordCount > 0) then
      begin
        { MOVE TO AN ARRAY }
        Query.Recordset.MoveFirst;
        while not Query.Recordset.EOF do
        begin
          { READ FROM AND WRITE TO }
          Database.ArrGroupList[iCNT, 0]:=Query.Recordset.Fields['GROUP_ID'  ].Value;
          Database.ArrGroupList[iCNT, 1]:=Query.Recordset.Fields['GROUP_NAME'].Value;
          { MOVE COUNTERS }
          inc(iCNT);
          Query.Recordset.MoveNext;
          { EXPAND ARRAY BY ONE ROW }
          SetLength(Database.ArrGroupList, iCNT + 1, 2);
        end;
        { POPULATE LIST BOX }
        MainForm.GroupListBox.Clear;
        for iCNT:=0 to high(Database.ArrGroupList) - 1 do MainForm.GroupListBox.Items.Add(Database.ArrGroupList[iCNT, 1]);
        MainForm.GroupListBox.ItemIndex:=0;
        MainForm.GroupListBox.Enabled:=True;
      end;
    except
      on E: Exception do
        LogText(Settings.AppDir + Settings.LogFile, '[UAC READER]: Query error. Exception thrown: ' + E.Message);
    end;
  finally
    { RELEASE FROM MEMORY }
    Query.Close;
    Query.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------- UAC | READ ALL DATES FOR GIVEN ID }
procedure TDatabase.UACAgeDates(StrGroupID: string);
var
  StrSQL:  string;
  Query:   TADOQuery;
begin
  { INITIALIZE }
  MainForm.GroupListDates.Clear;
  MainForm.GroupListDates.Enabled:=False;
  { BUILD SQL QUERY }
  StrSQL:='SELECT DISTINCT AGE_DATE FROM tbl_snapshots WHERE GROUP_ID = ' + QuotedStr(StrGroupID);
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    { EXECUTE WITH GIVEN SQL AND DATABASE }
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    try
      Query.Open;
      if (Query.RecordCount > 0) then
      begin
        Query.Recordset.MoveFirst;
        while not Query.Recordset.EOF do
        begin
          MainForm.GroupListDates.Items.Add(Query.Recordset.Fields['AGE_DATE'].Value);
          Query.Recordset.MoveNext;
        end;
      end;
    except
      on E: Exception do
        LogText(Settings.AppDir + Settings.LogFile, '[UAC READER]: Query error. Exception thrown: ' + E.Message);
    end;
  finally
    MainForm.GroupListDates.ItemIndex:=MainForm.GroupListDates.Items.Count - 1;
    if Database.AccessLevel = 'AD' then MainForm.GroupListDates.Enabled:=True;
    { RELEASE FROM MEMORY }
    Query.Close;
    Query.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------- UAC | RETURN SPECIFIC 'COCOE' FROM THE LIST }
function TDatabase.UACString(ListPos: integer; CoPos: integer; Mode: integer): string;
{ WARNING! GROUP ID FORMAT: SERIES OF 4 GROUPS OF 5 DIGITS, I.E.: '020470034000043' MUST BE READ AS FOLLOWS: }
{   1. 1ST CO CODE: 02047 (2047)                                                                             }
{   2. 2ND CO CODE: 00340 (340)                                                                              }
{   3. 3RD CO CODE: 00043 (43)                                                                               }
{   4. 4TH CO CODE: 00000 (0)                                                                                }
{ PARAMETERS:                                                                                                }
{   1. 'LISTPOS' = POSITION OF THE GROUP HOLING 'COCODES'                                                    }
{   2. 'COPOS'   = NUMBER OF THE 'COCODE' TO BE RETURNED                                                     }
{   3. 'MODE'    = 0 (COCODE) OR 1 (GROUP NAME) OR 2 (GROUP ID)                                              }
begin
  { VALIDATE INPUT DATA }
  if ( ListPos > High(Database.ArrGroupList) ) or (CoPos > 4) then Exit;
  { EXTRACT | 'COCODE' FROM GROUP ID }
  if Mode = 0 then
  begin
    if CoPos = 1 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 1,  5)));
    if CoPos = 2 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 6,  5)));
    if CoPos = 3 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 11, 5)));
    if CoPos = 4 then Result:=IntToStr(StrToInt(MidStr(Database.ArrGroupList[ListPos, Mode], 16, 5)));
  end;
  { EXTRACT | GROUP NAME }
  if Mode = 1 then Result:=Database.ArrGroupList[ListPos, Mode];
  { EXTRACT | FULL GROUP ID }
  if Mode = 2 then Result:=Database.ArrGroupList[ListPos, 0];
end;

{ ########################################################## ! INVOICE TRACKER CLASS ! ###################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TInvoiceTracker.Create;
begin
  (* DO NOTHING *)
end;

{ ----------------------------------------------------------------------------------------------------------------------------- SCAN INVOICES AND SEND EMAILS }
(* IT REQUIRES PRE-FORMATTED HTML LAYOUT. THE PREPARED PART WITH THE HTML TABLE THAT CARRY INVOICES REPLACE TAG '{INVOICE_LIST}' IN THE LAYOUT FILE *)
procedure TInvoiceTracker.Scanner(idThd: integer);  (* ASYNC *)

{ ------------------------------------------------------ ! COMMON VARIABLES AND CONSTANTS ! ----------------------------------------------------------------- }

var
  { COUNTERS }
  iCNT:        integer;
  jCNT:        integer;
  Rem1:        integer;
  Rem2:        integer;
  Rem3:        integer;
  Leg0:        integer;
  { HTML FOR BODY LAYOUT }
  HTMLTable:   string;
  HTMLRow:     string;
  HTMLTemp:    string;
  HTMLRem1:    string;
  HTMLRem2:    string;
  HTMLRem3:    string;
  HTMLLega:    string;
  { FILES AND EMAILS }
  EmailFr:     string;
  EmailTo:     string;
  EmailCc:     string;
  EmailBc:     string;
  EmailRt:     string;
  EmailSub:    string;
  EmailBody:   string;
  { CUSTOMER DETAILS }
  CustName:    string;
  CustAddr:    string;
  { EXCLUSIONS }
  REM_EX1:     string;
  REM_EX2:     string;
  REM_EX3:     string;
  REM_EX4:     string;
  REM_EX5:     string;
  { DATA FOR COMPARISION }
  CoCode:      string;
  Branch:      string;
  CUID:        string;
  Reminder1:   string;
  Reminder2:   string;
  Reminder3:   string;
  Reminder4:   string;
  { HTML DATA WITH POPULLATED TABLES PER REMINDER }
  Table1:      string;    { REMINDER 1 INVOICES   }
  Table2:      string;    { REMINDER 2 INVOICES   }
  Table3:      string;    { REMINDER 3 INVOICES   }
  Table4:      string;    { LEGAL ACTION INVOICES }

{ -------------------------------------------------------------- ! COMMON METHODS ! ------------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------ MOVE OPEN ITEMS DATA TO HTML TABLE }
procedure OpenItemsToHtmlTable(var HtmlReminder: string; var ReminderCount: integer; var SG: TStringGrid; ActualRow: integer);
begin
  HTMLTemp:=HTMLRow;
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[11, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[4,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[12, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[8,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[9,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[5,  ActualRow], [rfReplaceAll]);
  HtmlReminder:=HtmlReminder + HTMLTemp;
  inc(ReminderCount);
end;
{ ---------------------------------------------------------------------------------------------------------------------------------- SEND EMAIL WITH REMINDER }
procedure SendReminderEmail(ReminderNumber: integer; var RemCount: integer; var TableNum: string; var HTMLRemNum: string; var SG: TStringGrid; ActualRow: integer);
var
  SL:           TStringList;
  BankDetails:  string;
begin
  BankDetails:=MainForm.sgInvoiceTracker.Cells[24, iCNT];
  SL:=TStringList.Create;
  try
    { -------------------------------------------------------------------------------------------------------------------------------------- READ LAYOUT FORM }
    if ReminderNumber = 4 then SL.LoadFromFile(Settings.LayoutDir + 'fm_notify.html')
      else
        SL.LoadFromFile(Settings.LayoutDir + SG.Cells[6, ActualRow] + '_' + IntToStr(ReminderNumber) + '.html');
    { ------------------------------------------------------------------------------------------------------------------------------------ FILL WITH THE DATA }
    TableNum  :=StringReplace(TableNum,  '{ROWS}',         HTMLRemNum, [rfReplaceAll]);
    EmailBody :=StringReplace(SL.Text,   '{INVOICE_LIST}', TableNum,   [rfReplaceAll]);
    EmailBody :=StringReplace(EmailBody, '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
    EmailBody :=StringReplace(EmailBody, '{BANKS}',        BankDetails,[rfReplaceAll]);
    { ------------------------------------------------------------------------------------------------------------------------------------- E-MAIL PARANETERS }
    if ReminderNumber < 4 then EmailTo:=SG.Cells[9,  ActualRow];
    if ReminderNumber = 4 then EmailTo:=SG.Cells[11, ActualRow];
    EmailFr   :=MainForm.sgInvoiceTracker.Cells[8,  iCNT];
    EmailCc   :=EmailFr;
    EmailBc   :='';
    EmailRt   :='';
    EmailSub  :='Unpaid invoices (' + CustName + '): Reminder ' + IntToStr(ReminderNumber) + ' / 3';
    { ------------------------------------------------------------------------------------------------------ RESET THE INVOICE COUNTER AND HTML TABLE CONTENT }
    RemCount  :=0;
    TableNum  :=HTMLTable;
    HTMLRemNum:='';
    { -------------------------------------------------------------------------------------------------------------------------------------------- SEND EMAIL }
    XMailer    :=EmailFr;
    MailFrom   :=EmailFr;
    MailTo     :=EmailTo;
    MailCc     :=EmailCc;
    MailBcc    :=EmailBc;
    MailRt     :=EmailRt;
    MailSubject:=EmailSub;
    MailBody   :=EmailBody;
    Self.idThd :=idThd;
    SendNow;
    { ------------------------------------------------------------------------------------------------------------------------------------------- DEBUG LINES }
    //SL.Text:=EmailBody;
    //SL.SaveToFile('e:\projects\test.html_' + IntToStr(ReminderNumber) + '.html');
  finally
    SL.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------- LOG INVOICE WITH INVOICE TRACKER LIST }
procedure AddInvoiceToList(var CUID: string; InvoiceNumber: string; ReminderNumber: integer);
var
  Stamp:   TDateTime;
  Query:   TADOQuery;
  StrSQL:  string;
  Return:  integer;
begin
    { -------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
    Stamp:=Now;
    Query:=TADOQuery.Create(nil);
    Query.Connection:=Database.ADOConnect;
    try
      { ------------------------------------------------------------------------------------------------------------------------------- LOG TO 'TBL_INVOICES' }
      Query.SQL.Clear;
      StrSQL:='INSERT INTO tbl_invoices (SK, CUID, INVOICENO, INVOICESTATE, STAMP) ' +
              'VALUES ( (SELECT ID FROM tbl_tracker WHERE CUID = ' + CUID + '), ' + CUID + ', ' + InvoiceNumber + ', ' + IntToStr(ReminderNumber) + ', :uParam )';
      Query.SQL.Text:=StrSQL;
      Query.Parameters.ParamByName('uParam').Value:=Stamp;
      Query.ExecSQL;
      { ---------------------------------------------------------------------------------------------------------------------------------- LOG TO 'TBL_DAILY' }
      Query.SQL.Clear;
      StrSQL:='SELECT EMAIL FROM tbl_daily WHERE CUID = ' + CUID + ' AND AGEDATE = :uParam';
      Query.SQL.Text:=StrSQL;
      Query.Parameters.ParamByName('uParam').Value:=StrToDate(MainForm.GroupListDates.Text);
      Query.Open;
      { ------------------------------------------------------------------------------------------------------------------------------------- UPDATE EXISTING }
      if Query.RecordCount = 1 then
      begin
        { CHECK WHAT VALUE WE HAVE }
        Query.Recordset.MoveFirst;
        Return:=Query.Recordset.Fields[0].Value;
        Inc(Return);
        Query.Close;
        { MAKE NEW SQL }
        Query.SQL.Clear;
        StrSQL:='UPDATE tbl_daily SET EMAIL = ' + IntToStr(Return) +
                ' WHERE CUID = '                + CUID +
                ' AND GROUP_ID = '              + Database.ArrGroupList[MainForm.GroupListBox.ItemIndex, 0] +
                ' AND AGEDATE = :uParam';
        Query.SQL.Text:=StrSQL;
        Query.Parameters.ParamByName('uParam').Value:=StrToDate(MainForm.GroupListDates.Text);
        Query.ExecSQL;
      end else
        { -------------------------------------------------------------------------------------------------------------------------------- LOG EMAIL REMINDER }
        if Query.RecordCount = 0 then
        begin
          Query.Close;
          Query.SQL.Clear;
          { MAKE NEW SQL }
          StrSQL:='INSERT INTO tbl_daily (GROUP_ID, CUID, AGEDATE, STAMP, USER_ALIAS, EMAIL) VALUES (:uParam1, :uParam2, :uParam3, :uParam4, :uParam5, :uParam6)';
          Query.SQL.Text:=StrSQL;
          { APPLY PARAMETERS }
          Query.Parameters.ParamByName('uParam1').Value:=Database.ArrGroupList[MainForm.GroupListBox.ItemIndex, 0];
          Query.Parameters.ParamByName('uParam2').Value:=CUID;
          Query.Parameters.ParamByName('uParam3').Value:=StrToDate(MainForm.GroupListDates.Text);
          Query.Parameters.ParamByName('uParam4').Value:=Stamp;
          Query.Parameters.ParamByName('uParam5').Value:=UpperCase(Settings.WinUserName);
          Query.Parameters.ParamByName('uParam6').Value:='1';
          Query.ExecSQL;
        end;
    finally
      Query.Free;
    end;
end;

{ --------------------------------------------------------------------------------------------------------- CHECK IF GIVEN REMINDER WAS SENT FOR THIS INVOICE }
function CheckInvoiceState(InvNum: string; SearchState: integer): boolean;
var
  Query:   TADOQuery;
  StrSQL:  string;
begin
  { INITIALIZE }
  Result:=False;
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  if SearchState = 0 then StrSQL:='SELECT INVOICESTATE FROM tbl_invoices WHERE INVOICENO = :uParam1'
    else
      StrSQL:='SELECT INVOICESTATE FROM tbl_invoices WHERE INVOICENO = :uParam1 AND INVOICESTATE = :uParam2';
  try
    { EXCUTE }
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Parameters.ParamByName('uParam1').Value:=InvNum;
    if not SearchState = 0 then Query.Parameters.ParamByName('uParam2').Value:=SearchState;
    Query.Open;
    if Query.RecordCount = 0  then Result:=False;
    if SearchState = 0 then
      { WE CAN HAVE MORE THAN ONE INVOICE STATES }
      if Query.RecordCount >= 1 then Result:=True;
    if not SearchState = 0 then
      { WE CAN HAVE JUST ONE INVOICE STATE, THE ONE WE LOOK FOR }
      if Query.RecordCount = 1 then Result:=True;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{ --------------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------- }

begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Rem1:=0;
  Rem2:=0;
  Rem3:=0;
  Leg0:=0;
  SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Processing reminders...')));
  { -------------------------------------------------------------------------------------------------------- HTML TABLE WITH COLUMNS AND PLACEHOLDER FOR ROWS }
  HTMLTable:='<table class="data">'                   +#13#10+
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
  { ------------------------------------------------------------------------------------------- HTML ROW FORM TO BE FILLED WITH DATA AND PUT INTO 'HTMLTABLE' }
  HTMLRow:='<tr>'                                     +#13#10+
           '  <td class="col1">{INV_NUM}</td>'        +#13#10+
           '  <td class="col2">{INV_DAT}</td>'        +#13#10+
           '  <td class="col3">{DUE_DAT}</td>'        +#13#10+
           '  <td class="col4">{INV_CUR}</td>'        +#13#10+
           '  <td class="col5">{INV_AMT}</td>'        +#13#10+
           '  <td class="col6">{INV_OSA}</td>'        +#13#10+
           '</tr>'                                    +#13#10;
  { WE CAN PROCEED IF OPEN ITEMS AND INVOICE TRACKER ARE NOT EMPTY }
  if (MainForm.sgOpenItems.RowCount > 1) and (MainForm.sgInvoiceTracker.RowCount > 1) then
  begin
    { ---------------------------------------------------------------------------------------------------------------------------------------- PREPARE TABLES }
    Table1:=HTMLTable;
    Table2:=HTMLTable;
    Table3:=HTMLTable;
    Table4:=HTMLTable;
    { ------------------------------------------------------------------------------------ MASTER LOOP - GO THROUGH ALL CUSTOMERS LOGGED WITH INVOICE TRACKER }
    for iCNT:=1 to MainForm.sgInvoiceTracker.ColCount - 1 do
    begin
      { DATA FOR COMPARISION }
      CUID     :=MainForm.sgInvoiceTracker.Cells[2,  iCNT];
      Reminder1:=MainForm.sgInvoiceTracker.Cells[12, iCNT];
      Reminder2:=MainForm.sgInvoiceTracker.Cells[13, iCNT];
      Reminder3:=MainForm.sgInvoiceTracker.Cells[14, iCNT];
      Reminder4:=MainForm.sgInvoiceTracker.Cells[15, iCNT];
      CoCode   :=MainForm.sgInvoiceTracker.Cells[4,  iCNT];
      Branch   :=MainForm.sgInvoiceTracker.Cells[5,  iCNT];
      REM_EX1  :=MainForm.sgInvoiceTracker.Cells[19, iCNT];
      REM_EX2  :=MainForm.sgInvoiceTracker.Cells[20, iCNT];
      REM_EX3  :=MainForm.sgInvoiceTracker.Cells[21, iCNT];
      REM_EX4  :=MainForm.sgInvoiceTracker.Cells[22, iCNT];
      REM_EX5  :=MainForm.sgInvoiceTracker.Cells[23, iCNT];
      { CUSTOMER NAME }
      CustName :=MainForm.sgInvoiceTracker.Cells[3,  iCNT];
      { -------------------------------------------------------------------------------------------------------------- LOCAL LOOP - GO THROUGH ALL OPEN ITEMS }
      for jCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
      begin
        { COMPARE 'CUID' IN BOTH TABLES }
        if (CUID = MainForm.sgOpenItems.Cells[37, jCNT])

        and

           { NOTE: WE EXCLUDE INVOICES WITH 'CONTROL STATUS' THAT }
           {       IS DIFFERENT THAN GIVEN NUMBER IN THE TABLE    }

           (

           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX1) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX2) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX3) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX4) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX5)

           )

        then
        begin

          { BUILD HTML CODE FOR CUSTOMER ADDRESS }
          CustAddr:='<p class="p"><b>' + CustName + '</b><br />' +#13#10;
          { ADD ADDRESS FIELD IF NOT EMPTY }
          if (MainForm.sgOpenItems.Cells[21, jCNT] <> '') and (MainForm.sgOpenItems.Cells[21, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[21, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[22, jCNT] <> '') and (MainForm.sgOpenItems.Cells[22, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[22, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[23, jCNT] <> '') and (MainForm.sgOpenItems.Cells[23, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[23, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[24, jCNT] <> '') and (MainForm.sgOpenItems.Cells[24, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[24, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[25, jCNT] <> '') and (MainForm.sgOpenItems.Cells[25, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[25, jCNT] + '<br />' +#13#10;
          CustAddr:=CustAddr + '</p>' +#13#10;
          { COMPARE 'PAYMENT STATUS' AND REQUIREMENTS FOR REMINDER 1..3 AND LEGAL }
          { REMINDER 1 }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder1 then
          begin
            { CHECK IF IT IS NOT ON INVOICE LIST, ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 0) = False then
            begin
              OpenItemsToHtmlTable(HTMLRem1, Rem1, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 1);
            end;
          end;
          { REMINDER 2 }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder2 then
          begin
            { CHECK IF IT IS ON INVOICE LIST MARKED AS '1', ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 1) then
            begin
              OpenItemsToHtmlTable(HTMLRem2, Rem2, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 2);
            end;
          end;
          { REMINDER 3 }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder3 then
          begin
            { CHECK IF IT IS ON INVOICE LIST MARKED AS '2', ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 2) then
            begin
              OpenItemsToHtmlTable(HTMLRem3, Rem3, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 3);
            end;
          end;
          { REMINDER 4 | LEGAL ACTION }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder4 then
          begin
            { CHECK IF IT IS ON INVOICE LIST MARKED AS '3', ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 3) then
            begin
              OpenItemsToHtmlTable(HTMLLega, Leg0, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 4);
            end;
          end;
        end;
      end;
      { ------------------------------------------------------------------------------------------------------------------------------- PROCESS THE REMINDERS }
      if Rem1 > 0 then SendReminderEmail(1, Rem1, Table1, HTMLRem1, MainForm.sgInvoiceTracker, iCNT);
      if Rem2 > 0 then SendReminderEmail(2, Rem2, Table2, HTMLRem2, MainForm.sgInvoiceTracker, iCNT);
      if Rem3 > 0 then SendReminderEmail(3, Rem3, Table3, HTMLRem3, MainForm.sgInvoiceTracker, iCNT);
      if Leg0 > 0 then SendReminderEmail(4, Leg0, Table4, HTMLLega, MainForm.sgInvoiceTracker, iCNT);
    end;
  end;
  SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
end;

{ ----------------------------------------------------------------------------------------------------------------------------- REFRESH INVOICE TRACKER TABLE }
procedure TInvoiceTracker.Refresh(var SG: TStringGrid; Param: string);
var
  { SQL }
  Query:     TADOQuery;
  StrSQL:    array of string;
  Columns:   string;
  Params:    string;
  AddWhere:  string;
  { COUNTERS }
  iCNT:      integer;
  jCNT:      integer;
  { CURRENT DATE/TIME }
  Stamp:     TDateTime;
  { STRING GRID }
begin
  { ---------------------------------------------------------------------------------------------------------------------------- DISABLE DRAWING 'STRINGGRID' }
  with SG do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  Query.SQL.Clear;
  Stamp:=Now;
  SetLength(StrSQL, 2);
  { --------------------------------------------------------------------------------------------------------------------------------------------- CHECK PARAM }
  if not (Param = 'ADD') and not (Param = 'REMOVE') then
  begin
    { ---------------------------------------------------------------------------------------------------------------------------------- BUILD SQL EXPRESSION }
    if Param <> 'ALL' then AddWhere:=' where tbl_tracker.user_alias = ' + QuotedStr(Param);
    StrSQL[0]:='SELECT '                                                      +
                 'tbl_tracker.user_alias       as ''User Alias'','            +
                 'tbl_tracker.cuid             as ''CUID'','                  +
                 'tbl_tracker.custname         as ''Customer Name'','         +
                 'tbl_tracker.co_code          as ''Co Code'','               +
                 'tbl_tracker.branch           as ''Agent'','                 +
                 'tbl_tracker.layout           as ''Applied Layout'','        +
                 'tbl_tracker.stamp            as ''Created'','               +
                 'tbl_company.send_note_from   as ''Send From'','             +
                 'tbl_addressbook.emails       as ''Reminder To'','           +
                 'tbl_addressbook.estatements  as ''Statement To'','          +
                 'tbl_company.legalto          as ''Notification To'','       +
                 'tbl_company.reminder1        as ''Timing (Reminder 1)'','   +
                 'tbl_company.reminder2        as ''Timing (Reminder 2)'','   +
                 'tbl_company.reminder3        as ''Timing (Reminder 3)'','   +
                 'tbl_company.legalaction      as ''Timing (Legal Note)'','   +
                 'tbl_company.first_statement  as ''1st Statement'','         +
                 'tbl_company.second_statement as ''2nd Statement'','         +
                 'tbl_company.stat_except      as ''Statement exception'','   +
                 'tbl_company.rem_ex1          as ''Exception (1)'','         +
                 'tbl_company.rem_ex2          as ''Exception (2)'','         +
                 'tbl_company.rem_ex3          as ''Exception (3)'','         +
                 'tbl_company.rem_ex4          as ''Exception (4)'','         +
                 'tbl_company.rem_ex5          as ''Exception (5)'','         +
                 'tbl_company.bankdetails      as ''Bank Account'','          +
                 'tbl_company.coname           as ''Co Name'','               +
                 'tbl_company.coaddress        as ''Co Address'','            +
                 'tbl_company.vatno            as ''VAT number'','            +
                 'tbl_company.telephone        as ''Phone number'' '          +
               'FROM '                                                        +
                 'tbl_tracker '                                               +
               'LEFT JOIN '                                                   +
                 'tbl_addressbook on tbl_tracker.cuid = tbl_addressbook.cuid '+
               'LEFT JOIN '                                                   +
                 'tbl_company on (tbl_tracker.co_code = tbl_company.co_code and tbl_tracker.branch = tbl_company.branch)' + AddWhere + ' ORDER BY tbl_tracker.co_code ASC ';
    Query.SQL.Text:=StrSQL[0];
    { ----------------------------------------------------------------------------------------------------------------------------- CONNECT, READ AND DISPLAY }
    try
      Query.Open;
      if Query.RecordCount > 0 then
      begin
        SG.ClearAll(2, 1, 1, False);
        iCNT:=0;
        SG.RowCount:=Query.RecordCount + 1;
        SG.ColCount:=Query.Recordset.Fields.Count + 1;
        while not Query.Recordset.EOF do
        begin
          for jCNT:=0 to Query.Recordset.Fields.Count - 1 do
          begin
            if iCNT = 0 then SG.Cells[jCNT + 1, iCNT]:=Query.Recordset.Fields[jCNT].Name;
            SG.Cells[jCNT + 1, iCNT + 1]:=VarToStr(Query.Recordset.Fields[jCNT].Value);
          end;
          Query.Recordset.MoveNext;
          inc(iCNT);
        end;
      end;
      Query.Close;
      SG.SetColWidth(10, 20);
    finally
      Query.Free;
      if not SG.Enabled then SG.Enabled:=True;
    end;
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------------- CHECK PARAM }
  if (Param = 'ADD') or (Param = 'REMOVE') then
  begin
    { ADD ITEM TO INVOICE TRACKER LIST }
    if Param = 'ADD' then
    begin
      { --------------------------------------------------------------------------------------------------- CHECK IF SELECTED CUSTOMER IS ALREADY ON THE LIST }
      StrSQL[0]:='SELECT user_alias FROM tbl_tracker WHERE CUID = :uParam';
      Query.SQL.Add(StrSQL[0]);
      Query.Parameters.ParamByName('uParam').Value:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1), MainForm.sgAgeView.Row];
      try
        Query.Open;
        { ALREADY EXISTS }
        if Query.RecordCount > 0 then SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('This customer is already on the Invoice Tracker list.')));
        { NOT ON THE LIST }
        if Query.RecordCount = 0 then
        begin
          { ---------------------------------------------------------------------------------------------------------------------------- BUILD SQL EXPRESSION }
          Query.SQL.Clear;
          Columns:='USER_ALIAS, CUID, CO_CODE, BRANCH, CUSTNAME, LAYOUT, STAMP';
          Params:=':uParam1, :uParam2, :uParam3, :uParam4, :uParam5, :uParam6, :uParam7';
          StrSQL[0]:='INSERT INTO tbl_tracker (' + Columns + ') VALUES (' + Params + ')';
          Query.SQL.Add(StrSQL[0]);
          { --------------------------------------------------------------------------------------------------------------------------- ASSIGN ALL PARAMETERS }
          Query.Parameters.ParamByName('uParam1').Value :=UpperCase(Settings.WinUserName);
          Query.Parameters.ParamByName('uParam2').Value :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID',   1, 1),  MainForm.sgAgeView.Row];
          Query.Parameters.ParamByName('uParam3').Value :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CO CODE', 1, 1), MainForm.sgAgeView.Row];
          Query.Parameters.ParamByName('uParam4').Value :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('AGENT',   1, 1), MainForm.sgAgeView.Row];
          Query.Parameters.ParamByName('uParam5').Value :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUSTOMER NAME', 1, 1), MainForm.sgAgeView.Row];
          Query.Parameters.ParamByName('uParam6').Value:=TrackerForm.LayoutList.Text;
          Query.Parameters.ParamByName('uParam7').Value:=Stamp;
          { ----------------------------------------------------------------------------------------------------------------------------------------- EXECUTE }
          try
            Query.ExecSQL;
          finally
            Query.Close;
            if not SG.Enabled then SG.Enabled:=True;
          end;
          SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Customer has been added to the Invoice Tracker.')));
        end;
      finally
        Query.Free;
      end;
    end;
    { ---------------------------------------------------------------------------------------------------------------------------------- REMOVE FROM THE LIST }
    { 'TBL_TRACKER' IS RELATED WITH 'TBL_INVOICES'. WE USE ONE-TO-MANY RELATIONSHIP. TBL_TRACKER CONTAINS WITH CUSTOMER 'UNDER WATCH' WHILE 'TBL_INVOICES'    }
    { CONTAINS WITH INVOICES THAT HAS BEEN ACTUALLY SENT (IF ANY) THEREFORE, WE MUST REMOVE ITEMS FROM 'TBL_INVOICES' AND THEN 'TBL_TRACKER', NOT THE         }
    { OTHER WAY. IF THERE IS NO INVOICES ON THE 'TBL_INVOICES' LIST, DELETE STATEMENT RETURNS ONLY '0 ROWS AFFECTED'.                                         }
    if Param = 'REMOVE' then
    begin
      StrSQL[0]:='DELETE FROM tbl_invoices WHERE CUID = :uParam';
      StrSQL[1]:='DELETE FROM tbl_tracker WHERE CUID = :uParam';
      try
        for iCNT:=0 to 1 do
        begin
          Query.SQL.Clear;
          Query.SQL.Add(StrSQL[iCNT]);
          Query.Parameters.ParamByName('uParam').Value:=SG.Cells[2, SG.Row];
          Query.ExecSQL;
        end;
      finally
        Query.Free;
        { REMOVE FROM 'STRINGGRID' }
        SG.DeleteRowFrom(1, 1);
      end;
    end;
  end;
  { ----------------------------------------------------------------------------------------------------------------------------- ENABLE DRAWING 'STRINGGRID' }
  with SG do SendMessage(Handle, WM_SETREDRAW, 1, 0);
  SG.Repaint;
end;

{ ############################################################## ! MAILER CLASS ! ########################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- MAILER }
constructor TMailer.Create;
begin
  (* INITIALIZE PRIVATE VARIABES *)
  pidThd   :=0;
  pXMailer :='';
  pmFrom   :='';
  pmTo     :='';
  pmCc     :='';
  pmBcc    :='';
  pmRt     :='';
  pmSubject:='';
  pmBody   :='';
  pmLogo   :='';
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- SEND USING CDOSYS | NTLM }
function TMailer.SendCDOSYS: boolean;
var
  CdoMessage:  CDO_TLB.IMessage;
  Schema:      string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  CdoMessage:=CDO_TLB.CoMessage.Create;
  { -------------------------------------------------------------------------------------------------------------------------------- ASSIGN | CANNOT BE EMPTY }
  CdoMessage.From:=MailFrom;
  CdoMessage.To_ :=MailTo;
  CdoMessage.CC  :=MailCc;
  { ---------------------------------------------------------------------------------------------------------------- OPTIONAL | CAN BE EMPTY, THEN WE SKIP IT }
  if MailBcc <> '' then CdoMessage.BCC    :=MailBcc;
  if MailRt  <> '' then CdoMessage.ReplyTo:=MailRt;
  { ------------------------------------------------------------------------------------------------------------------------------------- SUBJECT & HTML BODY }
  CdoMessage.Subject :=MailSubject;
  CdoMessage.HTMLBody:=MailBody;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- CONFIGURE }
  Schema:='http://schemas.microsoft.com/cdo/configuration/';
  with CdoMessage.Configuration.Fields do
  begin
    item[Schema + 'sendusing'       ].Value:=2; (* SEND THE MESSAGE USING THE NETWORK *)
    item[Schema + 'smtpserver'      ].Value:=Settings.TMIG.ReadString(Settings.MailerCDOSYS, 'SMTP', '');
    item[Schema + 'smtpserverport'  ].Value:=Settings.TMIG.ReadString(Settings.MailerCDOSYS, 'PORT', '');
    item[Schema + 'smtpauthenticate'].Value:=2; (* NTLM *)
    item[Schema + 'NNTPAccountName' ].Value:=XMailer;
    update;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------------------- SEND }
  try
    CdoMessage.BodyPart.Charset:='utf-8';
    CdoMessage.Send;
    Result:=True;
  except
    on E: Exception do
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Could not send an e-mail. Error message thrown: ' + E.Message);
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- SEND USING SYNAPSE }
function TMailer.SendSynapse;
var
  Email:        TSMTPSend;
  MailContent:  TStringList;
  Msg:          TMimeMess;
  SendTo:       TStringList;
  SendCc:       TStringList;
  SendBc:       TStringList;
  Delimiter:    char;
  iCNT:         integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result     :=False;
  Delimiter  :=';';
  SendTo     :=TStringList.Create;
  SendCc     :=TStringList.Create;
  SendBc     :=TStringList.Create;
  MailContent:=TStringList.Create;
  Msg        :=TMimeMess.Create;
  Email      :=TSMTPSend.Create;
  try
    try
      { ------------------------------------------------------------------------------------------------------------------------ ADD PRE-PREPARED E-MAIL BODY }
      MailContent.Add(MailBody);
      { -------------------------------------------------------------------------------------------------------------------------------------- E-MAIL HEADERS }
      Msg.Header.From    :=XMailer;
      Msg.Header.Priority:=mp_High;
      Msg.Header.Subject :=MailSubject;
      Msg.Header.XMailer :=XMailer;
      if MailRt <> '' then Msg.Header.ReplyTo:=MailRt;

      (* SCAN FOR ALL EMAILS DELIMINATED BY SEMI-COLON          *)
      (* AND ADD THEM TO 'MAIL TO', 'CARBON COPY' AND           *)
      (* 'BLIND CARBON COPY' RESPECTIVELY                       *)
      (* ALL ARRAYS WILL BE MERGED LATER AND WILL BE ADDED TO   *)
      (* SMTP BEFORE SEND AN EMAIL                              *)

      { -------------------------------------------------------------------------------------------------------------------------------------------------- TO }
      SendTo.Delimiter      :=Delimiter;
      SendTo.StrictDelimiter:=False;
      SendTo.DelimitedText  :=MailTo;
      for iCNT:=0 to SendTo.Count - 1 do Msg.Header.ToList.Add(SendTo[iCNT]);
      { ----------------------------------------------------------------------------------------------------------------------------------------- CARBON COPY }
      if MailCc <> '' then
      begin
        SendCc.Delimiter      :=Delimiter;
        SendCc.StrictDelimiter:=False;
        SendCC.DelimitedText  :=MailCc;
        for iCNT:=0 to SendCc.Count - 1 do Msg.Header.CCList.Add(SendCc[iCNT]);
      end;
      { ----------------------------------------------------------------------------------------------------------------------------------- BLIND CARBON COPY }
      if MailBcc <> '' then
      begin
        SendBc.Delimiter      :=Delimiter;
        SendBc.StrictDelimiter:=False;
        SendBC.DelimitedText  :=MailBcc;
      end;
      { ------------------------------------------------------------------------------------------------------------------------------------------ SETTING UP }
      Msg.AddPartHTML(MailContent, nil);
      Msg.EncodeMessage;
      { ------------------------------------------------------------------------------------------------------------------------ EMAIL CREDENTIALS AND SERVER }
      Email.UserName  :=Settings.TMIG.ReadString(Settings.MailerSynapse, 'USERNAME', '');
      Email.Password  :=Settings.TMIG.ReadString(Settings.MailerSynapse, 'PASSWORD', '');
      Email.TargetHost:=Settings.TMIG.ReadString(Settings.MailerSynapse, 'SMTP', '');
      Email.TargetPort:=Settings.TMIG.ReadString(Settings.MailerSynapse, 'PORT', '');
      { ---------------------------------------------------------------------------------------------------------------------------------------- TLS OVER SSL }
      if Settings.TMIG.ReadBool(Settings.MailerSynapse, 'TLS', False) = True then
      begin
        Email.AutoTLS:=True;
        Email.FullSSL:=False;
      end;
      { ---------------------------------------------------------------------------------------------------------------------------------------- SSL OVER TLS }
      if Settings.TMIG.ReadBool(Settings.MailerSynapse, 'SSL', True) = True then
      begin
        Email.AutoTLS:=False;
        Email.FullSSL:=True;
      end;
      { -------------------------------------------------------------------------------------------------------------------------------------- SENDING E-MAIL }
      if Email.Login then
      begin
        if Email.AuthDone then
        begin
          Email.MailFrom(MailFrom, length(MailFrom));

          (* ADD ALL RECIPIENTS REGARDLESS IF BCC OR NOT *)
          (* BCC CANNOT BE INCLUDED IN HEADERS           *)
          (* ADD ONE PER 'MAILTO' FUNCTION               *)

          for iCNT:=0 to SendTo.Count - 1 do Email.MailTo(SendTo[iCNT]);
          for iCNT:=0 to SendCc.Count - 1 do Email.MailTo(SendCc[iCNT]);
          for iCNT:=0 to SendBc.Count - 1 do Email.MailTo(SendBc[iCNT]);
          { --------------------------------------------------------------------------------------------------------------------------------- SEND AND LOGOUT }
          if Email.MailData(Msg.Lines) then
          begin
            Result:=True;
            Email.Logout;
          end;
        end;
      end;
    except
      on E: Exception do
        LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Could not send an e-mail. Error message thrown: ' + E.Message);
    end;
  finally
    MailContent.Free;
    Msg.Free;
    Email.Free;
    SendTo.Free;
    SendCc.Free;
    SendBc.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SEND E-MAIL }
function TMailer.SendNow: boolean;  (* ASYNC *)
begin
  Result:=False;

  (* NTLM AUTHENTICATE ONLY *)
  if Settings.TMIG.ReadString(Settings.MailerSetup, 'ACTIVE', '') = Settings.MailerCDOSYS then Result:=SendCDOSYS;

  (* REQUIRE USERNAME AND PASSWORD, SSL/TLS *)
  if Settings.TMIG.ReadString(Settings.MailerSetup, 'ACTIVE', '') = Settings.MailerSYNAPSE then Result:=SendSynapse;

end;

{ ########################################################## ! ADRESSBOOK MAILER CLASS ! #################################################################### }

                                                                      //refactor!!!

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TAddressBook.Create(UseDelimiter: string; UserNameDef: string);
begin
  (* INITIALIZE WITH GIVEN PARAMETERS *)
  pDelimiter:=UseDelimiter;
  pUserName :=UserNameDef;
  pidThd    :=0;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ READ }
function TAddressBook.Read: boolean;
var
  Columns:  string;
  StrSQL:   string;
  MSSQL:    TMSSQL;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  MSSQL:=TMSSQL.Create(Database.ADOConnect);
  { ------------------------------------------------------------------------------------------------------------------------------------------- COLUMNS SETUP }
(*
  Columns:='USER_ALIAS  AS ''User Alias '','         +
           'CUID,'                                   +
           'CUSTNUMBER  AS ''Customer Number'','     +
           'CUSTNAME    AS ''Customer Name'','       +
           'EMAILS      AS ''E-mails (general)'','   +
           'ESTATEMENTS AS ''E-mails (statements)'','+
           'TELEPHONE   AS ''Phone Number'','        +
           'CONTACT     AS ''Contact Person'','      +
           'CUSTADDR    AS ''Customer Address'''     ;
*)
  Columns:='USER_ALIAS,'     +
           'CUID,'           +
           'CUSTNUMBER,'     +
           'CUSTNAME,'       +
           'EMAILS      ,'   +
           'ESTATEMENTS ,'   +
           'TELEPHONE   ,'   +
           'CONTACT     ,'   +
           'CUSTADDR    '    ;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- SQL QUERY }
  if UserName = '' then
    StrSQL:='SELECT ' + Columns + ' FROM tbl_addressbook ORDER BY ID ASC'
      else
        StrSQL:='SELECT ' + Columns + ' FROM tbl_addressbook WHERE USER_ALIAS = ' + QuotedStr(UserName) + ' ORDER BY ID ASC';
  { ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE SQL COMMAND }
  try
    MSSQL.StrSQL:=StrSQL;
    Result:=MSSQL.SqlToGrid(MainForm.sgAddressBook, MSSQL.OpenSQL, True);
  finally
    MSSQL.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------- EVENT LOG AND MESSAGE WINDOW }
  if not (Result) then
  begin
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Unexpected error. Cannot open Address Book.');
    SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('Cannot open Address Book for user "' + UserName + '". Please contact IT support.')));
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- WRITE }
function TAddressBook.Write: boolean;
var
  Columns:  string;
  MSSQL:    TMSSQL;
  iCNT:     integer;
  Start:    integer;
  RS:       _Recordset;
  TheSame:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result :=False;
  Start  :=0;
  TheSame:=0;
  MSSQL  :=TMSSQL.Create(Database.ADOConnect);
  { ------------------------------------------------------------------------------------------------------------------------------------------------- COLUMNS }

  (* NOTE: COLUMN ORDER MUST BE THE SAME AS IN GIVEN STRING GRID *)

  Columns:='USER_ALIAS,' +
           'CUID,'       +
           'CUSTNUMBER,' +
           'CUSTNAME,'   +
           'EMAILS,'     +
           'ESTATEMENTS,'+
           'TELEPHONE,'  +
           'CONTACT,'    +
           'CUSTADDR'    ;
  { ----------------------------------------------------------------------------------------------------------------- PERFORM INSERT ON NEWLY ADDED ROWS ONLY }
  for iCNT:=1 to MainForm.sgAddressBook.RowCount - 1 do
    if MainForm.sgAddressBook.Cells[0, iCNT] = '' then
    begin
      Start:=iCNT;
      Break;
    end;
  { -------------------------------------------------------------------------------------------------------------------------- MAKE SURE THAT NEW ROWS EXISTS }
  if not (Start = 0) then begin
    { ---------------------------------------------------------------------------------------------------------------------------- CHECK IF CUID ALREADY EXISTS }
    for iCNT:=Start to MainForm.sgAddressBook.RowCount - 1 do
    begin
      MSSQL.StrSQL:='SELECT CUID FROM tbl_AddressBook WHERE CUID = ' + MSSQL.CleanStr(MainForm.sgAddressBook.Cells[2, iCNT], True);
      RS:=MSSQL.OpenSQL;
      if RS.RecordCount > 0 then Inc(TheSame);
    end;
    { ----------------------------------------------------------------------------------------------------------------------------------- PROCESS BUILT QUERY }
    if TheSame = 0 then
    begin
      try
        MSSQL.ClearSQL;
        MSSQL.StrSQL:=MSSQL.GridToSql(MainForm.sgAddressBook, 'tbl_AddressBook', Columns, Start, 1);
        { RE-DO LIST POSITION }
        for iCNT:=1 to MainForm.sgAddressBook.RowCount - 1 do MainForm.sgAddressBook.Cells[0, iCNT]:= IntToStr(iCNT);
        Result:=MSSQL.ExecSQL;
      finally
        MSSQL.Free;
        LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: New records have beed successfully added to Address Book.');
        SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('New records have beed saved successfully!')));
      end;
    end
      else
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('Cannot add already existing customers.' + #13#10 + 'Please re-check and remove doubles before next attempt.')));
  end
    else
      SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('No new records have beed found. Process has been stopped.')));
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- IMPORT }
function TAddressBook.ImportCSV: boolean;
var
  iCNT:       integer;
  jCNT:       integer;
  Count:      integer;
  Data:       TStringList;
  Transit:    TStringList;
  fPath:      string;
  IsError:    boolean;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  IsError:=False;
  Count:=0;
  { ----------------------------------------------------------------------------------------------------------------------------- GET THE FILE PATH AND PARSE }
  if MainForm.CSVImport.Execute = True then
  begin
    fPath:=MainForm.CSVImport.FileName;
    Data:=TStringList.Create;
    Transit:=TStringList.Create;
    try
      { LOAD DATA }
      Data.LoadFromFile(fPath);
      { COUNT ALL COLUMNS }
      for iCNT:=0 to Length(Data[0]) do if copy(Data[0], iCNT, 1) = Delimiter then inc(Count);
      { COUNT ALL ROWS & SETUP OFFSET }
      MainForm.sgAddressBook.RowCount:=Data.Count + 1;
      { SETUP TRANSIT THAT WILL HOLD SPLIT LINE }
      Transit.StrictDelimiter:=True;
      Transit.Delimiter:=Delimiter[1];
      { ITERATE THROUGH ALL ROWS }
      try
        for iCNT:= 0 to Data.Count - 1 do
        begin
          { SPLIT STRING USING GIVEN DELIMITER }
          Transit.DelimitedText:=Data[iCNT];
          for jCNT:=1 to Count do MainForm.sgAddressBook.Cells[jCNT, iCNT + 1]:=Transit[jCNT - 1];
          MainForm.sgAddressBook.Cells[0, iCNT + 1]:=IntToStr((iCNT + 1));
          Transit.Clear;
        end;
        Result:=True;
      { ---------------------------------------------------------------------------------------------------------------------------------------- ON EXCEPTION }
      except
        on E: Exception do
        begin
          LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: CSV Import faild: ' + ExtractFileName(fPath));
          SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('CSV Import faild. Please check the file and try again.')));
          IsError:=True;
        end;
      end;
    { ----------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
    finally
      Data.Free;
      Transit.Free;
      PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
      if not IsError then
      begin
        LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Data has been imported successfully!');
        SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Data has been imported successfully!')));
      end;
    end;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- EXPORT }
function TAddressBook.ExportCSV: boolean;
var
  iCNT:       integer;
  jCNT:       integer;
  fPath:      string;
  CSVData:    TStringList;
  MyStr:      string;
  CleanStr:   string;
  IsError:    boolean;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  IsError:=False;
  CSVData:=TStringList.Create;
  { ------------------------------------------------------------------------------------------------------------------------------------------ WRITE CSV FILE }
  try
    { ADD ROWS AND COLUMNS WITH DELIMITER }
    for iCNT:=1 to MainForm.sgAddressBook.RowCount - 1 do
    begin
      for jCNT:= 1 to MainForm.sgAddressBook.ColCount - 1 do
      begin
        CleanStr :=MainForm.sgAddressBook.Cells[jCNT, iCNT];
        CleanStr :=StringReplace(CleanStr, #13#10, ' ', [rfReplaceAll]);
        MyStr    :=MyStr + CleanStr + Delimiter;
      end;
      CSVData.Add(MyStr);
      MyStr:='';
    end;
    { SAVE TO FILE AS PLAIN TEXT }
    try
      if MainForm.CSVExport.Execute = True then CSVData.SaveToFile(MainForm.CSVExport.FileName);
      Result:=True;
    { ------------------------------------------------------------------------------------------------------------------------------------------ ON EXCEPTION }
    except
      on E: Exception do
      begin
        LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Cannot saved file: ' + ExtractFileName(fPath));
        SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('Cannot save the file in the given location.')));
        IsError:=True;
      end;
    end;
  { ------------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
  finally
    CSVData.Free;
    if not IsError then
    begin
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Data has been exported successfully!');
      SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Address Book have been exported successfully!')));
    end;
  end;
end;

{ ############################################################## ! AGE VIEW CLASS ! ######################################################################### }

                                                                    //refactor!!!

(* ******************************************************* ! AGE VIEW FOR GIVEN GROUP ID ! ********************************************************************

IMPORTANT NOTE:
---------------

DATA SNAPSHOT FOR AGING CONTAINS WITH 0..28 COLUMNS. HOWEVER, FOR USER 'AGING VIEW' WE SHOW ONLY 0..25 COLUMNS.

DATA SNAPSHOT   | NR | AGE VIEW COLUMN | NOTE
----------------|----|-----------------|-------------------------------------------------------------------------------
ID              | 0  |                 | TECHNICAL COLUMN (DO NOT QUERY)
GROUP_ID        | 1  |                 | TECHNICAL COLUMN (DO NOT QUERY)
AGE_DATE        | 2  |                 | TECHNICAL COLUMN (DO NOT QUERY)
SNAPSHOT_DT     | 3  |                 | TECHNICAL COLUMN (DO NOT QUERY)
CUSTOMER_NAME   | 4  |  2              | RENAME AS PER GENERAL SETTINGS
CUSTOMER_NUMBER | 5  |  1              | RENAME AS PER GENERAL SETTINGS
COUNTRY_CODE    | 6  |  16             | RENAME AS PER GENERAL SETTINGS
NOT_DUE         | 7  |  3              | RENAME AS PER GENERAL SETTINGS
RANGE1          | 8  |  4              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE2          | 9  |  5              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE3          | 10 |  6              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE4          | 11 |  7              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
RANGE5          | 12 |  8              | TAKE FROM CAPTIONS, IT DEPENDS ON GENERAL SETTINGS
TOTAL           | 13 |  9              | RENAME AS PER GENERAL SETTINGS
CREDIT_LIMIT    | 14 |  10             | RENAME AS PER GENERAL SETTINGS
EXCEEDED_AMOUNT | 15 |  11             | RENAME AS PER GENERAL SETTINGS
PAYMENT_TERMS   | 16 |  17             | RENAME AS PER GENERAL SETTINGS
AGENT           | 17 |  14             | RENAME AS PER GENERAL SETTINGS
DIVISION        | 18 |  15             | RENAME AS PER GENERAL SETTINGS
CO_CODE         | 19 |  13             | RENAME AS PER GENERAL SETTINGS
LEDGER_ISO      | 20 |  12             | RENAME AS PER GENERAL SETTINGS
INF4            | 21 |  18             | RENAME AS PER GENERAL SETTINGS
INF7            | 22 |  19             | RENAME AS PER GENERAL SETTINGS
PERSON          | 23 |  20             | RENAME AS PER GENERAL SETTINGS
GROUP3          | 24 |  21             | RENAME AS PER GENERAL SETTINGS
RISK_CLASS      | 25 |  22             | RENAME AS PER GENERAL SETTINGS
QUALITY_IDX     | 26 |  23             | RENAME AS PER GENERAL SETTINGS, TECHNICAL COLUMN, CAN BE HIDDEN
WALLET_SHARE    | 27 |  24             | RENAME AS PER GENERAL SETTINGS, TECHNICAL COLUMN, CAN BE HIDDEN
CUID            | 28 |  25             | UNIQUE CUSTOMER ID ACROSS ALL ITEMS IN THE TABLE PER GIVEN DAY, IT CAN BE RENAMED AND CAN BE HIDDEN

************************************************************************************************************************************************************ *)

procedure TAgeView.Read(GroupID: string; AgeDate: TDateTime; idThd: integer);  (* ASYNC *)
{ ------------------------------------------------ ! VARIABES & CONSTANTS FOR INNER & MAIN BLOCKS ! --------------------------------------------------------- }
var
  { COUNTER }
  iCNT:          integer;
  jCNT:          integer;
  { SQL }
  StrSQL:        string;
  StrCol:        string;
  Query:         TADOQuery;
{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------- DISABLE DRAWING }
  with MainForm.sgAgeView do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  { TOTALS }
  AgeView.CustAll    :=0;
  AgeView.CallsAll   :=0; { <== QUERY SEPARATE TABLE THAT HOLDS EMAILS AND CALLS FOR SPECIFIC DATE AND GROUP NUMBER }
  AgeView.EmailsAll  :=0; { <== QUERY SEPARATE TABLE THAT HOLDS EMAILS AND CALLS FOR SPECIFIC DATE AND GROUP NUMBER }
  { AMOUNTS }
  AgeView.NotDue     :=0;
  AgeView.Range1     :=0;
  AgeView.Range2     :=0;
  AgeView.Range3     :=0;
  AgeView.Range4     :=0;
  AgeView.Range5     :=0;
  AgeView.Range6     :=0;
  AgeView.Balance    :=0;
  AgeView.Limits     :=0;
  AgeView.Exceeders  :=0;
  AgeView.TotalExceed:=0;
  AgeView.RCA        :=0;
  AgeView.RCB        :=0;
  AgeView.RCC        :=0;
  { ------------------------------------------------------------------------------------------------------------------------------------ BUILD SQL EXPRESSION }
  { READ GRID LAYOUT, HOLDS COLUMN ORDER TO BE PASSED AS PARAMETER TO SQL EXPRESSION }
  MainForm.sgAgeView.LoadLayout(StrCol, Settings.ColumnWidthName, Settings.ColumnOrderName, Settings.ColumnNames, Settings.ColumnPrefix);

  (* DISPLAY COLUMNS FROM TABLE THAT HOLDS AGE DETAILS AND FOLLOWUP COLUMN *)
  (* WARNING! WE ALWAYS USE LEFT JOIN ON TWO TABLES                        *)

  StrSQL:='SELECT '                                 +
             StrCol                                 +
          'FROM '                                   +
            'tbl_snapshots '                        +
          'LEFT JOIN '                              +
          '  tbl_general '                          +
          'ON '                                     +
          '  tbl_snapshots.cuid = tbl_general.cuid '+
          'WHERE '                                  +
            'tbl_snapshots.GROUP_ID = :uParam1 '    +
            'AND tbl_snapshots.AGE_DATE = :uParam2 '+
          'ORDER BY '                               +
            'tbl_snapshots.RISK_CLASS ASC, '        +
            'tbl_snapshots.QUALITY_IDX ASC, '       +
            'tbl_snapshots.TOTAL DESC;'             ;
//            'tbl_general.FOLLOWUP DESC;'            ;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- NEW QUERY }
  { QUERY WITH GIVEN SQL EXPRESSION AND PARAMETERS }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Parameters.ParamByName('uParam1').Value:=GroupID;
    Query.Parameters.ParamByName('uParam2').Value:=AgeDate;
    { EXECUTE SQL }
    Query.Open;
    { CLEAR 'STRINGGRID' }
    for iCNT:=1 to MainForm.sgAgeView.RowCount do for jCNT:=1 to MainForm.sgAgeView.ColCount do MainForm.sgAgeView.Cells[jCNT, iCNT]:='';
    { ASSIGN COLUMNS [FIELDS] AND ROWS [RECORDS] }
    iCNT:=1;
    MainForm.sgAgeView.RowCount:=Query.RecordCount + 1;
    MainForm.sgAgeView.ColCount:=Query.Recordset.Fields.Count + 1;
    { MOVE DATA FROM RECORDSET TO 'STRINGGRID' }
    while not Query.Recordset.EOF do
    begin
      for jCNT:=1 to Query.Recordset.Fields.Count do MainForm.sgAgeView.Cells[jCNT, iCNT]:=VarToStr(Query.Recordset.Fields[jCNT - 1].Value);
      Query.Recordset.MoveNext;
      inc(iCNT);
    end;
  finally
    Query.Close;
    Query.Free;
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: SQL statement applied [' + StrSQL + '].');
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: SQL statement parameters [uParam1 = ' + GroupID + '], [uParam2 = ' + DateToStr(AgeDate) + '].');
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- CALCULATE VALUES }
  for iCNT:=1 to MainForm.sgAgeView.RowCount - 1 do
  begin
    { NOT DUE & RANGE1..5 }
    AgeView.NotDue:=AgeView.NotDue + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('NOT DUE', 1, 1), iCNT], 0);
    AgeView.Range1:=AgeView.Range1 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('1 - 7',  1, 1), iCNT], 0);
    AgeView.Range2:=AgeView.Range2 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('8 - 30',  1, 1), iCNT], 0);
    AgeView.Range3:=AgeView.Range3 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('31 - 60',  1, 1), iCNT], 0);
    AgeView.Range4:=AgeView.Range4 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('61 - 90',  1, 1), iCNT], 0);
    AgeView.Range5:=AgeView.Range5 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('91 - 120',  1, 1), iCNT], 0);
    AgeView.Range6:=AgeView.Range6 + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('120 - oo',  1, 1), iCNT], 0);
    { TOTAL AMOUNT | LEDGER BALANCE }
    AgeView.Balance:=AgeView.Balance + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    { GRANTED LIMITS | SUM OF ALL LIMITS }
    AgeView.Limits:=AgeView.Limits + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CREDIT LIMIT', 1, 1), iCNT], 0);
    { EXCEEDERS }
    if StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('EXCEEDED AMOUNT', 1, 1), iCNT], 0) < 0 then
    begin
      { COUNT EXCEEDERS }
      inc(AgeView.Exceeders);
      { SUM ALL EXCEEDERS AMOUNT }
      AgeView.TotalExceed:=AgeView.TotalExceed + Abs(StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('EXCEEDED AMOUNT', 1, 1), iCNT], 0));
    end;
    { SUM ALL ITEMS FOR RISK CLASSES }
    if MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('RISK CLASS', 1, 1), iCNT] = 'A' then AgeView.RCA:=AgeView.RCA + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    if MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('RISK CLASS', 1, 1), iCNT] = 'B' then AgeView.RCB:=AgeView.RCB + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    if MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('RISK CLASS', 1, 1), iCNT] = 'C' then AgeView.RCC:=AgeView.RCC + StrToFloatDef(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('TOTAL', 1, 1), iCNT], 0);
    { COUNT ITEMS }
    inc(AgeView.CustAll);
  end;
  MainForm.sgAgeView.DefaultRowHeight:=17;
  { ----------------------------------------------------------------------------------------------------------------------- HIDE SOME COULMNS IN 'BASIC' MODE }

  { BASIC VIEW }
  if UpperCase(Database.AccessMode) = 'BASIC' then MainForm.Action_BasicViewClick(self);

  { FULL VIEW }
  if UpperCase(Database.AccessMode) = 'FULL' then mainForm.Action_FullViewClick(self);

  { ------------------------------------------------------------------------------------------------------------------------------------------ ENABLE DRAWING }
  with MainForm.sgAgeView do SendMessage(Handle, WM_SETREDRAW, 1, 0);
  MainForm.sgAgeView.Repaint;
end;

(* ********************************************************* ! PRE-PARE AGE VIEW ! ****************************************************************************

NOTES:
------

S - SOURCE TO BE MOVE 'AS IS'
D - DESTINATION
C - CALCULATED 'ON THE FLY'

WARNING!

IF NUMBERS ARE PROVIDED WITH NON-ENGLISH FORMAT '100000,00', THEN WE MUST REPLACE IT BY DOT DECIMAL SEPARATOR TO SENT SQL QUERY.
SUCH REPLACEMENT CAN BE OMITTED IF SOURCE IS ALREADY PRESENTED WITH DECIMAL POINT SEPARATOR.

OPEN ITEMS OWNLOADED FROM SOURCE FILE | AGE VIEW MADE FROM OPEN ITEMS      | COLUMN     | WORKER ARRAY
--------------------------------------|------------------------------------|------------|---------------
COLUMN NUMBER   | FIELD NAME          | COLUMN NUMBER   | FIELD NAME       | ASSIGNMENT | COLUMN NUMBER
----------------|---------------------|-----------------|------------------|------------|---------------
 0              | LP                  |  0              | ID               |            | -
 1            S | Co Code             |  1              | GROUP_ID         |            | 0
 2            S | Cust. Number        |  2              | AGE_DATE         |            | 1
 3              | Voucher Type        |  3              | SNAPSHOT_DT      |            | 2
 4              | Voucher Date        |  4              | CUSTOMER_NAME    | D          | 3
 5              | O/S in currency     |  5              | CUSTOMER_NUMBER  | D          | 4
 6              | O/S amount          |  6              | COUNTRY_CODE     | D          | 5
 7            S | Cust. Name          |  7              | NOT_DUE          | C          | 6
 8              | Currency            |  8              | RANGE1           | C          | 7
 9              | Amount in currency  |  9              | RANGE2           | C          | 8
 10             | Amount              |  10             | RANGE3           | C          | 9
 11             | Invoice Number      |  11             | RANGE4           | C          | 10
 12             | Due Date            |  12             | RANGE5           | C          | 11
 13           S | INF4                |  13             | RANGE6           | C          | 12
 14           S | INF7                |  14             | OVERDUE          | C          | 13
 15           S | Credit Limit        |  15             | TOTAL            | C          | 14
 16           S | Country Code        |  16  NULLABLE   | CREDIT_LIMIT     | D          | 15
 17           S | Payment Terms       |  17  NULLABLE   | EXCEEDED_AMOUNT  | C          | 16
 18             | Paid Info           |  18  NULLABLE   | PAYMENT_TERMS    | D          | 17
 19           S | Agent               |  19             | AGENT            | D          | 18
 20             | Control Status      |  20             | DIVISION         | D          | 19
 21             | Address 1           |  21  NULLABLE   | CO_CODE          | D          | 20
 22             | Address 2           |  22  NULLABLE   | LEDGER_ISO       | C          | 21
 23             | Address 3           |  23  NULLABLE   | INF4             | D          | 22
 24             | Postal Number       |  24  NULLABLE   | INF7             | D          | 23
 25             | Postal Area         |  25             | PERSON           | D          | 24
 26             | GL Account          |  26             | GROUP3           | D          | 25
 27             | Value Date          |  27  NULLABLE   | RISK_CLASS       | C          | 26
 28           S | Division            |  28  NULLABLE   | QUALITY_IDX      | C          | 27
 29           S | Group 3             |  29             | WALET_SHARE      | C          | 28
 30             | Text                | -               | CUID             | D          | 29   6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 26, 27, 28
 31           S | Persons             | -               | -                |            |
 32             | Direct Debit        | -               | -                |            |
 33             | AddTxt              | -               | -                |            |
 34             | Payment Status      | -               | -                |            |
 35             | Discounted Amount   | -               | -                |            |
 36             | Decrease in Value   | -               | -                |            |
 37             | Recover Value       | -               | -                |            |
 38           S | CUID                | -               | -                |            |

************************************************************************************************************************************************************ *)

procedure TAgeView.Make(GroupID: string; OSAmount: double; idThd: integer);  (* ASYNC *)
{ ------------------------------------------------------ ! MAIN VARIABLES AND CONSTANTS ! ------------------------------------------------------------------- }
var
  { COUNTERS }
  iCNT:      integer;                   { NUMBER OF ROWS OF OPEN ITEMS 'STRINGGRID'      }
  jCNT:      integer;                   { NUMBER OF COLUMNS OF OPEN ITEMS 'STRINGGRID'   }
  { ROW COUNTERS FO AGING VIEW }
  avRow:     integer;                   { TOTAL ROWS IN AGE VIEW                         }
  exRow:     integer;                   { ROW COUNTER WHEN FILLING AGE VIEW WITH NUMBERS }
  { FIXED DATA PER ROW }
  DatTim:    string;                    { CURRENT DATE AND TIME                          }
  CutOff:    TDateTime;                 { CUT-OFF DATE, USUALLY TODAY - 1 OR - 3         }
  { BUCKET RANGES - LOWER BOUND }
  R1lo:      integer;                   { EXAMPLE: 1   }
  R2lo:      integer;                   { EXAMPLE: 8   }
  R3lo:      integer;                   { EXAMPLE: 31  }
  R4lo:      integer;                   { EXAMPLE: 61  }
  R5lo:      integer;                   { EXAMPLE: 91  }
  R6lo:      integer;                   { EXAMPLE: 121 }
  { BUCKET RANGES - UPPER BOUND }
  R1hi:      integer;                   { EXAMPLE: 7   }
  R2hi:      integer;                   { EXAMPLE: 30  }
  R3hi:      integer;                   { EXAMPLE: 60  }
  R4hi:      integer;                   { EXAMPLE: 90  }
  R5hi:      integer;                   { EXAMPLE: 120 }
  R6hi:      integer;                   { EXAMPLE: OO  }
  { BIAS }
  bias:      integer;
  { DISCOUNTED AMOUNT }
  DiscAmnt:  double;
  { RISK CLASS }
  MyWallet:  array of double;           { CONTAIN WALLET SHARE VALUES  }
  MyList:    array of integer;          { CONTAIN ASSOCIATED POSITIONS }
  RcHi:      double;                    { UPPER BOUND                  }
  RcLo:      double;                    { LOWER BOUND                  }
  Count:     double;                    { SUMS WALLET SHARE            }
const
  { WARNING! BELOW MUST BE ALIGNED WITH OPEN ITEMS SOURCE AND DESTINATION TABLE IN DATABASE FOR AGE VIEW }
  oiCol: array[0..12] of integer = (1,  2, 7, 13, 14, 15, 16, 17, 19, 28, 29, 31, 38);    { DEFINES SOURCE COLUMNS TO BE TRANSFERRED TO AGE VIEW 'AS IS' }
  avCol: array[0..12] of integer = (20, 4, 3, 22, 23, 15,  5, 17, 18, 19, 25, 24, 29);    { DEFINES DESTINATION COLUMNS IN AGE VIEW ARRAY                }
  rnCol: array[0..7]  of integer = (6, 7, 8, 9, 10, 11, 12, 13);                          { DEFINES BUCKET COLUMNS: NOT DUE, RANGE1..6, OVERDUE          }
  rfCol: array[0..13] of integer = (6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 26, 27, 28);  { DEFINES ALL COLUMNS WITH VALUES, TO BE REPLACED WITH '.'     }
{ ------------------------------------------------------------- ! INNER FUNCTION ! -------------------------------------------------------------------------- }
function bucket(pmtstat: integer; bias: integer): integer;
begin
  { 'PMTSTAT' IS COUNTED BETWEEN DUE DATE AND CURRENT DATE, THUS IT MUST BE CORRECTED FOR CUTOFF DATE. }
  { EXAMPLE BEING:                                                                                     }
  {   ON MONDAY WE MAKE AGING FOR FRIDAY, THRERFORE BIAS EQUALS 3 AND CUT-OFF IS 'CDATETIME' - 3.      }
  {   NON-MONDAY GOES WITH BIAS = 1, THUS I.E. WEDNESDAY - 1.                                          }
  {   HOWEVER, OPEN ITEM COLUMN 'PAYMENT STATUS' CONTAINS WITH CALCULATION:                            }
  {     'PMTSTAT' = DUE DATE - TODAY, THUS:                                                            }
  {     'PMTSTAT' = 2017-10-10 - 2017-10-23 = -13 (OVERDUE).                                           }
  {   CUT-OFF DATE IS 2017-10-23 (MONDAY) - 3 = 2017-10-20 (FRIDAY).                                   }
  {   THIS IS WHY WE COMPUTE 'PMTSTAT' = -13 + 3 = -10 (BETWEEN 8 AND 30).                             }
  pmtstat:=pmtstat + bias;
  Result:=0;
  { NOT DUE }
  if pmtstat >=0 then
  begin
    Result:=rnCol[0];
    exit;
  end;
  { OVERDUE RANGES 1..6 }
  if (abs(pmtstat) >= R1lo) and (abs(pmtstat) <= R1hi)   then result:=rnCol[1];
  if (abs(pmtstat) >= R2lo) and (abs(pmtstat) <= R2hi)   then result:=rnCol[2];
  if (abs(pmtstat) >= R3lo) and (abs(pmtstat) <= R3hi)   then result:=rnCol[3];
  if (abs(pmtstat) >= R4lo) and (abs(pmtstat) <= R4hi)   then result:=rnCol[4];
  if (abs(pmtstat) >= R5lo) and (abs(pmtstat) <= R5hi)   then result:=rnCol[5];
  if (abs(pmtstat) >= R6lo) {and (abs(pmtstat) <= R6hi)} then result:=rnCol[6];  { USE MORE THAN AS WE CAN HAVE ABOVE 365 DAYS }
end;
{ ------------------------------------------------------------------------------------------------------------------------------------ FILL ARRAY WITH ZEROES }
procedure AgeViewZeroFields(WhatRow: integer);
begin
  AgeView.ArrAgeView[WhatRow, rnCol[0]]:='0';  { NOT DUE }
  AgeView.ArrAgeView[WhatRow, rnCol[1]]:='0';  { RANGE 1 }
  AgeView.ArrAgeView[WhatRow, rnCol[2]]:='0';  { RANGE 2 }
  AgeView.ArrAgeView[WhatRow, rnCol[3]]:='0';  { RANGE 3 }
  AgeView.ArrAgeView[WhatRow, rnCol[4]]:='0';  { RANGE 4 }
  AgeView.ArrAgeView[WhatRow, rnCol[5]]:='0';  { RANGE 5 }
  AgeView.ArrAgeView[WhatRow, rnCol[6]]:='0';  { RANGE 6 }
  AgeView.ArrAgeView[WhatRow, rnCol[7]]:='0';  { OVERDUE }
end;
{ ------------------------------------------------------------------------------------------------------------------------------------------------ QUICK SORT }
procedure QuickSortR(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
{ 'A' VARIABLE HOLDS NUMERICAL DATA TO BE SORTED. 'L' VARIABLE IS ASSOCIATED COLUMN WITH ORIGINAL LIST POSITION. THE SECOND ASSOCIATED COLUMN FOLLOWS   }
{ 'A' COLUMN, BUT IT IS NOT SORTED. IT ALLOWS TO ASSIGN SORTED VALUES BACK TO ORIGINAL LIST POSITION AFTER COMPUTATION IS DONE. THIS IS TO BE USED WHEN }
{ SORTING IS NECESSARY BEFORE APPLAYING COMPUTATION AND AFTER WHICH WE MUST PUT VALUES BACK TO ITS ORIGINAL POSITIONS.                                  }
var
  Lo:     integer;
  Hi:     integer;
  Pivot:  double;
  T1:     double;   { FOR SORTING COLUMN    }
  T2:     integer;  { FOR ASSOCIATED COLUMN }
begin
   Lo:=iLo;
   Hi:=iHi;
   Pivot:=A[(Lo + Hi) div 2];
   repeat
     { ASCENDING }
     if ASC then
     begin
       while A[Lo] < Pivot do Inc(Lo);
       while A[Hi] > Pivot do Dec(Hi);
     end;
     { DESCENDING }
     if not ASC then
     begin
       while A[Lo] > Pivot do Inc(Lo);
       while A[Hi] < Pivot do Dec(Hi);
     end;
     { MOVING POSITIONS }
     if Lo <= Hi then
     begin
       T1:=A[Lo];
       T2:=L[Lo];
       { SORTING COLUMN }
       A[Lo]:= A[Hi];
       A[Hi]:= T1;
       { ASSOCIATED COLUMN }
       L[Lo]:= L[Hi];
       L[Hi]:= T2;
       { MOVE NEXT }
       Inc(Lo);
       Dec(Hi);
     end;
   until Lo > Hi;
   if Hi > iLo then QuickSortR(A, L, iLo, Hi, ASC);
   if Lo < iHi then QuickSortR(A, L, Lo, iHi, ASC);
end;
{ --------------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------- }
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------- DISPLAY MESSAGE }
  PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Generating age view...')));
  LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Generating age view...');
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  RcLo :=0;
  RcHi :=0;
  avRow:=0;
  SetLength(AgeView.ArrAgeView, 1, 30);  { MAKE 1 ROW AND 1..30 COLUMNS }
  { PUT ZERO FIELDS }
  AgeViewZeroFields(0);
  { ------------------------------------------------------------------------------------------------------------------------------------------  DATE AND TIME }
  cDateTime:=Now;
  DatTim:=DateToStr(cDateTime) + ' ' + TimeToStr(cDateTime);
  if SysUtils.DayOfWeek(cDateTime) = 2 then bias:=3 else bias:=1;
  CutOff:=cDateTime - bias;
  { -------------------------------------------------------------------------------------------------- REMOVE DUPLICATES AND MAKE AGE VIEW WITHOUT AGE VALUES }
  { WARNING! IT REQUIRES OPEN ITEMS 'STRINGGRID' TO BE SORTED BY    }
  {          SUPPORTED COLUMN 'UID', WE ASSUME THAT IS ALREADY DONE }
  {          AND WE START WITH "ONE" BECAUSE ZERO POINTS TO HEADERS }
  {          IN 'STRINGGRID'                                        }
  for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
    { GO THROUGH THE ROWS AND POPULATE WHEN FIND THAT THE ROW BELOW IS DIFFERENT THAN CURRENT }
    if (MainForm.sgOpenItems.Cells[38, iCNT] <> MainForm.sgOpenItems.Cells[38, iCNT + 1]) then
    begin
      { ---------------------------------------------------------------------------------------------------------------------------------- FIXED DATA PER ROW }
      AgeView.ArrAgeView[avRow, 0]:=GroupID;
      AgeView.ArrAgeView[avRow, 1]:=DateToStr(CutOff);
      AgeView.ArrAgeView[avRow, 2]:=DatTim;
      { ------------------------------------------------------------------------------------------------------------------------ RE-WRITE REST OF THE COLUMNS }
      for jCNT:=0 to high(oiCol) do AgeView.ArrAgeView[avRow, avCol[jCNT]]:=MainForm.sgOpenItems.Cells[oiCol[jCNT], iCNT];
      { ---------------------------------------------------------------------------------------------------------------------------------------- ALTERNATIONS }
      { REMOVE 'TAB' CHARACTER IF FOUND IN CUSTOMER NAME }
      AgeView.ArrAgeView[avRow, 3]:=StringReplace(AgeView.ArrAgeView[avRow, 3], #9, '', [rfReplaceAll]);
      { REPLACE SINGLE QUOTES TO DOUBLE QUOTES }
      AgeView.ArrAgeView[avRow, 3]:=StringReplace(AgeView.ArrAgeView[avRow, 3], '''', '''''', [rfReplaceAll]);
      { REMOVE FROM CO CODE 'F' PREFIX }
      AgeView.ArrAgeView[avRow, 20]:=OpenItems.ConvertName(MidStr(AgeView.ArrAgeView[avRow, 20], 2, 5), '', 2);
      { LEDGER ISO }
      if MainForm.COC1.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR1.Text;
      if MainForm.COC2.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR2.Text;
      if MainForm.COC3.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR3.Text;
      if MainForm.COC4.Text = AgeView.ArrAgeView[avRow, 20] then AgeView.ArrAgeView[avRow, 21]:=MainForm.CUR4.Text;
      { -------------------------------------------------------------------------------------------------------------------------------------------- COUNTERS }
      { MOVE COUNTER }
      inc(avRow);
      { EXPAND ARRAY BY ONE EMPTY ROW }
      SetLength(AgeView.ArrAgeView, avRow + 1, 30);
      { ZERO FIELDS }
      AgeViewZeroFields(avRow);
    end;
  { ------------------------------------------------------------------------------------------------------------------ CALCULATE VALUES FOR GIVEN AGE BUCKETS }
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  { LOWER BOUNDS }
  R1lo:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE1A', 0);
  R2lo:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE2A', 0);
  R3lo:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE3A', 0);
  R4lo:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE4A', 0);
  R5lo:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE5A', 0);
  R6lo:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE6A', 0);
  { UPPER BOUNDS }
  R1hi:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE1B', 0);
  R2hi:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE2B', 0);
  R3hi:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE3B', 0);
  R4hi:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE4B', 0);
  R5hi:=Settings.TMIG.ReadInteger(Settings.AgingRanges, 'RANGE5B', 0);
  { ------------------------------------------------------------------------------------------------------------------------------------------------ POPULATE }
  { LOOP VIA CUID COLUMN IN AGE VIEW [29] }
  for exRow:=0 to avRow - 1 do
  begin
    DiscAmnt:=0;
    { LOOP VIA CUID COLUMN IN OPEN ITEMS [38] }
    for iCNT:=0 to MainForm.sgOpenItems.RowCount - 1 do
    begin
      { COMPARE AND EXECUTE IF THE SAME }
      if MainForm.sgOpenItems.Cells[38, iCNT] = AgeView.ArrAgeView[exRow, 29] then
      begin
        { SUM ITEMS: NOT DUE, RANGE1..5 }
        AgeView.ArrAgeView[exRow, bucket(StrToInt(MainForm.sgOpenItems.Cells[34, iCNT]), bias)]:=FloatToStr(
                                                                           StrToFloat(AgeView.ArrAgeView[exRow, bucket(StrToInt(MainForm.sgOpenItems.Cells[34, iCNT]), bias)]) +
                                                                           StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT])
                                                                           );
        { SUM ITEMS: DISCOUNTED AMOUNT [35] | TECHNICAL VARIABLE }
        DiscAmnt:=DiscAmnt + StrToFloat(MainForm.sgOpenItems.Cells[35, iCNT]);
      end;
    end;
    { CALCULATE TOTAL AMOUNT [14] }
    AgeView.ArrAgeView[exRow, 14]:=FloatToStr(
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[0]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[1]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[2]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[3]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[4]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[5]]) +
                        StrToFloat(AgeView.ArrAgeView[exRow, rnCol[6]])
                        );
    { TOTAL OVERDUE [13] = TOTAL AMOUNT [14] - NOT DUE [6] }
    AgeView.ArrAgeView[exRow, 13]:=FloatToStr(
                        StrToFloat(AgeView.ArrAgeView[exRow, 14]) -
                        StrToFloat(AgeView.ArrAgeView[exRow,  6])
                        );
    { EXCEEDED AMOUNT [16] = CREDIT LIMIT [15] - TOTAL AMOUNT [14] }
    AgeView.ArrAgeView[exRow, 16]:=FloatToStr(StrToFloat(AgeView.ArrAgeView[exRow, 15]) - StrToFloat(AgeView.ArrAgeView[exRow, 14]));
    { WALLET SHARE [28] | TECHNICAL COLUMN }
    if OSAmount <> 0 then AgeView.ArrAgeView[exRow, 28]:=FloatToStrF(( (StrToFloat(AgeView.ArrAgeView[exRow, 14]) / OSAmount) * 1), ffFixed, 4, 4)
      else
        AgeView.ArrAgeView[exRow, 28]:='0';
    { CALCULATE QUALITY INDEX [27] }
    if OSAmount <> 0 then AgeView.ArrAgeView[exRow, 27]:=FloatToStrF(( 1 - (DiscAmnt / OSAmount) ), ffFixed, 6, 6)
      else
        AgeView.ArrAgeView[exRow, 27]:='0';
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------- RISK CLASS BOUNDS }
  if FormatSettings.DecimalSeparator = ',' then
  begin
    RcLo:=StrToFloat(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_A_MAX', '0,80'));     { EXAMPLE: 80% }
    RcHi:=StrToFloat(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_A_MAX', '0,80')) +    { EXAMPLE: 95% }
          StrToFloat(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_B_MAX', '0,15'));
  end;
  if FormatSettings.DecimalSeparator = '.' then
  begin
    RcLo:=StrToFloat(StringReplace(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_A_MAX', '0,80'), ',', '.', [rfReplaceAll]));
    RcHi:=StrToFloat(StringReplace(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_A_MAX', '0,80'), ',', '.', [rfReplaceAll])) +
          StrToFloat(StringReplace(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_B_MAX', '0,15'), ',', '.', [rfReplaceAll]));
  end;
  { --------------------------------------------------------------------------------------------------------------------------------- RISK CLASS CALCULATIONS }
  SetLength(MyWallet, avRow);
  SetLength(MyList,   avRow);
  Count:=0;
  { MOVE REQUIRED ITEMS TO ARRAYS }
  for iCNT:=0 to avRow - 1 do
  begin
    MyList[iCNT]  :=iCNT;                                     { ORIGINAL LP  }
    MyWallet[iCNT]:=StrToFloat(AgeView.ArrAgeView[iCNT, 28]); { WALLET SHARE }
  end;
  { SORT DESCENDING VIA WALLET SHARTE }
  QuickSortR(MyWallet, MyList, Low(MyWallet), High(MyWallet), False);
  { CALCULATE AND ASSIGN }
  for iCNT:=0 to avRow - 1 do
  begin
    Count:=Count + MyWallet[iCNT];
    { ASSIGN RISK CLASS 'A' }
    if Count <= RcLo then   AgeView.ArrAgeView[MyList[iCNT], 26]:='A';
    { ASSIGN RISK CLASS 'B' }
    if (Count > RcLo)  and
       (Count <= RcHi) then AgeView.ArrAgeView[MyList[iCNT], 26]:='B';
    { ASSIGN RISK CLASS 'C' }
    if Count > RcHi then    AgeView.ArrAgeView[MyList[iCNT], 26]:='C';
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------- DECIMAL SEPARATOR }
  if FormatSettings.DecimalSeparator = ',' then
    for exRow:=0 to avRow - 1 do
      for jCNT:=0 to high(rfCol) do
        { REPLACE ',' TO '.' FOR ALL VALUES [6..14] AND [25..26] }
        AgeView.ArrAgeView[exRow, rfCol[jCNT]]:=StringReplace(AgeView.ArrAgeView[exRow, rfCol[jCNT]], ',', '.', [rfReplaceAll]);
end;

{ -------------------------------------------------------------------------------------------------------------------------- TRANSFER 'AGEVIEW' TO SQL SERVER }
procedure TAgeView.Write(DestTable: string; idThd: integer);  (* ASYNC *)  //refactor!!
const
  AllColumns = 'GROUP_ID,AGE_DATE,SNAPSHOT_DT,CUSTOMER_NAME,CUSTOMER_NUMBER,COUNTRY_CODE,NOT_DUE,RANGE1,RANGE2,RANGE3,RANGE4,RANGE5,RANGE6,OVERDUE,TOTAL,'+
               'CREDIT_LIMIT,EXCEEDED_AMOUNT,PAYMENT_TERMS,AGENT,DIVISION,CO_CODE,LEDGER_ISO,INF4,INF7,PERSON,GROUP3,RISK_CLASS,QUALITY_IDX,WALLET_SHARE,CUID';
var
  MSSQL:        TMSSQL;
  Transaction:  string;
  DeleteData:   string;
begin
  { INITIALIZE }
  MSSQL:=TMSSQL.Create(Database.ADOConnect);
  DeleteData:='DELETE FROM ' + DestTable + ' WHERE GROUP_ID = ' + QuotedStr(AgeView.ArrAgeView[0, 0]) + ' AND AGE_DATE = ' + QuotedStr(LeftStr(AgeView.ArrAgeView[0, 1], 10));
  try
    { BUILD AND EXECUTE }
    Transaction:=MSSQL.ArrayToSql(AgeView.ArrAgeView, DestTable, AllColumns);
    Transaction:='BEGIN TRANSACTION'                                              + #13#10 +
                 'SELECT TOP 1 * FROM ' + DestTable + ' WITH (TABLOCK, HOLDLOCK)' + #13#10 +
                 DeleteData                                                       + #13#10 +
                 Transaction                                                      + #13#10 +
                 'COMMIT TRANSACTION';
    { ASSIGN AND EXECUTE }
    MSSQL.StrSQL:=Transaction;
    try
      MSSQL.ExecSQL;
    except
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Cannot send to server. Error has been thrown: ' + IntToStr(High(AgeView.ArrAgeView)) + '.');
    end;
  finally
    MSSQL.Free;
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Age View transferred to Microsoft SQL Server. Rows affected: ' + IntToStr(High(AgeView.ArrAgeView)) + '.');
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------- RETURN CO CODE AND CUREENCY }
procedure TAgeView.Details(GroupID: string; AgeDate: TDateTime; idThd: integer);  (* SYNCHRONIZE *)  // REFACTOR !!!!
var
  { COUNTER }
  iCNT:     integer;
  { SQL }
  StrSQL:   string;
  Query:    TADOQuery;
begin
  { --------------------------------------------------------------------------------------------------------------------------------------------------- QUERY }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    { -------------------------------------------------------------------------------------------------------------------- DISTINCT CO CODES & CURRENCY CODES }
    StrSQL:='SELECT DISTINCT '+
               'CO_CODE, LEDGER_ISO '+
             'FROM '+
               'tbl_snapshots '+
             'WHERE '+
               'CO_CODE '+
             'IN '+
               '(SELECT CO_CODE FROM tbl_snapshots WHERE GROUP_ID = :uParam1 AND AGE_DATE = :uParam2);';
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Parameters.ParamByName('uParam1').Value:=GroupID;
    Query.Parameters.ParamByName('uParam2').Value:=AgeDate;
    try
      { --------------------------------------------------------------------------------------------------------------------------------------------- EXECUTE }
      Query.Open;
      { ---------------------------------------------------------------------------------------------------------------------------- READ ALL RETURNED VALUES }
      iCNT:=1;
      Query.Recordset.MoveFirst;
      while not Query.Recordset.EOF do begin
        { WE SHOULD HAVE ONLY FOUR COMPANIES IN THE GIVEN GROUP }
        if Query.RecordCount < 5 then begin
          if iCNT = 1 then begin
            MainForm.COC1.Text:=Query.Recordset.Fields[0].Value;
            MainForm.CUR1.Text:=Query.Recordset.Fields[1].Value;
          end;
          if iCNT = 2 then begin
            MainForm.COC2.Text:=Query.Recordset.Fields[0].Value;
            MainForm.CUR2.Text:=Query.Recordset.Fields[1].Value;
          end;
          if iCNT = 3 then begin
            MainForm.COC3.Text:=Query.Recordset.Fields[0].Value;
            MainForm.CUR3.Text:=Query.Recordset.Fields[1].Value;
          end;
          if iCNT = 4 then begin
            MainForm.COC4.Text:=Query.Recordset.Fields[0].Value;
            MainForm.CUR4.Text:=Query.Recordset.Fields[1].Value;
          end;
        end;
        (* DebugMsg(Query.Recordset.Fields[0].Value + ' | ' + Query.Recordset.Fields[1].Value); *)
        Query.Recordset.MoveNext;
        inc(iCNT);
      end;
      Query.Close;
    except
      on E: Exception do begin
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Method ''AgeViewDetails'' cannot execute SQL statement. Please contact IT support.')));
        LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Method ''AgeViewDetails'' cannot execute SQL statement. Error thrown: ' + E.Message);
        PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
      end;
    end;
    StrSQL:='SELECT DISTINCT '+
               ' CO_CODE, INTEREST_RATE, AGENTS '+
             'FROM '+
               'tbl_company '+
             'WHERE '+
               'CO_CODE = :uParam1 '+
             'OR '+
               'CO_CODE = :uParam2 '+
             'OR '+
               'CO_CODE = :uParam3 '+
             'OR '+
               'CO_CODE = :uParam4 ';
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Parameters.ParamByName('uParam1').Value:=MainForm.COC1.Text;
    Query.Parameters.ParamByName('uParam2').Value:=MainForm.COC2.Text;
    Query.Parameters.ParamByName('uParam3').Value:=MainForm.COC3.Text;
    Query.Parameters.ParamByName('uParam4').Value:=MainForm.COC4.Text;
    try
      { --------------------------------------------------------------------------------------------------------------------------------------------- EXECUTE }
      Query.Open;
      { ---------------------------------------------------------------------------------------------------------------------------- READ ALL RETURNED VALUES }
      iCNT:=1;
      Query.Recordset.MoveFirst;
      while not Query.Recordset.EOF do begin
        { WE SHOULD HAVE ONLY FOUR COMPANIES IN THE GIVEN GROUP }
        if Query.RecordCount < 5 then begin
          if iCNT = 1 then begin
            MainForm.INT1.Text:=Query.Recordset.Fields[1].Value;
            MainForm.AGT1.Text:=Query.Recordset.Fields[2].Value;
          end;
          if iCNT = 2 then begin
            MainForm.INT2.Text:=Query.Recordset.Fields[1].Value;
            MainForm.AGT2.Text:=Query.Recordset.Fields[2].Value;
          end;
          if iCNT = 3 then begin
            MainForm.INT3.Text:=Query.Recordset.Fields[1].Value;
            MainForm.AGT3.Text:=Query.Recordset.Fields[2].Value;
          end;
          if iCNT = 4 then begin
            MainForm.INT4.Text:=Query.Recordset.Fields[1].Value;
            MainForm.AGT4.Text:=Query.Recordset.Fields[2].Value;
          end;
        end;
        (* DebugMsg(Query.Recordset.Fields[0].Value + ' | ' + Query.Recordset.Fields[1].Value); *)
        Query.Recordset.MoveNext;
        inc(iCNT);
      end;
      Query.Close;
    except
      on E: Exception do begin
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Method ''AgeViewDetails'' cannot execute SQL statement. Please contact IT support.')));
        LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Method ''AgeViewDetails'' cannot execute SQL statement. Error thrown: ' + E.Message);
        PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
      end;
    end;
  finally
    Query.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------- DISPLAY CO CODE AND CURRENCY }
  MainForm.tcCOCODE.Caption   :='';
  MainForm.tcCURRENCY.Caption :='';
  { CO CODE }
  if MainForm.COC1.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC1.Text + ' | ';
  if MainForm.COC2.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC2.Text + ' | ';
  if MainForm.COC3.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC3.Text + ' | ';
  if MainForm.COC4.Text <> '0' then MainForm.tcCOCODE.Caption:=MainForm.tcCOCODE.Caption + MainForm.COC4.Text + ' | ';
  MainForm.tcCOCODE.Caption:=MidStr(MainForm.tcCOCODE.Caption, 1, Length(MainForm.tcCOCODE.Caption) - 2);
  { CURRENCY }
  if MainForm.CUR1.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR1.Text + ' | ';
  if MainForm.CUR2.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR2.Text + ' | ';
  if MainForm.CUR3.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR3.Text + ' | ';
  if MainForm.CUR4.Text <> 'N/A' then MainForm.tcCURRENCY.Caption:=MainForm.tcCURRENCY.Caption + MainForm.CUR4.Text + ' | ';
  MainForm.tcCURRENCY.Caption:=MidStr(MainForm.tcCURRENCY.Caption, 1, Length(MainForm.tcCURRENCY.Caption) - 2);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- LOOK FOR DATA }
function TAgeView.MapData(AgeGrid: TStringGrid; WhichCol: string; tblMap: TStringGrid): string;  // REFACTOR!!!
var
  iCNT:  integer;
  jCNT:  integer;
begin
  Result:='unassigned';
  { FIND GIVEN COLUMN }
  for jCNT:=1 to AgeGrid.ColCount - 1 do
    if WhichCol = AgeGrid.Cells[jCNT, 0] then break;
  { FIND DATA }
  for iCNT:=1 to tblMap.RowCount - 1 do
  begin
    if AgeGrid.Cells[jCNT, AgeGrid.Row] = tblMap.Cells[2, iCNT] then
    begin
      if (tblMap.Cells[3, iCNT] = '') or (tblMap.Cells[3, iCNT] = ' ') then Result:='unassigned' else Result:=tblMap.Cells[3, iCNT];
      Break;
    end;
  end;
end;

{ ############################################################## ! OPEN ITEMS CLASS ! ####################################################################### }

                                                                      //refactor!!!

{ ------------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS LOAD }
procedure TOpenItems.Load(idThd: integer);  (* ASYNC & SYNC *)

{ --------------------------------------------------------------- ! INNER METHODS ! ------------------------------------------------------------------------- }

{ --------------------------------------------------------------- ! CONVERT DATE ! -------------------------------------------------------------------------- }
function ConvDate(sDate: string): string;
{ INPUT FORMAT:  YYYYMMDD   }
{ OUTPUT FORMAT: YYYY-MM-DD }
begin
  Result:=LeftStr(sDate, 4) + '-' + MidStr(sDate, 5, 2) + '-' + RightStr(sDate, 2);
end;

{ -------------------------------------------------------------- ! CONVERT FLOAT ! -------------------------------------------------------------------------- }
function IsNumeric(str, mode: string): boolean;
begin
  try
    if mode = UpperCase('float')   then StrToFloat(str);
    if mode = UpperCase('integer') then StrToInt(str);
    Result:=True;
  except
    Result:=False;
  end;
end;

{ -------------------------------------------------------------- ! DAY CALCULATIONS ! ----------------------------------------------------------------------- }
function HowManyDays(refDate1, refDate2: string): string;
var
  diff: extended;
begin
  try
    diff:=StrToDateTime(refDate1) - StrToDateTime(refDate2);
    Result:=IntToStr(Round(diff));
  except
    Result:='N/A';
  end;
end;

{ ----------------------------------------------------------- ! LOOK FOR VOUCHER TYPE ! --------------------------------------------------------------------- }
function IsVoType(VoType: string): boolean;
var
  tsVAL:     TStringList;
  iCNT:      integer;
begin
  Result:=False;
  tsVAL:=TStringList.Create;
  try
    Settings.TMIG.ReadSectionValues(Settings.InvoiceTypes, tsVAL);
    for iCNT:=0 to tsVAL.Count - 1 do
      if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
      begin
        Result:=True;
        break;
      end;
  finally
    tsVAL.Free;
  end;
end;

{ ------------------------------------------------------------- ! LOAD DATA FROM DB ! ----------------------------------------------------------------------- }
function LoadFromDB(COC: string; OffSet: integer; CoPos: integer): integer;
var
  { COUNTERS }
  iCNT:             integer;
  jCNT:             integer;
  Count:            integer;
  Inner:            integer;
  { CSV DATA AND DELIMITER }
  Delimiter:        Char;
  Data:             TStringList;
  Transit:          TStringList;
  { FILE }
  fSource, fPath:   string;
  { AGENT COLUMN NUMBER }
  AgentCol:         integer;
  { CUT-OFF }
  NrCutOffPos:      integer;             { WHAT COLUMN HOLDS THE VALUE TO BE CHECKED }
  NrCutOffNum:      integer;             { NUMERIC CONDITION                         }
  TxCutOffPos:      integer;             { WHAT COLUMN HOLDS THE VALUE TO BE CHECKED }
  TxCutOffTxt:      string;              { TEXT CONDITION                            }
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------------- INITIAIZE }
  Result:=0;
  Count :=0;
  Inner :=1;
  OpenItems.FileExist:=True;
  { ------------------------------------------------------------------------------------------------------------- CHECK IF GIVEN COMPANY HAVE AGENT ON OR OFF }
  AgentCol:=-100;
  if (CoPos = 1) and (MainForm.AGT1.Text = 'OFF') then AgentCol:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'AGENTCOLUMN', 0);
  if (CoPos = 2) and (MainForm.AGT2.Text = 'OFF') then AgentCol:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'AGENTCOLUMN', 0);
  if (CoPos = 3) and (MainForm.AGT3.Text = 'OFF') then AgentCol:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'AGENTCOLUMN', 0);
  if (CoPos = 4) and (MainForm.AGT4.Text = 'OFF') then AgentCol:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'AGENTCOLUMN', 0);
  { ----------------------------------------------------------------------------------------------------------------------------- FILE PATH FOR GIVEN CO CODE }
  fSource:=OpenItems.ConvertName(COC, '', 0);
  fPath:=OpenItems.OpenItemsDir + OpenItems.OpenItemsFor;
  fPath:=StringReplace(fPath, '{NUM}', fSource, [rfReplaceAll]);
  { ----------------------------------------------------------------------------------------------------------------------------- CHECK IF SOURCE FILE EXISTS }
  if FileExists(fPath) = False then
  begin
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Cannot find ' + fSource + ' in database. Process halted automatically.');
    SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Cannot find ' + fSource + ' in database. Process halted automatically.')));
    OpenItems.FileExist:=False;
  end else
  begin
    { -------------------------------------------------------------------------------------------------------------- PROCESS THE SOURCE FILE | PARSE CSV DATA }
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Downloading company number ' + COC + '...');
    Data   :=TStringList.Create;
    Transit:=TStringList.Create;
    try
      { LOAD DATA }
      Data.LoadFromFile(fPath);
      { SETUP DELIMITER }
      Delimiter:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'DELIMITER', ';')[1];
      { COUNT ALL COLUMNS }
      for iCNT:=0 to Length(Data[0]) do if copy(Data[0], iCNT, 1) = delimiter then inc(Count);
      Count:=Count + 1;
      { COUNT ALL ROWS & SETUP OFFSET }
      MainForm.sgOpenItems.RowCount:=Data.Count + OffSet;
      { SETUP TRANSIT THAT WILL HOLD SPLIT LINE }
      Transit.StrictDelimiter:=True;
      Transit.Delimiter:=Delimiter;
      { ASSIGN CONDITIONS }
      NrCutOffPos:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'NRCUTOFFPOS', 0);
      NrCutOffNum:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'NRCUTOFFNUM', 0);
      TxCutOffPos:=Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'TXCUTOFFPOS', 0);
      TxCutOffTxt:=Settings.TMIG.ReadString(Settings.OpenItemsData,  'TXCUTOFFTXT', '');
      { ITERATE VIA ALL ROWS }
      for iCNT:= 1 to Data.Count - 1 do
      begin
        try
          { SPLIT STRING USING GIVEN DELIMITER }
          Transit.DelimitedText:=Data[iCNT];

          { CHECK IF THERE IS INVALID DUE DATE VALUE, IF SO PUT DEFAULT DATE 1901-01-01 }
          if Transit[11]='0' then Transit[11]:='19010101';

          { CONVERT DECIMAL SEPARATOR | COMMA TO POINT }
          if (Settings.TMIG.ReadString(Settings.OpenItemsData, 'DECIMAL_SEPARATOR', ',') = ',') and (FormatSettings.DecimalSeparator = '.') then
          begin
            Transit[4] :=StringReplace(Transit[4],  ',', '.', [rfReplaceAll]);
            Transit[5] :=StringReplace(Transit[5],  ',', '.', [rfReplaceAll]);
            Transit[8] :=StringReplace(Transit[8],  ',', '.', [rfReplaceAll]);
            Transit[9] :=StringReplace(Transit[9],  ',', '.', [rfReplaceAll]);
            Transit[14]:=StringReplace(Transit[14], ',', '.', [rfReplaceAll]);
          end;

          { CONVERT DECIMAL SEPARATOR | POINT TO COMMA }
          if (Settings.TMIG.ReadString(Settings.OpenItemsData, 'DECIMAL_SEPARATOR', ',') = '.') and (FormatSettings.DecimalSeparator = ',') then
          begin
            Transit[4] :=StringReplace(Transit[4],  '.', ',', [rfReplaceAll]);
            Transit[5] :=StringReplace(Transit[5],  '.', ',', [rfReplaceAll]);
            Transit[8] :=StringReplace(Transit[8],  '.', ',', [rfReplaceAll]);
            Transit[9] :=StringReplace(Transit[9],  '.', ',', [rfReplaceAll]);
            Transit[14]:=StringReplace(Transit[14], '.', ',', [rfReplaceAll]);
          end;

          { ALLOW ONLY ROW THAT MEET THE REQUIREMENTS FOUND IN COLUMN INICATED BY CUTOFFPOS }
          { WE HAVE TWO BUILT-IN CONDITIONS:                   }
          {   1. NRCUTOFF - NUMERIC CONDITION 'LESS OR EQUAL'  }
          {   2. TXCUTOFF - TEXT CONDITION    'DIFFERENT THAN' }
          if (StrToInt(Transit[NrCutOffPos - 1]) <= NrCutOffNum) and (UpperCase(Transit[TxCutOffPos - 1]) <> UpperCase(TxCutOffTxt)) then
          begin
            { PUT EACH COLUMN INTO STRING GRID FOR GIVEN ROW }
            for jCNT:=1 to Count do
            begin
              { MAKE AGENT COLUMN EMPTY IF 'AGENTCOL' IS POSITIVE }
              if AgentCol < 0 then MainForm.sgOpenItems.Cells[jCNT, Inner + OffSet]:=Transit[jCNT - 1];
              if AgentCol > 0 then
              begin
                if jCNT <> AgentCol then MainForm.sgOpenItems.Cells[jCNT, Inner + OffSet]:=Transit[jCNT - 1];
                if jCNT = AgentCol  then MainForm.sgOpenItems.Cells[jCNT, Inner + OffSet]:='';
              end;
            end;

            { UID COLUMN CONTAINS WITH 'CUSTOMER NUMBER' AND 'CO CODE' AND 'AGENT NUMBER'             }
            { THIS WILL EVENTUALLY LEAD TO LARGE NUMBER, EXTENDED TYPE SHOULD BE USED FOR THIS COLUMN }
            { IT ALLOWS TO HAVE ABSOLUTE UNIQUE NUMBER FOR GIVEN SNAPSHOT DAY                         }
            MainForm.sgOpenItems.Cells[38, Inner + OffSet]:=MainForm.sgOpenItems.Cells[2, Inner + OffSet] +
                                                 MidStr(MainForm.sgOpenItems.Cells[1, Inner + OffSet], 2, 5) +
                                                 (IntToStr(StrToIntDef(MainForm.sgOpenItems.Cells[19, Inner + OffSet], 0)));
            { MOVE ON }
            inc(Inner);
            { CLEAR IT }
            Transit.Clear;
          end;
        { THERE MAY BE A SITUATION WHEN CSV DATA IS BROKEN AND HAVE INCORRECT NUMBER OF DELIMITER IN SINGLE ROW }
        Except
          on E: Exception do
          begin
            { LOG ERROR AND THROW MESSAGE ON MAIN TAB }
            LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: The source file: ' + ExtractFileName(fPath) + ' (line number: ' + IntToStr(iCNT) + ') is broken.');
          end;
        end;
      end;
    finally
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Downloading company number ' + COC + '..., ' + IntToStr((Data.Count - 1)) + ' row(s) has been processed.');
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Downloading company number ' + COC + '..., ' + IntToStr((Inner - 1)) + ' row(s) has been affected.');
      Result:=Inner;
      MainForm.sgOpenItems.RowCount:=Inner + OffSet;
      Data.Free;
      Transit.Free;
    end;
  end;
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
var
  { COUNTERS }
  iCNT:             integer;
  TotalRows:        integer;
  { TIME DIFFERENCE }
  TimeDiff:         integer;
  { REFLECTS TIME VALUE OF MONEY: DISCOUNTED AMOUNT, DECREASE IN AMOUNT AND RECOVERY AMOUNT }
  DiscountedAmt:    double;
  DecreaseAmt:      double;
  RecoveryAmt:      double;
  { INTEREST RATE FOR GIVEN COMPANY CODE }
  InterestRate:     double;
  { AMOUNT OF OUTSTANDING INVOICES ONLY }
  InvoiceAmt:       double;
begin

  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }

  (* RESET VARIABLES *)

  OpenItems.nInvoices     :=0;
  OpenItems.Overdue       :=0;
  OpenItems.OverdueAmt    :=0;
  OpenItems.OSamt         :=0;
  OpenItems.UNamt         :=0;
  OpenItems.cDiscountedAmt:=0;
  OpenItems.cDecreaseAmt  :=0;
  OpenItems.cRecoveryAmt  :=0;
  OpenItems.KPI_overdue   :=0;
  OpenItems.KPI_unalloc   :=0;
  InterestRate            :=0;
  TotalRows               :=0;
  { ----------------------------------------------------------------------------------------------- STOP IF THERE IS NO COMPANIES SELECTED, OTHERWISE EXECUTE }
  if StrToInt(MainForm.COC1.Text) + StrToInt(MainForm.COC2.Text) + StrToInt(MainForm.COC3.Text) + StrToInt(MainForm.COC4.Text)  <= 0 then
  begin
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Incorrect companies combination.');
    LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Downloading halted automatically.');
    SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Incorrect companies combination. Downloading halted automatically.')));
  end else
  begin

    { -------------------------------------------------- ! SETTING-UP AND LOADING OPEN ITEMS ! -------------------------------------------------------------- }
                                                                    { MAIN BLOCK }

    { ----------------------------------------------------------------------------------------------------------------------------------------- CHANGE STATUS }
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Downloading Open Items...')));
    { ------------------------------------------------------------------------------------------------------------------------------ DO NOT DRAW 'STRINGGRID' }
    with MainForm.sgOpenItems do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    { ------------------------------------------------------------------------------------------------- LOAD ALL OPEN ITEMS INTO 'STRINGGRID'AND SORT VIA UID }
    if MainForm.COC1.Text > '0' then TotalRows:=LoadFromDB(MainForm.COC1.Text, 0, 1);
    if MainForm.COC2.Text > '0' then TotalRows:=TotalRows + LoadFromDB(MainForm.COC2.Text, MainForm.sgOpenItems.RowCount - 1, 2);
    if MainForm.COC3.Text > '0' then TotalRows:=TotalRows + LoadFromDB(MainForm.COC3.Text, MainForm.sgOpenItems.RowCount - 1, 3);
    if MainForm.COC4.Text > '0' then TotalRows:=TotalRows + LoadFromDB(MainForm.COC4.Text, MainForm.sgOpenItems.RowCount - 1, 4);
    { --------------------------------------------------------------------------------------------------------------------------- PROCEED IF OPEN ITEMS EXIST }
    if OpenItems.FileExist then
    begin
      { --------------------------------------------------------------------------------------------------------------------------------------------- SORT IT }
      MainForm.sgOpenItems.MSort(Settings.TMIG.ReadInteger(Settings.OpenItemsData, 'SORTPOS', 0), 2, True);

      { ---------------------------------------------------------------------------------------------------------------------------------------------- LOG IT }
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Open Items sorted by ''UID'' column (ascending).');
      LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(idThd) + ']: Open Items downloaded. ' + IntToStr(TotalRows) + ' row(s) have been affected in total.');

      { -------------------------------------------- ! LP, MAPPING, FORMATTING AND RE-CALCULATIONS ! -------------------------------------------------------- }
                                                            { MAIN LOOP THROUGH 'STRINGGRID' }

      for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
      begin

        { ------------------------------------------------------------------------------------------------------------------------------------------------ LP }
        MainForm.sgOpenItems.Cells[0, iCNT]:=IntToStr(iCNT);

        { --------------------------------------------------------------------------------------------- CHECK SEPARATOR AND ADD ZERO BEFORE DECIMAL SEPARATOR }
        { POSITIVE VALUES }
        if (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[5,  iCNT]:= '0' + MainForm.sgOpenItems.Cells[5,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[6,  iCNT]:= '0' + MainForm.sgOpenItems.Cells[6,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[9,  iCNT]:= '0' + MainForm.sgOpenItems.Cells[9,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[10, iCNT]:= '0' + MainForm.sgOpenItems.Cells[10, iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[15, iCNT]:= '0' + MainForm.sgOpenItems.Cells[15, iCNT];

        { NEGATIVE VALUES }
        if (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[5,  iCNT]:= '-0' + MainForm.sgOpenItems.Cells[5,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[6,  iCNT]:= '-0' + MainForm.sgOpenItems.Cells[6,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[9,  iCNT]:= '-0' + MainForm.sgOpenItems.Cells[9,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[10, iCNT]:= '-0' + MainForm.sgOpenItems.Cells[10, iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[15, iCNT]:= '-0' + MainForm.sgOpenItems.Cells[15, iCNT];

        { ----------------------------------------------------------------------------------------------------------- DATE FORMAT FROM YYYYMMDD TO YYYY-MM-DD }
        MainForm.sgOpenItems.Cells[4,  iCNT]:=ConvDate(MainForm.sgOpenItems.Cells[4,  iCNT]);
        MainForm.sgOpenItems.Cells[12, iCNT]:=ConvDate(MainForm.sgOpenItems.Cells[12, iCNT]);
        MainForm.sgOpenItems.Cells[27, iCNT]:=ConvDate(MainForm.sgOpenItems.Cells[27, iCNT]);

        { ----------------------------------------------------------------------------------------------------------------------- PAYMENT STATUS CALCULATIONS }
        MainForm.sgOpenItems.Cells[34, iCNT]:=HowManyDays(MainForm.sgOpenItems.Cells[12, iCNT], DateTimeToStr(Now));
        TimeDiff:=StrToInt(HowManyDays(MainForm.sgOpenItems.Cells[12, iCNT], DateTimeToStr(Now)));

        { ------------------------------------------------------------------------ RE-CALCULATIONS FOR: DISCOUNTED AMOUNT, DECREASE IN AMOUNT, RECOVERY VALUE }
        InvoiceAmt:=StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT]);
        if MainForm.sgOpenItems.Cells[1, iCNT] = OpenItems.ConvertName(MainForm.COC1.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT1.Text);
        if MainForm.sgOpenItems.Cells[1, iCNT] = OpenItems.ConvertName(MainForm.COC2.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT2.Text);
        if MainForm.sgOpenItems.Cells[1, iCNT] = OpenItems.ConvertName(MainForm.COC3.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT3.Text);
        if MainForm.sgOpenItems.Cells[1, iCNT] = OpenItems.ConvertName(MainForm.COC4.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT4.Text);
        if (TimeDiff < 0) and (InterestRate > 0) and (InvoiceAmt > 0) and (IsVoType(MainForm.sgOpenItems.Cells[3, iCNT]) = True) then
        begin
          DiscountedAmt:=InvoiceAmt / ( 1 + ( (InterestRate / 365) * ABS(TimeDiff)) );
          DecreaseAmt:=InvoiceAmt - DiscountedAmt;
          RecoveryAmt:=InvoiceAmt + DecreaseAmt;
          MainForm.sgOpenItems.Cells[35, iCNT]:=FormatFloat('###0.00', DiscountedAmt);
          MainForm.sgOpenItems.Cells[36, iCNT]:=FormatFloat('###0.00', DecreaseAmt);
          MainForm.sgOpenItems.Cells[37, iCNT]:=FormatFloat('###0.00', RecoveryAmt);
          OpenItems.cDiscountedAmt:=OpenItems.cDiscountedAmt + DiscountedAmt;
          OpenItems.cDecreaseAmt:=OpenItems.cDecreaseAmt + DecreaseAmt;
          OpenItems.cRecoveryAmt:=OpenItems.cRecoveryAmt + RecoveryAmt;
        end else
        begin
          MainForm.sgOpenItems.Cells[35, iCNT]:='0';
          MainForm.sgOpenItems.Cells[36, iCNT]:='0';
          MainForm.sgOpenItems.Cells[37, iCNT]:='0';
        end;

        { -------------------------------------------------------------------------------------------------------------------------------- COUNT ALL INVOICES }
        { DEPENDS ON INVOICE TYPE DEFINED IN THE GENERAL SETTINGS }
        if IsVoType(MainForm.sgOpenItems.Cells[3, iCNT]) = True then inc(OpenItems.nInvoices);

        { ---------------------------------------------------------------------------------------------------------- COUNT ALL OVERDUE INVOICES AND IT AMOUNT }
        { CHECK DIFFERENCE BETWEEN CURRENT DATE AND VOUCHER DATE }
        { VOUCHER TYPE TELLS IF WE HAVE INVOICE OR OTHER ITEM    }
        { WE COUNT ONLY INVOICES                                 }
        if (MainForm.sgOpenItems.Cells[34, iCNT] < '0') and (IsVoType(MainForm.sgOpenItems.Cells[3, iCNT]) = True) then
        begin
          inc(OpenItems.Overdue);
          OpenItems.OverdueAmt:=OpenItems.OverdueAmt + StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT])
        end;

        { ---------------------------------------------------------------------------------------------------------------------------------- COUNT O/S AMOUNT }
        if IsNumeric(MainForm.sgOpenItems.Cells[6, iCNT], 'float') = True then OpenItems.OSamt:=OpenItems.OSamt + StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT]);

        { ------------------------------------------------------------------------------------------------------------------------------ UNALLOCATED PAYMENTS }
        { WE TAKE INTO CONSIDERATION NEGATIVE AMOUNTS }
        { AND VOUCHER THAT INDICATE BANK POSTINGS     }
        if (MainForm.sgOpenItems.Cells[6, iCNT] < '0') and (MainForm.sgOpenItems.Cells[3, iCNT] = Settings.TMIG.ReadString(Settings.Unallocated, 'VOUCHER_NUM', '')) then
          OpenItems.UNamt:=OpenItems.UNamt + StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT]);

      end;

      { ----------------------------------------------- ! REPAINT 'STRINGGRID' AND END PROCESS ! ------------------------------------------------------------ }
      with MainForm.sgOpenItems do
      begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Repaint;
      end;
      PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
    end;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- SOURCE FILE SCANNING }
function TOpenItems.Scan(mode: integer): boolean;  (* ASYNC & SYNC *)
var
  iCNT:      integer;
  SR:        TSearchRec;
  TDate:     TDateTime;
  TTime1:    TDateTime;
  TTime2:    TDateTime;
  CheckSum:  integer;
begin
  Result:=False;
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  if mode = 0 then
  begin
    { IT IS ABSOLUTE NECESSARY FOR FILE-TIME SCANNING   }
    { IF SOURCES FILES ARE UPDATED BY THE SERVER        }
    { THEN WE WILL COMPARE TIMES AND IF ALL ARE CHANGED }
    { THEN WE START THREAD FOR OPEN ITEMS LOAD          }
    if FindFirst(OpenItems.OpenItemsDir + '*.txt', faAnyFile, SR) = 0 then
    begin
      iCNT:=0;
      SetLength(OpenItems.ArrOpenItems, iCNT + 1, 4);
      repeat
        if (SR.Attr <> faDirectory) then
        begin
          OpenItems.ArrOpenItems[iCNT, 0]:=SR.Name;
          FileAge(OpenItems.OpenItemsDir + SR.Name, TDate);
          OpenItems.ArrOpenItems[iCNT, 1]:=LeftStr(DateTimeToStr(TDate), 10);
          OpenItems.ArrOpenItems[iCNT, 2]:=RightStr(DateTimeToStr(TDate), 8);
          OpenItems.ArrOpenItems[iCNT, 3]:='0';
          (* DebugMsg(OpenItemsSources[iCNT, 0] + ' | ' + OpenItemsSources[iCNT, 1] + ' | ' + OpenItemsSources[iCNT, 2] + ' | ' + OpenItemsSources[iCNT, 3]); DEBUG LINE *)
          inc(iCNT);
          SetLength(OpenItems.ArrOpenItems, iCNT + 1, 4);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- SCAN AND COMPARE }
  if mode = 1 then
  begin
    { INITIALIZE }
    iCNT:=0;
    CheckSum:=0;
    { CHECK IF TIME-STAMP HAS CHANGED WITHIN A GIVEN FILE THAT WAS PRE-LOADED AT STARTUP }
    while iCNT < (High(OpenItems.ArrOpenItems) - 1) do
    begin
      FileAge(OpenItems.OpenItemsDir + OpenItems.ArrOpenItems[iCNT, 0], TDate);
      OpenItems.ArrOpenItems[iCNT, 3]:=RightStr(DateTimeToStr(TDate), 8);
      TTime1:=StrToTime(OpenItems.ArrOpenItems[iCNT, 2]);
      TTime2:=StrToTime(OpenItems.ArrOpenItems[iCNT, 3]);
      if ((TTime1 - TTime2) <> 0) then inc(CheckSum);
      (* DebugMsg(OpenItemsSources[iCNT, 0] + ' | ' + OpenItemsSources[iCNT, 1] + ' | ' + OpenItemsSources[iCNT, 2] + ' | ' + OpenItemsSources[iCNT, 3]); DEBUG LINE *)
      inc(iCNT);
    end;
    if iCNT = CheckSum then
    begin
      for iCNT:=0 to (High(OpenItems.ArrOpenItems) - 1) do OpenItems.ArrOpenItems[iCNT, 2]:=OpenItems.ArrOpenItems[iCNT, 3];
      Result:=True;
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- CO CODE NAME CONVERTION }
function TOpenItems.ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
var
  iCNT:  integer;
begin
  { INITIALIZE }
  Result:= '';
  { ALLOW TO CONVERT '2020' TO 'F2020', ETC. }
  { USED ONLY FOR OPEN ITEMS AND AGING VIEW  }
  if mode = 0 then
  begin
    if Length(CoNumber) = 4 then Result:=Prefix + CoNumber;
    if Length(CoNumber) = 3 then Result:=Prefix + '0'  + CoNumber;
    if Length(CoNumber) = 2 then Result:=Prefix + '00' + CoNumber;
  end;
  { CONVERTS FROM                            }
  {  1. 2020 TO 02020                        }
  {  2. 340  TO 00340                        }
  {  3. 43   TO 00043                        }
  {  4. 0    TO 00000                        }
  { USED ONLY TO BUILD GROUP_ID              }
  if mode = 1 then
  begin
    if Length(CoNumber) = 4 then Result:='0'   + CoNumber;
    if Length(CoNumber) = 3 then Result:='00'  + CoNumber;
    if Length(CoNumber) = 2 then Result:='000' + CoNumber;
    if Length(CoNumber) = 1 then Result:='00000';
  end;
  { CONVERTS FROM 02020 TO 2020              }
  if mode = 2 then
  begin
    for iCNT:= 1 to Length(CoNumber) do
      if CoNumber[iCNT] <> '0' then
      begin
        Result:=Copy(CoNumber, iCNT, MaxInt);
        Exit;
      end;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ RETURN KPI FOR GIVEN COMPANY }
function TOpenItems.ReturnKPI(SG: TStringGrid; StrCoCode: string; mode: integer): double;
{ MODE = 0 ===> KPI | OVERDUE FOR GIVEN CO CODE     }
{ MODE = 1 ===> KPI | UNALLOCATED FOR GIVEN CO CODE }
var
  iCNT:  integer;
begin
   { INITIALIZE }
   Result:=0;
   { LOOK FOR 'COCODE' AND SUM NUMBERS }
   for iCNT:=1 to SG.RowCount - 1 do
   begin
     if (mode = 0) and (StrCoCode = SG.Cells[1, iCNT]) then Result:=Result + StrToFloat(SG.Cells[15, iCNT]);
     if (mode = 1) and (StrCoCode = SG.Cells[1, iCNT]) then Result:=Result + StrToFloat(SG.Cells[16, iCNT]);
   end;
end;

{ ########################################################### ! MAIN FORM METHODS ! ######################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------- CONVERT TO STRING }
function TMainForm.OleGetStr(RecordsetField: variant): string;
begin
  try
    OleGetStr:=RecordsetField;
  except
    OleGetStr:=VarToStr(RecordsetField);  { CASE OF NULL FIELD }
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------ RETURN KEY VALUE FOR GIVEN LIST POSITION }
function TMainForm.FindKey(INI: TMemIniFile; OpenedSection: string; KeyPosition: integer): string;
var
  SL:  TStringList;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:='N/A';
  SL:=TStringList.Create;
  { ------------------------------------------------------------------------------------------------------------------------------------- EVALUATE AND RETURN }
  try
    INI.ReadSectionValues(OpenedSection, SL);
    if KeyPosition > SL.Count then Exit else
      begin
        Result:=LeftStr(SL.Strings[KeyPosition], AnsiPos('=', SL.Strings[KeyPosition]) - 1);
      end;
  finally
    SL.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- MODAL WINDOW }
function TMainForm.WndCall(WinForm: TForm; Mode: integer): integer;
begin
  Result:=0;

  { SETUP POPUPS }
  WinForm.PopupMode  :=pmAuto;
  WinForm.PopupParent:=MainForm;

  { CALL WINDOW }
  if Mode = 0 then Result:=WinForm.ShowModal;
  if Mode = 1 then WinForm.Show;

end;

{ --------------------------------------------------------------------------------------------------------------------------- WRAPPER FOR WINDOWS MESSAGE BOX }
function TMainForm.MsgCall(WndType: integer; WndText: string): integer;
begin
  Result:=0;
  if WndText = '' then Exit;
  if WndType = 1 { INFO       } then Result:=Application.MessageBox(PChar(WndText), PChar(Settings.AppName), MB_OK       + MB_ICONINFORMATION);
  if WndType = 2 { WARNING    } then Result:=Application.MessageBox(PChar(WndText), PChar(Settings.AppName), MB_OK       + MB_ICONWARNING);
  if WndType = 3 { ERROR      } then Result:=Application.MessageBox(PChar(WndText), PChar(Settings.AppName), MB_OK       + MB_ICONERROR);
  if WndType = 4 { QUESTION 1 } then Result:=Application.MessageBox(PChar(WndText), PChar(Settings.AppName), MB_OKCANCEL + MB_ICONQUESTION);
  if WndType = 5 { QUESTION 2 } then Result:=Application.MessageBox(PChar(WndText), PChar(Settings.AppName), MB_YESNO    + MB_ICONQUESTION);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- RESET SETTINGS }
procedure TMainForm.LockSettingsPanel;
begin

  { VISIBLE ON }
  MainForm.imgOFF.Visible         :=True;
  MainForm.btnPassUpdate.Enabled  :=False;

  { EDIT BOXES }
  MainForm.Edit_CurrPassWd.Enabled:=False;
  MainForm.Edit_NewPassWd.Enabled :=False;
  MainForm.Edit_ConfPassWd.Enabled:=False;
  MainForm.Edit_CurrPassWd.Text   :='';
  MainForm.Edit_NewPassWd.Text    :='';
  MainForm.Edit_ConfPassWd.Text   :='';
  MainForm.Edit_PASSWORD.Text     :='';

  { STRING GRIDS }
  MainForm.sgListSection.ClearAll(2, 0, 0, False);
  MainForm.sgListValue.ClearAll(2, 0, 0, False);
  MainForm.sgListSection.Row:=1;
  MainForm.sgListValue.Row:=1;
  MainForm.sgListSection.Enabled :=False;
  MainForm.sgListValue.Enabled   :=False;

  { ENDING }
  MainForm.btnUnlock.Caption:='Unlock';
  MainForm.Edit_PASSWORD.SetFocus;

end;

{ ################################################################## ! EVENTS ! ############################################################################# }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TMainForm.FormCreate(Sender: TObject);
var
  AppVersion:     string;
  MSSQL:          TMSSQL;
  InvoiceTracker: TInvoiceTracker;
begin

  { ------------------------------------------------------------ ! INITIALIZATION ! ------------------------------------------------------------------------- }

  (* SINGLETONS OBJECTS *)
  Settings   :=TSettings.Create('Unity');
  DataBase   :=TDataBase.Create;

  AgeView    :=TAgeView.Create;
  OpenItems  :=TOpenItems.Create;

  (* OTHER *)
  AppVersion:=GetBuildInfoAsString;
  KeyPreview:=True;
  AllowClose:=False;

  { ------------------------------------------------------------------------------------------------------------------------------ APPLICATION NAME | CAPTION }
  MainForm.Caption  :=Settings.TMIG.ReadString(Settings.ApplicationDetails, 'VALUE', Settings.APPNAME);
  GroupName.Caption :=Settings.TMIG.ReadString(Settings.ApplicationDetails, 'GROUP_NAME', 'n/a');

  { --------------------------------------------------------------- ! WINDOW POSITION ! --------------------------------------------------------------------- }

  MainForm.DefaultMonitor:=dmDesktop;          (* DO NOT CHANGE THAT *)
  MainForm.Position      :=poDefaultSizeOnly;  (* DO NOT CHANGE THAT *)
  MainForm.Top           :=Settings.TMIG.ReadInteger(Settings.ApplicationDetails, 'WINDOW_TOP',  0);
  MainForm.Left          :=Settings.TMIG.ReadInteger(Settings.ApplicationDetails, 'WINDOW_LEFT', 0);

  { ------------------------------------------------------------- ! DATE & TIME ! --------------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------------------------ FORMAT DATE AND TIME }
  cDateTime:=Now;
  StartTime:=Now;

  { --------------------------------------------------------------------------------------------------------------------------------------- REGIONAL SETTINGS }
  FormatDateTime('hh:mm:ss', cDateTime);
  FormatDateTime('hh:mm:ss', StartTime);

  { ------------------------------------------------------------- ! STATUS BAR ! ---------------------------------------------------------------------------- }
  StatBar_TXT1.Caption:='Ready.';
  StatBar_TXT2.Caption:=Settings.WinUserName + '.';
  StatBar_TXT3.Caption:=DateToStr(cDateTime);

  { ----------------------------------------------------------- ! DEFAULT VALUES ! -------------------------------------------------------------------------- }

  { -------------------------------------------------------------------------------------------------------------------------------------- SET INITIAL VALUES }
  Edit_PASSWORD.Text   :='';
  Edit_CurrPassWd.Text :='';
  Edit_NewPassWd.Text  :='';
  Edit_ConfPassWd.Text :='';
  btnPassUpdate.Enabled:=False;
  MyPages.ActivePage   :=TabSheet1;

  { ------------------------------------------------------------ ! RISK CLASSESS ! -------------------------------------------------------------------------- }

  { -------------------------------------------------------------------------------------------------------------------------------------------------- ASSIGN }
  { WE USE COMMA DECIMAL SEPARATOR BY DEFAULT }
  if FormatSettings.DecimalSeparator = ',' then
  begin
    procRISKA.Caption:=FloatToStr(StrToFloat(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_A_MAX', '0,80')) * 100) + '%';
    procRISKB.Caption:=FloatToStr(StrToFloat(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_B_MAX', '0,15')) * 100) + '%';
    procRISKC.Caption:=FloatToStr(StrToFloat(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_C_MAX', '0,5' )) * 100) + '%';
  end;
  { CHANGE COMMA DECIMAL SEPARATOR TO POINT DECIMAL SEPARATOR }
  if FormatSettings.DecimalSeparator = '.' then
  begin
    procRISKA.Caption:=FloatToStr(StrToFloat(StringReplace(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_A_MAX', '0,80'), ',', '.', [rfReplaceAll])) * 100) + '%';
    procRISKB.Caption:=FloatToStr(StrToFloat(StringReplace(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_B_MAX', '0,15'), ',', '.', [rfReplaceAll])) * 100) + '%';
    procRISKC.Caption:=FloatToStr(StrToFloat(StringReplace(Settings.TMIG.ReadString(Settings.RiskClassDetails, 'CLASS_C_MAX', '0,5' ), ',', '.', [rfReplaceAll])) * 100) + '%';
  end;
  { ------------------------------------------------------------- ! TAB SHEETS ! ---------------------------------------------------------------------------- }

  { --------------------------------------------------------------------------------------------------------------------------------------------------- NAMES }
  TabSheet1.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB1', 'TAB1');
  TabSheet2.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB2', 'TAB2');
  TabSheet3.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB3', 'TAB3');
  TabSheet4.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB4', 'TAB4');
  TabSheet5.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB5', 'TAB5');
  TabSheet6.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB6', 'TAB6');
  TabSheet7.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB7', 'TAB7');
  TabSheet8.Caption:=Settings.TMIG.ReadString(Settings.TabSheetsNames, 'TAB8', 'TAB8');

  { -------------------------------------------------------------- ! OPEN ITEMS ! --------------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------------------ FOLDERS, FORMAT AND LOADER }
  OpenItems.OpenItemsDir:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'FOLDER', '');
  OpenItems.OpenItemsFor:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'FORMAT', 'F{NUM}.TXT');

  { ------------------------------------------------------- ! ASSIGN PRE-DEFINED HEADERS ! ------------------------------------------------------------------ }

  { ----------------------------------------------------------------------------------------------------------------------------------- HEADERS AND FIRST ROW }
  sgOpenItems.RowCount:=2;
  sgOpenItems.Cols[0].Text:= 'Lp';
  sgOpenItems.Cols[1].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER1',  'SourceDBName');
  sgOpenItems.Cols[2].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER2',  'CustNo');
  sgOpenItems.Cols[3].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER3',  'Votp');
  sgOpenItems.Cols[4].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER4',  'VoDt');
  sgOpenItems.Cols[5].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER5',  'OpenCurAm');
  sgOpenItems.Cols[6].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER6',  'OpenAm');
  sgOpenItems.Cols[7].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER7',  'Nm');
  sgOpenItems.Cols[8].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER8',  'ISO');
  sgOpenItems.Cols[9].Text:= Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER9',  'CurAm');
  sgOpenItems.Cols[10].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER10', 'Am');
  sgOpenItems.Cols[11].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER11', 'InvoNo');
  sgOpenItems.Cols[12].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER12', 'DueDt');
  sgOpenItems.Cols[13].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER13', 'Inf4');
  sgOpenItems.Cols[14].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER14', 'Inf7');
  sgOpenItems.Cols[15].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER15', 'CrLmt');
  sgOpenItems.Cols[16].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER16', 'Ctry');
  sgOpenItems.Cols[17].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER17', 'CPmtTrm');
  sgOpenItems.Cols[18].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER18', 'PdSts');
  sgOpenItems.Cols[19].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER19', 'Agent');
  sgOpenItems.Cols[20].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER20', 'Ctrl');
  sgOpenItems.Cols[21].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER21', 'Ad1');
  sgOpenItems.Cols[22].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER22', 'Ad2');
  sgOpenItems.Cols[23].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER23', 'Ad3');
  sgOpenItems.Cols[24].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER24', 'PNo');
  sgOpenItems.Cols[25].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER25', 'PArea');
  sgOpenItems.Cols[26].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER26', 'GenAcNo');
  sgOpenItems.Cols[27].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER27', 'ValDt');
  sgOpenItems.Cols[28].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER28', 'R1');
  sgOpenItems.Cols[29].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER29', 'Gr3');
  sgOpenItems.Cols[30].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER30', 'Txt');
  sgOpenItems.Cols[31].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER31', 'R8');
  sgOpenItems.Cols[32].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER32', 'DirDeb');
  sgOpenItems.Cols[33].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER33', 'AddTxt');
  { ---------------------------------------------------------------------------------------------- ADDITIONAL COLUMNS | HOLDS DATA THAT HAVE TO BE CALCULATED }
  sgOpenItems.Cols[34].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER34', 'PmtStat');
  sgOpenItems.Cols[35].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER35', 'Discount');
  sgOpenItems.Cols[36].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER36', 'Decrease');
  sgOpenItems.Cols[37].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER37', 'Recover');
  sgOpenItems.Cols[38].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER38', 'HASHCOLUMN');

  { ------------------------------------------------------- ! CAPTIONS FOR ALL SHAPES ! --------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------------- PRE-DEFINED TEXTS INSIDE SHAPES }

  (* AGING REPORT | TABSHEET1 *)

  Cap01.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS1TXT01', 'EMPTY'), [fsBold]);
  Cap02.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS1TXT02', 'EMPTY'), [fsBold]);
  Cap03.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS1TXT03', 'EMPTY'), [fsBold]);
  Cap05.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS1TXT05', 'EMPTY'), [fsBold]);
  Cap06.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS1TXT06', 'EMPTY'), [fsBold]);
  Cap07.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS1TXT07', 'EMPTY'), [fsBold]);

  (* OPEN ITEMS | TABSHEET2 *)

  Cap10.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS2TXT01', 'EMPTY'), [fsBold]);
  Cap11.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS2TXT02', 'EMPTY'), [fsBold]);
  Cap12.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS2TXT03', 'EMPTY'), [fsBold]);

  (* ADDRESS BOOK | TABSHEET3 *)

  Cap13.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS3TXT01', 'EMPTY'), [fsBold]);

  (* INVOICE TRACKER | TABSHEET4 *)

  Cap43.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS4TXT01', 'EMPTY'), [fsBold]);

  (* GENERAL TABLES  | TABSHEET7 *)

  Cap15.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS7TXT01', 'EMPTY'), [fsBold]);
  Cap16.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS7TXT02', 'EMPTY'), [fsBold]);
  Cap17.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS7TXT03', 'EMPTY'), [fsBold]);
  Cap18.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS7TXT04', 'EMPTY'), [fsBold]);
  Cap19.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS7TXT05', 'EMPTY'), [fsBold]);
  Cap20.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS7TXT06', 'EMPTY'), [fsBold]);

  (* SETTINGS | TABSHEET8 *)

  Cap21.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS8TXT01', 'EMPTY'), [fsBold]);
  Cap22.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS8TXT02', 'EMPTY'), [fsBold]);
  Cap23.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS8TXT03', 'EMPTY'), [fsBold]);
  Cap27.ShapeText(10, 1, Settings.TMIG.ReadString(Settings.TabSheetsCaps, 'TS8TXT04', 'EMPTY'), [fsBold]);

  { -------------------------------------------------------- ! ADDRESS BOOK TABSHEET ! ---------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------------------------------------ DEFAULTS }
  sgAddressBook.RowCount:=2;

  { -------------------------------------------------------------- ! MAIN VIEW ! ---------------------------------------------------------------------------- }

  { --------------------------------------------------------------------------------------------------------------------------------- AGING BUCKETS | CAPTION }
  tR1.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE1A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE1B','');
  tR2.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE2A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE2B','');
  tR3.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE3A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE3B','');
  tR4.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE4A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE4B','');
  tR5.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE5A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE5B','');
  tR6.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE6A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE6B','');

  { ---------------------------------------------------------------------------------------------------------------------------------- SUMMARY BOX | CAPTIONS }
  Text21.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE1A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE3B','') + ':';
  Text22.Caption:=Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE4A','') + ' - ' + Settings.TMIG.ReadString(Settings.AgingRanges,'RANGE6B','') + ':';

  { -------------------------------------------------------- ! OTHER INITIALIZATIONS ! ---------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------- SOURCE FILE SCANNING | INITIALIZATION }
  OpenItems.Scan(0);

  { ------------------------------------------------------- ! DATABASE INITIALIZATION  ! -------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------ DATABASE CONNECTION AND READ OUT OF GENERAL TABLES }
  DataBase.InitializeConnection(MainThreadID, True);
  if Database.ADOConnect.Connected then
    if not (Database.UACinitialize) then Application.Terminate else
    begin
      { --------------------------------------------------------------------------------------------------------- UAC GROUP LIST READ OUT & POPULATE LIST BOX }
      Database.UACGroupReader(UpperCase(Settings.WinUserName));
      Database.UACAgeDates(Database.ArrGroupList[GroupListBox.ItemIndex, 0]);
      if GroupListDates.Text <> '' then TTReadAgeView.Create('1') else GroupListDates.Enabled:=False;
      { ------------------------------------------------------------------------------------------------------------------------------------------- LOAD MAPS }
      MSSQL:=TMSSQL.Create(Database.ADOConnect);
      try
        MSSQL.StrSQL:='SELECT * FROM ' + Settings.TMIG.ReadString(Settings.GeneralTables, 'MAP1', ''); MSSQL.SqlToGrid(sgCoCodes,  MSSQL.OpenSQL, False);
        MSSQL.StrSQL:='SELECT * FROM ' + Settings.TMIG.ReadString(Settings.GeneralTables, 'MAP4', ''); MSSQL.SqlToGrid(sgPmtTerms, MSSQL.OpenSQL, False);
        MSSQL.StrSQL:='SELECT * FROM ' + Settings.TMIG.ReadString(Settings.GeneralTables, 'MAP5', ''); MSSQL.SqlToGrid(sgPaidInfo, MSSQL.OpenSQL, False);
        MSSQL.StrSQL:='SELECT * FROM ' + Settings.TMIG.ReadString(Settings.GeneralTables, 'MAP6', ''); MSSQL.SqlToGrid(sgGroup3,   MSSQL.OpenSQL, False);
        MSSQL.StrSQL:='SELECT * FROM ' + Settings.TMIG.ReadString(Settings.GeneralTables, 'MAP7', ''); MSSQL.SqlToGrid(sgPerson,   MSSQL.OpenSQL, False);
      finally
        MSSQL.Free;
      end;
      { --------------------------------------------------------------------------------------------------------------------------- LOAD INVOICE TRACKER LIST }
      InvoiceTracker:=TInvoiceTracker.Create;
      try
        InvoiceTracker.Refresh(MainForm.sgInvoiceTracker, 'ALL');
      finally
        InvoiceTracker.Free;
      end;
    end;

  { -------------------------------------------------------------------------------------------------------------------------- APPLICATION VERSION & USER SID }
  LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: Application version = ' + AppVersion);
  LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: User SID = ' + GetCurrentUserSid);

  { ---------------------------------------------------------- ! TIMERS INTERVALS ! ------------------------------------------------------------------------- }

  (* 'INETTIMER' IS EXCLUDED FROM BELOW LIST BECAUSE IT IS CONTROLED BY 'INITIAIZECONNECTION' METHOD *)

  EventLogTimer.Interval   :=Settings.TMIG.ReadInteger(Settings.TimersSettings, 'EVENTLOG_UPDATE', 60000);  { DEFAULT VALUE 60000   MILISECONDS = 1  MINUTE  }
  InvoiceScanTimer.Interval:=Settings.TMIG.ReadInteger(Settings.TimersSettings, 'INVOICE_SCANNER', 900000); { DEFAULT VALUE 900000  MILISECONDS = 15 MINUTES }
  UpdaterTimer.Interval    :=Settings.TMIG.ReadInteger(Settings.TimersSettings, 'UPDATE_CHECKER',  60000);  { DEFAULT VALUE 60000   MILISECONDS = 1  MINUTE  }
  OILoader.Interval        :=Settings.TMIG.ReadInteger(Settings.TimersSettings, 'OI_LOADER',       300000); { DEFAULT VALUE 3000000 MILISECONDS = 5  MINUTES }

  { START }
  EventLogTimer.Enabled    :=True;
  InvoiceScanTimer.Enabled :=True;
  UpdaterTimer.Enabled     :=True;
  OILoader.Enabled         :=True;
  { TIME AND DATE ON STATUS BAR }
  CurrentTime.Enabled      :=True;
  UpTime.Enabled           :=True;

end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ ON DESTROY }
procedure TMainForm.FormDestroy(Sender: TObject);
begin

  (* RELEASE OBJECTS FROM MEMORY *)
  //replace this singleton by dependancy injection!!!
  FreeAndNil(Settings);

  //SINGLETONS! REFACTOR !!!
  FreeAndNil(DataBase);
  FreeAndNil(AgeView);
  FreeAndNil(OpenItems);
end;

{ ------------------------------------------------------------- ! MAIN FORM EVENTS ! ------------------------------------------------------------------------ }

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TMainForm.FormShow(Sender: TObject);
begin
  FormResize(self);
  (* CustomPanelBorders(Header1, clWhite, clRed, clRed, clRed, clRed); TPANEL TEST LINE *)
  MainForm.sgCoCodes.SetColWidth (10, 30);
  MainForm.sgPmtTerms.SetColWidth(10, 30);
  MainForm.sgPaidInfo.SetColWidth(10, 30);
  MainForm.sgGroup3.SetColWidth  (10, 30);
  MainForm.sgPerson.SetColWidth  (10, 30);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ MAIN FORM RESIZE }
procedure TMainForm.FormResize(Sender: TObject);
begin
  (* EACH TIME FORM IS RESIZED WE UPDATE THUMB SIZE OF STRING GRID COMPONENT *)
  sgOpenItems.AutoThumbSize;
  sgAddressBook.AutoThumbSize;
  sgListSection.AutoThumbSize;
  sgListValue.AutoThumbSize;
  sgInvoiceTracker.AutoThumbSize;
  sgAgeView.AutoThumbSize;
  sgCoCodes.AutoThumbSize;
  sgPmtTerms.AutoThumbSize;
  sgPaidInfo.AutoThumbSize;
  sgPerson.AutoThumbSize;
  sgGroup3.AutoThumbSize;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- ON CLOSE QUERY }
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  { GO MINIMIZE AND HIDE FROM TASKBAR | DO NOT CLOSE }
  if not AllowClose then
  begin
    CanClose:=False;
    ShowWindow(Handle, SW_MINIMIZE);
    Hide();
  end
    else
      { SHUTDOWN APPLICATION }
      begin

        { -------------------------------------------------------------------------------------------------------------------------------- LAST FORM POSITION }
        Settings.TMIG.WriteInteger(Settings.ApplicationDetails, 'WINDOW_TOP',   MainForm.Top);
        Settings.TMIG.WriteInteger(Settings.ApplicationDetails, 'WINDOW_LEFT',  MainForm.Left);
        if MainForm.WindowState = wsNormal    then Settings.TMIG.WriteString(Settings.ApplicationDetails,  'WINDOW_STATE', 'wsNormal');
        if MainForm.WindowState = wsMaximized then Settings.TMIG.WriteString(Settings.ApplicationDetails,  'WINDOW_STATE', 'wsMaximized');
        if MainForm.WindowState = wsMinimized then Settings.TMIG.WriteString(Settings.ApplicationDetails,  'WINDOW_STATE', 'wsMinimized');

        { -------------------------------------------------------------------------------------------------------------------------- SAVE CURRENT GRID LAYOUT }
        if sgAgeView.RowCount > 2 then sgAgeView.SaveLayout(Settings.ColumnWidthName, Settings.ColumnOrderName, Settings.ColumnNames, Settings.ColumnPrefix);

        { ------------------------------------------------------------------------------------------------------------------- ENCODE TMIp AND SAVE IT TO FILE }
        Encode(Settings.AppDir + Settings.LogonFile, Settings.FILEKEY, True, Error, Settings.TMIP);
        if Error <> 0 then
        begin
          Application.MessageBox(PChar('Cannot write user configuration file. Error code: ' + IntToStr(Error) + '. Please contact support.'), PChar(CAPTION), MB_OK + MB_ICONSTOP);
          Application.Terminate;
        end;

        { ------------------------------------------------------------------------------------------------------------------- ENCODE TMIg AND SAVE IT TO FILE }
        Encode(Settings.AppDir + Settings.ConfigFile, Settings.FILEKEY, True, Error, Settings.TMIG);
        if Error <> 0 then
        begin
          Application.MessageBox(PChar('Cannot write master configuration file. Error code: ' + IntToStr(Error) + '. Please contact support.'), PChar(CAPTION), MB_OK + MB_ICONSTOP);
          Application.Terminate;
        end;

        { ------------------------------------------------------------------------------------------------------------------------------------------- LOG END }
        LogText(Settings.AppDir + Settings.LogFile, 'Application closed by the user.');

        { ----------------------------------------------------------------------------------------------------------------------------------- RELEASE & CLOSE }
        Settings.TMIG.Free;
        Settings.TMIP.Free;

        { ---------------------------------------------------------------------------------------------------------------------------------------- TIMERS OFF }
        if InvoiceScanTimer.Enabled then InvoiceScanTimer.Enabled:=False;
        if OILoader.Enabled         then OILoader.Enabled        :=False;
        if EventLogTimer.Enabled    then EventLogTimer.Enabled   :=False;
        if UpdaterTimer.Enabled     then UpdaterTimer.Enabled    :=False;
        if UpTime.Enabled           then UpTime.Enabled          :=False;
        if CurrentTime.Enabled      then CurrentTime.Enabled     :=False;
        if InetTimer.Enabled        then InetTimer.Enabled       :=False;

        { --------------------------------------------------------------------------------------------------------------------------------------- ALLOW CLOSE }
        CanClose:=True;

      end;
end;

{ ------------------------------------------------------------------ ! TIMERS ! ----------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------- CHECK PERIODICALLY FOR UPDATES }
procedure TMainForm.UpdaterTimerTimer(Sender: TObject);
begin
  { ... CODE HERE ... }
end;

{ ----------------------------------------------------------------------------------------------------------------------- MIRROR EVENT LOG FILE IN MEMO FIELD }
procedure TMainForm.EventLogTimerTimer(Sender: TObject);
begin
  try
    try
      EventLog.Lines.LoadFromFile(Settings.AppDir + Settings.LogFile);
    except
      EventLog.Lines.Text:='Exception catch message: cannot load event log file. The file is locked by another process. Please try again later.';
    end;
  finally
    EventLog.SelStart:=Length(EventLog.Text);
    EventLog.SelLength:=0;
    SendMessage(EventLog.Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------- CHECK INTERNET CONNECTION PERIODICALLY }
procedure TMainForm.InetTimerTimer(Sender: TObject);
begin
  TTCheckServerConnection.Create(False);
end;

{ --------------------------------------------------------------------------------------------------------------------------- INVOICE TRACKER SCANNING METHOD }
procedure TMainForm.InvoiceScanTimerTimer(Sender: TObject);
begin
  TTInvoiceTrackerScanner.Create(False);
end;

{ --------------------------------------------------------------------------------------------------------------------------------- AUTOLOADER FOR OPEN ITEMS }
procedure TMainForm.OILoaderTimer(Sender: TObject);
begin
  LogText(Settings.AppDir + Settings.LogFile, 'Thread [' + IntToStr(MainThreadID) + ']: Calling open items scanner...');
  TTOpenItemsScanner.Create(False);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SHOW CURRENT TIME }
procedure TMainForm.CurrentTimeTimer(Sender: TObject);
begin
  cDateTime:=Now;
  StatBar_TXT4.Caption:=TimeToStr(cDateTime);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW UPTIME }
procedure TMainForm.UpTimeTimer(Sender: TObject);
var
  Result: TDateTime;
begin
  Result:=cDateTime - StartTime;
  StatBar_TXT5.Caption:=TimeToStr(Result);
end;

{ ---------------------------------------------------------------- ! POPUP MENUS ! -------------------------------------------------------------------------- }

{ ------------------------------------------------------------- ! ADDRESS BOOK MENU ! ----------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK CONTEXT MENU }
procedure TMainForm.BookPopupPopup(Sender: TObject);
begin
  Action_ShowMyEntries.Caption:='Show ' + UpperCase(Settings.WinUserName) + ' entries';
  { CHECK IF USER SELECT A RANGE }
  if (sgAddressBook.Selection.Bottom - sgAddressBook.Selection.Top) > 0 then
    { WE ALLOW TO DELETE ONLY ONE LINE AT THE TIME }
    Action_DelRow.Enabled:=False
      else
        Action_DelRow.Enabled:=True;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------- CUT }
procedure TMainForm.Action_CutClick(Sender: TObject);
begin
  sgAddressBook.CopyCutPaste(2);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ COPY }
procedure TMainForm.Action_CopyClick(Sender: TObject);
begin
  sgAddressBook.CopyCutPaste(1);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- PASTE }
procedure TMainForm.Action_PasteClick(Sender: TObject);
begin
  sgAddressBook.CopyCutPaste(0);
end;

{ ------------------------------------------------------------------------------------------------------------------------- ADD EMPTY ROW TO THE ADDRESS BOOK }
procedure TMainForm.Action_AddRowClick(Sender: TObject);
var
  jCNT:  integer;
begin
  sgAddressBook.RowCount:=sgAddressBook.RowCount + 1;
  sgAddressBook.Row:=sgAddressBook.RowCount - 1;
  sgAddressBook.Cells[0, sgAddressBook.RowCount - 1]:='';
  sgAddressBook.Cells[1, sgAddressBook.RowCount - 1]:=Settings.WinUserName;
  for jCNT:=2 to sgAddressBook.ColCount - 1 do sgAddressBook.Cells[jCNT, sgAddressBook.RowCount - 1]:='';
end;

{ --------------------------------------------------------------------------------------------------------------------------------- DELETE GIVEN CUID FROM DB }
procedure TMainForm.Action_DelRowClick(Sender: TObject);
var
  MSSQL:  TMSSQL;
begin

  { ASK BEFORE DELETE }
  if MsgCall(5, 'Are you sure you want to delete this customer?' + #13#10 + 'This operation cannot be reverted.') = IDNO then Exit;

  { EXIT IF NO DATABASE CONNECTION }
  if not (DataBase.LastError = 0) then
  begin
    MsgCall(2, 'Cannot connect with database. Please contact IT support.');
    Exit;
  end
    else
    begin
      { EXECUTE DELETE QUERY }
      if MainForm.sgAddressBook.Cells[0, MainForm.sgAddressBook.Row] <> '' then
      begin
        MSSQL:=TMSSQL.Create(Database.ADOConnect);
        try
          MSSQL.StrSQL:='DELETE FROM tbl_AddressBook WHERE CUID = ' + MSSQL.CleanStr(MainForm.sgAddressBook.Cells[2, MainForm.sgAddressBook.Row], True);
          MSSQL.ExecSQL;
        finally
          MSSQL.Free;
          sgAddressBook.DeleteRowFrom(1, 1);
        end;
      end
        else
          sgAddressBook.DeleteRowFrom(1, 1);
    end;

end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- OPEN SEARCH WINDOW }
procedure TMainForm.Action_SearchBookClick(Sender: TObject);
begin
  { SETUP AND CALL WINDOW }
  SearchForm.SGrid     :=MainForm.sgAddressBook;
  SearchForm.SColName  :='CUSTNAME';
  SearchForm.SColNumber:='CUSTNUMBER';
  WndCall(SearchForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ SHOW ALL ENTRIES }
procedure TMainForm.Action_ShowAsIsClick(Sender: TObject);
begin
  TTAddressBook.Create('1', '');
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ SHOW USER ENTRIES ONLY }
procedure TMainForm.Action_ShowMyEntriesClick(Sender: TObject);
begin
  TTAddressBook.Create('1', Settings.WinUserName);
end;

{ -------------------------------------------------------------- ! MAIN FORM MENU ! ------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- EXECUTE WHEN MENU OPENS }
procedure TMainForm.AgeViewPopupPopup(Sender: TObject);
begin
  { CHECK IF USER SELECT A RANGE ON AGEGRID }
  if (sgAgeView.Selection.Bottom - sgAgeView.Selection.Top) > 0 then
    { WE ALLOW TO ADD CUSTOMER TO INVOICE TRACKER ONLY ONE AT A TIME }
    Action_Tracker.Enabled:=False
      else
        Action_Tracker.Enabled:=True;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- HIDE WINDOW }
procedure TMainForm.Action_HideAppClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_MINIMIZE);
  Hide();
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW WINDOW }
procedure TMainForm.Action_ShowAppClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_NORMAL);
  Show();
  Application.BringToFront;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- CLOSE }
procedure TMainForm.Action_CloseClick(Sender: TObject);
begin
  (* DO NOTHING *)
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- ABOUT }
procedure TMainForm.Action_AboutClick(Sender: TObject);
begin
  WndCall(AboutForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ HELP }
procedure TMainForm.Action_HelpClick(Sender: TObject);
begin
  (* EXECUTE EXTERNAL PDF FILE *)
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- STAY ON TOP }
procedure TMainForm.Action_OnTopClick(Sender: TObject);
begin
  if Action_OnTop.Checked = False then
  begin
    MainForm.FormStyle:=fsStayOnTop;
    Action_OnTop.Checked:=True;
  end else
  begin
    MainForm.FormStyle:=fsNormal;
    Action_OnTop.Checked:=False;
  end;
end;

{ ----------------------------------------------------------------- ! AGE VIEW ! ---------------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- LYNC CALL }
procedure TMainForm.Action_LyncCallClick(Sender: TObject);
begin
  WndCall(ActionsForm, 0);
end;

{ ---------------------------------------------------------------------------------------------------------------------- ADD TO INVOICE TRACKER | WINDOW CALL }

procedure TMainForm.Action_TrackerClick(Sender: TObject);
begin
  WndCall(TrackerForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------ ADD SELECTED ITEMS TO ADDRESS BOOK }
procedure TMainForm.Action_AddToBookClick(Sender: TObject);
var
  iCNT:    integer;
  jCNT:    integer;
  cCNT:    integer;
  Row:     integer;
  OffSet:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  Row:=sgAgeView.Selection.Top;
  if sgAddressBook.Cells[0, 1] = '' then OffSet:=-1 else OffSet:=0;
  { ----------------------------------------------------------------------------------------------------------------------- GO ONE BY ONE AND LOOK FOR 'CUID' }
  for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
  begin
    for jCNT:=1 to sgOpenItems.RowCount - 1 do
      { ------------------------------------------------------------------------------------------------------------------- ADD DATA TO ADDRESS BOOK IF FOUND }
      if (sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1), iCNT] = sgOpenItems.Cells[38, jCNT]) and (sgAgeView.RowHeights[Row] <> - 1) then
      begin
        sgAddressBook.RowCount:=sgAddressBook.RowCount + 1;
        { ----------------------------------------------------------------------------------------------------------------------------------------- MOVE DATA }
        sgAddressBook.Cells[0, sgAddressBook.RowCount - 1 + OffSet]:='';
        sgAddressBook.Cells[1, sgAddressBook.RowCount - 1 + OffSet]:=Settings.WinUserName;
        sgAddressBook.Cells[2, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[38, jCNT];
        sgAddressBook.Cells[3, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[2,  jCNT];
        sgAddressBook.Cells[4, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[7,  jCNT];
        sgAddressBook.Cells[9, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[22, jCNT] + ' ' +
                                                                     sgOpenItems.Cells[23, jCNT] + ' ' +
                                                                     sgOpenItems.Cells[24, jCNT] + ' ' +
                                                                     sgOpenItems.Cells[25, jCNT] + ' ' +
                                                                     sgOpenItems.Cells[26, jCNT];
        { ---------------------------------------------------------------------------------------------------------------------------------------- EMPTY ROWS }
        for cCNT:=5 to 8 do sgAddressBook.Cells[cCNT, sgAddressBook.RowCount - 1]:='';
        { ---------------------------------------------------------------------------------------------------------------------------------------------- QUIT }
        Break;
      end;
    inc(Row);
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  if (sgAgeView.Selection.Bottom - sgAgeView.Selection.Top) = 0 then
  begin
    MsgCall(1, 'Customer has been addedd to the Address Book tabsheet.');
  end else
  begin
      MsgCall(1, 'Customers have been addedd to the Address Book tabsheet.');
  end;
  Screen.Cursor:=crDefault;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- FILTER INF7 COLUMN }
procedure TMainForm.Action_FilterINF7Click(Sender: TObject);
begin
  FilterForm.FColName:='INF7';
  FilterForm.FGrid   :=MainForm.sgAgeView;
  WndCall(FilterForm, 0);
end;

{ --------------------------------------------------------------------------------------------------------------------------------- EXCLUDE NON-OVERDUE ITEMS }
procedure TMainForm.Action_OverdueClick(Sender: TObject);
begin
  // code here...
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- SEARCH CUSTOMER }
procedure TMainForm.Action_SearchClick(Sender: TObject);
begin
  { SETUP AND CALL WINDOW }
  SearchForm.SGrid     :=MainForm.sgAgeView;
  SearchForm.SColName  :='CUSTOMER NAME';
  SearchForm.SColNumber:='CUSTOMER NUMBER';
  WndCall(SearchForm, 0);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SHOW PAYMENT TERM }
procedure TMainForm.Action_PaymentTermClick(Sender: TObject);
begin
  MsgCall(1, 'Payment term: ' + AgeView.MapData(sgAgeView, 'PAYMENT_TERMS', sgPmtTerms) + '.');
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW GROUP3 }
procedure TMainForm.Action_Group3Click(Sender: TObject);
begin
  MsgCall(1, 'Assigned to Group3: ' + AgeView.MapData(sgAgeView, 'GROUP3', sgGroup3) + '.');
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW PERSON }
procedure TMainForm.Action_PersonClick(Sender: TObject);
begin
  MsgCall(1, 'Assigned to Person: ' + AgeView.MapData(sgAgeView, 'PERSON', sgPerson) + '.');
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- SHOW INF4 }
procedure TMainForm.Action_INF4Click(Sender: TObject);
var
  Return:  string;
begin
  Return:=sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('INF4', 1, 1) , sgAgeView.Row];
  if (Return = '') or (Return = ' ') then Return:='unassigned';
  MsgCall(1, 'Assigned to INF4: ' + Return + '.');
end;

{ ------------------------------------------------------------------------------------------------------------------------------ SAVE STRING GRID TO MS EXCEL }
procedure TMainForm.Action_ToExceClick(Sender: TObject);
begin
  TTExcelExport.Create;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- AUTO COLUMN RE-SIZE }
procedure TMainForm.Action_AutoColumnSizeClick(Sender: TObject);
begin
  MainForm.sgAgeView.SetColWidth(10, 20);
end;

{ ------------------------------------------------------------------------------------------------------- SHOW ONLY BASIC VIEW DEFINIED IN CONFIGURATION FILE }
procedure TMainForm.Action_BasicViewClick(Sender: TObject);   //refactor!!!
var
  iCNT:  integer;
begin
  { RE-SET VIEW }
  for iCNT:=0 to MainForm.sgAgeView.ColCount - 2 do
    if Settings.TMIG.ReadString(Settings.AgingBasic, MainForm.FindKey(Settings.TMIG, Settings.AgingBasic, iCNT), 'True') = 'False' then
      MainForm.sgAgeView.ColWidths[MainForm.sgAgeView.ReturnColumn(MainForm.FindKey(Settings.TMIG, Settings.AgingBasic, iCNT), 1, 1)]:= -1
        else
          MainForm.sgAgeView.ColWidths[MainForm.sgAgeView.ReturnColumn(MainForm.FindKey(Settings.TMIG, Settings.AgingBasic, iCNT), 1, 1)]:= 100;
  { AUTO RESIZE }
  MainForm.sgAgeView.SetColWidth(10, 20);
  { TICK }
  Action_BasicView.Checked:=True;
  Action_FullView.Checked:=False;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- SHOW ALL AVAILABLE COLUMNS }
procedure TMainForm.Action_FullViewClick(Sender: TObject);  //refactor!!!
var
  iCNT:  integer;
begin
  { RE-SET VIEW }
  for iCNT:=0 to MainForm.sgAgeView.ColCount - 2 do
    if Settings.TMIG.ReadString(Settings.AgingFull, MainForm.FindKey(Settings.TMIG, Settings.AgingFull, iCNT), 'True') = 'False' then
      MainForm.sgAgeView.ColWidths[MainForm.sgAgeView.ReturnColumn(MainForm.FindKey(Settings.TMIG, Settings.AgingFull, iCNT), 1, 1)]:= -1
        else
          MainForm.sgAgeView.ColWidths[MainForm.sgAgeView.ReturnColumn(MainForm.FindKey(Settings.TMIG, Settings.AgingBasic, iCNT), 1, 1)]:= 100;
  { AUTO RESIZE }
  MainForm.sgAgeView.SetColWidth(10, 20);
  { TICK }
  Action_BasicView.Checked:=False;
  Action_FullView.Checked:=True;
end;

{ -------------------------------------------------------------- ! INVOICE TRACKER ! ------------------------------------------------------------------------ }

{ ------------------------------------------------------------------------------------------------------------------------------------------ REMOVE FROM LIST }
procedure TMainForm.Action_RemoveClick(Sender: TObject);
begin

  { R/W USER CAN REMOVE ITEM }
  if (Database.AccessLevel = 'RW') and (UpperCase(Settings.WinUserName) = UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
    if MsgCall(5, 'Are you sure you want to remove selected customer?') = IDYES then
      TTInvoiceTrackerRefresh.Create('REMOVE');

  { R/W USER CANNOT REMOVE OTHER ITEM }
  if (Database.AccessLevel = 'RW') and (UpperCase(Settings.WinUserName) <> UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
    MsgCall(2, 'You cannot remove someone''s else item.');

  { ADMINISTRATOR CAN REMOVE ANY ITEM }
  if (Database.AccessLevel = 'AD') then
    if MsgCall(5, 'Are you sure you want to remove selected customer?') = IDYES then TTInvoiceTrackerRefresh.Create('REMOVE');

  { READ ONLY USER CANNOT REMOVE ANYTHING }
  if (Database.AccessLevel = 'RO') then MsgCall(2, 'You don''t have permission to remove items.');

end;

{ ----------------------------------------------------------------------------------------------------------------------- SHOW SENT INVOICES FOR GIVEN 'CUID' }
procedure TMainForm.Action_ShowRegisteredClick(Sender: TObject);
begin
  WndCall(InvoicesForm, 0);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- SHOW MY ITEMS }
procedure TMainForm.Action_ShowMyClick(Sender: TObject);
begin
  TTInvoiceTrackerRefresh.Create(UpperCase(Settings.WinUserName));
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SHOW ALL ITEMS }
procedure TMainForm.Action_ShowAllClick(Sender: TObject);
begin
  TTInvoiceTrackerRefresh.Create('ALL');
end;

{ ---------------------------------------------------------------- ! TRYICON CALLS ! ------------------------------------------------------------------------ }

{ --------------------------------------------------------------------------------------------------------------------------------- SHOW THE APPLICATION FORM }
procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  MainForm.Action_ShowAppClick(self);
end;

{ ---------------------------------------------------------- ! COMPONENT EVENTS | LISTBOX ! ----------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------- UAC | LIST BOX | UPDATE AGE DATES }
procedure TMainForm.GroupListBoxSelect(Sender: TObject);
begin
  Database.UACAgeDates(Database.ArrGroupList[MainForm.GroupListBox.ItemIndex, 0]);
  GroupListDates.ItemIndex:=GroupListDates.Items.Count - 1;
end;

{ ------------------------------------------------------- ! COMPONENT EVENTS | TABSHEETS ! ------------------------------------------------------------------ }

{ ---------------------------------------------------------------------------------------------------------------------------------------------- REFRESH VIEW }
procedure TMainForm.TabSheet7Resize(Sender: TObject);
begin
  TabSheet7Show(self);
end;

{ --------------------------------------------------------------------------------------------------------------- REFRESH THE LIST AFTER ADDING/REMOVING ITEM }
procedure TMainForm.TabSheet4Show(Sender: TObject);
begin
  TTInvoiceTrackerRefresh.Create('ALL');
end;

{ ------------------------------------------------------------------------------------------------------------ MAKE PAYMENT TERMS AND PAID INFO TABLES HEIGHT }
procedure TMainForm.TabSheet7Show(Sender: TObject);
begin
  sgPmtTerms.Height:=Round(0.5 * sgCoCodes.Height);
  sgPaidInfo.Height:=Round(0.5 * sgCoCodes.Height);
end;

{ ----------------------------------------------------------------------------------------------------------------------------- LOCK SETTING PANEL WHEN LEAVE }
procedure TMainForm.TabSheet8Show(Sender: TObject);
begin
  MainForm.LockSettingsPanel;
end;

{ -------------------------------------------------------- ! COMPONENT EVENTS | GRIDS ! --------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------- MOVE COLUMN AND UPDATE SQL COLUMN ARRAY }
procedure TMainForm.sgAgeViewColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
var
  iCNT:    integer;
  jCNT:    integer;
  Temp:    TStrArray;
  SqlRows: integer;
  TmpRows: integer;
begin
  try
    try
      { ------------------------------------------------------------------------------------------------------------------------------- SETTING UP DIMENSIONS }

      (* WARNING! "HIGH" METHOD RETURNS NUMBER OF ROWS COUNTING FROM ZERO *)
      (*          WHILE "SETLENGTH" METHOD SETUP ARRAY COUNTING FROM ONE  *)
      (*          THEREFORE, WE NEED ADD ONE TO MATCH DIMENSIONS          *)

      SqlRows:=high(sgAgeView.SqlColumns);
      Inc(SqlRows);
      SetLength(Temp, SqlRows, 2);
      TmpRows:=high(Temp);
      { ------------------------------------------------------------------------------------------------------------------------ COPY SQL ARRAY TO TEMP ARRAY }

      (* DO NOT USE "COPY" METHOD *)

      for iCNT:=0 to TmpRows do
        for jCNT:=0 to 1 do
          Temp[iCNT, jCNT]:=sgAgeView.SqlColumns[iCNT, jCNT];
      { ------------------------------------------------------------------------------------------------------------------- UPDATE TITLES IN SQL COLUMN ARRAY }
      for iCNT:=0 to sgAgeView.ColCount - 1 do
        sgAgeView.SqlColumns[iCNT, 1]:=sgAgeView.Cells[iCNT, 0];
      { ---------------------------------------------------------------------------------------------------------------- RE-WRITE OTHER SQL COLUMNS FROM TEMP }
      sgAgeView.SqlColumns[ToIndex, 0]:=Temp[FromIndex, 0];
      { ---------------------------------------------------------------------------------------------------------------------------------- FROM RIGHT TO LEFT }
      if FromIndex > ToIndex then
        for iCNT:=ToIndex to (FromIndex - 1) do
          sgAgeView.SqlColumns[iCNT + 1, 0]:=Temp[iCNT, 0];
      { ---------------------------------------------------------------------------------------------------------------------------------- FROM LEFT TO RIGHT }
      if FromIndex < ToIndex then
        for iCNT:=(FromIndex + 1) to ToIndex do
          sgAgeView.SqlColumns[iCNT - 1, 0]:=Temp[iCNT, 0];
    except
      on E: Exception do
        MainForm.MsgCall(2, 'Unexpected error has occured. Description: ' + E.Message + ' Please contact IT support.')
    end;
  finally
    { SAVE CHANGES }
    sgAgeView.SaveLayout(Settings.ColumnWidthName, Settings.ColumnOrderName, Settings.ColumnNames, Settings.ColumnPrefix);
    { REMOVE }
    Temp:=nil;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- OPEN TRANSACTION WINDOW }
procedure TMainForm.sgAgeViewDblClick(Sender: TObject);
begin
  WndCall(ActionsForm, 0);
end;

{ --------------------------------------------------------------------------------------------------------------- ALLOW OR DISALLOW EDIT CELL IN ADDRESS BOOK }
procedure TMainForm.sgAddressBookDblClick(Sender: TObject);
begin
  { FIRST COLUMN ARE NOT EDITABLE }
  if (sgAddressBook.Col = 1) then Exit;
  sgAddressBook.Options:=sgAddressBook.Options + [goEditing];
end;

procedure TMainForm.sgAddressBookClick(Sender: TObject);
begin
  sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- LIST OF SENT INVOICES }
procedure TMainForm.sgInvoiceTrackerDblClick(Sender: TObject);
begin
  WndCall(InvoicesForm, 0);
end;

{ ---------------------------------------------------- ! SHOW NEGATIVE VALUES AND ROW SELECTION ! ----------------------------------------------------------- }

procedure TMainForm.sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);     //refactor!!!
var
  Col1:   integer;
  Col2:   integer;
  Col3:   integer;
  Col4:   integer;
  Col5:   integer;
  Col6:   integer;
  Col7:   integer;
  Col8:   integer;
  Col9:   integer;
  Col10:  integer;
  Col11:  integer;
begin

  (* CALL SG_DRAWSELECTED BEFORE SG_COLORVALUES *)

  { DRAW SELECTED ROW | SKIP HEADERS }
  MainForm.sgAgeView.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);

  { COLUMNS ORDER MAY BE CHANGED BY THE USER  }
  { FIND COLUMN NUMBERS FOR GIVEN COLUMN NAME }
  Col1 :=MainForm.sgAgeView.ReturnColumn('NOT DUE',         1, 1);
  Col2 :=MainForm.sgAgeView.ReturnColumn('1 - 7',           1, 1);
  Col3 :=MainForm.sgAgeView.ReturnColumn('8 - 30',          1, 1);
  Col4 :=MainForm.sgAgeView.ReturnColumn('31 - 60',         1, 1);
  Col5 :=MainForm.sgAgeView.ReturnColumn('61 - 90',         1, 1);
  Col6 :=MainForm.sgAgeView.ReturnColumn('91 - 120',        1, 1);
  Col7 :=MainForm.sgAgeView.ReturnColumn('120 - oo',        1, 1);
  Col8 :=MainForm.sgAgeView.ReturnColumn('OVERDUE',         1, 1);
  Col9 :=MainForm.sgAgeView.ReturnColumn('TOTAL',           1, 1);
  Col10:=MainForm.sgAgeView.ReturnColumn('CREDIT LIMIT',    1, 1);
  Col11:=MainForm.sgAgeView.ReturnColumn('EXCEEDED AMOUNT', 1, 1);

  { DRAW ONLY SELECTED COLUMNS }
  if ( (ACol = Col1) or (ACol = Col2) or (ACol = Col3) or (ACol = Col4) or (ACol = Col5) or (ACol = Col6) or (ACol = Col7) or (ACol = Col8) or (ACol = Col9) or (ACol = Col10) or (ACol = Col11) )
    and (ARow > 0) then
      begin
        MainForm.sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clBlack);
      end;

end;

procedure TMainForm.sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

  (* CALL SG_DRAWSELECTED BEFORE SG_COLORVALUES *)

  { DRAW SELECTED ROW | SKIP HEADERS }
  MainForm.sgOpenItems.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);

  { ONLY FOR COLUMNS 5, 6, 9, 10 THAT SHOWS NEGATIVE AMOUNTS AND COLUMN 34 FOR PAYMENT STATUS }
  if ( (ACol = 5) or (ACol = 6) or (ACol = 9) or (ACol = 10) or (ACol = 34) ) and (ARow > 0) then begin
    MainForm.sgOpenItems.ColorValues(ARow, ACol, Rect, clRed, clBlack);
  end;

end;

{ ------------------------------------------------------------ ! STRING GRID ROW SELECTION ! ---------------------------------------------------------------- }

procedure TMainForm.sgAddressBookSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if ACol = 1 then
  begin
    CanSelect:=False;
    sgAddressBook.Options:=sgAddressBook.Options - [goRangeSelect];
  end
    else
      begin
        CanSelect:=True;
        sgAddressBook.Options:=sgAddressBook.Options + [goRangeSelect];
      end;
end;

procedure TMainForm.sgAddressBookDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgAddressBook.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgInvoiceTrackerDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgInvoiceTracker.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgCoCodesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgCoCodes.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPaidInfoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgPaidInfo.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPmtTermsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgPmtTerms.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPersonDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgPerson.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgGroup3DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgGroup3.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgListSectionDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgListSection.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgListValueDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgListValue.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

{ ---------------------------------------------------- ! SETTING PANEL STRING GRID EVENTS !  ---------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------- LIST ALL KEYS WITH VALUES }
procedure TMainForm.sgListSectionSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  tsKEY:      TStringList;
  tsVAL:      TStringList;
  iCNT:       integer;
  junk:       string;
  clean:      string;
begin
  CanSelect:=True;
  { LIST KEYS AND VALUES }
  tsKEY:=TStringList.Create();
  tsVAL:=TStringList.Create();
  try
    Settings.TMIG.ReadSection(sgListSection.Cells[ACol, ARow], tsKEY);
    Settings.TMIG.ReadSectionValues(sgListSection.Cells[ACol, ARow], tsVAL);
    sgListValue.RowCount:=tsKEY.Count + 1;
    if tsKEY.Count = 0 then
    begin
      sgListValue.RowCount:=2;
      sgListValue.FixedRows:=1;
      sgListValue.Cells[0, 1]:='1';
      sgListValue.Cells[1, 1]:='';
      sgListValue.Cells[2, 1]:='';
    end else
    begin
      for iCNT:=1 to tsKEY.Count do
      begin
        sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);
        sgListValue.Cells[1, iCNT]:=tsKEY.Strings[iCNT - 1];
        junk:=tsKEY.Strings[iCNT - 1] + '=';
        clean:=StringReplace(tsVAL.Strings[iCNT - 1], junk, '', [rfReplaceAll]);
        sgListValue.Cells[2, iCNT]:=clean;
      end;
    end;
  finally
    tsKEY.Free;
    tsVAL.Free;
  end;
end;

{ ------------------------------------------------------------ ! KEYBOARD EVENTS ! -------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------- CLOSE WHEN HIT 'ALT + Y' }
procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { -------------------------------------------------------------------------------------------------------------------------- SWITCH OFF STANDARD 'ALT + F4' }
  if (Key=VK_F4) and (Shift=[ssALT]) then Key:=0;
  { ----------------------------------------------------------------------------------------------------------------- CLOSE APPLICATION IF USER HIT 'ALT + Y' }
  if (Key=89) and (Shift=[ssALT]) then
    if MsgCall(4, 'Are you sure you want to exit the application?') = IDOK then
    begin
      AllowClose:=True;
      Close;
    end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | COMPANY CODE 1..4 }
procedure TMainForm.COC1KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8]))) then Key:=#0;
end;

procedure TMainForm.COC2KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8]))) then Key:=#0;
end;

procedure TMainForm.COC3KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8]))) then Key:=#0;
end;

procedure TMainForm.COC4KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8]))) then Key:=#0;
end;

{ --------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | CURRENCY CODE 1..4 }
procedure TMainForm.CUR1KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['A'..'Z', #8]))) then Key:=#0;
end;

procedure TMainForm.CUR2KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['A'..'Z', #8]))) then Key:=#0;
end;

procedure TMainForm.CUR3KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['A'..'Z', #8]))) then Key:=#0;
end;

procedure TMainForm.CUR4KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['A'..'Z', #8]))) then Key:=#0;
end;

{ --------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | INTEREST RATE 1..4 }
procedure TMainForm.INT1KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8, ',', '.']))) then Key:=#0;
end;

procedure TMainForm.INT2KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8, ',', '.']))) then Key:=#0;
end;

procedure TMainForm.INT3KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8, ',', '.']))) then Key:=#0;
end;

procedure TMainForm.INT4KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['0'..'9', #8, ',', '.']))) then Key:=#0;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | AGENTS 1..4 }
procedure TMainForm.AGT1KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['O', 'N', 'F', #8]))) then Key:=#0;
end;

procedure TMainForm.AGT2KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['O', 'N', 'F', #8]))) then Key:=#0;
end;

procedure TMainForm.AGT3KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['O', 'N', 'F', #8]))) then Key:=#0;
end;

procedure TMainForm.AGT4KeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['O', 'N', 'F', #8]))) then Key:=#0;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | MAKE GROUP }
procedure TMainForm.EditGroupNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['A'..'Z', '0'..'9', '-', #8]))) then Key:=#0;
end;

{ -------------------------------------------------------------- ! COPY PASTE CUT ! ------------------------------------------------------------------------- }

                                                  (* ALLOW COPY "CTRL + C" ON ALL STRING GRIDS *)

procedure TMainForm.sgCoCodesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgCoCodes.CopyCutPaste(1);
end;

procedure TMainForm.sgPaidInfoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgPaidInfo.CopyCutPaste(1);
end;

procedure TMainForm.sgPmtTermsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgPmtTerms.CopyCutPaste(1);
end;

procedure TMainForm.sgPersonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgPerson.CopyCutPaste(1);
end;

procedure TMainForm.sgGroup3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgGroup3.CopyCutPaste(1);
end;

procedure TMainForm.sgInvoiceTrackerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgInvoiceTracker.CopyCutPaste(1);
end;

procedure TMainForm.sgOpenItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgOpenItems.CopyCutPaste(1);
end;

procedure TMainForm.sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgAgeView.CopyCutPaste(1);
end;

{ --------------------------------------------------------------------------------------------------------------------- PASTE, CUT, COPY TO/FROM ADDRESS BOOK }
procedure TMainForm.sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); //refactor!!!
var
  MSSQL:    TMSSQL;
  Value:    string;
begin

  { FIRST COLUMN ARE NOT EDITABLE }
  if (sgAddressBook.Col = 1) then Exit;

  { PASTE | CTRL + V }
  { COPY  | CTRL + C }
  { CUT   | CTRL + X }
  if (Key = 86) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(0);
  if (Key = 67) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(1);
  if (Key = 88) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(2);

  { ON DELETE }
  if Key = 46 then sgAddressBook.DelEsc(1, sgAddressBook.Col, sgAddressBook.Row);

  { ON ESCAPE }
  if Key = 27 then
  begin
    sgAddressBook.DelEsc(0, sgAddressBook.Col, sgAddressBook.Row);
    sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
  end;

  { UPDATE SQL DATABASE TABLE ON ENTER }
  if Key = 13 then
  begin
    { LP COLUMN CANNOT BE EMPTY }
    if not (sgAddressBook.Cells[0, sgAddressBook.Row] = '') then
    begin
      { CONNECT AND UPDATE }
      MSSQL:=TMSSQL.Create(Database.ADOConnect);
      try
        Value:=MSSQL.CleanStr(sgAddressBook.Cells[sgAddressBook.Col, sgAddressBook.Row], True);
        MSSQL.StrSQL:='UPDATE tbl_AddressBook SET '                                        +
                      sgAddressBook.Cells[sgAddressBook.Col, 0]                            +     //!!!
                      ' = '                                                                +
                      Value                                                                +
                      ' WHERE CUID = '                                                     +
                      MSSQL.CleanStr(sgAddressBook.Cells[2, sgAddressBook.Row], True);
        MSSQL.ExecSQL;
      finally
        MSSQL.Free;
      end;
    end;
    sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
  end;

  { ALLOW EDITING | F2 }
  if Key = VK_F2 then sgAddressBook.Options:=sgAddressBook.Options + [goEditing];

end;

{ -------------------------------------------------------------------------------------------------------------------------------------- UPDATE SECTION VALUE }
procedure TMainForm.sgListSectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  { PASTE | CTRL + V }
  { CUT   | CTRL + X }
  if Text50.Font.Style = [fsBold] then
  begin
    if (Key = 86) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(0);
    if (Key = 88) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(2);
  end;

  { COPY | CTRL + C }
  if (Key = 67) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(1);

  { DELETE }
  if (Key = 46) and (Text50.Font.Style = [fsBold]) then sgListSection.DelEsc(1, sgListSection.Col, sgListSection.Row);

  { ESCAPE }
  if Key = 27 then sgListSection.DelEsc(0, sgListSection.Col, sgListSection.Row);

end;

procedure TMainForm.sgListSectionKeyPress(Sender: TObject; var Key: Char);
begin
  { UPDATE IF <ENTER> IS PRESSED }
  if Key = #13 then sgListSection.Cells[1, sgListSection.Row]:=UpperCase(sgListSection.Cells[1, sgListSection.Row]);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ UPDATE VALUE KEY }
procedure TMainForm.sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  { PASTE | CTRL + V }
  { CUT   | CTRL + X }
  if Text50.Font.Style = [fsBold] then
  begin
    if (Key = 86) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(0);
    if (Key = 88) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(2);
  end;

  { COPY | CTRL + C }
  if (Key = 67) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(1);

  { DELETE }
  if (Key = 46) and (Text50.Font.Style = [fsBold]) then sgListValue.DelEsc(1, sgListValue.Col, sgListValue.Row);

  { ESCAPE }
  if Key = 27 then sgListValue.DelEsc(0, sgListValue.Col, sgListValue.Row);

end;

procedure TMainForm.sgListValueKeyPress(Sender: TObject; var Key: Char);
begin
  { FORCE CAPITAL CHARACTERS IF <ENTER> IS PRESSED }
  //if Key = #13 then sgListValue.Cells[1, sgListValue.Row]:=UpperCase(sgListValue.Cells[1, sgListValue.Row]); // OFF
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- CALL PASSWORD UNLOCK }
procedure TMainForm.Edit_PASSWORDKeyPress(Sender: TObject; var Key: Char);
begin
  { ON <ENTER> }
  if Key = #13 then btnUnlockClick(self);
end;

{ -------------------------------------------------------------- ! MOUSE EVENTS ! --------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SCROLL BARS }
{ AGE VIEW | WHEEL DOWN }
procedure TMainForm.sgAgeViewMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgAgeView.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ AGE VIEW | WHEEL UP }
procedure TMainForm.sgAgeViewMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgAgeView.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ OPEN ITEMS | WHEEL DOWN }
procedure TMainForm.sgOpenItemsMouseWheelDown(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgOpenItems.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ OPEN ITEMS | WHEEL UP }
procedure TMainForm.sgOpenItemsMouseWheelUp(Sender: TObject; Shift:     TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgOpenItems.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ ADDRESS BOOK | WHEEL DOWN }
procedure TMainForm.sgAddressBookMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgAddressBook.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ ADDRESS BOOK | WHEEL UP }
procedure TMainForm.sgAddressBookMouseWheelUp(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgAddressBook.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ INVOICE TRACKER | WHEEL DOWN }
procedure TMainForm.sgInvoiceTrackerMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgInvoiceTracker.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ INVOICE TRACKER | WHEEL UP }
procedure TMainForm.sgInvoiceTrackerMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgInvoiceTracker.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ SECTION LIST | WHEEL DOWN }
procedure TMainForm.sgListSectionMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgListSection.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ SECTION LIST | WHEEL UP }
procedure TMainForm.sgListSectionMouseWheelUp(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgListSection.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ VALUE LIST | WHEEL DOWN }
procedure TMainForm.sgListValueMouseWheelDown(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgListValue.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ VALUE LIST  | WHEEL UP }
procedure TMainForm.sgListValueMouseWheelUp(Sender: TObject; Shift:     TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgListValue.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ COMPANY CODES | WHEEL DOWN }
procedure TMainForm.sgCoCodesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgCoCodes.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ COMPANY CODES | WHEEL UP }
procedure TMainForm.sgCoCodesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgCoCodes.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ PAYMENT TERMS | WHEEL DOWN }
procedure TMainForm.sgPmtTermsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgPmtTerms.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ PAYMENT TERMS | WHEEL UP }
procedure TMainForm.sgPmtTermsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgPmtTerms.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ PAID INFO | WHEEL DOWN }
procedure TMainForm.sgPaidInfoMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgPaidInfo.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ PAID INFO | WHEEL UP }
procedure TMainForm.sgPaidInfoMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgPaidInfo.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ GROUP3 | WHEEL DOWN }
procedure TMainForm.sgGroup3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgGroup3.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ GROUP3 | WHEEL UP }
procedure TMainForm.sgGroup3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgGroup3.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ PERSON | WHEEL DOWN }
procedure TMainForm.sgPersonMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgPerson.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ PERSON | WHEEL UP }
procedure TMainForm.sgPersonMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgPerson.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | RELOAD BUTTON }
procedure TMainForm.btnReloadMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  if Database.AccessLevel = 'AD' then btnReload.Cursor:=crHandPoint
    else
      btnReload.Cursor:=crNo;
  Text54L1.Font.Color:=Settings.FONCOLOR;
  Text54L2.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnReloadMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnReload.Cursor:=crDefault;
  Text54L1.Font.Color:=clBlack;
  Text54L2.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ MAKE GROUP }
procedure TMainForm.btnMakeGroupMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  if Database.AccessLevel = 'AD' then btnMakeGroup.Cursor:=crHandPoint
    else
      btnMakeGroup.Cursor:=crNo;
  Text83L1.Font.Color:=Settings.FONCOLOR;
  Text83L2.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnMakeGroupMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnMakeGroup.Cursor:=crDefault;
  Text83L1.Font.Color:=clBlack;
  Text83L2.Font.Color:=clBlack;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- OPEN AB }
procedure TMainForm.btnOpenABMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  btnOpenAB.Cursor:=crHandPoint;
  Text64.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnOpenABMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnOpenAB.Cursor:=crDefault;
  Text64.Font.Color:=clBlack;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- SAVE AB }
procedure TMainForm.btnSaveMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  btnSave.Cursor:=crHandPoint;
  Text66.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnSaveMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnSave.Cursor:=crDefault;
  Text66.Font.Color:=clBlack;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------------- CLOSE AB }
procedure TMainForm.btnCloseMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  btnClose.Cursor:=crHandPoint;
  Text67.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnCloseMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnClose.Cursor:=crDefault;
  Text67.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- IMPORT AB }
procedure TMainForm.btnImportMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  btnImport.Cursor:=crHandPoint;
  Text68.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnImportMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnImport.Cursor:=crDefault;
  Text68.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- EXPORT AB }
procedure TMainForm.btnExportMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  btnExport.Cursor:=crHandPoint;
  Text69.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.btnExportMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  btnExport.Cursor:=crDefault;
  Text69.Font.Color:=clBlack;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ADD KEY }
procedure TMainForm.imgKeyAddMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgKeyAdd.Cursor:=crHandPoint;
  Text41.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.imgKeyAddMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgKeyAdd.Cursor:=crDefault;
  Text41.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ REMOVE KEY }
procedure TMainForm.imgKeyRemoveMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgKeyRemove.Cursor:=crHandPoint;
  Text42.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.imgKeyRemoveMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgKeyRemove.Cursor:=crDefault;
  Text42.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------ SAVE ALL VALUES, KEYS AND SECTIONS }
procedure TMainForm.imgUpdateValuesMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgUpdateValues.Cursor:=crHandPoint;
  Text43.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.imgUpdateValuesMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgUpdateValues.Cursor:=crDefault;
  Text43.Font.Color:=clBlack;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ADD SECTION }
procedure TMainForm.imgSectionAddMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgSectionAdd.Cursor:=crHandPoint;
  Text48.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.imgSectionAddMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgSectionAdd.Cursor:=crDefault;
  Text48.Font.Color:=clBlack;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- DELETE SECTION }
procedure TMainForm.imgSectionRemoveMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgSectionRemove.Cursor:=crHandPoint;
  Text49.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.imgSectionRemoveMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgSectionRemove.Cursor:=crDefault;
  Text49.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ ALLOW EDIT }
procedure TMainForm.imgAllowEditMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgAllowEdit.Cursor:=crHandPoint;
  Text50.Font.Color:=Settings.FONCOLOR;
end;

procedure TMainForm.imgAllowEditMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgAllowEdit.Cursor:=crDefault;
  Text50.Font.Color:=clBlack;
end;

{ ---------------------------------------------------------------------------------------------------------------------- SECTION LIST | SWITCH OFF/ON BUTTONS }
procedure TMainForm.sgListSectionClick(Sender: TObject);
begin
  { IMAGES }
  imgSectionAdd.Enabled   :=True;
  imgSectionRemove.Enabled:=True;
  imgKeyAdd.Enabled       :=False;
  imgKeyRemove.Enabled    :=False;
  imgUpdateValues.Enabled :=False;
  { CAPTIONS }
  Text41.Enabled:=False;
  Text42.Enabled:=False;
  Text43.Enabled:=False;
  Text48.Enabled:=True;
  Text49.Enabled:=True;
end;

{ ------------------------------------------------------------------------------------------------------------------------ VALUE LIST | SWITCH OFF/ON BUTTONS }
procedure TMainForm.sgListValueClick(Sender: TObject);
begin
  { IMAGES }
  imgSectionAdd.Enabled   :=False;
  imgSectionRemove.Enabled:=False;
  imgKeyAdd.Enabled       :=True;
  imgKeyRemove.Enabled    :=True;
  imgUpdateValues.Enabled :=True;
  { CAPTIONS }
  Text41.Enabled:=True;
  Text42.Enabled:=True;
  Text43.Enabled:=True;
  Text48.Enabled:=False;
  Text49.Enabled:=False;
end;

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------ RELOAD EVENT LOG IN THE MEMO }
procedure TMainForm.EventReloadClick(Sender: TObject);
begin
  try
    try
      EventLog.Lines.LoadFromFile(Settings.AppDir + Settings.LogFile);
    except
      EventLog.Lines.Text:='Exception catch message: cannot load event log file. The file is locked by another process. Please try again later.';
    end;
  finally
    EventLog.SelStart:=Length(EventLog.Text);
    EventLog.SelLength:=0;
    SendMessage(EventLog.Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- OPEN SELECTED GROUP }
procedure TMainForm.btnOpenGroupClick(Sender: TObject);
begin
  if GroupListDates.Text = '' then
  begin
    MsgCall(2, 'Cannot upload this group, please choose another one.');
    Exit;
  end;
  { CALL READ AGE VIEW IF ACCOMPANIED PROCESSES ARE FREED }
  if (Worker.ActiveThreads[3] = True) or
     (Worker.ActiveThreads[4] = True) or
     (Worker.ActiveThreads[7] = True) then
  begin
    MsgCall(2, 'Open items are currently being processed by the Unity. Please wait until the process is finished and try again.');
  end
    else
      TTReadAgeView.Create('1');
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- FORCE RELOAD }
procedure TMainForm.btnReloadClick(Sender: TObject);
begin
  { ------------------------------------------------------------------------------------------------- CHECK USER PERMISSION | ONLY ADMINISTRATORS ARE ALLOWED }
  StatBar_TXT1.Caption :='Checking...';
  if Database.AccessLevel = 'AD' then
  begin
    { START THREAD WITH NO PARAMETERS PASSED TO AN OBJECT }
    TTReadOpenItems.Create('0');
  end else
  begin
    StatBar_TXT1.Caption:='Insufficient UAC level.';
    LogText(Settings.AppDir + Settings.LogFile, '[Open Items]: User have no R/W access, process halted.');
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- ACTIONS | MAKE AGE }
procedure TMainForm.btnMakeGroupClick(Sender: TObject);
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  cbDump.Checked:=False;
  if sgOpenItems.RowCount < 2 then Exit;
  { --------------------------------------------------------------------------------------------- CHECK USER PERMISSION | ONLY ADMINISTRATORS ARE NOT ALLOWED }
  if Database.AccessLevel = 'AD' then
    if PanelGroupName.Visible then
    begin
      PanelGroupName.Visible:=False;
      ReloadCover.Visible:=False;
    end else
    begin
      PanelGroupName.Visible:=True;
      ReloadCover.Visible:=True;
      { SUGGEST THE SAME GROUP NAME }
      EditGroupName.Text:=GroupListBox.Text;
    end else
    begin
      StatBar_TXT1.Caption:='Insufficient UAC level.';
      LogText(Settings.AppDir + Settings.LogFile, '[Make Group]: User have no R/W access, process halted.');
    end;
end;

procedure TMainForm.btnMakeGroupAgeClick(Sender: TObject);
begin
  if EditGroupName.Text <> '' then
  begin
    { START THREAD WITH NO PARAMETERS PASSED TO AN OBJECT }
    TTMakeAgeView.Create(False);
    PanelGroupName.Visible:=False;
    ReloadCover.Visible:=False;
  end
    else
      MsgCall(2, 'Please enter group name and try again.' + #13#10 + 'If you will use existing one, then it will be overwritten.');
end;

{ --------------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | CLOSE }
procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  if MsgCall(5, 'Are you sure you want to close address book?') = IDYES then
  begin
    { CLEAR ALL }
    sgAddressBook.ClearAll(2, 1, 1, True);
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | SAVE CURRENTLY OPENED }
procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  { SET 0 = SAVE BUTTON }
  TTAddressBook.Create('0', Settings.WinUserName);
end;

{ -------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | OPEN FROM DB }
procedure TMainForm.btnOpenABClick(Sender: TObject);
begin
  { SET 1 = OPEN BUTTON FOR ALL REGISTERED CUSTOMERS }
  TTAddressBook.Create('1', '');
end;

{ --------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | IMPORT DATA }
procedure TMainForm.btnImportClick(Sender: TObject);
begin
  { SET 2 = IMPORT BUTTON }
  TTAddressBook.Create('2', '');
end;

{ ---------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | EXPORT ALL }
procedure TMainForm.btnExportClick(Sender: TObject);
begin
  { SET 3 = EXPORT BUTTON }
  TTAddressBook.Create('3', '');
end;

{ -------------------------------------------------------------------------------------------------------------------------------- SECTION LIST | ADD SECTION }
procedure TMainForm.imgSectionAddClick(Sender: TObject);
var
  iCNT:  integer;
begin
  { ADD ROW AT THE END OF THE LIST }
  sgListSection.RowCount:=sgListSection.RowCount + 1;
  sgListSection.Cells[1, sgListSection.RowCount]:= '';
  for iCNT:= 1 to sgListSection.RowCount do sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);
end;

{ ----------------------------------------------------------------------------------------------------------------------------- SECTION LIST | DELETE SECTION }
procedure TMainForm.imgSectionRemoveClick(Sender: TObject);
var
  iCNT:  integer;
begin

  { ASK USER IF HE IS SURE BECAUSE ONCE DONE CANNOT BE UNDONE }
  if MsgCall(5, 'Are you sure you want to delete this section? It cannot be undone.') = IDNO then exit;
  if sgListSection.RowCount = 1 then exit;

  { DELETE SECTION FROM TMIg }
  Settings.TMIG.EraseSection(sgListSection.Cells[1, sgListSection.Row]);

  { DELETE SELECTED ROW FROM STRING GRID }
  sgListSection.DeleteRowFrom(1, 1);

  { RE-NUMBER }
  for iCNT := 1 to sgListSection.RowCount do
    sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);

end;

{ --------------------------------------------------------------------------------------------------------------------------------- SECTION LIST | ALLOW EDIT }
procedure TMainForm.imgAllowEditClick(Sender: TObject);
begin
  if Text50.Font.Style = [fsBold] then
  begin
    sgListSection.Options:=sgListSection.Options - [goEditing];
    sgListValue.Options  :=sgListValue.Options   - [goEditing];
    Text50.Font.Style    :=[];
  end else
  begin
    sgListSection.Options:=sgListSection.Options + [goEditing];
    sgListValue.Options  :=sgListValue.Options   + [goEditing];
    Text50.Font.Style    :=[fsBold];
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- VALUES LIST | ADD KEY }
procedure TMainForm.imgKeyAddClick(Sender: TObject);
var
  iCNT:  integer;
begin

  { ADD ROW AT THE END OF LIST }
  iCNT:=sgListValue.RowCount + 1;
  sgListValue.RowCount:=iCNT;

  { MAKE SURE WE ADD EMPTY ROW }
  sgListValue.Cells[1, iCNT - 1]:='';
  sgListValue.Cells[2, iCNT - 1]:='';
  for iCNT:= 1 to sgListValue.RowCount do sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;

{ ---------------------------------------------------------------------------------------------------------------------------------- VALUES LIST | DELETE KEY }
procedure TMainForm.imgKeyRemoveClick(Sender: TObject);
var
  iCNT:  integer;
begin

  { ASK USER IF HE IS SURE BECAUSE ONCE DONE CANNOT BE UNDONE }
  if MsgCall(5, 'Are you sure you want to delete this key? It cannot be undone.') = IDNO then Exit;

  { CHECK FOR LAST ROW }
  if sgListValue.RowCount = 1 then exit;

  { DELETE SECTION FROM TMIg }
  Settings.TMIG.DeleteKey(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, sgListValue.Row]);

  { DELETE SELECTED ROW FROM STRING GRID }
  sgListValue.DeleteRowFrom(1, 1);

  { RE-NUMBER }
  for iCNT:= 1 to sgListValue.RowCount do sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;

{ ------------------------------------------------------------------------------------------------------------------------ VALUES, KEYS & SECTIONS | SAVE ALL }
procedure TMainForm.imgUpdateValuesClick(Sender: TObject);
var
  iCNT:   integer;
begin

  { ASK USER IF HE IS SURE BECAUSE ONCE DONE CANNOT BE UNDONE }
  if MsgCall(5, 'Are you sure you want to save all the changes? It cannot be undone.') = IDNO then exit;

  { CHECK IF THERE IS NO EMPTY KEYS }
  for iCNT:= 1 to (sgListValue.RowCount - 1) do
    if sgListValue.Cells[1, iCNT] = '' then
    begin
      MsgCall(2, 'Cannot save. At least one key has no label.');
      Exit;
    end;

  { WRITE ALL KEYS AND VALUES INTO TMEMINI }
  for iCNT:= 1 to (sgListValue.RowCount - 1) do
    Settings.TMIG.WriteString(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, iCNT], sgListValue.Cells[2, iCNT]);

  MsgCall(1, 'All Keys and its values has been saved successfully.');

end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SAVE NEW PASSWORD }
procedure TMainForm.btnPassUpdateClick(Sender: TObject);
begin
  if (Settings.TMIG.ReadString(Settings.Password, 'VALUE', '') = Edit_CurrPassWd.Text) then
  begin
    if Edit_NewPassWd.Text=Edit_ConfPassWd.text then
    begin
      Settings.TMIG.WriteString(Settings.Password,'VALUE',Edit_NewPassWd.Text);
      MsgCall(1, 'New password has been saved.');
      btnPassUpdate.Enabled:=False;
      Edit_CurrPassWd.Enabled:=False;
      Edit_NewPassWd.Enabled:=False;
      Edit_ConfPassWd.Enabled:=False;
      Edit_PASSWORD.Text:='';
      Edit_CurrPassWd.Text:='';
      Edit_NewPassWd.Text:='';
      Edit_ConfPassWd.Text:='';
    end
      else
        MsgCall(2, 'New password and confirmation does not match, please re-type it and try again.');
  end
    else
      MsgCall(2, 'Current password is incorrect, please re-type it and try again.');
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- UNLOCK }
procedure TMainForm.btnUnlockClick(Sender: TObject);
var
  tStrings:   TStringList;
  iCNT:       integer;
  inner:      integer;
begin
  { LOCK / UNLOCK }
  if btnUnlock.Caption = 'Unlock' then
  begin
    { IF PASSWORD IS OK, THEN UNLOCK AND LOAD CONFIGURATION SCRIPT FOR EDITING }
    if (Settings.TMIG.ReadString(Settings.Password, 'VALUE', '') <> '') and
       (Settings.TMIG.ReadString(Settings.Password, 'VALUE', '') = Edit_PASSWORD.Text) then
    begin
      sgListSection.Cols[0].Text:='Lp';
      sgListSection.Cols[1].Text:='Sections';
      sgListValue.Cols[0].Text  :='Lp';
      sgListValue.Cols[1].Text  :='Key';
      sgListValue.Cols[2].Text  :='Value';
      { CREDENTIALS }
      btnPassUpdate.Enabled  :=True;
      Edit_CurrPassWd.Enabled:=True;
      Edit_NewPassWd.Enabled :=True;
      Edit_ConfPassWd.Enabled:=True;
      { STRING GRIDS }
      sgListSection.Enabled:=True;
      sgListValue.Enabled:=True;
      sgListSectionClick(self);
      sgListSection.Row:=1;
      sgListValue.Row:=1;
      { TRANSPARENCY OFF }
      imgOFF.Visible:=False;
      { POPULATE STRING GRID }
      tStrings:=TStringList.Create();
      try
        { READ ALL }
        Settings.TMIG.ReadSections(tStrings);
        { LIST ALL SECTIONS EXCEPT 'PASSWD' SECTION }
        sgListSection.RowCount:=tStrings.Count;
        inner:=1;
        for iCNT:=0 to tStrings.Count - 1 do
        begin
          if tStrings.Strings[iCNT] <> Settings.Password then
          begin
            sgListSection.Cells[0, inner]:=IntToStr(inner);
            sgListSection.Cells[1, inner]:=tStrings.Strings[iCNT];
            inc(inner);
          end;
        end;
      finally
        tStrings.Free;
        btnUnlock.Caption:='Lock';
        sgListValue.RowCount:=2;
      end;
    end;

    { STOP IF PASSWORD IS INVALID }
    if (Settings.TMIG.ReadString(Settings.Password, 'VALUE', '') <> '') and
       (Settings.TMIG.ReadString(Settings.Password, 'VALUE', '') <> Edit_PASSWORD.Text) then
    begin
      MsgCall(2, 'Incorrect password, please re-type it and try again.');
    end;

    { SETUP NEW PASSWORD }
    if (Settings.TMIG.ReadString(Settings.Password, 'VALUE', '') = '') then
    begin
      Edit_CurrPassWd.Enabled:=True;
      Edit_NewPassWd.Enabled :=True;
      Edit_ConfPassWd.Enabled:=True;
      MsgCall(2, 'Please provide with new password.');
    end;
    Edit_PASSWORD.Text:='';
    Edit_PASSWORD.SetFocus;
  end else
  { LOCK TABSHEET }
  begin
    MainForm.LockSettingsPanel;
  end;
end;

end.
