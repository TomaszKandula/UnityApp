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
unit Main;  (* !!! CHANGE ALL 'VARTOSTR' TO 'OLEGETSTR' BEFORE RELEASE !!! *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, Grids, ExtCtrls, StdCtrls, CheckLst, Buttons, PNGimage,
  DBGrids, AppEvnts, ShellAPI, INIFiles, StrUtils, ValEdit, DateUtils, Clipbrd, DB, ADODB, ActiveX, CDO_TLB, Diagnostics, GIFImg, Math, Wininet, ComObj,
  OleCtrls, SHDocVw, blcksock, smtpsend { MODIFIED FROM ORIGINAL }, pop3send, ssl_openssl, synautil, synacode, mimemess { MODIFIED FROM ORIGINAL };

{ REFERENCE TO M.DIM. ARRAY }
type
  TLists = array of array of string;

{ 'TSTRINGGRID' REFERENCE FOR DELETE OPTION }
type
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
    function  ToExcel(ASheetName, AFileName: string): Boolean;
    procedure Freeze(PaintWnd: boolean);
    function  ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
    function  ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
  public
    var OpenThdId:  integer;
    var SqlColumns: TLists;
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
    Text53: TLabel;
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
    DetailsGrid: TStringGrid;
    btnLoadAgeView: TSpeedButton;
    EditGroupID: TLabeledEdit;
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
    procedure DetailsGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DetailsGridKeyPress(Sender: TObject; var Key: Char);
    procedure DetailsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure DetailsGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnLoadAgeViewClick(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);

    { ------------------------------------------------------------- ! HELPERS ! ----------------------------------------------------------------------------- }

    { VARIABLES }

  private
    var PAllowClose         :  boolean;
    var PStartTime          :  TTime;
    var PAccessLevel        :  string;
    var PAccessMode         :  string;
    var POpenItemsUpdate    :  string;
 public
    var FUserName           :  string;
    var FEventLogPath       :  string;
    var FDbConnect          :  TADOConnection;
    var FGroupList          :  TLists;
    var GroupIdSel          :  string;
    var GroupNmSel          :  string;
    var AgeDateSel          :  string;
    var OSAmount            :  double;

    { PROPERTIES }

    property   AccessLevel    : string  read PAccessLevel     write PAccessLevel;
    property   AccessMode     : string  read PAccessMode      write PAccessMode;
    property   OpenItemsUpdate: string  read POpenItemsUpdate write POpenItemsUpdate;

    { HEPER METHODS }

    procedure  DebugMsg(const Msg: String);
    procedure  ExecMessage(IsPostType: boolean; WM_CONST: integer; YOUR_INT: integer; YOUR_TEXT: string);
    function   OleGetStr(RecordsetField: variant): string;
    function   FindKey(INI: TMemIniFile; OpenedSection: string; KeyPosition: integer): string;
    function   WndCall(WinForm: TForm; Mode: integer): integer;
    function   MsgCall(WndType: integer; WndText: string): integer;
    procedure  LockSettingsPanel;
    function   ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
    procedure  SwitchTimers(state: integer);

  protected

    { WINDOWS MESSAGES }

    procedure  WndProc(var msg: Messages.TMessage); override;

  end;

{ ---------------------------------------------------------- ! POINTERS TO DLL IMPORTS ! -------------------------------------------------------------------- }

{ IF LIBRARY IS WRITTEN IN DELPHI, THEN DELPHI TYPES CAN BE USED AS USUAL, }
{ HOWEVER, IF LIBRARY IS WRITTEN IN 'C' OR ANY OTHER LANGUAGE, THEN        }
{ PLEASE USE PLAIN 'C' LANGUAGE TYPES ONLY, SO INSTEAD OF PASCAL 'STRING'  }
{ TYPE, PLEASE USE 'PCHAR' TYPE, ETC., ALSO, IN CASE OF C# LANGUAGE,       }
{ PLEASE REFER TO MANUAL ON 'MAKING C# DLL LIBRARY FOR DELPHI USAGE'       }

TLogText              = procedure(filename: string; text: string); stdcall;
TMergeSort            = procedure(Grid: TStringgrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall;
TGetOSVer             = function(mode: integer): string; stdcall;
TGetCurrentUserSid    = function: string stdcall;
TGetBuildInfoAsString = function: string stdcall;

const Assembly = 'Unitylib.dll';

function  GetCurrentUserSid: string; stdcall; external Assembly;
function  GetOSVer(mode: integer): string; stdcall; external Assembly;
function  GetBuildInfoAsString: string; stdcall; external Assembly;
procedure LogText(filename: string; text: string); stdcall; external Assembly;
procedure MergeSort(grid: TStringgrid; var vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall; external Assembly;

{$I Common.inc}

{ ------------------------------------------------------------- ! MAIN FORM REFERENCE ! --------------------------------------------------------------------- }
var
  MainForm :  TMainForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Filter, Tracker, Invoices, Actions, Calendar, About, Search, Worker, Model, SQL, Settings, Database, UAC, AgeView, Transactions;

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
//var
//  DailyComment:  TDaily;
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

      if Msg.LParam > 0 then
      begin
(*
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
*)
      end;

  end;
  { --------------------------------------------------------------------------------------------------------------- CLOSE UNITY WHEN WINDOWS IS SHUTTING DOWN }

  (* *********************************************************************************************************************************************************

  WARNING!
  ========

  IF WE USE "ON CLOSE QUERY" AND PREVENT USER/WINDOWS FROM CLOSING APPLICATION BY CLICKING "X" (REQUIRE USER DECISION TO CLOSE THE PROGRAM, ETC.),
  THEN WE SHOULD USE GLOBAL FLAG SET BY DEFAULT TO FALSE AND LISTENING TO WINDOWS MESSAGES. IF WINDOWS PASS "QUERY END SESSION" OR "END SESSION",
  THEN WE SHOULD SET THIS FLAG TO TRUE AND ALLOW TO CLOSE WHEN METHOD "ON CLOSE QUERY" IS CALLED. OTHERWISE, APPLICATION WILL NOT BE TO CLOSED (AWAITING
  USER RESPOND, ETC.) AND WINDOWS WILL DISPLAY NOTIFICATION THAT APPLICATION PREVENT WINDOWS FROM SHUTTING DOWN.

  ********************************************************************************************************************************************************** *)

  (* WINDOWS' QUERY FOR SHUTDOWN *)

  if Msg.Msg = WM_QUERYENDSESSION then
  begin
    LogText(FEventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' (WM_QUERYENDSESSION). Windows is going to be shut down. Closing ' + APPNAME + '...');
    PAllowClose:=True;
    Msg.Result:=1;
  end;

  (* WINDOWS IS SHUTTING DOWN *)

  if Msg.Msg = WM_ENDSESSION then
  begin
    LogText(FEventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' (WM_ENDSESSION). Windows is shutting down...');
    PAllowClose:=True;
  end;

end;

{ ----------------------------------------------------------------------------------------------------------------------------- WRAPPER FOR SEND/POST MESSAGE }
procedure TMainForm.ExecMessage(IsPostType: boolean; WM_CONST: Integer; YOUR_INT: Integer; YOUR_TEXT: string);
begin
  if IsPostType     then PostMessage(MainForm.Handle, WM_CONST, YOUR_INT, LPARAM(PCHAR(YOUR_TEXT)));
  if not IsPostType then SendMessage(MainForm.Handle, WM_CONST, YOUR_INT, LPARAM(PCHAR(YOUR_TEXT)));
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
  if mode = adPaste then
  begin
    GRect:=Selection;
    L    :=GRect.Left;
    R    :=GRect.Top;
    S    :=ClipBoard.AsText;
    R    :=R - 1;
    { GO 'BREAK_LINE' BY 'BREAK_LINE' }
    while Pos(CR, S) > 0 do
    begin
      R :=R + 1;
      C :=L - 1;
      CS:=Copy(S, 1, Pos(CR, S));
      while Pos(TAB, CS) > 0 do
      begin
        C:=C + 1;
        if (C <= ColCount - 1) and (R <= RowCount - 1) then Cells[C, R]:=Copy(CS, 1, Pos(TAB, CS) - 1);
        F:=Copy(CS, 1, Pos(TAB, CS) - 1);
        Delete(CS,  1, Pos(TAB, CS));
      end;
      if (C <= ColCount - 1) and (R <= RowCount - 1) then Cells[C + 1, R]:=Copy(CS, 1, Pos(CR, CS) - 1);
      Delete(S, 1,Pos(CR, S));
      if Copy(S, 1, 1) = LF then Delete(S, 1, 1);
    end;
  end;
  { ----------------------------------------------------------------------------------------------------------------------- COPY OR CUT DATA FROM STRING GRID }
  if (mode = adCopy) or (mode = adCut) then
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
          if mode = adCut then Cells[Col, Row]:='';  { CUT DATA FROM STRING GRID }
          if Col < Sel.Right then TxtFromSel:=TxtFromSel + TAB;
        end;
        if Row < Sel.Bottom then TxtFromSel:=TxtFromSel + CRLF;
      end;
      inc(RowNum);
    end;
    ClipBoard.AsText:=TxtFromSel + CRLF;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------- DELETE AND ESCAPE BEHAVIOUR FOR STRING GRID }
procedure TStringGrid.DelEsc(mode: integer; pCol, pRow: integer);
begin
  if mode = adESC then EditorMode:=False;
  if mode = adDEL then Cells[pCol, pRow]:='';
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
    Canvas.Font.Color :=FontColorSel;
    Canvas.Brush.Color:=BrushColorSel
  end else begin
    Canvas.Font.Color :=FontColor;
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
  AppSettings:  TSettings;
  iCNT:         integer;
begin
  AppSettings:=TSettings.Create;
  try
    { COLUMN WIDTH }
    for iCNT:=0 to Self.ColCount - 1 do
    begin
      if iCNT = 0 then AppSettings.TMIP.WriteInteger(ColWidthName, ColPrefix + IntToStr(iCNT), 10);
      if iCNT > 0 then AppSettings.TMIP.WriteInteger(ColWidthName, ColPrefix + IntToStr(iCNT), Self.ColWidths[iCNT]);
    end;
    { SQL COLUMN NAME }
    for iCNT:=0 to Self.ColCount - 1 do AppSettings.TMIP.WriteString(ColOrderName, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 0]);
    { COLUMN TITLE }
    for iCNT:=0 to Self.ColCount - 1 do
    begin
      if iCNT = 0 then AppSettings.TMIP.WriteString(ColNames, ColPrefix + IntToStr(iCNT), '');
      if iCNT > 0 then AppSettings.TMIP.WriteString(ColNames, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 1]);
    end;
  finally
    AppSettings.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- LAYOUT | LOAD }
function TStringGrid.LoadLayout(var StrCol: string; ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string): boolean;
var
  AppSettings:  TSettings;
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
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIP.ReadSection(ColWidthName, ColWidthSec);
    AppSettings.TMIP.ReadSection(ColOrderName, ColOrderSec);
    AppSettings.TMIP.ReadSection(ColNames, ColNamesSec);
    if (ColWidthSec.Count = ColOrderSec.Count) and (ColWidthSec.Count = ColNamesSec.Count) then
    begin
      Self.ColCount:=ColWidthSec.Count;
      Result:=True;
      SetLength(Self.SqlColumns, Self.ColCount, 2);
      for iCNT:=0 to Self.ColCount - 1 do
      begin
        { SKIP FIRST COLUMN AS IT HOLDS EMPTY COLUMN }
        if iCNT > 0 then
        begin
          if iCNT < (Self.ColCount - 1) then
            StrCol:=StrCol + AppSettings.TMIP.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ','
              else
                StrCol:=StrCol + AppSettings.TMIP.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ' ';

          { STORE SQL "COLUMN NAMES" AND "USER FRIENDLY NAMES" INTO HELPER ARRAY }
          Self.SqlColumns[iCNT, 0]:=AppSettings.TMIP.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '');
          Self.SqlColumns[iCNT, 1]:=AppSettings.TMIP.ReadString(ColNames, ColPrefix + IntToStr(iCNT), '');

          { DISPLAY "USER FRIENDLY" COLUMN NAME }
          Self.Cells[iCNT, 0]:=AppSettings.TMIP.ReadString(ColNames, ColPrefix + IntToStr(iCNT), '');
        end;
        { ASSIGN SAVED WIDTH }
        Self.ColWidths[iCNT]:=AppSettings.TMIP.ReadInteger(ColWidthName, ColPrefix + IntToStr(iCNT), 100);
      end;
    end;
  finally
    AppSettings.Free;
    ColWidthSec.Free;
    ColOrderSec.Free;
    ColNamesSec.Free;
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

function TStringGrid.ToExcel(ASheetName: string; AFileName: string): boolean;
var
  Col:        integer;
  Row:        integer;
  RowOffset:  integer;     (* OFFSET CANNOT BE < 1 *)
  ColOffset:  integer;     (* OFFSET CANNOT BE < 1 *)
  XLApp:      OLEVariant;
  Sheet:      OLEVariant;
  DataTables: TDataTables;
begin
  Result:=False;
  DataTables:=TDataTables.Create(MainForm.FDbConnect);
  try
    { EXECUTE STORED PROCEDURE }
    DataTables.StrSQL:=EXECUTE + AgeViewExport + SPACE +
                       QuotedStr(MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 0]) + COMMA +
                       QuotedStr(MainForm.GroupListDates.Text);
    { QUERIED DATA TO SELF AND RELEASE }
    DataTables.SqlToGrid(Self, DataTables.ExecSQL, False, True);
  finally
    DataTables.Free;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------ INITIATE EXCEL APPLICATION }
  try
    XLApp:=CreateOleObject('Excel.Application');
    try
      XLApp.Visible:=False;
      XLApp.Caption:='Unity For Debt Management - Data Export';
      XLApp.DisplayAlerts:=False;
      XLApp.Workbooks.Add(xlWBatWorkSheet);
      Sheet:=XLApp.Workbooks[1].WorkSheets[1]; (* WARNING! OLDER VERSION OF CODE INSIGHT MAY SHOW FALSE ERROR, IGNORE IT *)
      Sheet.Name:=ASheetName;
      ColOffset:=1;
      RowOffset:=1;
      { -------------------------------------------------------------------------------------------------------------------------- STRING GRID TO EXCEL SHEET }
      for Col:=0 to Self.ColCount - 1 do
        for Row:=0 to Self.RowCount - 1 do
          { WE OMITT FIRST STRING GRID COLUMN }
          Sheet.Cells[Row + RowOffset, Col + ColOffset]:=Self.Cells[Col + 1, Row];
      { SIMPLE FORMATTING }
      for Col:=0 to Self.ColCount - 1 do Sheet.Columns[Col + ColOffset].ColumnWidth:=15;
      for Row:=0 to Self.RowCount - 1 do Sheet.Rows[Row + RowOffset].RowHeight:=15;
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
          LogText(MainForm.FEventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data has been successfully transferred to Excel.');
          SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PCHAR('The data has been successfully transferred to Excel.')));
        end;
      end;
    end;
  { --------------------------------------------------------------------------------------------------------------------------------- ON INITIALIZATION ERROR }
  except
    on E: Exception do
      begin
        if E.Message = xlWARN_MESSAGE then
        { EXCEL NOT FOUND }
        begin
          LogText(MainForm.FEventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data cannot be exported because Excel cannot be found.');
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported because Excel cannot be found.')));
        end else
        { GENERIC MESSAGE }
        begin
          LogText(MainForm.FEventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data cannot be exported, error message thrown: ' + E.Message + '.');
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported. Please contact IT support.')));
        end;
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

{ --------------------------------------------------------------------------------------------------------------------------------------------- IMPORT TO CSV }
function TStringGrid.ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
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
  Result :=False;
  IsError:=False;
  Count  :=0;
  { ----------------------------------------------------------------------------------------------------------------------------- GET THE FILE PATH AND PARSE }
  if DialogBox.Execute = True then
  begin
    MainForm.ExecMessage(True, WM_GETINFO, 10, stImportCSV);
    fPath  :=DialogBox.FileName;
    Data   :=TStringList.Create;
    Transit:=TStringList.Create;
    try
      { LOAD DATA }
      Data.LoadFromFile(fPath);
      { COUNT ALL COLUMNS }
      for iCNT:=0 to Length(Data[0]) do if copy(Data[0], iCNT, 1) = Delimiter then inc(Count);
      { COUNT ALL ROWS & SETUP OFFSET }
      Self.RowCount:=Data.Count + 1;
      { SETUP TRANSIT THAT WILL HOLD SPLIT LINE }
      Transit.StrictDelimiter:=True;
      Transit.Delimiter:=Delimiter[1];
      { ITERATE THROUGH ALL ROWS }
      try
        for iCNT:= 0 to Data.Count - 1 do
        begin
          { SPLIT STRING USING GIVEN DELIMITER }
          Transit.DelimitedText:=Data[iCNT];
          for jCNT:=1 to Count do Self.Cells[jCNT, iCNT + 1]:=Transit[jCNT - 1];
          Self.Cells[0, iCNT + 1]:=IntToStr((iCNT + 1));
          Transit.Clear;
        end;
        Result:=True;
      { ---------------------------------------------------------------------------------------------------------------------------------------- ON EXCEPTION }
      except
        on E: Exception do
        begin
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: CSV Import has failed: ' + ExtractFileName(fPath));
          SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('CSV Import has failed. Please check the file and try again.')));
          IsError:=True;
        end;
      end;
    { ----------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
    finally
      MainForm.ExecMessage(True, WM_GETINFO, 10, stReady);
      if not IsError then
      begin
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Data has been imported successfully!');
        MainForm.ExecMessage(False, WM_GETINFO, 1, 'Data has been imported successfully!');
      end;
      Data.Free;
      Transit.Free;
    end;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- EXPORT }
function TStringGrid.ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
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
  Result :=False;
  IsError:=False;
  CSVData:=TStringList.Create;
  { ------------------------------------------------------------------------------------------------------------------------------------------ WRITE CSV FILE }
  try
    MainForm.ExecMessage(True, WM_GETINFO, 10, stExportCSV);
    { ADD ROWS AND COLUMNS WITH DELIMITER }
    for iCNT:=1 to Self.RowCount - 1 do
    begin
      for jCNT:= 1 to Self.ColCount - 1 do
      begin
        CleanStr :=Self.Cells[jCNT, iCNT];
        CleanStr :=StringReplace(CleanStr, CRLF, ' ', [rfReplaceAll]);
        MyStr    :=MyStr + CleanStr + Delimiter;
      end;
      CSVData.Add(MyStr);
      MyStr:='';
    end;
    { SAVE TO FILE AS PLAIN TEXT }
    try
      if DialogBox.Execute = True then CSVData.SaveToFile(DialogBox.FileName);
      Result:=True;
    { ------------------------------------------------------------------------------------------------------------------------------------------ ON EXCEPTION }
    except
      on E: Exception do
      begin
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Cannot saved file: ' + ExtractFileName(fPath));
        SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('Cannot save the file in the given location.')));
        IsError:=True;
      end;
    end;
  { ------------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
  finally
    if not IsError then
    begin
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Data has been exported successfully!');
      SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Address Book have been exported successfully!')));
    end;
    CSVData.Free;
    MainForm.ExecMessage(True, WM_GETINFO, 10, stReady);
  end;
end;

{ ################################################################## ! HELPERS ! ############################################################################ }

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
  Result:=unNA;
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
  if WndType = 1 { INFO       } then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OK       + MB_ICONINFORMATION);
  if WndType = 2 { WARNING    } then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OK       + MB_ICONWARNING);
  if WndType = 3 { ERROR      } then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OK       + MB_ICONERROR);
  if WndType = 4 { QUESTION 1 } then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OKCANCEL + MB_ICONQUESTION);
  if WndType = 5 { QUESTION 2 } then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_YESNO    + MB_ICONQUESTION);
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

{ ----------------------------------------------------------------------------------------------------------------------------------- CO CODE NAME CONVERTION }
function TMainForm.ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
var
  iCNT:  integer;
begin
  { INITIALIZE }
  Result:= '';

  (* USED ONLY FOR OPEN ITEMS AND AGING VIEW *)

  { ALLOW TO CONVERT '2020' TO 'F2020', ETC. }
  if mode = 0 then
  begin
    if Length(CoNumber) = 4 then Result:=Prefix + CoNumber;
    if Length(CoNumber) = 3 then Result:=Prefix + '0'  + CoNumber;
    if Length(CoNumber) = 2 then Result:=Prefix + '00' + CoNumber;
  end;

  (* USED ONLY TO BUILD GROUP_ID *)

  { CONVERTS FROM     }
  {  1. 2020 TO 02020 }
  {  2. 340  TO 00340 }
  {  3. 43   TO 00043 }
  {  4. 0    TO 00000 }
  if mode = 1 then
  begin
    if Length(CoNumber) = 4 then Result:='0'   + CoNumber;
    if Length(CoNumber) = 3 then Result:='00'  + CoNumber;
    if Length(CoNumber) = 2 then Result:='000' + CoNumber;
    if Length(CoNumber) = 1 then Result:='00000';
  end;

  { CONVERTS FROM 02020 TO 2020 }
  if mode = 2 then
  begin
    for iCNT:= 1 to Length(CoNumber) do
    begin
      if CoNumber[iCNT] <> '0' then
      begin
        Result:=Copy(CoNumber, iCNT, MaxInt);
        Exit;
      end;
    end;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------- TURN ON OR OFF ALL TIMERS }

(* TURN OFF ALL TIMERS DURING UPDATING CYCLE OF MAKING NEW AGE VIEW BASED ON FRESH DOWNLOAD OF OPEN ITEMS *)

procedure TMainForm.SwitchTimers(state: Integer);
begin
  { ALL ON }
  if state = tmEnabled then
  begin
    EventLogTimer.Enabled    :=True;
    InvoiceScanTimer.Enabled :=True;
    UpdaterTimer.Enabled     :=True;
    OILoader.Enabled         :=True;
    InetTimer.Enabled        :=True;
  end;
  { ALL OFF }
  if state = tmDisabled then
  begin
    EventLogTimer.Enabled    :=False;
    InvoiceScanTimer.Enabled :=False;
    UpdaterTimer.Enabled     :=False;
    OILoader.Enabled         :=False;
    InetTimer.Enabled        :=False;
  end;
end;

{ ################################################################## ! EVENTS ! ############################################################################# }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TMainForm.FormCreate(Sender: TObject);
var
  AppVersion:   string;
  AppSettings:  TSettings;
  DataBase:     TDataBase;
  DataTables:   TDataTables;
  UserControl:  TUserControl;
  Transactions: TTransactions;
  RegSettings:  TFormatSettings;
  NowTime:      TTime;
  iCNT:         integer;
begin

  { ------------------------------------------------------------ ! INITIALIZATION ! ------------------------------------------------------------------------- }
  AppSettings  :=TSettings.Create;
  FUserName    :=AppSettings.WinUserName;
  FEventLogPath:=AppSettings.FPathEventLog;
  AppVersion   :=GetBuildInfoAsString;
  KeyPreview   :=True;
  PAllowClose  :=False;

  { --------------------------------------------------------------------------------------------------------------------------------------- REGIONAL SETTINGS }
  RegSettings:=TFormatSettings.Create;
  RegSettings.CurrencyDecimals    :=4;
  RegSettings.DateSeparator       :='-';
  RegSettings.ShortDateFormat     :='yyyy-mm-dd';
  RegSettings.LongDateFormat      :='yyyy-mm-dd';
  RegSettings.TimeSeparator       :=':';
  RegSettings.TimeAMString        :='AM';
  RegSettings.TimePMString        :='PM';
  RegSettings.ShortTimeFormat     :='hh:mm:ss';
  RegSettings.LongTimeFormat      :='hh:mm:ss';
  FormatSettings                  :=RegSettings;
  Application.UpdateFormatSettings:=False;

  { ------------------------------------------------------------------------------------------------------------------------------ APPLICATION NAME | CAPTION }
  MainForm.Caption :=AppSettings.TMIG.ReadString(ApplicationDetails, 'VALUE', APPNAME);
  GroupName.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'GROUP_NAME', 'n/a');

  { --------------------------------------------------------------- ! WINDOW POSITION ! --------------------------------------------------------------------- }

  MainForm.DefaultMonitor:=dmDesktop;          (* DO NOT CHANGE THAT *)
  MainForm.Position      :=poDefaultSizeOnly;  (* DO NOT CHANGE THAT *)
  MainForm.Top           :=AppSettings.TMIG.ReadInteger(ApplicationDetails, 'WINDOW_TOP',  0);
  MainForm.Left          :=AppSettings.TMIG.ReadInteger(ApplicationDetails, 'WINDOW_LEFT', 0);

  { ------------------------------------------------------------- ! DATE & TIME ! --------------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------------------------ FORMAT DATE AND TIME }
  NowTime  :=Now;
  PStartTime:=Now;
  FormatDateTime('hh:mm:ss', NowTime);
  FormatDateTime('hh:mm:ss', PStartTime);

  { ------------------------------------------------------------- ! STATUS BAR ! ---------------------------------------------------------------------------- }
  StatBar_TXT1.Caption:=stReady;
  StatBar_TXT2.Caption:=FUserName + '.';
  StatBar_TXT3.Caption:=DateToStr(Now);

  { ----------------------------------------------------------- ! DEFAULT VALUES ! -------------------------------------------------------------------------- }

  Edit_PASSWORD.Text   :='';
  Edit_CurrPassWd.Text :='';
  Edit_NewPassWd.Text  :='';
  Edit_ConfPassWd.Text :='';
  btnPassUpdate.Enabled:=False;
  MyPages.ActivePage   :=TabSheet1;

  { ------------------------------------------------------------ ! RISK CLASSESS ! -------------------------------------------------------------------------- }

  { WE USE COMMA DECIMAL SEPARATOR BY DEFAULT }
  if FormatSettings.DecimalSeparator = ',' then
  begin
    procRISKA.Caption:=FloatToStr(StrToFloat(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', RISK_CLASS_A)) * 100) + '%';
    procRISKB.Caption:=FloatToStr(StrToFloat(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_B_MAX', RISK_CLASS_B)) * 100) + '%';
    procRISKC.Caption:=FloatToStr(StrToFloat(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_C_MAX', RISK_CLASS_C)) * 100) + '%';
  end;
  { CHANGE COMMA DECIMAL SEPARATOR TO POINT DECIMAL SEPARATOR }
  if FormatSettings.DecimalSeparator = '.' then
  begin
    procRISKA.Caption:=FloatToStr(StrToFloat(StringReplace(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_A_MAX', RISK_CLASS_A), ',', '.', [rfReplaceAll])) * 100) + '%';
    procRISKB.Caption:=FloatToStr(StrToFloat(StringReplace(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_B_MAX', RISK_CLASS_B), ',', '.', [rfReplaceAll])) * 100) + '%';
    procRISKC.Caption:=FloatToStr(StrToFloat(StringReplace(AppSettings.TMIG.ReadString(RiskClassDetails, 'CLASS_C_MAX', RISK_CLASS_C), ',', '.', [rfReplaceAll])) * 100) + '%';
  end;
  { ------------------------------------------------------------- ! TAB SHEETS ! ---------------------------------------------------------------------------- }

  TabSheet1.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB1', 'TAB1');
  TabSheet2.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB2', 'TAB2');
  TabSheet3.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB3', 'TAB3');
  TabSheet4.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB4', 'TAB4');
  TabSheet5.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB5', 'TAB5');
  TabSheet6.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB6', 'TAB6');
  TabSheet7.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB7', 'TAB7');
  TabSheet8.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB8', 'TAB8');

  { ------------------------------------------------------- ! ASSIGN PRE-DEFINED HEADERS ! ------------------------------------------------------------------ }

  { ---------------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS }
  sgOpenItems.RowCount:=2;
  sgOpenItems.Cols[0].Text:= '';
  for iCNT:=1 to sgOpenItems.ColCount do
  begin
    sgOpenItems.Cols[iCNT].Text:=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER' + IntToStr(iCNT),  '(column)');
  end;

  { ------------------------------------------------------- ! CAPTIONS FOR ALL SHAPES ! --------------------------------------------------------------------- }

  (* AGING REPORT | TABSHEET1 *)
  Cap01.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT01', 'EMPTY'), [fsBold]);
  Cap02.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT02', 'EMPTY'), [fsBold]);
  Cap03.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT03', 'EMPTY'), [fsBold]);
  Cap05.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT05', 'EMPTY'), [fsBold]);
  Cap06.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT06', 'EMPTY'), [fsBold]);
  Cap07.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT07', 'EMPTY'), [fsBold]);

  (* OPEN ITEMS | TABSHEET2 *)
  Cap10.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS2TXT01', 'EMPTY'), [fsBold]);
  Cap11.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS2TXT02', 'EMPTY'), [fsBold]);
  Cap12.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS2TXT03', 'EMPTY'), [fsBold]);

  (* ADDRESS BOOK | TABSHEET3 *)
  Cap13.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS3TXT01', 'EMPTY'), [fsBold]);

  (* INVOICE TRACKER | TABSHEET4 *)
  Cap43.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS4TXT01', 'EMPTY'), [fsBold]);

  (* GENERAL TABLES  | TABSHEET7 *)
  Cap15.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT01', 'EMPTY'), [fsBold]);
  Cap16.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT02', 'EMPTY'), [fsBold]);
  Cap17.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT03', 'EMPTY'), [fsBold]);
  Cap18.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT04', 'EMPTY'), [fsBold]);
  Cap19.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT05', 'EMPTY'), [fsBold]);
  Cap20.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT06', 'EMPTY'), [fsBold]);

  (* SETTINGS | TABSHEET8 *)
  Cap21.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT01', 'EMPTY'), [fsBold]);
  Cap22.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT02', 'EMPTY'), [fsBold]);
  Cap23.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT03', 'EMPTY'), [fsBold]);
  Cap27.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT04', 'EMPTY'), [fsBold]);

  { -------------------------------------------------------- ! ADDRESS BOOK TABSHEET ! ---------------------------------------------------------------------- }

  sgAddressBook.RowCount:=2;

  { -------------------------------------------------------------- ! MAIN VIEW ! ---------------------------------------------------------------------------- }

  { --------------------------------------------------------------------------------------------------------------------------------- AGING BUCKETS | CAPTION }
  tR1.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE1A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE1B','');
  tR2.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE2A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE2B','');
  tR3.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE3A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE3B','');
  tR4.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE4A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE4B','');
  tR5.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE5A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE5B','');
  tR6.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE6A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE6B','');

  { ---------------------------------------------------------------------------------------------------------------------------------- SUMMARY BOX | CAPTIONS }
  Text21.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE1A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE3B','') + ':';
  Text22.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE4A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE6B','') + ':';

  { ------------------------------------------------------ ! DATABASE INITIALIZATION & UAC ! ---------------------------------------------------------------- }

  { ESTABLISH ACTIVE CONNECTIVITY }
  FDbConnect:=TADOConnection.Create(Self);
  DataBase  :=TDataBase.Create(True);
  try
    DataBase.InitializeConnection(MainThreadID, True, FDbConnect);
  finally
    DataBase.Free;
  end;
  { UPLOAD USER DETAILS }
  UserControl:=TUserControl.Create(FDbConnect);
  try
    FEventLogPath:=AppSettings.FPathEventLog;
    UserControl.UserName:=FUserName;
    AccessLevel:=UserControl.GetAccessData(adAccessLevel);
    AccessMode :=UserControl.GetAccessData(adAccessMode);
    UserControl.GetGroupList(FGroupList, GroupListBox);
    UserControl.GetAgeDates(GroupListDates, FGroupList[0, 0]);
    { RESTRICTED FOR "ADMINS" }
    if AccessLevel <> acADMIN then
    begin
      DetailsGrid.Enabled   :=False;
      ReloadCover.Visible   :=True;
      ReloadCover.Cursor    :=crNo;
      GroupListDates.Enabled:=False;
    end;
    { NOT ALLOWED FOR "RO" USERS }
    if AccessLevel = acReadOnly then
    begin
      Action_Tracker.Enabled  :=False;
      Action_AddToBook.Enabled:=False;
    end;
  finally
    UserControl.Free;
  end;

  { --------------------------------------------------------- ! READ DEFAULT AGE VIEW ! --------------------------------------------------------------------- }

  if (GroupListBox.Text <> '') and (GroupListDates.Text <> '') then
  begin
    GroupIdSel:=FGroupList[GroupListBox.ItemIndex, 0];
    GroupNmSel:=FGroupList[GroupListBox.ItemIndex, 1];
    AgeDateSel:=GroupListDates.Text;
    sgAgeView.Enabled:=True;
    Transactions:=TTransactions.Create(FDbConnect);
    try
      OpenItemsUpdate:=Transactions.GetDateTime(gdDateTime);
      if OpenItemsUpdate = '' then
      begin
        MsgCall(2, 'Cannot find open items in database. Please contact IT support.');
        TTReadAgeView.Create(thNullParameter);
      end
        else
          TTReadAgeView.Create(thCallOpenItems);
    finally
      Transactions.Free;
    end;
  end;

  { ------------------------------------------------------------ ! GENERAL TABLES ! ------------------------------------------------------------------------- }

  DataTables:=TDataTables.Create(FDbConnect);
  try
    DataTables.OpenTable(AppSettings.TMIG.ReadString(GeneralTables, 'MAP1', '')); DataTables.SqlToGrid(sgCoCodes,  DataTables.ExecSQL, False, True);
    DataTables.OpenTable(AppSettings.TMIG.ReadString(GeneralTables, 'MAP4', '')); DataTables.SqlToGrid(sgPmtTerms, DataTables.ExecSQL, False, True);
    DataTables.OpenTable(AppSettings.TMIG.ReadString(GeneralTables, 'MAP5', '')); DataTables.SqlToGrid(sgPaidInfo, DataTables.ExecSQL, False, True);
    DataTables.OpenTable(AppSettings.TMIG.ReadString(GeneralTables, 'MAP6', '')); DataTables.SqlToGrid(sgGroup3,   DataTables.ExecSQL, False, True);
    DataTables.OpenTable(AppSettings.TMIG.ReadString(GeneralTables, 'MAP7', '')); DataTables.SqlToGrid(sgPerson,   DataTables.ExecSQL, False, True);
  finally
    DataTables.Free;
  end;

  { ----------------------------------------------------------------------------------------------------------------------------- START WEB PAGE | UNITY INFO }
  WebBrowser.Navigate(WideString(AppSettings.TMIG.ReadString(ApplicationDetails, 'START_PAGE', '')), $02);

  { -------------------------------------------------------------------------------------------------------------------------- APPLICATION VERSION & USER SID }
  LogText(FEventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Application version = ' + AppVersion);
  LogText(FEventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User SID = ' + GetCurrentUserSid);

  { ---------------------------------------------------------- ! TIMERS INTERVALS ! ------------------------------------------------------------------------- }

  (* 'INETTIMER' IS EXCLUDED FROM BELOW LIST BECAUSE IT IS CONTROLED BY 'INITIAIZECONNECTION' METHOD *)

  EventLogTimer.Interval   :=AppSettings.TMIG.ReadInteger(TimersSettings, 'EVENTLOG_UPDATE', 60000);  { DEFAULT VALUE 60000   MILISECONDS = 1  MINUTE  }
  InvoiceScanTimer.Interval:=AppSettings.TMIG.ReadInteger(TimersSettings, 'INVOICE_SCANNER', 900000); { DEFAULT VALUE 900000  MILISECONDS = 15 MINUTES }
  UpdaterTimer.Interval    :=AppSettings.TMIG.ReadInteger(TimersSettings, 'UPDATE_CHECKER',  60000);  { DEFAULT VALUE 60000   MILISECONDS = 1  MINUTE  }
  OILoader.Interval        :=AppSettings.TMIG.ReadInteger(TimersSettings, 'OI_LOADER',       300000); { DEFAULT VALUE 3000000 MILISECONDS = 5  MINUTES }

  { DISPOSE OBJECTS }
  FreeAndNil(AppSettings);

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
  { DO NOTHING }
end;

{ ------------------------------------------------------------- ! MAIN FORM EVENTS ! ------------------------------------------------------------------------ }

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TMainForm.FormShow(Sender: TObject);
begin
  FormResize(self);
  (* CustomPanelBorders(Header1, clWhite, clRed, clRed, clRed, clRed); TPANEL TEST LINE *)
  MainForm.sgCoCodes.SetColWidth       (10, 30);
  MainForm.sgPmtTerms.SetColWidth      (10, 30);
  MainForm.sgPaidInfo.SetColWidth      (10, 30);
  MainForm.sgGroup3.SetColWidth        (10, 30);
  MainForm.sgPerson.SetColWidth        (10, 30);
  MainForm.sgOpenItems.SetColWidth     (10, 20);
  MainForm.sgAddressBook.SetColWidth   (10, 20);
  MainForm.sgInvoiceTracker.SetColWidth(10, 20);
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
var
  AppSettings:  TSettings;
begin
  { GO MINIMIZE AND HIDE FROM TASKBAR | DO NOT CLOSE }
  if not PAllowClose then
  begin
    CanClose:=False;
    ShowWindow(Handle, SW_MINIMIZE);
    Hide();
  end
  else
  { SHUTDOWN APPLICATION }
  begin
    AppSettings:=TSettings.Create;
    try
      { ----------------------------------------------------------------------------------------------------------------------------------- SAVE ALL SETTINGS }
      AppSettings.TMIG.WriteInteger(ApplicationDetails, 'WINDOW_TOP',   MainForm.Top);
      AppSettings.TMIG.WriteInteger(ApplicationDetails, 'WINDOW_LEFT',  MainForm.Left);
      if MainForm.WindowState = wsNormal    then AppSettings.TMIG.WriteString(ApplicationDetails,  'WINDOW_STATE', 'wsNormal');
      if MainForm.WindowState = wsMaximized then AppSettings.TMIG.WriteString(ApplicationDetails,  'WINDOW_STATE', 'wsMaximized');
      if MainForm.WindowState = wsMinimized then AppSettings.TMIG.WriteString(ApplicationDetails,  'WINDOW_STATE', 'wsMinimized');
      if sgAgeView.RowCount > 2 then sgAgeView.SaveLayout(ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);
      if not (AppSettings.Encode(UserConfig)) then Application.MessageBox(PChar('Cannot write user configuration file. Please contact support.'), PChar(CAPTION), MB_OK + MB_ICONSTOP);
      if not(AppSettings.Encode(AppConfig))   then Application.MessageBox(PChar('Cannot write master configuration file. Please contact support.'), PChar(CAPTION), MB_OK + MB_ICONSTOP);
      LogText(FEventLogPath, 'Application closed by the user.');
      { ------------------------------------------------------------------------------------------------------------------------------------- RELEASE & CLOSE }
    finally
      AppSettings.Free;
    end;
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
      EventLog.Lines.LoadFromFile(FEventLogPath);
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
  LogText(FEventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Calling open items scanner...');
  TTOpenItemsScanner.Create;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SHOW CURRENT TIME }
procedure TMainForm.CurrentTimeTimer(Sender: TObject);
begin
  StatBar_TXT4.Caption:=TimeToStr(Now);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW UPTIME }
procedure TMainForm.UpTimeTimer(Sender: TObject);
var
  Result: TTime;
begin
  Result:=Now - PStartTime;
  StatBar_TXT5.Caption:=TimeToStr(Result);
end;

{ ---------------------------------------------------------------- ! POPUP MENUS ! -------------------------------------------------------------------------- }

{ ------------------------------------------------------------- ! ADDRESS BOOK MENU ! ----------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK CONTEXT MENU }
procedure TMainForm.BookPopupPopup(Sender: TObject);
begin
  Action_ShowMyEntries.Caption:='Show ' + UpperCase(MainForm.FUserName) + ' entries';
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
  sgAddressBook.CopyCutPaste(adCut);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ COPY }
procedure TMainForm.Action_CopyClick(Sender: TObject);
begin
  sgAddressBook.CopyCutPaste(adCopy);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- PASTE }
procedure TMainForm.Action_PasteClick(Sender: TObject);
begin
  sgAddressBook.CopyCutPaste(adPaste);
end;

{ ------------------------------------------------------------------------------------------------------------------------- ADD EMPTY ROW TO THE ADDRESS BOOK }
procedure TMainForm.Action_AddRowClick(Sender: TObject);
var
  jCNT:  integer;
begin
  sgAddressBook.RowCount:=sgAddressBook.RowCount + 1;
  sgAddressBook.Row:=sgAddressBook.RowCount - 1;
  sgAddressBook.Cells[0, sgAddressBook.RowCount - 1]:='';
  sgAddressBook.Cells[1, sgAddressBook.RowCount - 1]:=MainForm.FUserName;
  for jCNT:=2 to sgAddressBook.ColCount - 1 do sgAddressBook.Cells[jCNT, sgAddressBook.RowCount - 1]:='';
end;

{ --------------------------------------------------------------------------------------------------------------------------------- DELETE GIVEN CUID FROM DB }
procedure TMainForm.Action_DelRowClick(Sender: TObject);
var
  DataTables: TDataTables;
begin
  { ASK BEFORE DELETE }
  if MsgCall(5, 'Are you sure you want to delete this customer?' + CRLF + 'This operation cannot be reverted.') = IDNO then Exit;
  { EXECUTE DELETE QUERY }
  if sgAddressBook.Cells[0, sgAddressBook.Row] <> '' then
  begin
    DataTables:=TDataTables.Create(FDbConnect);
    try
      DataTables.StrSQL:=DELETE_FROM         +
                           TblAddressbook    +
                         WHERE               +
                           TAddressBook.CUID +
                         EQUAL               +
                           DataTables.CleanStr(sgAddressBook.Cells[2, sgAddressBook.Row], True);
      DataTables.ExecSQL;
    finally
      DataTables.Free;
    end;
  end;
  { REMOVE FROM STRING GRID ANYWAY }
  sgAddressBook.DeleteRowFrom(1, 1);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- OPEN SEARCH WINDOW }
procedure TMainForm.Action_SearchBookClick(Sender: TObject);
begin
  { SETUP AND CALL WINDOW }
  SearchForm.SGrid     :=MainForm.sgAddressBook;
  SearchForm.SColName  :=TAddressBook.CUSTNAME;
  SearchForm.SColNumber:=TAddressBook.CUSTNUMBER;
  WndCall(SearchForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ SHOW ALL ENTRIES }
procedure TMainForm.Action_ShowAsIsClick(Sender: TObject);
begin
  TTAddressBook.Create(adOpenAll, sgAddressBook);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ SHOW USER ENTRIES ONLY }
procedure TMainForm.Action_ShowMyEntriesClick(Sender: TObject);
begin
  TTAddressBook.Create(adOpenForUser, sgAddressBook);
end;

{ -------------------------------------------------------------- ! MAIN FORM MENU ! ------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- EXECUTE WHEN MENU OPENS }
procedure TMainForm.AgeViewPopupPopup(Sender: TObject);
begin

  { ONLY ADMINS AND RW USERS CAN USE ADDRESSBOOK AND INVOICE TRACKER }
  if AccessLevel = acReadOnly then Exit;

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
      if (sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1), iCNT] = sgOpenItems.Cells[38, jCNT]) and (sgAgeView.RowHeights[Row] <> - 1) then
      begin
        sgAddressBook.RowCount:=sgAddressBook.RowCount + 1;
        { ----------------------------------------------------------------------------------------------------------------------------------------- MOVE DATA }
        sgAddressBook.Cells[0, sgAddressBook.RowCount - 1 + OffSet]:='';
        sgAddressBook.Cells[1, sgAddressBook.RowCount - 1 + OffSet]:=MainForm.FUserName;
        sgAddressBook.Cells[2, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[38, jCNT];
        sgAddressBook.Cells[3, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[2,  jCNT];
        sgAddressBook.Cells[4, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[7,  jCNT];
        sgAddressBook.Cells[9, sgAddressBook.RowCount - 1 + OffSet]:=sgOpenItems.Cells[22, jCNT] + SPACE +
                                                                     sgOpenItems.Cells[23, jCNT] + SPACE +
                                                                     sgOpenItems.Cells[24, jCNT] + SPACE +
                                                                     sgOpenItems.Cells[25, jCNT] + SPACE +
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
  FilterForm.FColName:=TSnapshots.fINF7;
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
  SearchForm.SColName  :=TSnapshots.fCUSTOMER_NAME;
  SearchForm.SColNumber:=TSnapshots.fCUSTOMER_NUMBER;
  WndCall(SearchForm, 0);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SHOW PAYMENT TERM }
procedure TMainForm.Action_PaymentTermClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(FDbConnect);
  try
    MsgCall(1, 'Payment term: ' + AgeView.GetData(sgAgeView, sgPmtTerms, TSnapshots.fPAYMENT_TERMS));
  finally
    AgeView.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW GROUP3 }
procedure TMainForm.Action_Group3Click(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(FDbConnect);
  try
    MsgCall(1, 'Assigned to Group3: ' + AgeView.GetData(sgAgeView, sgGroup3, TSnapshots.fGROUP3));
  finally
    AgeView.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW PERSON }
procedure TMainForm.Action_PersonClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(FDbConnect);
  try
    MsgCall(1, 'Assigned to Person: ' + AgeView.GetData(sgAgeView, sgPerson, TSnapshots.fPERSON));
  finally
    AgeView.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- SHOW INF4 }
procedure TMainForm.Action_INF4Click(Sender: TObject);
var
  Return:  string;
begin
  Return:=sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fINF4, 1, 1) , sgAgeView.Row];
  if (Return = '') or (Return = ' ') then Return:=unUnassigned;
  MsgCall(1, 'Assigned to INF4: ' + Return);
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
procedure TMainForm.Action_BasicViewClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(FDbConnect);
  try
    AgeView.AgeViewMode(mainForm.sgAgeView, AgingBasic);
  finally
    AgeView.Free;
  end;
  { TICK }
  Action_BasicView.Checked:=True;
  Action_FullView.Checked :=False;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- SHOW ALL AVAILABLE COLUMNS }
procedure TMainForm.Action_FullViewClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(FDbConnect);
  try
    AgeView.AgeViewMode(mainForm.sgAgeView, AgingFull);
  finally
    AgeView.Free;
  end;
  { TICK }
  Action_BasicView.Checked:=False;
  Action_FullView.Checked :=True;
end;

{ -------------------------------------------------------------- ! INVOICE TRACKER ! ------------------------------------------------------------------------ }

{ ------------------------------------------------------------------------------------------------------------------------------------------ REMOVE FROM LIST }
procedure TMainForm.Action_RemoveClick(Sender: TObject);
begin

  { R/W USER CAN REMOVE ITEM }
  if (MainForm.AccessLevel = acReadWrite) and (UpperCase(MainForm.FUserName) = UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
    if MsgCall(5, 'Are you sure you want to remove selected customer?') = IDYES then
      TTInvoiceTrackerRefresh.Create('REMOVE');

  { R/W USER CANNOT REMOVE OTHER ITEM }
  if (MainForm.AccessLevel = acReadWrite) and (UpperCase(MainForm.FUserName) <> UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
    MsgCall(2, 'You cannot remove someone''s else item.');

  { ADMINISTRATOR CAN REMOVE ANY ITEM }
  if (MainForm.AccessLevel = acADMIN) then
    if MsgCall(5, 'Are you sure you want to remove selected customer?') = IDYES then TTInvoiceTrackerRefresh.Create('REMOVE');

  { READ ONLY USER CANNOT REMOVE ANYTHING }
  if (MainForm.AccessLevel = acReadOnly) then MsgCall(2, 'You don''t have permission to remove items.');

end;

{ ----------------------------------------------------------------------------------------------------------------------- SHOW SENT INVOICES FOR GIVEN 'CUID' }
procedure TMainForm.Action_ShowRegisteredClick(Sender: TObject);
begin
  WndCall(InvoicesForm, 0);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- SHOW MY ITEMS }
procedure TMainForm.Action_ShowMyClick(Sender: TObject);
begin
  TTInvoiceTrackerRefresh.Create(UpperCase(MainForm.FUserName));
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

{ ---------------------------------------------------------------------------------------------------------------------------- UPDATE LIST FOR SELECTED GROUP }
procedure TMainForm.GroupListBoxSelect(Sender: TObject);
var
  UserControl: TUserControl;
begin
  UserControl:=TUserControl.Create(FDbConnect);
  try
    UserControl.UserName:=FUserName;
    UserControl.GetAgeDates(GroupListDates, FGroupList[GroupListBox.ItemIndex, 0]);
  finally
    UserControl.Free;
  end;
end;

{ ------------------------------------------------------- ! COMPONENT EVENTS | TABSHEETS ! ------------------------------------------------------------------ }

{ ---------------------------------------------------------------------------------------------------------------------------------------------- REFRESH VIEW }
procedure TMainForm.TabSheet7Resize(Sender: TObject);
begin
  TabSheet7Show(self);
end;

procedure TMainForm.TabSheet2Show(Sender: TObject);
begin
  sgOpenItems.SetColWidth(10, 20);
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
  Temp:    TLists;
  SqlRows: integer;
  TmpRows: integer;
begin
  try
    try
      { ------------------------------------------------------------------------------------------------------------------------------- SETTING UP DIMENSIONS }

      (* WARNING! "HIGH" METHOD RETURNS NUMBER OF ROWS COUNTING FROM ZERO *)
      (*          WHILE "SETLENGTH" METHOD SETUP ARRAY COUNTING FROM ONE  *)
      (*          THEREFORE, WE NEED TO ADD ONE TO MATCH DIMENSIONS       *)

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
    sgAgeView.SaveLayout(ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);
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
  sgAgeView.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);

  { COLUMNS ORDER MAY BE CHANGED BY THE USER  }
  { FIND COLUMN NUMBERS FOR GIVEN COLUMN NAME }
  Col1 :=sgAgeView.ReturnColumn(TSnapshots.fNOT_DUE,         1, 1);
  Col2 :=sgAgeView.ReturnColumn(TSnapshots.fRANGE1,          1, 1);
  Col3 :=sgAgeView.ReturnColumn(TSnapshots.fRANGE2,          1, 1);
  Col4 :=sgAgeView.ReturnColumn(TSnapshots.fRANGE3,          1, 1);
  Col5 :=sgAgeView.ReturnColumn(TSnapshots.fRANGE4,          1, 1);
  Col6 :=sgAgeView.ReturnColumn(TSnapshots.fRANGE5,          1, 1);
  Col7 :=sgAgeView.ReturnColumn(TSnapshots.fRANGE6,          1, 1);
  Col8 :=sgAgeView.ReturnColumn(TSnapshots.fOVERDUE,         1, 1);
  Col9 :=sgAgeView.ReturnColumn(TSnapshots.fTOTAL,           1, 1);
  Col10:=sgAgeView.ReturnColumn(TSnapshots.fCREDIT_LIMIT,    1, 1);
  Col11:=sgAgeView.ReturnColumn(TSnapshots.fEXCEEDED_AMOUNT, 1, 1);

  { DRAW ONLY SELECTED COLUMNS }
  if ( (ACol = Col1) or (ACol = Col2) or (ACol = Col3) or (ACol = Col4) or (ACol = Col5) or (ACol = Col6) or (ACol = Col7) or (ACol = Col8) or (ACol = Col9) or (ACol = Col10) or (ACol = Col11) )
    and (ARow > 0) then
      begin
        sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clBlack);
      end;

end;

procedure TMainForm.sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

  (* CALL SG_DRAWSELECTED BEFORE SG_COLORVALUES *)

  { DRAW SELECTED ROW | SKIP HEADERS }
  MainForm.sgOpenItems.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);

  { ONLY FOR COLUMNS 4, 5, 8, 9 THAT SHOWS NEGATIVE AMOUNTS AND COLUMN 33 FOR PAYMENT STATUS }
  if ( (ACol = 4) or (ACol = 5) or (ACol = 8) or (ACol = 9) or (ACol = 33) ) and (ARow > 0) then begin
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

procedure TMainForm.DetailsGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  DetailsGrid.Selection:=TGridRect(Rect(-1, -1, -1, -1));
end;

procedure TMainForm.DetailsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  DetailsGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, clCream, clBlack, clCream, False);
end;

procedure TMainForm.sgAddressBookDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgAddressBook.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgInvoiceTrackerDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgInvoiceTracker.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgCoCodesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgCoCodes.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPaidInfoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgPaidInfo.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPmtTermsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgPmtTerms.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPersonDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgPerson.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgGroup3DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgGroup3.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgListSectionDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgListSection.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgListValueDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgListValue.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

{ ---------------------------------------------------- ! SETTING PANEL STRING GRID EVENTS !  ---------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------- LIST ALL KEYS WITH VALUES }
procedure TMainForm.sgListSectionSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  tsKEY:      TStringList;
  tsVAL:      TStringList;
  AppSettings: TSettings;
  iCNT:       integer;
  junk:       string;
  clean:      string;
begin
  CanSelect:=True;
  { LIST KEYS AND VALUES }
  tsKEY:=TStringList.Create();
  tsVAL:=TStringList.Create();
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.ReadSection(sgListSection.Cells[ACol, ARow], tsKEY);
    AppSettings.TMIG.ReadSectionValues(sgListSection.Cells[ACol, ARow], tsVAL);
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
    AppSettings.Free;
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
      PAllowClose:=True;
      Close;
    end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- LOCK ROWS FOR EDITING }
procedure TMainForm.DetailsGridKeyPress(Sender: TObject; var Key: Char);
begin
  { WE WILL AUTOFILL ROWS 1..3, THUS PREVENT USER FROM MANIPULATING }
  if (DetailsGrid.Row > 0) and (DetailsGrid.Row < 4) then  Key:=#0;
  { ALLOW ONLY DIGITS FOR CO CODE CELLS }
  if (DetailsGrid.Row = 0) and (not (CharInSet(Key, ['0'..'9', #8]))) then Key:=#0;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- INVOKE AUTOFILL }
procedure TMainForm.DetailsGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  (* NESTED PROCEDURE *)

  procedure Find(ColumnNum: integer);
  var
    iCNT:  integer;
  begin
    for iCNT:=1 to sgCoCodes.RowCount - 1 do
    begin
      if DetailsGrid.Cells[ColumnNum, 0] = sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.CO_CODE, 1, 1), iCNT] then
      begin
        DetailsGrid.Cells[ColumnNum, 1]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.COCURRENCY,    1, 1), iCNT];
        DetailsGrid.Cells[ColumnNum, 2]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.INTEREST_RATE, 1, 1), iCNT];
        DetailsGrid.Cells[ColumnNum, 3]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.AGENTS,        1, 1), iCNT];
        Break;
      end
      else
      begin
        DetailsGrid.Cells[ColumnNum, 1]:=unNA;
        DetailsGrid.Cells[ColumnNum, 2]:=unNA;
        DetailsGrid.Cells[ColumnNum, 3]:=unNA;
      end;
    end;
  end;

  (* MAIN BLOCK *)

begin
  { FIND GIVEN COMPANY CODE IN GENERAL TABLE AND RETURN ASSIGNED CURRENCY, INTEREST RATE AND AGENT INFO }
  if DetailsGrid.Col = 0 then Find(DetailsGrid.Col);
  if DetailsGrid.Col = 1 then Find(DetailsGrid.Col);
  if DetailsGrid.Col = 2 then Find(DetailsGrid.Col);
  if DetailsGrid.Col = 3 then Find(DetailsGrid.Col);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | MAKE GROUP }
procedure TMainForm.EditGroupNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (not (CharInSet(Key, ['A'..'Z', 'a'..'z', '0'..'9', '-', #8]))) then Key:=#0;
end;

{ -------------------------------------------------------------- ! COPY PASTE CUT ! ------------------------------------------------------------------------- }

                                                  (* ALLOW COPY "CTRL + C" ON ALL STRING GRIDS *)

procedure TMainForm.sgCoCodesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgCoCodes.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPaidInfoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgPaidInfo.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPmtTermsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgPmtTerms.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPersonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgPerson.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgGroup3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgGroup3.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgInvoiceTrackerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgInvoiceTracker.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgOpenItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgOpenItems.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then sgAgeView.CopyCutPaste(adCopy);
end;

{ --------------------------------------------------------------------------------------------------------------------- PASTE, CUT, COPY TO/FROM ADDRESS BOOK }
procedure TMainForm.sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  DataTables: TDataTables;
  Value:      string;
  Column:     string;
begin
  { FIRST COLUMN ARE NOT EDITABLE }
  if (sgAddressBook.Col = 1) then Exit;
  if (Key = 86) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(adPaste);
  if (Key = 67) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(adCopy);
  if (Key = 88) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(adCut);
  if Key = 46 then sgAddressBook.DelEsc(1, sgAddressBook.Col, sgAddressBook.Row);
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
      DataTables:=TDataTables.Create(FDbConnect);
      try
        { GET USER VALUE AND COLUMN NAME }
        Value:=DataTables.CleanStr(sgAddressBook.Cells[sgAddressBook.Col, sgAddressBook.Row], True);
        Column:=sgAddressBook.Cells[sgAddressBook.Col, 0];
        { PREPARE NON-QUERY }
        DataTables.StrSQL:=_UPDATE             +
                             TblAddressbook    +
                           _SET                +
                             Column            +
                           EQUAL               +
                             Value             +
                           WHERE               +
                             TAddressBook.CUID +
                           EQUAL               +
                             DataTables.CleanStr(sgAddressBook.Cells[2, sgAddressBook.Row], True);
        DataTables.ExecSQL;
      finally
        DataTables.Free;
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
  if Text50.Font.Style = [fsBold] then
  begin
    if (Key = 86) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(adPaste);
    if (Key = 88) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(adCut);
  end;
  if (Key = 67) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(adCopy);
  if (Key = 46) and (Text50.Font.Style = [fsBold]) then sgListSection.DelEsc(adDEL, sgListSection.Col, sgListSection.Row);
  if Key = 27 then sgListSection.DelEsc(adESC, sgListSection.Col, sgListSection.Row);
end;

procedure TMainForm.sgListSectionKeyPress(Sender: TObject; var Key: Char);
begin
  { UPDATE IF <ENTER> IS PRESSED }
  if Key = CR then sgListSection.Cells[1, sgListSection.Row]:=UpperCase(sgListSection.Cells[1, sgListSection.Row]);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ UPDATE VALUE KEY }
procedure TMainForm.sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Text50.Font.Style = [fsBold] then
  begin
    if (Key = 86) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(adPaste);
    if (Key = 88) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(adCut);
  end;
  if (Key = 67) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(adCopy);
  if (Key = 46) and (Text50.Font.Style = [fsBold]) then sgListValue.DelEsc(adDEL, sgListValue.Col, sgListValue.Row);
  if Key = 27 then sgListValue.DelEsc(adESC, sgListValue.Col, sgListValue.Row);
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
  if Key = CR then btnUnlockClick(self);
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
  Text54L1.Font.Color:=FONCOLOR;
  Text54L2.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnReloadMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  Text54L1.Font.Color:=clBlack;
  Text54L2.Font.Color:=clBlack;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ MAKE GROUP }
procedure TMainForm.btnMakeGroupMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  Text83L1.Font.Color:=FONCOLOR;
  Text83L2.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnMakeGroupMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  Text83L1.Font.Color:=clBlack;
  Text83L2.Font.Color:=clBlack;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- OPEN AB }
procedure TMainForm.btnOpenABMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  btnOpenAB.Cursor:=crHandPoint;
  Text64.Font.Color:=FONCOLOR;
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
  Text66.Font.Color:=FONCOLOR;
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
  Text67.Font.Color:=FONCOLOR;
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
  Text68.Font.Color:=FONCOLOR;
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
  Text69.Font.Color:=FONCOLOR;
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
  Text41.Font.Color:=FONCOLOR;
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
  Text42.Font.Color:=FONCOLOR;
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
  Text43.Font.Color:=FONCOLOR;
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
  Text48.Font.Color:=FONCOLOR;
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
  Text49.Font.Color:=FONCOLOR;
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
  Text50.Font.Color:=FONCOLOR;
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

{ ----------------------------------------------------------------------------------------------------------------------------------- MEMO | RELOAD EVENT LOG }
procedure TMainForm.EventReloadClick(Sender: TObject);
begin
  try
    try
      EventLog.Lines.LoadFromFile(FEventLogPath);
    except
      EventLog.Lines.Text:='Exception catch message: cannot load event log file. The file is locked by another process. Please try again later.';
    end;
  finally
    EventLog.SelStart:=Length(EventLog.Text);
    EventLog.SelLength:=0;
    SendMessage(EventLog.Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- AGE VIEW | LOAD }
procedure TMainForm.btnLoadAgeViewClick(Sender: TObject);
begin
  if (GroupListBox.Text <> '') and (GroupListDates.Text <> '') then
  begin
    { REMEMBER USER'S CHOICE }
    { AUTOMATION WIIL FOLLOW }
    GroupIdSel:=FGroupList[GroupListBox.ItemIndex, 0];
    GroupNmSel:=FGroupList[GroupListBox.ItemIndex, 1];
    AgeDateSel:=GroupListDates.Text;
    { SWITCH OFF ALL TIMERS }
    MainForm.SwitchTimers(tmDisabled);
    { LOAD AGE VIEW FOR SELECTED GROUP }
    TTReadAgeView.Create(thCallOpenItems);
  end
    else
      MsgCall(2, 'Cannot load selected group.');
end;

{ --------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | FORCE RELOAD }
procedure TMainForm.btnReloadClick(Sender: TObject);
begin
  { ONLY ADMINISTRATORS ARE ALLOWED }
  StatBar_TXT1.Caption :=stProcessing;
  if MainForm.AccessLevel = acADMIN then
  begin
    TTReadOpenItems.Create(thNullParameter);
  end else
  begin
    StatBar_TXT1.Caption:=stReady;
    LogText(FEventLogPath, '[Open Items]: User have no R/W access, process halted.');
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- ACTIONS | MAKE AGE }
procedure TMainForm.btnMakeGroupClick(Sender: TObject);
begin
  cbDump.Checked:=False;
  if sgOpenItems.RowCount < 2 then Exit;
  { ONLY ADMINISTRATORS ARE ALLOWED }
  if MainForm.AccessLevel = acADMIN then
    if PanelGroupName.Visible then
    begin
      PanelGroupName.Visible:=False;
    end else
    begin
      PanelGroupName.Visible:=True;
      { SUGGEST THE SAME GROUP NAME AND GROUP ID }
      EditGroupName.Text:=GroupListBox.Text;
      EditGroupID.Text  :=ConvertName(DetailsGrid.Cells[0, 0], '', 1) +
                          ConvertName(DetailsGrid.Cells[1, 0], '', 1) +
                          ConvertName(DetailsGrid.Cells[2, 0], '', 1) +
                          ConvertName(DetailsGrid.Cells[3, 0], '', 1);
    end
    else
    begin
      StatBar_TXT1.Caption:='Insufficient UAC level.';
      LogText(FEventLogPath, '[Make Group]: User have no R/W access, process halted.');
    end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------- ACTIONS | MAKE AGING REPORT }
procedure TMainForm.btnMakeGroupAgeClick(Sender: TObject);
begin
  if (EditGroupName.Text <> '') and (EditGroupID.Text <> '') then
  begin
    { START THREAD WITH NO PARAMETERS PASSED TO AN OBJECT }
    PanelGroupName.Visible:=False;
    ReloadCover.Visible   :=False;
    TTMakeAgeView.Create(MainForm.OSAmount);
  end
    else
      MsgCall(2, 'Please enter group name and try again.' + CRLF + 'If you will use existing one, then it will be overwritten.');
end;

{ -------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | OPEN FROM DB }
procedure TMainForm.btnOpenABClick(Sender: TObject);
begin
  TTAddressBook.Create(adOpenAll, sgAddressBook);
end;

{ ---------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | SAVE NEWLY ADDED }
procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  TTAddressBook.Create(adSaveNew, sgAddressBook);
end;

{ --------------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | CLOSE }
procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  if MsgCall(5, 'Are you sure you want to close address book?') = IDYES then sgAddressBook.ClearAll(2, 1, 1, True);
end;

{ --------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | IMPORT DATA }
procedure TMainForm.btnImportClick(Sender: TObject);
begin
  TTAddressBook.Create(adImport, sgAddressBook);
end;

{ ---------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | EXPORT ALL }
procedure TMainForm.btnExportClick(Sender: TObject);
begin
  TTAddressBook.Create(adExport, sgAddressBook);
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
  AppSettings:  TSettings;
  iCNT:         integer;
begin

  { ASK USER IF HE IS SURE BECAUSE ONCE DONE CANNOT BE UNDONE }
  if MsgCall(5, 'Are you sure you want to delete this section? It cannot be undone.') = IDNO then exit;
  if sgListSection.RowCount = 1 then exit;

  { DELETE SECTION FROM TMIg }
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.EraseSection(sgListSection.Cells[1, sgListSection.Row]);
    AppSettings.Encode(AppConfig);
  finally
    AppSettings.Free;
  end;

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
  AppSettings:  TSettings;
  iCNT:         integer;
begin

  { ASK USER IF HE IS SURE BECAUSE ONCE DONE CANNOT BE UNDONE }
  if MsgCall(5, 'Are you sure you want to delete this key? It cannot be undone.') = IDNO then Exit;

  { CHECK FOR LAST ROW }
  if sgListValue.RowCount = 1 then exit;

  { DELETE SECTION FROM TMIg }
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.DeleteKey(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, sgListValue.Row]);
    AppSettings.Encode(AppConfig);
  finally
    AppSettings.Free;
  end;

  { DELETE SELECTED ROW FROM STRING GRID }
  sgListValue.DeleteRowFrom(1, 1);

  { RE-NUMBER }
  for iCNT:= 1 to sgListValue.RowCount do sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;

{ ------------------------------------------------------------------------------------------------------------------------ VALUES, KEYS & SECTIONS | SAVE ALL }
procedure TMainForm.imgUpdateValuesClick(Sender: TObject);
var
  AppSettings:  TSettings;
  iCNT:         integer;
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

  AppSettings:=TSettings.Create;
  { SAVE TO SETTINGS FILE }
  try
    { WRITE ALL KEYS AND VALUES INTO TMEMINI }
    for iCNT:= 1 to (sgListValue.RowCount - 1) do
      AppSettings.TMIG.WriteString(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, iCNT], sgListValue.Cells[2, iCNT]);
    AppSettings.Encode(AppConfig);
  finally
    AppSettings.Free;
  end;

  MsgCall(1, 'All Keys and its values has been saved successfully.');

end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SAVE NEW PASSWORD }
procedure TMainForm.btnPassUpdateClick(Sender: TObject);
var
  AppSettings:  TSettings;
begin
  AppSettings:=TSettings.Create;
  if (AppSettings.TMIG.ReadString(Password, 'VALUE', '') = Edit_CurrPassWd.Text) then
  begin
    if Edit_NewPassWd.Text=Edit_ConfPassWd.text then
    begin
      AppSettings.TMIG.WriteString(Password,'VALUE',Edit_NewPassWd.Text);
      AppSettings.Encode(AppConfig);
      AppSettings.Free;
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

  (* COMMON VARIABLES *)
  var
    AppSettings:  TSettings;

  (* NESTED PROCEDURE *)

  procedure LockAction;
  var
    tStrings:     TStringList;
    iCNT:         integer;
    inner:        integer;
  begin

    if not Assigned(AppSettings) then Exit;

    { LOCK / UNLOCK }
    if btnUnlock.Caption = 'Unlock' then
    begin
      { IF PASSWORD IS OK, THEN UNLOCK AND LOAD CONFIGURATION SCRIPT FOR EDITING }
      if (AppSettings.TMIG.ReadString(Password, 'VALUE', '') <> '') and
         (AppSettings.TMIG.ReadString(Password, 'VALUE', '') = Edit_PASSWORD.Text) then
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
          AppSettings.TMIG.ReadSections(tStrings);
          { LIST ALL SECTIONS EXCEPT 'PASSWD' SECTION }
          sgListSection.RowCount:=tStrings.Count;
          inner:=1;
          for iCNT:=0 to tStrings.Count - 1 do
          begin
            if tStrings.Strings[iCNT] <> Password then
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
      if (AppSettings.TMIG.ReadString(Password, 'VALUE', '') <> '') and
         (AppSettings.TMIG.ReadString(Password, 'VALUE', '') <> Edit_PASSWORD.Text) then
      begin
        MsgCall(2, 'Incorrect password, please re-type it and try again.');
      end;

      { SETUP NEW PASSWORD }
      if (AppSettings.TMIG.ReadString(Password, 'VALUE', '') = '') then
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

  (* MAIN BLOCK *)

begin
  AppSettings:=TSettings.Create;
  try
    LockAction;
  finally
    AppSettings.Free;
  end;
end;

end.
