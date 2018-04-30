{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, Grids, ExtCtrls, StdCtrls, CheckLst, Buttons, PNGImage,
  DBGrids, AppEvnts, ShellAPI, INIFiles, StrUtils, ValEdit, DateUtils, Clipbrd, DB, ADODB, ActiveX, CDO_TLB, Diagnostics, Math, Wininet, ComObj, OleCtrls,
  SHDocVw;

{ REFERENCE TO ARRAYS }
type
  TLists    = array of array of string;
  TStrings  = array of string;
  TIntigers = array of integer;

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
    procedure SelectAll;
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
    tcCOCODE1: TLabel;
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
    ShapeList1: TShape;
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
    Header2: TPanel;
    hShapeActionsVOI: TShape;
    Cap10: TShape;
    hShapeSettingsVOI: TShape;
    Cap11: TShape;
    hShapeDetailsVOI: TShape;
    Cap12: TShape;
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
    Text58: TLabel;
    tcOpenItems: TLabel;
    tcInvoices: TLabel;
    tcOSAmt: TLabel;
    tcOverdue: TLabel;
    Text68: TLabel;
    btnImport: TImage;
    btnExport: TImage;
    Text69: TLabel;
    Header4: TPanel;
    hShapeInfoAM: TShape;
    Cap43: TShape;
    Header6: TPanel;
    ShapeContent6: TShape;
    Cap61: TShape;
    Header7: TPanel;
    hShapeInfoGT: TShape;
    Cap15: TShape;
    Text54L2: TLabel;
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
    Cap19: TShape;
    Cap20: TShape;
    LeftPanel: TPanel;
    ContentPanel7: TPanel;
    RightPanel: TPanel;
    MidPanel: TPanel;
    ContentPanel6: TPanel;
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
    Text73: TLabel;
    OILoader: TTimer;
    Text80: TLabel;
    Text81: TLabel;
    tcKPIoverdue: TLabel;
    tcKPIUnallocated: TLabel;
    Text82: TLabel;
    tcOvdAmt: TLabel;
    GroupListBox: TComboBox;
    GroupListDates: TComboBox;
    Text31: TLabel;
    procRISKA: TLabel;
    procRISKB: TLabel;
    procRISKC: TLabel;
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
    Cap27: TShape;
    N9: TMenuItem;
    Action_FilterINF7: TMenuItem;
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
    WebBrowser1: TWebBrowser;
    WebContainer: TPanel;
    DetailsGrid: TStringGrid;
    btnLoadAgeView: TSpeedButton;
    EditGroupID: TLabeledEdit;
    Action_RowHighlight: TMenuItem;
    N16: TMenuItem;
    Action_Update: TMenuItem;
    Action_Report: TMenuItem;
    N17: TMenuItem;
    OpenPopup: TPopupMenu;
    Action_AutoColumn: TMenuItem;
    Action_ExportTransactions: TMenuItem;
    Action_SelectAll: TMenuItem;
    N18: TMenuItem;
    Action_CopyToCB: TMenuItem;
    N19: TMenuItem;
    Action_ColumnWidth: TMenuItem;
    FollowupPopup: TTimer;
    custRISKA: TLabel;
    custRISKB: TLabel;
    custRISKC: TLabel;
    Action_FollowUpColors: TMenuItem;
    ShapeList2: TShape;
    SplitLine3: TBevel;
    imgEventLog: TImage;
    Text51: TLabel;
    sgGroups: TStringGrid;
    sgUAC: TStringGrid;
    ReportContainer: TPanel;
    Action_INF7_Filter: TMenuItem;
    Action_CoCode_Filter: TMenuItem;
    Action_Agent_Filter: TMenuItem;
    Action_Division_Filter: TMenuItem;
    Action_FollowUp_Filter: TMenuItem;
    Action_GroupFollowUp: TMenuItem;
    tcCOCODE2: TLabel;
    tcCOCODE3: TLabel;
    tcCOCODE4: TLabel;
    Action_INF4_Filter: TMenuItem;
    Action_Gr3_Filter: TMenuItem;
    TopPanel8: TPanel;
    RightPanel8: TPanel;
    PanelDetailsGrid: TPanel;
    Action_HideSummary: TMenuItem;
    Action_ExportCSV: TMenuItem;
    Action_SelectAgeView: TMenuItem;
    Action_CopyAll: TMenuItem;
    Action_RemoveFilters: TMenuItem;
    Action_Free1: TMenuItem;
    NavigationBar: TPanel;
    btnReport1: TSpeedButton;
    btnReport2: TSpeedButton;
    btnReport3: TSpeedButton;
    btnReport4: TSpeedButton;
    WebBrowser2: TWebBrowser;
    TextReport: TLabel;
    SeparateLine: TBevel;
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
    procedure Action_CloseClick(Sender: TObject);
    procedure sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
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
    procedure sgCoCodesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgPaidInfoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgPmtTermsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgPersonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgGroup3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgInvoiceTrackerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgOpenItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure Action_RowHighlightClick(Sender: TObject);
    procedure Action_ReportClick(Sender: TObject);
    procedure Action_ExportTransactionsClick(Sender: TObject);
    procedure Action_SelectAllClick(Sender: TObject);
    procedure Action_CopyToCBClick(Sender: TObject);
    procedure Action_AutoColumnClick(Sender: TObject);
    procedure Action_ColumnWidthClick(Sender: TObject);
    procedure FollowupPopupTimer(Sender: TObject);
    procedure Action_FollowUpColorsClick(Sender: TObject);
    procedure imgEventLogClick(Sender: TObject);
    procedure imgEventLogMouseEnter(Sender: TObject);
    procedure imgEventLogMouseLeave(Sender: TObject);
    procedure Action_INF7_FilterClick(Sender: TObject);
    procedure Action_CoCode_FilterClick(Sender: TObject);
    procedure Action_Agent_FilterClick(Sender: TObject);
    procedure Action_Division_FilterClick(Sender: TObject);
    procedure Action_FollowUp_FilterClick(Sender: TObject);
    procedure sgUACDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgUACMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgUACMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgGroupsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure Action_GroupFollowUpClick(Sender: TObject);
    procedure Action_INF4_FilterClick(Sender: TObject);
    procedure Action_Gr3_FilterClick(Sender: TObject);
    procedure sgAddressBookKeyPress(Sender: TObject; var Key: Char);
    procedure Action_HideSummaryClick(Sender: TObject);
    procedure Action_ExportCSVClick(Sender: TObject);
    procedure Action_SelectAgeViewClick(Sender: TObject);
    procedure Action_CopyAllClick(Sender: TObject);
    procedure Action_RemoveFiltersClick(Sender: TObject);
    procedure Action_Free1Click(Sender: TObject);
    procedure btnReport1Click(Sender: TObject);
    procedure btnReport2Click(Sender: TObject);
    procedure btnReport3Click(Sender: TObject);
    procedure btnReport4Click(Sender: TObject);
    { ------------------------------------------------------------- ! HELPERS ! ----------------------------------------------------------------------------- }
  private
    { GENERAL }
    var pAllowClose           :  boolean;
    var pStartTime            :  TTime;
    { GETTERS AND SETTERS FOR "FOLLOW-UP" COLORS SAVED IN SETTINGS FILE }
    function  GetTodayFColor  : TColor;
    function  GetTodayBColor  : TColor;
    function  GetPastFColor   : TColor;
    function  GetPastBColor   : TColor;
    function  GetFutureFColor : TColor;
    function  GetFutureBColor : TColor;
    procedure SetTodayFColor (NewColor: TColor);
    procedure SetTodayBColor (NewColor: TColor);
    procedure SetPastFColor  (NewColor: TColor);
    procedure SetPastBColor  (NewColor: TColor);
    procedure SetFutureFColor(NewColor: TColor);
    procedure SetFutureBColor(NewColor: TColor);
  public
    { GENERAL }
    var WinUserName         :  string;
    var EventLogPath        :  string;
    var DbConnect           :  TADOConnection;
    var GroupList           :  TLists;
    var GroupIdSel          :  string;
    var GroupNmSel          :  string;
    var AgeDateSel          :  string;
    var OSAmount            :  double;
    var GridPicture         :  TImage;
    var AccessLevel         :  string;
    var AccessMode          :  string;
    var OpenItemsUpdate     :  string;
    var ConnLastError       :  cardinal;
    { FOR "FOLLOW-UP" COLOR PICKER }
    property TodayFColor:  TColor read GetTodayFColor  write SetTodayFColor;
    property TodayBColor:  TColor read GetTodayBColor  write SetTodayBColor;
    property PastFColor:   TColor read GetPastFColor   write SetPastFColor;
    property PastBColor:   TColor read GetPastBColor   write SetPastBColor;
    property FutureFColor: TColor read GetFutureFColor write SetFutureFColor;
    property FutureBColor: TColor read GetFutureBColor write SetFutureBColor;
    { HELPER METHODS }
    procedure  DebugMsg(const Msg: String);
    procedure  ExecMessage(IsPostType: boolean; YOUR_INT: integer; YOUR_TEXT: string);
    function   OleGetStr(RecordsetField: variant): string;
    function   FindKey(INI: TMemIniFile; OpenedSection: string; KeyPosition: integer): string;
    function   WndCall(WinForm: TForm; Mode: integer): integer;
    function   MsgCall(WndType: integer; WndText: string): integer;
    procedure  LockSettingsPanel;
    function   ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
    function   GetCoCode(CoPos: integer; GroupId: string): string;
    procedure  Find(ColumnNum: integer);
    procedure  SwitchTimers(state: integer);
    procedure  LoadImageFromStream(Image: TImage; const FileName: string);
    function   CDate(StrDate: string): TDate;
    function   ShowReport(ReportNumber: cardinal): cardinal;
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

{ RELATED TO UNITYLIB.DLL }

TLogText              = procedure(filename: string; text: string); stdcall;
TMergeSort            = procedure(Grid: TStringgrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall;
TPrintf               = function(text: string; s: string): string; stdcall;
TGetOSVer             = function(mode: integer): string; stdcall;
TGetCurrentUserSid    = function: string stdcall;
TGetBuildInfoAsString = function: string stdcall;

const Assembly = 'Unitylib.dll';

function  Printf(Text: string; s: string): string; stdcall; external Assembly;
function  GetCurrentUserSid: string; stdcall; external Assembly;
function  GetOSVer(mode: integer): string; stdcall; external Assembly;
function  GetBuildInfoAsString: string; stdcall; external Assembly;
procedure LogText(filename: string; text: string); stdcall; external Assembly;
procedure MergeSort(grid: TStringgrid; var vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall; external Assembly;

const Win32API = 'wtsapi32.dll';

function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): Boolean; stdcall; external Win32API name 'WTSRegisterSessionNotification';
function WTSUnRegisterSessionNotification(hWnd: HWND):               Boolean; stdcall; external Win32API name 'WTSUnRegisterSessionNotification';

{$I Common.inc}

{ ------------------------------------------------------------- ! MAIN FORM REFERENCE ! --------------------------------------------------------------------- }
var
  MainForm :  TMainForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Filter, Tracker, Invoices, Actions, Calendar, About, Search, Worker, Model, Settings, Database, UAC, AgeView, Transactions, ReportBug, Colors, EventLog,
  uCEFApplication;

{$R *.dfm}

{ ----------------------------------------------------------------- ! DEBUGGING ! --------------------------------------------------------------------------- }
procedure TMainForm.DebugMsg(const Msg: String);
begin
  OutputDebugString(PChar(Msg));
end;

{ ################################################## ! GETTERS AND SETTERS FOR FOLLOW-UP COLORS ! ########################################################### }

{ --------------------------------------------------------------------------------------------------------------------------------------------------- GETTERS }

{ FONT COLOR }
function TMainForm.GetTodayFColor: TColor;
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    Result:=AppSet.TMIG.ReadInteger(FollowUpColors, 'TODAY_FCOLOR', 0);
  finally
    AppSet.Free;
  end;
end;

{ BACKGROUND COLOR }
function TMainForm.GetTodayBColor: TColor;
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    Result:=AppSet.TMIG.ReadInteger(FollowUpColors, 'TODAY_BCOLOR', 0);
  finally
    AppSet.Free;
  end;
end;

{ FONT COLOR }
function TMainForm.GetPastFColor: TColor;
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    Result:=AppSet.TMIG.ReadInteger(FollowUpColors, 'PAST_FCOLOR', 0);
  finally
    AppSet.Free;
  end;
end;

{ BACKGROUND COLOR }
function TMainForm.GetPastBColor: TColor;
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    Result:=AppSet.TMIG.ReadInteger(FollowUpColors, 'PAST_BCOLOR', 0);
  finally
    AppSet.Free;
  end;
end;

{ FONT COLOR }
function TMainForm.GetFutureFColor: TColor;
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    Result:=AppSet.TMIG.ReadInteger(FollowUpColors, 'FUTURE_FCOLOR', 0);
  finally
    AppSet.Free;
  end;
end;

{ BACKGROUND COLOR }
function TMainForm.GetFutureBColor: TColor;
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    Result:=AppSet.TMIG.ReadInteger(FollowUpColors, 'FUTURE_BCOLOR', 0);
  finally
    AppSet.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- SETTERS }

{ FONT COLOR }
procedure TMainForm.SetTodayFColor(NewColor: TColor);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    AppSet.TMIG.WriteInteger(FollowUpColors, 'TODAY_FCOLOR', NewColor);
    AppSet.Encode(AppConfig);
  finally
    AppSet.Free;
  end;
end;

{ BACKGROUND COLOR }
procedure TMainForm.SetTodayBColor(NewColor: TColor);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    AppSet.TMIG.WriteInteger(FollowUpColors, 'TODAY_BCOLOR', NewColor);
    AppSet.Encode(AppConfig);
  finally
    AppSet.Free;
  end;
end;

{ FONT COLOR }
procedure TMainForm.SetPastFColor(NewColor: TColor);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    AppSet.TMIG.WriteInteger(FollowUpColors, 'PAST_FCOLOR', NewColor);
    AppSet.Encode(AppConfig);
  finally
    AppSet.Free;
  end;
end;

{ BACKGROUND COLOR }
procedure TMainForm.SetPastBColor(NewColor: TColor);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    AppSet.TMIG.WriteInteger(FollowUpColors, 'PAST_BCOLOR', NewColor);
    AppSet.Encode(AppConfig);
  finally
    AppSet.Free;
  end;
end;

{ FONT COLOR }
procedure TMainForm.SetFutureFColor(NewColor: TColor);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    AppSet.TMIG.WriteInteger(FollowUpColors, 'FUTURE_FCOLOR', NewColor);
    AppSet.Encode(AppConfig);
  finally
    AppSet.Free;
  end;
end;

{ BACKGROUND COLOR }
procedure TMainForm.SetFutureBColor(NewColor: TColor);
var
  AppSet: TSettings;
begin
  AppSet:=TSettings.Create;
  try
    AppSet.TMIG.WriteInteger(FollowUpColors, 'FUTURE_BCOLOR', NewColor);
    AppSet.Encode(AppConfig);
  finally
    AppSet.Free;
  end;
end;

{ ############################################################## ! WINDOWS MESSAGES ! ####################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------- MESSAGE RECEIVER FOR WORKER THREADS }
procedure TMainForm.WndProc(var Msg: Messages.TMessage);
var
  DailyText:  TDataTables;
  CUID:       string;
  Condition:  string;
  CallEvent:  cardinal;
begin
  inherited;

  { ------------------------------------------------------------------------------------------------ INTERNAL MESSAGES BETWEEN WORKER THREADS AND MAIN THREAD }

  if Msg.Msg = WM_GETINFO then
  begin
    { DEBUG LINE }
    DebugMsg('WM_GETINFO RECEIVED');
    { SHOW MESSEGAE WINDOW | CALL FROM WORKER THREAD }
    if ( (Msg.WParam > 0) and (Msg.WParam <= 4) ) and (PChar(Msg.LParam) <> '') then MainForm.MsgCall(Msg.WParam, PChar(Msg.LParam));
    { POOL OF NUMBER TO BE USED 10..20 FOR OTHER ACTIONS }
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
    { DEBUG LINE }
    DebugMsg('WM_EXTINFO RECEIVED');
    { IF WPARAM EQUALS '14' THEN WE EXPECT LPARAM TO RETURN }
    { PHONE CALL DURATION FROM 'LYNCCALL.EXE' APPLICATION   }
    if Msg.WParam = 14 then
      { DEBUG LINE }
      DebugMsg(IntToStr(Msg.LParam));
      { LOG TIME IN SECONDS IN DATABASE }
      if Msg.LParam > 0 then
      begin
        DailyText:=TDataTables.Create(DbConnect);
        try
          DailyText.OpenTable(TblDaily);
          CUID:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1), MainForm.sgAgeView.Row];
          Condition:=TDaily.CUID + EQUAL + QuotedStr(CUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(AgeDateSel);
          DailyText.DataSet.Filter:=Condition;
          { UPDATE EXISTING COMMENT }
          if not (DailyText.DataSet.RecordCount = 0) then
          begin
            { GET AND SET CALL EVENT }
            CallEvent:=StrToIntDef(OleGetStr(DailyText.DataSet.Fields[TDaily.CALLEVENT].Value), 0);
            Inc(CallEvent);
            DailyText.CleanUp;
            { DEFINE COLUMNS, VALUES AND CONDITIONS }
            DailyText.Columns.Add(TDaily.STAMP);         DailyText.Values.Add(DateTimeToStr(Now));               DailyText.Conditions.Add(Condition);
            DailyText.Columns.Add(TDaily.USER_ALIAS);    DailyText.Values.Add(UpperCase(MainForm.WinUserName));  DailyText.Conditions.Add(Condition);
            DailyText.Columns.Add(TDaily.CALLEVENT);     DailyText.Values.Add(IntToStr(CallEvent));              DailyText.Conditions.Add(Condition);
            DailyText.Columns.Add(TDaily.CALLDURATION);  DailyText.Values.Add(IntToStr(Msg.LParam));             DailyText.Conditions.Add(Condition);
            { EXECUTE }
            DailyText.UpdateRecord(TblDaily);
          end
          else
          { INSERT NEW RECORD }
          begin
            DailyText.CleanUp;
            { DEFINE COLUMNS AND VALUES }
            DailyText.Columns.Add(TDaily.GROUP_ID);        DailyText.Values.Add(MainForm.GroupIdSel);
            DailyText.Columns.Add(TDaily.CUID);            DailyText.Values.Add(CUID);
            DailyText.Columns.Add(TDaily.AGEDATE);         DailyText.Values.Add(MainForm.AgeDateSel);
            DailyText.Columns.Add(TDaily.STAMP);           DailyText.Values.Add(DateTimeToStr(Now));
            DailyText.Columns.Add(TDaily.USER_ALIAS);      DailyText.Values.Add(UpperCase(MainForm.WinUserName));
            DailyText.Columns.Add(TDaily.EMAIL);           DailyText.Values.Add('0');
            DailyText.Columns.Add(TDaily.CALLEVENT);       DailyText.Values.Add('1');
            DailyText.Columns.Add(TDaily.CALLDURATION);    DailyText.Values.Add(IntToStr(Msg.LParam));
            DailyText.Columns.Add(TDaily.FIXCOMMENT);      DailyText.Values.Add('');
            DailyText.Columns.Add(TDaily.EMAIL_Reminder);  DailyText.Values.Add('0');
            DailyText.Columns.Add(TDaily.EMAIL_AutoStat);  DailyText.Values.Add('0');
            DailyText.Columns.Add(TDaily.EMAIL_ManuStat);  DailyText.Values.Add('0');
            { EXECUTE }
            DailyText.InsertInto(TblDaily);
          end;
        finally
          DailyText.Free;
        end;
      end;
  end;

  { --------------------------------------------------------------------------------------------------------------- CLOSE UNITY WHEN WINDOWS IS SHUTTING DOWN }

  { WINDOWS' QUERY FOR SHUTDOWN }
  if Msg.Msg = WM_QUERYENDSESSION then
  begin
    LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_QUERYENDSESSION. Windows is going to be shut down. Closing ' + APPNAME + '...');
    pAllowClose:=True;
    Msg.Result:=1;
  end;

  { WINDOWS IS SHUTTING DOWN }
  if Msg.Msg = WM_ENDSESSION then
  begin
    LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_ENDSESSION. Windows is shutting down...');
    pAllowClose:=True;
  end;

  { WINDOWS HAS RESUMED AFTER BEING SUSPENDED }
  if Msg.Msg = WM_POWERBROADCAST then
  begin
    { DISCONNECT FROM SERVER ON SUSPEND MODE EVENT }
    if Msg.WParam = PBT_APMSUSPEND then
    begin
      DbConnect.Connected:=False;
      ConnLastError:=404;
      LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_POWERBROADCAST with PBT_APMSUSPEND. Going into suspension, disconnecting from server.');
    end;
    { RESUME FROM SUSPEND MODE }
    if Msg.WParam = PBT_APMRESUMESUSPEND then
    begin
      LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_POWERBROADCAST. Windows has resumed after being suspended.');
    end;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------- WRAPPER FOR INTERNAL SEND/POST MESSAGE }
procedure TMainForm.ExecMessage(IsPostType: boolean; YOUR_INT: Integer; YOUR_TEXT: string);
begin
  if IsPostType     then PostMessage(MainForm.Handle, WM_GETINFO, YOUR_INT, LPARAM(PCHAR(YOUR_TEXT)));
  if not IsPostType then SendMessage(MainForm.Handle, WM_GETINFO, YOUR_INT, LPARAM(PCHAR(YOUR_TEXT)));
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
  with info do
  begin
    cbSize:=SizeOf(info);
    fmask :=SIF_ALL;
    GetScrollInfo(Self.Handle, SB_VERT, info);
    fMask :=fmask or SIF_PAGE;
    nPage :=Self.VisibleRowCount * (nmax - nmin) div Self.RowCount;
  end;
  SetScrollInfo(Self.Handle, SB_VERT, info, True);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- HORIZONTAL }
  with info do
  begin
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
      if iCNT = 0 then AppSettings.TMIG.WriteInteger(ColWidthName, ColPrefix + IntToStr(iCNT), 10);
      if iCNT > 0 then AppSettings.TMIG.WriteInteger(ColWidthName, ColPrefix + IntToStr(iCNT), Self.ColWidths[iCNT]);
    end;
    { SQL COLUMN NAME }
    for iCNT:=0 to Self.ColCount - 1 do AppSettings.TMIG.WriteString(ColOrderName, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 0]);
    { COLUMN TITLE }
    for iCNT:=0 to Self.ColCount - 1 do
    begin
      if iCNT = 0 then AppSettings.TMIG.WriteString(ColNames, ColPrefix + IntToStr(iCNT), '');
      if iCNT > 0 then AppSettings.TMIG.WriteString(ColNames, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 1]);
    end;
    { ENCODE }
    AppSettings.Encode(AppConfig);
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
  (* ARE USED TO BUILD SQL QUERY, THIS IS BECAUSE WE USE SQL EXPRESSIONS TO OBTAIN INITIAL OUTPUT    *)
  (* WITH FILTERING AND/OR SORTING ETC. SEPARATE FILTERING TO SOME EXTEND IS ALLOWED IN STRING GRID  *)
  (* HOWEVER, SEPARATE SORTING IS NOT IMPLEMENTED TO RESTRICT USER FORM "PLAYING AROUND"             *)
  (* THEREFORE, THERE IS ONE PLACE (SERVER) WHERE THERE IS DECIDED HOW TO DISPLAY DATA TO USER,      *)
  (* THIS IS PART OF AUTOMATION AND STANDARD APPROACH ACROSS ALL USERS, SO THE USER IS FORCED        *)
  (* IN CERTAIN DIRECTION BY AUTOMATION, AND THUS CAN OBTAIN BETTER RESULTS, ETC.                    *)

  { CHECK NUMBER OF KEYS IN GIVEN SECTION }
  Result:=False;
  ColWidthSec:=TStringList.Create;
  ColOrderSec:=TStringList.Create;
  ColNamesSec:=TStringList.Create;
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.ReadSection(ColWidthName, ColWidthSec);
    AppSettings.TMIG.ReadSection(ColOrderName, ColOrderSec);
    AppSettings.TMIG.ReadSection(ColNames, ColNamesSec);
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
            StrCol:=StrCol + AppSettings.TMIG.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ','
              else
                StrCol:=StrCol + AppSettings.TMIG.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ' ';

          { STORE SQL "COLUMN NAMES" AND "USER FRIENDLY NAMES" INTO HELPER ARRAY }
          Self.SqlColumns[iCNT, 0]:=AppSettings.TMIG.ReadString(ColOrderName, ColPrefix + IntToStr(iCNT), '');
          Self.SqlColumns[iCNT, 1]:=AppSettings.TMIG.ReadString(ColNames, ColPrefix + IntToStr(iCNT), '');

          { DISPLAY "USER FRIENDLY" COLUMN NAME }
          Self.Cells[iCNT, 0]:=AppSettings.TMIG.ReadString(ColNames, ColPrefix + IntToStr(iCNT), '');
        end;
        { ASSIGN SAVED WIDTH }
        Self.ColWidths[iCNT]:=AppSettings.TMIG.ReadInteger(ColWidthName, ColPrefix + IntToStr(iCNT), 100);
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
  DataTables:=TDataTables.Create(MainForm.DbConnect);
  try
    { EXECUTE STORED PROCEDURE }
    DataTables.StrSQL:=EXECUTE + AgeViewExport + SPACE +
                       QuotedStr(MainForm.GroupList[MainForm.GroupListBox.ItemIndex, 0]) + COMMA +
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
          LogText(MainForm.EventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data has been successfully transferred to Excel.');
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
          LogText(MainForm.EventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data cannot be exported because Excel cannot be found.');
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported because Excel cannot be found.')));
        end else
        { GENERIC MESSAGE }
        begin
          LogText(MainForm.EventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data cannot be exported, error message has been thrown: ' + E.Message + '.');
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported. Description received: ' + E.Message)));
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
    MainForm.ExecMessage(True, 10, stImportCSV);
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
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: CSV Import has failed: ' + ExtractFileName(fPath));
          SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('CSV Import has failed. Please check the file and try again.')));
          IsError:=True;
        end;
      end;
    { ----------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
    finally
      MainForm.ExecMessage(True, 10, stReady);
      if not IsError then
      begin
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Data has been imported successfully!');
        MainForm.ExecMessage(False, 1, 'Data has been imported successfully!');
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
    MainForm.ExecMessage(True, 10, stExportCSV);
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
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Cannot saved file: ' + ExtractFileName(fPath));
        SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('Cannot save the file in the given location.')));
        IsError:=True;
      end;
    end;
  { ------------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
  finally
    if not IsError then
    begin
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Data has been exported successfully!');
      SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Address Book have been exported successfully!')));
    end;
    CSVData.Free;
    MainForm.ExecMessage(True, 10, stReady);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SELECT ALL }
procedure TStringGrid.SelectAll;
var
  GridRect: TGridRect;
begin
  GridRect.Left  :=1;
  GridRect.Top   :=1;
  GridRect.Right :=Self.ColCount;
  GridRect.Bottom:=Self.RowCount;
  Self.Selection :=GridRect;
end;

{ ################################################################## ! HELPERS ! ############################################################################ }

{ ----------------------------------------------------------------------------------------------------------------------------------------- CONVERT TO STRING }
function TMainForm.OleGetStr(RecordsetField: variant): string;
begin
  {$D-}
  try
    OleGetStr:=RecordsetField;
  except
    OleGetStr:=VarToStr(RecordsetField);  { CASE OF NULL FIELD }
  end;
  {$D+}
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
  if Mode = stModal    then Result:=WinForm.ShowModal;
  if Mode = stModeless then WinForm.Show;
end;

{ --------------------------------------------------------------------------------------------------------------------------- WRAPPER FOR WINDOWS MESSAGE BOX }
function TMainForm.MsgCall(WndType: integer; WndText: string): integer;
begin
  Result:=0;
  if WndText = '' then Exit;
  if WndType = mcInfo      then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OK       + MB_ICONINFORMATION);
  if WndType = mcWarn      then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OK       + MB_ICONWARNING);
  if WndType = mcError     then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OK       + MB_ICONERROR);
  if WndType = mcQuestion1 then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_OKCANCEL + MB_ICONQUESTION);
  if WndType = mcQuestion2 then Result:=Application.MessageBox(PChar(WndText), PChar(APPNAME), MB_YESNO    + MB_ICONQUESTION);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- RESET SETTINGS }
procedure TMainForm.LockSettingsPanel;
begin
  { VISIBLE ON }
  imgOFF.Visible         :=True;
  btnPassUpdate.Enabled  :=False;
  { EDIT BOXES }
  Edit_CurrPassWd.Enabled:=False;
  Edit_NewPassWd.Enabled :=False;
  Edit_ConfPassWd.Enabled:=False;
  Edit_CurrPassWd.Text   :='';
  Edit_NewPassWd.Text    :='';
  Edit_ConfPassWd.Text   :='';
  Edit_PASSWORD.Text     :='';
  { STRING GRIDS }
  sgListSection.ClearAll(2, 0, 0, False);
  sgListValue.ClearAll(2, 0, 0, False);
  sgListSection.Row:=1;
  sgListValue.Row:=1;
  sgUAC.ClearAll(2, 0, 0, False);
  sgGroups.ClearAll(2, 0, 0, False);
  sgUAC.Row:=1;
  sgGroups.Row:=1;
  sgListSection.Enabled:=False;
  sgListValue.Enabled  :=False;
  sgUAC.Enabled        :=False;
  sgGroups.Enabled     :=False;
  { ENDING }
  btnUnlock.Caption:='Unlock';
  Edit_PASSWORD.SetFocus;
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

  { CONVERTS FROM    }
  {  1. 2020 TO 2020 }
  {  2. 340  TO 0340 }
  {  3. 43   TO 0043 }
  {  4. 5    TO 0005 }
  if mode = 3 then
  begin
    if Length(CoNumber) = 4 then Result:=CoNumber;
    if Length(CoNumber) = 3 then Result:='0'    + CoNumber;
    if Length(CoNumber) = 2 then Result:='00'   + CoNumber;
    if Length(CoNumber) = 1 then Result:='000'  + CoNumber;
  end;

end;

{ -------------------------------------------------------------------------------------------------------------------- RETURN SPECIFIC 'COCOE' FROM THE GROUP }
function TMainForm.GetCoCode(CoPos: integer; GroupId: string): string;
{ WARNING! GROUP ID FORMAT: SERIES OF 4 GROUPS OF 5 DIGITS, I.E.: '020470034000043' MUST BE READ AS FOLLOWS: }
{   1. 1ST CO CODE: 02047 (2047)                                                                             }
{   2. 2ND CO CODE: 00340 (340)                                                                              }
{   3. 3RD CO CODE: 00043 (43)                                                                               }
{   4. 4TH CO CODE: 00000 (0)                                                                                }
begin
  if CoPos = 1 then Result:=IntToStr(StrToInt(MidStr(GroupId, 1,  5)));
  if CoPos = 2 then Result:=IntToStr(StrToInt(MidStr(GroupId, 6,  5)));
  if CoPos = 3 then Result:=IntToStr(StrToInt(MidStr(GroupId, 11, 5)));
  if CoPos = 4 then Result:=IntToStr(StrToInt(MidStr(GroupId, 16, 5)));
end;

{ ----------------------------------------------------------------------------------------------------------------- FIND OTHER DETAILS FOR GIVEN COMPANY CODE }
procedure TMainForm.Find(ColumnNum: integer);
var
  iCNT:  integer;
begin
  for iCNT:=1 to sgCoCodes.RowCount - 1 do
  begin
    if DetailsGrid.Cells[ColumnNum, 0] = sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.CO_CODE, 1, 1), iCNT] then
    begin
      DetailsGrid.Cells[ColumnNum, 1]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.COCURRENCY,    1, 1), iCNT];
      DetailsGrid.Cells[ColumnNum, 2]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.DIVISIONS,     1, 1), iCNT];
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

{ ------------------------------------------------------------------------------------------------------------------------------------- TURN ON OR OFF TIMERS }
procedure TMainForm.SwitchTimers(state: integer);
begin
  { ENABLE ALL CHECKERS }
  if state = tmEnabled then
  begin
    InvoiceScanTimer.Enabled :=True;
    FollowupPopup.Enabled    :=True;
    OILoader.Enabled         :=True;
  end;
  { DISABLE ALL CHECKERS }
  if state = tmDisabled then
  begin
    InvoiceScanTimer.Enabled :=False;
    FollowupPopup.Enabled    :=False;
    OILoader.Enabled         :=False;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------- LOAD ANY IMAGE FORMAT TO TIMAGE }
procedure TMainForm.LoadImageFromStream(Image: TImage; const FileName: string);
var
  WIC: TWICImage;
  FS:  TFileStream;
begin
  FS:=TFileStream.Create(FileName, fmOpenRead);
  FS.Position:=0;
  WIC:=TWICImage.Create;
  try
    WIC.LoadFromStream(FS);
    Image.Picture.Assign(WIC);
  finally
    WIC.Free;
    FS.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------ CONVERT STRING DATE TO DATE FORMAT }
function TMainForm.CDate(StrDate: string): TDate;
begin
    Result:=StrToDateDef(StrDate, NULLDATE);
end;

{ --------------------------------------------------------------------------------------------------------------------- EXECUTE CHROMIUM READER FOR REPORTING }
function TMainForm.ShowReport(ReportNumber: cardinal): cardinal;
var
  AppSettings: TSettings;
  AppParam:    string;
begin
  AppSettings:=TSettings.Create;
  try
    AppParam:=AppSettings.TMIG.ReadString(ApplicationDetails, 'REPORT_Report' + IntToStr(ReportNumber), 'about:blank');
    Result:=ShellExecute(MainForm.Handle, seOpen, PChar(UnityReader), PChar(AppParam), nil, SW_SHOWNORMAL);
  finally
    AppSettings.Free;
  end;
end;

{ ############################################################## ! MAIN THREAD EVENTS ! ##################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TMainForm.FormCreate(Sender: TObject);
var
  AppVersion:    string;
  AppSettings:   TSettings;
  DataBase:      TDataBase;
  DataTables:    TDataTables;
  UserControl:   TUserControl;
  Transactions:  TTransactions;
  NowTime:       TTime;
begin

  { ------------------------------------------------------------ ! INITIALIZATION ! ------------------------------------------------------------------------- }

  AppVersion :=GetBuildInfoAsString;
  pAllowClose:=False;

  { --------------------------------------------------------- ! READ SETTINGS AND INITIALIZE ! -------------------------------------------------------------- }

  AppSettings :=TSettings.Create;
  try

    { USER NAME AND EVENT LOG PATH }
    WinUserName :=AppSettings.FWinUserName;
    EventLogPath:=AppSettings.FPathEventLog;

    { ------------------------------------------------------ ! DATABASE INITIALIZATION & UAC ! -------------------------------------------------------------- }

    { ------------------------------------------------------------------------------------------------------------------------- ESTABLISH ACTIVE CONNECTIVITY }
    DbConnect:=TADOConnection.Create(MainForm);
    DataBase :=TDataBase.Create(True);
    try
      ConnLastError:=DataBase.Check;
      if ConnLastError = 0 then
      begin
        DataBase.InitializeConnection(MainThreadID, True, DbConnect);
        MainForm.InvoiceScanTimer.Enabled:=True;
        MainForm.OILoader.Enabled        :=True;
      end
      else
      begin
        MainForm.InvoiceScanTimer.Enabled:=False;
        MainForm.OILoader.Enabled        :=False;
      end;
    finally
      DataBase.Free;
    end;

    { ----------------------------------------------------------------------------------------------------------------------------------- UPLOAD USER DETAILS }
    UserControl:=TUserControl.Create(DbConnect);
    try
      EventLogPath        :=AppSettings.FPathEventLog;
      UserControl.UserName:=WinUserName;
      AccessLevel         :=UserControl.GetAccessData(adAccessLevel);

      { IF USERNAME IS NOT FOUND, THEN CLOSE APPLICATION }
      if AccessLevel = '' then
      begin
        MsgCall(mcError, 'Cannot find account for user alias: ' + UpperCase(WinUserName) + '. Please contact your administrator. Application will be closed.');
        ExitProcess(0);
      end;

      { OTHERWISE PROCESS }
      AccessMode:=UserControl.GetAccessData(adAccessMode);
      if AccessMode = adAccessFull  then Action_FullView.Checked :=True;
      if AccessMode = adAccessBasic then Action_BasicView.Checked:=True;
      UserControl.GetGroupList(GroupList, GroupListBox);
      UserControl.GetAgeDates(GroupListDates, GroupList[0, 0]);

      (* NOTE: REPLACE BELOW WITH USER MATRIX *)

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
        Action_Tracker.Enabled        :=False;
        Action_AddToBook.Enabled      :=False;
        ActionsForm.DailyCom.Enabled  :=False;
        ActionsForm.GeneralCom.Enabled:=False;
        btnSave.Enabled               :=False;
      end;

    finally
      UserControl.Free;
    end;

    { -------------------------------------------------- ! LOAD IMAGE FOR STRING GRID CELLS ! --------------------------------------------------------------- }

    GridPicture:=TImage.Create(MainForm);
    GridPicture.SetBounds(0, 0, 16, 16);
    LoadImageFromStream(GridPicture, AppSettings.FPathGridImage);

    { ----------------------------------------------------- ! APPLICATION NAME | CAPTION ! ------------------------------------------------------------------ }

    MainForm.Caption :=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_MAIN', APPNAME);
    GroupName.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'GROUP_NAME', 'n/a');

    { --------------------------------------------------------- ! WINDOW POSITION ! ------------------------------------------------------------------------- }

    MainForm.DefaultMonitor:=dmDesktop;          (* DO NOT CHANGE THAT *)
    MainForm.Position      :=poDefaultSizeOnly;  (* DO NOT CHANGE THAT *)
    MainForm.Top           :=AppSettings.TMIG.ReadInteger(ApplicationDetails, 'WINDOW_TOP',  0);
    MainForm.Left          :=AppSettings.TMIG.ReadInteger(ApplicationDetails, 'WINDOW_LEFT', 0);

    { --------------------------------------------------------------------------------------------------------------------------- START WEB PAGE | UNITY INFO }

    WebBrowser1.Navigate(WideString(AppSettings.TMIG.ReadString(ApplicationDetails, 'START_PAGE',  'about:blank')), $02);
    WebBrowser2.Navigate(WideString(AppSettings.TMIG.ReadString(ApplicationDetails, 'REPORT_PAGE', 'about:blank')), $02);

    { -------------------------------------------------------- ! TIMERS INTERVALS ! ------------------------------------------------------------------------- }

    (* 'INETTIMER' IS EXCLUDED FROM BELOW LIST BECAUSE IT IS CONTROLED BY 'INITIAIZECONNECTION' METHOD *)

    InvoiceScanTimer.Interval:=AppSettings.TMIG.ReadInteger(TimersSettings, 'INVOICE_SCANNER', 900000);  { DEFAULT VALUE 900000  MILISECONDS = 15 MINUTES }
    FollowupPopup.Interval   :=AppSettings.TMIG.ReadInteger(TimersSettings, 'FOLLOWUP_CHECKER',1800000); { DEFAULT VALUE 1800000 MILISECONDS = 30 MINUTES }
    OILoader.Interval        :=AppSettings.TMIG.ReadInteger(TimersSettings, 'OI_LOADER',       300000);  { DEFAULT VALUE 3000000 MILISECONDS = 5  MINUTES }

    { ---------------------------------------------------------- ! RISK CLASSES ! --------------------------------------------------------------------------- }

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

    { ----------------------------------------------------------- ! TAB SHEETS ! ---------------------------------------------------------------------------- }

    TabSheet1.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB1', 'TAB1');
    TabSheet2.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB2', 'TAB2');
    TabSheet3.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB3', 'TAB3');
    TabSheet4.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB4', 'TAB4');
    TabSheet5.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB5', 'TAB5');
    TabSheet6.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB6', 'TAB6');
    TabSheet7.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB7', 'TAB7');
    TabSheet8.Caption:=AppSettings.TMIG.ReadString(TabSheetsNames, 'TAB8', 'TAB8');

    { ----------------------------------------------------- ! CAPTIONS FOR ALL SHAPES ! --------------------------------------------------------------------- }

    { AGING REPORT | TABSHEET1 }
    Cap01.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT01', 'EMPTY'), [fsBold]);
    Cap02.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT02', 'EMPTY'), [fsBold]);
    Cap03.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT03', 'EMPTY'), [fsBold]);
    Cap05.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT05', 'EMPTY'), [fsBold]);
    Cap06.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT06', 'EMPTY'), [fsBold]);
    Cap07.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS1TXT07', 'EMPTY'), [fsBold]);

    { OPEN ITEMS | TABSHEET2 }
    Cap10.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS2TXT01', 'EMPTY'), [fsBold]);
    Cap11.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS2TXT02', 'EMPTY'), [fsBold]);
    Cap12.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS2TXT03', 'EMPTY'), [fsBold]);

    { ADDRESS BOOK | TABSHEET3 }
    Cap13.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS3TXT01', 'EMPTY'), [fsBold]);

    { INVOICE TRACKER | TABSHEET4 }
    Cap43.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS4TXT01', 'EMPTY'), [fsBold]);

    { GENERAL TABLES  | TABSHEET7 }
    Cap15.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT01', 'EMPTY'), [fsBold]);
    Cap16.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT02', 'EMPTY'), [fsBold]);
    Cap17.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT03', 'EMPTY'), [fsBold]);
    Cap18.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT04', 'EMPTY'), [fsBold]);
    Cap19.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT05', 'EMPTY'), [fsBold]);
    Cap20.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS7TXT06', 'EMPTY'), [fsBold]);

    { SETTINGS | TABSHEET8 }
    Cap21.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT01', 'EMPTY'), [fsBold]);
    Cap22.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT02', 'EMPTY'), [fsBold]);
    Cap23.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT03', 'EMPTY'), [fsBold]);
    Cap27.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS8TXT04', 'EMPTY'), [fsBold]);

    { UNIDENTIFIED TRANSACTIONS | TABSHEET6 }
    Cap61.ShapeText(10, 1, AppSettings.TMIG.ReadString(TabSheetsCaps, 'TS6TXT01', 'EMPTY'), [fsBold]);

    { ------------------------------------------------------------ ! MAIN VIEW ! ---------------------------------------------------------------------------- }

    { ------------------------------------------------------------------------------------------------------------------------------- AGING BUCKETS | CAPTION }

    tR1.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE1A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE1B','');
    tR2.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE2A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE2B','');
    tR3.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE3A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE3B','');
    tR4.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE4A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE4B','');
    tR5.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE5A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE5B','');
    tR6.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE6A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE6B','');

    { -------------------------------------------------------------------------------------------------------------------------------- SUMMARY BOX | CAPTIONS }

    Text21.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE1A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE3B','') + ':';
    Text22.Caption:=AppSettings.TMIG.ReadString(AgingRanges,'RANGE4A','') + ' - ' + AppSettings.TMIG.ReadString(AgingRanges,'RANGE6B','') + ':';

  finally
    AppSettings.Free;
  end;

  { ------------------------------------------------------------- ! DATE & TIME ! --------------------------------------------------------------------------- }

  NowTime   :=Now;
  pStartTime:=Now;
  FormatDateTime('hh:mm:ss', NowTime);
  FormatDateTime('hh:mm:ss', pStartTime);

  { ------------------------------------------------------------- ! STATUS BAR ! ---------------------------------------------------------------------------- }

  StatBar_TXT1.Caption:=stReady;
  StatBar_TXT2.Caption:=WinUserName;
  StatBar_TXT3.Caption:=DateToStr(Now);

  { ----------------------------------------------------------- ! DEFAULT VALUES ! -------------------------------------------------------------------------- }

  Edit_PASSWORD.Text   :='';
  Edit_CurrPassWd.Text :='';
  Edit_NewPassWd.Text  :='';
  Edit_ConfPassWd.Text :='';
  btnPassUpdate.Enabled:=False;
  MyPages.ActivePage   :=TabSheet1;

  { --------------------------------------------------------- ! READ DEFAULT AGE VIEW ! --------------------------------------------------------------------- }

  if (GroupListBox.Text <> '') and (GroupListDates.Text <> '') then
  begin
    GroupIdSel:=GroupList[GroupListBox.ItemIndex, 0];
    GroupNmSel:=GroupList[GroupListBox.ItemIndex, 1];
    AgeDateSel:=GroupListDates.Text;
    sgAgeView.Enabled:=True;
    Transactions:=TTransactions.Create(DbConnect);
    try
      OpenItemsUpdate:=Transactions.GetDateTime(gdDateTime);
      if OpenItemsUpdate = '' then
      begin
        MsgCall(mcWarn, 'Cannot find open items in database. Please contact IT support.');
        TTReadAgeView.Create(thNullParameter);
      end
        else
          TTReadAgeView.Create(thCallOpenItems);
    finally
      Transactions.Free;
    end;
  end;

  { ------------------------------------------------------------ ! GENERAL TABLES ! ------------------------------------------------------------------------- }

  DataTables:=TDataTables.Create(DbConnect);
  try
    { SELECTED COLUMNS }
    DataTables.CleanUp;
    DataTables.Columns.Add(TCompany.CO_CODE);
    DataTables.Columns.Add(TCompany.BRANCH);
    DataTables.Columns.Add(TCompany.CONAME);
    DataTables.Columns.Add(TCompany.COADDRESS);
    DataTables.Columns.Add(TCompany.VATNO);
    DataTables.Columns.Add(TCompany.DUNS);
    DataTables.Columns.Add(TCompany.COUNTRY);
    DataTables.Columns.Add(TCompany.CITY);
    DataTables.Columns.Add(TCompany.FMANAGER);
    DataTables.Columns.Add(TCompany.Telephone);
    DataTables.Columns.Add(TCompany.COTYPE);
    DataTables.Columns.Add(TCompany.COCURRENCY);
    DataTables.Columns.Add(TCompany.INTEREST_RATE);
    DataTables.Columns.Add(TCompany.KPI_OVERDUE_TARGET);
    DataTables.Columns.Add(TCompany.KPI_UNALLOCATED_TARGET);
    DataTables.Columns.Add(TCompany.AGENTS);
    DataTables.Columns.Add(TCompany.DIVISIONS);
    { READ }
    DataTables.OpenTable(TblCompany);
    DataTables.DataSet.Sort:=TCompany.CO_CODE + ASC;
    DataTables.SqlToGrid(sgCoCodes,  DataTables.DataSet, False, True);
    { READ BEOW TABLES "AS IS" }
    DataTables.CleanUp; DataTables.OpenTable(TblPmtterms); DataTables.SqlToGrid(sgPmtTerms, DataTables.DataSet, False, True);
    DataTables.CleanUp; DataTables.OpenTable(TblPaidinfo); DataTables.SqlToGrid(sgPaidInfo, DataTables.DataSet, False, True);
    DataTables.CleanUp; DataTables.OpenTable(TblGroup3);   DataTables.SqlToGrid(sgGroup3,   DataTables.DataSet, False, True);
    DataTables.CleanUp; DataTables.OpenTable(TblPerson);   DataTables.SqlToGrid(sgPerson,   DataTables.DataSet, False, True);
  finally
    DataTables.Free;
  end;

  { -------------------------------------------------------------------------------------------------------------------------- APPLICATION VERSION & USER SID }

  LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Application version = ' + AppVersion);
  LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User SID = ' + GetCurrentUserSid);

  { TIME ON STATBAR }
  UpTime.Enabled     :=True;
  CurrentTime.Enabled:=True;

  { START CHECKERS }
  SwitchTimers(tmEnabled);

end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ ON DESTROY }
procedure TMainForm.FormDestroy(Sender: TObject);
var
  AppSettings: TSettings;
begin
  { RELEASE "TWEBBROWSER" COMPONENT MANUALLY }
  WebBrowser1.Free;
  WebBrowser2.Free;
  { CLOSE DB CONNECTION }
  DbConnect.Close;
  { SAVE AGE VIEW LAYOUT }
  if sgAgeView.RowCount > 2 then sgAgeView.SaveLayout(ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);
  { SAVE OTHER SETTINGS }
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.WriteInteger(ApplicationDetails, 'WINDOW_TOP',   MainForm.Top);
    AppSettings.TMIG.WriteInteger(ApplicationDetails, 'WINDOW_LEFT',  MainForm.Left);
    if MainForm.WindowState = wsNormal    then AppSettings.TMIG.WriteString(ApplicationDetails,  'WINDOW_STATE', 'wsNormal');
    if MainForm.WindowState = wsMaximized then AppSettings.TMIG.WriteString(ApplicationDetails,  'WINDOW_STATE', 'wsMaximized');
    if MainForm.WindowState = wsMinimized then AppSettings.TMIG.WriteString(ApplicationDetails,  'WINDOW_STATE', 'wsMinimized');
    AppSettings.Encode(AppConfig);
    LogText(EventLogPath, 'Application closed.');
  finally
    AppSettings.Free;
  end;
end;

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
  sgAgeView.AutoThumbSize;
  sgOpenItems.AutoThumbSize;
  sgAddressBook.AutoThumbSize;
  sgInvoiceTracker.AutoThumbSize;
  sgCoCodes.AutoThumbSize;
  sgPmtTerms.AutoThumbSize;
  sgPaidInfo.AutoThumbSize;
  sgPerson.AutoThumbSize;
  sgGroup3.AutoThumbSize;
  sgListSection.AutoThumbSize;
  sgListValue.AutoThumbSize;
  sgUAC.AutoThumbSize;
  sgGroups.AutoThumbSize;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- ON CLOSE QUERY }
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { GO MINIMIZE AND HIDE FROM TASKBAR | DO NOT CLOSE }
  if not pAllowClose then
  begin
    CanClose:=False;
    ShowWindow(Handle, SW_MINIMIZE);
    Hide();
  end
  else
  { SHUTDOWN APPLICATION }
  begin
    CanClose:=True;
  end;
end;

{ ------------------------------------------------------------------ ! TIMERS ! ----------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------- COUNT CURRENT FOLLOW-UP'S }
procedure TMainForm.FollowupPopupTimer(Sender: TObject);
var
  iCNT: integer;
  Sum:  integer;
begin
  { COUNT ALL TODAY'S FOLLOW UPS }
  Sum:=0;
  for iCNT:=1 to sgAgeView.RowCount - 1 do
    if
      (
        CDate(sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneral.fFOLLOWUP, 1, 1), iCNT]) = CDate(StatBar_TXT3.Caption)
      )

      and

      (
        UpperCase(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.INF7, 1, 1), iCNT]) = UpperCase(WinUserName)
      )

      then Inc(Sum);
  { DISPLAY IN TRAY BALOON }
  if not (Sum = 0) then
  begin
    TrayIcon.Visible:=True;
    TrayIcon.BalloonHint:='Hello, you have ' + IntToStr(Sum) + ' follow-up dates registered for today.' + CRLF +
                          'Let''s bother some customers and collect some money money!' + CRLF;
    TrayIcon.ShowBalloonHint;
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
  LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Calling open items scanner...');
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
  Result:=Now - pStartTime;
  StatBar_TXT5.Caption:=TimeToStr(Result);
end;

{ ---------------------------------------------------------------- ! POPUP MENUS ! -------------------------------------------------------------------------- }

{ -------------------------------------------------------------- ! OPEN ITEMS MENU ! ------------------------------------------------------------------------ }

{ --------------------------------------------------------------------------------------------------------------------------------- EXPORT ENTIRE GRID TO CSV }
procedure TMainForm.Action_ExportTransactionsClick(Sender: TObject);
begin
  sgOpenItems.ExportCSV(CSVExport, '|');
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SELECT ALL }
procedure TMainForm.Action_SelectAllClick(Sender: TObject);
begin
  sgOpenItems.SelectAll;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- COPY TO CLIPBOARD }
procedure TMainForm.Action_CopyToCBClick(Sender: TObject);
begin
  sgOpenItems.CopyCutPaste(adCopy);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ SET COLUMN WIDTH }
procedure TMainForm.Action_AutoColumnClick(Sender: TObject);
begin
  sgOpenItems.SetColWidth(10, 20);
end;

{ ------------------------------------------------------------- ! ADDRESS BOOK MENU ! ----------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK CONTEXT MENU }
procedure TMainForm.BookPopupPopup(Sender: TObject);
begin

  Action_ShowMyEntries.Caption:='Show ' + UpperCase(MainForm.WinUserName) + ' entries';

  { CHECK IF USER SELECT A RANGE }
  if (sgAddressBook.Selection.Bottom - sgAddressBook.Selection.Top) > 0 then
    { WE ALLOW TO DELETE ONLY ONE LINE AT THE TIME }
    Action_DelRow.Enabled:=False
      else
        Action_DelRow.Enabled:=True;

  { DISABLE "CUT" AND "PASTE" IF SELECTED ALREADY SAVED ITEM }
  if sgAddressBook.Cells[0, sgAddressBook.Row] <> '' then
  begin
    Action_Cut.Enabled  :=False;
    Action_Paste.Enabled:=False;
  end
  else
  begin
    Action_Cut.Enabled  :=True;
    Action_Paste.Enabled:=True;
  end;

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
  sgAddressBook.Cells[1, sgAddressBook.RowCount - 1]:=MainForm.WinUserName;
  for jCNT:=2 to sgAddressBook.ColCount - 1 do sgAddressBook.Cells[jCNT, sgAddressBook.RowCount - 1]:='';
end;

{ --------------------------------------------------------------------------------------------------------------------------------- DELETE GIVEN CUID FROM DB }
procedure TMainForm.Action_DelRowClick(Sender: TObject);
var
  DataTables: TDataTables;
begin
  { ASK BEFORE DELETE }
  if MsgCall(mcQuestion2, 'Are you sure you want to delete this customer?' + CRLF + 'This operation cannot be reverted.') = IDNO then Exit;
  { EXECUTE DELETE QUERY }
  if sgAddressBook.Cells[0, sgAddressBook.Row] <> '' then
  begin
    DataTables:=TDataTables.Create(DbConnect);
    try
      DataTables.StrSQL:=DELETE_FROM          +
                           TblAddressbook     +
                         WHERE                +
                           TAddressBook.SCUID +
                         EQUAL                +
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
  SearchForm.SColName  :=TAddressBook.CUSTOMER_NAME;
  SearchForm.SColNumber:=TAddressBook.CUSTOMER_NUMBER;
  WndCall(SearchForm, stModal);
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

{ ------------------------------------------------------------------------------------------------------------------------------------------ SET COLUMN WIDTH }
procedure TMainForm.Action_ColumnWidthClick(Sender: TObject);
begin
  sgAddressBook.SetColWidth(40, 10);
end;

{ -------------------------------------------------------------- ! MAIN FORM MENU ! ------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW WINDOW }
procedure TMainForm.Action_ShowAppClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_NORMAL);
  Show();
  Application.BringToFront;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- HIDE WINDOW }
procedure TMainForm.Action_HideAppClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_MINIMIZE);
  Hide();
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

{ ---------------------------------------------------------------------------------------------------------------------------------------------- REPORT A BUG }
procedure TMainForm.Action_ReportClick(Sender: TObject);
begin
  WndCall(ReportForm, stModal);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ HELP }
procedure TMainForm.Action_HelpClick(Sender: TObject);
begin
  (* EXECUTE EXTERNAL PDF FILE *)
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- ABOUT }
procedure TMainForm.Action_AboutClick(Sender: TObject);
begin
  WndCall(AboutForm, stModal);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- CLOSE }
procedure TMainForm.Action_CloseClick(Sender: TObject);
begin
  (* DO NOTHING *)
end;

{ ----------------------------------------------------------------- ! AGE VIEW ! ---------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- EXECUTE WHEN MENU OPENS }
procedure TMainForm.AgeViewPopupPopup(Sender: TObject);
begin

  { ONLY ADMINS AND RW USERS CAN USE ADDRESSBOOK AND INVOICE TRACKER }
  if AccessLevel = acReadOnly then Exit;

  { ENABLE OR DISABLE FILTER REMOVAL }
  if FilterForm.InUse then
    Action_RemoveFilters.Enabled:=True
      else
        Action_RemoveFilters.Enabled:=False;

  { CHECK IF USER SELECT A RANGE ON AGEGRID }
  if (sgAgeView.Selection.Bottom - sgAgeView.Selection.Top) > 0 then
  begin
    { WE CAN ADD THE SAME FOLLOW UP ONLY TO A SELECTED GROUP }
    Action_GroupFollowUp.Enabled:=True;
  end
  else
  begin
    Action_GroupFollowUp.Enabled:=False;
  end;

end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- LYNC CALL }
procedure TMainForm.Action_LyncCallClick(Sender: TObject);
begin
  if ConnLastError = 0 then
  begin
    if MainForm.StatBar_TXT1.Caption = stReady then
      WndCall(ActionsForm, stModal)
        else
          MainForm.MsgCall(mcWarn, 'Wait until "Ready" status and try again.');
  end
      else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

{ ---------------------------------------------------------------------------------------------------------------------- ADD TO INVOICE TRACKER | WINDOW CALL }
procedure TMainForm.Action_TrackerClick(Sender: TObject);
var
  iCNT: integer;
  Item: TListItem;
begin
  if ConnLastError = 0 then
  begin
    TrackerForm.CustomerList.Clear;
    { ONE CUSTOMER HAS BEEN SELECTED }
    if (sgAgeView.Selection.Top - sgAgeView.Selection.Bottom) = 0 then
    begin
      Item:=TrackerForm.CustomerList.Items.Add;
      Item.Caption:='1';
      Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1),           sgAgeView.Row]);
      Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), sgAgeView.Row]);
    end
    { MANY CUSTOMERS HAS BEEN SELECTED }
    else
    begin
      for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
      begin
        if sgAgeView.RowHeights[iCNT] <> - 1 then
        begin
          Item:=TrackerForm.CustomerList.Items.Add;
          Item.Caption:=IntToStr(iCNT);
          Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1),           iCNT]);
          Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), iCNT]);
        end;
      end;
    end;
    { OPEN FORM }
    WndCall(TrackerForm, stModal);
  end
  else
  begin
    MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------ ADD SELECTED ITEMS TO ADDRESS BOOK }
procedure TMainForm.Action_AddToBookClick(Sender: TObject);
var
  iCNT:    integer;
  OffSet:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  if sgAddressBook.Cells[0, 1] = '' then OffSet:=-1 else OffSet:=0;
  { ----------------------------------------------------------------------------------------------------------------------- GO ONE BY ONE AND LOOK FOR 'CUID' }
  for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
  begin
    if sgAgeView.RowHeights[iCNT] <> - 1 then
    begin
      { EMPTY ID BAR }
      sgAddressBook.Cells[0, sgAddressBook.RowCount - 1 + OffSet]:='';
      { MANDATORY FIELDS }
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.USER_ALIAS, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:=UpperCase(MainForm.WinUserName);
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.SCUID, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:=
        sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), iCNT] +
          ConvertName(
                       sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCO_CODE, 1, 1), iCNT],
                       'F',
                       3
                     );
      { FIELDS WITH DATA }
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.CUSTOMER_NUMBER, 1, 1) , sgAddressBook.RowCount - 1 + OffSet]:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), iCNT];
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.CUSTOMER_NAME, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), iCNT];
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.AGENT, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fAGENT, 1, 1), iCNT];
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.DIVISION, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fDIVISION, 1, 1), iCNT];
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.COCODE, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCO_CODE, 1, 1), iCNT];
      { EMPTY FIELDS }
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.EMAILS, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:='';
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.PHONE_NUMBERS, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:='';
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.CONTACT, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:='';
      sgAddressBook.Cells[sgAddressBook.ReturnColumn(TAddressBook.ESTATEMENTS, 1, 1), sgAddressBook.RowCount - 1 + OffSet]:='';
      { NEXT ROW }
      sgAddressBook.RowCount:=sgAddressBook.RowCount + 1;
    end;
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  if (sgAgeView.Selection.Bottom - sgAgeView.Selection.Top) = 0 then
  begin
    MsgCall(mcInfo, 'Customer has been addedd to the Address Book tabsheet.');
  end else
  begin
    MsgCall(mcInfo, 'Customers have been addedd to the Address Book tabsheet.');
  end;
  Screen.Cursor:=crDefault;
end;

{ --------------------------------------------------------------------------------------------------------------------------- ADD FOLLOW-UP TO SELECTED GROUP }
procedure TMainForm.Action_GroupFollowUpClick(Sender: TObject);
var
  iCNT: integer;
begin
  Screen.Cursor:=crHourGlass;
  CalendarForm.CalendarMode:=cfGetDate;
  MainForm.WndCall(CalendarForm, 0);
  { IF SELECTED MORE THAN ONE CUSTOMER, ASSIGN GIVEN DATE TO SELECTED CUSTOMERS }
  if CalendarForm.SelectedDate <> NULLDATE then
  begin
    for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
      if sgAgeView.RowHeights[iCNT] <> - 1 then
        CalendarForm.SetFollowUp(CalendarForm.SelectedDate, sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1), iCNT], iCNT);
  end;
  Screen.Cursor:=crDefault;
end;

{ ------------------------------------------------------------- ! FILTER AGE VIEW GRID ! -------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------------------------------------------------------------------------- VIA INF7 }
procedure TMainForm.Action_INF7_FilterClick(Sender: TObject);
begin
  FilterForm.FColName:=TSnapshots.fINF7;
  FilterForm.FOverdue:=TSnapshots.fOVERDUE;
  FilterForm.FGrid   :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_INF7;
  WndCall(FilterForm, stModal);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ INF4 }
procedure TMainForm.Action_INF4_FilterClick(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fINF4;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_INF4;
  WndCall(FilterForm, stModal);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ VIA COCODE }
procedure TMainForm.Action_CoCode_FilterClick(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fCO_CODE;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_COCODE;
  WndCall(FilterForm, stModal);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- VIA AGENT }
procedure TMainForm.Action_Agent_FilterClick(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fAGENT;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_AGENT;
  WndCall(FilterForm, stModal);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- VIA DIVISION }
procedure TMainForm.Action_Division_FilterClick(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fDIVISION;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_DIVISION;
  WndCall(FilterForm, stModal);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- VIA FOLLOW UP }
procedure TMainForm.Action_FollowUp_FilterClick(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fFOLLOWUP;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_FOLLOWUP;
  WndCall(FilterForm, stModal);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- GROUP 3 }
procedure TMainForm.Action_Gr3_FilterClick(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fGROUP3;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_GR3;
  WndCall(FilterForm, stModal);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- FREE1 }
procedure TMainForm.Action_Free1Click(Sender: TObject);
begin
  FilterForm.FColName  :=TSnapshots.fFREE1;
  FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
  FilterForm.FGrid     :=MainForm.sgAgeView;
  FilterForm.FFilterNum:=flt_Free1;
  WndCall(FilterForm, stModal);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- REMOVE ALL FILTERS }
procedure TMainForm.Action_RemoveFiltersClick(Sender: TObject);
var
  iCNT:  integer;
  AgeView: TAgeView;
begin
  sgAgeView.Freeze(True);
  for iCNT:=1 to sgAgeView.RowCount - 1 do sgAgeView.RowHeights[iCNT]:=sgRowHeight;
  FilterForm.FilterClearAll;
  { RE-COMPUTE AGING SUMMARY }
  AgeView:=TAgeView.Create(MainForm.DbConnect);
  try
    AgeView.ComputeAgeSummary(MainForm.sgAgeView);
    AgeView.ComputeAndShowRCA(MainForm.sgAgeView);
    AgeView.UpdateSummary;
  finally
   AgeView.Free;
  end;
  sgAgeView.Freeze(False);
end;

{ --------------------------------------------------------------------------------------------------------------------------------- EXCLUDE NON-OVERDUE ITEMS }
procedure TMainForm.Action_OverdueClick(Sender: TObject);
begin
  if Action_Overdue.Checked
    then
      Action_Overdue.Checked:=False
        else
          Action_Overdue.Checked:=True;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- SEARCH CUSTOMER }
procedure TMainForm.Action_SearchClick(Sender: TObject);
begin
  { SETUP AND CALL WINDOW }
  SearchForm.SGrid     :=MainForm.sgAgeView;
  SearchForm.SColName  :=TSnapshots.fCUSTOMER_NAME;
  SearchForm.SColNumber:=TSnapshots.fCUSTOMER_NUMBER;
  WndCall(SearchForm, stModeless);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SHOW PAYMENT TERM }
procedure TMainForm.Action_PaymentTermClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(DbConnect);
  try
    MsgCall(
             mcInfo,
             'Payment term: ' +
             AgeView.GetData(
                              sgAgeView.Cells[
                                               sgAgeView.ReturnColumn(TSnapshots.fPAYMENT_TERMS, 1, 1),
                                               sgAgeView.Row
                                             ],
                              TblPmtterms
                            )
           );
  finally
    AgeView.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SHOW PERSON }
procedure TMainForm.Action_PersonClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(DbConnect);
  try
    MsgCall(
             mcInfo,
             'Payment term: ' +
             AgeView.GetData(
                              sgAgeView.Cells[
                                               sgAgeView.ReturnColumn(TSnapshots.fPERSON, 1, 1),
                                               sgAgeView.Row
                                             ],
                              TblPerson
                            )
           );
  finally
    AgeView.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ SAVE STRING GRID TO MS EXCEL }
procedure TMainForm.Action_ToExceClick(Sender: TObject);
begin
  TTExcelExport.Create;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ EXPORT CSV }
procedure TMainForm.Action_ExportCSVClick(Sender: TObject);
begin
  sgAgeView.ExportCSV(CSVExport, '|');
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SELECT ALL }
procedure TMainForm.Action_SelectAgeViewClick(Sender: TObject);
begin
  sgAgeView.SelectAll;
end;

{ ------------------------------------------------------------------------------------------------------------------------------- COPY SELECTION TO CLIPBOARD }
procedure TMainForm.Action_CopyAllClick(Sender: TObject);
begin
  sgAgeView.CopyCutPaste(adCopy);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- HIDE OR SHOW SUMMARY }
procedure TMainForm.Action_HideSummaryClick(Sender: TObject);
begin
  if Action_HideSummary.Checked then
  begin
    Footer1.Visible:=False;
    sgAgeView.Margins.Bottom:=17;
    Action_HideSummary.Checked:=False;
  end
  else
  begin
    Footer1.Visible:=True;
    sgAgeView.Margins.Bottom:=0;
    Action_HideSummary.Checked:=True;
  end;
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
  AgeView:=TAgeView.Create(DbConnect);
  try
    AgeView.AgeViewMode(mainForm.sgAgeView, AgingBasic);
  finally
    AgeView.Free;
  end;
  Action_AutoColumnSizeClick(Self);
  { TICK }
  Action_BasicView.Checked:=True;
  Action_FullView.Checked :=False;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- SHOW ALL AVAILABLE COLUMNS }
procedure TMainForm.Action_FullViewClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  AgeView:=TAgeView.Create(DbConnect);
  try
    AgeView.AgeViewMode(mainForm.sgAgeView, AgingFull);
  finally
    AgeView.Free;
  end;
  Action_AutoColumnSizeClick(Self);
  { TICK }
  Action_BasicView.Checked:=False;
  Action_FullView.Checked :=True;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- SET FOLLOW-UP COLORS }
procedure TMainForm.Action_FollowUpColorsClick(Sender: TObject);
begin
  WndCall(ColorsForm, stModal);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- ROW HIGHLIGHT }
procedure TMainForm.Action_RowHighlightClick(Sender: TObject);
begin
  if Action_RowHighlight.Checked then
  begin
    sgAgeView.Options:=sgAgeView.Options - [goRowSelect];
    sgAgeView.Options:=sgAgeView.Options + [goRangeSelect];
    Action_RowHighlight.Checked:=False;
  end
  else
  begin
    sgAgeView.Options:=sgAgeView.Options + [goRowSelect];
    sgAgeView.Options:=sgAgeView.Options - [goRangeSelect];
    Action_RowHighlight.Checked:=True;
  end;
end;

{ -------------------------------------------------------------- ! INVOICE TRACKER ! ------------------------------------------------------------------------ }

{ ------------------------------------------------------------------------------------------------------------------------------------------ REMOVE FROM LIST }
procedure TMainForm.Action_RemoveClick(Sender: TObject);
begin

  { EXIT IF NO CONNECTION }
  if ConnLastError <> 0 then
  begin
    MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    Exit;
  end;

  TrackerForm.CUID:=sgInvoiceTracker.Cells[sgInvoiceTracker.ReturnColumn(TTracker.CUID, 1, 1), sgInvoiceTracker.Row];

  { R/W USER CAN REMOVE ITEM }
  if (MainForm.AccessLevel = acReadWrite) and (UpperCase(MainForm.WinUserName) = UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
    if MsgCall(mcQuestion2, 'Are you sure you want to remove selected customer?') = IDYES then
      TrackerForm.Delete;

  { R/W USER CANNOT REMOVE OTHER ITEM }
  if (MainForm.AccessLevel = acReadWrite) and (UpperCase(MainForm.WinUserName) <> UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
    MsgCall(mcWarn, 'You cannot remove someone''s else item.');

  { ADMINISTRATOR CAN REMOVE ANY ITEM }
  if (MainForm.AccessLevel = acADMIN) then
    if MsgCall(mcQuestion2, 'Are you sure you want to remove selected customer?') = IDYES then
      TrackerForm.Delete;

  { READ ONLY USER CANNOT REMOVE ANYTHING }
  if (MainForm.AccessLevel = acReadOnly) then MsgCall(mcWarn, 'You don''t have permission to remove items.');

end;

{ ----------------------------------------------------------------------------------------------------------------------- SHOW SENT INVOICES FOR GIVEN 'CUID' }
procedure TMainForm.Action_ShowRegisteredClick(Sender: TObject);
begin
  WndCall(InvoicesForm, stModal);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- SHOW MY ITEMS }
procedure TMainForm.Action_ShowMyClick(Sender: TObject);
begin
  if ConnLastError = 0 then
    TTInvoiceTrackerRefresh.Create(UpperCase(MainForm.WinUserName))
      else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SHOW ALL ITEMS }
procedure TMainForm.Action_ShowAllClick(Sender: TObject);
begin
  if ConnLastError = 0 then
    TTInvoiceTrackerRefresh.Create('*')
      else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
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
  if ConnLastError <> 0 then
  begin
    MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    Exit;
  end;
  UserControl:=TUserControl.Create(DbConnect);
  try
    UserControl.UserName:=WinUserName;
    if not (UserControl.GetAgeDates(GroupListDates, GroupList[GroupListBox.ItemIndex, 0])) then
    begin
      MsgCall(mcError, 'Cannot list age dates for selected group. Please contact IT support.');
      LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: "GetAgeDates" returned false. Cannot get list of age dates for selected group (' + GroupList[GroupListBox.ItemIndex, 0] + ').');
    end;
  finally
    UserControl.Free;
  end;
end;

{ ------------------------------------------------------- ! COMPONENT EVENTS | TABSHEETS ! ------------------------------------------------------------------ }

{ -------------------------------------------------------------------------------------------------------------------------------------------- SHOW ALL ITEMS }
procedure TMainForm.TabSheet4Show(Sender: TObject);
begin
  TTInvoiceTrackerRefresh.Create('*');
end;

{ ------------------------------------------------------------------------------------------------------------ MAKE PAYMENT TERMS AND PAID INFO TABLES HEIGHT }
procedure TMainForm.TabSheet7Show(Sender: TObject);
begin
  sgPmtTerms.Height:=Round(0.5 * sgCoCodes.Height);
  sgPaidInfo.Height:=Round(0.5 * sgCoCodes.Height);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- REFRESH VIEW }
procedure TMainForm.TabSheet7Resize(Sender: TObject);
begin
  TabSheet7Show(self);
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
      { ----------------------------------------------------------------------------------------------------------------------------- MOVE FROM RIGHT TO LEFT }
      if FromIndex > ToIndex then
        for iCNT:=ToIndex to (FromIndex - 1) do
          sgAgeView.SqlColumns[iCNT + 1, 0]:=Temp[iCNT, 0];
      { ----------------------------------------------------------------------------------------------------------------------------- MOVE FROM LEFT TO RIGHT }
      if FromIndex < ToIndex then
        for iCNT:=(FromIndex + 1) to ToIndex do
          sgAgeView.SqlColumns[iCNT - 1, 0]:=Temp[iCNT, 0];
    except
      on E: Exception do
        MainForm.MsgCall(mcWarn, 'Unexpected error has occured. Description: ' + E.Message + '. Please contact IT support.')
    end;
  finally
    { SAVE CHANGES }
    sgAgeView.SaveLayout(ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);
    { REMOVE FROM MEMORY }
    Temp:=nil;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- OPEN TRANSACTION WINDOW }
procedure TMainForm.sgAgeViewDblClick(Sender: TObject);
begin
  Action_LyncCallClick(Self);
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
  if ConnLastError = 0 then
    WndCall(InvoicesForm, stModal)
      else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

{ ---------------------------------------------------- ! SHOW NEGATIVE VALUES AND ROW SELECTION ! ----------------------------------------------------------- }
procedure TMainForm.sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Col1:        integer;
  Col2:        integer;
  Col3:        integer;
  Col4:        integer;
  Col5:        integer;
  Col6:        integer;
  Col7:        integer;
  Col8:        integer;
  Col9:        integer;
  Col10:       integer;
  Col11:       integer;
  Col12:       integer;
  Col13:       integer;
  Col14:       integer;
  Col15:       integer;
  Col16:       integer;
  iCNT:        integer;
  Width:       integer;
  AgeViewCUID: string;
begin

  { SKIP HEADER }
  if ARow = 0 then Exit;

  (* CALL DRAWSELECTED BEFORE COLORVALUES *)

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
  Col12:=sgAgeView.ReturnColumn(TGeneral.fFOLLOWUP,          1, 1);
  Col13:=sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1);
  Col14:=sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1);
  Col15:=sgAgeView.ReturnColumn(TSnapshots.fRISK_CLASS,      1, 1);
  Col16:=sgAgeView.ReturnColumn(TSnapshots.fGROUP3,          1, 1);

  { MAP GROUP3 COLUMN }
  if ACol = Col16 then
  begin
    for iCNT:=1 to sgGroup3.RowCount - 1 do
    begin
      if sgGroup3.Cells[sgGroup3.ReturnColumn(TGroup3.ERP_CODE, 1, 1), iCNT] = sgAgeView.Cells[ACol, ARow] then
        sgAgeView.Cells[ACol, ARow]:=sgGroup3.Cells[sgGroup3.ReturnColumn(TGroup3.DESCRIPTION, 1, 1), iCNT];
    end;
  end;

  { HIGHLIGHT FOLLOW UP COLUMN }
  if not (CDate(sgAgeView.Cells[ACol, ARow]) = 0) then
  begin
    { FUTURE DAYS }
    if (ACol = Col12) and (CDate(sgAgeView.Cells[ACol, ARow]) > CDate(StatBar_TXT3.Caption)) then
    begin
      sgAgeView.Canvas.Brush.Color:=FutureBColor;
      sgAgeView.Canvas.Font.Color :=FutureFColor;
      sgAgeView.Canvas.FillRect(Rect);
      sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
    end;
    { TODAY }
    if (ACol = Col12) and (CDate(sgAgeView.Cells[ACol, ARow]) = CDate(StatBar_TXT3.Caption)) then
    begin
      sgAgeView.Canvas.Brush.Color:=TodayBColor;
      sgAgeView.Canvas.Font.Color :=TodayFColor;
      sgAgeView.Canvas.FillRect(Rect);
      sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
    end;
    { PAST DAYS }
    if (ACol = Col12) and (CDate(sgAgeView.Cells[ACol, ARow]) < CDate(StatBar_TXT3.Caption)) then
    begin
      sgAgeView.Canvas.Brush.Color:=PastBColor;
      sgAgeView.Canvas.Font.Color :=PastFColor;
      sgAgeView.Canvas.FillRect(Rect);
      sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
    end;
  end;

  { CUSTOMER CONTRIBUTING TO RISK CLASS "A" SET TO BOLD FONT }
  if (ACol = Col14) and (sgAgeView.Cells[Col15, ARow] = 'A') then
  begin
    sgAgeView.Canvas.Font.Style:=[fsBold];
    sgAgeView.Canvas.FillRect(Rect);
    sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
  end;

  { MARK CUSTOMER WITH STAR IF IT IS REGISTERED ON INVOICE TRACKER LIST }
  if ACol = Col14 then
  begin
    Width:=sgAgeView.ColWidths[Col14];
    AgeViewCUID:=sgAgeView.Cells[Col13, ARow];
    for iCNT:=1 to sgInvoiceTracker.RowCount - 1 do
    begin
      if AgeViewCUID = sgInvoiceTracker.Cells[2, iCNT] then
      begin
        sgAgeView.Canvas.Draw(Rect.Left + Width - 16, Rect.Top, GridPicture.Picture.Graphic);
        Break;
      end;
    end;
  end;

  { COLOUR NEGATIVE VALUES IN SELECTED COLUMNS }
  if (ACol = Col1)  or (ACol = Col2) or (ACol = Col3) or
     (ACol = Col4)  or (ACol = Col5) or (ACol = Col6) or
     (ACol = Col7)  or (ACol = Col8) or (ACol = Col9) or
     (ACol = Col10) or (ACol = Col11)
  then
    sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clBlack);

end;

procedure TMainForm.sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

  (* CALL DRAWSELECTED BEFORE COLORVALUES *)

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

procedure TMainForm.sgUACDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgUAC.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgGroupsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  sgGroups.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
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
  begin
    if MsgCall(mcQuestion1, 'Are you sure you want to exit the application?') = IDOK then
    begin
      pAllowClose:=True;
      Close;
    end;
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
  if (not (CharInSet(Key, ['A'..'Z', 'a'..'z', '0'..'9', '-', BACKSPACE]))) then Key:=#0;
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

{ ------------------------------------------------------------------------------------------------------------------ ADDRESS BOOK | RESTRICT TELEPHONE COLUMN }
procedure TMainForm.sgAddressBookKeyPress(Sender: TObject; var Key: Char);
begin
  if sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.PHONE_NUMBERS, 1, 1) then
    if (not (CharInSet(Key, ['0'..'9', ';', BACKSPACE]))) then Key:=#0;
end;

{ --------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK | PASTE, CUT, COPY }
procedure TMainForm.sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  DataTables: TDataTables;
begin
  { "READ ONLY" USERS ARE NOT ALLOWED TO MAKE CHANGES }
  if AccessLevel = acReadOnly then
  begin
    MsgCall(mcWarn, 'You don''t have permission to edit Address Book records.');
    Exit;
  end;
  { FIRST COLUMN ARE NOT EDITABLE }
  if (sgAddressBook.Col = 1) then Exit;
  { CALL "SAVE NEW" IF "CTRL + S" IS PRESSED }
  if (Key = 115) and (Shift = [ssCtrl]) then TTAddressBook.Create(adSaveNew, sgAddressBook);
  { COPY, PASTE, CUT }
  if (Key = 67) and (Shift = [ssCtrl]) then sgAddressBook.CopyCutPaste(adCopy);
  if (Key = 86) and (Shift = [ssCtrl]) and (sgAddressBook.Cells[0, sgAddressBook.Row] <> '') then sgAddressBook.CopyCutPaste(adPaste);
  if (Key = 88) and (Shift = [ssCtrl]) and (sgAddressBook.Cells[0, sgAddressBook.Row] <> '') then sgAddressBook.CopyCutPaste(adCut);
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
      DataTables:=TDataTables.Create(DbConnect);
      try
        { GET USER VALUE AND COLUMN NAME }
        DataTables.CleanUp;
        DataTables.Columns.Add(sgAddressBook.Cells[sgAddressBook.Col, 0]);
        DataTables.Values.Add(DataTables.CleanStr(sgAddressBook.Cells[sgAddressBook.Col, sgAddressBook.Row], False));
        DataTables.Conditions.Add(TAddressBook.SCUID + EQUAL + DataTables.CleanStr(sgAddressBook.Cells[2, sgAddressBook.Row], True));
        DataTables.UpdateRecord(TblAddressbook);
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
  (* if Key = #13 then sgListValue.Cells[1, sgListValue.Row]:=UpperCase(sgListValue.Cells[1, sgListValue.Row]); OFF *)
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

{ UAC | WHEEL DOWN }
procedure TMainForm.sgUACMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgUAC.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ UAC | WHEE UP }
procedure TMainForm.sgUACMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgUAC.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ GROUPS | WHEEL DOWN }
procedure TMainForm.sgGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgGroups.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

{ GROUPS | WHEEL UP }
procedure TMainForm.sgGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  sgGroups.Perform(WM_VSCROLL, SB_LINEUP, 0);
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

{ ------------------------------------------------------------------------------------------------------------------------------------------------- EVENT LOG }
procedure TMainForm.imgEventLogMouseEnter(Sender: TObject);
begin
  { CHANGE CURSOR TO HAND POINT }
  imgAllowEdit.Cursor:=crHandPoint;
  Text51.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgEventLogMouseLeave(Sender: TObject);
begin
  { CHANGE CURSOR TO DEFAULT (ARROW) }
  imgAllowEdit.Cursor:=crDefault;
  Text51.Font.Color:=clBlack;
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

{ -------------------------------------------------------------------------------------------------------------------------------------------- GO TO REPORT 1 }
procedure TMainForm.btnReport1Click(Sender: TObject);
var
  Return: cardinal;
begin
  Return:=ShowReport(1);
  if not(Return > 32) then
  begin
    MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
    LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- GO TO REPORT 2 }
procedure TMainForm.btnReport2Click(Sender: TObject);
var
  Return: cardinal;
begin
  Return:=ShowReport(2);
  if not(Return > 32) then
  begin
    MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
    LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- GO TO REPORT 3 }
procedure TMainForm.btnReport3Click(Sender: TObject);
var
  Return: cardinal;
begin
  Return:=ShowReport(3);
  if not(Return > 32) then
  begin
    MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
    LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- GO TO REPORT 4 }
procedure TMainForm.btnReport4Click(Sender: TObject);
var
  Return: cardinal;
begin
  Return:=ShowReport(4);
  if not(Return > 32) then
  begin
    MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
    LogText(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- AGE VIEW | LOAD }
procedure TMainForm.btnLoadAgeViewClick(Sender: TObject);
begin
  { WAIT UNTIL READY }
  if not (StatBar_TXT1.Caption = stReady) then
  begin
    MsgCall(mcWarn, 'Please wait until "Ready" status and try again.');
    Exit;
  end;
  { PROCEED }
  if (GroupListBox.Text <> '') and (GroupListDates.Text <> '') then
  begin
    { REMEMBER USER'S CHOICE }
    { AUTOMATION WIIL FOLLOW }
    GroupIdSel:=GroupList[GroupListBox.ItemIndex, 0];
    GroupNmSel:=GroupList[GroupListBox.ItemIndex, 1];
    AgeDateSel:=GroupListDates.Text;
    { SWITCH OFF ALL TIMERS }
    MainForm.SwitchTimers(tmDisabled);
    { LOAD AGE VIEW FOR SELECTED GROUP }
    TTReadAgeView.Create(thCallOpenItems);
  end
    else
      MsgCall(mcWarn, 'Cannot load selected group.');
end;

{ --------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS | FORCE RELOAD }
procedure TMainForm.btnReloadClick(Sender: TObject);
begin
  { EXIT ON NO CONNECTION }
  if ConnLastError <> 0 then
  begin
    MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    Exit;
  end;
  { ONLY ADMINISTRATORS ARE ALLOWED }
  StatBar_TXT1.Caption :=stProcessing;
  if MainForm.AccessLevel = acADMIN then
  begin
    TTReadOpenItems.Create(thNullParameter);
  end else
  begin
    StatBar_TXT1.Caption:=stReady;
    LogText(EventLogPath, '[Open Items]: User have no R/W access, process halted.');
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------- ACTIONS | OPEN MAKE AGE PANEL }
procedure TMainForm.btnMakeGroupClick(Sender: TObject);
begin
  cbDump.Checked:=False;
  { EXIT ON NO CONNECTION }
  if ConnLastError <> 0 then
  begin
    MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    Exit;
  end;
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
      EditGroupName.Text:=GroupNmSel;
      EditGroupID.Text  :=GroupIdSel;
    end
    else
    begin
      StatBar_TXT1.Caption:='Insufficient UAC level.';
      LogText(EventLogPath, '[Make Group]: User have no R/W access, process halted.');
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
      MsgCall(mcWarn, 'Please enter group name and try again.' + CRLF + 'If you will use existing one, then it will be overwritten.');
end;

{ -------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | OPEN FROM DB }
procedure TMainForm.btnOpenABClick(Sender: TObject);
begin
  if ConnLastError = 0 then
    TTAddressBook.Create(adOpenAll, sgAddressBook)
      else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

{ ---------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | SAVE NEWLY ADDED }
procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  if ConnLastError = 0 then
    TTAddressBook.Create(adSaveNew, sgAddressBook)
      else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

{ --------------------------------------------------------------------------------------------------------------------------------- USER ADDRESS BOOK | CLOSE }
procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  if MsgCall(mcQuestion2, 'Are you sure you want to close address book?') = IDYES then sgAddressBook.ClearAll(2, 1, 1, True);
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
  if MsgCall(mcQuestion2, 'Are you sure you want to delete this section? It cannot be undone.') = IDNO then exit;
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

{ -------------------------------------------------------------------------------------------------------------------------------------------- OPEN EVENT LOG }
procedure TMainForm.imgEventLogClick(Sender: TObject);
begin
  WndCall(EventForm, stModeless);
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
  if MsgCall(mcQuestion2, 'Are you sure you want to delete this key? It cannot be undone.') = IDNO then Exit;
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
  if MsgCall(mcQuestion2, 'Are you sure you want to save all the changes? It cannot be undone.') = IDNO then exit;
  { CHECK IF THERE IS NO EMPTY KEYS }
  for iCNT:= 1 to (sgListValue.RowCount - 1) do
    if sgListValue.Cells[1, iCNT] = '' then
    begin
      MsgCall(mcWarn, 'Cannot save. At least one key has no label.');
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
  MsgCall(mcInfo, 'All Keys and its values has been saved successfully.');
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
      MsgCall(mcInfo, 'New password has been saved.');
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
        MsgCall(mcWarn, 'New password and confirmation does not match, please re-type it and try again.');
  end
    else
      MsgCall(mcWarn, 'Current password is incorrect, please re-type it and try again.');
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- UNLOCK }
procedure TMainForm.btnUnlockClick(Sender: TObject);

  (* COMMON VARIABLES *)
  var
    AppSettings:  TSettings;
    DataTables:   TDataTables;

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
      if (AppSettings.TMIG.ReadString(Password, 'HASH', '') <> '') and
         (AppSettings.TMIG.ReadString(Password, 'HASH', '') = Edit_PASSWORD.Text) then
      begin
        { SETUP HEADERS }
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
        sgUAC.Enabled:=True;
        sgGroups.Enabled:=True;
        sgListSection.Enabled:=True;
        sgListValue.Enabled:=True;
        sgListSectionClick(self);
        sgListSection.Row:=1;
        sgListValue.Row:=1;
        { TRANSPARENCY OFF }
        imgOFF.Visible:=False;
        { POPULATE STRING GRIDS }
        tStrings:=TStringList.Create();
        try
          { READ ALL SETTINGS }
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
          { READ UAC AND GROUPS }
          DataTables:=TDataTables.Create(DbConnect);
          try
            DataTables.OpenTable(TblUAC);    DataTables.SqlToGrid(sgUAC,    DataTables.ExecSQL, False, True);
            DataTables.OpenTable(TblGroups); DataTables.SqlToGrid(sgGroups, DataTables.ExecSQL, False, True);
          finally
            DataTables.Free;
            sgUAC.SetColWidth   (10, 20);
            sgGroups.SetColWidth(10, 20);
          end;
        finally
          tStrings.Free;
          btnUnlock.Caption:='Lock';
          sgListValue.RowCount:=2;
        end;
      end;

      { STOP IF PASSWORD IS INVALID }
      if (AppSettings.TMIG.ReadString(Password, 'HASH', '') <> '') and
         (AppSettings.TMIG.ReadString(Password, 'HASH', '') <> Edit_PASSWORD.Text) then
      begin
        MsgCall(mcWarn, 'Incorrect password, please re-type it and try again.');
      end;

      { SETUP NEW PASSWORD }
      if (AppSettings.TMIG.ReadString(Password, 'HASH', '') = '') then
      begin
        Edit_CurrPassWd.Enabled:=True;
        Edit_NewPassWd.Enabled :=True;
        Edit_ConfPassWd.Enabled:=True;
        MsgCall(mcWarn, 'Please provide with new password.');
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
