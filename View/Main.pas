
{$I .\Include\Header.inc}

unit Main;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, Grids, ExtCtrls, StdCtrls, CheckLst, Buttons,
    PNGImage, DBGrids, AppEvnts, ShellAPI, INIFiles, StrUtils, ValEdit, DateUtils, Clipbrd, DB, ADODB, ActiveX, CDO_TLB, Diagnostics, Math, Wininet, ComObj,
    OleCtrls, SHDocVw, GIFImg, System.UITypes, Bcrypt, InterposerClasses, Arrays, EventLogger, System.ImageList, Vcl.ImgList, uCEFChromium, uCEFWindowParent,
    uCEFChromiumWindow, uCEFTypes, uCEFInterfaces;

type

    /// <summary>
    ///     Main form class, represents GUI/main application thread.
    /// </summary>

    TMainForm = class(TForm)
        MyPages: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
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
        EditPassword: TEdit;
        Cap21: TShape;
        hShapeCred: TShape;
        Text32: TLabel;
        Text33: TLabel;
        hShapePass: TShape;
        Cap22: TShape;
        Text37: TLabel;
        Text38: TLabel;
        Text39: TLabel;
        EditCurrentPassword: TEdit;
        EditNewPassword: TEdit;
        EditNewPasswordConfirmation: TEdit;
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
        btnUpdateAB: TImage;
        btnCloseAB: TImage;
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
        btnExportAB: TImage;
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
        StatBar_CAP3: TLabel;
        StatBar_TXT3: TLabel;
        StatBar_CAP4: TLabel;
        StatBar_TXT4: TLabel;
        StatBar_CAP5: TLabel;
        StatBar_TXT5: TLabel;
        CurrentTime: TTimer;
        UpTime: TTimer;
        txtInfo1: TLabel;
        txtInfo3: TLabel;
        txtInfo2: TLabel;
        CSVExport: TSaveDialog;
        CSVImport: TOpenDialog;
        ContentPanel7: TPanel;
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
        N11: TMenuItem;
        Action_ShowAsIs: TMenuItem;
        Action_ShowMyEntries: TMenuItem;
        Action_ToExce: TMenuItem;
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
        DetailsGrid: TStringGrid;
        btnLoadAgeView: TSpeedButton;
        EditGroupID: TLabeledEdit;
        Action_RowHighlight: TMenuItem;
        Action_Update: TMenuItem;
        Action_Report: TMenuItem;
        N17: TMenuItem;
        CommonPopupMenu: TPopupMenu;
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
        Action_RemoveFilters: TMenuItem;
        Action_Free1: TMenuItem;
        Separate1: TBevel;
        Separate2: TBevel;
        Separate3: TBevel;
        AppHeader: TPanel;
        Bevel1: TBevel;
        Bevel2: TBevel;
        Bevel3: TBevel;
        PanelOpenItems: TPanel;
        ImgLoadingOpenItems: TImage;
        PanelAddressBook: TPanel;
        PanelInvoiceTracker: TPanel;
        ImgLoadingInvoiceTracker: TImage;
        PanelCoCodes: TPanel;
        PanelPaidInfo: TPanel;
        PanelPerson: TPanel;
        PanelPmtTerms: TPanel;
        PanelGroup3: TPanel;
        PanelSettingsSections: TPanel;
        PanelSettingsValues: TPanel;
        PanelUAC: TPanel;
        PanelGroups: TPanel;
        btnUnlock: TSpeedButton;
        btnPassUpdate: TSpeedButton;
        Action_AddFollowUpGroup: TMenuItem;
        Action_RemoveFollowUps: TMenuItem;
        Cap24: TShape;
        Shape2: TShape;
        Action_MassMailer: TMenuItem;
        btnPasswordPreview: TSpeedButton;
        hShapeEye: TShape;
        N20: TMenuItem;
        N21: TMenuItem;
        Action_Ranges: TMenuItem;
        Action_Range1: TMenuItem;
        Action_Range2: TMenuItem;
        Action_Range3: TMenuItem;
        Action_Range4: TMenuItem;
        Action_Range5: TMenuItem;
        Action_Range6: TMenuItem;
        Action_Amounts: TMenuItem;
        Action_TotalAmount: TMenuItem;
        Action_Overdues: TMenuItem;
        Action_Free2: TMenuItem;
        Action_ViewOptions: TMenuItem;
        N16: TMenuItem;
        N22: TMenuItem;
        Action_ShowDetails: TMenuItem;
        Action_QuickReporting: TMenuItem;
        SortListBox: TComboBox;
        btnSortApply: TSpeedButton;
        imgStart: TImage;
        btnStart: TPanel;
        txtStart: TLabel;
        btnReports: TPanel;
        imgReports: TImage;
        txtReports: TLabel;
        btnDebtors: TPanel;
        imgDebtors: TImage;
        txtDebtors: TLabel;
        btnTracker: TPanel;
        imgTracker: TImage;
        txtTracker: TLabel;
        btnAddressBook: TPanel;
        imgBook: TImage;
        txtAddressBook: TLabel;
        btnOpenItems: TPanel;
        imgOpenItems: TImage;
        txtOpenItems: TLabel;
        btnUnidentified: TPanel;
        imgUnidentified: TImage;
        txtUnidentified: TLabel;
        btnQueries: TPanel;
        imgQueries: TImage;
        txtQueries: TLabel;
        btnTables: TPanel;
        imgTables: TImage;
        txtTables: TLabel;
        btnSettings: TPanel;
        imgSettings: TImage;
        txtSettings: TLabel;
        ChromiumWindow: TChromiumWindow;
        Chromium: TChromium;
        ChromiumTimer: TTimer;
        txtOverdueItems: TLabel;
        txtCreditLimitsReport: TLabel;
        txtDebtorsReport: TLabel;
        txtControlStatusReport: TLabel;
        imgHideBar: TImage;
        PanelControlStatus: TPanel;
        sgControlStatus: TStringGrid;
        Tables: TPageControl;
        Page1: TTabSheet;
        Page2: TTabSheet;
        Page3: TTabSheet;
        Page4: TTabSheet;
        Page5: TTabSheet;
        Page6: TTabSheet;
        Page7: TTabSheet;
        Page8: TTabSheet;
        Page9: TTabSheet;
        Page10: TTabSheet;
        PanelPersonResp: TPanel;
        sgPersonResp: TStringGrid;
        PanelSalesResp: TPanel;
        sgSalesResp: TStringGrid;
        PanelAccountType: TPanel;
        sgAccountType: TStringGrid;
        PanelCustomerGr: TPanel;
        sgCustomerGr: TStringGrid;
        PanelAgeView: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure Action_HideAppClick(Sender: TObject);
        procedure Action_ShowAppClick(Sender: TObject);
        procedure Action_HelpClick(Sender: TObject);
        procedure Action_AboutClick(Sender: TObject);
        procedure Action_OnTopClick(Sender: TObject);
        procedure TabSheet8Show(Sender: TObject);
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
        procedure btnUpdateABMouseEnter(Sender: TObject);
        procedure btnUpdateABMouseLeave(Sender: TObject);
        procedure btnCloseABMouseEnter(Sender: TObject);
        procedure btnCloseABMouseLeave(Sender: TObject);
        procedure btnExportABMouseEnter(Sender: TObject);
        procedure btnExportABMouseLeave(Sender: TObject);
        procedure btnReloadClick(Sender: TObject);
        procedure sgListSectionKeyPress(Sender: TObject; var Key: Char);
        procedure sgListValueClick(Sender: TObject);
        procedure sgListSectionClick(Sender: TObject);
        procedure EditPasswordKeyPress(Sender: TObject; var Key: Char);
        procedure CurrentTimeTimer(Sender: TObject);
        procedure UpTimeTimer(Sender: TObject);
        procedure sgListSectionMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListSectionMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListValueMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListValueMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgListSectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnCloseABClick(Sender: TObject);
        procedure btnUpdateABClick(Sender: TObject);
        procedure btnOpenABClick(Sender: TObject);
        procedure btnExportABClick(Sender: TObject);
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
        procedure FormDestroy(Sender: TObject);
        procedure Action_SearchClick(Sender: TObject);
        procedure Action_CutClick(Sender: TObject);
        procedure Action_CopyClick(Sender: TObject);
        procedure Action_PasteClick(Sender: TObject);
        procedure Action_DelRowClick(Sender: TObject);
        procedure BookPopupPopup(Sender: TObject);
        procedure Action_ShowAsIsClick(Sender: TObject);
        procedure Action_ShowMyEntriesClick(Sender: TObject);
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
        procedure Action_INF4_FilterClick(Sender: TObject);
        procedure Action_Gr3_FilterClick(Sender: TObject);
        procedure Action_HideSummaryClick(Sender: TObject);
        procedure Action_ExportCSVClick(Sender: TObject);
        procedure Action_RemoveFiltersClick(Sender: TObject);
        procedure Action_Free1Click(Sender: TObject);
        procedure sgAgeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnUnlockClick(Sender: TObject);
        procedure btnPassUpdateClick(Sender: TObject);
        procedure sgAddressBookKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgListSectionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgListValueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Action_AddFollowUpGroupClick(Sender: TObject);
        procedure Action_RemoveFollowUpsClick(Sender: TObject);
        procedure Action_MassMailerClick(Sender: TObject);
        procedure btnPasswordPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure btnPasswordPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure Action_Free2Click(Sender: TObject);
        procedure Action_Range1Click(Sender: TObject);
        procedure Action_Range2Click(Sender: TObject);
        procedure Action_Range3Click(Sender: TObject);
        procedure Action_Range4Click(Sender: TObject);
        procedure Action_Range5Click(Sender: TObject);
        procedure Action_Range6Click(Sender: TObject);
        procedure Action_TotalAmountClick(Sender: TObject);
        procedure Action_OverduesClick(Sender: TObject);
        procedure btnSortApplyClick(Sender: TObject);
        procedure sgCoCodesMouseEnter(Sender: TObject);
        procedure sgPaidInfoMouseEnter(Sender: TObject);
        procedure sgPersonMouseEnter(Sender: TObject);
        procedure sgPmtTermsMouseEnter(Sender: TObject);
        procedure sgGroup3MouseEnter(Sender: TObject);
        procedure sgInvoiceTrackerMouseEnter(Sender: TObject);
        procedure sgAddressBookMouseEnter(Sender: TObject);
        procedure sgOpenItemsMouseEnter(Sender: TObject);
        procedure sgAgeViewMouseEnter(Sender: TObject);
        procedure GroupListBoxMouseEnter(Sender: TObject);
        procedure GroupListDatesMouseEnter(Sender: TObject);
        procedure SortListBoxMouseEnter(Sender: TObject);
        procedure sgOpenItemsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgInvoiceTrackerKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgGroup3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgPersonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgPmtTermsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgPaidInfoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgCoCodesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgAgeViewClick(Sender: TObject);
        procedure sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnStartMouseEnter(Sender: TObject);
        procedure btnStartMouseLeave(Sender: TObject);
        procedure btnReportsMouseEnter(Sender: TObject);
        procedure btnReportsMouseLeave(Sender: TObject);
        procedure btnDebtorsMouseEnter(Sender: TObject);
        procedure btnDebtorsMouseLeave(Sender: TObject);
        procedure btnTrackerMouseEnter(Sender: TObject);
        procedure btnTrackerMouseLeave(Sender: TObject);
        procedure btnAddressBookMouseEnter(Sender: TObject);
        procedure btnAddressBookMouseLeave(Sender: TObject);
        procedure btnOpenItemsMouseEnter(Sender: TObject);
        procedure btnOpenItemsMouseLeave(Sender: TObject);
        procedure btnUnidentifiedMouseEnter(Sender: TObject);
        procedure btnUnidentifiedMouseLeave(Sender: TObject);
        procedure btnQueriesMouseEnter(Sender: TObject);
        procedure btnQueriesMouseLeave(Sender: TObject);
        procedure btnTablesMouseEnter(Sender: TObject);
        procedure btnTablesMouseLeave(Sender: TObject);
        procedure btnSettingsMouseEnter(Sender: TObject);
        procedure btnSettingsMouseLeave(Sender: TObject);
        procedure ChromiumTimerTimer(Sender: TObject);
        procedure ChromiumWindowAfterCreated(Sender: TObject);
        procedure txtStartClick(Sender: TObject);
        procedure txtReportsClick(Sender: TObject);
        procedure txtDebtorsClick(Sender: TObject);
        procedure txtTrackerClick(Sender: TObject);
        procedure txtAddressBookClick(Sender: TObject);
        procedure txtOpenItemsClick(Sender: TObject);
        procedure txtUnidentifiedClick(Sender: TObject);
        procedure txtQueriesClick(Sender: TObject);
        procedure txtTablesClick(Sender: TObject);
        procedure txtSettingsClick(Sender: TObject);
        procedure txtOverdueItemsClick(Sender: TObject);
        procedure txtCreditLimitsReportClick(Sender: TObject);
        procedure txtDebtorsReportClick(Sender: TObject);
        procedure txtControlStatusReportClick(Sender: TObject);
        procedure txtOverdueItemsMouseEnter(Sender: TObject);
        procedure txtOverdueItemsMouseLeave(Sender: TObject);
        procedure txtCreditLimitsReportMouseEnter(Sender: TObject);
        procedure txtCreditLimitsReportMouseLeave(Sender: TObject);
        procedure txtDebtorsReportMouseEnter(Sender: TObject);
        procedure txtDebtorsReportMouseLeave(Sender: TObject);
        procedure txtControlStatusReportMouseEnter(Sender: TObject);
        procedure txtControlStatusReportMouseLeave(Sender: TObject);
        procedure AppHeaderClick(Sender: TObject);
        procedure imgHideBarClick(Sender: TObject);
        procedure AppHeaderMouseEnter(Sender: TObject);
        procedure AppHeaderMouseLeave(Sender: TObject);
        procedure sgControlStatusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgControlStatusMouseEnter(Sender: TObject);
        procedure sgControlStatusMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgControlStatusMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgControlStatusDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgAccountTypeDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgPersonRespDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgSalesRespDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgCustomerGrDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgPersonRespKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgSalesRespKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgAccountTypeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgCustomerGrKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgPersonRespMouseEnter(Sender: TObject);
        procedure sgSalesRespMouseEnter(Sender: TObject);
        procedure sgAccountTypeMouseEnter(Sender: TObject);
        procedure sgCustomerGrMouseEnter(Sender: TObject);
        procedure sgPersonRespMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgPersonRespMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgSalesRespMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgSalesRespMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgAccountTypeMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgAccountTypeMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgCustomerGrMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgCustomerGrMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    private
        var pAllowClose:    boolean;
        var pStartTime:     TTime;
    public
        // Helpers
        var LogText:            TThreadFileLog;
        var WinUserName:        string;
        var EventLogPath:       string;
        var DbConnect:          TADOConnection;
        var GroupList:          TLists;
        var GroupIdSel:         string;
        var GroupNmSel:         string;
        var AgeDateSel:         string;
        var OSAmount:           double;
        var GridPicture:        TImage;
        var AccessLevel:        string;
        var AccessMode:         string;
        var OpenItemsUpdate:    string;
        var OpenItemsStatus:    string;
        var IsConnected:        boolean;
        var CurrentEvents:      string;
        var OpenItemsRefs:      TOpenItemsRefs;
        var ControlStatusRefs:  TControlStatusRefs;
        procedure  DebugMsg(const Msg: String);
        procedure  TryInitConnection;
        procedure  ExecMessage(IsPostType: boolean; YOUR_INT: integer; YOUR_TEXT: string);
        procedure  UpdateOpenItemsRefs(SourceGrid: TStringGrid);
        procedure  UpdateControlStatusRefs(SourceGrid: TStringGrid);
        function   OleGetStr(RecordsetField: variant): string;
        function   FindKey(OpenedSection: string; KeyPosition: integer): string;
        function   WndCall(WinForm: TForm; Mode: integer): integer;
        function   MsgCall(WndType: integer; WndText: string): integer;
        procedure  SetSettingsPanel(Mode: integer);
        function   ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
        function   GetCoCode(CoPos: integer; GroupId: string): string;
        procedure  Find(ColumnNum: integer);
        procedure  SwitchTimers(state: integer);
        procedure  LoadImageFromStream(Image: TImage; const FileName: string);
        function   CDate(StrDate: string): TDate;
        function   ShowReport(ReportNumber: cardinal): cardinal;
        procedure  CopyFile(const Source, Dest: string);
        procedure  ResetTabsheetButtons;
        procedure  UnfoldReportsTab(Header: TPanel; Panel: TPanel; ShouldHide: boolean = false);
        procedure  SetPanelBorders;
        procedure  SetGridColumnWidths;
        procedure  SetGridRowHeights;
        procedure  SetGridThumbSizes;
        procedure  SetGridFocus;
        function   Explode(Text: string; SourceDelim: char): string;
        function   Implode(Text: Classes.TStrings; TargetDelim: char): string;
        function   CheckGivenPassword(Password: string): boolean;
        function   SetNewPassword(Password: string): boolean;
        function   AddressBookExclusion: boolean;
        function   CheckIfDate(StrDate: string): boolean;
    protected
        procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean; var Result: Boolean);
        // Process all Windows messgaes
        procedure  WndProc(var msg: Messages.TMessage); override;
    end;

    /// <remarks>
    ///     If library is written in Delphi, then Delphi types can be used as usual,
    ///     however, if library is written in 'C' or any other language, then
    ///     please use plain 'C' language types only, so instead of pascal 'string'
    ///     type, please use silly 'pchar' type, etc., also, in case of c# language,
    ///     please refer to manual on 'making C# dll library for delphi usage'
    /// </remarks>

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
    procedure MergeSort(grid: TStringgrid; var vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall; external Assembly;

    const Win32API = 'wtsapi32.dll';

    function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): Boolean; stdcall; external Win32API name 'WTSRegisterSessionNotification';
    function WTSUnRegisterSessionNotification(hWnd: HWND):               Boolean; stdcall; external Win32API name 'WTSUnRegisterSessionNotification';

{$I .\Include\Common.inc}

var
    MainForm :  TMainForm;


implementation


uses
    Filter, Tracker, Invoices, Actions, Calendar, About, AVSearch, Worker, SQL, Model, Settings, Database, UAC, AgeView, Transactions, Colors, EventLog,
    SendFeedback, ABSearch, MassMailer, Splash, Await, uCEFApplication;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------ DEBUGER OUTPUT //


procedure TMainForm.DebugMsg(const Msg: String);
begin
    OutputDebugString(PChar(Msg));
end;


// ---------------------------------------------------------------------------------------------------------------------------------------- WINDOWS MESSAGES //


/// <summary>
///     Listen to all Windows messages and react upon.
/// </summary>

procedure TMainForm.WndProc(var Msg: Messages.TMessage);
var
    CUID:   string;
    Param:  integer;
begin
    inherited;

    // CHROMIUM -------------------------------------------------------------------------------------------------------------------------------------------- //

    if Msg.Msg = WM_MOVE then
        if (ChromiumWindow <> nil) then
            ChromiumWindow.NotifyMoveOrResizeStarted;

    if Msg.Msg = WM_MOVING then
        if (ChromiumWindow <> nil) then
            ChromiumWindow.NotifyMoveOrResizeStarted;

    if Msg.Msg = WM_ENTERMENULOOP then
        if (Msg.wParam = 0) and (GlobalCEFApp <> nil) then
            GlobalCEFApp.OsmodalLoop:=True;

    if Msg.Msg = WM_EXITMENULOOP  then
        if (Msg.wParam = 0) and (GlobalCEFApp <> nil) then
            GlobalCEFApp.OsmodalLoop:=False;

    // INTERNAL MESSAGES BETWEEN WORKER THREADS AND MAIN THREAD -------------------------------------------------------------------------------------------- //

    if Msg.Msg = WM_GETINFO then
    begin

        // Debug line
        DebugMsg('WM_GETINFO RECEIVED');

        // Show message window, call from worker thread
        if ( (Msg.WParam > 0) and (Msg.WParam <= 4) ) and (not(string.IsNullOrEmpty(PChar(Msg.LParam)))) then
            MainForm.MsgCall(Msg.WParam, PChar(Msg.LParam));

        // Pool of number to be used: 10..40 for other events
        if (Msg.WParam >= 10) and (Msg.WParam <= 40) then
        begin

            // Status bar change
            if Msg.WParam = 10 then
                StatBar_TXT1.Caption:=PChar(Msg.LParam);

            // Connection with database server OK
            if Msg.WParam = 14 then
            begin
                MainForm.StatBar_TXT7.Font.Style:=[];
                MainForm.StatBar_TXT7.Caption   :='Connected with Microsoft SQL Server.';
            end;

            // Connection with database server lost
            if Msg.WParam = 15 then
            begin
                MainForm.StatBar_TXT7.Font.Style:=[fsBold];
                MainForm.StatBar_TXT7.Caption   :='Connection lost, re-connecting...';
            end;

            // MASS MAILER --------------------------------------------------------------------------------------------------------------------------------- //

            // Countdown processed emails
            if (Msg.WParam = 26) then
            begin
                Param:=StrToIntDef(PChar(Msg.LParam), -1);
                if Param > -1 then
                begin
                    ViewMailerForm.CustomerList.Items[Param].SubItems[3]:='Sent';
                    Dec(ViewMailerForm.ThreadCount);
                end;
            end;

            // Close Await window if e-mails have been processed by worker thread
            if (Msg.WParam = 27) and (PChar(Msg.LParam) = 'True') then
                AwaitForm.Close;

            // AWAIT WINDOW -------------------------------------------------------------------------------------------------------------------------------- //

            if (Msg.WParam = 28) then
            begin

                // Turn busy window on
                if (PCHar(Msg.LParam) = 'OFF') then
                    AwaitForm.Close;

                // Turn busy window off
                if (PCHar(Msg.LParam) = 'ON') then
                    AwaitForm.Show;

            end;

        end;

    end;

    // RECEIVE MESSAGE FROM EXTERNAL APPLICATION ----------------------------------------------------------------------------------------------------------- //

    if Msg.Msg = WM_EXTINFO then
    begin

        // Debug line
        DebugMsg('WM_EXTINFO RECEIVED');

        /// <remarks>
        ///     If WPARAM equals 14, then we expect LPARAM to return phone call duration from LYNCCALL.EXE.
        /// </remarks>

        if Msg.WParam = 14 then
            // Debug line
            DebugMsg(IntToStr(Msg.LParam));

        // Log time (seconds) in database "general comment" table
        if Msg.LParam > 0 then
        begin
            CUID:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1) , sgAgeView.Row];
            TTDailyComment.Create(
                CUID,
                False,
                True,
                Msg.LParam,
                '',
                False,
                False,
                False,
                True
            );
        end;

    end;

    // CLOSE UNITY WHEN WINDOWS IS SHUTTING DOWN ----------------------------------------------------------------------------------------------------------- //

    // Windows query for shutdown
    if Msg.Msg = WM_QUERYENDSESSION then
    begin
        LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_QUERYENDSESSION. Windows is going to be shut down. Closing ' + APPCAPTION + '...');
        pAllowClose:=True;
        Msg.Result:=1;
    end;

    // Windows is shutting down
    if Msg.Msg = WM_ENDSESSION then
    begin
        LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_ENDSESSION. Windows is shutting down...');
        pAllowClose:=True;
    end;

    // Power-management event has occurred (resume or susspend)
    if Msg.Msg = WM_POWERBROADCAST then
    begin

        // System is suspending operation
        if Msg.WParam = PBT_APMSUSPEND then
        begin
            // Turn off timers
            SwitchTimers(tmDisabled);
            // Disconnect
            InetTimer.Enabled:=False;
            DbConnect.Connected:=False;
            DbConnect:=nil;
            IsConnected:=False;
            MainForm.ExecMessage(False, conERROR, strNULL);
            LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_POWERBROADCAST with PBT_APMSUSPEND. Going into suspension mode, Unity is disconnected from server.');
        end;

        // Operation is resuming automatically from a low-power state
        // This message is sent every time the system resumes
        if Msg.WParam = PBT_APMRESUMEAUTOMATIC then
        begin
            // Turn on timer responsible for periodic connection check
            InetTimer.Enabled:=True;
            LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Windows Message detected: ' + IntToStr(Msg.Msg) + ' WM_POWERBROADCAST with PBT_APMRESUMEAUTOMATIC. Windows has resumed after being suspended.');
        end;

    end;

end;

/// <summary>
///     Get column reference on demand for Open Items string grid. The reason is, despite we do not change columns order
///     at run time programatically, it may be changed on server-side and that will be immediatelly reflected
///     in Open Items string grid that serves the user and program as a fast source access.
///     Additional purpose of the code is - to get the columns at once instead using ReturnColumn multiple times in given
///     method, this increase the overall performance of the code and decreases complexity.
/// </summary>

/// <remarks>
///     The nature of open items is that, it changes continuously, but due to ERP database workload during the day
///     we have decided to update the data in Open Items table few times a day (on regular basis).
/// </remarks>

procedure TMainForm.UpdateOpenItemsRefs(SourceGrid: TStringGrid);
begin
    OpenItemsRefs.CuidCol     :=SourceGrid.ReturnColumn(TOpenitems.CUID,      1, 1);
    OpenItemsRefs.OpenAmCol   :=SourceGrid.ReturnColumn(TOpenitems.OpenAm,    1, 1);
    OpenItemsRefs.PmtStatCol  :=SourceGrid.ReturnColumn(TOpenitems.PmtStat,   1, 1);
    OpenItemsRefs.CtrlCol     :=SourceGrid.ReturnColumn(TOpenitems.Ctrl,      1, 1);
    OpenItemsRefs.InvoNoCol   :=SourceGrid.ReturnColumn(TOpenitems.InvoNo,    1, 1);
    OpenItemsRefs.ValDtCol    :=SourceGrid.ReturnColumn(TOpenitems.ValDt,     1, 1);
    OpenItemsRefs.DueDtCol    :=SourceGrid.ReturnColumn(TOpenitems.DueDt,     1, 1);
    OpenItemsRefs.ISOCol      :=SourceGrid.ReturnColumn(TOpenitems.ISO,       1, 1);
    OpenItemsRefs.CurAmCol    :=SourceGrid.ReturnColumn(TOpenitems.CurAm,     1, 1);
    OpenItemsRefs.OpenCurAmCol:=SourceGrid.ReturnColumn(TOpenitems.OpenCurAm, 1, 1);
    OpenItemsRefs.Ad1Col      :=SourceGrid.ReturnColumn(TOpenitems.Ad1,       1, 1);
    OpenItemsRefs.Ad2Col      :=SourceGrid.ReturnColumn(TOpenitems.Ad2,       1, 1);
    OpenItemsRefs.Ad3Col      :=SourceGrid.ReturnColumn(TOpenitems.Ad3,       1, 1);
    OpenItemsRefs.PnoCol      :=SourceGrid.ReturnColumn(TOpenitems.Pno,       1, 1);
    OpenItemsRefs.PAreaCol    :=SourceGrid.ReturnColumn(TOpenitems.PArea,     1, 1);
end;

/// <summary>
///     Get column reference of Control Status table located in General Tables. Similarly to the "UpdateOpenItemsRefs" method, we use it
///     to decrease level of usage of ReturnColumn method.
/// </summary>

procedure TMainForm.UpdateControlStatusRefs(SourceGrid: TStringGrid);
begin
    ControlStatusRefs.Id         :=SourceGrid.ReturnColumn(TControlStatus.Id,   1, 1);
    ControlStatusRefs.Code       :=SourceGrid.ReturnColumn(TControlStatus.Code, 1, 1);
    ControlStatusRefs.Text       :=SourceGrid.ReturnColumn(TControlStatus.Text, 1, 1);
    ControlStatusRefs.Description:=SourceGrid.ReturnColumn(TControlStatus.Description, 1, 1);
end;

/// <summary>
///     Execute on "before popup" - ignore tab sheets and pop-ups.
/// </summary>

procedure TMainForm.Chromium_OnBeforePopup(Sender: TObject;
    const browser: ICefBrowser;
    const frame: ICefFrame;
    const targetUrl, targetFrameName: ustring;
    targetDisposition: TCefWindowOpenDisposition;
    userGesture: Boolean;
    const popupFeatures: TCefPopupFeatures;
    var windowInfo: TCefWindowInfo;
    var client: ICefClient;
    var settings: TCefBrowserSettings;
    var noJavascriptAccess: Boolean;
    var Result: Boolean);
begin
    Result:=(
                targetDisposition in
                [
                    WOD_NEW_FOREGROUND_TAB,
                    WOD_NEW_BACKGROUND_TAB,
                    WOD_NEW_POPUP,
                    WOD_NEW_WINDOW
                ]
            );
end;

/// <summary>
///     Initialize connection with database server.
/// </summary>

procedure TMainForm.TryInitConnection;
var
    DataBase: TDataBase;
begin
    DbConnect:=TADOConnection.Create(nil);
    DataBase :=TDataBase.Create(True);
    try
        if DataBase.Check = 0 then
        begin
            DataBase.InitializeConnection(MainThreadID, True, DbConnect);
            IsConnected:=True;
            SwitchTimers(tmEnabled);
        end
        else
        begin
            IsConnected:=False;
            SwitchTimers(tmDisabled);
        end;
    finally
        DataBase.Free;
    end;
end;

/// <summary>
///     Simple wrapper for PostMessage and SendMessage.
/// </summary>

procedure TMainForm.ExecMessage(IsPostType: boolean; YOUR_INT: Integer; YOUR_TEXT: string);
begin
  if IsPostType     then PostMessage(MainForm.Handle, WM_GETINFO, YOUR_INT, LPARAM(PCHAR(YOUR_TEXT)));
  if not IsPostType then SendMessage(MainForm.Handle, WM_GETINFO, YOUR_INT, LPARAM(PCHAR(YOUR_TEXT)));
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Use this when dealing with database datasets/recordset results, field may be null and thus must be converted into string.
/// </summary>

function TMainForm.OleGetStr(RecordsetField: variant): string;
begin
    {$D-}
    try
        OleGetStr:=RecordsetField;
    except
        { CASE OF NULL FIELD }
        OleGetStr:=VarToStr(RecordsetField);
    end;
    {$D+}
end;

/// <summary>
///     Return key value for given list position.
/// </summary>

function TMainForm.FindKey(OpenedSection: string; KeyPosition: integer): string;
var
    SL:         TStringList;
    Settings:   ISettings;
begin

    Result:=unNA;
    SL:=TStringList.Create;
    Settings:=TSettings.Create;

    Settings.GetSection(OpenedSection, SL);

    if KeyPosition > SL.Count then
        Exit
            else
                Result:=LeftStr(SL.Strings[KeyPosition], AnsiPos('=', SL.Strings[KeyPosition]) - 1);

end;

/// <summary>
///     Wrapper for calling moda or modless window.
/// </summary>

function TMainForm.WndCall(WinForm: TForm; Mode: integer): integer;
begin
    Result:=0;

    // Setup popups
    WinForm.PopupMode  :=pmAuto;
    WinForm.PopupParent:=MainForm;

    // Call window
    if Mode = stModal    then Result:=WinForm.ShowModal;
    if Mode = stModeless then WinForm.Show;

end;

/// <summary>
///     Wrapper for windows message boxes.
/// </summary>

function TMainForm.MsgCall(WndType: integer; WndText: string): integer;
begin
    Result:=0;
    if WndText = '' then Exit;
    if WndType = mcInfo      then Result:=Application.MessageBox(PChar(WndText), PChar(APPCAPTION), MB_OK       + MB_ICONINFORMATION);
    if WndType = mcWarn      then Result:=Application.MessageBox(PChar(WndText), PChar(APPCAPTION), MB_OK       + MB_ICONWARNING);
    if WndType = mcError     then Result:=Application.MessageBox(PChar(WndText), PChar(APPCAPTION), MB_OK       + MB_ICONERROR);
    if WndType = mcQuestion1 then Result:=Application.MessageBox(PChar(WndText), PChar(APPCAPTION), MB_OKCANCEL + MB_ICONQUESTION);
    if WndType = mcQuestion2 then Result:=Application.MessageBox(PChar(WndText), PChar(APPCAPTION), MB_YESNO    + MB_ICONQUESTION);
end;

/// <summary>
///     Lock/unlock administrator panel on Settings tabsheet.
/// </summary>

procedure TMainForm.SetSettingsPanel(Mode: integer);
begin
    if Mode = spLock then
    begin
        // Visibility on
        imgOFF.Visible         :=True;
        btnPassUpdate.Enabled  :=False;

        // Edit boxes
        EditCurrentPassword.Enabled:=False;
        EditNewPassword.Enabled:=False;
        EditNewPasswordConfirmation.Enabled:=False;
        EditCurrentPassword.Text:='';
        EditNewPassword.Text    :='';
        EditNewPasswordConfirmation.Text:='';
        EditPassword.Text:='';

        // String grids
        sgListSection.ClearAll(2, 0, 0, False);
        sgListValue.ClearAll(2, 0, 0, False);
        sgListSection.Row:=1;
        sgListValue.Row:=1;
        sgListSection.Visible:=False;
        sgListValue.Visible:=False;
        sgUAC.Visible:=False;
        sgGroups.Visible:=False;
        sgUAC.ClearAll(2, 0, 0, False);
        sgGroups.ClearAll(2, 0, 0, False);
        sgUAC.Row:=1;
        sgGroups.Row:=1;
        sgListSection.Enabled:=False;
        sgListValue.Enabled  :=False;
        sgUAC.Enabled        :=False;
        sgGroups.Enabled     :=False;

        btnUnlock.Caption:='Unlock';
        EditPassword.SetFocus;

    end;

    if Mode = spUnLock then
    begin
        // Setup headers
        sgListSection.Cols[0].Text:='Lp';
        sgListSection.Cols[1].Text:='Sections';
        sgListValue.Cols[0].Text  :='Lp';
        sgListValue.Cols[1].Text  :='Key';
        sgListValue.Cols[2].Text  :='Value';

        // Credentials
        btnPassUpdate.Enabled  :=True;
        EditCurrentPassword.Enabled:=True;
        EditNewPassword.Enabled :=True;
        EditNewPasswordConfirmation.Enabled:=True;

        // String grids
        sgUAC.Enabled:=True;
        sgGroups.Enabled:=True;
        sgListSection.Enabled:=True;
        sgListValue.Enabled:=True;
        sgListSectionClick(self);
        sgListSection.Row:=1;
        sgListValue.Row:=1;
        sgListSection.Visible:=True;
        sgListValue.Visible:=True;
        sgUAC.Visible:=True;
        sgGroups.Visible:=True;

        // Transparency off
        imgOFF.Visible:=False;

        btnUnlock.Caption:='Lock';
        EditPassword.SetFocus;
    end;

end;

/// <summary>
///     Co name convertion.
/// </summary>

function TMainForm.ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
var
    iCNT:  integer;
begin

    Result:= '';

    /// <remarks>
    ///     Used only for open items and aging view.
    /// </remarks>

    // Allow to convert '2020' to 'F2020', etc.
    if mode = 0 then
    begin
        if Length(CoNumber) = 4 then Result:=Prefix + CoNumber;
        if Length(CoNumber) = 3 then Result:=Prefix + '0'  + CoNumber;
        if Length(CoNumber) = 2 then Result:=Prefix + '00' + CoNumber;
    end;

    /// <remarks>
    ///     Used only to build GroupID.
    /// </remarks>

    // Converts from 2020 to 02020, 340 to 00340 and so on.
    if mode = 1 then
    begin
        if Length(CoNumber) = 4 then Result:='0'   + CoNumber;
        if Length(CoNumber) = 3 then Result:='00'  + CoNumber;
        if Length(CoNumber) = 2 then Result:='000' + CoNumber;
        if Length(CoNumber) = 1 then Result:='00000';
    end;

    // Converts from 02020 to 2020.
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

    // Converts from 2020 to 2020, 340 to 0340... .
    if mode = 3 then
    begin
        if Length(CoNumber) = 4 then Result:=CoNumber;
        if Length(CoNumber) = 3 then Result:='0'    + CoNumber;
        if Length(CoNumber) = 2 then Result:='00'   + CoNumber;
        if Length(CoNumber) = 1 then Result:='000'  + CoNumber;
    end;

end;

/// <summary>
///     Return specific CoCode from the given group.
/// </summary>

function TMainForm.GetCoCode(CoPos: integer; GroupId: string): string;
begin

    /// <remarks>
    ///     Group id format: series of 4 groups of 5 digits, i.e.: '020470034000043' must be read as follows:
    ///     1. 1ST CO CODE: 02047 (2047)
    ///     2. 2ND CO CODE: 00340 (340)
    ///     3. 3RD CO CODE: 00043 (43)
    ///     4. 4TH CO CODE: 00000 (0)
    /// </remarks>

    if CoPos = 1 then Result:=IntToStr(StrToInt(MidStr(GroupId, 1,  5)));
    if CoPos = 2 then Result:=IntToStr(StrToInt(MidStr(GroupId, 6,  5)));
    if CoPos = 3 then Result:=IntToStr(StrToInt(MidStr(GroupId, 11, 5)));
    if CoPos = 4 then Result:=IntToStr(StrToInt(MidStr(GroupId, 16, 5)));

end;

/// <summary>
///     Find other details (currency, division, agents) for given company code. It searches age view string grid.
/// </summary>

procedure TMainForm.Find(ColumnNum: integer);
var
    iCNT:  integer;
begin

    for iCNT:=1 to sgCoCodes.RowCount - 1 do
    begin
        if DetailsGrid.Cells[ColumnNum, 0] = sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.CO_CODE, 1, 1), iCNT] then
        begin
            DetailsGrid.Cells[ColumnNum, 1]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.COCURRENCY, 1, 1), iCNT];
            DetailsGrid.Cells[ColumnNum, 2]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.DIVISIONS,  1, 1), iCNT];
            DetailsGrid.Cells[ColumnNum, 3]:=sgCoCodes.Cells[sgCoCodes.ReturnColumn(TCompany.AGENTS,     1, 1), iCNT];
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

/// <summary>
///     Turn off or on given timers.
/// </summary>

procedure TMainForm.SwitchTimers(state: integer);
begin

    if state = tmEnabled then
    begin
        InvoiceScanTimer.Enabled :=True;
        FollowupPopup.Enabled    :=True;
        OILoader.Enabled         :=True;
    end;

    if state = tmDisabled then
    begin
        InvoiceScanTimer.Enabled :=False;
        FollowupPopup.Enabled    :=False;
        OILoader.Enabled         :=False;
    end;

end;

/// <summary>
///     oad desired image format to TImage component.
/// </summary>

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

/// <summary>
///     Convert string date to date format.
/// </summary>

function TMainForm.CDate(StrDate: string): TDate;
begin
    Result:=StrToDateDef(StrDate, NULLDATE);
end;

/// <summary>
///     Execute chromium reader for reporting.
/// </summary>

function TMainForm.ShowReport(ReportNumber: cardinal): cardinal;
var
    Settings: ISettings;
    AppParam: string;
begin
    Settings:=TSettings.Create;
    AppParam:=Settings.GetStringValue(ApplicationDetails, 'REPORT_Report' + IntToStr(ReportNumber), 'about:blank');

    Result:=ShellExecute(
        MainForm.Handle,
        seOpen,
        PChar(Settings.GetAppDir + UnityReader),
        PChar(AppParam),
        nil,
        SW_SHOWNORMAL
    );

end;

/// <summary>
///     Copy file between locations.
/// </summary>

procedure TMainForm.CopyFile(const Source, Dest: string);
var
    SourceStream: TFileStream;
    DestStream:   TFileStream;
begin
    DestStream:=nil;

    SourceStream:=TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
    try
        DestStream:=TFileStream.Create(Dest, fmCreate or fmShareExclusive);
        DestStream.CopyFrom(SourceStream, SourceStream.Size);

        {$WARN SYMBOL_PLATFORM OFF}
        // You may use platform independent: function FileSetDate(const FileName: string; Age: Integer): Integer; overload;
        FileSetDate(DestStream.Handle, FileGetDate(SourceStream.Handle));
        {$WARN SYMBOL_PLATFORM ON}

    finally
        DestStream.Free;
        SourceStream.Free;
    end;

end;

/// <summary>
///     Switch off bold for all fonts used in top menu.
/// </summary>

procedure TMainForm.ResetTabsheetButtons;
begin
    txtStart.Font.Style       :=[];
    txtStart.Font.Color       :=clBlack;
    txtReports.Font.Style     :=[];
    txtReports.Font.Color     :=clBlack;
    txtDebtors.Font.Style     :=[];
    txtDebtors.Font.Color     :=clBlack;
    txtTracker.Font.Style     :=[];
    txtTracker.Font.Color     :=clBlack;
    txtAddressBook.Font.Style :=[];
    txtAddressBook.Font.Color :=clBlack;
    txtOpenItems.Font.Style   :=[];
    txtOpenItems.Font.Color   :=clBlack;
    txtUnidentified.Font.Style:=[];
    txtUnidentified.Font.Color:=clBlack;
    txtQueries.Font.Style     :=[];
    txtQueries.Font.Color     :=clBlack;
    txtTables.Font.Style      :=[];
    txtTables.Font.Color      :=clBlack;
    txtSettings.Font.Style    :=[];
    txtSettings.Font.Color    :=clBlack;
end;

/// <summary>
///     Check if header panel is already folded (based on its heigh) and unfold it back.
/// </summary>
/// <remarks>
///     To display additional content, we extend button panel. Header panel that contains
///     button panels must be also extended.
/// </remarks>

procedure TMainForm.UnfoldReportsTab(Header: TPanel; Panel: TPanel; ShouldHide: boolean = false);
begin

    if not(ShouldHide) then
    begin
        if Panel.Height > 32 then
        begin
            // Fold
            Panel.Height:=32;
            Header.Height:=57;
        end
    else
        begin
            // Unfold
            Panel.Height:=116;
            Header.Height:=Panel.Height + 13;
        end;
    end
    else
    begin
        Panel.Height:=32;
        Header.Height:=57;
    end;

end;

/// <summary>
///     Draw custom border around panels.
/// </summary>

procedure TMainForm.SetPanelBorders;
begin

    /// <remarks>
    ///     TPanel component must have properties such as BevelInner, BevelKind and BevelOuter and BorderStyle set to none.
    /// </remarks>

    AppHeader.PanelBorders            (clWhite, clSkyBlue, clWhite,   clWhite,   clWhite);
    PanelAgeView.PanelBorders         (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelOpenItems.PanelBorders       (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelAddressBook.PanelBorders     (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelInvoiceTracker.PanelBorders  (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelCoCodes.PanelBorders         (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelControlStatus.PanelBorders   (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelPaidInfo.PanelBorders        (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelPerson.PanelBorders          (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelPmtTerms.PanelBorders        (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelGroup3.PanelBorders          (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelSettingsSections.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelSettingsValues.PanelBorders  (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelUAC.PanelBorders             (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelGroups.PanelBorders          (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelSalesResp.PanelBorders       (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelPersonResp.PanelBorders      (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelCustomerGr.PanelBorders      (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelAccountType.PanelBorders     (clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

/// <summary>
///     Set default column width for string grids.
/// </summary>

procedure TMainForm.SetGridColumnWidths;
begin
    sgOpenItems.SetColWidth     (10, 20, 400);
    sgAddressBook.SetColWidth   (10, 20, 400);
    sgListValue.SetColWidth     (25, 20, 400);
    sgListSection.SetColWidth   (25, 20, 400);
    sgInvoiceTracker.SetColWidth(10, 20, 400);
    sgCoCodes.SetColWidth       (10, 30, 400);
    sgControlStatus.SetColWidth (10, 30, 400);
    sgPaidInfo.SetColWidth      (10, 30, 400);
    sgPerson.SetColWidth        (10, 30, 400);
    sgGroup3.SetColWidth        (10, 30, 400);
    sgPmtTerms.SetColWidth      (10, 30, 400);
    sgGroups.SetColWidth        (10, 20, 400);
    sgUAC.SetColWidth           (10, 20, 400);
    sgSalesResp.SetColWidth     (10, 20, 400);
    sgPersonResp.SetColWidth    (10, 20, 400);
    sgCustomerGr.SetColWidth    (10, 20, 400);
    sgAccountType.SetColWidth   (10, 20, 400);
end;

/// <summary>
///     Set row height for string grids.
/// </summary>

procedure TMainForm.SetGridRowHeights;
begin
    sgOpenItems.SetRowHeight     (sgRowHeight, 25);
    sgAddressBook.SetRowHeight   (sgRowHeight, 25);
    sgListValue.SetRowHeight     (sgRowHeight, 25);
    sgListSection.SetRowHeight   (sgRowHeight, 25);
    sgAgeView.SetRowHeight       (sgRowHeight, 25);
    sgInvoiceTracker.SetRowHeight(sgRowHeight, 25);
    sgCoCodes.SetRowHeight       (sgRowHeight, 25);
    sgControlStatus.SetRowHeight (sgRowHeight, 25);
    sgPaidInfo.SetRowHeight      (sgRowHeight, 25);
    sgPerson.SetRowHeight        (sgRowHeight, 25);
    sgGroup3.SetRowHeight        (sgRowHeight, 25);
    sgPmtTerms.SetRowHeight      (sgRowHeight, 25);
    sgGroups.SetRowHeight        (sgRowHeight, 25);
    sgUAC.SetRowHeight           (sgRowHeight, 25);
    sgSalesResp.SetRowHeight     (sgRowHeight, 25);
    sgPersonResp.SetRowHeight    (sgRowHeight, 25);
    sgAccountType.SetRowHeight   (sgRowHeight, 25);
    sgCustomerGr.SetRowHeight    (sgRowHeight, 25);
end;

/// <summary>
///     Set thumb size for scroll
/// </summary>

procedure TMainForm.SetGridThumbSizes;
begin
    sgAgeView.AutoThumbSize;
    sgOpenItems.AutoThumbSize;
    sgAddressBook.AutoThumbSize;
    sgInvoiceTracker.AutoThumbSize;
    sgCoCodes.AutoThumbSize;
    sgControlStatus.AutoThumbSize;
    sgPmtTerms.AutoThumbSize;
    sgPaidInfo.AutoThumbSize;
    sgPerson.AutoThumbSize;
    sgGroup3.AutoThumbSize;
    sgListSection.AutoThumbSize;
    sgListValue.AutoThumbSize;
    sgUAC.AutoThumbSize;
    sgGroups.AutoThumbSize;
    sgSalesResp.AutoThumbSize;
    sgPersonResp.AutoThumbSize;
    sgAccountType.AutoThumbSize;
    sgCustomerGr.AutoThumbSize;
end;

/// <summary>
///     Switch off focusing on all grids.
/// </summary>

procedure TMainForm.SetGridFocus;
begin
//    sgAgeView.FHideFocusRect       :=True;
//    sgOpenItems.FHideFocusRect     :=True;
//    sgAddressBook.FHideFocusRect   :=True;
//    sgInvoiceTracker.FHideFocusRect:=True;
//    sgCoCodes.FHideFocusRect       :=True;
//    sgControlStatus.FHideFocusRect :=True;
//    sgPmtTerms.FHideFocusRect      :=True;
//    sgPaidInfo.FHideFocusRect      :=True;
//    sgPerson.FHideFocusRect        :=True;
//    sgGroup3.FHideFocusRect        :=True;
//    sgListSection.FHideFocusRect   :=True;
//    sgListValue.FHideFocusRect     :=True;
//    sgUAC.FHideFocusRect           :=True;
//    sgGroups.FHideFocusRect        :=True;
//    sgSalesResp.FHideFocusRect     :=True;
//    sgPersonResp.FHideFocusRect    :=True;
//    sgAccountType.FHideFocusRect   :=True;
//    sgCustomerGr.FHideFocusRect    :=True;
end;

/// <summary>
///     Convert to multiline string.
/// </summary>

function TMainForm.Explode(Text: string; SourceDelim: char): string;
begin
    Result:=StringReplace(Text, SourceDelim, CRLF, [rfReplaceAll]);
end;

/// <summary>
///     Convert multiline string to one line string.
/// </summary>

function TMainForm.Implode(Text: Classes.TStrings; TargetDelim: char): string;
var
    iCNT:  integer;
    Str:   string;
begin

    for iCNT:=0 to Text.Count do
    begin
        if iCNT < Text.Count then
            Str:=Str + Text.Strings[iCNT] + TargetDelim
                else
                    Str:=Str + Text.Strings[iCNT];
    end;

    Result:=Str;

end;

/// <summary>
///     Validate password.
/// </summary>

function TMainForm.CheckGivenPassword(Password: string): boolean;
var
    Settings:   ISettings;
    Hash:       string;
    ReHashed:   boolean;
begin

    Result:=False;

    Settings:=TSettings.Create;
    Hash:=Settings.GetStringValue(PasswordSection, 'HASH', '');

    if Hash = '' then
        Exit
            else
                Result:=TBcrypt.CheckPassword(Password, Hash, ReHashed);

end;

/// <summary>
///     Hash given password.
/// </summary>

function TMainForm.SetNewPassword(Password: string): boolean;
var
    Settings:    ISettings;
    HashPasswd:  string;
begin

    // Exit condition
    Result:=False;
    if Password = '' then Exit;

    // Generate hash and salt it
    HashPasswd:=TBcrypt.HashPassword(Password);

    // Save it
    Settings:=TSettings.Create;
    Settings.SetStringValue(PasswordSection, 'HASH', HashPasswd);
    Settings.Encode(AppConfig);
    Result:=True;

end;

/// <summary>
///     Indicates editable columns.
/// </summary>

function TMainForm.AddressBookExclusion: boolean;
begin
    if
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.EMAILS, 1, 1)
        )
    or
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.PHONE_NUMBERS, 1, 1)
        )
    or
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.CONTACT, 1, 1)
        )
    or
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.ESTATEMENTS, 1, 1)
        )
    then
        // Do not exclude column from editing
        Result:=False
            else
                // Exclude column from editing
                Result:=True;
end;

/// <summary>
///     validate string date.
/// </summary>

function TMainForm.CheckIfDate(StrDate: string): boolean;
begin
  Result:=False;

  if StrToDateDef(StrDate, NULLDATE) <> NULLDATE then
    Result:=True;

end;

// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


/// <summary>
///     Initialize application, load all settings, establish database connectivity.
/// </summary>

procedure TMainForm.FormCreate(Sender: TObject);

    /// <summary>
    ///     Nested method for Splash Screen update with loading details.
    /// </summary>

    procedure OnCreateJob(Text: string);
    begin
        if Assigned(SplashForm) then
        begin
            SplashForm.TextStatus.Caption:='Application initialization: ' + Text;
            SplashForm.Update;
        end;
    end;

/// <summary>
///     Local variables, inaccessibe for nested method.
/// </summary>

var
    AppVersion:    string;
    Settings:      ISettings;
    UserControl:   TUserControl;
    Transactions:  TTransactions;
    NowTime:       TTime;
    iCNT:          integer;
    MapTable:      TTGeneralTables;
begin

    LogText    :=TThreadFileLog.Create;
    AppVersion :=GetBuildInfoAsString;
    CurrentEvents:='# -- SESSION START --';
    pAllowClose:=False;

    // -------------------------------------------------------------------------------------------------------------------------------- GET AND SET SETTINGS //
    OnCreateJob(spSetting);

    Settings :=TSettings.Create;
    MainForm.Caption :=Settings.GetStringValue(ApplicationDetails, 'WND_MAIN', APPCAPTION);
    GroupName.Caption:=Settings.GetStringValue(ApplicationDetails, 'GROUP_NAME', 'n/a');

    WinUserName :=Settings.GetWinUserName;
    EventLogPath:=Settings.GetPathEventLog;

    GridPicture:=TImage.Create(MainForm);
    GridPicture.SetBounds(0, 0, 16, 16);
    LoadImageFromStream(GridPicture, Settings.GetPathGridImage);

    /// <remarks>
    ///     Window position. Do not change Default Monitor and Position.
    /// </remarks>

    MainForm.DefaultMonitor:=dmDesktop;
    MainForm.Position      :=poDefaultSizeOnly;
    MainForm.Top           :=Settings.GetIntegerValue(ApplicationDetails, 'WINDOW_TOP',  0);
    MainForm.Left          :=Settings.GetIntegerValue(ApplicationDetails, 'WINDOW_LEFT', 0);

    /// <remarks>
    ///     "InteTimer" is excluded from below list because it is controlled by "InitializeConnection" method.
    /// </remarks>

    /// <remarks>
    ///     Default value 900000 miliseconds = 15 minutes.
    /// </remarks>

    InvoiceScanTimer.Interval:=Settings.GetIntegerValue(TimersSettings, 'INVOICE_SCANNER', 900000);

    /// <remarks>
    ///     Default value 1800000 miliseconds = 30 minutes.
    /// </remarks>

    FollowupPopup.Interval:=Settings.GetIntegerValue(TimersSettings, 'FOLLOWUP_CHECKER', 1800000);

    /// <remarks>
    ///     Default value 300000 miliseconds = 5 minutes.
    /// </remarks>

    OILoader.Interval:=Settings.GetIntegerValue(TimersSettings, 'OI_LOADER', 300000);

    /// <remarks>
    ///     Get risk class values and convert default decimal separator.
    /// </remarks>

    if FormatSettings.DecimalSeparator = ',' then
    begin
        procRISKA.Caption:=FloatToStr(StrToFloat(Settings.GetStringValue(RiskClassDetails, 'CLASS_A_MAX', RISK_CLASS_A)) * 100) + '%';
        procRISKB.Caption:=FloatToStr(StrToFloat(Settings.GetStringValue(RiskClassDetails, 'CLASS_B_MAX', RISK_CLASS_B)) * 100) + '%';
        procRISKC.Caption:=FloatToStr(StrToFloat(Settings.GetStringValue(RiskClassDetails, 'CLASS_C_MAX', RISK_CLASS_C)) * 100) + '%';
    end;

    if FormatSettings.DecimalSeparator = '.' then
    begin
        procRISKA.Caption:=FloatToStr(StrToFloat(StringReplace(Settings.GetStringValue(RiskClassDetails, 'CLASS_A_MAX', RISK_CLASS_A), ',', '.', [rfReplaceAll])) * 100) + '%';
        procRISKB.Caption:=FloatToStr(StrToFloat(StringReplace(Settings.GetStringValue(RiskClassDetails, 'CLASS_B_MAX', RISK_CLASS_B), ',', '.', [rfReplaceAll])) * 100) + '%';
        procRISKC.Caption:=FloatToStr(StrToFloat(StringReplace(Settings.GetStringValue(RiskClassDetails, 'CLASS_C_MAX', RISK_CLASS_C), ',', '.', [rfReplaceAll])) * 100) + '%';
    end;

    /// <remarks>
    ///     Hide all tabs on TPageControl component and set "Debtors" [TabSheet1] as starting page.
    /// </remarks>

    for iCNT:=0 to MyPages.PageCount - 1 do MyPages.Pages[iCNT].TabVisible:=False;
    MyPages.ActivePage:=TabSheet1;

    /// <summary>
    ///     Main form captions.
    /// </summary>

    Cap01.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT01', 'EMPTY'), [fsBold]);
    Cap02.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT02', 'EMPTY'), [fsBold]);
    Cap03.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT03', 'EMPTY'), [fsBold]);
    Cap05.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT05', 'EMPTY'), [fsBold]);
    Cap06.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT06', 'EMPTY'), [fsBold]);
    Cap07.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT07', 'EMPTY'), [fsBold]);
    Cap24.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS1TXT08', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     Open items captions.
    /// </summary>

    Cap10.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS2TXT01', 'EMPTY'), [fsBold]);
    Cap11.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS2TXT02', 'EMPTY'), [fsBold]);
    Cap12.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS2TXT03', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     Address Book captions.
    /// </summary>

    Cap13.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS3TXT01', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     Invoice tracker captions.
    /// </summary>

    Cap43.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS4TXT01', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     Unidentified transactions.
    /// </summary>

    Cap61.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS6TXT01', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     General tables captions.
    /// </summary>

    Cap15.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS7TXT01', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     Settings captions.
    /// </summary>

    Cap21.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS8TXT01', 'EMPTY'), [fsBold]);
    Cap22.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS8TXT02', 'EMPTY'), [fsBold]);
    Cap23.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS8TXT03', 'EMPTY'), [fsBold]);
    Cap27.ShapeText(10, 1, Settings.GetStringValue(TabSheetsCaps, 'TS8TXT04', 'EMPTY'), [fsBold]);

    /// <summary>
    ///     Aging buckets displayed on Age View.
    /// </summary>

    tR1.Caption:=Settings.GetStringValue(AgingRanges,'RANGE1A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE1B','');
    tR2.Caption:=Settings.GetStringValue(AgingRanges,'RANGE2A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE2B','');
    tR3.Caption:=Settings.GetStringValue(AgingRanges,'RANGE3A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE3B','');
    tR4.Caption:=Settings.GetStringValue(AgingRanges,'RANGE4A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE4B','');
    tR5.Caption:=Settings.GetStringValue(AgingRanges,'RANGE5A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE5B','');
    tR6.Caption:=Settings.GetStringValue(AgingRanges,'RANGE6A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE6B','');

    /// <summary>
    ///     Age report summary.
    /// </summary>

    Text21.Caption:=Settings.GetStringValue(AgingRanges,'RANGE1A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE3B','') + ':';
    Text22.Caption:=Settings.GetStringValue(AgingRanges,'RANGE4A','') + ' - ' + Settings.GetStringValue(AgingRanges,'RANGE6B','') + ':';

    // ------------------------------------------------------------------------------------------------------------------------------- DATABASE CONNECTIVITY //
    OnCreateJob(spConnecting);

    /// <remarks>
    ///     Establish connection with server.
    /// </remarks>

    TryInitConnection;

    // ---------------------------------------------------------------------------------------------------------------------------------------- GET UAC DATA //
    OnCreateJob(spUserAccess);

    UserControl:=TUserControl.Create(DbConnect);
    try
        UserControl.UserName:=WinUserName;
        AccessLevel         :=UserControl.GetAccessData(adAccessLevel);

        // Quit if username is not found
        if AccessLevel = '' then
        begin
            MsgCall(mcError, 'Cannot find account for user alias: ' + UpperCase(WinUserName) + '. Please contact your administrator. Application will be closed.');
            ExitProcess(0);
        end;

        AccessMode:=UserControl.GetAccessData(adAccessMode);
        if AccessMode = adAccessFull  then Action_FullView.Checked :=True;
        if AccessMode = adAccessBasic then Action_BasicView.Checked:=True;
        UserControl.GetGroupList(GroupList, GroupListBox);
        UserControl.GetAgeDates(GroupListDates, GroupList[0, 0]);

    finally
        UserControl.Free;
    end;

    /// <remarks>
    ///     Depending on access level, enable/disable features.
    /// </remarks>

    {TODO -oTomek -cReplaceWith : ApprovalMatrix}

    // Restricted for "ADMINS"
    if AccessLevel <> acADMIN then
    begin
      DetailsGrid.Enabled   :=False;
      ReloadCover.Visible   :=True;
      ReloadCover.Cursor    :=crNo;
      GroupListDates.Enabled:=False;
    end;

    // Not allowed for "RO" users
    if AccessLevel = acReadOnly then
    begin
      Action_Tracker.Enabled        :=False;
      Action_AddToBook.Enabled      :=False;
      ActionsForm.DailyCom.Enabled  :=False;
      ActionsForm.GeneralCom.Enabled:=False;
      btnUpdateAB.Enabled           :=False;
    end;

    // ------------------------------------------------------------------------------------------------------------------ ASYNC IN SERIES LOAD OF MAP TABLES //
    OnCreateJob(spMapping);

    MapTable:=TTGeneralTables.Create(TblSalesResponsible, sgSalesResp, '', '', False);
    MapTable.WaitFor;
    MapTable:=nil;

    MapTable:=TTGeneralTables.Create(TblPersonResponsible, sgPersonResp, '', '', False);
    MapTable.WaitFor;
    MapTable:=nil;

    MapTable:=TTGeneralTables.Create(TblAccountType, sgAccountType, '', '', False);
    MapTable.WaitFor;
    MapTable:=nil;

    MapTable:=TTGeneralTables.Create(TblCustomerGroup, sgCustomerGr, '', '', False);
    MapTable.WaitFor;
    MapTable:=nil;

    MapTable:=TTGeneralTables.Create(TblGroup3, sgGroup3, '', '', False);
    MapTable.WaitFor;
    MapTable:=nil;

    // ------------------------------------------------------------------------------------------------------------------------ ASYNC LOAD OF GENERAL TABLES //
    OnCreateJob(spGeneral);

    TTGeneralTables.Create(
        TblCompany,
        sgCoCodes,
        TCompany.CO_CODE + COMMA +
        TCompany.BRANCH + COMMA +
        TCompany.CONAME + COMMA +
        TCompany.COADDRESS + COMMA +
        TCompany.VATNO + COMMA +
        TCompany.DUNS + COMMA +
        TCompany.COUNTRY + COMMA +
        TCompany.CITY + COMMA +
        TCompany.FMANAGER + COMMA +
        TCompany.Telephone + COMMA +
        TCompany.COTYPE + COMMA +
        TCompany.COCURRENCY + COMMA +
        TCompany.INTEREST_RATE + COMMA +
        TCompany.KPI_OVERDUE_TARGET + COMMA +
        TCompany.KPI_UNALLOCATED_TARGET + COMMA +
        TCompany.AGENTS + COMMA +
        TCompany.DIVISIONS,
        ORDER + TCompany.CO_CODE + ASC
    );

    TTGeneralTables.Create(TblPmtterms,      sgPmtTerms);
    TTGeneralTables.Create(TblPaidinfo,      sgPaidInfo);
    TTGeneralTables.Create(TblPerson,        sgPerson);
    TTGeneralTables.Create(TblControlStatus, sgControlStatus);

    // ----------------------------------------------------------------------------------------------------------------------------- SYNC LOAD AGE SNAPSHOTS //
    OnCreateJob(spSnapshots);

    if not(string.IsNullOrEmpty(GroupListBox.Text)) and not(string.IsNullOrEmpty(GroupListDates.Text)) then
    begin
        GroupIdSel:=GroupList[GroupListBox.ItemIndex, 0];
        GroupNmSel:=GroupList[GroupListBox.ItemIndex, 1];
        AgeDateSel:=GroupListDates.Text;
        sgAgeView.Enabled:=True;

        Transactions:=TTransactions.Create(DbConnect);
        try
            OpenItemsUpdate:=Transactions.GetDateTime(gdDateTime);
            OpenItemsStatus:=Transactions.GetStatus(OpenItemsUpdate);
            if string.IsNullOrEmpty(OpenItemsUpdate) then
            begin
                MsgCall(mcWarn, 'Cannot find open items in database. Please contact IT support.');
                TTReadAgeView.Create(thNullParameter, smRanges);
            end
            else
                TTReadAgeView.Create(thCallOpenItems, smRanges);
        finally
            Transactions.Free;
        end;
    end;

    // ------------------------------------------------------------------------------------------------------------------------------------------- FINISHING //
    OnCreateJob(spFinishing);

    NowTime   :=Now;
    pStartTime:=Now;
    FormatDateTime('hh:mm:ss', NowTime);
    FormatDateTime('hh:mm:ss', pStartTime);

    StatBar_TXT1.Caption:=stReady;
    StatBar_TXT2.Caption:=WinUserName;
    StatBar_TXT3.Caption:=DateToStr(Now);

    UpTime.Enabled     :=True;
    CurrentTime.Enabled:=True;

    LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Application version = ' + AppVersion);
    LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: User SID = ' + GetCurrentUserSid);

end;

procedure TMainForm.FormShow(Sender: TObject);
begin

    // Update thumb size
    FormResize(self);

    // Draw panel borders
    SetPanelBorders;

    // Update grids width, height and thumb size
    SetGridColumnWidths;
    SetGridRowHeights;
    (* SetGridThumbSizes; *)

    // Initialize Chromium
    ChromiumWindow.ChromiumBrowser.OnBeforePopup:=Chromium_OnBeforePopup;
    if not(ChromiumWindow.CreateBrowser) then
        ChromiumTimer.Enabled:=True;

end;

procedure TMainForm.FormResize(Sender: TObject);
begin

    /// <remarks>
    ///     Do not use it. Scroll thumb size is buggy in Windows.
    /// </remarks>

    // SetGridThumbSizes;

end;

/// <summary>
///     Load Unity Info web page.
/// </summary>

procedure TMainForm.ChromiumWindowAfterCreated(Sender: TObject);
var
    URL:        string;
    Settings:   ISettings;
begin
    Settings:=TSettings.Create;
    URL:=Settings.GetStringValue(ApplicationDetails, 'START_PAGE', 'about:blank');
    try
        ChromiumWindow.LoadURL(WideString(URL));
    except
        on E: exception do
            LogText.Log(EventLogPath, '[Chromium] Cannot load URL: ' + URL + '. The error has been thrown: ' + E.Message);
    end;
end;


// --------------------------------------------------------------------------------------------------------------------------------------- CLOSE APPLICATION //


/// <summary>
///     Execute when application receives windows message on shutting down the system; or user press key combination of <ALT> + <Y>; or
///     simply clicks close button on application caption bar. Standard behaviour of application close button is changed to minimisation
///     of the application to system tray (removes icon from taskbar).
/// </summary>

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
    Today:    string;
    UserLogs: TDataTables;
begin

    // Go minimize and hide from taskbar | do not close
    if not pAllowClose then
    begin
        CanClose:=False;
        ShowWindow(Handle, SW_MINIMIZE);
        Hide();
    end
    else
    // Shutdown application
    begin

        Visible :=False;
        ChromiumWindow.CloseBrowser(True);

        ExecMessage(False, mcStatusBar, 'Ending session...');
        CurrentEvents:=CurrentEvents + '# -- SESSION END --';

        // Update user event log in database
        UserLogs:=TDataTables.Create(DbConnect);
        try
            Today:=FormatDateTime(gdDateTimeFormat, Now);

            // Columns
            UserLogs.Columns.Add(TUnityEventLogs.UserAlias);
            UserLogs.Columns.Add(TUnityEventLogs.DateTimeStamp);
            UserLogs.Columns.Add(TUnityEventLogs.AppEventLog);
            UserLogs.Columns.Add(TUnityEventLogs.AppName);
            // Values
            UserLogs.Values.Add(WinUserName.ToUpper);
            UserLogs.Values.Add(Today);
            UserLogs.Values.Add(CurrentEvents);
            UserLogs.Values.Add('Unity for Debt Management');
            // Insert
            UserLogs.InsertInto(TblUnityEventLogs, ttExplicit);

        finally
            UserLogs.Free;
        end;

        CurrentEvents:=EmptyStr;
        LogText.Log(EventLogPath, 'Application closed.');
        CanClose:=True;

    end;
end;

/// <summary>
///     Save window position and layout; and disconnect from the server.
/// </summary>

procedure TMainForm.FormDestroy(Sender: TObject);
var
    Settings: ISettings;
begin

    if sgAgeView.RowCount > 2 then
        sgAgeView.SaveLayout(ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);

    Settings:=TSettings.Create;
    Settings.SetIntegerValue(ApplicationDetails, 'WINDOW_TOP',  MainForm.Top);
    Settings.SetIntegerValue(ApplicationDetails, 'WINDOW_LEFT', MainForm.Left);
    if MainForm.WindowState = wsNormal    then Settings.SetStringValue(ApplicationDetails,  'WINDOW_STATE', 'wsNormal');
    if MainForm.WindowState = wsMaximized then Settings.SetStringValue(ApplicationDetails,  'WINDOW_STATE', 'wsMaximized');
    if MainForm.WindowState = wsMinimized then Settings.SetStringValue(ApplicationDetails,  'WINDOW_STATE', 'wsMinimized');

    Settings.Encode(AppConfig);

    LogText.Free;
    DbConnect.Close;
    DbConnect:=nil;

end;


// -------------------------------------------------------------------------------------------------------------------------------------------------- TIMERS //


/// <summary>
///     Initiaize ChromiumWindow with time lag.
/// </summary>

procedure TMainForm.ChromiumTimerTimer(Sender: TObject);
begin
    ChromiumTimer.Enabled:=False;
    if not(ChromiumWindow.CreateBrowser) and not(ChromiumWindow.Initialized) then
        ChromiumTimer.Enabled:=True
end;

/// <summary>
///     Count current follow-ups and display in notification baloon.
/// </summary>

procedure TMainForm.FollowupPopupTimer(Sender: TObject);
var
    iCNT: integer;
    Sum:  integer;
begin

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
        then
            Inc(Sum);

    if not (Sum = 0) then
    begin
        TrayIcon.Visible:=True;
        TrayIcon.BalloonHint:='Hello, you have ' + IntToStr(Sum) + ' follow-up dates registered for today.' + CRLF +
                              'Let''s bother some customers and collect some money money!' + CRLF;
        TrayIcon.ShowBalloonHint;
    end;

end;

/// <summary>
///     Check internet connection on regular basis.
/// </summary>

procedure TMainForm.InetTimerTimer(Sender: TObject);
begin
    TTCheckServerConnection.Create(False);
end;

/// <summary>
///     Call Invoice Tracker scanning method.
/// </summary>

procedure TMainForm.InvoiceScanTimerTimer(Sender: TObject);
begin
    TTInvoiceTrackerScanner.Create(False);
end;

/// <summary>
///     Autoloader for open items.
/// </summary>

procedure TMainForm.OILoaderTimer(Sender: TObject);
begin
    LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Calling open items scanner...');
    TTOpenItemsScanner.Create;
end;

/// <summary>
///     Display current time.
/// </summary>

procedure TMainForm.CurrentTimeTimer(Sender: TObject);
begin
    StatBar_TXT4.Caption:=TimeToStr(Now);
end;

/// <summary>
///     Display current uptime.
/// </summary>

procedure TMainForm.UpTimeTimer(Sender: TObject);
var
    Result: TTime;
begin
    Result:=Now - pStartTime;
    StatBar_TXT5.Caption:=TimeToStr(Result);
end;


// --------------------------------------------------------------------------------------------------------------------------------------------- POPUP MENUS //


// ------------------------------------------------------------------------------------------------------------------------------------- COMMON MENU ACTIONS //


/// <summary>
///     Export entire grid content to CSV file.
/// </summary>

procedure TMainForm.Action_ExportTransactionsClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused   then sgOpenItems.ExportCSV(CSVExport, '|');
    if sgCoCodes.Focused     then sgCoCodes.ExportCSV(CSVExport, '|');
    if sgPaidInfo.Focused    then sgPaidInfo.ExportCSV(CSVExport, '|');
    if sgPerson.Focused      then sgPerson.ExportCSV(CSVExport, '|');
    if sgGroup3.Focused      then sgGroup3.ExportCSV(CSVExport, '|');
    if sgPmtTerms.Focused    then sgPmtTerms.ExportCSV(CSVExport, '|');
    if sgListValue.Focused   then sgListValue.ExportCSV(CSVExport, '|');
    if sgListSection.Focused then sgListSection.ExportCSV(CSVExport, '|');
    if sgGroups.Focused      then sgGroups.ExportCSV(CSVExport, '|');
    if sgUAC.Focused         then sgUAC.ExportCSV(CSVExport, '|');

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.ExportCSV(CSVExport, '|');

end;

/// <summary>
///     Select all items on focused string grid.
/// </summary>

procedure TMainForm.Action_SelectAllClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused   then sgOpenItems.SelectAll;
    if sgCoCodes.Focused     then sgCoCodes.SelectAll;
    if sgPaidInfo.Focused    then sgPaidInfo.SelectAll;
    if sgPerson.Focused      then sgPerson.SelectAll;
    if sgGroup3.Focused      then sgGroup3.SelectAll;
    if sgPmtTerms.Focused    then sgPmtTerms.SelectAll;
    if sgListValue.Focused   then sgListValue.SelectAll;
    if sgListSection.Focused then sgListSection.SelectAll;
    if sgGroups.Focused      then sgGroups.SelectAll;
    if sgUAC.Focused         then sgUAC.SelectAll;

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.SelectAll;

end;

/// <summary>
///     Copy to clipboard selected cells on focused string grid.
/// </summary>

procedure TMainForm.Action_CopyToCBClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused   then sgOpenItems.CopyCutPaste(adCopy);
    if sgCoCodes.Focused     then sgCoCodes.CopyCutPaste(adCopy);
    if sgPaidInfo.Focused    then sgPaidInfo.CopyCutPaste(adCopy);
    if sgPerson.Focused      then sgPerson.CopyCutPaste(adCopy);
    if sgGroup3.Focused      then sgGroup3.CopyCutPaste(adCopy);
    if sgPmtTerms.Focused    then sgPmtTerms.CopyCutPaste(adCopy);
    if sgListValue.Focused   then sgListValue.CopyCutPaste(adCopy);
    if sgListSection.Focused then sgListSection.CopyCutPaste(adCopy);
    if sgGroups.Focused      then sgGroups.CopyCutPaste(adCopy);
    if sgUAC.Focused         then sgUAC.CopyCutPaste(adCopy);

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.CopyCutPaste(adCopy);

end;

/// <summary>
///     Set column width.
/// </summary>

procedure TMainForm.Action_AutoColumnClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused   then sgOpenItems.SetColWidth(10, 20, 400);
    if sgCoCodes.Focused     then sgCoCodes.SetColWidth(10, 20, 400);
    if sgPaidInfo.Focused    then sgPaidInfo.SetColWidth(10, 20, 400);
    if sgPerson.Focused      then sgPerson.SetColWidth(10, 20, 400);
    if sgGroup3.Focused      then sgGroup3.SetColWidth(10, 20, 400);
    if sgPmtTerms.Focused    then sgPmtTerms.SetColWidth(10, 20, 400);
    if sgListValue.Focused   then sgListValue.SetColWidth(25, 20, 400);
    if sgListSection.Focused then sgListSection.SetColWidth(25, 20, 400);
    if sgGroups.Focused      then sgGroups.SetColWidth(10, 20, 400);
    if sgUAC.Focused         then sgUAC.SetColWidth(10, 20, 400);

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.SetColWidth(10, 20, 400);

end;


// --------------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK MENU //


/// <summary>
///     Address Book context menu.
/// </summary>

procedure TMainForm.BookPopupPopup(Sender: TObject);
begin

    Action_ShowMyEntries.Caption:='Show ' + UpperCase(MainForm.WinUserName) + ' entries';

    // Check if user select a range
    if (sgAddressBook.Selection.Bottom - sgAddressBook.Selection.Top) > 0 then
        // We allow to delete only one line at the time
        Action_DelRow.Enabled:=False
            else
                Action_DelRow.Enabled:=True;

end;

/// <summary>
///     Allow to cut/paste the data from cell(s) if selected column is marked for editing.
/// </summary>

procedure TMainForm.Action_CutClick(Sender: TObject);
begin

    if AddressBookExclusion then
    begin
        MsgCall(mcWarn, 'This column is locked for editing.');
        Exit;
    end;

    sgAddressBook.CopyCutPaste(adCut);
    sgAddressBook.RecordRowsAffected;

end;

procedure TMainForm.Action_PasteClick(Sender: TObject);
begin

  if AddressBookExclusion then
  begin
    MsgCall(mcWarn, 'This column is locked for editing.');
    Exit;
  end;

  sgAddressBook.CopyCutPaste(adPaste);
  sgAddressBook.RecordRowsAffected;
end;

/// <summary>
///     Copy :)
/// </summary>

procedure TMainForm.Action_CopyClick(Sender: TObject);
begin
    sgAddressBook.CopyCutPaste(adCopy);
end;

/// <summary>
///     Deete given SCUID from database.
/// </summary>

procedure TMainForm.Action_DelRowClick(Sender: TObject);
var
    DataTables: TDataTables;
begin

    if MsgCall(mcQuestion2, 'Are you sure you want to delete this customer?' + CRLF + 'This operation cannot be reverted.') = IDNO then Exit;

    DataTables:=TDataTables.Create(DbConnect);
    try

        DataTables.DeleteRecord(TblAddressbook, TAddressBook.SCUID, DataTables.CleanStr(sgAddressBook.Cells[2, sgAddressBook.Row], False), ttExplicit);

        if DataTables.RowsAffected > 0 then
        begin
            sgAddressBook.DeleteRowFrom(1, 1)
        end
        else
        begin
            LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Cannot delete selected row (rows affected: ' + IntToStr(DataTables.RowsAffected) + ').');
            MainForm.ExecMessage(False, mcError, 'Cannot delete selected row. Please contact IT support.');
        end;

    finally
        DataTables.Free;
    end;

end;

/// <summary>
///     Open search window.
/// </summary>

procedure TMainForm.Action_SearchBookClick(Sender: TObject);
begin
    WndCall(ViewSearchForm, stModeless);
end;

/// <summary>
///     Show all entries in Address Book from database.
/// </summary>

procedure TMainForm.Action_ShowAsIsClick(Sender: TObject);
begin
    TTAddressBook.Create(
        adOpenAll,
        sgAddressBook,
        '',
        '',
        '',
        '',
        '',
        ''
    );
end;

/// <summary>
///     Show Address Book entries
/// </summary>

procedure TMainForm.Action_ShowMyEntriesClick(Sender: TObject);
begin
    TTAddressBook.Create(
        adOpenForUser,
        sgAddressBook,
        '',
        '',
        '',
        '',
        '',
        WHERE + TAddressBook.USER_ALIAS + EQUAL + QuotedStr(MainForm.WinUserName)
    );
end;

/// <summary>
///     Set column width.
/// </summary>

procedure TMainForm.Action_ColumnWidthClick(Sender: TObject);
begin
    sgAddressBook.SetColWidth(40, 10, 400);
end;


// ---------------------------------------------------------------------------------------------------------------------------- MAIN FORM MENU (SYSTEM TRAY) //


/// <summary>
///     Bring to front main application window.
/// </summary>

procedure TMainForm.Action_ShowAppClick(Sender: TObject);
begin
    ShowWindow(Handle, SW_NORMAL);
    Show();
    Application.BringToFront;
end;

/// <summary>
///     Hide main application window to task bar.
/// </summary>

procedure TMainForm.Action_HideAppClick(Sender: TObject);
begin
    ShowWindow(Handle, SW_MINIMIZE);
    Hide();
end;

/// <summary>
///     Allow to set stay on top.
/// </summary>

procedure TMainForm.Action_OnTopClick(Sender: TObject);
begin
    if Action_OnTop.Checked = False then
    begin
        MainForm.FormStyle:=fsStayOnTop;
        Action_OnTop.Checked:=True;
    end
    else
    begin
        MainForm.FormStyle:=fsNormal;
        Action_OnTop.Checked:=False;
    end;
end;

/// <summary>
///     Show user feedback window.
/// </summary>

procedure TMainForm.Action_ReportClick(Sender: TObject);
begin
    WndCall(ReportForm, stModal);
end;

/// <summary>
///     Open help file with basic manual on application usage. Use external PDF file.
/// </summary>

procedure TMainForm.Action_HelpClick(Sender: TObject);
begin
    // code here ...
end;

/// <summary>
///     Display about window.
/// </summary>

procedure TMainForm.Action_AboutClick(Sender: TObject);
begin
    WndCall(AboutForm, stModal);
end;

/// <summary>
///     Close button (disabled).
/// </summary>

procedure TMainForm.Action_CloseClick(Sender: TObject);
begin
  (* DO NOTHING *)
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ AGE VIEW //


/// <summary>
///     Execute before context menu is shown.
/// </summary>

procedure TMainForm.AgeViewPopupPopup(Sender: TObject);
begin

    // Only admins and rw users can use addressbook and invoice tracker
    if AccessLevel = acReadOnly then Exit;

    // Enable or disable filter removal
    if FilterForm.InUse then
        Action_RemoveFilters.Enabled:=True
            else
                Action_RemoveFilters.Enabled:=False;

end;

/// <summary>
///     Execute Action Log window with Lync abilities.
/// </summary>

procedure TMainForm.Action_LyncCallClick(Sender: TObject);
begin

    if IsConnected then
    begin
        if MainForm.StatBar_TXT1.Caption = stReady then
            WndCall(ActionsForm, stModal)
                else
                    MainForm.MsgCall(mcWarn, 'Wait until "Ready" status and try again.');
    end
        else
            MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

/// <summary>
///     Add item(s) to Invoice Tracker list.
/// </summary>

procedure TMainForm.Action_TrackerClick(Sender: TObject);

    // Common variables
    var
        iCNT: integer;
        Item: TListItem;
        CustNumber: string;
        CoCode: string;

    // Nested method
    function GetSCUID(position: integer): string;
    begin
        CustNumber:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), position];
        CoCode    :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), position];
        Result    :=CustNumber + MainForm.ConvertName(CoCode, 'F', 3);
    end;

begin

    if IsConnected then
    begin
        TrackerForm.CustomerList.Clear;

        // One customer
        if (sgAgeView.Selection.Top - sgAgeView.Selection.Bottom) = 0 then
        begin
            Item:=TrackerForm.CustomerList.Items.Add;
            Item.Caption:=IntToStr(sgAgeView.Row);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(GetSCUID(sgAgeView.Row));
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), sgAgeView.Row]);
        end
        // Many customers
        else
        begin
            for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
            begin
                if sgAgeView.RowHeights[iCNT] <> sgRowHidden then
                begin
                    Item:=TrackerForm.CustomerList.Items.Add;
                    Item.Caption:=IntToStr(iCNT);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), iCNT]);
                    Item.SubItems.Add(GetSCUID(iCNT));
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), iCNT]);
                end;
            end;
        end;

        WndCall(TrackerForm, stModal);

    end
    else
    begin
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    end;

end;

/// <summary>
///     Add customer(s) to address book.
/// </summary>

procedure TMainForm.Action_AddToBookClick(Sender: TObject);
begin
    if IsConnected then
        TTAddressBook.Create(
            adInsert,
            sgAgeView,
            '',
            '',
            '',
            '',
            '',
            ''
        )
    else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

/// <summary>
///     Add customer(s) to mass mailer.
/// </summary>

procedure TMainForm.Action_MassMailerClick(Sender: TObject);
var
    iCNT:       integer;
    Item:       TListItem;
    SCUID:      string;
    CoCode:     string;
    CustNumber: string;
    CustName:   string;
begin

    if IsConnected then
    begin
        ViewMailerForm.CustomerList.Clear;

        // One customer
        if (sgAgeView.Selection.Top - sgAgeView.Selection.Bottom) = 0 then
        begin
            Item:=ViewMailerForm.CustomerList.Items.Add;
            Item.Caption:=IntToStr(sgAgeView.Row);
            CustName  :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), sgAgeView.Row];
            CustNumber:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), sgAgeView.Row];
            CoCode    :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), sgAgeView.Row];
            SCUID     :=CustNumber + MainForm.ConvertName(CoCode, 'F', 3);
            Item.SubItems.Add(SCUID);
            Item.SubItems.Add(CustName);
            Item.SubItems.Add('Not Found');
            Item.SubItems.Add('No');
        end
        // Many customers
        else
        begin
            for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
            begin
                if sgAgeView.RowHeights[iCNT] <> sgRowHidden then
                begin
                    Item:=ViewMailerForm.CustomerList.Items.Add;
                    Item.Caption:=IntToStr(iCNT);
                    CustName  :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), iCNT];
                    CustNumber:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), iCNT];
                    CoCode    :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), iCNT];
                    SCUID     :=CustNumber + MainForm.ConvertName(CoCode, 'F', 3);
                    Item.SubItems.Add(SCUID);
                    Item.SubItems.Add(CustName);
                    Item.SubItems.Add('Not Found');
                    Item.SubItems.Add('No');
                end;
            end;
        end;

        WndCall(ViewMailerForm, stModal);

    end
    else
    begin
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    end;

end;

/// <summary>
///     Add follow-up to selected group of customers.
/// </summary>

procedure TMainForm.Action_AddFollowUpGroupClick(Sender: TObject);
var
    iCNT: integer;
begin

    Screen.Cursor:=crHourGlass;
    CalendarForm.CalendarMode:=cfGetDate;
    MainForm.WndCall(CalendarForm, 0);

    // IF SELECTED MORE THAN ONE CUSTOMER, ASSIGN GIVEN DATE TO SELECTED CUSTOMERS
    if CalendarForm.SelectedDate <> NULLDATE then
    begin
        for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do

            if sgAgeView.RowHeights[iCNT] <> sgRowHidden then
                CalendarForm.SetFollowUp(CalendarForm.SelectedDate, sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1), iCNT], iCNT);

            LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ''GeneralComment'' table with column FollowUp has been updated with ' + DateToStr(CalendarForm.SelectedDate) + ' for multiple items.');
    end;

    Screen.Cursor:=crDefault;

end;

/// <summary>
///     Remove selected follow-up.
/// </summary>

procedure TMainForm.Action_RemoveFollowUpsClick(Sender: TObject);
var
    iCNT: integer;
begin

    Screen.Cursor:=crHourGlass;

    for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
    begin

        if sgAgeView.RowHeights[iCNT] <> sgRowHidden then
        begin
            TTGeneralComment.Create(
                sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1), iCNT],
                strNULL,
                SPACE,
                strNULL,
                strNULL,
                strNULL,
                False
            );
            MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneral.fFOLLOWUP, 1, 1), iCNT]:=SPACE;
        end;

    end;

    LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ''GeneralComment'' table with column FollowUp has been updated with removal for multiple items.');

    Screen.Cursor:=crDefault;

end;


// ------------------------------------------------------------------------------------------------------------------------------------ FILTER AGE VIEW GRID //

/// <summary>
///     Below methods setups FilterForm properties and execute filter window.
/// </summary>

// Filter via INF7
procedure TMainForm.Action_INF7_FilterClick(Sender: TObject);
begin
    FilterForm.FColName:=TSnapshots.fINF7;
    FilterForm.FOverdue:=TSnapshots.fOVERDUE;
    FilterForm.FGrid   :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltINF7;
    WndCall(FilterForm, stModal);
end;

// Filter via INF4
procedure TMainForm.Action_INF4_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fINF4;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltINF4;
    WndCall(FilterForm, stModal);
end;

// Filter via COCODE
procedure TMainForm.Action_CoCode_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fCO_CODE;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltCOCODE;
    WndCall(FilterForm, stModal);
end;

// Filter via AGENT
procedure TMainForm.Action_Agent_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fAGENT;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltAGENT;
    WndCall(FilterForm, stModal);
end;

// Filter via DIVISION
procedure TMainForm.Action_Division_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fDIVISION;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltDIVISION;
    WndCall(FilterForm, stModal);
end;

// Filter via FOLLOW UP
procedure TMainForm.Action_FollowUp_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneral.fFOLLOWUP;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltFOLLOWUP;
    WndCall(FilterForm, stModal);
end;

// Filter via GROUP 3
procedure TMainForm.Action_Gr3_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fGROUP3;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltGR3;
    WndCall(FilterForm, stModal);
end;

// Filter via FREE1
procedure TMainForm.Action_Free1Click(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneral.Free1;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltFree1;
    WndCall(FilterForm, stModal);
end;

// Filter via FREE 2
procedure TMainForm.Action_Free2Click(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneral.Free2;
    FilterForm.FOverdue  :=TSnapshots.fOVERDUE;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=fltFree2;
    WndCall(FilterForm, stModal);
end;

// Filter via OVERDUE
procedure TMainForm.Action_OverduesClick(Sender: TObject);
begin
//
end;

// Filter via TOTAL AMOUNT
procedure TMainForm.Action_TotalAmountClick(Sender: TObject);
begin
//
end;

// Filter via RANGE1
procedure TMainForm.Action_Range1Click(Sender: TObject);
begin
//
end;

// Filter via RANGE2
procedure TMainForm.Action_Range2Click(Sender: TObject);
begin
//
end;

// Filter via RANGE3
procedure TMainForm.Action_Range3Click(Sender: TObject);
begin
//
end;

// Filter via RANGE4
procedure TMainForm.Action_Range4Click(Sender: TObject);
begin
//
end;

// Filter via RANGE5
procedure TMainForm.Action_Range5Click(Sender: TObject);
begin
//
end;

// Filter via RANGE6
procedure TMainForm.Action_Range6Click(Sender: TObject);
begin
//
end;

// Filter via  REMOVE ALL FILTERS
procedure TMainForm.Action_RemoveFiltersClick(Sender: TObject);
var
    iCNT:    integer;
    AgeView: TAgeView;
begin

    sgAgeView.Freeze(True);

    for iCNT:=1 to sgAgeView.RowCount - 1 do
        sgAgeView.RowHeights[iCNT]:=sgRowHeight;

    FilterForm.FilterClearAll;

    // Re-compute aging summary

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

/// <summary>
///     Allow to exclude non-overdue items.
/// </summary>

procedure TMainForm.Action_OverdueClick(Sender: TObject);
begin
    if Action_Overdue.Checked
        then
            Action_Overdue.Checked:=False
                else
                    Action_Overdue.Checked:=True;
end;

/// <summary>
///     Display window for searching customer.
/// </summary>

procedure TMainForm.Action_SearchClick(Sender: TObject);
begin
    SearchForm.SGrid     :=MainForm.sgAgeView;
    SearchForm.SColName  :=TSnapshots.fCUSTOMER_NAME;
    SearchForm.SColNumber:=TSnapshots.fCUSTOMER_NUMBER;
    WndCall(SearchForm, stModeless);
end;

/// <summary>
///     Show payment term for selected customer.
/// </summary>

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
                TblPmtterms,
                sgAgeView.Cells[
                    sgAgeView.ReturnColumn(TSnapshots.fCO_CODE, 1, 1),
                    sgAgeView.Row
                ]
            )
        );

    finally

        AgeView.Free;

    end;

end;

/// <summary>
///     Show assigned person term for selected customer.
/// </summary>

procedure TMainForm.Action_PersonClick(Sender: TObject);
var
    AgeView: TAgeView;
begin

    AgeView:=TAgeView.Create(DbConnect);

    try

        MsgCall(
            mcInfo,
            'Person assigned: ' +
            AgeView.GetData(
                sgAgeView.Cells[
                    sgAgeView.ReturnColumn(TSnapshots.fPERSON, 1, 1),
                    sgAgeView.Row
                ],
                TblPerson,
                sgAgeView.Cells[
                    sgAgeView.ReturnColumn(TSnapshots.fCO_CODE, 1, 1),
                    sgAgeView.Row
                ]
            )
        );

    finally

        AgeView.Free;

    end;

end;

/// <summary>
///     Export aging view (report) to Microsoft Excel file.
/// </summary>
/// <remarks>
///     Requires installed Microsopft Excel 2013 or higher.
/// </remarks>

procedure TMainForm.Action_ToExceClick(Sender: TObject);
begin
    TTExcelExport.Create;
end;

/// <summary>
///     Export aging view (report) to CSV file. Delimiter is fixed on vertical pipe.
/// </summary>

procedure TMainForm.Action_ExportCSVClick(Sender: TObject);
begin
    sgAgeView.ExportCSV(CSVExport, '|');
end;

/// <summary>
///     Hide or show aging summary displayed on the bottom of the page.
/// </summary>

procedure TMainForm.Action_HideSummaryClick(Sender: TObject);
begin
    if Action_HideSummary.Checked then
    begin
        Footer1.Visible:=False;
        //PanelAgeView.Margins.Bottom:=17;
        //sgAgeViewShape.Margins.Bottom:=0;
        Action_HideSummary.Checked:=False;
    end
    else
    begin
        Footer1.Visible:=True;
        //PanelAgeView.Margins.Bottom:=0;
        //sgAgeViewShape.Margins.Bottom:=12;
        Action_HideSummary.Checked:=True;
    end;
end;

/// <summary>
///     Auto column re-size.
/// </summary>

procedure TMainForm.Action_AutoColumnSizeClick(Sender: TObject);
begin
    MainForm.sgAgeView.SetColWidth(10, 20, 400);
end;

/// <summary>
///     Show only basic view defined in configuration file.
/// </summary>

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

    // Tick
    Action_BasicView.Checked:=True;
    Action_FullView.Checked :=False;

end;

/// <summary>
///     Show all available columns (defined in configuration file).
/// </summary>

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

    // Tick
    Action_BasicView.Checked:=False;
    Action_FullView.Checked :=True;

end;

/// <summary>
///     Display window to setup follow-up colors (font, background and foreground).
/// </summary>

procedure TMainForm.Action_FollowUpColorsClick(Sender: TObject);
begin
    WndCall(ColorsForm, stModal);
end;

/// <summary>
///     Row highlight (on/off).
/// </summary>

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


// ------------------------------------------------------------------------------------------------------------------------------------ INVOICE TRACKER MENU //


/// <summary>
///     Remove selected customer from list.
/// </summary>

procedure TMainForm.Action_RemoveClick(Sender: TObject);
begin

    // Exit condition
    if not(IsConnected) then
    begin
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit;
    end;

    TrackerForm.CUID:=sgInvoiceTracker.Cells[sgInvoiceTracker.ReturnColumn(TTracker.CUID, 1, 1), sgInvoiceTracker.Row];

    // R/W user can remove item
    if (MainForm.AccessLevel = acReadWrite) and (UpperCase(MainForm.WinUserName) = UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
        if MsgCall(mcQuestion2, 'Are you sure you want to remove selected customer?') = IDYES then
            TrackerForm.Delete;

    // R/W user cannot remove other item
    if (MainForm.AccessLevel = acReadWrite) and (UpperCase(MainForm.WinUserName) <> UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
        MsgCall(mcWarn, 'You cannot remove someone''s else item.');

    // Administrator can remove any item
    if (MainForm.AccessLevel = acADMIN) then
        if MsgCall(mcQuestion2, 'Are you sure you want to remove selected customer?') = IDYES then
            TrackerForm.Delete;

    // Read only user cannot remove anything
    if (MainForm.AccessLevel = acReadOnly) then MsgCall(mcWarn, 'You don''t have permission to remove items.');

end;

/// <summary>
///     Show sent invoices for given CUID.
/// </summary>

procedure TMainForm.Action_ShowRegisteredClick(Sender: TObject);
begin
    WndCall(InvoicesForm, stModal);
end;

/// <summary>
///     Show customers registered by logged user.
/// </summary>

procedure TMainForm.Action_ShowMyClick(Sender: TObject);
begin
    if IsConnected then
        TTInvoiceTrackerRefresh.Create(UpperCase(MainForm.WinUserName))
            else
                MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

/// <summary>
///     Show all registered customers.
/// </summary>

procedure TMainForm.Action_ShowAllClick(Sender: TObject);
begin
    if IsConnected then
        TTInvoiceTrackerRefresh.Create('*')
            else
                MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- TRYICON CALLS //


/// <summary>
///     Show the application form when user double click application icon visible on system tray.
/// </summary>

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
    MainForm.Action_ShowAppClick(self);
end;


// ------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS | ENTITY GROUP //


/// <summary>
///     Update list for selected group of companies.
/// </summary>

procedure TMainForm.GroupListBoxSelect(Sender: TObject);
var
    UserControl: TUserControl;
begin

    if not(IsConnected) then
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
            LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: "GetAgeDates" returned false. Cannot get list of age dates for selected group (' + GroupList[GroupListBox.ItemIndex, 0] + ').');
        end;

    finally
        UserControl.Free;
    end;

end;


// ---------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS | TABSHEETS //


/// <summary>
///     Refresh all items registered with Invoice Tracer.
/// </summary>

procedure TMainForm.TabSheet4Show(Sender: TObject);
begin
    //TTInvoiceTrackerRefresh.Create('*');
end;

{ ------------------------------------------------------------------------------------------------------------ MAKE PAYMENT TERMS AND PAID INFO TABLES HEIGHT }

/// <summary>
///     Setup tables height for payment term tabe and paid info table. General table is fixed.
/// </summary>

procedure TMainForm.TabSheet7Show(Sender: TObject);
begin
    sgPmtTerms.Height:=Round(0.5 * sgCoCodes.Height);
    sgPaidInfo.Height:=Round(0.5 * sgCoCodes.Height);
end;

procedure TMainForm.TabSheet7Resize(Sender: TObject);
begin
    TabSheet7Show(self);
end;

/// <summary>
///     Force lock settings panel. This is necessary, when administrator open settings panel and go to other tab without locking it. That prevents from
///     leaving unlocked settings panel by mistake.
/// </summary>

procedure TMainForm.TabSheet8Show(Sender: TObject);
begin
    SetSettingsPanel(spLock);
end;


// -------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS | GRIDS //


/// <summary>
///     Force range select.
/// </summary>

procedure TMainForm.sgAgeViewClick(Sender: TObject);
begin
    sgAgeView.Options:=sgAgeView.Options - [goEditing];
    sgAgeView.Options:=sgAgeView.Options - [goAlwaysShowEditor];
    sgAgeView.SetFocus;
    sgAgeView.EditorMode:=False;
end;

/// <summary>
///     Move column and update SQL column array.
/// </summary>

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
            // Setting up dimensions

            /// <remarks>
            ///     "High" method returns number of rows counting from zero
            ///     while "setlength" method setup array counting from one
            ///     therefore, we need to add one to match dimensions.
            /// </remarks>

            SqlRows:=high(sgAgeView.SqlColumns);
            Inc(SqlRows);
            SetLength(Temp, SqlRows, 2);
            TmpRows:=high(Temp);

            // Copy SQL array to temp array

            /// <remarks>
            ///     Do not use "copy" method.
            /// </remarks>

            for iCNT:=0 to TmpRows do
                for jCNT:=0 to 1 do
                    Temp[iCNT, jCNT]:=sgAgeView.SqlColumns[iCNT, jCNT];

            // Update titles in SQL column array

            for iCNT:=0 to sgAgeView.ColCount - 1 do
                sgAgeView.SqlColumns[iCNT, 1]:=sgAgeView.Cells[iCNT, 0];

            // Re-write other SQL columns from temp

            sgAgeView.SqlColumns[ToIndex, 0]:=Temp[FromIndex, 0];

            // Move column from right to left

            if FromIndex > ToIndex then
                for iCNT:=ToIndex to (FromIndex - 1) do
                    sgAgeView.SqlColumns[iCNT + 1, 0]:=Temp[iCNT, 0];

            // Move from left to right

            if FromIndex < ToIndex then
                for iCNT:=(FromIndex + 1) to ToIndex do
                    sgAgeView.SqlColumns[iCNT - 1, 0]:=Temp[iCNT, 0];
        except
            on E: Exception do
                MainForm.MsgCall(mcWarn, 'Unexpected error has occured. Description: ' + E.Message + '. Please contact IT support.')
        end;

    finally

        sgAgeView.SaveLayout(ColumnWidthName, ColumnOrderName, ColumnNames, ColumnPrefix);
        Temp:=nil;

    end;

end;

/// <summary>
///     Display Action Log.
/// </summary>

procedure TMainForm.sgAgeViewDblClick(Sender: TObject);
begin
    Action_LyncCallClick(Self);
end;

/// <summary>
///     Allow or disallow edit cell in Address Book. First column are not editable.
/// </summary>

procedure TMainForm.sgAddressBookDblClick(Sender: TObject);
begin

    if (sgAddressBook.Col = 1) then
        Exit;

    sgAddressBook.Options:=sgAddressBook.Options + [goEditing];

end;

/// <summary>
///     Disallow (quit) editing Address Book when user clicks on string grid component.
/// </summary>

procedure TMainForm.sgAddressBookClick(Sender: TObject);
begin
    sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
end;

/// <summary>
///     Display list of invoices sent by Invoice Tracker.
/// </summary>

procedure TMainForm.sgInvoiceTrackerDblClick(Sender: TObject);
begin
    if IsConnected then
        WndCall(InvoicesForm, stModal)
            else
                MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;


// ----------------------------------------------------------------------------------------------------------------------- CUSTOMIZE DRAWING OF STRING GRIDS //


procedure TMainForm.sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    Settings:    ISettings;
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
    iCNT:        integer;
    Width:       integer;
    AgeViewCUID: string;
begin

    // Skip header
    if ARow = 0 then Exit;

    // Find column numbers for given column name
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

    // Draw selected row | skip headers
    sgAgeView.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);

    // Mark bold all customers marked with risk class "A"
    if (ACol = Col14) and (sgAgeView.Cells[Col15, ARow] = 'A') then
    begin
        sgAgeView.Canvas.Font.Style:=[fsBold];
        sgAgeView.Canvas.FillRect(Rect);
        sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
    end;

    // Draw only if not selected
    if not(gdSelected in State) then
    begin

        // Highlight follow-up column
        if not (CDate(sgAgeView.Cells[ACol, ARow]) = 0) then
        begin
            Settings:=TSettings.Create;

            // Future days
            if (ACol = Col12) and (CDate(sgAgeView.Cells[ACol, ARow]) > CDate(StatBar_TXT3.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Settings.FutureBColor;
                sgAgeView.Canvas.Font.Color :=Settings.FutureFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

            // Today
            if (ACol = Col12) and (CDate(sgAgeView.Cells[ACol, ARow]) = CDate(StatBar_TXT3.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Settings.TodayBColor;
                sgAgeView.Canvas.Font.Color :=Settings.TodayFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

            // Past days
            if (ACol = Col12) and (CDate(sgAgeView.Cells[ACol, ARow]) < CDate(StatBar_TXT3.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Settings.PastBColor;
                sgAgeView.Canvas.Font.Color :=Settings.PastFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;
        end;

        // Highlight risk class "A"
        if (ACol = Col15) and (sgAgeView.Cells[Col15, ARow] = 'A') then
        begin
            sgAgeView.Canvas.Brush.Color:=BClassA;
            sgAgeView.Canvas.Font.Color :=FClassA;
            sgAgeView.Canvas.FillRect(Rect);
            sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
        end;

        // Highlight risk class "B"
        if (ACol = Col15) and (sgAgeView.Cells[Col15, ARow] = 'B') then
        begin
            sgAgeView.Canvas.Brush.Color:=BClassB;
            sgAgeView.Canvas.Font.Color :=FClassB;
            sgAgeView.Canvas.FillRect(Rect);
            sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
        end;

        // Highlight risk class "C"
        if (ACol = Col15) and (sgAgeView.Cells[Col15, ARow] = 'C') then
        begin
            sgAgeView.Canvas.Brush.Color:=BClassC;
            sgAgeView.Canvas.Font.Color :=FClassC;
            sgAgeView.Canvas.FillRect(Rect);
            sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
        end;

        /// <remarks>
        ///     Mark customers with picture, if it is registered on Invoice Tracker list.
        ///     We loop through loaded list on another string grid component. Therefore,
        ///     changes in database will not impact age view as long as Tracker List is
        ///     not refreshed.
        /// </remarks>

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

    end;

    /// <remarks>
    ///     After all drawing (when cells not selected) is done, change font for numeric values. This shoud be executed always last.
    /// </remarks>

    if (ACol = Col1)  or (ACol = Col2) or (ACol = Col3) or
       (ACol = Col4)  or (ACol = Col5) or (ACol = Col6) or
       (ACol = Col7)  or (ACol = Col8) or (ACol = Col9) or
       (ACol = Col10) or (ACol = Col11)
    then
        sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clBlack);

end;

procedure TMainForm.sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    Col1: integer;
    Col2: integer;
    Col3: integer;
    Col4: integer;
    Col5: integer;
begin

    if ARow = 0 then Exit;

    Col1:=sgOpenItems.ReturnColumn(TOpenitems.OpenCurAm, 1, 1);
    Col2:=sgOpenItems.ReturnColumn(TOpenitems.OpenAm,    1, 1);
    Col3:=sgOpenItems.ReturnColumn(TOpenitems.CurAm,     1, 1);
    Col4:=sgOpenItems.ReturnColumn(TOpenitems.Am,        1, 1);
    Col5:=sgOpenItems.ReturnColumn(TOpenitems.PmtStat,   1, 1);

    // Selection
    MainForm.sgOpenItems.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);

    // Numeric values colors
    if
        (
            ACol = Col1
        )
    or
        (
            ACol = Col2
        )
    or
        (
            ACol = Col3
        )
    or
        (
            ACol = Col4
        )
    or
        (
            ACol = Col5
        )
    then
        MainForm.sgOpenItems.ColorValues(ARow, ACol, Rect, clRed, clBlack);

end;


// ------------------------------------------------------------------------------------------------------------------------------- STRING GRID ROW SELECTION //


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

procedure TMainForm.sgControlStatusDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgControlStatus.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgAccountTypeDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgAccountType.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgPersonRespDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgPersonResp.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgSalesRespDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgSalesResp.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

procedure TMainForm.sgCustomerGrDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgCustomerGr.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;


// ------------------------------------------------------------------------------------------------------------------------ SETTING PANEL STRING GRID EVENTS //


/// <summary>
///     List all keys with theirs values.
/// </summary>

procedure TMainForm.sgListSectionSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
    Keys:       TStringList;
    Values:     TStringList;
    Settings:   ISettings;
    iCNT:       integer;
    junk:       string;
    clean:      string;
begin

    CanSelect:=True;

    Keys:=TStringList.Create();
    Values:=TStringList.Create();

    Settings:=TSettings.Create;
    try
        Settings.GetSection(sgListSection.Cells[ACol, ARow], Keys);
        Settings.GetSectionValues(sgListSection.Cells[ACol, ARow], Values);
        sgListValue.RowCount:=Keys.Count + 1;

        if Keys.Count = 0 then
        begin
            sgListValue.RowCount:=2;
            sgListValue.FixedRows:=1;
            sgListValue.Cells[0, 1]:='1';
            sgListValue.Cells[1, 1]:='';
            sgListValue.Cells[2, 1]:='';
        end
        else
        begin
            for iCNT:=1 to Keys.Count do
            begin
                sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);
                sgListValue.Cells[1, iCNT]:=Keys.Strings[iCNT - 1];
                junk:=Keys.Strings[iCNT - 1] + '=';
                clean:=StringReplace(Values.Strings[iCNT - 1], junk, '', [rfReplaceAll]);
                sgListValue.Cells[2, iCNT]:=clean;
            end;
        end;

    finally
        Keys.Free;
        Values.Free;
        sgListValue.SetColWidth(25, 30, 400);
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


/// <summary>
///     Assign <ALT> + <Y> to application close.
/// </summary>

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // Turn off standard <ALT> + <F4>
    if (Key=VK_F4) and (Shift=[ssALT]) then Key:=0;

    // Bind close application with <ALT> + <Y>
    if (Key=89) and (Shift=[ssALT]) then
    begin
        if MsgCall(mcQuestion1, 'Are you absolutely sure you want to exit the Unity?') = IDOK then
        begin
            pAllowClose:=True;
            Close;
        end;
    end;

end;

/// <summary>
///     Lock rows for editing. We autofill rows 1..3 so user is prevented from manipulation. Additionally, we allow only digits
///     for CoCode cell.
/// </summary>

procedure TMainForm.DetailsGridKeyPress(Sender: TObject; var Key: Char);
begin

    if (DetailsGrid.Row > 0) and (DetailsGrid.Row < 4) then
        Key:=#0;

    if (DetailsGrid.Row = 0) and (not (CharInSet(Key, ['0'..'9', #8]))) then
        Key:=#0;

end;

/// <summary>
///     Invoke autofill. Find given company code in general table and return assigned currency, agent and division info.
/// </summary>

procedure TMainForm.DetailsGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if DetailsGrid.Col = 0 then Find(DetailsGrid.Col);
    if DetailsGrid.Col = 1 then Find(DetailsGrid.Col);
    if DetailsGrid.Col = 2 then Find(DetailsGrid.Col);
    if DetailsGrid.Col = 3 then Find(DetailsGrid.Col);
end;

/// <summary>
///     Allow only specific characters when naming the group for aging snapshot.
/// </summary>

procedure TMainForm.EditGroupNameKeyPress(Sender: TObject; var Key: Char);
begin
    if (not (CharInSet(Key, ['A'..'Z', 'a'..'z', '0'..'9', '-', BACKSPACE]))) then
        Key:=#0;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ COPY PASTE CUT //


/// <remarks>
///     Allow <CTRL> + <C> on all string grids.
/// </remarks>

procedure TMainForm.sgOpenItemsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgOpenItems.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgInvoiceTrackerKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgInvoiceTracker.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgCoCodesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgCoCodes.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPaidInfoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPaidInfo.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPmtTermsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPmtTerms.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPersonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPerson.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgGroup3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgGroup3.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgControlStatusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgControlStatus.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgPersonRespKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPersonResp.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgSalesRespKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgSalesResp.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgAccountTypeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgAccountType.CopyCutPaste(adCopy);
end;

procedure TMainForm.sgCustomerGrKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgCustomerGr.CopyCutPaste(adCopy);
end;

/// <summary>
///     Edit selected age view columns.
/// </summary>

procedure TMainForm.sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    // Nested common constants

    const
        ctFree1    = 0;
        ctFree2    = 1;
        ctFree3    = 3;
        ctFollowUp = 2;

    // Nested methods

    // Modify data for given column
    procedure ModifyCell(CUIDRef: integer; ColumnType: integer; Text: string);
    begin
        if ColumnType = ctFree1    then TTGeneralComment.Create(sgAgeView.Cells[CUIDRef, sgAgeView.Row], strNULL, strNULL, Text, strNULL, strNULL, True);
        if ColumnType = ctFree2    then TTGeneralComment.Create(sgAgeView.Cells[CUIDRef, sgAgeView.Row], strNULL, strNULL, strNULL, Text, strNULL, True);
        if ColumnType = ctFree3    then TTGeneralComment.Create(sgAgeView.Cells[CUIDRef, sgAgeView.Row], strNULL, strNULL, strNULL, strNULL, Text, True);
        if ColumnType = ctFollowUp then TTGeneralComment.Create(sgAgeView.Cells[CUIDRef, sgAgeView.Row], strNULL, Text, strNULL, strNULL, strNULL, True);
    end;

    // Quit editing
    procedure QuitEditing;
    begin
        sgAgeView.Options:=sgAgeView.Options - [goEditing];
        sgAgeView.EditorMode:=False;
    end;

begin

    // Allow editing only free columns
    if
        (
            sgAgeView.Col <> sgAgeView.ReturnColumn(TGeneral.Free1, 1, 1)
        )
    and
        (
            sgAgeView.Col <> sgAgeView.ReturnColumn(TGeneral.Free2, 1, 1)
        )
    and
        (
            sgAgeView.Col <> sgAgeView.ReturnColumn(TGeneral.Free3, 1, 1)
        )
    then
        Exit;

    // Disallows "keyboard arrows" when inplace editor is enabled
    if (sgAgeView.EditorMode) and ( (Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN) ) then
    begin
        Key:=0;
        QuitEditing;
        Exit;
    end;

    // Allow editing
    if Key = VK_F2 then
    begin
        Key:=0;
        sgAgeView.Options:=sgAgeView.Options + [goEditing];
        sgAgeView.EditorMode:=True;
    end;

    // Quit editing
    if Key = VK_ESCAPE then
    begin
        Key:=0;
        QuitEditing;
    end;

    // Quit editing and write to database
    if Key = VK_RETURN then
    begin

        Key:=0;
        QuitEditing;

        // Free 1
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneral.Free1, 1, 1) then
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), ctFree1, sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]);

        // Free 2
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneral.Free2, 1, 1) then
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), ctFree2, sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]);

        // Free 3
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneral.Free3, 1, 1) then
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), ctFree3, sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]);

    end;

    // Delete entry from database
    if Key = VK_DELETE then
    begin

        Key:=0;

        // Free 1
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneral.Free1, 1, 1) then
        begin
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), ctFree1, '');
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:='';
        end;

        // Free 2
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneral.Free2, 1, 1) then
        begin
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), ctFree2, '');
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:='';
        end;

        // Free 3
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneral.Free3, 1, 1) then
        begin
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.CUID, 1, 1), ctFree3, '');
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:='';
        end;

    end;

end;

/// <summary>
///     Allow to copy selected area on age view string grid.
/// </summary>

procedure TMainForm.sgAgeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // <CTRL> + <C>
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgAgeView.CopyCutPaste(adCopy);

    // <CTRL> + <A>
    if (Key = 65) and (Shift = [ssCtrl]) then
    begin
        sgAgeView.SelectAll;
        sgAgeView.CopyCutPaste(adCopy);
        MsgCall(mcInfo, 'The selected spreadsheet has been copied to clipboard.');
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------- EDIT ADDRESS BOOK //


/// <summary>
///     Allow to edit specific Address Book cells. Once edited, it will be saved to database if user click "Update" button.
/// </summary>

procedure TMainForm.sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if
        (
            Key <> VK_LEFT
        )
    and
        (
            Key <> VK_RIGHT
        )
    and
        (
            Key <> VK_UP
        )
    and
        (
            Key <> VK_DOWN
        )
    and
        (
            Shift <> [ssShift]
        )
    and
        (
            Shift <> [ssCtrl]
        )
    and
        (
            Shift <> [ssAlt]
        )
    and
        (
            Key <> VK_BACK
        )
    and
        (
            Key <> VK_TAB
        )
    and
        (
            Key <> VK_ESCAPE
        )
    then
    begin

        if AddressBookExclusion then
        begin
            MsgCall(mcWarn, 'This column is locked for editing.');
            Exit;
        end;

        if Key = VK_RETURN then
        begin
            sgAddressBook.DelEsc(adESC, sgAddressBook.Col, sgAddressBook.Row);
            sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
            sgAddressBook.SetUpdatedRow(sgAddressBook.Row);
        end;

        if Key = VK_DELETE then
        begin
            sgAddressBook.DelEsc(adDEL, sgAddressBook.Col, sgAddressBook.Row);
            sgAddressBook.SetUpdatedRow(sgAddressBook.Row);
        end;

    end;

end;

procedure TMainForm.sgAddressBookKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if AccessLevel = acReadOnly then
    begin
        MsgCall(mcWarn, 'You don''t have permission to edit Address Book records.');
        Exit;
    end;

    if
        (
            Key = VK_F2
        )
    or
        (
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.EMAILS, 1, 1)
            )
        or
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.PHONE_NUMBERS, 1, 1)
            )
        or
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.CONTACT, 1, 1)
            )
        or
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(TAddressBook.ESTATEMENTS, 1, 1)
            )
        )
    and
        (
            Key <> VK_RETURN
        )
    then
        sgAddressBook.Options:=sgAddressBook.Options + [goEditing];

    if Key = VK_ESCAPE then
    begin
        sgAddressBook.DelEsc(adESC, sgAddressBook.Col, sgAddressBook.Row);
        sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then
        sgAddressBook.CopyCutPaste(adCopy);

    if (Key = 86) and (Shift = [ssCtrl]) then
    begin
        sgAddressBook.CopyCutPaste(adPaste);
        sgAddressBook.RecordRowsAffected;
    end;

    if (Key = 88) and (Shift = [ssCtrl]) then
    begin
        sgAddressBook.CopyCutPaste(adCut);
        sgAddressBook.RecordRowsAffected;
    end;

end;

/// <summary>
///     Update section value of settings file (decoded config.cfg).
/// </summary>

procedure TMainForm.sgListSectionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if Text50.Font.Style = [fsBold] then
    begin

        if (Key = 86) and (Shift = [ssCtrl]) then
            sgListSection.CopyCutPaste(adPaste);

        if (Key = 88) and (Shift = [ssCtrl]) then
            sgListSection.CopyCutPaste(adCut);
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then
        sgListSection.CopyCutPaste(adCopy);

    if (Key = 46) and (Text50.Font.Style = [fsBold]) then
        sgListSection.DelEsc(adDEL, sgListSection.Col, sgListSection.Row);
end;

procedure TMainForm.sgListSectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then
        sgListSection.DelEsc(adESC, sgListSection.Col, sgListSection.Row);
end;

procedure TMainForm.sgListSectionKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = CR { ENTER } then
        sgListSection.Cells[1, sgListSection.Row]:=UpperCase(sgListSection.Cells[1, sgListSection.Row]);
end;

/// <summary>
///     Update value key of settings file (decoded config.cfg).
/// </summary>

procedure TMainForm.sgListValueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if Text50.Font.Style = [fsBold] then
    begin
        if (Key = 86) and (Shift = [ssCtrl]) then
            sgListValue.CopyCutPaste(adPaste);

        if (Key = 88) and (Shift = [ssCtrl]) then
            sgListValue.CopyCutPaste(adCut);
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then
        sgListValue.CopyCutPaste(adCopy);

    if (Key = 46) and (Text50.Font.Style = [fsBold]) then
        sgListValue.DelEsc(adDEL, sgListValue.Col, sgListValue.Row);

end;

procedure TMainForm.sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then
        sgListValue.DelEsc(adESC, sgListValue.Col, sgListValue.Row);
end;

/// <summary>
///     Unlock with given password settings panel.
/// </summary>

procedure TMainForm.EditPasswordKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = CR { ENTER } then
        btnUnlockClick(self);
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


/// <summary>
///     Buttons (combined elements on common TPanel component) placed on application top panel "AppHeader".
///     Providing the effect of text style/color change on mouse hoover.
/// </summary>

procedure TMainForm.AppHeaderMouseEnter(Sender: TObject);
begin
    if AppHeader.Height = 13 then
    begin
        AppHeader.Color:=clSkyBlue;
        AppHeader.PanelBorders(clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    end;
end;

procedure TMainForm.AppHeaderMouseLeave(Sender: TObject);
begin
    if AppHeader.Height = 13 then
    begin
        AppHeader.Color:=clWhite;
        AppHeader.PanelBorders(clWhite, clWhite, clWhite, clWhite, clWhite);
    end;
end;

procedure TMainForm.btnStartMouseEnter(Sender: TObject);
begin
    txtStart.Font.Color:=$006433C9;
end;

procedure TMainForm.btnStartMouseLeave(Sender: TObject);
begin
    if txtStart.Font.Style <> [fsBold] then
        txtStart.Font.Color:=0;
end;

procedure TMainForm.btnReportsMouseEnter(Sender: TObject);
begin
    if btnReports.Height = 32 then
       txtReports.Font.Color:=$006433C9;
end;

procedure TMainForm.btnReportsMouseLeave(Sender: TObject);
begin
    if btnReports.Height = 32 then
        if txtReports.Font.Style <> [fsBold] then
            txtReports.Font.Color:=0;
end;

procedure TMainForm.btnDebtorsMouseEnter(Sender: TObject);
begin
    txtDebtors.Font.Color:=$006433C9;
end;

procedure TMainForm.btnDebtorsMouseLeave(Sender: TObject);
begin
    if txtDebtors.Font.Style <> [fsBold] then
        txtDebtors.Font.Color:=0;
end;

procedure TMainForm.btnTrackerMouseEnter(Sender: TObject);
begin
    txtTracker.Font.Color:=$006433C9;
end;

procedure TMainForm.btnTrackerMouseLeave(Sender: TObject);
begin
    if txtTracker.Font.Style <> [fsBold] then
        txtTracker.Font.Color:=0
end;

procedure TMainForm.btnAddressBookMouseEnter(Sender: TObject);
begin
    txtAddressBook.Font.Color:=$006433C9;
end;

procedure TMainForm.btnAddressBookMouseLeave(Sender: TObject);
begin
    if txtAddressBook.Font.Style <> [fsBold] then
        txtAddressBook.Font.Color:=0;
end;

procedure TMainForm.btnOpenItemsMouseEnter(Sender: TObject);
begin
    txtOpenItems.Font.Color:=$006433C9;
end;

procedure TMainForm.btnOpenItemsMouseLeave(Sender: TObject);
begin
    if txtOpenItems.Font.Style <> [fsBold] then
        txtOpenItems.Font.Color:=0;
end;

procedure TMainForm.btnUnidentifiedMouseEnter(Sender: TObject);
begin
    txtUnidentified.Font.Color:=$006433C9;
end;

procedure TMainForm.btnUnidentifiedMouseLeave(Sender: TObject);
begin
    if txtUnidentified.Font.Style <> [fsBold] then
        txtUnidentified.Font.Color:=0;
end;

procedure TMainForm.btnQueriesMouseEnter(Sender: TObject);
begin
    txtQueries.Font.Color:=$006433C9;
end;

procedure TMainForm.btnQueriesMouseLeave(Sender: TObject);
begin
    if txtQueries.Font.Style <> [fsBold] then
        txtQueries.Font.Color:=0;
end;

procedure TMainForm.btnTablesMouseEnter(Sender: TObject);
begin
    txtTables.Font.Color:=$006433C9;
end;

procedure TMainForm.btnTablesMouseLeave(Sender: TObject);
begin
    if txtTables.Font.Style <> [fsBold] then
        txtTables.Font.Color:=0;
end;

procedure TMainForm.btnSettingsMouseEnter(Sender: TObject);
begin
    txtSettings.Font.Color:=$006433C9;
end;

procedure TMainForm.btnSettingsMouseLeave(Sender: TObject);
begin
    if txtSettings.Font.Style <> [fsBold] then
        txtSettings.Font.Color:=0;
end;

/// <summary>
///     Extended button panel content. Introducing mouse hoover effect.
/// </summary>

procedure TMainForm.txtOverdueItemsMouseEnter(Sender: TObject);
begin
    txtOverdueItems.Font.Color:=$006433C9;
    txtOverdueItems.Font.Style:=[fsUnderline];
end;

procedure TMainForm.txtOverdueItemsMouseLeave(Sender: TObject);
begin
    txtOverdueItems.Font.Color:=0;
    txtOverdueItems.Font.Style:=[];
end;

procedure TMainForm.txtCreditLimitsReportMouseEnter(Sender: TObject);
begin
    txtCreditLimitsReport.Font.Color:=$006433C9;
    txtCreditLimitsReport.Font.Style:=[fsUnderline];
end;

procedure TMainForm.txtCreditLimitsReportMouseLeave(Sender: TObject);
begin
    txtCreditLimitsReport.Font.Color:=0;
    txtCreditLimitsReport.Font.Style:=[];
end;

procedure TMainForm.txtDebtorsReportMouseEnter(Sender: TObject);
begin
    txtDebtorsReport.Font.Color:=$006433C9;
    txtDebtorsReport.Font.Style:=[fsUnderline];
end;

procedure TMainForm.txtDebtorsReportMouseLeave(Sender: TObject);
begin
    txtDebtorsReport.Font.Color:=0;
    txtDebtorsReport.Font.Style:=[];
end;

procedure TMainForm.txtControlStatusReportMouseEnter(Sender: TObject);
begin
    txtControlStatusReport.Font.Color:=$006433C9;
    txtControlStatusReport.Font.Style:=[fsUnderline];
end;

procedure TMainForm.txtControlStatusReportMouseLeave(Sender: TObject);
begin
    txtControlStatusReport.Font.Color:=0;
    txtControlStatusReport.Font.Style:=[];
end;

/// <summary>
///     Set focus on the string grid.
/// </summary>

procedure TMainForm.sgAgeViewMouseEnter(Sender: TObject);
begin
//    if (sgAgeView.Enabled) and (sgAgeView.Visible) then
//        sgAgeView.SetFocus;
end;

procedure TMainForm.GroupListBoxMouseEnter(Sender: TObject);
begin
//    if (GroupListBox.Enabled) and (GroupListBox.Visible) then
//        GroupListBox.SetFocus;
end;

procedure TMainForm.GroupListDatesMouseEnter(Sender: TObject);
begin
//    if (GroupListDates.Enabled) and (GroupListDates.Visible) then
//        GroupListDates.SetFocus;
end;

procedure TMainForm.SortListBoxMouseEnter(Sender: TObject);
begin
//    if (SortListBox.Enabled) and (SortListBox.Visible) then
//        SortListBox.SetFocus;
end;

procedure TMainForm.sgOpenItemsMouseEnter(Sender: TObject);
begin
//    if (sgOpenItems.Enabled) and (sgOpenItems.Visible) then
//        sgOpenItems.SetFocus;
end;

procedure TMainForm.sgAddressBookMouseEnter(Sender: TObject);
begin
//    if (sgAddressBook.Enabled) and (sgAddressBook.Visible) then
//        sgAddressBook.SetFocus;
end;

procedure TMainForm.sgInvoiceTrackerMouseEnter(Sender: TObject);
begin
//    if (sgInvoiceTracker.Enabled) and (sgInvoiceTracker.Visible) then
//        sgInvoiceTracker.SetFocus;
end;

procedure TMainForm.sgCoCodesMouseEnter(Sender: TObject);
begin
//    if (sgCoCodes.Enabled) and (sgCoCodes.Visible) then
//        sgCoCodes.SetFocus;
end;

procedure TMainForm.sgPaidInfoMouseEnter(Sender: TObject);
begin
//    if (sgPaidInfo.Enabled) and (sgPaidInfo.Visible) then
//        sgPaidInfo.SetFocus;
end;

procedure TMainForm.sgPersonMouseEnter(Sender: TObject);
begin
//    if (sgPerson.Enabled) and (sgPerson.Visible) then
//        sgPerson.SetFocus;
end;

procedure TMainForm.sgPmtTermsMouseEnter(Sender: TObject);
begin
//    if (sgPmtTerms.Enabled) and (sgPmtTerms.Visible) then
//        sgPmtTerms.SetFocus;
end;

procedure TMainForm.sgGroup3MouseEnter(Sender: TObject);
begin
//    if (sgGroup3.Enabled) and (sgGroup3.Visible) then
//        sgGroup3.SetFocus;
end;

procedure TMainForm.sgControlStatusMouseEnter(Sender: TObject);
begin
//    if (sgControlStatus.Enabled) and (sgControlStatus.Visible) then
//        sgControlStatus.SetFocus;
end;

procedure TMainForm.sgPersonRespMouseEnter(Sender: TObject);
begin
//    if (sgPersonResp.Enabled) and (sgPersonResp.Visible) then
//        sgPersonResp.SetFocus;
end;

procedure TMainForm.sgSalesRespMouseEnter(Sender: TObject);
begin
//    if (sgSalesResp.Enabled) and (sgSalesResp.Visible) then
//        sgSalesResp.SetFocus;
end;

procedure TMainForm.sgAccountTypeMouseEnter(Sender: TObject);
begin
//    if (sgAccountType.Enabled) and (sgAccountType.Visible) then
//        sgAccountType.SetFocus;
end;

procedure TMainForm.sgCustomerGrMouseEnter(Sender: TObject);
begin
//    if (sgCustomerGr.Enabled) and (sgCustomerGr.Visible) then
//        sgCustomerGr.SetFocus;
end;

/// <summary>
///     Change standard behaviour of scroll bars on all string grids. The mouse weel on string grid component controls row selection instead of
///     moving up/down thumb on scroll bar. Below methods introduce mouse weel controlling scroll bar.
/// </summary>

// AGE VIEW | WHEEL DOWN
procedure TMainForm.sgAgeViewMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgAgeView.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// AGE VIEW | WHEEL UP
procedure TMainForm.sgAgeViewMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgAgeView.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// OPEN ITEMS | WHEEL DOWN
procedure TMainForm.sgOpenItemsMouseWheelDown(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgOpenItems.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// OPEN ITEMS | WHEEL UP
procedure TMainForm.sgOpenItemsMouseWheelUp(Sender: TObject; Shift:     TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgOpenItems.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// ADDRESS BOOK | WHEEL DOWN
procedure TMainForm.sgAddressBookMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgAddressBook.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// ADDRESS BOOK | WHEEL UP
procedure TMainForm.sgAddressBookMouseWheelUp(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgAddressBook.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// INVOICE TRACKER | WHEEL DOWN
procedure TMainForm.sgInvoiceTrackerMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgInvoiceTracker.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// INVOICE TRACKER | WHEEL UP
procedure TMainForm.sgInvoiceTrackerMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgInvoiceTracker.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// SECTION LIST | WHEEL DOWN
procedure TMainForm.sgListSectionMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgListSection.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// SECTION LIST | WHEEL UP
procedure TMainForm.sgListSectionMouseWheelUp(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgListSection.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// VALUE LIST | WHEEL DOWN
procedure TMainForm.sgListValueMouseWheelDown(Sender: TObject; Shift:   TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgListValue.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// VALUE LIST  | WHEEL UP
procedure TMainForm.sgListValueMouseWheelUp(Sender: TObject; Shift:     TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgListValue.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// COMPANY CODES | WHEEL DOWN
procedure TMainForm.sgCoCodesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgCoCodes.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// COMPANY CODES | WHEEL UP
procedure TMainForm.sgCoCodesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgCoCodes.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// PAYMENT TERMS | WHEEL DOWN
procedure TMainForm.sgPmtTermsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPmtTerms.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// PAYMENT TERMS | WHEEL UP
procedure TMainForm.sgPmtTermsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPmtTerms.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// PAID INFO | WHEEL DOWN
procedure TMainForm.sgPaidInfoMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPaidInfo.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// PAID INFO | WHEEL UP
procedure TMainForm.sgPaidInfoMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPaidInfo.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// GROUP3 | WHEEL DOWN
procedure TMainForm.sgGroup3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgGroup3.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// GROUP3 | WHEEL UP
procedure TMainForm.sgGroup3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgGroup3.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// PERSON | WHEEL DOWN
procedure TMainForm.sgPersonMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPerson.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// PERSON | WHEEL UP
procedure TMainForm.sgPersonMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPerson.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// UAC | WHEEL DOWN
procedure TMainForm.sgUACMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgUAC.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// UAC | WHEE UP
procedure TMainForm.sgUACMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgUAC.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// GROUPS | WHEEL DOWN
procedure TMainForm.sgGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgGroups.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// GROUPS | WHEEL UP
procedure TMainForm.sgGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgGroups.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// CONTROL STATUS | WHEEL DOWN
procedure TMainForm.sgControlStatusMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgControlStatus.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// CONTROL STATUS | WHEEL UP
procedure TMainForm.sgControlStatusMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgControlStatus.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// PERSON RESP. | WHEEL DOWN
procedure TMainForm.sgPersonRespMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPersonResp.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// PERSON RESP. | WHEEL UP
procedure TMainForm.sgPersonRespMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgPersonResp.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// SALES RESP. | WHEEL DOWN
procedure TMainForm.sgSalesRespMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgSalesResp.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// SALES RESP. | WHEEL UP
procedure TMainForm.sgSalesRespMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgSalesResp.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// ACCOUNT TYPE | WHEEL DOWN
procedure TMainForm.sgAccountTypeMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgAccountType.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// ACCOUNT TYPE | WHEEL UP
procedure TMainForm.sgAccountTypeMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgAccountType.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

// CUSTOMER GROUP | WHEEL UP
procedure TMainForm.sgCustomerGrMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgCustomerGr.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

// CUSTOMER GROUP | WHEEL UP
procedure TMainForm.sgCustomerGrMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgCustomerGr.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

/// <summary>
///     Open items buttons - mouse hoover effect.
/// </summary>

procedure TMainForm.btnReloadMouseEnter(Sender: TObject);
begin
    Text54L1.Font.Color:=FONCOLOR;
    Text54L2.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnReloadMouseLeave(Sender: TObject);
begin
    Text54L1.Font.Color:=clBlack;
    Text54L2.Font.Color:=clBlack;
end;

procedure TMainForm.btnMakeGroupMouseEnter(Sender: TObject);
begin
    Text83L1.Font.Color:=FONCOLOR;
    Text83L2.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnMakeGroupMouseLeave(Sender: TObject);
begin
    Text83L1.Font.Color:=clBlack;
    Text83L2.Font.Color:=clBlack;
end;

/// <summary>
///     Adress Book buttons - mouse hoover effect.
/// </summary>

procedure TMainForm.btnOpenABMouseEnter(Sender: TObject);
begin
    btnOpenAB.Cursor:=crHandPoint;
    Text64.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnOpenABMouseLeave(Sender: TObject);
begin
    btnOpenAB.Cursor:=crDefault;
    Text64.Font.Color:=clBlack;
end;

procedure TMainForm.btnUpdateABMouseEnter(Sender: TObject);
begin
    btnUpdateAB.Cursor:=crHandPoint;
    Text66.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnUpdateABMouseLeave(Sender: TObject);
begin
    btnUpdateAB.Cursor:=crDefault;
    Text66.Font.Color:=clBlack;
end;

procedure TMainForm.btnCloseABMouseEnter(Sender: TObject);
begin
    btnCloseAB.Cursor:=crHandPoint;
    Text67.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnCloseABMouseLeave(Sender: TObject);
begin
    btnCloseAB.Cursor:=crDefault;
    Text67.Font.Color:=clBlack;
end;

procedure TMainForm.btnExportABMouseEnter(Sender: TObject);
begin
    btnExportAB.Cursor:=crHandPoint;
    Text69.Font.Color:=FONCOLOR;
end;

procedure TMainForm.btnExportABMouseLeave(Sender: TObject);
begin
    btnExportAB.Cursor:=crDefault;
    Text69.Font.Color:=clBlack;
end;

/// <summary>
///     Settigs panel buttons - mouse hoover effect.
/// </summary>

procedure TMainForm.imgKeyAddMouseEnter(Sender: TObject);
begin
    imgKeyAdd.Cursor:=crHandPoint;
    Text41.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgKeyAddMouseLeave(Sender: TObject);
begin
    imgKeyAdd.Cursor:=crDefault;
    Text41.Font.Color:=clBlack;
end;

procedure TMainForm.imgKeyRemoveMouseEnter(Sender: TObject);
begin
    imgKeyRemove.Cursor:=crHandPoint;
    Text42.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgKeyRemoveMouseLeave(Sender: TObject);
begin
    imgKeyRemove.Cursor:=crDefault;
    Text42.Font.Color:=clBlack;
end;

procedure TMainForm.imgUpdateValuesMouseEnter(Sender: TObject);
begin
    imgUpdateValues.Cursor:=crHandPoint;
    Text43.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgUpdateValuesMouseLeave(Sender: TObject);
begin
    imgUpdateValues.Cursor:=crDefault;
    Text43.Font.Color:=clBlack;
end;

procedure TMainForm.imgSectionAddMouseEnter(Sender: TObject);
begin
    imgSectionAdd.Cursor:=crHandPoint;
    Text48.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgSectionAddMouseLeave(Sender: TObject);
begin
    imgSectionAdd.Cursor:=crDefault;
    Text48.Font.Color:=clBlack;
end;

procedure TMainForm.imgSectionRemoveMouseEnter(Sender: TObject);
begin
    imgSectionRemove.Cursor:=crHandPoint;
    Text49.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgSectionRemoveMouseLeave(Sender: TObject);
begin
    imgSectionRemove.Cursor:=crDefault;
    Text49.Font.Color:=clBlack;
end;

procedure TMainForm.imgAllowEditMouseEnter(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crHandPoint;
    Text50.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgAllowEditMouseLeave(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crDefault;
    Text50.Font.Color:=clBlack;
end;

procedure TMainForm.imgEventLogMouseEnter(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crHandPoint;
    Text51.Font.Color:=FONCOLOR;
end;

procedure TMainForm.imgEventLogMouseLeave(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crDefault;
    Text51.Font.Color:=clBlack;
end;

procedure TMainForm.sgListSectionClick(Sender: TObject);
begin
    // Images
    imgSectionAdd.Enabled   :=True;
    imgSectionRemove.Enabled:=True;
    imgKeyAdd.Enabled       :=False;
    imgKeyRemove.Enabled    :=False;
    imgUpdateValues.Enabled :=False;

    // Captions
    Text41.Enabled:=False;
    Text42.Enabled:=False;
    Text43.Enabled:=False;
    Text48.Enabled:=True;
    Text49.Enabled:=True;
end;

procedure TMainForm.sgListValueClick(Sender: TObject);
begin
    // Images
    imgSectionAdd.Enabled   :=False;
    imgSectionRemove.Enabled:=False;
    imgKeyAdd.Enabled       :=True;
    imgKeyRemove.Enabled    :=True;
    imgUpdateValues.Enabled :=True;

    // Captions
    Text41.Enabled:=True;
    Text42.Enabled:=True;
    Text43.Enabled:=True;
    Text48.Enabled:=False;
    Text49.Enabled:=False;
end;

/// <summary>
///     Settings panel - password edit boxes.
/// </summary>

procedure TMainForm.btnPasswordPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EditPassword.PasswordChar:=#0;
end;

procedure TMainForm.btnPasswordPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EditPassword.PasswordChar:='*';
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //


/// <summary>
///     Application top bar/menu buttons.
/// </summary>

procedure TMainForm.AppHeaderClick(Sender: TObject);
begin
    if AppHeader.Height = 13 then
    begin
        UnfoldReportsTab(AppHeader, btnReports, True);
        AppHeader.Height:=57;
        AppHeader.Cursor:=crDefault;
        AppHeader.Color:=clWhite;
        AppHeader.PanelBorders(clWhite, clSkyBlue, clWhite, clWhite, clWhite);
    end;
end;

procedure TMainForm.imgHideBarClick(Sender: TObject);
begin
    if AppHeader.Height > 13 then
    begin
        UnfoldReportsTab(AppHeader, btnReports, True);
        AppHeader.Height:=13;
        AppHeader.Cursor:=crHandPoint;
        AppHeader.Color:=clWhite;
        AppHeader.PanelBorders(clWhite, clWhite, clWhite, clWhite, clWhite);
    end;
end;

procedure TMainForm.txtStartClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet9;
    ResetTabsheetButtons;
    txtStart.Font.Style:=[fsBold];
    txtStart.Font.Color:=$006433C9;

    /// <remarks>
    ///     ChromiumWindow placed on TPageControl crashes application when the application starts maximised and
    ///     ChromiumWindow completly fill the container. To overcome this, it is necessary to initialize and load
    ///     given web page when ChromiumWindow is "small", and then re-size to fill the TabSheet at once.
    /// </remarks>

    if ChromiumWindow.Align <> alClient then ChromiumWindow.Align:=alClient;

end;

procedure TMainForm.txtReportsClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports);
    ResetTabsheetButtons;
    txtReports.Font.Style:=[fsBold];
    txtReports.Font.Color:=$006433C9;
end;

procedure TMainForm.txtDebtorsClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet1;
    ResetTabsheetButtons;
    txtDebtors.Font.Style:=[fsBold];
    txtDebtors.Font.Color:=$006433C9;
end;

procedure TMainForm.txtTrackerClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet4;
    ResetTabsheetButtons;
    txtTracker.Font.Style:=[fsBold];
    txtTracker.Font.Color:=$006433C9;
end;

procedure TMainForm.txtAddressBookClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet3;
    ResetTabsheetButtons;
    txtAddressBook.Font.Style:=[fsBold];
    txtAddressBook.Font.Color:=$006433C9;
end;

procedure TMainForm.txtOpenItemsClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet2;
    ResetTabsheetButtons;
    txtOpenItems.Font.Style:=[fsBold];
    txtOpenItems.Font.Color:=$006433C9;
end;

procedure TMainForm.txtUnidentifiedClick(Sender: TObject);
begin
    MsgCall(mcInfo, 'This feature is not yet accessible. Please try later.');
end;

procedure TMainForm.txtQueriesClick(Sender: TObject);
begin
    MsgCall(mcInfo, 'This feature is not yet accessible. Please try later.');
end;

procedure TMainForm.txtTablesClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet7;
    ResetTabsheetButtons;
    txtTables.Font.Style:=[fsBold];
    txtTables.Font.Color:=$006433C9;
end;

procedure TMainForm.txtSettingsClick(Sender: TObject);
begin
    UnfoldReportsTab(AppHeader, btnReports, True);
    MyPages.ActivePage:=TabSheet8;
    ResetTabsheetButtons;
    txtSettings.Font.Style:=[fsBold];
    txtSettings.Font.Color:=$006433C9;
end;

/// <summary>
///     Extended list for Reports button.
/// </summary>

procedure TMainForm.txtOverdueItemsClick(Sender: TObject);
var
    Return: cardinal;
begin
    Return:=ShowReport(1);
    if not(Return > 32) then
    begin
        MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
        LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;
    UnfoldReportsTab(AppHeader, btnReports);
end;

procedure TMainForm.txtCreditLimitsReportClick(Sender: TObject);
var
    Return: cardinal;
begin
    Return:=ShowReport(2);
    if not(Return > 32) then
    begin
        MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
        LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;
    UnfoldReportsTab(AppHeader, btnReports);
end;

procedure TMainForm.txtDebtorsReportClick(Sender: TObject);
var
    Return: cardinal;
begin
    Return:=ShowReport(3);
    if not(Return > 32) then
    begin
        MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
        LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;
    UnfoldReportsTab(AppHeader, btnReports);
end;

procedure TMainForm.txtControlStatusReportClick(Sender: TObject);
var
    Return: cardinal;
begin
    Return:=ShowReport(4);
    if not(Return > 32) then
    begin
        MsgCall(mcWarn, 'Cannot execute report. Please contact with IT support.');
        LogText.Log(EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;
    UnfoldReportsTab(AppHeader, btnReports);
end;

/// <summary>
///     Age View, loading selected group ID.
/// </summary>

procedure TMainForm.btnLoadAgeViewClick(Sender: TObject);
var
    iCNT: integer;
begin

    // Wait until "Ready" status
    if not (StatBar_TXT1.Caption = stReady) then
    begin
        MsgCall(mcWarn, 'Please wait until "Ready" status and try again.');
        Exit;
    end;

    if (not(string.IsNullOrEmpty(GroupListBox.Text))) and (not(string.IsNullOrEmpty(GroupListDates.Text))) then
    begin

        // Remember user's choice, automation will follow
        GroupIdSel:=GroupList[GroupListBox.ItemIndex, 0];
        GroupNmSel:=GroupList[GroupListBox.ItemIndex, 1];
        AgeDateSel:=GroupListDates.Text;

        // Remove filters
        for iCNT:=1 to sgAgeView.RowCount - 1 do
            sgAgeView.RowHeights[iCNT]:=sgRowHeight;

        FilterForm.FilterClearAll;

        // Turn off all timers
        MainForm.SwitchTimers(tmDisabled);

        // Load age view for selected group ID
        TTReadAgeView.Create(thCallOpenItems, smRanges);
    end
        else
            MsgCall(mcWarn, 'Cannot load selected group.');
end;

/// <summary>
///     Apply sorting on age view string grid.
/// </summary>

procedure TMainForm.btnSortApplyClick(Sender: TObject);
var
    iCNT: integer;
begin

    if
        (
            SortListBox.ItemIndex <> smRanges
        )
    and
        (
            SortListBox.ItemIndex <> smFollowUp
        )
    and
        (
            SortListBox.ItemIndex <> smTotal
        )
    and
        (
            SortListBox.ItemIndex <> smOverdue
        )
    then
        Exit;

    if not (StatBar_TXT1.Caption = stReady) then
    begin
        MsgCall(mcWarn, 'Please wait until "Ready" status and try again.');
        Exit;
    end;

    if (not(string.IsNullOrEmpty(GroupListBox.Text))) and (not(string.IsNullOrEmpty(GroupListDates.Text))) then
    begin
        // Remember user's choice, automation will follow
        GroupIdSel:=GroupList[GroupListBox.ItemIndex, 0];
        GroupNmSel:=GroupList[GroupListBox.ItemIndex, 1];
        AgeDateSel:=GroupListDates.Text;

        // Remove filters
        for iCNT:=1 to sgAgeView.RowCount - 1 do
            sgAgeView.RowHeights[iCNT]:=sgRowHeight;

        FilterForm.FilterClearAll;

        // Turn off all timers
        MainForm.SwitchTimers(tmDisabled);

        // Load age view for selected group ID
        TTReadAgeView.Create(thNullParameter, SortListBox.ItemIndex);

    end
        else
            MsgCall(mcWarn, 'Cannot load selected group.');
end;

/// <summary>
///     Reload on open items list on demand.
/// </summary>

procedure TMainForm.btnReloadClick(Sender: TObject);
begin

    if not(IsConnected) then
    begin
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit;
    end;

    // Only administrator is allowed
    StatBar_TXT1.Caption :=stProcessing;
    if MainForm.AccessLevel = acADMIN then
    begin
        TTReadOpenItems.Create(thNullParameter);
    end
    else
    begin
        StatBar_TXT1.Caption:=stReady;
        LogText.Log(EventLogPath, '[Open Items]: User have no R/W access, process halted.');
    end;

end;

/// <summary>
///     Make snapshots for given selected group ID/name.
/// </summary>

procedure TMainForm.btnMakeGroupClick(Sender: TObject);
begin

    cbDump.Checked:=False;

    if not(IsConnected) then
    begin
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit;
    end;

    if sgOpenItems.RowCount < 2 then
        Exit;

    // Only administrator is allowed
    if MainForm.AccessLevel = acADMIN then
    begin
        if PanelGroupName.Visible then
        begin
            PanelGroupName.Visible:=False;
        end
        else
        begin
            PanelGroupName.Visible:=True;
            // Suggest the same group name and group ID
            EditGroupName.Text:=GroupNmSel;
            EditGroupID.Text  :=GroupIdSel;
        end
    end
    else
    begin
        StatBar_TXT1.Caption:='Insufficient UAC level.';
        LogText.Log(EventLogPath, '[Make Group]: User have no R/W access, process halted.');
    end;

end;

/// <summary>
///     Make aging report (snapshot).
/// </summary>

procedure TMainForm.btnMakeGroupAgeClick(Sender: TObject);
begin

    if (not(string.IsNullOrEmpty(EditGroupName.Text))) and (not(string.IsNullOrEmpty(EditGroupID.Text))) then
    begin
        // Start thread with no parameters passed to an object
        PanelGroupName.Visible:=False;
        ReloadCover.Visible   :=False;
        TTMakeAgeView.Create(MainForm.OSAmount);
    end
        else
            MsgCall(mcWarn, 'Please enter group name and try again.' + CRLF + 'If you will use existing one, then it will be overwritten.');
end;

/// <summary>
///     Address Book - open action.
/// </summary>

procedure TMainForm.btnOpenABClick(Sender: TObject);
begin
    if IsConnected then
    begin
        sgAddressBook.SetUpdatedRow(0);
        TTAddressBook.Create(
            adOpenAll,
            sgAddressBook,
            '',
            '',
            '',
            '',
            '',
            ''
        )
    end
        else
            MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

/// <summary>
///     Address Book - update records.
/// </summary>

procedure TMainForm.btnUpdateABClick(Sender: TObject);
begin

    if not(sgAddressBook.Visible) then
    begin
        MsgCall(mcWarn, 'Please open Address Book first.');
        Exit;
    end;

    if IsConnected then
        TTAddressBook.Create(
            adUpdate,
            sgAddressBook,
            '',
            '',
            '',
            '',
            '',
            ''
        )
    else
        MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;

/// <summary>
///     Address Book - clear all content of populated string grid.
/// </summary>

procedure TMainForm.btnCloseABClick(Sender: TObject);
begin
    if MsgCall(mcQuestion2, 'Are you sure you want to close Address Book?') = IDYES then
    begin
        sgAddressBook.SetUpdatedRow(0);
        sgAddressBook.ClearAll(2, 1, 1, True);
        sgAddressBook.Visible:=False;
    end;
end;

/// <summary>
///     Address Book - export all content to CSV file.
/// </summary>

procedure TMainForm.btnExportABClick(Sender: TObject);
begin

    if not(sgAddressBook.Visible) then
    begin
        MsgCall(mcWarn, 'Please open Address Book first.');
        Exit;
    end;
    TTAddressBook.Create(
        adExport,
        sgAddressBook,
        '',
        '',
        '',
        '',
        '',
        ''
    );
end;

/// <summary>
///     Settings panel - section list. Add new section.
/// </summary>

procedure TMainForm.imgSectionAddClick(Sender: TObject);
var
    iCNT:  integer;
begin

    // Add row at the end of the list
    sgListSection.RowCount:=sgListSection.RowCount + 1;
    sgListSection.Cells[1, sgListSection.RowCount]:= '';

    for iCNT:= 1 to sgListSection.RowCount do
        sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);

end;

/// <summary>
///     Settings panel - section list. Delete selected section.
/// </summary>

procedure TMainForm.imgSectionRemoveClick(Sender: TObject);
var
    Settings:   ISettings;
    iCNT:       integer;
begin

    if MsgCall(mcQuestion2, 'Are you sure you want to delete this section? It cannot be undone.') = IDNO then exit;
    if sgListSection.RowCount = 1 then exit;

    // Remove given section
    Settings:=TSettings.Create;
    Settings.DeleteSection(sgListSection.Cells[1, sgListSection.Row]);
    Settings.Encode(AppConfig);

    // Remove from string grid
    sgListSection.DeleteRowFrom(1, 1);

    // Re-number
    for iCNT := 1 to sgListSection.RowCount do
        sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);

end;

/// <summary>
///     Settings panel - section list. Allow edit. We require separate action for administrator to prevent from mistake changes etc.
/// </summary>

procedure TMainForm.imgAllowEditClick(Sender: TObject);
begin

    if Text50.Font.Style = [fsBold] then
    begin
        sgListSection.Options:=sgListSection.Options - [goEditing];
        sgListValue.Options  :=sgListValue.Options   - [goEditing];
        Text50.Font.Style    :=[];
    end
    else
    begin
        sgListSection.Options:=sgListSection.Options + [goEditing];
        sgListValue.Options  :=sgListValue.Options   + [goEditing];
        Text50.Font.Style    :=[fsBold];
    end;

end;


/// <summary>
///     Settings panel - open event log in separate window.
/// </summary>

procedure TMainForm.imgEventLogClick(Sender: TObject);
begin
    WndCall(EventForm, stModeless);
end;

/// <summary>
///     Settings panel - section list. Add key to selected section.
/// </summary>

procedure TMainForm.imgKeyAddClick(Sender: TObject);
var
    iCNT:  integer;
begin

    // Add row at the end of the list
    iCNT:=sgListValue.RowCount + 1;
    sgListValue.RowCount:=iCNT;

    // Make sure we add empty row
    sgListValue.Cells[1, iCNT - 1]:='';
    sgListValue.Cells[2, iCNT - 1]:='';

    for iCNT:= 1 to sgListValue.RowCount do
        sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;

/// <summary>
///     Settings panel - remove key with value from selected section.
/// </summary>

procedure TMainForm.imgKeyRemoveClick(Sender: TObject);
var
    Settings:  ISettings;
    iCNT:      integer;
begin

    if MsgCall(mcQuestion2, 'Are you sure you want to delete this key? It cannot be undone.') = IDNO then Exit;

    // Check for last row
    if sgListValue.RowCount = 1 then
        exit;

    // Remove key
    Settings:=TSettings.Create;
    Settings.DeleteKey(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, sgListValue.Row]);
    Settings.Encode(AppConfig);

    // Remove from string grid list
    sgListValue.DeleteRowFrom(1, 1);

    // Re-number
    for iCNT:= 1 to sgListValue.RowCount do
        sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;

/// <summary>
///     Settings panel - save all values and keys.
/// </summary>

procedure TMainForm.imgUpdateValuesClick(Sender: TObject);
var
    Settings:  ISettings;
    iCNT:      integer;
begin

    if MsgCall(mcQuestion2, 'Are you sure you want to save all the changes? It cannot be undone.') = IDNO then exit;

    // Check if there is no empty keys
    for iCNT:= 1 to (sgListValue.RowCount - 1) do
    begin
        if string.IsNullOrEmpty(sgListValue.Cells[1, iCNT]) then
        begin
            MsgCall(mcWarn, 'Cannot save. At least one key has no label.');
            Exit;
        end;
    end;

    Settings:=TSettings.Create;

    // Save to settings file all the keys and values
    for iCNT:= 1 to (sgListValue.RowCount - 1) do
        Settings.SetStringValue(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, iCNT], sgListValue.Cells[2, iCNT]);

    Settings.Encode(AppConfig);

    MsgCall(mcInfo, 'All Keys and its values has been saved successfully.');

end;

/// <summary>
///     Settings panel - save new password.
/// </summary>

procedure TMainForm.btnPassUpdateClick(Sender: TObject);
begin

    // Check fields

    if
        (
            not(string.IsNullOrEmpty(EditCurrentPassword.Text))
        )
    and
        (
            not(string.IsNullOrEmpty(EditNewPassword.Text))
        )
    and
        (
            not(string.IsNullOrEmpty(EditNewPasswordConfirmation.Text))
        )
    then
    begin

        // Check given password

        if not(CheckGivenPassword(EditPassword.Text)) then
        begin
            MsgCall(mcWarn, 'Incorrect password, please re-type it and try again.');
            Exit;
        end;

        // Incorrent match

        if EditNewPassword.Text <> EditNewPasswordConfirmation.Text then
        begin
            MsgCall(mcWarn, 'New password and its confirmation does not match, please re-type it and try again.');
            Exit;
        end
        else

        // Hash and save

        begin
            if SetNewPassword(EditNewPassword.Text) then
            begin
                MsgCall(mcInfo, 'New password has been saved.');
                btnPassUpdate.Enabled:=False;
                EditCurrentPassword.Enabled:=False;
                EditNewPassword.Enabled:=False;
                EditNewPasswordConfirmation.Enabled:=False;
                EditCurrentPassword.Text:='';
                EditNewPassword.Text:='';
                EditNewPasswordConfirmation.Text:='';
            end
            else

            // Cannot hash and save

            begin
                MsgCall(mcError, 'Cannot save new password. Please contact IT support.');
            end;

        end;

    end
    else

    // No fields can be empty

    begin
        MsgCall(mcWarn, 'Please provide with current password, new password and its confirmation.');
    end;

end;

/// <summary>
///     Settings panel - unlock panel with provided password.
/// </summary>

procedure TMainForm.btnUnlockClick(Sender: TObject);
var
    List:       TStringList;
    Settings:   ISettings;
    UserAcc:    TDataTables;
    iCNT:       integer;
    jCNT:       integer;
begin

    // No password given

    if btnUnlock.Caption = 'Lock' then
    begin
        SetSettingsPanel(spLock);
        Exit;
    end;

    if EditPassword.Text = '' then
    begin
        MsgCall(mcWarn, 'Please provide with password.');
        Exit;
    end
    else

    // Password is valid

    if CheckGivenPassword(EditPassword.Text) then
    begin

        // Enable controls
        SetSettingsPanel(spUnLock);

        // Populate string grids
        List:=TStringList.Create();
        Settings:=TSettings.Create;

        try
            Settings.GetSections(List);
            sgListSection.RowCount:=List.Count;
            jCNT:=1;

            for iCNT:=0 to List.Count - 1 do
            begin
                if List.Strings[iCNT] <> PasswordSection then
                begin
                    sgListSection.Cells[0, jCNT]:=IntToStr(jCNT);
                    sgListSection.Cells[1, jCNT]:=List.Strings[iCNT];
                    inc(jCNT);
                end;
            end;

        finally
            List.Free;
        end;

        // Get UAC and groups
        UserAcc:=TDataTables.Create(DbConnect);
        try
            UserAcc.OpenTable(TblUAC);    UserAcc.SqlToGrid(sgUAC,    UserAcc.ExecSQL, False, True);
            UserAcc.OpenTable(TblGroups); UserAcc.SqlToGrid(sgGroups, UserAcc.ExecSQL, False, True);
        finally
            UserAcc.Free;
            sgUAC.SetColWidth   (10, 20, 400);
            sgGroups.SetColWidth(10, 20, 400);
        end;

        // Grids dimensions
        sgListValue.SetColWidth(25, 30, 400);
        sgListSection.SetColWidth(25, 30, 400);
        sgUAC.SetColWidth(10, 20, 400);
        sgGroups.SetColWidth(10, 20, 400);

        // CLear edit box from provided password
        EditPassword.Text:='';

    end
    else

    // Invalid password

    begin
        MsgCall(mcWarn, 'Incorrect password, please re-type it and try again.');
        EditPassword.Text:='';
    end;

end;


end.
