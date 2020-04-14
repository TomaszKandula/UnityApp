unit View.Main;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellAPI,
    Winapi.ActiveX,
    Winapi.Wininet,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.INIFiles,
    System.StrUtils,
    System.DateUtils,
    System.Diagnostics,
    System.Threading,
    System.Math,
    System.Win.ComObj,
    System.UITypes,
    System.ImageList,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Menus,
    Vcl.ComCtrls,
    Vcl.Grids,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Vcl.CheckLst,
    Vcl.Buttons,
    Vcl.Imaging.pngimage,
    Vcl.DBGrids,
    Vcl.AppEvnts,
    Vcl.ValEdit,
    Vcl.Clipbrd,
    Vcl.OleCtrls,
    Vcl.Imaging.GIFImg,
    Vcl.ImgList,
    uCEFChromium,
    uCEFWindowParent,
    uCEFChromiumWindow,
    uCEFTypes,
    uCEFInterfaces,
    uCEFWinControl,
    Unity.Grid,
    Unity.Shape,
    Unity.Panel,
    Unity.ComboBox,
    Unity.Enums,
    Unity.Records,
    Unity.References;


type


    TMainForm = class(TForm) // Representing main user window
        Action_GoogleIt: TMenuItem;
        imgSSL: TImage;
        imgNavCover: TImage;
        Action_BrowserMode: TMenuItem;
        Action_AccountDetails: TMenuItem;
        PanelLock: TPanel;
        imgLock: TImage;
        shapeUrlSection: TShape;
        PanelUrl: TPanel;
        PanelButtons: TPanel;
        EditUrlSection: TEdit;
        btnNavHome: TImage;
        txtNavHome: TLabel;
        PanelNavigation: TPanel;
        btnNavBack: TImage;
        btnNavForward: TImage;
        btnNavReload: TImage;
        txtNavBack: TLabel;
        txtNavForward: TLabel;
        txtNavReload: TLabel;
        Action_ShowNavigation: TMenuItem;
        N10: TMenuItem;
        btnRating: TPanel;
        imgRating: TImage;
        txtRating: TLabel;
        ShapeFollowsFrm: TShape;
        ShapeFollowsCap: TShape;
        txtFollowsToday: TLabel;
        txtFollowsPast: TLabel;
        txtFollowsNext: TLabel;
        valFollowsToday: TLabel;
        valFollowsPast: TLabel;
        valFollowsNext: TLabel;
        TabSheets: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
        TabSheet7: TTabSheet;
        DebtorsHeader: TPanel;
        DebtorsFooter: TPanel;
        txtTotalCustomers: TLabel;
        valTotalCustomers: TLabel;
        ShapeRiskClassFrm: TShape;
        ShapeRiskClassCap: TShape;
        ShapeDetailsFrm: TShape;
        ShapeDetailsCap: TShape;
        txtNotDue: TLabel;
        txtRange1: TLabel;
        txtRange2: TLabel;
        txtRange3: TLabel;
        txtRange4: TLabel;
        txtRange5: TLabel;
        Text08: TLabel;
        procNotDue: TLabel;
        procRange1: TLabel;
        procRange2: TLabel;
        procRange3: TLabel;
        procRange4: TLabel;
        procRange5: TLabel;
        amtNotDue: TLabel;
        amtRange1: TLabel;
        amtRange2: TLabel;
        amtRange3: TLabel;
        amtRange4: TLabel;
        amtRange5: TLabel;
        txtTotal: TLabel;
        amtTotal: TLabel;
        procTotal: TLabel;
        txtRiskClassA: TLabel;
        txtRiskClassB: TLabel;
        txtRiskClassC: TLabel;
        ShapeSelectionFrm: TShape;
        ShapeSelectionCap: TShape;
        ShapeExceedersFrm: TShape;
        ShapeExceedersCap: TShape;
        txtExceeders: TLabel;
        txtCreditExcess: TLabel;
        txtGrantedLimits: TLabel;
        ShapeSummaryFrm: TShape;
        ShapeSummaryCap: TShape;
        txtNotOverdue: TLabel;
        txtPastDue: TLabel;
        txtDefaulted: TLabel;
        amtExceeders: TLabel;
        amtCreditExcess: TLabel;
        amtGrantedLimits: TLabel;
        amtNotOverdue: TLabel;
        amtPastDue: TLabel;
        amtDefaulted: TLabel;
        amtRiskClassA: TLabel;
        amtRiskClassB: TLabel;
        amtRiskClassC: TLabel;
        txtStatus: TLabel;
        valStatus: TLabel;
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
        AddressBookHeader: TPanel;
        sgAddressBook: TStringGrid;
        SettingsHeader: TPanel;
        EditPassword: TEdit;
        ShapeAdminEntryCap: TShape;
        ShapeAdminEntryFrm: TShape;
        txtAdminPassword: TLabel;
        txtInfoAdmin: TLabel;
        ShapeAdminPassFrm: TShape;
        ShapeAdminPassCap: TShape;
        txtCurrentPassword: TLabel;
        txtNewPassword: TLabel;
        txtConfirmPassword: TLabel;
        EditCurrentPassword: TEdit;
        EditNewPassword: TEdit;
        EditNewPasswordConfirmation: TEdit;
        ShapeAppSettingsFrm: TShape;
        imgKeyAdd: TImage;
        imgKeyRemove: TImage;
        imgUpdateValues: TImage;
        txtKeyAdd: TLabel;
        txtKeyRemove: TLabel;
        txtUpdateValues: TLabel;
        sgListValue: TStringGrid;
        sgListSection: TStringGrid;
        imgSectionAdd: TImage;
        imgSectionRemove: TImage;
        txtSectionAdd: TLabel;
        txtSectionRemove: TLabel;
        ShapeAppSettingsCap: TShape;
        txtSettingsWarn: TLabel;
        ShapeAddressBookFrm: TShape;
        ShapeAddressBookCap: TShape;
        imgOFF1: TImage;
        OpenItemsHeader: TPanel;
        ShapeReloadFrm: TShape;
        ShapeReloadCap: TShape;
        ShapeSumsFrm: TShape;
        ShapeSumsCap: TShape;
        btnReload: TImage;
        txtReloadBtnA: TLabel;
        btnOpenAb: TImage;
        btnCloseAb: TImage;
        txtOpenAb: TLabel;
        txtCloseAb: TLabel;
        txtAllOpenItems: TLabel;
        txtInvoices: TLabel;
        txtOverdueItems: TLabel;
        txtOutstanding: TLabel;
        valOpenItems: TLabel;
        valInvoices: TLabel;
        amtOutstanding: TLabel;
        valOverdue: TLabel;
        btnExportAb: TImage;
        txtExportAb: TLabel;
        InvoiceTrackerHeader: TPanel;
        ShapeTrackerInfoFrm: TShape;
        ShapeTrackerInfoCap: TShape;
        Header6: TPanel;
        ShapeContent6: TShape;
        Cap61: TShape;
        GeneralTablesHeader: TPanel;
        ShapeTablesInfoFrm: TShape;
        ShapeTablesInfoCap: TShape;
        txtReloadBtnB: TLabel;
        MainShape6: TPanel;
        AppFooter: TPanel;
        txtCurrentDate: TLabel;
        valCurrentDate: TLabel;
        txtCurrentTime: TLabel;
        valCurrentTime: TLabel;
        txtUpTime: TLabel;
        valUpTime: TLabel;
        TimerCurrentTime: TTimer;
        TimerUpTime: TTimer;
        txtInfo1: TLabel;
        txtInfo3: TLabel;
        txtInfo2: TLabel;
        FileCSVExport: TSaveDialog;
        FileCSVImport: TOpenDialog;
        GeneralTablesMainPanel: TPanel;
        ContentPanel6: TPanel;
        InvoiceTrackerMainPanel: TPanel;
        AddressBookMainPanel: TPanel;
        OpenItemsMainPanel: TPanel;
        DebtorsMainPanel: TPanel;
        DebtorsBottomPanel: TPanel;
        SettingsMainPanel: TPanel;
        SettingsInnerPanel: TPanel;
        amtUnallocated: TLabel;
        txtUnallocated: TLabel;
        TimerCustSnapshots: TTimer;
        txtOverdue: TLabel;
        amtOverdue: TLabel;
        txtCutOffDate: TLabel;
        valRiskClassA: TLabel;
        valRiskClassB: TLabel;
        valRiskClassC: TLabel;
        txtUpdateStamp: TLabel;
        valUpdateStamp: TLabel;
        sgAgeView: TStringGrid;
        sgInvoiceTracker: TStringGrid;
        PopupAgeView: TPopupMenu;
        Action_Tracker: TMenuItem;
        InfoLine1: TLabel;
        InfoLine2: TLabel;
        InfoLine3: TLabel;
        PopupTracker: TPopupMenu;
        Action_Remove: TMenuItem;
        Action_ShowMy: TMenuItem;
        Action_ShowAll: TMenuItem;
        Action_LyncCall: TMenuItem;
        TrayIcon: TTrayIcon;
        Action_ShowRegistered: TMenuItem;
        N8: TMenuItem;
        N9: TMenuItem;
        Action_FilterAgeView: TMenuItem;
        N7: TMenuItem;
        N6: TMenuItem;
        Action_AddToBook: TMenuItem;
        txtAllowEdit: TLabel;
        imgAllowEdit: TImage;
        SplitLine2: TBevel;
        sgCoCodes: TStringGrid;
        sgPaidInfo: TStringGrid;
        sgPmtTerms: TStringGrid;
        Action_AutoColumnSize: TMenuItem;
        SplitLine1: TBevel;
        Action_Search: TMenuItem;
        PopupBook: TPopupMenu;
        Action_Copy: TMenuItem;
        Action_DelRow: TMenuItem;
        Action_ToExce: TMenuItem;
        N13: TMenuItem;
        FileXLExport: TSaveDialog;
        txtRange6: TLabel;
        amtRange6: TLabel;
        procRange6: TLabel;
        N14: TMenuItem;
        Action_SearchBook: TMenuItem;
        Action_Overdue: TMenuItem;
        N15: TMenuItem;
        TabSheet9: TTabSheet;
        Action_RowHighlight: TMenuItem;
        Action_Update: TMenuItem;
        Action_Report: TMenuItem;
        N17: TMenuItem;
        PopupCommonMenu: TPopupMenu;
        Action_AutoColumn: TMenuItem;
        Action_ExportTransactions: TMenuItem;
        Action_SelectAll: TMenuItem;
        N18: TMenuItem;
        Action_CopyToCB: TMenuItem;
        N19: TMenuItem;
        Action_ColumnWidth: TMenuItem;
        TimerFollowUp: TTimer;
        itemRiskClassA: TLabel;
        itemRiskClassB: TLabel;
        itemRiskClassC: TLabel;
        Action_FollowUpColors: TMenuItem;
        SplitLine3: TBevel;
        imgEventLog: TImage;
        txtEventLog: TLabel;
        Action_INF7_Filter: TMenuItem;
        Action_CoCode_Filter: TMenuItem;
        Action_FollowUp_Filter: TMenuItem;
        Action_GroupFollowUp: TMenuItem;
        Action_INF4_Filter: TMenuItem;
        Action_HideSummary: TMenuItem;
        Action_ExportCSV: TMenuItem;
        Action_RemoveFilters: TMenuItem;
        Action_Free1: TMenuItem;
        AppMenu: TPanel;
        Separator1: TBevel;
        Separator2: TBevel;
        OpenItemsPanel: TPanel;
        AddressBookPanel: TPanel;
        InvoiceTrackerPanel: TPanel;
        CoCodesPanel: TPanel;
        PaidInfoPanel: TPanel;
        PmtTermsPanel: TPanel;
        SettingsInnerSections: TPanel;
        SettingsInnerValues: TPanel;
        btnUnlock: TSpeedButton;
        btnPassUpdate: TSpeedButton;
        Action_AddFollowUpGroup: TMenuItem;
        Action_RemoveFollowUps: TMenuItem;
        Action_MassMailer: TMenuItem;
        btnPasswordPreview: TSpeedButton;
        ShapeShowPassword: TShape;
        N20: TMenuItem;
        N21: TMenuItem;
        Action_Free2: TMenuItem;
        Action_ViewOptions: TMenuItem;
        N16: TMenuItem;
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
        ControlStatusPanel: TPanel;
        sgControlStatus: TStringGrid;
        Tables: TPageControl;
        Page1: TTabSheet;
        Page2: TTabSheet;
        Page3: TTabSheet;
        Page6: TTabSheet;
        Page7: TTabSheet;
        Page8: TTabSheet;
        Page9: TTabSheet;
        Page10: TTabSheet;
        PersonRespPanel: TPanel;
        sgPersonResp: TStringGrid;
        SalesRespPanel: TPanel;
        sgSalesResp: TStringGrid;
        AccountTypePanel: TPanel;
        sgAccountType: TStringGrid;
        CustomerGrPanel: TPanel;
        sgCustomerGr: TStringGrid;
        DebtorsPanel: TPanel;
        shapeFrame: TShape;
        Action_Free3: TMenuItem;
        N4: TMenuItem;
        N23: TMenuItem;
        Action_SalesResp: TMenuItem;
        Action_CustomerGrp: TMenuItem;
        Action_PersonResp: TMenuItem;
        Action_AccountType: TMenuItem;
        N24: TMenuItem;
        TabSheet5: TTabSheet;
        sgFSCview: TStringGrid;
        sgLBUview: TStringGrid;
        PanelFSC: TPanel;
        PanelLBU: TPanel;
        Cap62: TShape;
        PanelFscGrid: TPanel;
        Cap63: TShape;
        PanelLbuGrid: TPanel;
        FSCComment: TMemo;
        btnFscApprove: TSpeedButton;
        btnFscReject: TSpeedButton;
        LbuComment: TMemo;
        btnLbuUpdate: TSpeedButton;
        PanelFscComment: TPanel;
        PanelLbuComment: TPanel;
        PanelFscDetails: TPanel;
        PanelLbuDetails: TPanel;
        LabelOpAmountFsc: TLabel;
        LabelAmountFsc: TLabel;
        LabelOpAmCurrFsc: TLabel;
        LabelAmCurrFsc: TLabel;
        LabelFscDueDtFsc: TLabel;
        LabelFscValDtFsc: TLabel;
        ValueOpAmountFsc: TLabel;
        ValueAmountFsc: TLabel;
        ValueOpAmCurrFsc: TLabel;
        ValueAmCurrFsc: TLabel;
        ValueDueDtFsc: TLabel;
        ValueValDtFsc: TLabel;
        FscQueryDesc: TMemo;
        LabelDescFsc: TLabel;
        MemoBorders1: TShape;
        MemoBorders2: TShape;
        MemoBorders3: TShape;
        MemoBorders4: TShape;
        LabelOpAmountLbu: TLabel;
        ValueOpAmountLbu: TLabel;
        ValueAmountLbu: TLabel;
        ValueOpAmCurrLbu: TLabel;
        ValueAmCurrLbu: TLabel;
        ValueDueDtLbu: TLabel;
        ValueValDtLbu: TLabel;
        LabelDescLbu: TLabel;
        LabelAmountLbu: TLabel;
        LabelOpenAmCurrLbu: TLabel;
        LabelOpeAmCurrLbu: TLabel;
        LabelDueDtLbu: TLabel;
        LabelValDtLbu: TLabel;
        LbuQueryDesc: TMemo;
        Action_TurnRowHighlight: TMenuItem;
        AppHeader: TPanel;
        imgAppMenu: TImage;
        txtAppName: TLabel;
        btnInfo: TPanel;
        imgInfo: TImage;
        txtInfo: TLabel;
        btnFeedback: TPanel;
        imgFeedback: TImage;
        txtFeedback: TLabel;
        imgAADUser: TImage;
        valAadUser: TLabel;
        Separator3: TBevel;
        Separator4: TBevel;
        SettingsInnerHeader: TPanel;
        valCutOffDate: TLabel;
        txtRcaAmount: TLabel;
        txtRcbAmount: TLabel;
        txtRccAmount: TLabel;
        txtRcaItems: TLabel;
        txtRcbItems: TLabel;
        txtRccItems: TLabel;
        imgRefreshReport: TImage;
        imgGetAgingReport: TImage;
        txtGetAgingReport: TLabel;
        txtRefreshReport: TLabel;
        btnSearchAb: TImage;
        txtSearchAb: TLabel;
        selAgeSorting: TComboBox;
        txtAgeSorting: TLabel;
        bevelVertSeparator: TBevel;
        bevelVertLine: TBevel;
        bevelHorzLine: TBevel;
        ImageGrip: TImage;
        Action_HideThisColumn: TMenuItem;
        Action_ShowAllColumns: TMenuItem;
        N12: TMenuItem;
        N5: TMenuItem;
        N22: TMenuItem;
        TimerPermitCheck: TTimer;
        PopupLogin: TPopupMenu;
        Action_LoginRedeem: TMenuItem;
        N25: TMenuItem;
        Action_ClearCoockies: TMenuItem;
        Action_ClearCache: TMenuItem;
        imgOFF2: TImage;
        BevelSepLine: TBevel;
        txtInfoLine1: TLabel;
        txtInfoLine2: TLabel;
        txtInfoLine3: TLabel;
        Action_InGoogle: TMenuItem;
        Action_CopySelection: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure Action_HideAppClick(Sender: TObject);
        procedure Action_ShowAppClick(Sender: TObject);
        procedure Action_HelpClick(Sender: TObject);
        procedure Action_AboutClick(Sender: TObject);
        procedure Action_OnTopClick(Sender: TObject);
        procedure Action_AccountDetailsClick(Sender: TObject);
        procedure Action_BrowserModeClick(Sender: TObject);
        procedure Action_GoogleItClick(Sender: TObject);
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
        procedure imgLockClick(Sender: TObject);
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
        procedure btnOpenAbMouseEnter(Sender: TObject);
        procedure btnOpenAbMouseLeave(Sender: TObject);
        procedure btnCloseAbMouseEnter(Sender: TObject);
        procedure btnCloseAbMouseLeave(Sender: TObject);
        procedure btnExportAbMouseEnter(Sender: TObject);
        procedure btnExportAbMouseLeave(Sender: TObject);
        procedure btnRatingClick(Sender: TObject);
        procedure btnRatingMouseEnter(Sender: TObject);
        procedure btnRatingMouseLeave(Sender: TObject);
        procedure btnNavBackClick(Sender: TObject);
        procedure btnNavBackMouseLeave(Sender: TObject);
        procedure btnNavForwardClick(Sender: TObject);
        procedure btnNavForwardMouseEnter(Sender: TObject);
        procedure btnNavForwardMouseLeave(Sender: TObject);
        procedure btnNavReloadClick(Sender: TObject);
        procedure btnNavReloadMouseEnter(Sender: TObject);
        procedure btnNavReloadMouseLeave(Sender: TObject);
        procedure btnNavHomeClick(Sender: TObject);
        procedure btnNavHomeMouseEnter(Sender: TObject);
        procedure btnNavHomeMouseLeave(Sender: TObject);
        procedure btnNavBackMouseEnter(Sender: TObject);
        procedure sgListSectionKeyPress(Sender: TObject; var Key: Char);
        procedure sgListValueClick(Sender: TObject);
        procedure sgListSectionClick(Sender: TObject);
        procedure EditPasswordKeyPress(Sender: TObject; var Key: Char);
        procedure TimerCurrentTimeTimer(Sender: TObject);
        procedure TimerUpTimeTimer(Sender: TObject);
        procedure sgListSectionMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListSectionMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListValueMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListValueMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgListSectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnCloseAbClick(Sender: TObject);
        procedure btnOpenAbClick(Sender: TObject);
        procedure btnExportAbClick(Sender: TObject);
        procedure TabSheet7Show(Sender: TObject);
        procedure TabSheet7Resize(Sender: TObject);
        procedure sgCoCodesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgCoCodesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgPmtTermsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgPmtTermsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgPaidInfoMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgPaidInfoMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure TimerCustSnapshotsTimer(Sender: TObject);
        procedure EditGroupNameKeyPress(Sender: TObject; var Key: Char);
        procedure sgAgeViewMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgAgeViewMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgInvoiceTrackerMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure sgInvoiceTrackerMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure Action_TrackerClick(Sender: TObject);
        procedure Action_RemoveClick(Sender: TObject);
        procedure Action_ShowMyClick(Sender: TObject);
        procedure Action_ShowAllClick(Sender: TObject);
        procedure Action_LyncCallClick(Sender: TObject);
        procedure Action_CloseClick(Sender: TObject);
        procedure TabSheet4Show(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure TrayIconDblClick(Sender: TObject);
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
        procedure PopupAgeViewPopup(Sender: TObject);
        procedure sgCoCodesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgPaidInfoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgPmtTermsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure Action_AutoColumnSizeClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure Action_SearchClick(Sender: TObject);
        procedure Action_CopyClick(Sender: TObject);
        procedure Action_DelRowClick(Sender: TObject);
        procedure PopupBookPopup(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure Action_ToExceClick(Sender: TObject);
        procedure sgAgeViewColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
        procedure Action_SearchBookClick(Sender: TObject);
        procedure Action_OverdueClick(Sender: TObject);
        procedure Action_RowHighlightClick(Sender: TObject);
        procedure Action_ReportClick(Sender: TObject);
        procedure Action_ExportTransactionsClick(Sender: TObject);
        procedure Action_SelectAllClick(Sender: TObject);
        procedure Action_CopyToCBClick(Sender: TObject);
        procedure Action_AutoColumnClick(Sender: TObject);
        procedure Action_ColumnWidthClick(Sender: TObject);
        procedure TimerFollowUpTimer(Sender: TObject);
        procedure Action_FollowUpColorsClick(Sender: TObject);
        procedure imgEventLogClick(Sender: TObject);
        procedure imgEventLogMouseEnter(Sender: TObject);
        procedure imgEventLogMouseLeave(Sender: TObject);
        procedure Action_CoCode_FilterClick(Sender: TObject);
        procedure Action_FollowUp_FilterClick(Sender: TObject);
        procedure Action_INF4_FilterClick(Sender: TObject);
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
        procedure sgCoCodesMouseEnter(Sender: TObject);
        procedure sgPaidInfoMouseEnter(Sender: TObject);
        procedure sgPmtTermsMouseEnter(Sender: TObject);
        procedure sgInvoiceTrackerMouseEnter(Sender: TObject);
        procedure sgAddressBookMouseEnter(Sender: TObject);
        procedure sgOpenItemsMouseEnter(Sender: TObject);
        procedure sgAgeViewMouseEnter(Sender: TObject);
        procedure sgOpenItemsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgInvoiceTrackerKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
        procedure Action_Free3Click(Sender: TObject);
        procedure Action_SalesRespClick(Sender: TObject);
        procedure Action_CustomerGrpClick(Sender: TObject);
        procedure Action_PersonRespClick(Sender: TObject);
        procedure Action_AccountTypeClick(Sender: TObject);
        procedure Action_ViewOptionsClick(Sender: TObject);
        procedure Action_ShowNavigationClick(Sender: TObject);
        procedure TabSheet5Show(Sender: TObject);
        procedure sgFSCviewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgLBUviewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgLBUviewClick(Sender: TObject);
        procedure sgFSCviewClick(Sender: TObject);
        procedure btnFscApproveClick(Sender: TObject);
        procedure btnFscRejectClick(Sender: TObject);
        procedure btnLbuUpdateClick(Sender: TObject);
        procedure Action_TurnRowHighlightClick(Sender: TObject);
        procedure PopupCommonMenuPopup(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
        procedure imgAppMenuClick(Sender: TObject);
        procedure btnInfoMouseEnter(Sender: TObject);
        procedure btnInfoMouseLeave(Sender: TObject);
        procedure txtInfoClick(Sender: TObject);
        procedure txtFeedbackClick(Sender: TObject);
        procedure btnFeedbackMouseEnter(Sender: TObject);
        procedure btnFeedbackMouseLeave(Sender: TObject);
        procedure imgGetAgingReportClick(Sender: TObject);
        procedure imgRefreshReportClick(Sender: TObject);
        procedure imgGetAgingReportMouseEnter(Sender: TObject);
        procedure imgGetAgingReportMouseLeave(Sender: TObject);
        procedure imgRefreshReportMouseEnter(Sender: TObject);
        procedure imgRefreshReportMouseLeave(Sender: TObject);
        procedure btnSearchAbClick(Sender: TObject);
        procedure btnSearchAbMouseEnter(Sender: TObject);
        procedure btnSearchAbMouseLeave(Sender: TObject);
        procedure btnReloadClick(Sender: TObject);
        procedure Action_HideThisColumnClick(Sender: TObject);
        procedure Action_ShowAllColumnsClick(Sender: TObject);
        procedure TimerPermitCheckTimer(Sender: TObject);
        procedure Action_LoginRedeemClick(Sender: TObject);
        procedure ChromiumAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
        procedure Action_ClearCoockiesClick(Sender: TObject);
        procedure Action_ClearCacheClick(Sender: TObject);
        procedure PopupTrackerPopup(Sender: TObject);
        procedure PopupLoginPopup(Sender: TObject);
        procedure Action_InGoogleClick(Sender: TObject);
        procedure Action_CopySelectionClick(Sender: TObject);
        procedure Page1Show(Sender: TObject);
        procedure Page2Show(Sender: TObject);
        procedure Page3Show(Sender: TObject);
        procedure Page6Show(Sender: TObject);
        procedure Page7Show(Sender: TObject);
        procedure Page8Show(Sender: TObject);
        procedure Page9Show(Sender: TObject);
        procedure Page10Show(Sender: TObject);
    protected
        procedure CreateParams(var Params: TCreateParams); override;
        procedure WndProc(var msg: TMessage); override;   // Windows events
        procedure WndMessagesChromium(PassMsg: TMessage); // Chromium events
        procedure WndMessagesWindows(PassMsg: TMessage);  // Process windows close/suspend events
        procedure WndMessagesExternal(PassMsg: TMessage); // Get lync call details
        procedure NotifyMoveOrResizeStarted;
        procedure ChromiumModalLoopOn(PassMsg: TMessage);
        procedure ChromiumModalLoopOff(PassMsg: TMessage);
        procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
            targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
            var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean; var Result: Boolean);
    strict private
        const FPermitCheckTimeout = 900000; // 15 min.
        const AppMenuTextSelected = $006433C9;
        const AppMenuTextNormal = clGrayText;
        const AppButtonTxtNormal = $00555555;
        const AppButtonTxtSelected = $006433C9;
        const AppMenuShown = 170;
        const AppMenuHidden = 0;
        var FAllowEditConfig: boolean;
        var FLoadedCompanies: TList<string>;
        var FLoadedAgeDate: string;
        var FPermitCheckTimer: integer;
        var FIsAppMenuLocked: boolean;
        var FHadFirstLoad: boolean;
        var FAllowClose: boolean;
        var FIsAddressBookOpened: boolean;
        var FFollowsToday: integer;
        var FFollowsPast: integer;
        var FFollowsNext: integer;
        var FExcelFileName: string;
        procedure UserRatingCheck();
        function  CanAccessAppMenu(): boolean;
        procedure RequestUnityWebWithToken();
        procedure RedeemAccess();
        procedure PermitCheckInit();
        procedure GetUserPermissions();
        procedure SetPanelBorders();
        procedure SetGridColumnWidths();
        procedure SetGridRowHeights();
        procedure SetButtonsGlyphs();
        procedure SetSettingsPanel(IsLocked: boolean);
        procedure InitializeScreenSettings();
        function  AddressBookExclusion: boolean;
        procedure ClearMainViewInfo();
        procedure ClearAgingSummary();
        procedure ClearOpenItemsSummary();
        procedure LoadOpenItems();
        procedure SkypeCallUpdate(CallTime: cardinal);
        procedure FollowUpsUpdate(Source: TStringGrid; CommonDate: string);
        function  UpdateFreeFields(Source: TStringGrid): TFreeFieldsPayLoad;
        procedure OpenAddressBook_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
        procedure UpdateAddressBook_Callback(CallResponse: TCallResponse);
        procedure AddToAddressBook_Callback(ReturnedId: integer; CallResponse: TCallResponse);
        procedure ReadAgeView_Callback(ReturnedData: TStringGrid; PayLoad: TAgingPayLoad; CallResponse: TCallResponse);
        procedure ScanSnapshots_Callback(CanGetAge: boolean; ReceivedTime: string; CallResponse: TCallResponse);
        procedure ReadOpenItems_Callback(OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse);
        procedure FreeFieldsUpdate_Callback(CallResponse: TCallResponse);
        procedure CheckGivenPassword_Callback(CallResponse: TCallResponse);
        procedure SetNewPassword_Callback(CallResponse: TCallResponse);
        procedure BulkFollowUpUpdate_Callback(CallResponse: TCallResponse);
        procedure GetAgingReport_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
        procedure WriteToExcel_Callback(CallResponse: TCallResponse);
        procedure LoadRating_Callback(Rating: TRating; CallResponse: TCallResponse);
    public
        var FRiskClassGroup: TRiskClassGroup;
        var FStartTime: TTime;
        var FGridPicture: TImage;
        var FDataUpdate: string;
        var FDataStatus: string;
        procedure UpdateAgeSummary(PayLoad: TAgingPayLoad);
        procedure UpdateFollowUps(Source: TStringGrid);
        procedure SetActiveTabsheet(TabSheet: TTabSheet);
        procedure ResetTabsheetButtons();
        procedure InitMainWnd();
        procedure SetupMainWnd();
        procedure StartMainWnd();
        procedure SwitchTimers(State: TAppTimers);
        procedure UpdateStatusBar(Text: string);
        procedure LoadAgeReport();
        property LoadedAgeDate: string read FLoadedAgeDate;
        property LoadedCompanies: TList<string> read FLoadedCompanies;
        property FollowsToday: integer read FFollowsToday;
        property FollowsPast: integer read FFollowsPast;
        property FollowsNext: integer read FFollowsNext;
    end;


    function MainForm(): TMainForm;


implementation


{$R *.dfm}


uses
    System.NetEncoding,
    View.GridFilter,
    View.Actions,
    View.Calendar,
    View.About,
    View.GridSearch,
    View.ColorPicker,
    View.EventLog,
    View.UserFeedback,
    View.MassMailer,
    View.Startup,
    View.Reports,
    View.CompanyList,
    View.BusyScreen,
    View.RateApp,
    Unity.Constants,
    Unity.Helpers,
    Unity.Sorting,
    Unity.Settings,
    Unity.Service,
    uCEFApplication,
    Api.ReturnCompanies,
    Api.ReturnAccountType,
    Api.ReturnPaidInfo,
    Api.ReturnControlStatus,
    Api.ReturnPersonResponsible,
    Api.ReturnSalesResponsible,
    Api.ReturnPaymentTerms,
    Api.ReturnCustomerGroup,
    Api.ReturnOpenItems,
    Api.OpenItemsFields,
    Api.AddressBookFields,
    Api.CustomerSnapshotEx,
    Api.UserGeneralCommentUpdate;


var VMainForm: TMainForm;


function MainForm(): TMainForm;
begin
    if not(Assigned(VMainForm)) then Application.CreateForm(TMainForm, VMainForm);
    Result:=VMainForm;
end;


procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
    // --------------------------------------------
    // Assign task bar icon to this form,
    // so it will behave like application MainForm.
    // --------------------------------------------
    inherited
    CreateParams(Params);
    Params.ExStyle:=Params.ExStyle or WS_EX_APPWINDOW;
end;


{$REGION 'LOCAL HELPERS'}


procedure TMainForm.SetSettingsPanel(IsLocked: boolean);
begin

    if IsLocked then
    begin

        imgOFF1.Visible:=True;
        imgOFF2.Visible:=True;
        btnPassUpdate.Enabled:=False;

        EditCurrentPassword.Enabled:=False;
        EditNewPassword.Enabled:=False;
        EditNewPasswordConfirmation.Enabled:=False;
        EditCurrentPassword.Text:='';
        EditNewPassword.Text:='';
        EditNewPasswordConfirmation.Text:='';
        EditPassword.Text:='';

        sgListSection.ClearAll(2, 0, 0, False);
        sgListValue.ClearAll(2, 0, 0, False);
        sgListSection.Row:=1;
        sgListValue.Row:=1;
        sgListSection.Visible:=False;
        sgListValue.Visible:=False;
        sgListSection.Enabled:=False;
        sgListValue.Enabled:=False;

        btnUnlock.Caption:='Unlock';
        EditPassword.SetFocus();

    end
    else
    begin

        sgListSection.Cols[0].Text:='Lp';
        sgListSection.Cols[1].Text:='Sections';
        sgListValue.Cols[0].Text  :='Lp';
        sgListValue.Cols[1].Text  :='Key';
        sgListValue.Cols[2].Text  :='Value';

        btnPassUpdate.Enabled:=True;
        EditCurrentPassword.Enabled:=True;
        EditNewPassword.Enabled:=True;
        EditNewPasswordConfirmation.Enabled:=True;

        sgListSection.Enabled:=True;
        sgListValue.Enabled:=True;
        sgListSectionClick(self);
        sgListSection.Row:=1;
        sgListValue.Row:=1;
        sgListSection.Visible:=True;
        sgListValue.Visible:=True;

        imgOFF1.Visible:=False;
        imgOFF2.Visible:=False;

        btnUnlock.Caption:='Lock';
        EditPassword.SetFocus();

    end;

end;


procedure TMainForm.SwitchTimers(State: TAppTimers);
begin

    case State of

        TurnedOn:
        begin
            TimerFollowUp.Enabled:=True;
            TimerCustSnapshots.Enabled:=True;
        end;

        TurnedOff:
        begin
            TimerFollowUp.Enabled:=False;
            TimerCustSnapshots.Enabled:=False;
        end;

    end;

end;


procedure TMainForm.UpdateStatusBar(Text: string);
begin

    if not GetCurrentThreadId() = MainThreadID then
    begin

        TThread.Synchronize(nil, procedure
        begin
            valStatus.Caption:=Text;
        end);

    end
    else
    begin
        valStatus.Caption:=Text;
    end;

end;


procedure TMainForm.LoadAgeReport();
begin
    BusyForm.Show();
    Service.Mediator.Debtors.ReadAgeViewAsync(LoadedCompanies, selAgeSorting.Text, FRiskClassGroup, ReadAgeView_Callback);
end;


procedure TMainForm.LoadOpenItems();
begin

    sgOpenItems.Freeze(True);

    if LoadedCompanies.Count = 0 then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Please first load aging report for given company and/or companies.');
        Service.Logger.Log('[LoadOpenItems]: No aging report loded while open items requested.');
        Exit();
    end;

    Service.Logger.Log('[LoadOpenItems]: Calling ReadOpenItemsAsync for given company list.');
    Service.Mediator.OpenItems.ReadOpenItemsAsync(sgOpenItems, LoadedCompanies, ReadOpenItems_Callback);

end;


procedure TMainForm.ClearMainViewInfo();
begin
    valStatus.Caption     :='';
    valCurrentDate.Caption:='';
    valCurrentTime.Caption:='';
    valUpTime.Caption     :='';
    valAadUser.Caption    :='';
end;


procedure TMainForm.ClearAgingSummary();
begin

    valTotalCustomers.Caption:='0';

    amtTotal.Caption :='0';
    amtNotDue.Caption:='0';
    amtRange1.Caption:='0';
    amtRange2.Caption:='0';
    amtRange3.Caption:='0';
    amtRange4.Caption:='0';
    amtRange5.Caption:='0';
    amtRange6.Caption:='0';

    procTotal.Caption :='0';
    procNotDue.Caption:='0';
    procRange1.Caption:='0';
    procRange2.Caption:='0';
    procRange3.Caption:='0';
    procRange4.Caption:='0';
    procRange5.Caption:='0';
    procRange6.Caption:='0';

    amtExceeders.Caption    :='0';
    amtCreditExcess.Caption :='0';
    amtGrantedLimits.Caption:='0';
    amtNotOverdue.Caption   :='0';
    amtPastDue.Caption      :='0';
    amtDefaulted.Caption    :='0';

    amtRiskClassA.Caption:='0';
    amtRiskClassB.Caption:='0';
    amtRiskClassC.Caption:='0';

    itemRiskClassA.Caption:='0';
    itemRiskClassB.Caption:='0';
    itemRiskClassC.Caption:='0';

    procNotDue.Caption:='0';
    procRange1.Caption:='0';
    procRange2.Caption:='0';
    procRange3.Caption:='0';
    procRange4.Caption:='0';
    procRange5.Caption:='0';
    procRange6.Caption:='0';

    valFollowsToday.Caption:='0';
    valFollowsPast.Caption :='0';
    valFollowsNext.Caption :='0';

end;


procedure TMainForm.ClearOpenItemsSummary();
begin
    valOpenItems.Caption  :='0';
    valOverdue.Caption    :='0';
    valInvoices.Caption   :='0';
    amtOverdue.Caption    :='0';
    amtOutstanding.Caption:='0';
    amtUnallocated.Caption:='0';
end;


procedure TMainForm.SkypeCallUpdate(CallTime: cardinal);
begin

    var SourceDBName:=(sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._SourceDbName), sgAgeView.Row]);
    var CustNumber:=(sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._CustomerNumber), sgAgeView.Row]).ToInteger();

    var CommentExists: TCommentExists;
    var CallResponse: TCallResponse;
    CallResponse:=Service.Mediator.Comments.CheckDailyCommentAwaited(
        SourceDBName,
        CustNumber,
        LoadedAgeDate,
        CommentExists
    );

    var NewComment:=CommentExists.UserComment;
    if String.IsNullOrWhiteSpace(NewComment) then
        NewComment:='Called customer today.' else
            NewComment:=NewComment + #13#10 + 'Called customer today.';

    var LDailyCommentFields: TDailyCommentFields;
    LDailyCommentFields.CommentId           :=CommentExists.CommentId;
    LDailyCommentFields.SourceDBName        :=SourceDBName;
    LDailyCommentFields.CustomerNumber      :=CustNumber;
    LDailyCommentFields.AgeDate             :=LoadedAgeDate;
    LDailyCommentFields.CallDuration        :=CallTime;
    LDailyCommentFields.CallEvent           :=1;
    LDailyCommentFields.FixedStatementsSent :=0;
    LDailyCommentFields.CustomStatementsSent:=0;
    LDailyCommentFields.FixedRemindersSent  :=0;
    LDailyCommentFields.CustomRemindersSent :=0;
    LDailyCommentFields.UserComment         :=NewComment;
    LDailyCommentFields.UserAlias           :=Service.SessionData.AliasName;

    Service.Mediator.Comments.EditDailyCommentAsync(LDailyCommentFields, nil);

end;


procedure TMainForm.FollowUpsUpdate(Source: TStringGrid; CommonDate: string);
begin

    var Col1:=sgAgeView.GetCol(TCustomerSnapshotEx._SourceDbName);
    var Col2:=sgAgeView.GetCol(TCustomerSnapshotEx._CustomerNumber);
    var Col3:=sgAgeView.GetCol(TCustomerSnapshotEx._FollowUp);

    var Count:=(Source.Selection.Bottom - Source.Selection.Top) + 1;

    var FollowUpsPayLoad: TFollowUpsPayLoad;
    FollowUpsPayLoad.Initialize(Count);

    var Index:=0;
    for var SelIndex:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
    begin

        if sgAgeView.RowHeights[SelIndex] <> sgAgeView.sgRowHidden then
        begin

            sgAgeView.Cells[Col3, SelIndex]:=CommonDate;

            FollowUpsPayLoad.SourceDBNames[Index]  :=sgAgeView.Cells[Col1, SelIndex];
            FollowUpsPayLoad.CustomerNumbers[Index]:=sgAgeView.Cells[Col2, SelIndex].ToInt64();
            FollowUpsPayLoad.FollowUps[Index]      :=CommonDate;

            Inc(Index);

        end;

    end;

    Service.Mediator.Comments.BulkFollowUpUpdateAsync(FollowUpsPayLoad, BulkFollowUpUpdate_Callback);
    Service.Logger.Log('GeneralComment table with column FollowUp has been updated with ' + DateToStr(CalendarForm.FSelectedDate) + ' for multiple items.');

end;


function TMainForm.UpdateFreeFields(Source: TStringGrid): TFreeFieldsPayLoad;
begin

    var Count:=(Source.Selection.Bottom - Source.Selection.Top) + 1;

    var FreeFields: TFreeFieldsPayLoad;
    FreeFields.Initialize(Count);

    var Index:=0;
    for var SelIndex:=Source.Selection.Top to Source.Selection.Bottom do
    begin

        FreeFields.SourceDBNames[Index]  :=Source.Cells[Source.GetCol(TCustomerSnapshotEx._SourceDbName), SelIndex];
        FreeFields.CustomerNumbers[Index]:=Source.Cells[Source.GetCol(TCustomerSnapshotEx._CustomerNumber), SelIndex].ToInt64;
        FreeFields.Free1[Index]          :=Source.Cells[Source.GetCol(TCustomerSnapshotEx._Free1), SelIndex];
        FreeFields.Free2[Index]          :=Source.Cells[Source.GetCol(TCustomerSnapshotEx._Free2), SelIndex];
        FreeFields.Free3[Index]          :=Source.Cells[Source.GetCol(TCustomerSnapshotEx._Free3), SelIndex];

        Inc(Index);

    end;

    Result:=FreeFields;

end;


procedure TMainForm.SetActiveTabsheet(TabSheet: TTabSheet);
begin

    ResetTabsheetButtons();

    if TabSheet = TabSheet1 then
    begin
        txtDebtors.Font.Style:=[fsBold];
        txtDebtors.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet2 then
    begin
        txtOpenItems.Font.Style:=[fsBold];
        txtOpenItems.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet3 then
    begin
        txtAddressBook.Font.Style:=[fsBold];
        txtAddressBook.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet4 then
    begin
        txtTracker.Font.Style:=[fsBold];
        txtTracker.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet5 then
    begin
        txtQueries.Font.Style:=[fsBold];
        txtQueries.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet6 then
    begin
        txtUnidentified.Font.Style:=[fsBold];
        txtUnidentified.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet7 then
    begin
        txtTables.Font.Style:=[fsBold];
        txtTables.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet8 then
    begin
        txtSettings.Font.Style:=[fsBold];
        txtSettings.Font.Color:=$006433C9;
    end;

    if TabSheet = TabSheet9 then
    begin
        txtStart.Font.Style:=[fsBold];
        txtStart.Font.Color:=$006433C9;
    end;

    TabSheets.ActivePage:=TabSheet;

end;


procedure TMainForm.ResetTabsheetButtons();
begin
    txtStart.Font.Style       :=[];
    txtStart.Font.Color       :=AppMenuTextNormal;
    txtReports.Font.Style     :=[];
    txtReports.Font.Color     :=AppMenuTextNormal;
    txtDebtors.Font.Style     :=[];
    txtDebtors.Font.Color     :=AppMenuTextNormal;
    txtTracker.Font.Style     :=[];
    txtTracker.Font.Color     :=AppMenuTextNormal;
    txtAddressBook.Font.Style :=[];
    txtAddressBook.Font.Color :=AppMenuTextNormal;
    txtOpenItems.Font.Style   :=[];
    txtOpenItems.Font.Color   :=AppMenuTextNormal;
    txtUnidentified.Font.Style:=[];
    txtUnidentified.Font.Color:=AppMenuTextNormal;
    txtQueries.Font.Style     :=[];
    txtQueries.Font.Color     :=AppMenuTextNormal;
    txtTables.Font.Style      :=[];
    txtTables.Font.Color      :=AppMenuTextNormal;
    txtSettings.Font.Style    :=[];
    txtSettings.Font.Color    :=AppMenuTextNormal;
    txtFeedback.Font.Style    :=[];
    txtFeedback.Font.Color    :=AppMenuTextNormal;
    txtInfo.Font.Style        :=[];
    txtInfo.Font.Color        :=AppMenuTextNormal;
    txtRating.Font.Style      :=[];
    txtRating.Font.Color      :=AppMenuTextNormal;
end;


procedure TMainForm.UpdateAgeSummary(PayLoad: TAgingPayLoad);
begin

    valTotalCustomers.Caption:=IntToStr(PayLoad.CustAll);

    amtNotDue.Caption:=FormatFloat('#,##0.00', PayLoad.ANotDue);
    amtRange1.Caption:=FormatFloat('#,##0.00', PayLoad.ARange1);
    amtRange2.Caption:=FormatFloat('#,##0.00', PayLoad.ARange2);
    amtRange3.Caption:=FormatFloat('#,##0.00', PayLoad.ARange3);
    amtRange4.Caption:=FormatFloat('#,##0.00', PayLoad.ARange4);
    amtRange5.Caption:=FormatFloat('#,##0.00', PayLoad.ARange5);
    amtRange6.Caption:=FormatFloat('#,##0.00', PayLoad.ARange6);
    amtTotal.Caption :=FormatFloat('#,##0.00', PayLoad.Balance);

    if not (PayLoad.Balance = 0) then
    begin
        procNotDue.Caption:=FormatFloat('0.00', ( (PayLoad.ANotDue / PayLoad.Balance) * 100 )) + '%';
        procRange1.Caption:=FormatFloat('0.00', ( (PayLoad.ARange1 / PayLoad.Balance) * 100 )) + '%';
        procRange2.Caption:=FormatFloat('0.00', ( (PayLoad.ARange2 / PayLoad.Balance) * 100 )) + '%';
        procRange3.Caption:=FormatFloat('0.00', ( (PayLoad.ARange3 / PayLoad.Balance) * 100 )) + '%';
        procRange4.Caption:=FormatFloat('0.00', ( (PayLoad.ARange4 / PayLoad.Balance) * 100 )) + '%';
        procRange5.Caption:=FormatFloat('0.00', ( (PayLoad.ARange5 / PayLoad.Balance) * 100 )) + '%';
        procRange6.Caption:=FormatFloat('0.00', ( (PayLoad.ARange6 / PayLoad.Balance) * 100 )) + '%';
        procTotal.Caption :=FormatFloat('0.00', ( ( (PayLoad.ANotDue / PayLoad.Balance) +
                                                           (PayLoad.ARange1 / PayLoad.Balance) +
                                                           (PayLoad.ARange2 / PayLoad.Balance) +
                                                           (PayLoad.ARange3 / PayLoad.Balance) +
                                                           (PayLoad.ARange4 / PayLoad.Balance) +
                                                           (PayLoad.ARange5 / PayLoad.Balance) +
                                                           (PayLoad.ARange6 / PayLoad.Balance) ) * 100 ) ) + '%';
    end;

    amtRiskClassA.Caption :=FormatFloat('#,##0.00', PayLoad.RCA);
    amtRiskClassB.Caption :=FormatFloat('#,##0.00', PayLoad.RCB);
    amtRiskClassC.Caption :=FormatFloat('#,##0.00', PayLoad.RCC);

    itemRiskClassA.Caption:=IntToStr(PayLoad.RCAcount) + ' cust.';
    itemRiskClassB.Caption:=IntToStr(PayLoad.RCBcount) + ' cust.';
    itemRiskClassC.Caption:=IntToStr(PayLoad.RCCcount) + ' cust.';

    amtExceeders.Caption    :=IntToStr(PayLoad.Exceeders);
    amtCreditExcess.Caption :=FormatFloat('#,##0.00', PayLoad.TotalExceed);
    amtGrantedLimits.Caption:=FormatFloat('#,##0.00', PayLoad.Limits);
    amtNotOverdue.Caption   :=amtNotDue.Caption;
    amtPastDue.Caption      :=FormatFloat('#,##0.00', (PayLoad.ARange1 + PayLoad.ARange2 + PayLoad.ARange3));
    amtDefaulted.Caption    :=FormatFloat('#,##0.00', (PayLoad.ARange4 + PayLoad.ARange5 + PayLoad.ARange6));

end;


procedure TMainForm.UpdateFollowUps(Source: TStringGrid);
begin

    FFollowsToday:=0;
    FFollowsNext :=0;
    FFollowsPast :=0;

    var Col:=sgAgeView.GetCol(TCustomerSnapshotEx._FollowUp);

    for var Index:=0 to Source.RowCount - 1 do
    begin

        if not String.IsNullOrWhitespace(Source.Cells[Col, Index]) then
        begin

            // Today follow-ups
            if THelpers.CDate(Source.Cells[Col, Index]) = THelpers.CDate(valCurrentDate.Caption) then
                Inc(FFollowsToday);

            // Future follow-ups
            if THelpers.CDate(Source.Cells[Col, Index]) > THelpers.CDate(valCurrentDate.Caption) then
                Inc(FFollowsNext);

            // Past follow-ups
            if THelpers.CDate(Source.Cells[Col, Index]) < THelpers.CDate(valCurrentDate.Caption) then
                Inc(FFollowsPast);

        end;

    end;

    valFollowsToday.Caption:=FFollowsToday.ToString();
    valFollowsNext.Caption :=FFollowsNext.ToString();
    valFollowsPast.Caption :=FFollowsPast.ToString();

end;


procedure TMainForm.UserRatingCheck();
begin

    // Executes after 5 minutes
    THelpers.ExecWithDelay(300000, procedure
    begin
        Service.Mediator.Accounts.LoadRatingAsync(LoadRating_Callback);
    end);

end;


function TMainForm.CanAccessAppMenu(): boolean;
begin

    Result:=True;

    if FIsAppMenuLocked then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have access to this feature. Active Directory validation is not completed.');
        Result:=False;
    end;

end;


procedure TMainForm.RequestUnityWebWithToken();
begin

    var BaseUrl:=Service.Settings.GetStringValue(TConfigSections.ApplicationDetails, 'START_PAGE', '');
    var Url:=WideString(BaseUrl) + '/?sessiontoken=' + Service.SessionId;

    try
        Chromium.LoadURL(Url);
        Service.Logger.Log('[RequestUnityWebWithToken]: Requested URL = "' + Url + '".');
    except
        on E: exception do
            Service.Logger.Log('[RequestUnityWebWithToken]: Cannot load URL: ' + URL + '. The error has been thrown: ' + E.Message);
    end;

end;


procedure TMainForm.RedeemAccess();
begin

    var CallResponse: TCallResponse;
    CallResponse:=Service.Mediator.Accounts.CheckSessionAwaited(Service.SessionId);

    if CallResponse.IsSucceeded then
    begin
        TimerPermitCheck.Enabled:=False;
        FIsAppMenuLocked:=False;
        valAadUser.Caption:=Service.SessionData.DisplayName;
        Service.Logger.Log('[RedeemAccess]: Redeem user access to account has been completed successfully.');
        GetUserPermissions();
        UserRatingCheck();
    end;

end;


procedure TMainForm.GetUserPermissions();
begin

    var LCallResponse: TCallResponse;
    LCallResponse:=Service.Mediator.Accounts.GetUserPermissionsAwaited();
    Service.Logger.Log('[GetUserPermissions]: Getting current user permissions...');

    if not LCallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, '[GetUserPermissions]: Cannot get user permissions. Please contact IT Support. Description: ' + LCallResponse.LastMessage);
        Service.Logger.Log('[GetUserPermissions]: Cannot get user permissions. Error has been thrown: ' + LCallResponse.LastMessage);
    end
    else
    begin
        Service.Logger.Log('[GetUserPermissions]: User permissions refreshed.');
    end;

end;


procedure TMainForm.PermitCheckInit();
begin
    TimerPermitCheck.Interval:=3000;
    TimerPermitCheck.Enabled:=True;
    FPermitCheckTimer:=0;
end;


procedure TMainForm.SetPanelBorders();
begin
    PanelNavigation.Borders      (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    AppHeader.Borders            ($00E3B268, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    DebtorsPanel.Borders         (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    OpenItemsPanel.Borders       (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    AddressBookPanel.Borders     (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    InvoiceTrackerPanel.Borders  (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    CoCodesPanel.Borders         (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    ControlStatusPanel.Borders   (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PaidInfoPanel.Borders        (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PmtTermspanel.Borders        (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    SalesRespPanel.Borders       (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PersonRespPanel.Borders      (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    CustomerGrPanel.Borders      (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    AccountTypePanel.Borders     (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    SettingsInnerSections.Borders(clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    SettingsInnerValues.Borders  (clWhite,   $00E3B268, $00E3B268, $00E3B268, $00E3B268);
end;


procedure TMainForm.SetGridColumnWidths();
begin
    sgOpenItems.SetColWidth     (10, 20, 400);
    sgAddressBook.SetColWidth   (10, 20, 400);
    //sgListValue.SetColWidth     (25, 20, 400);
    //sgListSection.SetColWidth   (25, 20, 400);
    sgInvoiceTracker.SetColWidth(10, 20, 400);
    sgCoCodes.SetColWidth       (10, 30, 400);
    sgControlStatus.SetColWidth (10, 30, 400);
    sgPaidInfo.SetColWidth      (10, 30, 400);
    sgPmtTerms.SetColWidth      (10, 30, 400);
    sgSalesResp.SetColWidth     (10, 20, 400);
    sgPersonResp.SetColWidth    (10, 20, 400);
    sgCustomerGr.SetColWidth    (10, 20, 400);
    sgAccountType.SetColWidth   (10, 20, 400);
end;


procedure TMainForm.SetGridRowHeights();
begin
    sgOpenItems.SetRowHeight     (sgOpenItems.sgRowHeight,      25);
    sgAddressBook.SetRowHeight   (sgAddressBook.sgRowHeight,    25);
    sgListValue.SetRowHeight     (sgListValue.sgRowHeight,      25);
    sgListSection.SetRowHeight   (sgListSection.sgRowHeight,    25);
    sgAgeView.SetRowHeight       (sgAgeView.sgRowHeight,        25);
    sgInvoiceTracker.SetRowHeight(sgInvoiceTracker.sgRowHeight, 25);
    sgCoCodes.SetRowHeight       (sgCoCodes.sgRowHeight,        25);
    sgControlStatus.SetRowHeight (sgControlStatus.sgRowHeight,  25);
    sgPaidInfo.SetRowHeight      (sgPaidInfo.sgRowHeight,       25);
    sgPmtTerms.SetRowHeight      (sgPmtTerms.sgRowHeight,       25);
    sgSalesResp.SetRowHeight     (sgSalesResp.sgRowHeight,      25);
    sgPersonResp.SetRowHeight    (sgPersonResp.sgRowHeight,     25);
    sgAccountType.SetRowHeight   (sgAccountType.sgRowHeight,    25);
    sgCustomerGr.SetRowHeight    (sgCustomerGr.sgRowHeight,     25);
end;


procedure TMainForm.SetButtonsGlyphs();
begin
    // ---------------------------------------------------------------------------
    // Make sure that the glyph styled buttons have proper transparency color set.
    // Note: always set transparent color.
    // ---------------------------------------------------------------------------
    btnLbuUpdate.Glyph.Transparent:=True;
    btnLbuUpdate.Glyph.TransparentColor:=clWhite;
end;


function TMainForm.AddressBookExclusion(): boolean;
begin
    // ----------------------------------------------------------------------
    // Indicates editable columns. Use it to examin if user should be able to
    // edit selected cell in TStrigGrid component.
    // ----------------------------------------------------------------------
    if (sgAddressBook.Col = sgAddressBook.GetCol(TAddressBookFields._StatementEmails))
        or (sgAddressBook.Col = sgAddressBook.GetCol(TAddressBookFields._PhoneNumbers))
        or (sgAddressBook.Col = sgAddressBook.GetCol(TAddressBookFields._ContactPerson))
        or (sgAddressBook.Col = sgAddressBook.GetCol(TAddressBookFields._RegularEmails))
    then
        // Do not exclude above columns from editing
        Result:=False
    else
        // Exclude anything else from editing
        Result:=True;
end;


procedure TMainForm.InitializeScreenSettings();
begin

    if Screen.MonitorCount > 1 then
    begin

        var LastTopPos :=Service.Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_TOP', 24);
        var LastLeftPos:=Service.Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_LEFT', 24);
        var LastWidth  :=Service.Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_WIDTH', 1024);
        var LastHeight :=Service.Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_HEIGHT', 1024);

        MainForm.DefaultMonitor:=dmDesktop;
        MainForm.Top   :=LastTopPos;
        MainForm.Width :=LastWidth;
        MainForm.Height:=LastHeight;

        // ------------------------------------------------------------------
        // To prevent displaying program window out of the screen area,
        // we check whether last saved window position fits the desktop area,
        // and then setup the "Left" property.
        // ------------------------------------------------------------------
        if (LastLeftPos > 0) and (LastLeftPos < (Screen.DesktopWidth - MainForm.Width)) then MainForm.Left:=LastLeftPos;

    end
    else
    begin
        MainForm.DefaultMonitor:=dmPrimary;
        MainForm.Position:=poDesktopCenter;
    end;


end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TMainForm.OpenAddressBook_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
begin

    BusyForm.Close();

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        MainForm.UpdateStatusBar(TStatusBar.Ready);
        Service.Logger.Log('[OpenAddressBookAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    sgAddressBook.Freeze(True);
    try

        sgAddressBook.RowCount:=ReturnedData.RowCount;
        sgAddressBook.ColCount:=ReturnedData.ColCount;

        for var iCNT:=0 to ReturnedData.RowCount - 1 do
            for var jCNT:=0 to ReturnedData.ColCount - 1 do
                sgAddressBook.Cells[jCNT, iCNT]:=ReturnedData.Cells[jCNT, iCNT];

    finally
        sgAddressBook.Freeze(False);
        sgAddressBook.SetColWidth(10, 20, 400);
        sgAddressBook.ColWidths[1]:=-1;
    end;

    FIsAddressBookOpened:=True;
    MainForm.UpdateStatusBar(TStatusBar.Ready);
    Service.Logger.Log('[OpenAddressBookAsync_Callback]: Address Book has been opened.');

end;


procedure TMainForm.UpdateAddressBook_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, CallResponse.LastMessage);
        Service.Logger.Log('[UpdateAddressBookAsync_Callback]: Adddress Book has thrown an error "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    THelpers.MsgCall(MainForm.Handle, TAppMessage.Info, 'Address Book has been updated successfully.');
    Service.Logger.Log('[UpdateAddressBookAsync_Callback]: Address Book updated.');

end;


procedure TMainForm.AddToAddressBook_Callback(ReturnedId: integer; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[AddToAddressBookAsync_Callback]: Adddress Book has thrown an error "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    THelpers.MsgCall(MainForm.Handle, TAppMessage.Info, 'New data has been added to Address Book successfully.');
    Service.Logger.Log('[AddToAddressBookAsync_Callback]: New data has been inserted to Adddress Book successfully.');

end;


procedure TMainForm.ReadAgeView_Callback(ReturnedData: TStringGrid; PayLoad: TAgingPayLoad; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        UpdateStatusBar(TStatusBar.Ready);
        Service.Logger.Log('[ReadAgeViewAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    sgAgeView.Freeze(True);
    try

        sgAgeView.RowCount:=ReturnedData.RowCount;
        sgAgeView.ColCount:=ReturnedData.ColCount;

        for var iCNT:=0 to ReturnedData.RowCount - 1 do
            for var jCNT:=0 to ReturnedData.ColCount - 1 do
                sgAgeView.Cells[jCNT, iCNT]:=ReturnedData.Cells[jCNT, iCNT];

        Service.Logger.Log('[ReadAgeViewAsync_Callback]: Age View updated.');

    finally
        sgAgeView.Freeze(False);
        Service.Logger.Log('[ReadAgeViewAsync_Callback]: VCL unlocked and repainted.');
    end;

    sgAgeView.SetColWidth(10, 20, 400);
    FLoadedAgeDate:=THelpers.FormatDateTime(PayLoad.AgeDate, TCalendar.DateOnly);
    valCutOffDate.Caption:=FLoadedAgeDate;

    ClearAgingSummary();
    UpdateAgeSummary(PayLoad);
    Service.Logger.Log('[ReadAgeViewAsync_Callback]: Age View summary information updated.');

    ClearOpenItemsSummary();
    UpdateStatusBar(TStatusBar.Downloading);
    LoadOpenItems();

    Service.Mediator.AddressBook.OpenAddressBookAsync('', OpenAddressBook_Callback, LoadedCompanies);
    UpdateFollowUps(sgAgeView);

    SwitchTimers(TurnedOn);
    BusyForm.Close();

end;


procedure TMainForm.ReadOpenItems_Callback(OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse);
begin

    sgOpenItems.Freeze(False);
    UpdateStatusBar(TStatusBar.Ready);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[ReadOpenItemsAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    valOpenItems.Caption  :=FormatFloat('### ###',  OpenItemsData.TotalItems);
    valInvoices.Caption   :=FormatFloat('### ###',  OpenItemsData.NumOfInvoices);
    valOverdue.Caption    :=FormatFloat('### ###',  OpenItemsData.OverdueItems);
    amtOutstanding.Caption:=FormatFloat('#,##0.00', OpenItemsData.OsAmount);
    amtOverdue.Caption    :=FormatFloat('#,##0.00', OpenItemsData.OvdAmount);
    amtUnallocated.Caption:=FormatFloat('#,##0.00', OpenItemsData.UnallocatedAmt);

    sgOpenItems.SetColWidth(10, 20, 400);
    Service.Logger.Log('[ReadOpenItemsAsync_Callback]: Open items have been loaded successfully.');

end;


procedure TMainForm.FreeFieldsUpdate_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[FreeFieldsUpdate_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
    end;

end;


procedure TMainForm.ScanSnapshots_Callback(CanGetAge: boolean; ReceivedTime: string; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        MainForm.UpdateStatusBar(TStatusBar.Ready);
        Service.Logger.Log('[ScanSnapshots_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    if CanGetAge then
    begin
        FDataUpdate:=ReceivedTime;
        valUpdateStamp.Caption:=ReceivedTime;
        if FLoadedCompanies.Count > 0 then LoadAgeReport();
    end
    else
    begin
        Service.Logger.Log('[ScanSnapshots_Callback]: Nothing to update.');
    end;

end;


procedure TMainForm.CheckGivenPassword_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[CheckGivenPassword_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    SetSettingsPanel(False);

    var List: TStringList:=TStringList.Create();
    try

        Service.Settings.GetSections(List);
        sgListSection.RowCount:=List.Count;
        var jCNT: integer:=1;

        for var iCNT: integer:=0 to List.Count - 1 do
        begin

            if List.Strings[iCNT] <> TConfigSections.PasswordSection then
            begin
                sgListSection.Cells[0, jCNT]:=IntToStr(jCNT);
                sgListSection.Cells[1, jCNT]:=List.Strings[iCNT];
                inc(jCNT);
            end;

        end;

    finally
        List.Free();
    end;

    //sgListValue.SetColWidth(25, 30, 400);
    //sgListSection.SetColWidth(25, 30, 400);

    EditPassword.Text:='';

end;


procedure TMainForm.SetNewPassword_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[SetNewPassword_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    THelpers.MsgCall(MainForm.Handle, TAppMessage.Info, 'New password has been saved.');

    btnPassUpdate.Enabled:=False;
    EditCurrentPassword.Enabled:=False;
    EditNewPassword.Enabled:=False;
    EditNewPasswordConfirmation.Enabled:=False;

    EditCurrentPassword.Text:='';
    EditNewPassword.Text:='';
    EditNewPasswordConfirmation.Text:='';

end;


procedure TMainForm.BulkFollowUpUpdate_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[BulkFollowUpUpdate_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    UpdateFollowUps(sgAgeView);

end;


procedure TMainForm.GetAgingReport_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
begin

    UpdateStatusBar(TStatusBar.Ready);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[GetAgingReport_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    UpdateStatusBar(TStatusBar.WritingXLS);
    Service.Mediator.Utilities.WriteToExcelAsync(ReturnedData, FExcelFileName, WriteToExcel_Callback);

end;


procedure TMainForm.WriteToExcel_Callback(CallResponse: TCallResponse);
begin

    UpdateStatusBar(TStatusBar.Ready);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[WriteToExcel_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    if THelpers.MsgCall(
            MainForm.Handle,
            TAppMessage.Question2,
            'Report has been exported and saved successfuly! Do you want to open it?') = IDYES then
    begin

        ShellExecute(
            MainForm.Handle,
            'open',
            PChar(FExcelFileName),
            nil,
            nil,
            SW_SHOWNORMAL
        );

    end;

end;


procedure TMainForm.LoadRating_Callback(Rating: TRating; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[LoadRating_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    if Rating.UserRating = 0 then
    begin
        btnRatingClick(Self);
    end;

end;


{$ENDREGION}


{$REGION 'MESSAGE PROCESSING'}


procedure TMainForm.NotifyMoveOrResizeStarted();
begin
    if (ChromiumWindow <> nil) then ChromiumWindow.NotifyMoveOrResizeStarted();
end;


procedure TMainForm.ChromiumModalLoopOn(PassMsg: TMessage);
begin
    if (PassMsg.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop:=True;
end;


procedure TMainForm.ChromiumModalLoopOff(PassMsg: TMessage);
begin
    if (PassMsg.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop:=False;
end;


procedure TMainForm.WndMessagesChromium(PassMsg: TMessage);
begin

    case PassMsg.Msg of
        WM_MOVE:          NotifyMoveOrResizeStarted;
        WM_MOVING:        NotifyMoveOrResizeStarted;
        WM_ENTERMENULOOP: ChromiumModalLoopOn(PassMsg);
        WM_EXITMENULOOP:  ChromiumModalLoopOff(PassMsg);
    end;

end;


procedure TMainForm.WndMessagesExternal(PassMsg: TMessage);
begin

    if PassMsg.Msg <> THelpers.WM_EXTINFO then
        Exit();

    OutputDebugString(PChar('WM_EXTINFO RECEIVED'));

    if PassMsg.LParam > 0 then
        SkypeCallUpdate(PassMsg.LParam);

end;


procedure TMainForm.WndMessagesWindows(PassMsg: TMessage);
begin

    case PassMsg.Msg of

        // ---------------------------
        // Windows query for shutdown.
        // ---------------------------
        WM_QUERYENDSESSION:
        begin
            Service.Logger.Log('[WndMessagesWindows]: Detected ' + IntToStr(PassMsg.Msg)
                + ' WM_QUERYENDSESSION. Windows is going to be shut down. Closing '
                + TCommon.AppCaption + '...'
            );
            FAllowClose:=True;
            PassMsg.Result:=1;
        end;

        // -------------------------
        // Windows is shutting down.
        // -------------------------
        WM_ENDSESSION:
        begin
            Service.Logger.Log('[WndMessagesWindows]: Detected ' + IntToStr(PassMsg.Msg) + ' WM_ENDSESSION. Windows is shutting down...');
            FAllowClose:=True;
        end;

        // ---------------------------------------------------------
        // Power-management event has occurred (resume or susspend).
        // ---------------------------------------------------------
        WM_POWERBROADCAST:
        case PassMsg.WParam of

            // -------------------------------
            // System is suspending operation.
            // -------------------------------
            PBT_APMSUSPEND:
            begin
                SwitchTimers(TurnedOff);
                Service.Logger.Log('[WndMessagesWindows]: Detected '
                    + IntToStr(PassMsg.Msg) + ' WM_POWERBROADCAST with PBT_APMSUSPEND. Going into suspension mode, Unity is disconnected from server.'
                );
            end;

            // -----------------------------------------------------------
            // Operation is resuming automatically from a low-power state.
            // This message is sent every time the system resumes.
            // -----------------------------------------------------------
            PBT_APMRESUMEAUTOMATIC:
            begin
                SwitchTimers(TurnedOff);
                Service.Logger.Log('[WndMessagesWindows]: Detected '
                    + IntToStr(PassMsg.Msg) + ' WM_POWERBROADCAST with PBT_APMRESUMEAUTOMATIC. Windows has resumed after being suspended.'
                );
            end;

        end;

    end;

end;


procedure TMainForm.WndProc(var Msg: TMessage);
begin
    inherited;
    WndMessagesChromium(Msg);
    WndMessagesExternal(Msg);
    WndMessagesWindows(Msg);
end;


{$ENDREGION}


{$REGION 'STARTUP'}

procedure TMainForm.InitMainWnd();
begin

    FStartTime:=Now();
    FormatDateTime('hh:mm:ss', Now());
    FormatDateTime('hh:mm:ss', Now());

    valStatus.Caption:=TStatusBar.Ready;
    valCurrentDate.Caption:=DateToStr(Now);
    valAadUser.Caption:='Guest';

    Service.Logger.Log('Application version = ' + THelpers.GetBuildInfoAsString);
    Service.Logger.Log('User SID = ' + THelpers.GetCurrentUserSid);

end;


procedure TMainForm.SetupMainWnd();
begin

    if FHadFirstLoad then Exit();

    ChromiumWindow.ChromiumBrowser.OnBeforePopup:=Chromium_OnBeforePopup;
    if not(Chromium.Initialized) then Chromium.CreateBrowser(ChromiumWindow, '');

    for var iCNT:=0 to TabSheets.PageCount - 1 do
        TabSheets.Pages[iCNT].TabVisible:=False;

    TabSheets.ActivePage:=TabSheet9; //Unity Home

    SetPanelBorders();
    SetGridColumnWidths();
    SetGridRowHeights();
    SetButtonsGlyphs();

    TimerUpTime.Enabled:=True;
    TimerCurrentTime.Enabled:=True;

end;


procedure TMainForm.StartMainWnd();
begin

    if not FHadFirstLoad then
    begin

        var NewTask: ITask:=TTask.Create(procedure
        begin

            Sleep(500);

            TThread.Synchronize(nil, procedure
            begin

                var OpenItemsResponse: TCallResponse;

                OpenItemsResponse:=Service.Mediator.OpenItems.GetSSISDataAwaited(FDataUpdate, FDataStatus);
                FDataUpdate:=THelpers.FormatDateTime(FDataUpdate, TCalendar.DateTime);

                if not OpenItemsResponse.IsSucceeded then
                begin
                    THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, OpenItemsResponse.LastMessage);
                    Exit();
                end;

                Service.Logger.Log(
                    '[StartMainWnd]: Open items information loaded (FDataUpdate = ' +
                    FDataUpdate + '; FDataStatus = ' + FDataStatus + ').'
                );

                valUpdateStamp.Caption:=FDataUpdate.Substring(0, Length(FDataUpdate) - 3);
                valCutOffDate.Caption:='n/a';

                selAgeSorting.Clear();

                var SortingOptions:=TStringList.Create();
                try

                    var DebtorsResponse: TCallResponse;
                    DebtorsResponse:=Service.Mediator.Debtors.GetCustSortingOptionsAwaited(SortingOptions);

                    if not DebtorsResponse.IsSucceeded then
                    begin
                        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, DebtorsResponse.LastMessage);
                        Exit();
                    end;

                    selAgeSorting.Items.AddStrings(SortingOptions);

                    if selAgeSorting.Items.Count > 0 then
                    begin
                        selAgeSorting.ItemIndex:=0;
                        Service.Logger.Log('[StartMainWnd]: Sorting options for aging report has been loaded.');
                    end
                    else
                    begin
                        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, 'No sorting options have been found. Please contact IT Support.');
                        Service.Logger.Log('[StartMainWnd]: No sorting options have been found.');
                    end;

                finally
                    SortingOptions.Free();
                end;

                RequestUnityWebWithToken();
                PermitCheckInit();

            end);

        end);

        NewTask.Start();
        FHadFirstLoad:=True;

    end;

end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
    FLoadedCompanies:=TList<string>.Create();
    FIsAppMenuLocked:=True;
    FAllowClose:=False;
    EditUrlSection.Text:='';
    ClearMainViewInfo();
    ClearOpenItemsSummary();
    ClearAgingSummary();
    InitializeScreenSettings;
    SetActiveTabsheet(TabSheet9);
    Tables.ActivePage:=Page1;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
    if StartupForm.IsAppInitialized then SetupMainWnd();
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin

    if MainForm.WindowState = wsMaximized then
        ImageGrip.Visible:=False else ImageGrip.Visible:=True;

    if StartupForm.IsAppInitialized then StartMainWnd();

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


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

    // Execute on "before popup" - ignore listed
    // Optional: WOD_NEW_POPUP, WOD_NEW_WINDOW
    Result:=(
                targetDisposition in
                [
                    WOD_NEW_FOREGROUND_TAB,
                    WOD_NEW_BACKGROUND_TAB
                ]
            );
end;


procedure TMainForm.ChromiumAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin

    if Chromium.IsSameBrowser(browser) then
    begin

        var StrUrl: string:=url;
        EditUrlSection.Text:=StrUrl;

        if StrUrl.Contains('http://') then  imgSSL.Picture.LoadFromFile(Service.Settings.DirAssets + 'Insecure.bmp');
        if StrUrl.Contains('https://') then imgSSL.Picture.LoadFromFile(Service.Settings.DirAssets + 'Secure.bmp');

    end;

end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    // --------------------------------------------------------------------------------------
    // Execute when application receives windows message on shutting down the system;
    // or user press key combination of <ALT> + <Y>; or simply clicks close button on
    // application caption bar. Standard behaviour of application on close button is
    // changed to minimisation of the application to system tray (removes icon from taskbar).
    // --------------------------------------------------------------------------------------
    case FAllowClose of

        False:
        begin
            CanClose:=False;
            ShowWindow(Handle, SW_MINIMIZE);
            Hide();
        end;

        True:
        begin

            SwitchTimers(TAppTimers.TurnedOff);
            ChromiumWindow.CloseBrowser(True);

            var CallResponse: TCallResponse;
            CallResponse:=Service.Mediator.Accounts.SaveUserLogsAwaited();

            if not CallResponse.IsSucceeded then
                THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, CallResponse.LastMessage);

            Service.Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_TOP',  MainForm.Top);
            Service.Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_LEFT', MainForm.Left);
            Service.Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_WIDTH', MainForm.Width);
            Service.Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_HEIGHT', MainForm.Height);

            case MainForm.WindowState of
                wsNormal:    Service.Settings.SetStringValue(TConfigSections.ApplicationDetails, 'WINDOW_STATE', 'wsNormal');
                wsMaximized: Service.Settings.SetStringValue(TConfigSections.ApplicationDetails, 'WINDOW_STATE', 'wsMaximized');
                wsMinimized: Service.Settings.SetStringValue(TConfigSections.ApplicationDetails, 'WINDOW_STATE', 'wsMinimized');
            end;

            Service.Settings.Encode(TAppFiles.Configuration);

            if Assigned(FLoadedCompanies) then
                FLoadedCompanies.Free();

            FAllowClose:=False;
            CanClose:=not FAllowClose;
            StartupForm.Close();

        end;

    end;

end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
    // Do nothing
end;


procedure TMainForm.sgAgeViewColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
    // Do nothing
end;


procedure TMainForm.sgListSectionSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin

    CanSelect:=True;

    var Keys:   TStringList:=TStringList.Create();
    var Values: TStringList:=TStringList.Create();

    try

        Service.Settings.GetSection(sgListSection.Cells[ACol, ARow], Keys);
        Service.Settings.GetSectionValues(sgListSection.Cells[ACol, ARow], Values);
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

            for var iCNT: integer:=1 to Keys.Count do
            begin
                sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);
                sgListValue.Cells[1, iCNT]:=Keys.Strings[iCNT - 1];
                var junk: string:=Keys.Strings[iCNT - 1] + '=';
                var clean: string:=StringReplace(Values.Strings[iCNT - 1], junk, '', [rfReplaceAll]);
                sgListValue.Cells[2, iCNT]:=clean;
            end;

        end;

    finally
        Keys.Free();
        Values.Free();
        //sgListValue.SetColWidth(25, 30, 400);
    end;

end;


{$ENDREGION}


{$REGION 'TABSHEET EVENTS'}


procedure TMainForm.TabSheet4Show(Sender: TObject);
begin
    // Call to InvoiceTracker
end;


procedure TMainForm.TabSheet5Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.TabSheet7Show(Sender: TObject);
begin
    //SetGridColumnWidths();
end;


procedure TMainForm.TabSheet7Resize(Sender: TObject);
begin
    TabSheet7Show(self);
end;


procedure TMainForm.TabSheet8Show(Sender: TObject);
begin
    SetSettingsPanel(True);
end;


procedure TMainForm.Page1Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page2Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page3Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page6Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page7Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page8Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page9Show(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Page10Show(Sender: TObject);
begin
    // Empty
end;


{$ENDREGION}


{$REGION 'VIEW DRAWING EVENTS'}


procedure TMainForm.sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);//(-100) on GetCol
begin

    // Skip header
    if ARow = 0 then Exit;

    // Find column numbers for given column name
    var Col1 :=sgAgeView.GetCol(TCustomerSnapshotEx._NotDue);
    var Col2 :=sgAgeView.GetCol(TCustomerSnapshotEx._Range1);
    var Col3 :=sgAgeView.GetCol(TCustomerSnapshotEx._Range2);
    var Col4 :=sgAgeView.GetCol(TCustomerSnapshotEx._Range3);
    var Col5 :=sgAgeView.GetCol(TCustomerSnapshotEx._Range4);
    var Col6 :=sgAgeView.GetCol(TCustomerSnapshotEx._Range5);
    var Col7 :=sgAgeView.GetCol(TCustomerSnapshotEx._Range6);
    var Col8 :=sgAgeView.GetCol(TCustomerSnapshotEx._Overdue);
    var Col9 :=sgAgeView.GetCol(TCustomerSnapshotEx._Total);
    var Col10:=sgAgeView.GetCol(TCustomerSnapshotEx._CreditLimit);
    var Col11:=sgAgeView.GetCol(TCustomerSnapshotEx._CreditBalance);
    var Col12:=sgAgeView.GetCol(TCustomerSnapshotEx._FollowUp);
    var Col14:=sgAgeView.GetCol(TCustomerSnapshotEx._CustomerName);
    var Col15:=sgAgeView.GetCol('Risk Class');

    // Draw selected row | skip headers
    sgAgeView.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);

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
        if not (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) = 0) then
        begin

            // Future days
            if (ACol = Col12) and (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) > THelpers.CDate(valCurrentDate.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Service.Settings.FutureBColor;
                sgAgeView.Canvas.Font.Color :=Service.Settings.FutureFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

            // Today
            if (ACol = Col12) and (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) = THelpers.CDate(valCurrentDate.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Service.Settings.TodayBColor;
                sgAgeView.Canvas.Font.Color :=Service.Settings.TodayFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

            // Past days
            if (ACol = Col12) and (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) < THelpers.CDate(valCurrentDate.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Service.Settings.PastBColor;
                sgAgeView.Canvas.Font.Color :=Service.Settings.PastFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

        end;

        // Highlight risk class "A"
        if (ACol = Col15) and (sgAgeView.Cells[Col15, ARow] = 'A') then
        begin
            sgAgeView.Canvas.Brush.Color:=sgAgeView.FBackRed;
            sgAgeView.Canvas.Font.Color :=sgAgeView.FFontWhite;
            sgAgeView.Canvas.FillRect(Rect);
            sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
        end;

        // Highlight risk class "B"
        if (ACol = Col15) and (sgAgeView.Cells[Col15, ARow] = 'B') then
        begin
            sgAgeView.Canvas.Brush.Color:=sgAgeView.FBackYellow;
            sgAgeView.Canvas.Font.Color :=sgAgeView.FFontBlack;
            sgAgeView.Canvas.FillRect(Rect);
            sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
        end;

        // Highlight risk class "C"
        if (ACol = Col15) and (sgAgeView.Cells[Col15, ARow] = 'C') then
        begin
            sgAgeView.Canvas.Brush.Color:=sgAgeView.FBackGreen;
            sgAgeView.Canvas.Font.Color :=sgAgeView.FFontWhite;
            sgAgeView.Canvas.FillRect(Rect);
            sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
        end;

    end;

    // After all drawing (when cells are not selected) is done, change font only for numeric values.
    // This shoud always be executed last.
    if (ACol = Col1)  or (ACol = Col2) or (ACol = Col3) or (ACol = Col4)  or (ACol = Col5) or (ACol = Col6) or
       (ACol = Col7)  or (ACol = Col8) or (ACol = Col9) or (ACol = Col10) or (ACol = Col11) then
    begin
        if gdSelected in State then sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;

procedure TMainForm.sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    if ARow = 0 then Exit();

    var Col1: integer:=sgOpenItems.GetCol(TOpenItemsFields._OpenCurAmount);
    var Col2: integer:=sgOpenItems.GetCol(TOpenItemsFields._OpenAmount);
    var Col3: integer:=sgOpenItems.GetCol(TOpenItemsFields._CurAmount);
    var Col4: integer:=sgOpenItems.GetCol(TOpenItemsFields._Amount);
    var Col5: integer:=sgOpenItems.GetCol(TOpenItemsFields._PmtStatus);

    MainForm.sgOpenItems.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);

    if (ACol = Col1) or (ACol = Col2) or (ACol = Col3) or (ACol = Col4) or (ACol = Col5) then
    begin
        if gdSelected in State then sgOpenItems.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else sgOpenItems.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;


procedure TMainForm.sgAddressBookDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgAddressBook.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgInvoiceTrackerDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgInvoiceTracker.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgCoCodesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgCoCodes.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgPaidInfoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgPaidInfo.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgPmtTermsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgPmtTerms.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgListSectionDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgListSection.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgListValueDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgListValue.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgControlStatusDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgControlStatus.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgAccountTypeDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgAccountType.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgPersonRespDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgPersonResp.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgSalesRespDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgSalesResp.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgCustomerGrDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgCustomerGr.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgFSCviewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgFSCview.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgLBUviewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgLBUview.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;

{$ENDREGION}


{$REGION 'TIMER EVENTS'}


procedure TMainForm.TimerFollowUpTimer(Sender: TObject);
begin

    // ------------------------------------------------------------
    // Count current follow-ups and display in notification baloon.
    // ------------------------------------------------------------
    var Sum:=0;
    var ActiveUser:=Service.SessionData.AliasName.ToLower();

    for var iCNT:=1 to sgAgeView.RowCount - 1 do
    begin

        var FollowUpDate:=THelpers.CDate(sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._FollowUp), iCNT]);
        var CurrentDate :=THelpers.CDate(valCurrentDate.Caption);
        var AssignedUser:=sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._PersonResponsible), iCNT].ToLower();

        if (FollowUpDate = CurrentDate) {and (ActiveUser = AssignedUser)} then Inc(Sum);

    end;

    if Sum > 0 then
    begin

        TrayIcon.Visible:=True;
        TrayIcon.BalloonHint:='Hello, you have ' + IntToStr(Sum) + ' follow-up dates registered for today.' + TChars.CRLF +
                              'Let''s bother some customers and collect some money money!' + TChars.CRLF;

        TrayIcon.ShowBalloonHint();

    end;

end;


procedure TMainForm.TimerPermitCheckTimer(Sender: TObject);
begin

    Inc(FPermitCheckTimer);

    if FPermitCheckTimer = (FPermitCheckTimeout / TimerPermitCheck.Interval) then
    begin
        TimerPermitCheck.Enabled:=False;
        THelpers.MsgCall(
            MainForm.Handle,
            TAppMessage.Warn,
            'Azure Active Directory user validation check has timed out. Please try to redeem your access. If you will still encounter issues, please contact IT support.'
        );
    end;

    RedeemAccess();

end;


procedure TMainForm.TimerCustSnapshotsTimer(Sender: TObject);
begin
    Service.Mediator.Debtors.ScanSnapshotsAsync(FDataUpdate, ScanSnapshots_Callback);
end;


procedure TMainForm.TimerCurrentTimeTimer(Sender: TObject);
begin
    valCurrentTime.Caption:=TimeToStr(Now);
end;


procedure TMainForm.TimerUpTimeTimer(Sender: TObject);
begin
    var Result: TTime:=Now - FStartTime;
    valUpTime.Caption:=TimeToStr(Result);
end;


{$ENDREGION}


{$REGION 'POPUP EVENTS'}


procedure TMainForm.PopupCommonMenuPopup(Sender: TObject);
begin

    Action_ExportTransactions.Enabled:=True;
    Action_SelectAll.Enabled         :=True;
    Action_CopyToCB.Enabled          :=True;
    Action_AutoColumn.Enabled        :=True;
    Action_TurnRowHighlight.Enabled  :=True;

    if (sgOpenItems.Focused) and (sgOpenItems.RowCount < 3) then
    begin
        Action_ExportTransactions.Enabled:=False;
        Action_SelectAll.Enabled         :=False;
        Action_CopyToCB.Enabled          :=False;
        Action_AutoColumn.Enabled        :=False;
        Action_TurnRowHighlight.Enabled  :=False;
    end;

end;


procedure TMainForm.PopupLoginPopup(Sender: TObject);
begin

    if TabSheets.ActivePage = TabSheet9 then
    begin

        Action_BrowserMode.Enabled  :=True;
        Action_ClearCoockies.Enabled:=True;
        Action_ClearCache.Enabled   :=True;

        if Action_BrowserMode.Checked then
            Action_ShowNavigation.Enabled:=False
                else Action_ShowNavigation.Enabled:=True;

    end
    else
    begin
        Action_BrowserMode.Enabled   :=False;
        Action_ShowNavigation.Enabled:=False;
        Action_ClearCoockies.Enabled :=False;
        Action_ClearCache.Enabled    :=False;
    end;

end;


procedure TMainForm.PopupTrackerPopup(Sender: TObject);
begin

    Action_ShowRegistered.Enabled:=True;
    Action_ShowMy.Enabled        :=True;
    Action_ShowAll.Enabled       :=True;
    Action_Update.Enabled        :=True;
    Action_Remove.Enabled        :=True;

    if sgInvoiceTracker.RowCount < 3 then
    begin
        Action_ShowRegistered.Enabled:=False;
        Action_ShowMy.Enabled        :=False;
        Action_ShowAll.Enabled       :=False;
        Action_Update.Enabled        :=False;
        Action_Remove.Enabled        :=False;
    end;

end;


procedure TMainForm.PopupBookPopup(Sender: TObject);
begin

    if (sgAddressBook.Selection.Bottom - sgAddressBook.Selection.Top) > 0 then
    begin
        Action_DelRow.Enabled     :=False;
        Action_GoogleIt.Enabled   :=False;
        Action_SearchBook.Enabled :=False;
        Action_ColumnWidth.Enabled:=False;
    end
    else
    begin
        Action_DelRow.Enabled     :=True;
        Action_GoogleIt.Enabled   :=True;
        Action_SearchBook.Enabled :=True;
        Action_ColumnWidth.Enabled:=True;
    end;

end;


procedure TMainForm.PopupAgeViewPopup(Sender: TObject);
begin

    if valTotalCustomers.Caption = '0' then
    begin
        Action_LyncCall.Enabled      :=False;
        Action_InGoogle.Enabled      :=False;
        Action_Tracker.Enabled       :=False;
        Action_AddToBook.Enabled     :=False;
        Action_MassMailer.Enabled    :=False;
        Action_GroupFollowUp.Enabled :=False;
        Action_FilterAgeView.Enabled :=False;
        Action_RemoveFilters.Enabled :=False;
        Action_Overdue.Enabled       :=False;
        Action_Search.Enabled        :=False;
        Action_ViewOptions.Enabled   :=False;
        Action_HideThisColumn.Enabled:=False;
        Action_ShowAllColumns.Enabled:=False;
        Action_AutoColumnSize.Enabled:=False;
    end
    else
    begin

        if sgAgeView.Selection.Bottom - sgAgeView.Selection.Top > 0 then
        begin
            Action_LyncCall.Enabled      :=False;
            Action_InGoogle.Enabled      :=False;
            Action_Tracker.Enabled       :=True;
            Action_AddToBook.Enabled     :=True;
            Action_MassMailer.Enabled    :=True;
            Action_GroupFollowUp.Enabled :=True;
            Action_FilterAgeView.Enabled :=False;
            Action_RemoveFilters.Enabled :=False;
            Action_Overdue.Enabled       :=True;
            Action_Search.Enabled        :=False;
            Action_ViewOptions.Enabled   :=False;
            Action_HideThisColumn.Enabled:=True;
            Action_ShowAllColumns.Enabled:=True;
            Action_AutoColumnSize.Enabled:=True;
        end
        else
        begin

            Action_LyncCall.Enabled      :=True;
            Action_InGoogle.Enabled      :=True;
            Action_Tracker.Enabled       :=True;
            Action_AddToBook.Enabled     :=True;
            Action_MassMailer.Enabled    :=True;
            Action_GroupFollowUp.Enabled :=True;
            Action_FilterAgeView.Enabled :=True;
            Action_Overdue.Enabled       :=True;
            Action_Search.Enabled        :=True;
            Action_ViewOptions.Enabled   :=True;
            Action_HideThisColumn.Enabled:=True;
            Action_ShowAllColumns.Enabled:=True;
            Action_AutoColumnSize.Enabled:=True;

            if FilterForm.InUse then Action_RemoveFilters.Enabled:=True
                else Action_RemoveFilters.Enabled:=False;

        end;

    end;

end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}

procedure TMainForm.Action_ExportTransactionsClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused      then sgOpenItems.ExportCSV(FileCSVExport, '|');
    if sgCoCodes.Focused        then sgCoCodes.ExportCSV(FileCSVExport, '|');
    if sgPaidInfo.Focused       then sgPaidInfo.ExportCSV(FileCSVExport, '|');
    if sgPmtTerms.Focused       then sgPmtTerms.ExportCSV(FileCSVExport, '|');
    if sgListValue.Focused      then sgListValue.ExportCSV(FileCSVExport, '|');
    if sgListSection.Focused    then sgListSection.ExportCSV(FileCSVExport, '|');
    if sgInvoiceTracker.Focused then sgInvoiceTracker.ExportCSV(FileCSVExport, '|');
    if sgPersonResp.Focused     then sgPersonResp.ExportCSV(FileCSVExport, '|');
    if sgSalesResp.Focused      then sgSalesResp.ExportCSV(FileCSVExport, '|');
    if sgAccountType.Focused    then sgAccountType.ExportCSV(FileCSVExport, '|');
    if sgCustomerGr.Focused     then sgCustomerGr.ExportCSV(FileCSVExport, '|');
    if sgControlStatus.Focused  then sgControlStatus.ExportCSV(FileCSVExport, '|');

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.ExportCSV(FileCSVExport, '|');
    if ActionsForm.DailyComGrid.Focused  then ActionsForm.DailyComGrid.ExportCSV(FileCSVExport, '|');

end;


procedure TMainForm.Action_SelectAllClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused      then sgOpenItems.SelectAll();
    if sgCoCodes.Focused        then sgCoCodes.SelectAll();
    if sgPaidInfo.Focused       then sgPaidInfo.SelectAll();
    if sgPmtTerms.Focused       then sgPmtTerms.SelectAll();
    if sgListValue.Focused      then sgListValue.SelectAll();
    if sgListSection.Focused    then sgListSection.SelectAll();
    if sgInvoiceTracker.Focused then sgInvoiceTracker.SelectAll();
    if sgPersonResp.Focused     then sgPersonResp.SelectAll();
    if sgSalesResp.Focused      then sgSalesResp.SelectAll();
    if sgAccountType.Focused    then sgAccountType.SelectAll();
    if sgCustomerGr.Focused     then sgCustomerGr.SelectAll();
    if sgControlStatus.Focused  then sgControlStatus.SelectAll();

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.SelectAll();
    if ActionsForm.DailyComGrid.Focused  then ActionsForm.DailyComGrid.SelectAll();

end;


procedure TMainForm.Action_CopyToCBClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused      then sgOpenItems.CopyCutPaste(TActions.Copy);
    if sgCoCodes.Focused        then sgCoCodes.CopyCutPaste(TActions.Copy);
    if sgPaidInfo.Focused       then sgPaidInfo.CopyCutPaste(TActions.Copy);
    if sgPmtTerms.Focused       then sgPmtTerms.CopyCutPaste(TActions.Copy);
    if sgListValue.Focused      then sgListValue.CopyCutPaste(TActions.Copy);
    if sgListSection.Focused    then sgListSection.CopyCutPaste(TActions.Copy);
    if sgInvoiceTracker.Focused then sgInvoiceTracker.CopyCutPaste(TActions.Copy);
    if sgPersonResp.Focused     then sgPersonResp.CopyCutPaste(TActions.Copy);
    if sgSalesResp.Focused      then sgSalesResp.CopyCutPaste(TActions.Copy);
    if sgAccountType.Focused    then sgAccountType.CopyCutPaste(TActions.Copy);
    if sgCustomerGr.Focused     then sgCustomerGr.CopyCutPaste(TActions.Copy);
    if sgControlStatus.Focused  then sgControlStatus.CopyCutPaste(TActions.Copy);

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.CopyCutPaste(TActions.Copy);
    if ActionsForm.DailyComGrid.Focused  then ActionsForm.DailyComGrid.CopyCutPaste(TActions.Copy);

end;


procedure TMainForm.Action_AutoColumnClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused      then sgOpenItems.SetColWidth(10, 20, 400);
    if sgCoCodes.Focused        then sgCoCodes.SetColWidth(10, 20, 400);
    if sgPaidInfo.Focused       then sgPaidInfo.SetColWidth(10, 20, 400);
    if sgPmtTerms.Focused       then sgPmtTerms.SetColWidth(10, 20, 400);
    //if sgListValue.Focused     then sgListValue.SetColWidth(25, 20, 400);
    //if sgListSection.Focused   then sgListSection.SetColWidth(25, 20, 400);
    if sgInvoiceTracker.Focused then sgInvoiceTracker.SetColWidth(10, 20, 400);
    if sgPersonResp.Focused     then sgPersonResp.SetColWidth(10, 20, 400);
    if sgSalesResp.Focused      then sgSalesResp.SetColWidth(10, 20, 400);
    if sgAccountType.Focused    then sgAccountType.SetColWidth(10, 20, 400);
    if sgCustomerGr.Focused     then sgCustomerGr.SetColWidth(10, 20, 400);
    if sgControlStatus.Focused  then sgControlStatus.SetColWidth(10, 20, 400);

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.SetColWidth(10, 20, 400);
    if ActionsForm.DailyComGrid.Focused  then ActionsForm.DailyComGrid.SetColWidth(10, 20, 400);

end;


procedure TMainForm.Action_TurnRowHighlightClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused      then THelpers.TurnRowHighlight(sgOpenItems, Action_TurnRowHighlight);
    if sgCoCodes.Focused        then THelpers.TurnRowHighlight(sgCoCodes, Action_TurnRowHighlight);
    if sgPaidInfo.Focused       then THelpers.TurnRowHighlight(sgPaidInfo, Action_TurnRowHighlight);
    if sgPmtTerms.Focused       then THelpers.TurnRowHighlight(sgPmtTerms, Action_TurnRowHighlight);
    //if sgListValue.Focused     then THelpers.TurnRowHighlight(sgListValue, Action_TurnRowHighlight);
    //if sgListSection.Focused   then THelpers.TurnRowHighlight(sgListSection, Action_TurnRowHighlight);
    if sgInvoiceTracker.Focused then THelpers.TurnRowHighlight(sgInvoiceTracker, Action_TurnRowHighlight);
    if sgPersonResp.Focused     then THelpers.TurnRowHighlight(sgPersonResp, Action_TurnRowHighlight);
    if sgSalesResp.Focused      then THelpers.TurnRowHighlight(sgSalesResp, Action_TurnRowHighlight);
    if sgAccountType.Focused    then THelpers.TurnRowHighlight(sgAccountType, Action_TurnRowHighlight);
    if sgCustomerGr.Focused     then THelpers.TurnRowHighlight(sgCustomerGr, Action_TurnRowHighlight);
    if sgControlStatus.Focused  then THelpers.TurnRowHighlight(sgControlStatus, Action_TurnRowHighlight);

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then THelpers.TurnRowHighlight(ActionsForm.OpenItemsGrid, Action_TurnRowHighlight);
    if ActionsForm.DailyComGrid.Focused  then THelpers.TurnRowHighlight(ActionsForm.DailyComGrid, Action_TurnRowHighlight);

end;


procedure TMainForm.Action_GoogleItClick(Sender: TObject);
begin

    var CustomerName:=sgAddressBook.Cells[sgAddressBook.GetCol(TAddressBookFields._CustomerName), sgAddressBook.Row];
    var AppParam:='https://google.com/search?q=' + TNetEncoding.URL.Encode(CustomerName);

    ShellExecute(
        MainForm.Handle,
        'open',
        PChar(Service.Settings.DirApplication + TCommon.UnityReader),
        PChar(AppParam),
        nil,
        SW_SHOWNORMAL
    );

end;


procedure TMainForm.Action_CopyClick(Sender: TObject);
begin
    sgAddressBook.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.Action_DelRowClick(Sender: TObject);
begin

    var Msg:='Are you sure you want to delete this customer?' + TChars.CRLF + 'This operation cannot be reverted.';
    if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question2, Msg) = IDNO then Exit();

    var CallResponse: TCallResponse;
    CallResponse:=Service.Mediator.AddressBook.DelFromAddressBookAwaited(
        sgAddressBook.Cells[sgAddressBook.GetCol(TAddressBookFields._Id),
        sgAddressBook.Row].ToInteger()
    );

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Error, 'Cannot delete selected row. Please contact IT support.');
        Service.Logger.Log('[Action_DelRowClick]: Cannot delete selected row.');
        Exit();
    end;

    sgAddressBook.DeleteRowFrom(1, 1);

end;


procedure TMainForm.Action_SearchBookClick(Sender: TObject);
begin
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');
end;


procedure TMainForm.Action_ColumnWidthClick(Sender: TObject);
begin
    sgAddressBook.SetColWidth(10, 20, 400);
end;


procedure TMainForm.Action_ShowAppClick(Sender: TObject);
begin
    ShowWindow(Handle, SW_NORMAL);
    Show();
    Application.BringToFront;
end;


procedure TMainForm.Action_HideAppClick(Sender: TObject);
begin
    ShowWindow(Handle, SW_MINIMIZE);
    Hide();
end;


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


procedure TMainForm.Action_ReportClick(Sender: TObject);
begin
    THelpers.WndCall(FeedbackForm, TWindowState.Modal, MainForm);
end;


procedure TMainForm.Action_HelpClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Action_AboutClick(Sender: TObject);
begin
    THelpers.WndCall(AboutForm, TWindowState.Modal, MainForm);
end;


procedure TMainForm.Action_CloseClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Action_AccountDetailsClick(Sender: TObject);
begin
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');
end;


procedure TMainForm.Action_LoginRedeemClick(Sender: TObject);
begin
    RedeemAccess();
end;


procedure TMainForm.Action_ClearCoockiesClick(Sender: TObject);
begin
    if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question2, 'Do you want to clear stored coockies?') = ID_YES then
        Chromium.DeleteCookies();
end;


procedure TMainForm.Action_ClearCacheClick(Sender: TObject);
begin
    Service.Settings.SetStringValue(TConfigSections.ApplicationDetails, 'CLEAR_CACHE_AT_STARTUP', 'yes');
    Service.Settings.Encode(TAppFiles.Configuration);
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Info, 'The cache will be cleared next time you start the application.');
end;


procedure TMainForm.Action_ShowNavigationClick(Sender: TObject);
begin

    if Action_ShowNavigation.Checked then
    begin
        Action_ShowNavigation.Checked:=not Action_ShowNavigation.Checked;
        PanelNavigation.Visible:=False;
    end
    else
    begin
        Action_ShowNavigation.Checked:=not Action_ShowNavigation.Checked;
        PanelNavigation.Visible:=True;
    end;

end;


procedure TMainForm.Action_BrowserModeClick(Sender: TObject);
begin

    if Action_BrowserMode.Checked then
    begin

        TabSheet9.PopupMenu:=nil;
        Action_BrowserMode.Checked:=not Action_BrowserMode.Checked;

        Action_AccountDetails.Visible:=True;
        Action_LoginRedeem.Visible:=True;
        N25.Visible:=True;

        AppMenu.Width:=AppMenuShown;
        AppHeader.Visible:=True;

    end
    else
    begin

        TabSheet9.PopupMenu:=PopupLogin;
        Action_BrowserMode.Checked:=not Action_BrowserMode.Checked;

        if not Action_ShowNavigation.Checked then
        begin
            Action_ShowNavigation.Checked:=not Action_ShowNavigation.Checked;
            PanelNavigation.Visible:=True;
        end;

        Action_AccountDetails.Visible:=False;
        Action_LoginRedeem.Visible:=False;
        N25.Visible:=False;

        AppMenu.Width:=AppMenuHidden;
        AppHeader.Visible:=False;

    end;

end;


procedure TMainForm.Action_LyncCallClick(Sender: TObject);
begin

    if Service.GetUserPermission(TModules.ActionWindow) = TPermissions.Deny then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    THelpers.WndCall(ActionsForm, TWindowState.Modal, MainForm)

end;


procedure TMainForm.Action_InGoogleClick(Sender: TObject);
begin

    var AppParam:=
        'https://google.com/search?q=' +
        TNetEncoding.URL.Encode(
            sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._CustomerName), sgAgeView.Row]
        );

    ShellExecute(
        ActionsForm.Handle,
        'open',
        PChar(Service.Settings.DirApplication + TCommon.UnityReader),
        PChar(AppParam),
        nil,
        SW_SHOWNORMAL
    );

end;


procedure TMainForm.Action_CopySelectionClick(Sender: TObject);
begin
    sgAgeView.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.Action_TrackerClick(Sender: TObject);
begin
    //THelpers.WndCall(TrackerForm, TWindowState.Modal, MainForm);
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');
end;


procedure TMainForm.Action_ViewOptionsClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Action_AddToBookClick(Sender: TObject);
begin

    if sgAgeView.Selection.Bottom - sgAgeView.Selection.Top > 1 then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Bulk insertion is not supported in this version.');
        Exit();
    end;

    var CustDetails: TCustomerDetails;

    CustDetails.SourceDBName   :=sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._SourceDbName), sgAgeView.Row];
    CustDetails.CustomerNumber :=sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._CustomerNumber), sgAgeView.Row].ToInteger();
    CustDetails.CustomerName   :=sgAgeView.Cells[sgAgeView.GetCol(TCustomerSnapshotEx._CustomerName), sgAgeView.Row];
    CustDetails.ContactPerson  :=' ';
    CustDetails.RegularEmails  :=' ';
    CustDetails.StatementEmails:=' ';
    CustDetails.PhoneNumbers   :=' ';

    Service.Mediator.AddressBook.AddToAddressBookAsync(CustDetails, AddToAddressBook_Callback);

end;


procedure TMainForm.Action_MassMailerClick(Sender: TObject);
begin

    if (sgAgeView.Selection.Top - sgAgeView.Selection.Bottom) = 0 then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Please select more than one customer.');
        Exit();
    end;

    if not FIsAddressBookOpened then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Please make sure Address Book is currently opened.');
        Exit();
    end;

    THelpers.WndCall(MassMailerForm, TWindowState.Modal, MainForm);

end;


procedure TMainForm.Action_AddFollowUpGroupClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=TCalendar.GetDate;
    THelpers.WndCall(CalendarForm, TWindowState.Modal, MainForm);
    FollowUpsUpdate(sgAgeView, DateToStr(CalendarForm.FSelectedDate));
end;


procedure TMainForm.Action_RemoveFollowUpsClick(Sender: TObject);
begin
    FollowUpsUpdate(sgAgeView, ' ');
end;


// Filter via INF4 (to be removed)
procedure TMainForm.Action_INF4_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._Inf4;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.Inf4;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via Sales Responsible
procedure TMainForm.Action_SalesRespClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._SalesResponsible;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.SalesResponsible;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter vi Person Responsible
procedure TMainForm.Action_PersonRespClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._PersonResponsible;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.PersonResponsible;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via Customer Group
procedure TMainForm.Action_CustomerGrpClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._CustomerGroup;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.CustomerGroup;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via Account Type
procedure TMainForm.Action_AccountTypeClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._AccountType;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.AccountType;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via FOLLOW UP
procedure TMainForm.Action_FollowUp_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._FollowUp;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.Follow;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via COCODE
procedure TMainForm.Action_CoCode_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._SourceDbName;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.CoCode;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via FREE 1
procedure TMainForm.Action_Free1Click(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._Free1;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.Free1;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via FREE 2
procedure TMainForm.Action_Free2Click(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._Free2;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.Free2;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via FREE 3
procedure TMainForm.Action_Free3Click(Sender: TObject);
begin
    FilterForm.FColName  :=TCustomerSnapshotEx._Free3;
    FilterForm.FOverdue  :=TCustomerSnapshotEx._Overdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TColumns.Free3;
    THelpers.WndCall(FilterForm, TWindowState.Modal, MainForm);
end;


// Filter via REMOVE ALL FILTERS
procedure TMainForm.Action_RemoveFiltersClick(Sender: TObject);
begin

    sgAgeView.Freeze(True);

    for var Index:=1 to sgAgeView.RowCount - 1 do
        sgAgeView.RowHeights[Index]:=sgAgeView.sgRowHeight;

    FilterForm.FilterClearAll();
    sgAgeView.Freeze(False);

end;


procedure TMainForm.Action_OverdueClick(Sender: TObject);
begin
    if Action_Overdue.Checked then
        Action_Overdue.Checked:=False
    else
        Action_Overdue.Checked:=True;
end;


procedure TMainForm.Action_SearchClick(Sender: TObject);
begin
    GridSearchForm.Grid     :=MainForm.sgAgeView;
    GridSearchForm.ColName  :=TCustomerSnapshotEx._CustomerName;
    GridSearchForm.ColNumber:=TCustomerSnapshotEx._CustomerNumber;
    THelpers.WndCall(GridSearchForm, TWindowState.Modeless, MainForm);
end;


procedure TMainForm.Action_ToExceClick(Sender: TObject);
begin

    if MainForm.FileXLExport.Execute(MainForm.Handle) then
    begin
        FExcelFileName:=MainForm.FileXLExport.FileName;
        UpdateStatusBar(TStatusBar.MakeReport);
        Service.Mediator.Debtors.GetAgingReportAsync(FLoadedCompanies, GetAgingReport_Callback);
    end;

end;


procedure TMainForm.Action_ExportCSVClick(Sender: TObject);
begin
    sgAgeView.ExportCSV(FileCSVExport, '|');
end;


procedure TMainForm.Action_HideSummaryClick(Sender: TObject);
begin

    if Action_HideSummary.Checked then
    begin
        DebtorsPanel.Margins.Bottom:=12;
        DebtorsFooter.Visible:=False;
        Action_HideSummary.Checked:=False;
    end
    else
    begin
        DebtorsPanel.Margins.Bottom:=0;
        DebtorsFooter.Visible:=True;
        Action_HideSummary.Checked:=True;
    end;

end;


procedure TMainForm.Action_HideThisColumnClick(Sender: TObject);
begin
    sgAgeView.HideThisColumns();
end;


procedure TMainForm.Action_ShowAllColumnsClick(Sender: TObject);
begin
    sgAgeView.ShowAllColumns();
end;


procedure TMainForm.Action_AutoColumnSizeClick(Sender: TObject);
begin
    MainForm.sgAgeView.SetColWidth(10, 20, 400);
end;


procedure TMainForm.Action_FollowUpColorsClick(Sender: TObject);
begin
    THelpers.WndCall(ColorsForm, TWindowState.Modal, MainForm);
end;


procedure TMainForm.Action_RowHighlightClick(Sender: TObject);
begin
    THelpers.TurnRowHighlight(sgAgeView, Action_RowHighlight);
end;


procedure TMainForm.Action_RemoveClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.Action_ShowRegisteredClick(Sender: TObject);
begin
    // Call to Invoice Tracker
end;


procedure TMainForm.Action_ShowMyClick(Sender: TObject);
begin
    // Call to Invoice Tracker
end;


procedure TMainForm.Action_ShowAllClick(Sender: TObject);
begin
    // Call to Invoice Tracker
end;


procedure TMainForm.TrayIconClick(Sender: TObject);
begin
    // ------------------------------------------------------------
    // Show the application form when user double click application
    // icon visible on system tray.
    // ------------------------------------------------------------
    if not View.Startup.StartupForm.IsAppInitialized then
        TrayIcon.PopupMenu:=nil
    else
        TrayIcon.PopupMenu:=PopupMenu;
end;


procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
    if not View.Startup.StartupForm.IsAppInitialized then Exit();
    MainForm.Action_ShowAppClick(self);
end;


procedure TMainForm.sgAgeViewClick(Sender: TObject);
begin
    sgAgeView.Options:=sgAgeView.Options - [goEditing];
    sgAgeView.Options:=sgAgeView.Options - [goAlwaysShowEditor];
    sgAgeView.SetFocus();
    sgAgeView.EditorMode:=False;
end;


procedure TMainForm.sgAgeViewDblClick(Sender: TObject);
begin
    if valTotalCustomers.Caption <> '0' then Action_LyncCallClick(Self);
end;


procedure TMainForm.sgAddressBookDblClick(Sender: TObject);
begin
    Action_GoogleItClick(Self);
end;


procedure TMainForm.sgAddressBookClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.sgInvoiceTrackerDblClick(Sender: TObject);
begin
    // Call to Invoice Tracker
end;


procedure TMainForm.sgLBUviewClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.sgFSCviewClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.sgListSectionClick(Sender: TObject);
begin
    imgSectionAdd.Enabled   :=True;
    imgSectionRemove.Enabled:=True;
    imgKeyAdd.Enabled       :=False;
    imgKeyRemove.Enabled    :=False;
    imgUpdateValues.Enabled :=False;
    txtKeyAdd.Enabled       :=False;
    txtKeyRemove.Enabled    :=False;
    txtUpdateValues.Enabled :=False;
    txtSectionAdd.Enabled   :=True;
    txtSectionRemove.Enabled:=True;
end;


procedure TMainForm.sgListValueClick(Sender: TObject);
begin
    imgSectionAdd.Enabled   :=False;
    imgSectionRemove.Enabled:=False;
    imgKeyAdd.Enabled       :=True;
    imgKeyRemove.Enabled    :=True;
    imgUpdateValues.Enabled :=True;
    txtKeyAdd.Enabled       :=True;
    txtKeyRemove.Enabled    :=True;
    txtUpdateValues.Enabled :=True;
    txtSectionAdd.Enabled   :=False;
    txtSectionRemove.Enabled:=False;
end;


procedure TMainForm.btnPasswordPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EditPassword.PasswordChar:=#0;
end;


procedure TMainForm.btnPasswordPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EditPassword.PasswordChar:='*';
end;


procedure TMainForm.btnNavBackClick(Sender: TObject);
begin
    Chromium.GoBack();
end;


procedure TMainForm.btnNavBackMouseEnter(Sender: TObject);
begin
    txtNavBack.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnNavBackMouseLeave(Sender: TObject);
begin
    txtNavBack.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnNavForwardClick(Sender: TObject);
begin
    Chromium.GoForward();
end;


procedure TMainForm.btnNavForwardMouseEnter(Sender: TObject);
begin
    txtNavForward.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnNavForwardMouseLeave(Sender: TObject);
begin
    txtNavForward.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnNavReloadClick(Sender: TObject);
begin

    var Url: string:=EditUrlSection.Text;
    if (String.IsNullOrEmpty(Url)) or (String.IsNullOrWhiteSpace(Url)) then Exit();

    if Url.Contains(' ') then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'The URL address cannot contain whitespaces. Please corrected and try again.');
        Exit();
    end;

    Chromium.LoadURL(Url);

end;


procedure TMainForm.btnNavReloadMouseEnter(Sender: TObject);
begin
    txtNavReload.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnNavReloadMouseLeave(Sender: TObject);
begin
    txtNavReload.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnNavHomeClick(Sender: TObject);
begin
    RequestUnityWebWithToken();
end;


procedure TMainForm.btnNavHomeMouseEnter(Sender: TObject);
begin
    txtNavHome.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnNavHomeMouseLeave(Sender: TObject);
begin
    txtNavHome.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.imgAppMenuClick(Sender: TObject);
begin
    if AppMenu.Width = AppMenuShown then AppMenu.Width:=AppMenuHidden else AppMenu.Width:=AppMenuShown;
end;


procedure TMainForm.txtStartClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet9);
end;


procedure TMainForm.txtReportsClick(Sender: TObject);
begin

    if Service.GetUserPermission(TModules.Reporting) <> TPermissions.Read then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    if not CanAccessAppMenu then Exit();
    ReportsForm.FSetLastSelection:=TabSheets.ActivePage;
    ResetTabsheetButtons;
    txtReports.Font.Style:=[fsBold];
    txtReports.Font.Color:=$006433C9;
    THelpers.WndCall(ReportsForm, TWindowState.Modal, MainForm);

end;


procedure TMainForm.txtDebtorsClick(Sender: TObject);
begin
    if not CanAccessAppMenu then Exit();
    SetActiveTabsheet(TabSheet1);
end;


procedure TMainForm.txtTrackerClick(Sender: TObject);
begin

    if not CanAccessAppMenu then Exit();

    if Service.GetUserPermission(TModules.InvoiceTracker) = TPermissions.Deny then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    //SetActiveTabsheet(TabSheet4);
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');

end;


procedure TMainForm.txtAddressBookClick(Sender: TObject);
begin

    if not CanAccessAppMenu then Exit();

    if Service.GetUserPermission(TModules.AddressBook) = TPermissions.Deny then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    SetActiveTabsheet(TabSheet3);

end;


procedure TMainForm.txtOpenItemsClick(Sender: TObject);
begin

    if Service.GetUserPermission(TModules.OpenItems) = TPermissions.Deny then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    if not CanAccessAppMenu then Exit();
    SetActiveTabsheet(TabSheet2);

end;


procedure TMainForm.txtUnidentifiedClick(Sender: TObject);
begin
    if not CanAccessAppMenu then Exit();
    //SetActiveTabsheet(TabSheet6);
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');
end;


procedure TMainForm.txtQueriesClick(Sender: TObject);
begin
    if not CanAccessAppMenu then Exit();
    //SetActiveTabsheet(TabSheet5);
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');
end;


procedure TMainForm.txtTablesClick(Sender: TObject);
begin
    if not CanAccessAppMenu then Exit();
    SetActiveTabsheet(TabSheet7);
end;


procedure TMainForm.txtSettingsClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet8);
end;


procedure TMainForm.btnRatingClick(Sender: TObject);
begin
    RateForm.FSetLastSelection:=TabSheets.ActivePage;
    ResetTabsheetButtons;
    txtRating.Font.Style:=[fsBold];
    txtRating.Font.Color:=AppButtonTxtSelected;
    THelpers.WndCall(RateForm, TWindowState.Modal, MainForm);
end;


procedure TMainForm.txtFeedbackClick(Sender: TObject);
begin
    FeedbackForm.FSetLastSelection:=TabSheets.ActivePage;
    ResetTabsheetButtons;
    txtFeedback.Font.Style:=[fsBold];
    txtFeedback.Font.Color:=AppButtonTxtSelected;
    THelpers.WndCall(FeedbackForm, TWindowState.Modal, MainForm);
end;


procedure TMainForm.txtInfoClick(Sender: TObject);
begin
    AboutForm.FSetLastSelection:=TabSheets.ActivePage;
    ResetTabsheetButtons;
    txtInfo.Font.Style:=[fsBold];
    txtInfo.Font.Color:=AppButtonTxtSelected;
    THelpers.WndCall(AboutForm, TWindowState.Modal, MainForm);
end;


procedure TMainForm.btnReloadClick(Sender: TObject);
begin
    LoadOpenItems();
end;


procedure TMainForm.imgGetAgingReportClick(Sender: TObject);
begin
    THelpers.WndCall(CompanyListForm, Modal, MainForm);
end;


procedure TMainForm.imgRefreshReportClick(Sender: TObject);
begin

    if FLoadedCompanies.Count = 0 then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'No aging report has been uploaded.');
        Exit();
    end;

    LoadAgeReport();

end;


procedure TMainForm.btnOpenAbClick(Sender: TObject);
begin
    BusyForm.Show();
    UpdateStatusBar(TStatusBar.Processing);
    Service.Mediator.AddressBook.OpenAddressBookAsync(String.Empty, OpenAddressBook_Callback, LoadedCompanies);
end;


procedure TMainForm.btnCloseAbClick(Sender: TObject);
begin

    if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question2, 'Are you sure you want to close Address Book?') = IDYES then
    begin
        sgAddressBook.ClearAll(2, 1, 1, True);
        FIsAddressBookOpened:=False;
    end;

end;


procedure TMainForm.btnSearchAbClick(Sender: TObject);
begin
    //THelpers.WndCall(SqlSearchForm, TWindowState.Modeless, MainForm);
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is disabled in this version.');
end;


procedure TMainForm.btnExportAbClick(Sender: TObject);
begin

    if not(sgAddressBook.Visible) then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Please open Address Book first.');
        Exit();
    end;

    sgAddressBook.ExportCSV(MainForm.FileCSVExport, '|');

end;


procedure TMainForm.btnFscApproveClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.btnFscRejectClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.btnLbuUpdateClick(Sender: TObject);
begin
    // Empty
end;


procedure TMainForm.imgSectionAddClick(Sender: TObject);
begin

    // Add row at the end of the list
    sgListSection.RowCount:=sgListSection.RowCount + 1;
    sgListSection.Cells[1, sgListSection.RowCount]:= '';

    for var iCNT: integer:= 1 to sgListSection.RowCount do
        sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);

end;


procedure TMainForm.imgSectionRemoveClick(Sender: TObject);
begin

    if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question2, 'Are you sure you want to delete this section? It cannot be undone.') = IDNO then Exit();
    if sgListSection.RowCount = 1 then Exit();

    // Remove given section
    Service.Settings.DeleteSection(sgListSection.Cells[1, sgListSection.Row]);
    Service.Settings.Encode(TAppFiles.Configuration);

    // Remove from string grid
    sgListSection.DeleteRowFrom(1, 1);

    // Re-number
    for var iCNT: integer:= 1 to sgListSection.RowCount do
        sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);

end;


procedure TMainForm.imgAllowEditClick(Sender: TObject);
begin

    if FAllowEditConfig then
    begin
        FAllowEditConfig:=not FAllowEditConfig;
        sgListSection.Options  :=sgListSection.Options - [goEditing];
        sgListValue.Options    :=sgListValue.Options   - [goEditing];
        txtAllowEdit.Font.Style:=[];
    end
    else
    begin
        FAllowEditConfig:=not FAllowEditConfig;
        sgListSection.Options  :=sgListSection.Options + [goEditing];
        sgListValue.Options    :=sgListValue.Options   + [goEditing];
        txtAllowEdit.Font.Style:=[fsBold];
    end;

end;


procedure TMainForm.imgEventLogClick(Sender: TObject);
begin
    THelpers.WndCall(EventForm, TWindowState.Modeless, MainForm);
end;


procedure TMainForm.imgKeyAddClick(Sender: TObject);
begin

    // Add row at the end of the list
    var iCNT: integer:=sgListValue.RowCount + 1;
    sgListValue.RowCount:=iCNT;

    // Make sure we add empty row
    sgListValue.Cells[1, iCNT - 1]:='';
    sgListValue.Cells[2, iCNT - 1]:='';

    for iCNT:= 1 to sgListValue.RowCount do
        sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;


procedure TMainForm.imgKeyRemoveClick(Sender: TObject);
begin

    if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question2, 'Are you sure you want to delete this key? It cannot be undone.') = IDNO then Exit();

    // Check for last row
    if sgListValue.RowCount = 1 then Exit();

    // Remove key
    Service.Settings.DeleteKey(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, sgListValue.Row]);
    Service.Settings.Encode(TAppFiles.Configuration);

    // Remove from string grid list
    sgListValue.DeleteRowFrom(1, 1);

    // Re-number
    for var iCNT: integer:= 1 to sgListValue.RowCount do
        sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;


procedure TMainForm.imgUpdateValuesClick(Sender: TObject);
begin

    if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question2, 'Are you sure you want to save all the changes? It cannot be undone.') = IDNO then exit;

    // Check if there is no empty keys
    for var iCNT: integer:= 1 to (sgListValue.RowCount - 1) do
    begin
        if string.IsNullOrEmpty(sgListValue.Cells[1, iCNT]) then
        begin
            THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Cannot save. At least one key has no label.');
            Exit();
        end;
    end;

    // Save to settings file all the keys and values
    for var iCNT: integer:= 1 to (sgListValue.RowCount - 1) do
        Service.Settings.SetStringValue(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, iCNT], sgListValue.Cells[2, iCNT]);

    Service.Settings.Encode(TAppFiles.Configuration);

    THelpers.MsgCall(MainForm.Handle, TAppMessage.Info, 'All Keys and its values has been saved successfully.');

end;


procedure TMainForm.btnPassUpdateClick(Sender: TObject);
begin

    if (not(string.IsNullOrEmpty(EditCurrentPassword.Text))) and (not(string.IsNullOrEmpty(EditNewPassword.Text)))
        and (not(string.IsNullOrEmpty(EditNewPasswordConfirmation.Text)))
    then
    begin

        if EditNewPassword.Text <> EditNewPasswordConfirmation.Text then
        begin
            THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'New password and its confirmation does not match, please re-type it and try again.');
            Exit();
        end;

        Service.Mediator.Utilities.SetNewPasswordAsync(EditCurrentPassword.Text, EditNewPassword.Text, SetNewPassword_Callback);

    end
    else
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Please provide with current password, new password and its confirmation.');
    end;

end;


procedure TMainForm.btnUnlockClick(Sender: TObject);
begin

    if btnUnlock.Caption = 'Lock' then
    begin
        SetSettingsPanel(True);
        Exit();
    end;

    if String.IsNullOrEmpty(EditPassword.Text) then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'Please provide with password.');
        Exit();
    end
    else
    begin
        Service.Mediator.Utilities.CheckGivenPasswordAsync(EditPassword.Text, CheckGivenPassword_Callback);
    end;

end;


{$ENDREGION}


{$REGION 'MOUSE MOVE EVENTS'}


procedure TMainForm.btnStartMouseEnter(Sender: TObject);
begin
    txtStart.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnStartMouseLeave(Sender: TObject);
begin
    if txtStart.Font.Style <> [fsBold] then
        txtStart.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnReportsMouseEnter(Sender: TObject);
begin
    txtReports.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnReportsMouseLeave(Sender: TObject);
begin
    if txtReports.Font.Style <> [fsBold] then
        txtReports.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnDebtorsMouseEnter(Sender: TObject);
begin
    txtDebtors.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnDebtorsMouseLeave(Sender: TObject);
begin
    if txtDebtors.Font.Style <> [fsBold] then
        txtDebtors.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnTrackerMouseEnter(Sender: TObject);
begin
    txtTracker.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnTrackerMouseLeave(Sender: TObject);
begin
    if txtTracker.Font.Style <> [fsBold] then
        txtTracker.Font.Color:=AppMenuTextNormal;
end;

procedure TMainForm.btnAddressBookMouseEnter(Sender: TObject);
begin
    txtAddressBook.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnAddressBookMouseLeave(Sender: TObject);
begin
    if txtAddressBook.Font.Style <> [fsBold] then
        txtAddressBook.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnOpenItemsMouseEnter(Sender: TObject);
begin
    txtOpenItems.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnOpenItemsMouseLeave(Sender: TObject);
begin
    if txtOpenItems.Font.Style <> [fsBold] then
        txtOpenItems.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnUnidentifiedMouseEnter(Sender: TObject);
begin
    txtUnidentified.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnUnidentifiedMouseLeave(Sender: TObject);
begin
    if txtUnidentified.Font.Style <> [fsBold] then
        txtUnidentified.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnQueriesMouseEnter(Sender: TObject);
begin
    txtQueries.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnQueriesMouseLeave(Sender: TObject);
begin
    if txtQueries.Font.Style <> [fsBold] then
        txtQueries.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnTablesMouseEnter(Sender: TObject);
begin
    txtTables.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnTablesMouseLeave(Sender: TObject);
begin
    if txtTables.Font.Style <> [fsBold] then
        txtTables.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnSettingsMouseEnter(Sender: TObject);
begin
    txtSettings.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnSettingsMouseLeave(Sender: TObject);
begin
    if txtSettings.Font.Style <> [fsBold] then
        txtSettings.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnRatingMouseEnter(Sender: TObject);
begin
    txtRating.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnRatingMouseLeave(Sender: TObject);
begin
    if txtRating.Font.Style <> [fsBold] then
        txtRating.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnFeedbackMouseEnter(Sender: TObject);
begin
    txtFeedback.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnFeedbackMouseLeave(Sender: TObject);
begin
    if txtFeedback.Font.Style <> [fsBold] then
        txtFeedback.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.btnInfoMouseEnter(Sender: TObject);
begin
    txtInfo.Font.Color:=AppMenuTextSelected;
end;


procedure TMainForm.btnInfoMouseLeave(Sender: TObject);
begin
    if txtInfo.Font.Style <> [fsBold] then
        txtInfo.Font.Color:=AppMenuTextNormal;
end;


procedure TMainForm.sgAgeViewMouseEnter(Sender: TObject);
begin
    //if (sgAgeView.Enabled) and (sgAgeView.Visible) then sgAgeView.SetFocus();
end;


procedure TMainForm.sgOpenItemsMouseEnter(Sender: TObject);
begin
    //if (sgOpenItems.Enabled) and (sgOpenItems.Visible) then sgOpenItems.SetFocus();
end;


procedure TMainForm.sgAddressBookMouseEnter(Sender: TObject);
begin
    //if (sgAddressBook.Enabled) and (sgAddressBook.Visible) then sgAddressBook.SetFocus();
end;


procedure TMainForm.sgInvoiceTrackerMouseEnter(Sender: TObject);
begin
    //if (sgInvoiceTracker.Enabled) and (sgInvoiceTracker.Visible) then sgInvoiceTracker.SetFocus();
end;


procedure TMainForm.sgCoCodesMouseEnter(Sender: TObject);
begin
    //if (sgCoCodes.Enabled) and (sgCoCodes.Visible) then sgCoCodes.SetFocus();
end;


procedure TMainForm.sgPaidInfoMouseEnter(Sender: TObject);
begin
    //if (sgPaidInfo.Enabled) and (sgPaidInfo.Visible) then sgPaidInfo.SetFocus();
end;


procedure TMainForm.sgPmtTermsMouseEnter(Sender: TObject);
begin
    //if (sgPmtTerms.Enabled) and (sgPmtTerms.Visible) then sgPmtTerms.SetFocus();
end;


procedure TMainForm.sgControlStatusMouseEnter(Sender: TObject);
begin
    //if (sgControlStatus.Enabled) and (sgControlStatus.Visible) then sgControlStatus.SetFocus();
end;


procedure TMainForm.sgPersonRespMouseEnter(Sender: TObject);
begin
    //if (sgPersonResp.Enabled) and (sgPersonResp.Visible) then sgPersonResp.SetFocus();
end;


procedure TMainForm.sgSalesRespMouseEnter(Sender: TObject);
begin
    //if (sgSalesResp.Enabled) and (sgSalesResp.Visible) then sgSalesResp.SetFocus();
end;


procedure TMainForm.sgAccountTypeMouseEnter(Sender: TObject);
begin
    //if (sgAccountType.Enabled) and (sgAccountType.Visible) then sgAccountType.SetFocus();
end;


procedure TMainForm.sgCustomerGrMouseEnter(Sender: TObject);
begin
    //if (sgCustomerGr.Enabled) and (sgCustomerGr.Visible) then sgCustomerGr.SetFocus();
end;


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
procedure TMainForm.sgOpenItemsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgOpenItems.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;


// OPEN ITEMS | WHEEL UP
procedure TMainForm.sgOpenItemsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
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
procedure TMainForm.sgAddressBookMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
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
procedure TMainForm.sgListSectionMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgListSection.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;


// VALUE LIST | WHEEL DOWN
procedure TMainForm.sgListValueMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    sgListValue.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;


// VALUE LIST  | WHEEL UP
procedure TMainForm.sgListValueMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
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


procedure TMainForm.imgLockClick(Sender: TObject);
begin
    THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'This feature is locked by the administrator.');
end;


procedure TMainForm.imgGetAgingReportMouseEnter(Sender: TObject);
begin
    txtGetAgingReport.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgGetAgingReportMouseLeave(Sender: TObject);
begin
    txtGetAgingReport.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgRefreshReportMouseEnter(Sender: TObject);
begin
    txtRefreshReport.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgRefreshReportMouseLeave(Sender: TObject);
begin
    txtRefreshReport.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.btnReloadMouseEnter(Sender: TObject);
begin
    txtReloadBtnA.Font.Color:=AppButtonTxtSelected;
    txtReloadBtnB.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.btnReloadMouseLeave(Sender: TObject);
begin
    txtReloadBtnA.Font.Color:=AppButtonTxtNormal;
    txtReloadBtnB.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.btnSearchAbMouseEnter(Sender: TObject);
begin
    txtSearchAb.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.btnSearchAbMouseLeave(Sender: TObject);
begin
    txtSearchAb.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.btnOpenAbMouseEnter(Sender: TObject);
begin
    txtOpenAb.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.btnOpenAbMouseLeave(Sender: TObject);
begin
    txtOpenAb.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.btnCloseAbMouseEnter(Sender: TObject);
begin
    txtCloseAb.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.btnCloseAbMouseLeave(Sender: TObject);
begin
    txtCloseAb.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.btnExportAbMouseEnter(Sender: TObject);
begin
    txtExportAb.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.btnExportAbMouseLeave(Sender: TObject);
begin
    txtExportAb.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgKeyAddMouseEnter(Sender: TObject);
begin
    txtKeyAdd.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgKeyAddMouseLeave(Sender: TObject);
begin
    txtKeyAdd.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgKeyRemoveMouseEnter(Sender: TObject);
begin
    txtKeyRemove.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgKeyRemoveMouseLeave(Sender: TObject);
begin
    txtKeyRemove.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgUpdateValuesMouseEnter(Sender: TObject);
begin
    txtUpdateValues.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgUpdateValuesMouseLeave(Sender: TObject);
begin
    txtUpdateValues.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgSectionAddMouseEnter(Sender: TObject);
begin
    txtSectionAdd.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgSectionAddMouseLeave(Sender: TObject);
begin
    txtSectionAdd.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgSectionRemoveMouseEnter(Sender: TObject);
begin
    txtSectionRemove.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgSectionRemoveMouseLeave(Sender: TObject);
begin
    txtSectionRemove.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgAllowEditMouseEnter(Sender: TObject);
begin
    txtAllowEdit.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgAllowEditMouseLeave(Sender: TObject);
begin
    txtAllowEdit.Font.Color:=AppButtonTxtNormal;
end;


procedure TMainForm.imgEventLogMouseEnter(Sender: TObject);
begin
    txtEventLog.Font.Color:=AppButtonTxtSelected;
end;


procedure TMainForm.imgEventLogMouseLeave(Sender: TObject);
begin
    txtEventLog.Font.Color:=AppButtonTxtNormal;
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // Turn off standard <ALT> + <F4>.
    if (Key=VK_F4) and (Shift=[ssALT]) then Key:=0;

    // Bind close application with <ALT> + <Y>.
    if (Key=89) and (Shift=[ssALT]) then
    begin

        if THelpers.MsgCall(MainForm.Handle, TAppMessage.Question1, 'Are you sure you want to exit the Unity?') = IDOK then
        begin
            FAllowClose:=True;
            MainForm.Close();
        end;

    end;

end;


procedure TMainForm.EditGroupNameKeyPress(Sender: TObject; var Key: Char);
begin
    if (not (CharInSet(Key, ['A'..'Z', 'a'..'z', '0'..'9', '-', TChars.BACKSPACE]))) then
        Key:=#0;
end;


procedure TMainForm.sgOpenItemsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgOpenItems.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgInvoiceTrackerKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgInvoiceTracker.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgCoCodesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgCoCodes.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgPaidInfoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPaidInfo.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgPmtTermsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPmtTerms.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgControlStatusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgControlStatus.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgPersonRespKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPersonResp.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgSalesRespKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgSalesResp.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgAccountTypeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgAccountType.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgCustomerGrKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgCustomerGr.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // <CTRL> + <ALT> + <W>
    if (Key = 87) and (Shift = [ssAlt]) and (Shift = [ssCtrl]) then
    begin
        MainForm.sgAgeView.SetColWidth(10, 20, 400);
        Exit();
    end;

    // <CTRL> + <C>
    if (Key = 67) and (Shift = [ssCtrl]) then
    begin
        sgAgeView.CopyCutPaste(TActions.Copy);
        Exit();
    end;

    // <CTRL> + <A>
    if (Key = 65) and (Shift = [ssCtrl]) then
    begin
        sgAgeView.SelectAll();
        sgAgeView.CopyCutPaste(TActions.Copy);
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Info, 'The selected spreadsheet has been copied to clipboard.');
        Exit();
    end;

    if (sgAgeView.Col <> sgAgeView.GetCol(TCustomerSnapshotEx._Free1))
        and (sgAgeView.Col <> sgAgeView.GetCol(TCustomerSnapshotEx._Free2))
            and (sgAgeView.Col <> sgAgeView.GetCol(TCustomerSnapshotEx._Free3))
                then Exit();

    if (Service.GetUserPermission(TModules.Free1Field) <> TPermissions.ReadWrite) or
       (Service.GetUserPermission(TModules.Free2Field) <> TPermissions.ReadWrite) or
       (Service.GetUserPermission(TModules.Free3Field) <> TPermissions.ReadWrite) then
    begin
        THelpers.MsgCall(MainForm.Handle, TAppMessage.Warn, 'You do not have permission to edit this field.');
        Exit();
    end;

    if (sgAgeView.EditorMode) and ( (Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN) ) then
    begin
        Key:=0;
        sgAgeView.QuitEditing();
        Exit();
    end;

    if Key = VK_ESCAPE then
    begin
        Key:=0;
        sgAgeView.QuitEditing();
        Exit();
    end;

    // <CTRL> + <V>
    if (Key = 86) and (Shift = [ssCtrl]) then
    begin
        sgAgeView.CopyCutPaste(TActions.Paste, True);
        Service.Mediator.Comments.FreeFieldsUpdateAsync(UpdateFreeFields(sgAgeView), FreeFieldsUpdate_Callback);
        Exit();
    end;

    if CharInSet(Char(Key), [#48..#57{A..Z}, #65..#90{a..z}, #97..#122{0..9}]) then
    begin
        Key:=0;
        sgAgeView.AllowEditing();
        Exit();
    end;


    if Key = VK_RETURN then
    begin
        Key:=0;
        sgAgeView.QuitEditing();
        Service.Mediator.Comments.FreeFieldsUpdateAsync(UpdateFreeFields(sgAgeView), FreeFieldsUpdate_Callback);
        Exit();
    end;

    if Key = VK_DELETE then
    begin

        Key:=0;

        if sgAgeView.Col = sgAgeView.GetCol(TCustomerSnapshotEx._Free1) then
        begin
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:=' ';
            Service.Mediator.Comments.FreeFieldsUpdateAsync(UpdateFreeFields(sgAgeView), FreeFieldsUpdate_Callback);
        end;

        if sgAgeView.Col = sgAgeView.GetCol(TCustomerSnapshotEx._Free2) then
        begin
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:=' ';
            Service.Mediator.Comments.FreeFieldsUpdateAsync(UpdateFreeFields(sgAgeView), FreeFieldsUpdate_Callback);
        end;

        if sgAgeView.Col = sgAgeView.GetCol(TCustomerSnapshotEx._Free3) then
        begin
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:=' ';
            Service.Mediator.Comments.FreeFieldsUpdateAsync(UpdateFreeFields(sgAgeView), FreeFieldsUpdate_Callback);
        end;

    end;

end;


procedure TMainForm.sgAgeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    // Empty
end;


procedure TMainForm.sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    // Empty
end;


procedure TMainForm.sgAddressBookKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    // Empty
end;


procedure TMainForm.sgListSectionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if FAllowEditConfig then
    begin
        if (Key = 86) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(TActions.Paste);
        if (Key = 88) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(TActions.Cut);
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(TActions.Copy);
    if (Key = 46) and (txtAllowEdit.Font.Style = [fsBold]) then sgListSection.DelEsc(TActions.Delete, sgListSection.Col, sgListSection.Row);

end;


procedure TMainForm.sgListSectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then sgListSection.DelEsc(TActions.Escape, sgListSection.Col, sgListSection.Row);
end;


procedure TMainForm.sgListSectionKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.CR then sgListSection.Cells[1, sgListSection.Row]:=UpperCase(sgListSection.Cells[1, sgListSection.Row]);
end;


procedure TMainForm.sgListValueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if FAllowEditConfig then
    begin
        if (Key = 86) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(TActions.Paste);
        if (Key = 88) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(TActions.Cut);
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(TActions.Copy);
    if (Key = 46) and (txtAllowEdit.Font.Style = [fsBold]) then sgListValue.DelEsc(TActions.Delete, sgListValue.Col, sgListValue.Row);

end;


procedure TMainForm.sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then sgListValue.DelEsc(TActions.Escape, sgListValue.Col, sgListValue.Row);
end;


procedure TMainForm.EditPasswordKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.CR then btnUnlockClick(self);
end;


{$ENDREGION}


end.

