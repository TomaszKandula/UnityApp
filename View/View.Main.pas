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
    Data.DB,
    Data.Win.ADODB,
    Data.Bind.Components,
    Data.Bind.ObjectScope,
    IPPeerClient,
    SHDocVw,
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
    Unity.Arrays,
    Unity.Enums,
    Unity.Records,
    Unity.References;


type


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
        Text82: TLabel;
        tcOvdAmt: TLabel;
        GroupListBox: TComboBox;
        GroupListDates: TComboBox;
        Text31: TLabel;
        procRISKA: TLabel;
        procRISKB: TLabel;
        procRISKC: TLabel;
        Text36: TLabel;
        DataUpdated: TLabel;
        btnMakeGroup: TImage;
        Text83L1: TLabel;
        Text83L2: TLabel;
        Text53: TLabel;
        PanelGroupName: TPanel;
        btnMakeGroupAge: TSpeedButton;
        EditGroupName: TLabeledEdit;
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
        N13: TMenuItem;
        XLExport: TSaveDialog;
        tR6: TLabel;
        valR6: TLabel;
        procR6: TLabel;
        N14: TMenuItem;
        Action_SearchBook: TMenuItem;
        Action_Overdue: TMenuItem;
        N15: TMenuItem;
        TabSheet9: TTabSheet;
        sgCompanyData: TStringGrid;
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
        SplitLine3: TBevel;
        imgEventLog: TImage;
        Text51: TLabel;
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
        btnUnlock: TSpeedButton;
        btnPassUpdate: TSpeedButton;
        Action_AddFollowUpGroup: TMenuItem;
        Action_RemoveFollowUps: TMenuItem;
        Cap24: TShape;
        hShapeSorting: TShape;
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
        Shape: TShape;
        Action_Free3: TMenuItem;
        N4: TMenuItem;
        N12: TMenuItem;
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
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
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
        procedure sgAgeViewColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
        procedure Action_SearchBookClick(Sender: TObject);
        procedure Action_OverdueClick(Sender: TObject);
        procedure sgCompanyDataKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure sgCompanyDataKeyPress(Sender: TObject; var Key: Char);
        procedure sgCompanyDataDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
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
        procedure Action_Free3Click(Sender: TObject);
        procedure Action_SalesRespClick(Sender: TObject);
        procedure Action_CustomerGrpClick(Sender: TObject);
        procedure Action_PersonRespClick(Sender: TObject);
        procedure Action_AccountTypeClick(Sender: TObject);
        procedure Action_ViewOptionsClick(Sender: TObject);
        procedure TabSheet5Show(Sender: TObject);
        procedure sgFSCviewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgLBUviewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure sgLBUviewClick(Sender: TObject);
        procedure sgFSCviewClick(Sender: TObject);
        procedure btnFscApproveClick(Sender: TObject);
        procedure btnFscRejectClick(Sender: TObject);
        procedure btnLbuUpdateClick(Sender: TObject);
        procedure Action_TurnRowHighlightClick(Sender: TObject);
        procedure CommonPopupMenuPopup(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
    protected
        procedure CreateParams(var Params: TCreateParams); override;
        procedure WndProc(var msg: TMessage); override;
        procedure WndMessagesChromium(PassMsg: TMessage);
        procedure WndMessagesWindows(PassMsg: TMessage);
        procedure WndMessagesExternal(PassMsg: TMessage);
        // --------------------------------------------------------------------------------
        // Chromium component. Do not modify, rather follow Chromium implementation manual.
        // See more: https://github.com/salvadordf/CEF4Delphi.
        // --------------------------------------------------------------------------------
        procedure NotifyMoveOrResizeStarted;
        procedure ChromiumModalLoopOn(PassMsg: TMessage);
        procedure ChromiumModalLoopOff(PassMsg: TMessage);
        procedure Chromium_OnBeforePopup(
            Sender: TObject;
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
            var Result: Boolean
        );
    strict private
        var CustAll:     integer;
        var ANotDue:     extended;
        var ARange1:     extended;
        var ARange2:     extended;
        var ARange3:     extended;
        var ARange4:     extended;
        var ARange5:     extended;
        var ARange6:     extended;
        var Balance:     extended;
        var Limits:      extended;
        var Exceeders:   integer;
        var TotalExceed: extended;
        var RCA:         extended;
        var RCB:         extended;
        var RCC:         extended;
        var RCAcount:    cardinal;
        var RCBcount:    cardinal;
        var RCCcount:    cardinal;
        var FHadFirstLoad: boolean;
        var FAllowClose:   boolean;
        var FAbUpdateFields:       TAddressBookUpdateFields;
        var FDailyCommentFields:   TDailyCommentFields;
        var FGeneralCommentFields: TGeneralCommentFields;
        procedure SetPanelBorders;
        procedure SetGridColumnWidths;
        procedure SetGridRowHeights;
        procedure SetButtonsGlyphs;
        procedure SetSettingsPanel(IsLocked: boolean);
        procedure InitializeScreenSettings;
        function  AddressBookExclusion: boolean;
        procedure ClearAgeSummary();
        procedure UpdateOpenItemsDetails(var Grid: TStringGrid);
        procedure LoadColumnWidth(var Grid: TStringGrid);
        procedure MapGroup3(var Grid: TStringGrid; var Source: TStringGrid);
        procedure MapTable1(var Grid: TStringGrid; var Source: TStringGrid);
        procedure MapTable2(var Grid: TStringGrid; var Source: TStringGrid);
        procedure MapTable3(var Grid: TStringGrid; var Source: TStringGrid);
        procedure MapTable4(var Grid: TStringGrid; var Source: TStringGrid);
        function GetData(Code: string; Table: string; Entity: string): string; // split into awaited functions (1)
        procedure OpenAddressBook_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
        procedure UpdateAddressBook_Callback(CallResponse: TCallResponse);
        procedure AddToAddressBook_Callback(CallResponse: TCallResponse);
        procedure MakeAgeViewSQL_Callback(CallResponse: TCallResponse);
        procedure MakeAgeViewCSV_Callback(CsvContent: TStringList; CallResponse: TCallResponse);
        procedure ReadAgeView_Callback(ActionMode: TLoading; ReturnedData: TStringGrid; CallResponse: TCallResponse);
        procedure ScanOpenItems_Callback(CanMakeAge: boolean; ReadDateTime: string; CallResponse: TCallResponse);
        procedure ReadOpenItems_Callback(ActionMode: TLoading; OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse);
        procedure CheckGivenPassword_Callback(CallResponse: TCallResponse);
        procedure SetNewPassword_Callback(CallResponse: TCallResponse);
        procedure CheckServerConn_Callback(IsConnected: boolean; CallResponse: TCallResponse);
        procedure ExcelExport_Callback(CallResponse: TCallResponse);
        procedure RefreshInvoiceTracker_Callback(InvoiceList: TStringGrid; CallResponse: TCallResponse);
        procedure DeleteFromTrackerList_Callback(CallResponse: TCallResponse);
    public // remove!!!
        // Legacy code, to be removed [start]
        var FGroupIdSel:      string;
        var FGroupNmSel:      string;
        var FAgeDateSel:      string;
        var FAccessLevel:     string;
        var FAccessMode:      string;
        var FOpenItemsUpdate: string;
        var FOpenItemsStatus: string;
        var FOSAmount:        double;
        var FIsConnected:     boolean;
        procedure TryInitConnection;
        // Legacy code, to be removed [end]
        // replace by callbacks from async methods [start] (3)
        procedure CallbackAwaitForm(PassMsg: TMessage);
        procedure CallbackStatusBar(PassMsg: TMessage);
        procedure CallbackMessageBox(PassMsg: TMessage);
        procedure WndMessagesInternal(PassMsg: TMessage);
        // replace by callbacks from async methods [end]
    public
        var FClass_A:        double;
        var FClass_B:        double;
        var FClass_C:        double;
        var FStartTime:      TTime;
        var FGroupList:      TALists{Legacy};
        var FAgeDateList:    TALists;
        var FGridPicture:    TImage;
        var FOpenItemsRefs:  TFOpenItemsRefs;
        var FCtrlStatusRefs: TFCtrlStatusRefs;
        procedure SetActiveTabsheet(TabSheet: TTabSheet);
        procedure ResetTabsheetButtons();
        procedure UpdateAgeSummary();
        procedure ComputeAgeSummary(var Grid: TStringGrid);  // make async! (2)
        procedure ComputeRiskClass(var Grid: TStringGrid);
        procedure ClearOpenItemsSummary();
        procedure InitMainWnd(SessionFile: string);
        procedure SetupMainWnd();
        procedure StartMainWnd();
        procedure UpdateFOpenItemsRefs(SourceGrid: TStringGrid);
        procedure UpdateFControlStatusRefs(SourceGrid: TStringGrid);
        procedure SwitchTimers(State: TAppTimers);
    end;


    function MainForm(): TMainForm;


implementation


{$R *.dfm}


uses
    View.GridFilter,
    View.InvoiceTracker,
    View.InvoiceList,
    View.Actions,
    View.Calendar,
    View.About,
    View.GridSearch,
    View.ColorPicker,
    View.EventLog,
    View.UserFeedback,
    View.SqlSearch,
    View.MassMailer,
    View.AwaitScreen,
    View.Startup,
    View.Reports,
    Unity.Sql,
    Unity.Messaging,
    Unity.UserAccess,
    Unity.Filtering,
    Unity.Chars,
    Unity.Helpers,
    Unity.Common,
    Unity.Unknown,
    Unity.StatusBar,
    Unity.DateTimeFormats,
    Unity.Sorting,
    Unity.UserSid,
    Unity.Utilities,
    Handler.Sql{legacy},
    DbModel{legacy},
    Handler.Database{legacy},
    Handler.Account{legacy},
    Unity.EventLogger,
    Unity.Settings,
    Unity.SessionService,
    Sync.Documents,
    Async.Utilities,
    Async.Tracker,
    Async.Queries,
    Async.Debtors,
    Async.OpenItems,
    Async.AddressBook,
    Async.Comments,
    uCEFApplication;


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


// ----------------------------------------------------------------------------------------------------------------------------------------------- CALLBACKS //


// -----------------------------------
// Async.AddressBook callback methods.
// -----------------------------------

procedure TMainForm.OpenAddressBook_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
        ThreadFileLog.Log('[OpenAddressBookAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    sgAddressBook.Freeze(True);

    sgAddressBook.RowCount:=ReturnedData.RowCount;
    sgAddressBook.ColCount:=ReturnedData.ColCount;

    for var iCNT:=0 to ReturnedData.RowCount - 1 do
        for var jCNT:=0 to ReturnedData.ColCount - 1 do
            sgAddressBook.Cells[jCNT, iCNT]:=ReturnedData.Cells[jCNT, iCNT];

    sgAddressBook.Freeze(False);
    sgAddressBook.SetColWidth(40, 10, 400);

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
    ThreadFileLog.Log('[OpenAddressBookAsync_Callback]: Address Book has been opened.');

end;


procedure TMainForm.UpdateAddressBook_Callback(CallResponse: TCallResponse);
begin

    case CallResponse.IsSucceeded of

        True:
        begin
            THelpers.MsgCall(TAppMessage.Info, CallResponse.LastMessage);
            ThreadFileLog.Log('[UpdateAddressBookAsync_Callback]: Address Book updated.');
        end;

        False:
        begin
            THelpers.MsgCall(TAppMessage.Warn, CallResponse.LastMessage);
            ThreadFileLog.Log('[UpdateAddressBookAsync_Callback]: Adddress Book has thrown an error "' + CallResponse.LastMessage + '".');
        end;

    end;

end;


procedure TMainForm.AddToAddressBook_Callback(CallResponse: TCallResponse);
begin

    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);

    case CallResponse.IsSucceeded of

        True:
        begin
            THelpers.MsgCall(TAppMessage.Info, CallResponse.LastMessage);
            ThreadFileLog.Log('[AddToAddressBookAsync_Callback]: Item has been added to Adddress Book.');
        end;

        False:
        begin
            THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
            ThreadFileLog.Log('[AddToAddressBookAsync_Callback]: Adddress Book has thrown an error "' + CallResponse.LastMessage + '".');
        end;

    end;

end;


// -------------------------------
// Async.Debtors callback methods.
// -------------------------------

procedure TMainForm.MakeAgeViewSQL_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
        ThreadFileLog.Log('[MakeAgeViewSQLAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    // --------------------------------
    // Load newly generated aging view.
    // --------------------------------

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading, MainForm);
    sgAgeView.Freeze(True);

    var Debtors: IDebtors:=TDebtors.Create;
    Debtors.ReadAgeViewAsync(TLoading.NullParameter, TSorting.TMode.Ranges, FGroupIdSel, FAgeDateSel, ReadAgeView_Callback);
    ThreadFileLog.Log('[MakeAgeViewSQLAsync_Callback]: Aging has been generated, loading to AgeView...');

end;


procedure TMainForm.MakeAgeViewCSV_Callback(CsvContent: TStringList; CallResponse: TCallResponse);
begin

    EditGroupName.Enabled:=False;
    EditGroupID.Enabled:=False;
    cbDump.Enabled:=False;
    EditGroupName.Text:='';
    EditGroupID.Text  :='';
    cbDump.Checked:=False;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
        ThreadFileLog.Log('[MakeAgeViewCSVAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    // ---------------------------
    // Put CSV aging data to file.
    // ---------------------------

    if CSVExport.Execute then
        CsvContent.SaveToFile(CSVExport.FileName);

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
    ThreadFileLog.Log('[MakeAgeViewCSVAsync_Callback]: Aging has been saved to CSV file.');

end;


procedure TMainForm.ReadAgeView_Callback(ActionMode: TLoading; ReturnedData: TStringGrid; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
        ThreadFileLog.Log('[ReadAgeViewAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    // ---------------------
    // Update age view grid.
    // ---------------------

    MainForm.sgAgeView.Freeze(True);
    try

        MainForm.sgAgeView.SqlColumns:=ReturnedData.SqlColumns;
        MainForm.sgAgeView.RowCount  :=ReturnedData.RowCount;
        MainForm.sgAgeView.ColCount  :=ReturnedData.ColCount;

        for var iCNT:=0 to ReturnedData.RowCount - 1 do
            for var jCNT:=0 to ReturnedData.ColCount - 1 do
                MainForm.sgAgeView.Cells[jCNT, iCNT]:=ReturnedData.Cells[jCNT, iCNT];

    finally
        MainForm.sgAgeView.Freeze(False);
        ThreadFileLog.Log('[ReadAgeViewAsync_Callback]: Age View updated.');
    end;

    // -------------------------
    // Update aging information.
    // -------------------------

    MainForm.ClearAgeSummary();
    MainForm.ComputeAgeSummary(MainForm.sgAgeView); // make async!

    MainForm.ComputeRiskClass(MainForm.sgAgeView);
    MainForm.UpdateAgeSummary();
    MainForm.UpdateOpenItemsDetails(MainForm.sgCompanyData);

    ThreadFileLog.Log('[ReadAgeViewAsync_Callback]: Age View summary information updated.');

    // ---------------------------------------------------------------
    // Get descriptions from helper tables for given age view columns.
    // The helper grid names corresponds to age view columns.
    // ---------------------------------------------------------------

    MainForm.MapGroup3(MainForm.sgAgeView, MainForm.sgGroup3);
    MainForm.MapTable1(MainForm.sgAgeView, MainForm.sgPersonResp);
    MainForm.MapTable2(MainForm.sgAgeView, MainForm.sgSalesResp);
    MainForm.MapTable3(MainForm.sgAgeView, MainForm.sgAccountType);
    MainForm.MapTable4(MainForm.sgAgeView, MainForm.sgCustomerGr);
    ThreadFileLog.Log('[ReadAgeViewAsync_Callback]: Mapping performed.');

    // -------------------------------------
    // Unlock the component and repaint VCL.
    // -------------------------------------

    MainForm.LoadColumnWidth(MainForm.sgAgeView);
    MainForm.SwitchTimers(TurnedOn);
    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
    ThreadFileLog.Log('[ReadAgeViewAsync_Callback]: VCL unlocked and repainted.');

    // ----------------------------------------------------
    // Call open items loading after aging is presented.
    // Do not load open items if age view cannot be loaded.
    // ----------------------------------------------------

    if ActionMode = TLoading.CallOpenItems then
    begin

        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Downloading, MainForm);
        ThreadFileLog.Log('[ReadAgeViewAsync_Callback]: Calling method "ReadOpenItemsAsync".');

        MainForm.ClearOpenItemsSummary();
        MainForm.sgOpenItems.Freeze(True);

        var OpenItems: IOpenItems:=TOpenItems.Create();
        OpenItems.ReadOpenItemsAsync(TLoading.NullParameter, sgOpenItems, sgCompanyData, ReadOpenItems_Callback);

    end;

end;


// ---------------------------------
// Async.OpenItems callback methods.
// ---------------------------------

procedure TMainForm.ScanOpenItems_Callback(CanMakeAge: boolean; ReadDateTime: string; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
        ThreadFileLog.Log('[ScanOpenItemsAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    FOpenItemsUpdate:=ReadDateTime;
    FOpenItemsStatus:='';

    if CanMakeAge then
    begin

        // -----------------------------------------------------
        // Get latest open items and allow to make aging report.
        // -----------------------------------------------------

        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Downloading, MainForm);
        ClearOpenItemsSummary();
        sgOpenItems.Freeze(True);

        ThreadFileLog.Log('[ReadOpenItemsAsync_Callback]: Calling method "MakeAgeViewSQLAsync".');
        var OpenItems: IOpenItems:=TOpenItems.Create();
        OpenItems.ReadOpenItemsAsync(TLoading.CallMakeAge, sgOpenItems, sgCompanyData, ReadOpenItems_Callback);

    end
    else
    begin
        ThreadFileLog.Log('[ScanOpenItemsAsync_Callback]: Open Items check result is "nothing to update".');
    end;

end;


procedure TMainForm.ReadOpenItems_Callback(ActionMode: TLoading; OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse);
begin

    sgOpenItems.Freeze(False);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);
        ThreadFileLog.Log('[ReadOpenItemsAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    MainForm.tcOpenItems.Caption:=FormatFloat('### ###',  OpenItemsData.TotalItems);
    MainForm.tcInvoices.Caption :=FormatFloat('### ###',  OpenItemsData.NumOfInvoices);
    MainForm.tcOverdue.Caption  :=FormatFloat('### ###',  OpenItemsData.OverdueItems);
    MainForm.tcOSAmt.Caption    :=FormatFloat('#,##0.00', OpenItemsData.OsAmount);
    MainForm.tcOvdAmt.Caption   :=FormatFloat('#,##0.00', OpenItemsData.OvdAmount);
    MainForm.tcUNAmt.Caption    :=FormatFloat('#,##0.00', OpenItemsData.UnallocatedAmt);
    MainForm.FOSAmount          :=OpenItemsData.OsAmount;

    MainForm.sgOpenItems.SetColWidth(10, 20, 400);
    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

    // -----------------------------------------------------
    // Make age view from open items and send to SQL Server.
    // -----------------------------------------------------

    if ActionMode = CallMakeAge then
    begin

        MainForm.cbDump.Checked:=False;
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Generating, MainForm);

        ThreadFileLog.Log('[ReadOpenItemsAsync_Callback]: Calling method "MakeAgeViewSQLAsync".');
        var Debtors: IDebtors:=TDebtors.Create();
        Debtors.MakeAgeViewSQLAsync(FOSAmount, FGroupIdSel, sgOpenItems, sgCompanyData, MakeAgeViewSQL_Callback);

    end;

end;


// ---------------------------------
// Async.Utilities callback methods.
// ---------------------------------

procedure TMainForm.CheckGivenPassword_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log('[CheckGivenPassword_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    // Enable controls
    SetSettingsPanel(False);

    // Populate string grids
    var List: TStringList:=TStringList.Create();
    var Settings: ISettings:=TSettings.Create();

    try

        Settings.GetSections(List);
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

    // Grids dimensions
    {sgListValue.SetColWidth(25, 30, 400);}
    {sgListSection.SetColWidth(25, 30, 400);}

    // Clear edit box from provided password
    EditPassword.Text:='';

end;


procedure TMainForm.SetNewPassword_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log('[SetNewPassword_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, 'New password has been saved.');

    btnPassUpdate.Enabled:=False;
    EditCurrentPassword.Enabled:=False;
    EditNewPassword.Enabled:=False;
    EditNewPasswordConfirmation.Enabled:=False;

    EditCurrentPassword.Text:='';
    EditNewPassword.Text:='';
    EditNewPasswordConfirmation.Text:='';

end;


procedure TMainForm.CheckServerConn_Callback(IsConnected: boolean; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    case IsConnected of
        True:  MainForm.TryInitConnection();
        False: MainForm.FIsConnected:=False;
    end;

end;


procedure TMainForm.ExcelExport_Callback(CallResponse: TCallResponse);
begin

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, 'Data has been exported successfully!');

end;


// --------------------------------
// Async.Tracker callbacks methods.
// --------------------------------

procedure TMainForm.RefreshInvoiceTracker_Callback(InvoiceList: TStringGrid; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    MainForm.sgInvoiceTracker.Freeze(True);
    try

        MainForm.sgInvoiceTracker.SqlColumns:=InvoiceList.SqlColumns;
        MainForm.sgInvoiceTracker.RowCount  :=InvoiceList.RowCount;
        MainForm.sgInvoiceTracker.ColCount  :=InvoiceList.ColCount;

        for var iCNT:=0 to InvoiceList.RowCount - 1 do
            for var jCNT:=0 to InvoiceList.ColCount - 1 do
                MainForm.sgInvoiceTracker.Cells[jCNT, iCNT]:=InvoiceList.Cells[jCNT, iCNT];

    finally
        MainForm.sgInvoiceTracker.Freeze(False);
        ThreadFileLog.Log('[RefreshInvoiceTracker_Callback]: Invoice Tracker list updated.');
    end;

    if MainForm.sgInvoiceTracker.RowCount > 1 then
    begin
        MainForm.sgInvoiceTracker.SetColWidth(10, 20, 400);
        MainForm.sgInvoiceTracker.Visible:=True;
    end
    else
    begin
        MainForm.sgInvoiceTracker.Visible:=False;
    end;

end;


procedure TMainForm.DeleteFromTrackerList_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    MainForm.sgInvoiceTracker.DeleteRowFrom(1, 1);

end;


// ------------------------------------------------------------------------------------------------------------------------ LEGACY CODE - TO BE REMOVED [START]

{remove}
procedure TMainForm.TryInitConnection;
begin

    if not Assigned(SessionService.FDbConnect) then
        SessionService.FDbConnect:=TADOConnection.Create(nil);

    var DataBase:=TDataBase.Create(True);
    try

        if DataBase.Check = 0 then
        begin

            if DataBase.InitializeConnection(True, SessionService.FDbConnect) then
            begin

                // Check server connection on regular basis
                if not InetTimer.Enabled then
                begin
                    InetTimer.Interval:=DataBase.Interval;
                    InetTimer.Enabled:=True;
                end;

            end;

            ThreadFileLog.Log('[TryInitConnection]: Server connection has been established successfully.');
            FIsConnected:=True;
            SwitchTimers(TurnedOn);

        end
        else
        begin
            FIsConnected:=False;
            SwitchTimers(TurnedOff);
        end;

    finally
        DataBase.Free();
    end;

end;


{remove}
procedure TMainForm.CallbackAwaitForm(PassMsg: TMessage);
begin

    if PassMsg.WParam = TMessaging.TWParams.AwaitForm then
    begin

        case PassMsg.LParam of
            TMessaging.TAwaitForm.Show: AwaitForm.Show();
            TMessaging.TAwaitForm.Hide: AwaitForm.Close();
        end;

    end;

end;


{remove}
procedure TMainForm.CallbackStatusBar(PassMsg: TMessage);
begin

    case PassMsg.WParam of

        TMessaging.TWParams.StatusBar:
        begin
            StatBar_TXT1.Caption:=PChar(PassMsg.LParam);
        end;

        TMessaging.TWParams.ConnectionOk:
        begin
            StatBar_TXT7.Font.Style:=[];
            StatBar_TXT7.Caption:='Connected with Microsoft SQL Server.';
        end;

        TMessaging.TWParams.ConnectionError:
        begin
            StatBar_TXT7.Font.Style:=[fsBold];
            StatBar_TXT7.Caption:='Connection lost, re-connecting...';
        end;

    end;

end;


{remove}
procedure TMainForm.CallbackMessageBox(PassMsg: TMessage);
begin

    case PassMsg.WParam of
        TMessaging.TWParams.MessageInfo:      THelpers.MsgCall(TAppMessage.Info,      PChar(PassMsg.LParam));
        TMessaging.TWParams.MessageWarn:      THelpers.MsgCall(TAppMessage.Warn,      PChar(PassMsg.LParam));
        TMessaging.TWParams.MessageError:     THelpers.MsgCall(TAppMessage.Error,     PChar(PassMsg.LParam));
        TMessaging.TWParams.MessageQuestion1: THelpers.MsgCall(TAppMessage.Question1, PChar(PassMsg.LParam));
        TMessaging.TWParams.MessageQuestion2: THelpers.MsgCall(TAppMessage.Question2, PChar(PassMsg.LParam));
    end;

end;

{remove}
procedure TMainForm.WndMessagesInternal(PassMsg: TMessage);
begin

    if PassMsg.Msg <> THelpers.WM_GETINFO then Exit;
    OutputDebugString(PChar('WM_GETINFO RECEIVED'));

    CallbackMessageBox(PassMsg);
    CallbackStatusBar(PassMsg);
    CallbackAwaitForm(PassMsg);

end;


// -------------------------------------------------------------------------------------------------------------------------- LEGACY CODE - TO BE REMOVED [END]


// ---------------------------------------------------------------------------------------------------------------------------------------- WINDOWS MESSAGES //


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

    if PassMsg.Msg <> THelpers.WM_EXTINFO then Exit();
    OutputDebugString(PChar('WM_EXTINFO RECEIVED'));

    // Log time (seconds) in database "general comment" table
    if PassMsg.LParam > 0 then
    begin

        FDailyCommentFields.GroupIdSel   :=FGroupIdSel;
        FDailyCommentFields.AgeDateSel   :=FAgeDateSel;
        FDailyCommentFields.CUID         :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), sgAgeView.Row];
        FDailyCommentFields.Email        :=False;
        FDailyCommentFields.CallEvent    :=True;
        FDailyCommentFields.CallDuration :=PassMsg.LParam;
        FDailyCommentFields.Comment      :='';
        FDailyCommentFields.EmailReminder:=False;
        FDailyCommentFields.EmailAutoStat:=False;
        FDailyCommentFields.EmailManuStat:=False;
        FDailyCommentFields.EventLog     :=True;
        FDailyCommentFields.UpdateGrid   :=True;
        FDailyCommentFields.ExtendComment:=False;

        var Comments: IComments:=TComments.Create();
        Comments.EditDailyComment(FDailyCommentFields, nil);

    end;

end;


procedure TMainForm.WndMessagesWindows(PassMsg: TMessage);
begin

    case PassMsg.Msg of

        // ---------------------------
        // Windows query for shutdown.
        // ---------------------------

        WM_QUERYENDSESSION:
        begin
            ThreadFileLog.Log('Windows Message detected: ' + IntToStr(PassMsg.Msg) + ' WM_QUERYENDSESSION. Windows is going to be shut down. Closing ' + TCommon.AppCaption + '...');
            FAllowClose:=True;
            PassMsg.Result:=1;
        end;

        // -------------------------
        // Windows is shutting down.
        // -------------------------

        WM_ENDSESSION:
        begin
            ThreadFileLog.Log('Windows Message detected: ' + IntToStr(PassMsg.Msg) + ' WM_ENDSESSION. Windows is shutting down...');
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

                // -------------------------------
                // Turn off timers and disconnect.
                // -------------------------------

                SwitchTimers(TurnedOff);
                InetTimer.Enabled:=False;
                SessionService.FDbConnect.Connected:=False;
                SessionService.FDbConnect:=nil;
                FIsConnected:=False;
                THelpers.ExecMessage(False, TMessaging.TWParams.ConnectionError, TUnknown.NULL, MainForm);
                ThreadFileLog.Log('Windows Message detected: ' + IntToStr(PassMsg.Msg) + ' WM_POWERBROADCAST with PBT_APMSUSPEND. Going into suspension mode, Unity is disconnected from server.');

            end;

            // -----------------------------------------------------------
            // Operation is resuming automatically from a low-power state.
            // This message is sent every time the system resumes.
            // -----------------------------------------------------------

            PBT_APMRESUMEAUTOMATIC:
            begin

                // --------------------------------------------------------
                // Turn on timer responsible for periodic connection check.
                // --------------------------------------------------------

                InetTimer.Enabled:=True;
                ThreadFileLog.Log('Windows Message detected: ' + IntToStr(PassMsg.Msg) + ' WM_POWERBROADCAST with PBT_APMRESUMEAUTOMATIC. Windows has resumed after being suspended.');

            end;

        end;

    end;

end;


procedure TMainForm.WndProc(var Msg: TMessage);
begin
    inherited;
    WndMessagesChromium(Msg);
    WndMessagesInternal(Msg);
    WndMessagesExternal(Msg);
    WndMessagesWindows(Msg);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZATION //


procedure TMainForm.InitMainWnd(SessionFile: string);
begin

    ThreadFileLog.LogFileName:=SessionFile;

    FStartTime:=Now();
    FormatDateTime('hh:mm:ss', Now());
    FormatDateTime('hh:mm:ss', Now());

    StatBar_TXT1.Caption:=TStatusBar.Ready;
    StatBar_TXT2.Caption:=SessionService.SessionUser;
    StatBar_TXT3.Caption:=DateToStr(Now);

    ThreadFileLog.Log('Application version = ' + TCore.GetBuildInfoAsString);
    ThreadFileLog.Log('User SID = ' + TUserSid.GetCurrentUserSid);

end;


procedure TMainForm.SetupMainWnd();
begin

    if FHadFirstLoad then Exit();

    ChromiumWindow.ChromiumBrowser.OnBeforePopup:=Chromium_OnBeforePopup;
    if not ChromiumWindow.Initialized then ChromiumWindow.CreateBrowser();

    for var iCNT:=0 to MyPages.PageCount - 1 do MyPages.Pages[iCNT].TabVisible:=False;
        MyPages.ActivePage:=TabSheet9;

    SetPanelBorders();
    SetGridColumnWidths();
    SetGridRowHeights();
    SetButtonsGlyphs();

    UpTime.Enabled:=True;
    CurrentTime.Enabled:=True;

    var Queries: IQueries:=TQueries.Create;
    Queries.InitializeQms;

    if FAccessLevel <> TUserAccess.Admin then
    begin
        sgCompanyData.Enabled:=False;
        GroupListDates.Enabled:=False;
    end;

    GroupListBox.ListToComboBox(FGroupList, 1, TListSelection.First);
    GroupListDates.ListToComboBox(FAgeDateList, 0, TListSelection.Last);

end;


procedure TMainForm.StartMainWnd();
begin

    if not FHadFirstLoad then
    begin

        var NewTask: ITask:=TTask.Create(procedure
        begin

            // Delay
            Sleep(1500);

            TThread.Synchronize(nil, procedure
            begin

                // Load (async) default age snapshot
                if not(string.IsNullOrEmpty(GroupListBox.Text)) and not(string.IsNullOrEmpty(GroupListDates.Text)) then
                begin

                    FGroupIdSel:=FGroupList[GroupListBox.ItemIndex, 0];
                    FGroupNmSel:=FGroupList[GroupListBox.ItemIndex, 1];
                    FAgeDateSel:=GroupListDates.Text;
                    sgAgeView.Enabled:=True;

                    var OpenItems: IOpenItems:=TOpenItems.Create();

                    FOpenItemsUpdate:=OpenItems.GetDateTimeAwaited(DateTime);
                    FOpenItemsStatus:=OpenItems.GetStatusAwaited(FOpenItemsUpdate);

                    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading, MainForm);
                    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);
                    MainForm.ClearAgeSummary();

                    if string.IsNullOrEmpty(FOpenItemsUpdate) then
                    begin
                        THelpers.MsgCall(TAppMessage.Warn, 'Cannot find open items in database. Please contact IT support.');
                        var Debtors: IDebtors:=TDebtors.Create();
                        Debtors.ReadAgeViewAsync(TLoading.NullParameter, TSorting.TMode.Ranges, FGroupIdSel, FAgeDateSel, ReadAgeView_Callback);
                    end
                    else
                    begin
                        var Debtors: IDebtors:=TDebtors.Create();
                        Debtors.ReadAgeViewAsync(TLoading.CallOpenItems, TSorting.TMode.Ranges, FGroupIdSel, FAgeDateSel, ReadAgeView_Callback);
                    end;

                end;

            end);

        end);

        NewTask.Start();
        FHadFirstLoad:=True;

    end;

end;



procedure TMainForm.UpdateFOpenItemsRefs(SourceGrid: TStringGrid);
begin

    // -----------------------------------------------------------------------------------------------------------------
    // Get column reference on demand for Open Items string grid. The reason is, despite we do not change columns order
    // at run time programatically, it may be changed on server-side and that will be immediatelly reflected
    // in Open Items string grid that serves the user and the application as the source of data.
    // Additional purpose of the code is - to get the columns at once instead using ReturnColumn multiple times in given
    // method, this increase the overall performance of the code and decreases complexity.
    // -----------------------------------------------------------------------------------------------------------------

    // ----------------------------------------------------------------------------------------------------------
    // The nature of open items is that, it changes continuously, but due to ERP database workload during the day
    // we have decided to update the data in Open Items table few times a day (on regular basis).
    // ----------------------------------------------------------------------------------------------------------

    FOpenItemsRefs.CuidCol     :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Cuid,      1, 1);
    FOpenItemsRefs.OpenAmCol   :=SourceGrid.ReturnColumn(DbModel.TOpenitems.OpenAm,    1, 1);
    FOpenItemsRefs.PmtStatCol  :=SourceGrid.ReturnColumn(DbModel.TOpenitems.PmtStat,   1, 1);
    FOpenItemsRefs.CtrlCol     :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Ctrl,      1, 1);
    FOpenItemsRefs.InvoNoCol   :=SourceGrid.ReturnColumn(DbModel.TOpenitems.InvoNo,    1, 1);
    FOpenItemsRefs.ValDtCol    :=SourceGrid.ReturnColumn(DbModel.TOpenitems.ValDt,     1, 1);
    FOpenItemsRefs.DueDtCol    :=SourceGrid.ReturnColumn(DbModel.TOpenitems.DueDt,     1, 1);
    FOpenItemsRefs.ISOCol      :=SourceGrid.ReturnColumn(DbModel.TOpenitems.ISO,       1, 1);
    FOpenItemsRefs.CurAmCol    :=SourceGrid.ReturnColumn(DbModel.TOpenitems.CurAm,     1, 1);
    FOpenItemsRefs.OpenCurAmCol:=SourceGrid.ReturnColumn(DbModel.TOpenitems.OpenCurAm, 1, 1);
    FOpenItemsRefs.Ad1Col      :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Ad1,       1, 1);
    FOpenItemsRefs.Ad2Col      :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Ad2,       1, 1);
    FOpenItemsRefs.Ad3Col      :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Ad3,       1, 1);
    FOpenItemsRefs.PnoCol      :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Pno,       1, 1);
    FOpenItemsRefs.PAreaCol    :=SourceGrid.ReturnColumn(DbModel.TOpenitems.PArea,     1, 1);
    FOpenItemsRefs.Text        :=SourceGrid.ReturnColumn(DbModel.TOpenitems.Txt,       1, 1);

end;


procedure TMainForm.UpdateFControlStatusRefs(SourceGrid: TStringGrid);
begin

    // -----------------------------------------------------------------------
    // Get column reference of Control Status table located in General Tables.
    // Similarly to the "UpdateFOpenItemsRefs" method,
    // we use it to decrease level of usage of ReturnColumn method.
    // -----------------------------------------------------------------------

    FCtrlStatusRefs.Id         :=SourceGrid.ReturnColumn(TControlStatus.Id,   1, 1);
    FCtrlStatusRefs.Code       :=SourceGrid.ReturnColumn(TControlStatus.Code, 1, 1);
    FCtrlStatusRefs.Text       :=SourceGrid.ReturnColumn(TControlStatus.Text, 1, 1);
    FCtrlStatusRefs.Description:=SourceGrid.ReturnColumn(TControlStatus.Description, 1, 1);

end;


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

    // Execute on "before popup" - ignore tab sheets and pop-ups.
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


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


procedure TMainForm.SetSettingsPanel(IsLocked: boolean);
begin

    if IsLocked then
    begin

        // Visibility on
        imgOFF.Visible:=True;
        btnPassUpdate.Enabled:=False;

        // Edit boxes
        EditCurrentPassword.Enabled:=False;
        EditNewPassword.Enabled:=False;
        EditNewPasswordConfirmation.Enabled:=False;
        EditCurrentPassword.Text:='';
        EditNewPassword.Text:='';
        EditNewPasswordConfirmation.Text:='';
        EditPassword.Text:='';

        // String grids
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

        // Setup headers
        sgListSection.Cols[0].Text:='Lp';
        sgListSection.Cols[1].Text:='Sections';
        sgListValue.Cols[0].Text  :='Lp';
        sgListValue.Cols[1].Text  :='Key';
        sgListValue.Cols[2].Text  :='Value';

        // Credentials
        btnPassUpdate.Enabled:=True;
        EditCurrentPassword.Enabled:=True;
        EditNewPassword.Enabled:=True;
        EditNewPasswordConfirmation.Enabled:=True;

        // String grids
        sgListSection.Enabled:=True;
        sgListValue.Enabled:=True;
        sgListSectionClick(self);
        sgListSection.Row:=1;
        sgListValue.Row:=1;
        sgListSection.Visible:=True;
        sgListValue.Visible:=True;

        // Transparency off
        imgOFF.Visible:=False;

        btnUnlock.Caption:='Lock';
        EditPassword.SetFocus();

    end;

end;


procedure TMainForm.SwitchTimers(State: TAppTimers);
begin

    case State of

        TurnedOn:
        begin
            InvoiceScanTimer.Enabled:=True;
            FollowupPopup.Enabled   :=True;
            OILoader.Enabled        :=True;
        end;

        TurnedOff:
        begin
            InvoiceScanTimer.Enabled:=False;
            FollowupPopup.Enabled   :=False;
            OILoader.Enabled        :=False;
        end;

    end;

end;


procedure TMainForm.ClearAgeSummary();
begin

    // ------------------------
    // Clear private variables.
    // ------------------------

    CustAll    :=0;
    ANotDue    :=0;
    ARange1    :=0;
    ARange2    :=0;
    ARange3    :=0;
    ARange4    :=0;
    ARange5    :=0;
    ARange6    :=0;
    Balance    :=0;
    Limits     :=0;
    Exceeders  :=0;
    TotalExceed:=0;
    RCA        :=0;
    RCB        :=0;
    RCC        :=0;
    RCAcount   :=0;
    RCBcount   :=0;
    RCCcount   :=0;

    // ---------------------------
    // Clear formatted aging data.
    // ---------------------------

    MainForm.tcCOCODE1.Caption   :='n/a';
    MainForm.tcCOCODE2.Caption   :='n/a';
    MainForm.tcCOCODE3.Caption   :='n/a';
    MainForm.tcCOCODE4.Caption   :='n/a';
    MainForm.tcCURRENCY.Caption  :='n/a';
    MainForm.tcTOTAL.Caption     :='0';
    MainForm.valND.Caption       :='0';
    MainForm.valR1.Caption       :='0';
    MainForm.valR2.Caption       :='0';
    MainForm.valR3.Caption       :='0';
    MainForm.valR4.Caption       :='0';
    MainForm.valR5.Caption       :='0';
    MainForm.valR6.Caption       :='0';
    MainForm.custRISKA.Caption   :='0';
    MainForm.custRISKB.Caption   :='0';
    MainForm.custRISKC.Caption   :='0';
    MainForm.valTAMT.Caption     :='0';
    MainForm.procND.Caption      :='0';
    MainForm.procR1.Caption      :='0';
    MainForm.procR2.Caption      :='0';
    MainForm.procR3.Caption      :='0';
    MainForm.procR4.Caption      :='0';
    MainForm.procR5.Caption      :='0';
    MainForm.procR6.Caption      :='0';
    MainForm.valEXCEEDERS.Caption:='0';
    MainForm.valTEXCEES.Caption  :='0';
    MainForm.valTLIMITS.Caption  :='0';
    MainForm.valTND.Caption      :='0';
    MainForm.valPASTDUE.Caption  :='0';
    MainForm.valDEFAULTED.Caption:='0';

end;


procedure TMainForm.UpdateAgeSummary();
begin

    MainForm.tcTOTAL.Caption:=IntToStr(CustAll);

    // --------------------------
    // Trade receivables summary.
    // --------------------------

    MainForm.valND.Caption  :=FormatFloat('#,##0.00', ANotDue);
    MainForm.valR1.Caption  :=FormatFloat('#,##0.00', ARange1);
    MainForm.valR2.Caption  :=FormatFloat('#,##0.00', ARange2);
    MainForm.valR3.Caption  :=FormatFloat('#,##0.00', ARange3);
    MainForm.valR4.Caption  :=FormatFloat('#,##0.00', ARange4);
    MainForm.valR5.Caption  :=FormatFloat('#,##0.00', ARange5);
    MainForm.valR6.Caption  :=FormatFloat('#,##0.00', ARange6);
    MainForm.valTAMT.Caption:=FormatFloat('#,##0.00', Balance);

    if not (Balance = 0) then
    begin
        MainForm.procND.Caption  :=FormatFloat('0.00', ( (ANotDue / Balance) * 100 )) + '%';
        MainForm.procR1.Caption  :=FormatFloat('0.00', ( (ARange1 / Balance) * 100 )) + '%';
        MainForm.procR2.Caption  :=FormatFloat('0.00', ( (ARange2 / Balance) * 100 )) + '%';
        MainForm.procR3.Caption  :=FormatFloat('0.00', ( (ARange3 / Balance) * 100 )) + '%';
        MainForm.procR4.Caption  :=FormatFloat('0.00', ( (ARange4 / Balance) * 100 )) + '%';
        MainForm.procR5.Caption  :=FormatFloat('0.00', ( (ARange5 / Balance) * 100 )) + '%';
        MainForm.procR6.Caption  :=FormatFloat('0.00', ( (ARange6 / Balance) * 100 )) + '%';
        MainForm.procTAMT.Caption:=FormatFloat('0.00', ( ( (ANotDue / Balance) +
                                                           (ARange1 / Balance) +
                                                           (ARange2 / Balance) +
                                                           (ARange3 / Balance) +
                                                           (ARange4 / Balance) +
                                                           (ARange5 / Balance) +
                                                           (ARange6 / Balance) ) * 100 ) ) + '%';
    end;

    // --------------------
    // Update risk classes.
    // --------------------

    MainForm.valRISKA.Caption :=FormatFloat('#,##0.00', RCA);
    MainForm.valRISKB.Caption :=FormatFloat('#,##0.00', RCB);
    MainForm.valRISKC.Caption :=FormatFloat('#,##0.00', RCC);
    MainForm.custRISKA.Caption:=IntToStr(RCAcount) + ' customers';
    MainForm.custRISKB.Caption:=IntToStr(RCBcount) + ' customers';
    MainForm.custRISKC.Caption:=IntToStr(RCCcount) + ' customers';

    // ----------------------------
    // Update exceeders and ranges.
    // ----------------------------

    MainForm.valEXCEEDERS.Caption:=IntToStr(Exceeders);
    MainForm.valTEXCEES.Caption  :=FormatFloat('#,##0.00', TotalExceed);
    MainForm.valTLIMITS.Caption  :=FormatFloat('#,##0.00', Limits);
    MainForm.valTND.Caption      :=MainForm.valND.Caption;
    MainForm.valPASTDUE.Caption  :=FormatFloat('#,##0.00', (ARange1 + ARange2 + ARange3));
    MainForm.valDEFAULTED.Caption:=FormatFloat('#,##0.00', (ARange4 + ARange5 + ARange6));

end;


procedure TMainForm.ComputeAgeSummary(var Grid: TStringGrid); // make async!
begin

    for var iCNT: integer:=1 to Grid.RowCount - 1 do if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then
    begin

        // ---------------------------
        // Not due and overdue ranges.
        // ---------------------------

        ANotDue:=ANotDue + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fNotDue, 1, 1), iCNT], 0);
        ARange1:=ARange1 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange1, 1, 1), iCNT], 0);
        ARange2:=ARange2 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange2, 1, 1), iCNT], 0);
        ARange3:=ARange3 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange3, 1, 1), iCNT], 0);
        ARange4:=ARange4 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange4, 1, 1), iCNT], 0);
        ARange5:=ARange5 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange5, 1, 1), iCNT], 0);
        ARange6:=ARange6 + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fRange6, 1, 1), iCNT], 0);

        // -----------------------------
        // Total amount, ledger balance.
        // -----------------------------

        Balance:=Balance + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fTotal, 1, 1), iCNT], 0);

        // -------------------------------------------------
        // Granted limit, sum of all assigned credit limits.
        // -------------------------------------------------

        Limits:=Limits + StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCreditLimit, 1, 1), iCNT], 0);

        // ------------------
        // Compute exceeders.
        // ------------------

        if StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCreditBalance, 1, 1), iCNT], 0) < 0 then
        begin
            inc(Exceeders);
            TotalExceed:=TotalExceed + Abs(StrToFloatDef(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCreditBalance, 1, 1), iCNT], 0));
        end;

        inc(CustAll);

    end;

end;


procedure TMainForm.ComputeRiskClass(var Grid: TStringGrid);
begin

    if Balance = 0 then Exit();

    var TotalPerItem: Unity.Arrays.TADoubles;
    var ListPosition: Unity.Arrays.TAIntigers;
    var Count: double:=0;
    var Rows: integer:=0;

    RCA:=Balance * FClass_A;
    RCB:=Balance * FClass_B;
    RCC:=Balance * FClass_C;

    // Move totals and its positions into array
    for var iCNT: integer:=1 to Grid.RowCount do
    if Grid.RowHeights[iCNT] <> Grid.sgRowHidden then
    begin
        SetLength(ListPosition, Rows + 1);
        SetLength(TotalPerItem, Rows + 1);
        ListPosition[Rows]:=iCNT;
        TotalPerItem[Rows]:=StrToFloatDef((Grid.Cells[Grid.ReturnColumn(TSnapshots.fTotal, 1, 1), iCNT]), 0);
        inc(Rows);
    end;

    // Sort via total value
    THelpers.QuickSortExt(TotalPerItem, ListPosition, Low(TotalPerItem), High(TotalPerItem), False);

    // Compute and display RCA
    for var iCNT: integer:=Low(ListPosition) to High(ListPosition) do
    begin

        Count:=Count + TotalPerItem[iCNT];

        // Risk Class 'A'
        if Count <= RCA then
        begin
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fRiskClass, 1, 1), ListPosition[iCNT]]:='A';
            inc(RCAcount);
        end;

        // Risk Class 'B'
        if (Count > RCA) and (Count <= RCA + RCB) then
        begin
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fRiskClass, 1, 1), ListPosition[iCNT]]:='B';
            inc(RCBcount);
        end;

        // Risk Class 'C'
        if Count > RCA + RCB then
        begin
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fRiskClass, 1, 1), ListPosition[iCNT]]:='C';
            inc(RCCcount);
        end;

    end;

end;


procedure TMainForm.UpdateOpenItemsDetails(var Grid: TStringGrid);
begin

    // Clear grid
    Grid.ClearAll(4, 0, 0, False);

    // Get co codes from selected group (Group ID)
    MainForm.tcCOCODE1.Caption:=THelpers.GetCoCode(1, MainForm.FGroupIdSel);
    MainForm.tcCOCODE2.Caption:=THelpers.GetCoCode(2, MainForm.FGroupIdSel);
    MainForm.tcCOCODE3.Caption:=THelpers.GetCoCode(3, MainForm.FGroupIdSel);
    MainForm.tcCOCODE4.Caption:=THelpers.GetCoCode(4, MainForm.FGroupIdSel);

    if MainForm.tcCOCODE1.Caption = '0' then
        MainForm.tcCOCODE1.Font.Color:=clWhite else MainForm.tcCOCODE1.Font.Color:=clBlack;

    if MainForm.tcCOCODE2.Caption = '0' then
        MainForm.tcCOCODE2.Font.Color:=clWhite else MainForm.tcCOCODE2.Font.Color:=clBlack;

    if MainForm.tcCOCODE3.Caption = '0' then
        MainForm.tcCOCODE3.Font.Color:=clWhite else MainForm.tcCOCODE3.Font.Color:=clBlack;

    if MainForm.tcCOCODE4.Caption = '0' then
        MainForm.tcCOCODE4.Font.Color:=clWhite else MainForm.tcCOCODE4.Font.Color:=clBlack;

    Grid.Cells[0, 0]:=MainForm.tcCOCODE1.Caption; THelpers.FindCoData(0, MainForm.sgCompanyData, MainForm.sgCoCodes);
    Grid.Cells[1, 0]:=MainForm.tcCOCODE2.Caption; THelpers.FindCoData(1, MainForm.sgCompanyData, MainForm.sgCoCodes);
    Grid.Cells[2, 0]:=MainForm.tcCOCODE3.Caption; THelpers.FindCoData(2, MainForm.sgCompanyData, MainForm.sgCoCodes);
    Grid.Cells[3, 0]:=MainForm.tcCOCODE4.Caption; THelpers.FindCoData(3, MainForm.sgCompanyData, MainForm.sgCoCodes);

    // There should be always the same currency code for all stacked companies snapshots.
    var SL: TStringList:=TStringList.Create;
    try
        SL.Clear();
        SL.Sorted:=True;
        SL.Duplicates:=dupIgnore;

        for var iCNT: integer:=1 to MainForm.sgAgeView.RowCount - 1 do
            SL.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fLedgerIso, 1, 1), iCNT]);

    finally
        MainForm.tcCURRENCY.Caption:=SL.Text;
        SL.Free();
    end;

end;


procedure TMainForm.LoadColumnWidth(var Grid: TStringGrid);
begin

    var Settings: ISettings:=TSettings.Create();
    for var iCNT: integer:=0 to Grid.ColCount - 1 do
        {OutputDebugString(PChar(Settings.GetStringValue('COLUMNWIDTH', Settings.FindSettingsKey('COLUMNWIDTH', iCNT), '')));}
        Grid.ColWidths[iCNT]:=Settings.GetStringValue('COLUMNWIDTH', Settings.FindSettingsKey('COLUMNWIDTH', iCNT), '').ToInteger();

end;


procedure TMainForm.MapGroup3(var Grid: TStringGrid; var Source: TStringGrid);
begin

    for var iCNT: integer:=1 to Grid.RowCount - 1 do
        for var jCNT: integer:=1 to Source.RowCount - 1 do
        if
        (
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fGroup3, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TGroup3.ErpCode, 1, 1), jCNT]
        )
        and
        (
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TGroup3.Entity, 1, 1), jCNT]
        )
        then
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fGroup3, 1, 1), iCNT]:=Source.Cells[Source.ReturnColumn(TGroup3.Description, 1, 1), jCNT]

end;


procedure TMainForm.MapTable1(var Grid: TStringGrid; var Source: TStringGrid);
begin

    for var iCNT: integer:=1 to Grid.RowCount - 1 do
        for var jCNT: integer:=1 to Source.RowCount - 1 do
        if
        (
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fPersonResponsible, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TPersonResponsible.Id, 1, 1), jCNT]
        )
        and
        (
            THelpers.ConvertCoCode(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT], 'F', 0) = Source.Cells[Source.ReturnColumn(TPersonResponsible.SourceDBName, 1, 1), jCNT]
        )
        then
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fPersonResponsible, 1, 1), iCNT]:=Source.Cells[Source.ReturnColumn(TPersonResponsible.ErpCode, 1, 1), jCNT]

end;


procedure TMainForm.MapTable2(var Grid: TStringGrid; var Source: TStringGrid);
begin

    for var iCNT: integer:=1 to Grid.RowCount - 1 do
        for var jCNT: integer:=1 to Source.RowCount - 1 do
        if
        (
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fSalesResponsible, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TSalesResponsible.Id, 1, 1), jCNT]
        )
        and
        (
            THelpers.ConvertCoCode(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT], 'F', 0) = Source.Cells[Source.ReturnColumn(TSalesResponsible.SourceDBName, 1, 1), jCNT]
        )
        then
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fSalesResponsible, 1, 1), iCNT]:=Source.Cells[Source.ReturnColumn(TSalesResponsible.ErpCode, 1, 1), jCNT]

end;


procedure TMainForm.MapTable3(var Grid: TStringGrid; var Source: TStringGrid);
begin

    for var iCNT: integer:=1 to Grid.RowCount - 1 do
        for var jCNT: integer:=1 to Source.RowCount - 1 do
        if
        (
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fAccountType, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TAccountType.Id, 1, 1), jCNT]
        )
        and
        (
            THelpers.ConvertCoCode(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT], 'F', 0) = Source.Cells[Source.ReturnColumn(TAccountType.SourceDBName, 1, 1), jCNT]
        )
        then
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fAccountType, 1, 1), iCNT]:=Source.Cells[Source.ReturnColumn(TAccountType.ErpCode, 1, 1), jCNT]

end;


procedure TMainForm.MapTable4(var Grid: TStringGrid; var Source: TStringGrid);
begin

    for var iCNT: integer:=1 to Grid.RowCount - 1 do
        for var jCNT: integer:=1 to Source.RowCount - 1 do
        if
        (
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fCustomerGroup, 1, 1), iCNT] = Source.Cells[Source.ReturnColumn(TCustomerGroup.Id, 1, 1), jCNT]
        )
        and
        (
            THelpers.ConvertCoCode(Grid.Cells[Grid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT], 'F', 0) = Source.Cells[Source.ReturnColumn(TCustomerGroup.SourceDBName, 1, 1), jCNT]
        )
        then
            Grid.Cells[Grid.ReturnColumn(TSnapshots.fCustomerGroup, 1, 1), iCNT]:=Source.Cells[Source.ReturnColumn(TCustomerGroup.ErpCode, 1, 1), jCNT]

end;


function TMainForm.GetData(Code: string; Table: string; Entity: string): string; // split into awaited functions
begin

    Result:=TUnknown.Unassigned;
    var Field: string;

    if (Code = ' ') or (Code = '') or (Entity = ' ') or (Entity = '') then Exit;

    var DataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        try

            // -------------
            // Group3 table.
            // -------------

            if Table = TGroup3.Group3 then
            begin
                Field:=TGroup3.Description;
                DataTables.CleanUp;
                DataTables.Columns.Add(Field);
                DataTables.CustFilter:=TSql.WHERE + TGroup3.ErpCode + TSql.EQUAL + QuotedStr(Code) + TSql._AND + TGroup3.Entity + TSql.EQUAL + QuotedStr(Entity);
                DataTables.OpenTable(Table);
            end;

            // ----------------------------------------------
            // Paid info table (indepenent from entity code).
            // ----------------------------------------------

            if Table = TPaidinfo.Paidinfo then
            begin
                Field:=TPaidinfo.Description;
                DataTables.CleanUp;
                DataTables.Columns.Add(Field);
                DataTables.CustFilter:=TSql.WHERE + TPaidInfo.ErpCode + TSql.EQUAL + QuotedStr(Code);
                DataTables.OpenTable(Table);
            end;

            // --------------------
            // Payment terms table.
            // --------------------

            if Table = TPaymentTerms.PaymentTerms then
            begin
                Field:=TPaymentTerms.Description;
                DataTables.CleanUp;
                DataTables.Columns.Add(Field);
                DataTables.CustFilter:=TSql.WHERE + TPaymentTerms.ErpCode + TSql.EQUAL + QuotedStr(Code) + TSql._AND + TPaymentTerms.Entity + TSql.EQUAL + QuotedStr(Entity);
                DataTables.OpenTable(Table);
            end;

            // --------------
            // Persons table.
            // --------------

            if Table = TPerson.Person then
            begin
                Field:=TPerson.Description;
                DataTables.CleanUp;
                DataTables.Columns.Add(Field);
                DataTables.CustFilter:=TSql.WHERE + TPerson.ErpCode + TSql.EQUAL + QuotedStr(Code) + TSql._AND + TPerson.Entity + TSql.EQUAL + QuotedStr(Entity);
                DataTables.OpenTable(Table);
            end;

            if DataTables.DataSet.RecordCount = 1 then
                Result:=THelpers.OleGetStr(DataTables.DataSet.Fields[Field].Value);

        except
            Result:='';
        end;

    finally
        DataTables.Free();
    end;

end;


procedure TMainForm.ClearOpenItemsSummary();
begin
    MainForm.FOSAmount          :=0;
    MainForm.tcOpenItems.Caption:='0';
    MainForm.tcOverdue.Caption  :='0';
    MainForm.tcInvoices.Caption :='0';
    MainForm.tcOSAmt.Caption    :='0';
    MainForm.tcUNamt.Caption    :='0';
    MainForm.tcOvdAmt.Caption   :='0';
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

    MyPages.ActivePage:=TabSheet;

end;


procedure TMainForm.ResetTabsheetButtons();
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


procedure TMainForm.SetPanelBorders();
begin
    AppHeader.PanelBorders            (clWhite, $00E3B268, clWhite,   clWhite,   clWhite);
    PanelAgeView.PanelBorders         (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelOpenItems.PanelBorders       (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelAddressBook.PanelBorders     (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelInvoiceTracker.PanelBorders  (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelCoCodes.PanelBorders         (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelControlStatus.PanelBorders   (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelPaidInfo.PanelBorders        (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelPerson.PanelBorders          (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelPmtTerms.PanelBorders        (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelGroup3.PanelBorders          (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSettingsSections.PanelBorders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSettingsValues.PanelBorders  (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSalesResp.PanelBorders       (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelPersonResp.PanelBorders      (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelCustomerGr.PanelBorders      (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelAccountType.PanelBorders     (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelFSC.PanelBorders             (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelLBU.PanelBorders             (clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelLBUGrid.PanelBorders         (clWhite, $00F1F0EE, $00F1F0EE, $00F1F0EE, $00F1F0EE);
    PanelFSCGrid.PanelBorders         (clWhite, $00F1F0EE, $00F1F0EE, $00F1F0EE, $00F1F0EE);
    PanelFscComment.PanelBorders      (clWhite, $00F1F0EE, $00F1F0EE, $00F1F0EE, $00F1F0EE);
    PanelLbuComment.PanelBorders      (clWhite, $00F1F0EE, $00F1F0EE, $00F1F0EE, $00F1F0EE);
    PanelFscDetails.PanelBorders      (clWhite, $00F1F0EE, $00F1F0EE, $00F1F0EE, $00F1F0EE);
    PanelLbuDetails.PanelBorders      (clWhite, $00F1F0EE, $00F1F0EE, $00F1F0EE, $00F1F0EE);
end;


procedure TMainForm.SetGridColumnWidths();
begin
    sgOpenItems.SetColWidth     (10, 20, 400);
    sgAddressBook.SetColWidth   (10, 20, 400);
    {sgListValue.SetColWidth     (25, 20, 400);}
    {sgListSection.SetColWidth   (25, 20, 400);}
    sgInvoiceTracker.SetColWidth(10, 20, 400);
    sgCoCodes.SetColWidth       (10, 30, 400);
    sgControlStatus.SetColWidth (10, 30, 400);
    sgPaidInfo.SetColWidth      (10, 30, 400);
    sgPerson.SetColWidth        (10, 30, 400);
    sgGroup3.SetColWidth        (10, 30, 400);
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
    sgPerson.SetRowHeight        (sgPerson.sgRowHeight,         25);
    sgGroup3.SetRowHeight        (sgGroup3.sgRowHeight,         25);
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
    // ---------------------------------------------------------------------------

    Action_QuickReporting.Bitmap.Transparent:=True;
    Action_QuickReporting.Bitmap.TransparentColor:=clWhite;
    btnLbuUpdate.Glyph.Transparent:=True;
    btnLbuUpdate.Glyph.TransparentColor:=clWhite;

end;


function TMainForm.AddressBookExclusion(): boolean;
begin

    // ----------------------------------------------------------------------
    // Indicates editable columns. Use it to examin if user should be able to
    // edit selected cell in StrigGrid component.
    // ----------------------------------------------------------------------

    if
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.Emails, 1, 1)
        )
    or
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.PhoneNumbers, 1, 1)
        )
    or
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.Contact, 1, 1)
        )
    or
        (
            sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.Estatements, 1, 1)
        )
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

        var Settings:  ISettings:=TSettings.Create;
        var LastTopPos :=Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_TOP', 24);
        var LastLeftPos:=Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_LEFT', 24);
        var LastWidth  :=Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_WIDTH', 1024);
        var LastHeight :=Settings.GetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_HEIGHT', 1024);

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


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TMainForm.FormCreate(Sender: TObject);
begin
    FAllowClose:=False;
    InitializeScreenSettings;
    SetActiveTabsheet(TabSheet9);
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
    // Execurte before window is show
    if StartupForm.IsAppInitialized then SetupMainWnd();
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin
    // Execute after window is shown
    if StartupForm.IsAppInitialized then StartMainWnd();
end;


procedure TMainForm.ChromiumWindowAfterCreated(Sender: TObject);
begin

    var Settings: ISettings:=TSettings.Create();
    var URL: string:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'START_PAGE', 'about:blank');

    try
        ChromiumWindow.LoadURL(WideString(URL));
    except
        on E: exception do
            ThreadFileLog.Log('[Chromium] Cannot load URL: ' + URL + '. The error has been thrown: ' + E.Message);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------- CLOSE APPLICATION //


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

    // -----------------------------------------------------------------------------------------------------------------------------------
    // Execute when application receives windows message on shutting down the system; or user press key combination of <ALT> + <Y>; or
    // simply clicks close button on application caption bar. Standard behaviour of application on close button is changed to minimisation
    // of the application to system tray (removes icon from taskbar).
    // -----------------------------------------------------------------------------------------------------------------------------------

    case FAllowClose of

        // ------------------------------------------------
        // Go minimize and hide from taskbar, do not close.
        // ------------------------------------------------

        False:
        begin
            CanClose:=False;
            ShowWindow(Handle, SW_MINIMIZE);
            Hide();
        end;

        // ---------------------
        // Shutdown application.
        // ---------------------

        True:
        begin

            SwitchTimers(TAppTimers.TurnedOff);
            ChromiumWindow.CloseBrowser(True);

            var UserLogs: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                try

                    // ----------------------------------
                    // Update user event log in database.
                    // ----------------------------------

                    var Today: string:=FormatDateTime(TDateTimeFormats.DateTimeFormat, Now);

                    UserLogs.Columns.Add(TUnityEventLogs.UserAlias);
                    UserLogs.Columns.Add(TUnityEventLogs.DateTimeStamp);
                    UserLogs.Columns.Add(TUnityEventLogs.AppEventLog);
                    UserLogs.Columns.Add(TUnityEventLogs.AppName);
                    UserLogs.Values.Add(SessionService.SessionUser.ToUpper);
                    UserLogs.Values.Add(Today);
                    UserLogs.Values.Add(TCore.LoadFileToStr(ThreadFileLog.LogFileName));
                    UserLogs.Values.Add('Unity Cadiz.');
                    UserLogs.InsertInto(TUnityEventLogs.UnityEventLogs, True);

                    // -------------------------------------
                    // Save last window position and layout.
                    // -------------------------------------

                    if sgAgeView.RowCount > 2 then
                        sgAgeView.SaveLayout(
                            TConfigSections.ColumnWidthName,
                            TConfigSections.ColumnOrderName,
                            TConfigSections.ColumnNames,
                            TConfigSections.ColumnPrefix
                        );

                    var Settings: ISettings:=TSettings.Create;
                    Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_TOP',  MainForm.Top);
                    Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_LEFT', MainForm.Left);
                    Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_WIDTH', MainForm.Width);
                    Settings.SetIntegerValue(TConfigSections.ApplicationDetails, 'WINDOW_HEIGHT', MainForm.Height);

                    case MainForm.WindowState of
                        wsNormal:    Settings.SetStringValue(TConfigSections.ApplicationDetails, 'WINDOW_STATE', 'wsNormal');
                        wsMaximized: Settings.SetStringValue(TConfigSections.ApplicationDetails, 'WINDOW_STATE', 'wsMaximized');
                        wsMinimized: Settings.SetStringValue(TConfigSections.ApplicationDetails, 'WINDOW_STATE', 'wsMinimized');
                    end;

                    Settings.Encode(TAppFiles.Configuration);

                    // --------------------------------------------
                    // Close database connection and allow to quit.
                    // --------------------------------------------

                    if Assigned(SessionService.FDbConnect) then
                    begin
                        SessionService.FDbConnect.Close();
                        FreeAndNil(SessionService.FDbConnect);
                    end;

                    FAllowClose:=False;
                    CanClose:=not FAllowClose;
                    StartupForm.Close();

                except on
                    E: Exception do
                        THelpers.MsgCall(TAppMessage.Error, 'An error occured during exiting the application. Message: ' + E.Message);

                end;

            finally
                UserLogs.Free();
            end;

        end;

    end;

end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
    {Do nothing}
end;


// -------------------------------------------------------------------------------------------------------------------------------------------------- TIMERS //


procedure TMainForm.FollowupPopupTimer(Sender: TObject);
begin

    // ------------------------------------------------------------
    // Count current follow-ups and display in notification baloon.
    // ------------------------------------------------------------

    var Sum: integer:=0;
    for var iCNT: integer:=1 to sgAgeView.RowCount - 1 do
        if
            (
                THelpers.CDate(sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1), iCNT]) = THelpers.CDate(StatBar_TXT3.Caption)
            )
        and
        (
           (
                UpperCase(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fInf7, 1, 1), iCNT]) = UpperCase(SessionService.SessionUser)
           )
            or
           (
                UpperCase(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fPersonResponsible, 1, 1), iCNT]) = UpperCase(SessionService.SessionUser)
           )
        )
        then
            Inc(Sum);

    if not (Sum = 0) then
    begin

        TrayIcon.Visible:=True;
        TrayIcon.BalloonHint:='Hello, you have ' + IntToStr(Sum) + ' follow-up dates registered for today.' + TChars.CRLF +
                              'Let''s bother some customers and collect some money money!' + TChars.CRLF;

        TrayIcon.ShowBalloonHint();

    end;

end;


procedure TMainForm.InetTimerTimer(Sender: TObject);
begin
    var Utilities: IUtilities:=TUtilities.Create();
    Utilities.CheckServerConnAsync(FIsConnected, CheckServerConn_Callback);
end;


procedure TMainForm.InvoiceScanTimerTimer(Sender: TObject);
begin
    {Do nothing}
end;


procedure TMainForm.OILoaderTimer(Sender: TObject);
begin
    ThreadFileLog.Log('Calling open items scanner...');
    var OpenItems: IOpenItems:=TOpenItems.Create();
    OpenItems.ScanOpenItemsAsync(FOpenItemsUpdate, ScanOpenItems_Callback);
end;


procedure TMainForm.CurrentTimeTimer(Sender: TObject);
begin
    StatBar_TXT4.Caption:=TimeToStr(Now);
end;


procedure TMainForm.UpTimeTimer(Sender: TObject);
begin
    var Result: TTime:=Now - FStartTime;
    StatBar_TXT5.Caption:=TimeToStr(Result);
end;


// --------------------------------------------------------------------------------------------------------------------------------------------- POPUP MENUS //


// ------------------------------------------------------------------------------------------------------------------------------------- COMMON MENU ACTIONS //


procedure TMainForm.CommonPopupMenuPopup(Sender: TObject);
begin
    {Do nothing}
end;


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

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.ExportCSV(CSVExport, '|');

end;


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

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.SelectAll;

end;


procedure TMainForm.Action_CopyToCBClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused   then sgOpenItems.CopyCutPaste(TActions.Copy);
    if sgCoCodes.Focused     then sgCoCodes.CopyCutPaste(TActions.Copy);
    if sgPaidInfo.Focused    then sgPaidInfo.CopyCutPaste(TActions.Copy);
    if sgPerson.Focused      then sgPerson.CopyCutPaste(TActions.Copy);
    if sgGroup3.Focused      then sgGroup3.CopyCutPaste(TActions.Copy);
    if sgPmtTerms.Focused    then sgPmtTerms.CopyCutPaste(TActions.Copy);
    if sgListValue.Focused   then sgListValue.CopyCutPaste(TActions.Copy);
    if sgListSection.Focused then sgListSection.CopyCutPaste(TActions.Copy);

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.CopyCutPaste(TActions.Copy);

end;


procedure TMainForm.Action_AutoColumnClick(Sender: TObject);
begin

    // String grid placed on main view
    if sgOpenItems.Focused   then sgOpenItems.SetColWidth(10, 20, 400);
    if sgCoCodes.Focused     then sgCoCodes.SetColWidth(10, 20, 400);
    if sgPaidInfo.Focused    then sgPaidInfo.SetColWidth(10, 20, 400);
    if sgPerson.Focused      then sgPerson.SetColWidth(10, 20, 400);
    if sgGroup3.Focused      then sgGroup3.SetColWidth(10, 20, 400);
    if sgPmtTerms.Focused    then sgPmtTerms.SetColWidth(10, 20, 400);
    {if sgListValue.Focused   then sgListValue.SetColWidth(25, 20, 400);}
    {if sgListSection.Focused then sgListSection.SetColWidth(25, 20, 400);}

    // String grid placed on action view
    if ActionsForm.OpenItemsGrid.Focused then ActionsForm.OpenItemsGrid.SetColWidth(10, 20, 400);

end;


procedure TMainForm.Action_TurnRowHighlightClick(Sender: TObject);
begin
    THelpers.TurnRowHighlight(ActionsForm.OpenItemsGrid, Action_TurnRowHighlight);
end;


// --------------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK MENU //


procedure TMainForm.BookPopupPopup(Sender: TObject);
begin

    Action_ShowMyEntries.Caption:='Show ' + UpperCase(SessionService.SessionUser) + ' entries';

    // Check if user select a range (We allow to delete only one line at the time)
    if (sgAddressBook.Selection.Bottom - sgAddressBook.Selection.Top) > 0 then
        Action_DelRow.Enabled:=False
    else
        Action_DelRow.Enabled:=True;

end;



procedure TMainForm.Action_CutClick(Sender: TObject);
begin

    // ----------------------------------------------------------------------------------
    // Allow to cut/paste the data from cell(s) if selected column is marked for editing.
    // ----------------------------------------------------------------------------------

    if AddressBookExclusion then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'This column is locked for editing.');
        Exit();
    end;

    sgAddressBook.CopyCutPaste(TActions.Cut);
    sgAddressBook.RecordRowsAffected();

end;


procedure TMainForm.Action_PasteClick(Sender: TObject);
begin

    if AddressBookExclusion then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'This column is locked for editing.');
        Exit();
    end;

    sgAddressBook.CopyCutPaste(TActions.Paste);
    sgAddressBook.RecordRowsAffected();

end;


procedure TMainForm.Action_CopyClick(Sender: TObject);
begin
    sgAddressBook.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.Action_DelRowClick(Sender: TObject); // make it async!
begin

    if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to delete this customer?' + TChars.CRLF + 'This operation cannot be reverted.') = IDNO
        then Exit();

    var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        DataTables.DeleteRecord(DbModel.TAddressBook.AddressBook, DbModel.TAddressBook.Scuid, DataTables.CleanStr(sgAddressBook.Cells[2, sgAddressBook.Row], False), True);

        if DataTables.RowsAffected > 0 then
        begin
            sgAddressBook.DeleteRowFrom(1, 1)
        end
        else
        begin
            ThreadFileLog.Log('Cannot delete selected row (rows affected: ' + IntToStr(DataTables.RowsAffected) + ').');
            THelpers.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot delete selected row. Please contact IT support.', Self);
        end;

    finally
        DataTables.Free();
    end;

end;


procedure TMainForm.Action_SearchBookClick(Sender: TObject);
begin
    THelpers.WndCall(SqlSearchForm, TWindowState.Modeless);
end;


procedure TMainForm.Action_ShowAsIsClick(Sender: TObject);
begin

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Processing, MainForm);
    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);

    var AddressBook: IAddressBook:=TAddressBook.Create();
    AddressBook.OpenAddressBookAsync('', OpenAddressBook_Callback);

end;


procedure TMainForm.Action_ShowMyEntriesClick(Sender: TObject);
begin

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Processing, MainForm);
    THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);

    var AddressBook: IAddressBook:=TAddressBook.Create();
    AddressBook.OpenAddressBookAsync(SessionService.SessionUser, OpenAddressBook_Callback);

end;


procedure TMainForm.Action_ColumnWidthClick(Sender: TObject);
begin
    sgAddressBook.SetColWidth(40, 10, 400);
end;


// ---------------------------------------------------------------------------------------------------------------------------- MAIN FORM MENU (SYSTEM TRAY) //


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
    THelpers.WndCall(FeedbackForm, TWindowState.Modal);
end;


procedure TMainForm.Action_HelpClick(Sender: TObject);
begin
    {Do nothing}
end;


procedure TMainForm.Action_AboutClick(Sender: TObject);
begin
    THelpers.WndCall(AboutForm, TWindowState.Modal);
end;


procedure TMainForm.Action_CloseClick(Sender: TObject);
begin
    {Do nonthing}
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ AGE VIEW //


procedure TMainForm.AgeViewPopupPopup(Sender: TObject);
begin

    // Only admins and rw users can use addressbook and invoice tracker
    if FAccessLevel = TUserAccess.ReadOnly then Exit();

    // Enable or disable filter removal
    if FilterForm.InUse then
        Action_RemoveFilters.Enabled:=True
    else
        Action_RemoveFilters.Enabled:=False;

end;


procedure TMainForm.Action_LyncCallClick(Sender: TObject);
begin

    if FIsConnected then
    begin

        if MainForm.StatBar_TXT1.Caption = TStatusBar.Ready then
            THelpers.WndCall(ActionsForm, TWindowState.Modal)
        else
            THelpers.MsgCall(TAppMessage.Warn, 'Wait until "Ready" status and try again.');

    end
    else
    THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');

end;


procedure TMainForm.Action_TrackerClick(Sender: TObject);

    function GetSCUID(position: integer): string;
    begin
        var CustNumber: string:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), position];
        var CoCode: string:=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), position];
        Result:=CustNumber + THelpers.ConvertCoCode(CoCode, 'F', 3);
    end;

begin

    var Item: TListItem;
    if FIsConnected then
    begin

        TrackerForm.CustomerList.Clear();

        if (sgAgeView.Selection.Top - sgAgeView.Selection.Bottom) = 0 then
        begin

            // One customer
            Item:=TrackerForm.CustomerList.Items.Add;
            Item.Caption:=IntToStr(sgAgeView.Row);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(GetSCUID(sgAgeView.Row));
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerName, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add('Not set');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add('Not set');
            Item.SubItems.Add('Not set');
            Item.SubItems.Add('Not set');
            Item.SubItems.Add('Not set');
            Item.SubItems.Add('Not set');
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fAgent, 1, 1), sgAgeView.Row]);

        end
        else
        begin

            // Many customers
            for var iCNT: integer:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
            begin

                if sgAgeView.RowHeights[iCNT] <> sgAgeView.sgRowHidden then
                begin

                    Item:=TrackerForm.CustomerList.Items.Add;
                    Item.Caption:=IntToStr(iCNT);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), iCNT]);
                    Item.SubItems.Add(GetSCUID(iCNT));
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerName, 1, 1), iCNT]);
                    Item.SubItems.Add('Not set');
                    Item.SubItems.Add('Not found!');
                    Item.SubItems.Add('Not found!');
                    Item.SubItems.Add('Not set');
                    Item.SubItems.Add('Not set');
                    Item.SubItems.Add('Not set');
                    Item.SubItems.Add('Not set');
                    Item.SubItems.Add('Not set');
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT]);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fAgent, 1, 1), iCNT]);

                end;

            end;

        end;

        THelpers.WndCall(TrackerForm, TWindowState.Modal);

    end
    else
    begin
        THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    end;

end;


procedure TMainForm.Action_ViewOptionsClick(Sender: TObject);
begin
    {Do nothing}
end;


procedure TMainForm.Action_AddToBookClick(Sender: TObject);
begin

    if FIsConnected then
    begin
        var AddressBook: IAddressBook:=TAddressBook.Create();
        AddressBook.AddToAddressBookAsync(sgAgeView, AddToAddressBook_Callback);
    end
    else
    THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');

end;


procedure TMainForm.Action_MassMailerClick(Sender: TObject);
begin

    var Item: TListItem;
    if FIsConnected then
    begin

        MassMailerForm.CustomerList.Clear();

        if (sgAgeView.Selection.Top - sgAgeView.Selection.Bottom) = 0 then
        begin

            // One customer
            Item:=MassMailerForm.CustomerList.Items.Add;
            Item.Caption:=IntToStr(sgAgeView.Row);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerName, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add('No');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add('n/a');
            Item.SubItems.Add('n/a');
            Item.SubItems.Add('n/a');
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fAgent, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), sgAgeView.Row]);
            Item.SubItems.Add(
                sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), sgAgeView.Row] +
                THelpers.ConvertCoCode(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), sgAgeView.Row], 'F', 3)
            );

            Item.SubItems.Add('empty');

        end
        else
        begin

            // Many customers
            for var iCNT: integer:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
            begin

                if sgAgeView.RowHeights[iCNT] <> sgAgeView.sgRowHidden then
                begin

                    Item:=MassMailerForm.CustomerList.Items.Add();
                    Item.Caption:=IntToStr(iCNT);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerName, 1, 1), iCNT]);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), iCNT]);
                    Item.SubItems.Add('No');
                    Item.SubItems.Add('Not found!');
                    Item.SubItems.Add('Not found!');
                    Item.SubItems.Add('n/a');
                    Item.SubItems.Add('n/a');
                    Item.SubItems.Add('n/a');
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT]);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fAgent, 1, 1), iCNT]);
                    Item.SubItems.Add(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), iCNT]);
                    Item.SubItems.Add(
                        sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), iCNT] +
                        THelpers.ConvertCoCode(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT], 'F', 3)
                    );

                    Item.SubItems.Add('empty');

                end;

            end;

        end;

        THelpers.WndCall(MassMailerForm, TWindowState.Modal);

    end
    else
    begin
        THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
    end;

end;


procedure TMainForm.Action_AddFollowUpGroupClick(Sender: TObject);
begin

    Screen.Cursor:=crHourGlass;
    CalendarForm.FCalendarMode:=TCalendar.GetDate;
    THelpers.WndCall(CalendarForm, TWindowState.Modal);

    // If selected more than one customer, assign given date to selected customers
    if CalendarForm.FSelectedDate <> TDateTimeFormats.NullDate then
    begin

        for var iCNT: integer:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do

            if sgAgeView.RowHeights[iCNT] <> sgAgeView.sgRowHidden then
                CalendarForm.SetFollowUp(CalendarForm.FSelectedDate, sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), iCNT], iCNT);

            ThreadFileLog.Log('GeneralComment table with column FollowUp has been updated with ' + DateToStr(CalendarForm.FSelectedDate) + ' for multiple items.');

    end;

    Screen.Cursor:=crDefault;

end;


procedure TMainForm.Action_RemoveFollowUpsClick(Sender: TObject);
begin

    Screen.Cursor:=crHourGlass;

    for var iCNT: integer:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
    begin

        if sgAgeView.RowHeights[iCNT] <> sgAgeView.sgRowHidden then
        begin

            FGeneralCommentFields.CUID        :=sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), iCNT];
            FGeneralCommentFields.FixedComment:=TUnknown.NULL;
            FGeneralCommentFields.FollowUp    :=TChars.SPACE;
            FGeneralCommentFields.Free1       :=TUnknown.NULL;
            FGeneralCommentFields.Free2       :=TUnknown.NULL;
            FGeneralCommentFields.Free3       :=TUnknown.NULL;
            FGeneralCommentFields.EventLog    :=False;

            var Comments: IComments:=TComments.Create();
            Comments.EditGeneralComment(FGeneralCommentFields, nil);

            MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1), iCNT]:=TChars.SPACE;

        end;

    end;

    ThreadFileLog.Log('GeneralComment table with column FollowUp has been updated with removal for multiple items.');

    Screen.Cursor:=crDefault;

end;


// ------------------------------------------------------------------------------------------------------------------------------------ FILTER AGE VIEW GRID //


// Filter via INF7 (to be removed)
procedure TMainForm.Action_INF7_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fInf7;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Inf7;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via INF4 (to be removed)
procedure TMainForm.Action_INF4_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fInf4;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Inf4;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via GROUP 3 (to be removed)
procedure TMainForm.Action_Gr3_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fGroup3;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Group3;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via Sales Responsible
procedure TMainForm.Action_SalesRespClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fSalesResponsible;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.SalesResponsible;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter vi Person Responsible
procedure TMainForm.Action_PersonRespClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fPersonResponsible;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.PersonResponsible;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via Customer Group
procedure TMainForm.Action_CustomerGrpClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fCustomerGroup;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.CustomerGroup;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via Account Type
procedure TMainForm.Action_AccountTypeClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fAccountType;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.AccountType;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via FOLLOW UP
procedure TMainForm.Action_FollowUp_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneralComment.fFollowUp;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Follow;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via COCODE
procedure TMainForm.Action_CoCode_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fCoCode;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.CoCode;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via AGENT
procedure TMainForm.Action_Agent_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fAgent;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Agent;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via DIVISION
procedure TMainForm.Action_Division_FilterClick(Sender: TObject);
begin
    FilterForm.FColName  :=TSnapshots.fDivision;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Division;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via FREE 1
procedure TMainForm.Action_Free1Click(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneralComment.Free1;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Free1;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via FREE 2
procedure TMainForm.Action_Free2Click(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneralComment.Free2;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Free2;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
end;


// Filter via FREE 3
procedure TMainForm.Action_Free3Click(Sender: TObject);
begin
    FilterForm.FColName  :=TGeneralComment.Free3;
    FilterForm.FOverdue  :=TSnapshots.fOverdue;
    FilterForm.FGrid     :=MainForm.sgAgeView;
    FilterForm.FFilterNum:=TFiltering.TColumns.Free3;
    THelpers.WndCall(FilterForm, TWindowState.Modal);
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


// Filter via REMOVE ALL FILTERS
procedure TMainForm.Action_RemoveFiltersClick(Sender: TObject);
begin

    sgAgeView.Freeze(True);

    for var iCNT: integer:=1 to sgAgeView.RowCount - 1 do
        sgAgeView.RowHeights[iCNT]:=sgAgeView.sgRowHeight;

    FilterForm.FilterClearAll();

    // Re-compute aging summary
    MainForm.ComputeAgeSummary(MainForm.sgAgeView);
    MainForm.ComputeRiskClass(MainForm.sgAgeView);
    MainForm.UpdateAgeSummary;

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
    GridSearchForm.FGrid     :=MainForm.sgAgeView;
    GridSearchForm.FColName  :=TSnapshots.fCustomerName;
    GridSearchForm.FColNumber:=TSnapshots.fCustomerNumber;
    THelpers.WndCall(GridSearchForm, TWindowState.Modeless);
end;


procedure TMainForm.Action_PaymentTermClick(Sender: TObject);
begin

    THelpers.MsgCall(
        TAppMessage.Info,
        'Payment term: ' +
        MainForm.GetData(
            sgAgeView.Cells[
                sgAgeView.ReturnColumn(TSnapshots.fPaymentTerms, 1, 1),
                sgAgeView.Row
            ],
            TPaymentTerms.PaymentTerms,
            sgAgeView.Cells[
                sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1),
                sgAgeView.Row
            ]
        )
    );

end;


procedure TMainForm.Action_PersonClick(Sender: TObject);
begin

    THelpers.MsgCall(
        TAppMessage.Info,
       'Person assigned: ' +
        MainForm.GetData(
            sgAgeView.Cells[
                sgAgeView.ReturnColumn(TSnapshots.fPerson, 1, 1),
                sgAgeView.Row
            ],
            TPerson.Person,
            sgAgeView.Cells[
                sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1),
                sgAgeView.Row
            ]
        )
    );

end;


procedure TMainForm.Action_ToExceClick(Sender: TObject);
begin

    THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.ExportXLS, MainForm);

    var FileName: string;
    if MainForm.XLExport.Execute then FileName:=MainForm.XLExport.FileName else FileName:='';

    var Utilities: IUtilities:=TUtilities.Create();
    Utilities.ExcelExportAsync(MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 0], MainForm.GroupListDates.Text, FileName, ExcelExport_Callback);

end;


procedure TMainForm.Action_ExportCSVClick(Sender: TObject);
begin
    sgAgeView.ExportCSV(CSVExport, '|');
end;


procedure TMainForm.Action_HideSummaryClick(Sender: TObject);
begin

    if Action_HideSummary.Checked then
    begin
        PanelAgeView.Margins.Bottom:=12;
        Footer1.Visible:=False;
        Action_HideSummary.Checked:=False;
    end
    else
    begin
        PanelAgeView.Margins.Bottom:=0;
        Footer1.Visible:=True;
        Action_HideSummary.Checked:=True;
    end;

end;


procedure TMainForm.Action_AutoColumnSizeClick(Sender: TObject);
begin
    MainForm.sgAgeView.SetColWidth(10, 20, 400);
end;


procedure TMainForm.Action_FollowUpColorsClick(Sender: TObject);
begin
    THelpers.WndCall(ColorsForm, TWindowState.Modal);
end;


procedure TMainForm.Action_RowHighlightClick(Sender: TObject);
begin
    THelpers.TurnRowHighlight(sgAgeView, Action_RowHighlight);
end;


// ------------------------------------------------------------------------------------------------------------------------------------ INVOICE TRACKER MENU //


procedure TMainForm.Action_RemoveClick(Sender: TObject);
begin

    if not(FIsConnected) then
    begin
        THelpers.MsgCall(Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit();
    end;

    // R/W user can remove item
    if (MainForm.FAccessLevel = TUserAccess.ReadWrite) and (UpperCase(SessionService.SessionUser) = UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
        if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to remove selected customer?') = IDYES then
        begin
            var Tracker: ITracker:=TTracker.Create();
            Tracker.DeleteFromTrackerListAsync(sgInvoiceTracker.Cells[sgInvoiceTracker.ReturnColumn(TTrackerData.Cuid, 1, 1), sgInvoiceTracker.Row], DeleteFromTrackerList_Callback);
        end;

    // R/W user cannot remove other item
    if (MainForm.FAccessLevel = TUserAccess.ReadWrite) and (UpperCase(SessionService.SessionUser) <> UpperCase(sgInvoiceTracker.Cells[1, sgInvoiceTracker.Row])) then
        THelpers.MsgCall(TAppMessage.Warn, 'You cannot remove someone''s else item.');

    // Administrator can remove any item
    if (MainForm.FAccessLevel = TUserAccess.Admin) then
        if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to remove selected customer?') = IDYES then
        begin
            var Tracker: ITracker:=TTracker.Create();
            Tracker.DeleteFromTrackerListAsync(sgInvoiceTracker.Cells[sgInvoiceTracker.ReturnColumn(TTrackerData.Cuid, 1, 1), sgInvoiceTracker.Row], DeleteFromTrackerList_Callback);
        end;

    // Read only user cannot remove anything
    if (MainForm.FAccessLevel = TUserAccess.ReadOnly) then THelpers.MsgCall(TAppMessage.Warn, 'You don''t have permission to remove items.');

end;


procedure TMainForm.Action_ShowRegisteredClick(Sender: TObject);
begin
    THelpers.WndCall(InvoicesForm, TWindowState.Modal);
end;


procedure TMainForm.Action_ShowMyClick(Sender: TObject);
begin

    if FIsConnected then
    begin
        var Tracker: ITracker:=TTracker.Create();
        Tracker.RefreshInvoiceTrackerAsync(UpperCase(SessionService.SessionUser), RefreshInvoiceTracker_Callback);
    end
    else
    THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');

end;


procedure TMainForm.Action_ShowAllClick(Sender: TObject);
begin

    if FIsConnected then
    begin
        var Tracker: ITracker:=TTracker.Create();
        Tracker.RefreshInvoiceTrackerAsync(EmptyStr, RefreshInvoiceTracker_Callback);
    end
    else
    THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- TRYICON CALLS //


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


// ------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS | ENTITY GROUP //


procedure TMainForm.GroupListBoxSelect(Sender: TObject);
begin

    if not(FIsConnected) then
    begin
        THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit();
    end;

    var UserControl: TUserControl:=TUserControl.Create(SessionService.FDbConnect);
    try

        UserControl.UserName:=SessionService.SessionUser;

        if not UserControl.GetAgeDates(MainForm.FAgeDateList, MainForm.FGroupList[GroupListBox.ItemIndex, 0]) then
        begin
            THelpers.MsgCall(TAppMessage.Error, 'Cannot list age dates for selected group. Please contact IT support.');
            ThreadFileLog.Log('GetAgeDates returned false. Cannot get list of age dates for selected group (' + FGroupList[GroupListBox.ItemIndex, 0] + ').');
        end;

        GroupListDates.ListToComboBox(FAgeDateList, 0, TListSelection.Last);

    finally
        UserControl.Free();
    end;

end;


// ---------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS | TABSHEETS //


procedure TMainForm.TabSheet4Show(Sender: TObject);
begin
    var Tracker: ITracker:=TTracker.Create();
    Tracker.RefreshInvoiceTrackerAsync(EmptyStr, RefreshInvoiceTracker_Callback);
end;


procedure TMainForm.TabSheet5Show(Sender: TObject);
begin
    var Queries: IQueries:=TQueries.Create();
    Queries.UpdateQmsViewFsc(sgFSCview);
    Queries.UpdateQmsViewLbu(sgLBUview);
end;


// ---------------------------------------------------------------------------------------------------------- MAKE PAYMENT TERMS AND PAID INFO TABLES HEIGHT //



procedure TMainForm.TabSheet7Show(Sender: TObject);
begin

    // --------------------------------------------------------------------------------------
    // Setup tables height for payment term tabe and paid info table. General table is fixed.
    // --------------------------------------------------------------------------------------

    sgPmtTerms.Height:=System.Round(0.5 * sgCoCodes.Height);
    sgPaidInfo.Height:=System.Round(0.5 * sgCoCodes.Height);

end;


procedure TMainForm.TabSheet7Resize(Sender: TObject);
begin
    TabSheet7Show(self);
end;


procedure TMainForm.TabSheet8Show(Sender: TObject);
begin

    // ------------------------------------------------------------------------------------------------------
    // Force lock settings panel. This is necessary, when administrator open settings panel
    // and go to other tab without locking it. That prevents from leaving unlocked settings panel by mistake.
    // ------------------------------------------------------------------------------------------------------

    SetSettingsPanel(True);

end;


// -------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS | GRIDS //


procedure TMainForm.sgAgeViewClick(Sender: TObject);
begin
    sgAgeView.Options:=sgAgeView.Options - [goEditing];
    sgAgeView.Options:=sgAgeView.Options - [goAlwaysShowEditor];
    sgAgeView.SetFocus();
    sgAgeView.EditorMode:=False;
end;


procedure TMainForm.sgAgeViewColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin

    var Temp: TALists;
    try

        try

            var iCNT:    integer;
            var jCNT:    integer;
            var SqlRows: integer;
            var TmpRows: integer;

            // Setting up dimensions

            // -------------------------------------------------------
            // "High" method returns number of rows counting from zero
            // while "setlength" method setup array counting from one
            // therefore, we need to add one to match dimensions.
            // -------------------------------------------------------

            SqlRows:=high(sgAgeView.SqlColumns);
            Inc(SqlRows);
            SetLength(Temp, SqlRows, 2);
            TmpRows:=high(Temp);

            // -------------------------------
            // Copy SQL array to temp array.
            // Note: Do not use "copy" method.
            // -------------------------------

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
                THelpers.MsgCall(TAppMessage.Warn, 'Unexpected error has occured. Description: ' + E.Message + '. Please contact IT support.')
        end;

    finally

        sgAgeView.SaveLayout(TConfigSections.ColumnWidthName, TConfigSections.ColumnOrderName, TConfigSections.ColumnNames, TConfigSections.ColumnPrefix);
        Temp:=nil;

    end;

end;


procedure TMainForm.sgAgeViewDblClick(Sender: TObject);
begin
    Action_LyncCallClick(Self);
end;


procedure TMainForm.sgAddressBookDblClick(Sender: TObject);
begin
    if (sgAddressBook.Col = 1) then Exit();
    sgAddressBook.Options:=sgAddressBook.Options + [goEditing];
end;


procedure TMainForm.sgAddressBookClick(Sender: TObject);
begin
    sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
end;


procedure TMainForm.sgInvoiceTrackerDblClick(Sender: TObject);
begin
    if FIsConnected then
        THelpers.WndCall(InvoicesForm, TWindowState.Modal)
    else
        THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
end;


procedure TMainForm.sgLBUviewClick(Sender: TObject);
begin
    var Queries: IQueries:=TQueries.Create();
    Queries.ShowItemDetails(sgLBUview.Cells[sgLBUview.ReturnColumn(TQmsLog.Id, 1, 1), sgLBUview.Row].ToInteger, False);
end;


procedure TMainForm.sgFSCviewClick(Sender: TObject);
begin
    var Queries: IQueries:=TQueries.Create();
    Queries.ShowItemDetails(sgFSCview.Cells[sgFSCview.ReturnColumn(TQmsLog.Id, 1, 1), sgFSCview.Row].ToInteger, True);
end;


// ----------------------------------------------------------------------------------------------------------------------- CUSTOMIZE DRAWING OF STRING GRIDS //


procedure TMainForm.sgAgeViewDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    // Skip header
    if ARow = 0 then Exit;

    // Find column numbers for given column name
    var Col1:  integer:=sgAgeView.ReturnColumn(TSnapshots.fNotDue,        1, 1);
    var Col2:  integer:=sgAgeView.ReturnColumn(TSnapshots.fRange1,        1, 1);
    var Col3:  integer:=sgAgeView.ReturnColumn(TSnapshots.fRange2,        1, 1);
    var Col4:  integer:=sgAgeView.ReturnColumn(TSnapshots.fRange3,        1, 1);
    var Col5:  integer:=sgAgeView.ReturnColumn(TSnapshots.fRange4,        1, 1);
    var Col6:  integer:=sgAgeView.ReturnColumn(TSnapshots.fRange5,        1, 1);
    var Col7:  integer:=sgAgeView.ReturnColumn(TSnapshots.fRange6,        1, 1);
    var Col8:  integer:=sgAgeView.ReturnColumn(TSnapshots.fOverdue,       1, 1);
    var Col9:  integer:=sgAgeView.ReturnColumn(TSnapshots.fTotal,         1, 1);
    var Col10: integer:=sgAgeView.ReturnColumn(TSnapshots.fCreditLimit,   1, 1);
    var Col11: integer:=sgAgeView.ReturnColumn(TSnapshots.fCreditBalance, 1, 1);
    var Col12: integer:=sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1);
    var Col13: integer:=sgAgeView.ReturnColumn(TSnapshots.fCuid,          1, 1);
    var Col14: integer:=sgAgeView.ReturnColumn(TSnapshots.fCustomerName,  1, 1);
    var Col15: integer:=sgAgeView.ReturnColumn(TSnapshots.fRiskClass,     1, 1);
    var Col16: integer:=sgInvoiceTracker.ReturnColumn(TTrackerData.Cuid,  1, 1);

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

            var Settings: ISettings:=TSettings.Create();

            // Future days
            if (ACol = Col12) and (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) > THelpers.CDate(StatBar_TXT3.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Settings.FutureBColor;
                sgAgeView.Canvas.Font.Color :=Settings.FutureFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

            // Today
            if (ACol = Col12) and (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) = THelpers.CDate(StatBar_TXT3.Caption)) then
            begin
                sgAgeView.Canvas.Brush.Color:=Settings.TodayBColor;
                sgAgeView.Canvas.Font.Color :=Settings.TodayFColor;
                sgAgeView.Canvas.FillRect(Rect);
                sgAgeView.Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, sgAgeView.Cells[ACol, ARow]);
            end;

            // Past days
            if (ACol = Col12) and (THelpers.CDate(sgAgeView.Cells[ACol, ARow]) < THelpers.CDate(StatBar_TXT3.Caption)) then
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

        /// <remarks>
        /// Mark customers with picture, if it is registered on Invoice Tracker list.
        /// We loop through loaded list on another string grid component. Therefore,
        /// changes in database will not impact age view as long as Tracker List is
        /// not refreshed.
        /// </remarks>

        if ACol = Col14 then
        begin

            var Width: integer:=sgAgeView.ColWidths[Col14];
            var AgeViewCUID: string:=sgAgeView.Cells[Col13, ARow];

            for var iCNT: integer:=1 to sgInvoiceTracker.RowCount - 1 do
            begin
                if AgeViewCUID = sgInvoiceTracker.Cells[Col16, iCNT] then
                begin
                    sgAgeView.Canvas.Draw(Rect.Left + Width - 32, Rect.Top, FGridPicture.Picture.Graphic);
                    Break;
                end;
            end;

        end;

    end;

    /// <remarks>
    /// After all drawing (when cells are not selected) is done, change font only for numeric values. This shoud be executed always last.
    /// </remarks>

    if (ACol = Col1)  or (ACol = Col2) or (ACol = Col3) or
       (ACol = Col4)  or (ACol = Col5) or (ACol = Col6) or
       (ACol = Col7)  or (ACol = Col8) or (ACol = Col9) or
       (ACol = Col10) or (ACol = Col11)
    then
    begin
        if gdSelected in State then sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else sgAgeView.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;

procedure TMainForm.sgOpenItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    if ARow = 0 then Exit();

    var Col1: integer:=sgOpenItems.ReturnColumn(DbModel.TOpenitems.OpenCurAm,1, 1);
    var Col2: integer:=sgOpenItems.ReturnColumn(DbModel.TOpenitems.OpenAm,   1, 1);
    var Col3: integer:=sgOpenItems.ReturnColumn(DbModel.TOpenitems.CurAm,    1, 1);
    var Col4: integer:=sgOpenItems.ReturnColumn(DbModel.TOpenitems.Am,       1, 1);
    var Col5: integer:=sgOpenItems.ReturnColumn(DbModel.TOpenitems.PmtStat,  1, 1);

    // Selection
    MainForm.sgOpenItems.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);

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
    begin
        if gdSelected in State then sgOpenItems.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else sgOpenItems.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------- STRING GRID ROW SELECTION //


procedure TMainForm.sgCompanyDataDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgCompanyData.DrawSelected(ARow, ACol, State, Rect, clWhite, clCream, clBlack, clCream, False);
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


procedure TMainForm.sgPersonDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgPerson.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TMainForm.sgGroup3DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    sgGroup3.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);
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


// ------------------------------------------------------------------------------------------------------------------------ SETTING PANEL STRING GRID EVENTS //


procedure TMainForm.sgListSectionSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin

    CanSelect:=True;

    var Keys:   TStringList:=TStringList.Create();
    var Values: TStringList:=TStringList.Create();

    var Settings: ISettings:=TSettings.Create;
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
        {sgListValue.SetColWidth(25, 30, 400);}
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // -------------------------------
    // Turn off standard <ALT> + <F4>.
    // -------------------------------

    if (Key=VK_F4) and (Shift=[ssALT]) then Key:=0;

    // ----------------------------------------
    // Bind close application with <ALT> + <Y>.
    // ----------------------------------------

    if (Key=89) and (Shift=[ssALT]) then
    begin

        if THelpers.MsgCall(TAppMessage.Question1, 'Are you sure you want to exit the Unity?') = IDOK then
        begin
            FAllowClose:=True;
            MainForm.Close();
        end;

    end;

end;



procedure TMainForm.sgCompanyDataKeyPress(Sender: TObject; var Key: Char);
begin

    // ------------------------------------------------------------------------------------
    // Lock rows for editing. We autofill rows 1..3 so user is prevented from manipulation.
    // Additionally, we allow only digits for CoCode cell.
    // ------------------------------------------------------------------------------------

    if (sgCompanyData.Row > 0) and (sgCompanyData.Row < 4) then
        Key:=#0;

    if (sgCompanyData.Row = 0) and (not (CharInSet(Key, ['0'..'9', #8]))) then
        Key:=#0;

end;


procedure TMainForm.sgCompanyDataKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // ---------------------------------------------------------------------------------------
    // Invoke autofill. Find given company code in general table and return assigned currency,
    // agent and division info.
    // ---------------------------------------------------------------------------------------

    if sgCompanyData.Col = 0 then THelpers.FindCoData(sgCompanyData.Col, sgCompanyData, sgCoCodes);
    if sgCompanyData.Col = 1 then THelpers.FindCoData(sgCompanyData.Col, sgCompanyData, sgCoCodes);
    if sgCompanyData.Col = 2 then THelpers.FindCoData(sgCompanyData.Col, sgCompanyData, sgCoCodes);
    if sgCompanyData.Col = 3 then THelpers.FindCoData(sgCompanyData.Col, sgCompanyData, sgCoCodes);

end;


procedure TMainForm.EditGroupNameKeyPress(Sender: TObject; var Key: Char);
begin
    if (not (CharInSet(Key, ['A'..'Z', 'a'..'z', '0'..'9', '-', TChars.BACKSPACE]))) then
        Key:=#0;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ COPY PASTE CUT //


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


procedure TMainForm.sgPersonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgPerson.CopyCutPaste(TActions.Copy);
end;


procedure TMainForm.sgGroup3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then
        sgGroup3.CopyCutPaste(TActions.Copy);
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


procedure TMainForm.sgAgeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);  // REFACTOR !!!

    const
        ctFree1    = 0;
        ctFree2    = 1;
        ctFree3    = 3;
        ctFollowUp = 2;

    procedure ModifyCell(CUIDRef: integer; ColumnType: integer; Text: string);
    begin

        case ColumnType of

            ctFree1:
            begin
                FGeneralCommentFields.CUID        :=sgAgeView.Cells[CUIDRef, sgAgeView.Row];
                FGeneralCommentFields.FixedComment:=TUnknown.Null;
                FGeneralCommentFields.FollowUp    :=TUnknown.Null;
                FGeneralCommentFields.Free1       :=Text;
                FGeneralCommentFields.Free2       :=TUnknown.Null;
                FGeneralCommentFields.Free3       :=TUnknown.Null;
                FGeneralCommentFields.EventLog    :=True;
            end;

            ctFree2:
            begin
                FGeneralCommentFields.CUID        :=sgAgeView.Cells[CUIDRef, sgAgeView.Row];
                FGeneralCommentFields.FixedComment:=TUnknown.Null;
                FGeneralCommentFields.FollowUp    :=TUnknown.Null;
                FGeneralCommentFields.Free1       :=TUnknown.Null;
                FGeneralCommentFields.Free2       :=Text;
                FGeneralCommentFields.Free3       :=TUnknown.Null;
                FGeneralCommentFields.EventLog    :=True;
            end;

            ctFree3:
            begin
                FGeneralCommentFields.CUID        :=sgAgeView.Cells[CUIDRef, sgAgeView.Row];
                FGeneralCommentFields.FixedComment:=TUnknown.Null;
                FGeneralCommentFields.FollowUp    :=TUnknown.Null;
                FGeneralCommentFields.Free1       :=TUnknown.Null;
                FGeneralCommentFields.Free2       :=TUnknown.Null;
                FGeneralCommentFields.Free3       :=Text;
                FGeneralCommentFields.EventLog    :=True;
            end;

        end;

        var Comments: IComments:=TComments.Create();
        Comments.EditGeneralComment(FGeneralCommentFields, nil);

    end;

    procedure QuitEditing();
    begin
        sgAgeView.Options:=sgAgeView.Options - [goEditing];
        sgAgeView.EditorMode:=False;
    end;

    procedure AllowEditing();
    begin
        sgAgeView.Options:=sgAgeView.Options + [goEditing];
        sgAgeView.EditorMode:=True;
    end;

begin

    // <CTRL> + <C>
    if (Key = 67) and (Shift = [ssCtrl]) then
    begin
        sgAgeView.CopyCutPaste(TActions.Copy);
        sgAgeView.UpdatedRowsHolder:=nil;
        sgAgeView.RecordRowsAffected;
        Exit();
    end;

    // Allow editing only free columns
    if
        (
            sgAgeView.Col <> sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1)
        )
    and
        (
            sgAgeView.Col <> sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1)
        )
    and
        (
            sgAgeView.Col <> sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1)
        )
    then
        Exit();

    // <CTRL> + <V>
    if (Key = 86) and (Shift = [ssCtrl]) then
    begin

        sgAgeView.Freeze(True);
        Screen.Cursor:=crHourGlass;
        sgAgeView.CopyCutPaste(TActions.Paste, True);
        sgAgeView.UpdatedRowsHolder:=nil;
        sgAgeView.RecordRowsAffected;

        if sgAgeView.UpdatedRowsHolder <> nil then
        begin

            TThread.CreateAnonymousThread(procedure
            var
                iCNT: integer;
                Data: TDataTables;
            begin

                Data:=TDataTables.Create(SessionService.FDbConnect);
                try

                    Data.CmdType:=cmdText;
                    for iCNT:=Low(sgAgeView.UpdatedRowsHolder) to High(sgAgeView.UpdatedRowsHolder) do
                    begin

                        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1) then
                        begin
                            Data.StrSQL:=
                                TSql.EXECUTE +
                                    Data.UpsertFreeColumns +
                                TChars.SPACE +
                                    QuotedStr(SessionService.SessionUser.ToUpper) +
                                TChars.COMMA +
                                    QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), sgAgeView.UpdatedRowsHolder[iCNT]]) +
                                TChars.COMMA +
                                    QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1), sgAgeView.UpdatedRowsHolder[iCNT]]) +
                                TChars.COMMA +
                                    QuotedStr(TUnknown.Null) +
                                TChars.COMMA +
                                    QuotedStr(TUnknown.Null) +
                                TChars.COMMA +
                                    QuotedStr('1');
                        end;

                        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1) then
                        begin
                            Data.StrSQL:=
                                TSql.EXECUTE +
                                    Data.UpsertFreeColumns +
                                TChars.SPACE +
                                    QuotedStr(SessionService.SessionUser.ToUpper) +
                                TChars.COMMA +
                                    QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), sgAgeView.UpdatedRowsHolder[iCNT]]) +
                                TChars.COMMA +
                                    QuotedStr(TUnknown.Null) +
                                TChars.COMMA +
                                    QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1), sgAgeView.UpdatedRowsHolder[iCNT]]) +
                                TChars.COMMA +
                                    QuotedStr(TUnknown.Null) +
                                TChars.COMMA +
                                    QuotedStr('2');
                        end;

                        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1) then
                        begin
                            Data.StrSQL:=
                                TSql.EXECUTE +
                                    Data.UpsertFreeColumns +
                                TChars.SPACE +
                                    QuotedStr(SessionService.SessionUser.ToUpper) +
                                TChars.COMMA +
                                    QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), sgAgeView.UpdatedRowsHolder[iCNT]]) +
                                TChars.COMMA +
                                    QuotedStr(TUnknown.Null) +
                                TChars.COMMA +
                                    QuotedStr(TUnknown.Null) +
                                TChars.COMMA +
                                    QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1), sgAgeView.UpdatedRowsHolder[iCNT]]) +
                                TChars.COMMA +
                                    QuotedStr('3');
                        end;

                        Data.ExecSQL;

                    end;

                finally
                    Data.Free();
                    sgAgeView.Freeze(False);
                end;

            end).Start();

        end;

        Screen.Cursor:=crDefault;
        Exit();

    end;

    // Disallows "keyboard arrows" when inplace editor is enabled
    if (sgAgeView.EditorMode) and ( (Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN) ) then
    begin
        Key:=0;
        QuitEditing();
        Exit();
    end;

    // Allow editing
    if CharInSet(Char(Key), [#48..#57{A..Z}, #65..#90{a..z}, #97..#122{0..9}]) then
    begin
        Key:=0;
        AllowEditing();
        Exit();
    end;

    // Quit editing
    if Key = VK_ESCAPE then
    begin
        Key:=0;
        QuitEditing();
        Exit();
    end;

    // Quit editing and write to database
    if Key = VK_RETURN then
    begin

        Key:=0;
        QuitEditing();

        // Free 1
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1) then
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), ctFree1, sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]);

        // Free 2
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1) then
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), ctFree2, sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]);

        // Free 3
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1) then
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), ctFree3, sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]);

        Exit();

    end;

    // Delete entry from database (only Free1..3 columns)
    if Key = VK_DELETE then
    begin

        Key:=0;

        // Mass delete (all selected range)
        if (sgAgeView.Selection.Bottom - sgAgeView.Selection.Top) > 0 then
        begin

            sgAgeView.Freeze(True);
            Screen.Cursor:=crHourGlass;

            TThread.CreateAnonymousThread(procedure
            var
                iCNT: integer;
                Data: TDataTables;
            begin

                Data:=TDataTables.Create(SessionService.FDbConnect);
                try

                    Data.CmdType:=cmdText;
                    for iCNT:=sgAgeView.Selection.Top to sgAgeView.Selection.Bottom do
                    begin

                        // Skip hidden rows
                        if sgAgeView.RowHeights[iCNT] <> -1 then
                        begin

                            if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1) then
                            begin
                                Data.StrSQL:=
                                    TSql.EXECUTE +
                                        Data.UpsertFreeColumns +
                                    TChars.SPACE +
                                        QuotedStr(SessionService.SessionUser.ToUpper) +
                                    TChars.COMMA +
                                        QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), iCNT]) +
                                    TChars.COMMA +
                                        QuotedStr(String.Empty) +
                                    TChars.COMMA +
                                        QuotedStr(TUnknown.Null) +
                                    TChars.COMMA +
                                        QuotedStr(TUnknown.Null) +
                                    TChars.COMMA +
                                        QuotedStr('1');
                                sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1), iCNT]:='';
                            end;

                            if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1) then
                            begin
                                Data.StrSQL:=
                                    TSql.EXECUTE +
                                        Data.UpsertFreeColumns +
                                    TChars.SPACE +
                                        QuotedStr(SessionService.SessionUser.ToUpper) +
                                    TChars.COMMA +
                                        QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), iCNT]) +
                                    TChars.COMMA +
                                        QuotedStr(TUnknown.Null) +
                                    TChars.COMMA +
                                        QuotedStr(String.Empty) +
                                    TChars.COMMA +
                                        QuotedStr(TUnknown.Null) +
                                    TChars.COMMA +
                                        QuotedStr('2');
                                sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1), iCNT]:='';
                            end;

                            if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1) then
                            begin
                                Data.StrSQL:=
                                    TSQL.EXECUTE +
                                        Data.UpsertFreeColumns +
                                    TChars.SPACE +
                                        QuotedStr(SessionService.SessionUser.ToUpper) +
                                    TChars.COMMA +
                                        QuotedStr(sgAgeView.Cells[sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1), iCNT]) +
                                    TChars.COMMA +
                                        QuotedStr(TUnknown.Null) +
                                    TChars.COMMA +
                                        QuotedStr(TUnknown.Null) +
                                    TChars.COMMA +
                                        QuotedStr(String.Empty) +
                                    TChars.COMMA +
                                        QuotedStr('3');
                                sgAgeView.Cells[sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1), iCNT]:='';
                            end;

                        Data.ExecSQL;

                        end;

                    end;

                finally
                    Data.Free();
                    sgAgeView.Freeze(False);
                end;

            end).Start();

            Screen.Cursor:=crDefault;
            Exit();

        end;

        // Single cell detele

        // Free 1
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free1, 1, 1) then
        begin
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), ctFree1, '');
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:='';
        end;

        // Free 2
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free2, 1, 1) then
        begin
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), ctFree2, '');
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:='';
        end;

        // Free 3
        if sgAgeView.Col = sgAgeView.ReturnColumn(TGeneralComment.Free3, 1, 1) then
        begin
            ModifyCell(sgAgeView.ReturnColumn(TSnapshots.Cuid, 1, 1), ctFree3, '');
            sgAgeView.Cells[sgAgeView.Col, sgAgeView.Row]:='';
        end;

        Exit();

    end;

end;


procedure TMainForm.sgAgeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // <CTRL> + <A>
    if (Key = 65) and (Shift = [ssCtrl]) then
    begin
        sgAgeView.SelectAll();
        sgAgeView.CopyCutPaste(TActions.Copy);
        THelpers.MsgCall(TAppMessage.Info, 'The selected spreadsheet has been copied to clipboard.');
    end;

    {if ( Shift = [ssCtrl] ) and ( Key = 76 ) then btnMakeGroupAgeClick(self);}

end;


// --------------------------------------------------------------------------------------------------------------------------------------- EDIT ADDRESS BOOK //


procedure TMainForm.sgAddressBookKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // ---------------------------------------------------------------------------
    // Allow to edit specific Address Book cells. Once edited, it will be saved to
    // database if user click "Update" button.
    // ---------------------------------------------------------------------------

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
            THelpers.MsgCall(TAppMessage.Warn, 'This column is locked for editing.');
            Exit;
        end;

        if Key = VK_RETURN then
        begin
            sgAddressBook.DelEsc(TActions.Escape, sgAddressBook.Col, sgAddressBook.Row);
            sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
            sgAddressBook.SetUpdatedRow(sgAddressBook.Row);
        end;

        if Key = VK_DELETE then
        begin
            sgAddressBook.DelEsc(TActions.Delete, sgAddressBook.Col, sgAddressBook.Row);
            sgAddressBook.SetUpdatedRow(sgAddressBook.Row);
        end;

    end;

end;


procedure TMainForm.sgAddressBookKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if FAccessLevel = TUserAccess.ReadOnly then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'You don''t have permission to edit Address Book records.');
        Exit;
    end;

    if
        (
            Key = VK_F2
        )
    or
        (
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.Emails, 1, 1)
            )
        or
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.PhoneNumbers, 1, 1)
            )
        or
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.Contact, 1, 1)
            )
        or
            (
                sgAddressBook.Col = sgAddressBook.ReturnColumn(DbModel.TAddressBook.Estatements, 1, 1)
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
        sgAddressBook.DelEsc(TActions.Escape, sgAddressBook.Col, sgAddressBook.Row);
        sgAddressBook.Options:=sgAddressBook.Options - [goEditing];
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then
        sgAddressBook.CopyCutPaste(TActions.Copy);

    if (Key = 86) and (Shift = [ssCtrl]) then
    begin
        sgAddressBook.CopyCutPaste(TActions.Paste);
        sgAddressBook.RecordRowsAffected;
    end;

    if (Key = 88) and (Shift = [ssCtrl]) then
    begin
        sgAddressBook.CopyCutPaste(TActions.Cut);
        sgAddressBook.RecordRowsAffected;
    end;

end;


procedure TMainForm.sgListSectionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if Text50.Font.Style = [fsBold] then
    begin
        if (Key = 86) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(TActions.Paste);
        if (Key = 88) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(TActions.Cut);
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then sgListSection.CopyCutPaste(TActions.Copy);
    if (Key = 46) and (Text50.Font.Style = [fsBold]) then sgListSection.DelEsc(TActions.Delete, sgListSection.Col, sgListSection.Row);

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

    if Text50.Font.Style = [fsBold] then
    begin
        if (Key = 86) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(TActions.Paste);
        if (Key = 88) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(TActions.Cut);
    end;

    if (Key = 67) and (Shift = [ssCtrl]) then sgListValue.CopyCutPaste(TActions.Copy);
    if (Key = 46) and (Text50.Font.Style = [fsBold]) then sgListValue.DelEsc(TActions.Delete, sgListValue.Col, sgListValue.Row);

end;


procedure TMainForm.sgListValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then sgListValue.DelEsc(TActions.Escape, sgListValue.Col, sgListValue.Row);
end;


procedure TMainForm.EditPasswordKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.CR then btnUnlockClick(self);
end;


// --------------------------------------------------------------------------------------------------------------- MOUSE EVENTS | HOOVER EFFECT | TEXT COLOR //


procedure TMainForm.AppHeaderMouseEnter(Sender: TObject);
begin
    if AppHeader.Height = 13 then
    begin
        AppHeader.Color:=$00E3B268;
        AppHeader.PanelBorders($00E3B268, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
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


// ------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS | GRID FOCUS //


procedure TMainForm.sgAgeViewMouseEnter(Sender: TObject);
begin
    if (sgAgeView.Enabled) and (sgAgeView.Visible) then sgAgeView.SetFocus();
end;


procedure TMainForm.GroupListBoxMouseEnter(Sender: TObject);
begin
    if (GroupListBox.Enabled) and (GroupListBox.Visible) then GroupListBox.SetFocus();
end;


procedure TMainForm.GroupListDatesMouseEnter(Sender: TObject);
begin
    if (GroupListDates.Enabled) and (GroupListDates.Visible) then GroupListDates.SetFocus();
end;


procedure TMainForm.SortListBoxMouseEnter(Sender: TObject);
begin
    if (SortListBox.Enabled) and (SortListBox.Visible) then SortListBox.SetFocus();
end;


procedure TMainForm.sgOpenItemsMouseEnter(Sender: TObject);
begin
    if (sgOpenItems.Enabled) and (sgOpenItems.Visible) then sgOpenItems.SetFocus();
end;


procedure TMainForm.sgAddressBookMouseEnter(Sender: TObject);
begin
    if (sgAddressBook.Enabled) and (sgAddressBook.Visible) then sgAddressBook.SetFocus();
end;


procedure TMainForm.sgInvoiceTrackerMouseEnter(Sender: TObject);
begin
    if (sgInvoiceTracker.Enabled) and (sgInvoiceTracker.Visible) then sgInvoiceTracker.SetFocus();
end;


procedure TMainForm.sgCoCodesMouseEnter(Sender: TObject);
begin
    if (sgCoCodes.Enabled) and (sgCoCodes.Visible) then sgCoCodes.SetFocus();
end;


procedure TMainForm.sgPaidInfoMouseEnter(Sender: TObject);
begin
    if (sgPaidInfo.Enabled) and (sgPaidInfo.Visible) then sgPaidInfo.SetFocus();
end;


procedure TMainForm.sgPersonMouseEnter(Sender: TObject);
begin
    if (sgPerson.Enabled) and (sgPerson.Visible) then sgPerson.SetFocus();
end;


procedure TMainForm.sgPmtTermsMouseEnter(Sender: TObject);
begin
    if (sgPmtTerms.Enabled) and (sgPmtTerms.Visible) then sgPmtTerms.SetFocus();
end;


procedure TMainForm.sgGroup3MouseEnter(Sender: TObject);
begin
    if (sgGroup3.Enabled) and (sgGroup3.Visible) then sgGroup3.SetFocus();
end;


procedure TMainForm.sgControlStatusMouseEnter(Sender: TObject);
begin
    if (sgControlStatus.Enabled) and (sgControlStatus.Visible) then sgControlStatus.SetFocus();
end;


procedure TMainForm.sgPersonRespMouseEnter(Sender: TObject);
begin
    if (sgPersonResp.Enabled) and (sgPersonResp.Visible) then sgPersonResp.SetFocus();
end;


procedure TMainForm.sgSalesRespMouseEnter(Sender: TObject);
begin
    if (sgSalesResp.Enabled) and (sgSalesResp.Visible) then sgSalesResp.SetFocus();
end;


procedure TMainForm.sgAccountTypeMouseEnter(Sender: TObject);
begin
    if (sgAccountType.Enabled) and (sgAccountType.Visible) then sgAccountType.SetFocus();
end;


procedure TMainForm.sgCustomerGrMouseEnter(Sender: TObject);
begin
    if (sgCustomerGr.Enabled) and (sgCustomerGr.Visible) then sgCustomerGr.SetFocus();
end;


// -------------------------------------------------------------------------
// Change standard behaviour of scroll bars on all string grids.
// The mouse weel on string grid component controls row selection instead of
// moving up/down thumb on scroll bar.
// Below methods introduce mouse weel controlling scroll bar.
// -------------------------------------------------------------------------


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


// ---------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS | HOOVER EFFECT //


procedure TMainForm.btnReloadMouseEnter(Sender: TObject);
begin
    Text54L1.Font.Color:=TCommon.FontColor;
    Text54L2.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.btnReloadMouseLeave(Sender: TObject);
begin
    Text54L1.Font.Color:=clBlack;
    Text54L2.Font.Color:=clBlack;
end;


procedure TMainForm.btnMakeGroupMouseEnter(Sender: TObject);
begin
    Text83L1.Font.Color:=TCommon.FontColor;
    Text83L2.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.btnMakeGroupMouseLeave(Sender: TObject);
begin
    Text83L1.Font.Color:=clBlack;
    Text83L2.Font.Color:=clBlack;
end;


procedure TMainForm.btnOpenABMouseEnter(Sender: TObject);
begin
    btnOpenAB.Cursor:=crHandPoint;
    Text64.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.btnOpenABMouseLeave(Sender: TObject);
begin
    btnOpenAB.Cursor:=crDefault;
    Text64.Font.Color:=clBlack;
end;


procedure TMainForm.btnUpdateABMouseEnter(Sender: TObject);
begin
    btnUpdateAB.Cursor:=crHandPoint;
    Text66.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.btnUpdateABMouseLeave(Sender: TObject);
begin
    btnUpdateAB.Cursor:=crDefault;
    Text66.Font.Color:=clBlack;
end;


procedure TMainForm.btnCloseABMouseEnter(Sender: TObject);
begin
    btnCloseAB.Cursor:=crHandPoint;
    Text67.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.btnCloseABMouseLeave(Sender: TObject);
begin
    btnCloseAB.Cursor:=crDefault;
    Text67.Font.Color:=clBlack;
end;


procedure TMainForm.btnExportABMouseEnter(Sender: TObject);
begin
    btnExportAB.Cursor:=crHandPoint;
    Text69.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.btnExportABMouseLeave(Sender: TObject);
begin
    btnExportAB.Cursor:=crDefault;
    Text69.Font.Color:=clBlack;
end;


procedure TMainForm.imgKeyAddMouseEnter(Sender: TObject);
begin
    imgKeyAdd.Cursor:=crHandPoint;
    Text41.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgKeyAddMouseLeave(Sender: TObject);
begin
    imgKeyAdd.Cursor:=crDefault;
    Text41.Font.Color:=clBlack;
end;


procedure TMainForm.imgKeyRemoveMouseEnter(Sender: TObject);
begin
    imgKeyRemove.Cursor:=crHandPoint;
    Text42.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgKeyRemoveMouseLeave(Sender: TObject);
begin
    imgKeyRemove.Cursor:=crDefault;
    Text42.Font.Color:=clBlack;
end;


procedure TMainForm.imgUpdateValuesMouseEnter(Sender: TObject);
begin
    imgUpdateValues.Cursor:=crHandPoint;
    Text43.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgUpdateValuesMouseLeave(Sender: TObject);
begin
    imgUpdateValues.Cursor:=crDefault;
    Text43.Font.Color:=clBlack;
end;


procedure TMainForm.imgSectionAddMouseEnter(Sender: TObject);
begin
    imgSectionAdd.Cursor:=crHandPoint;
    Text48.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgSectionAddMouseLeave(Sender: TObject);
begin
    imgSectionAdd.Cursor:=crDefault;
    Text48.Font.Color:=clBlack;
end;


procedure TMainForm.imgSectionRemoveMouseEnter(Sender: TObject);
begin
    imgSectionRemove.Cursor:=crHandPoint;
    Text49.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgSectionRemoveMouseLeave(Sender: TObject);
begin
    imgSectionRemove.Cursor:=crDefault;
    Text49.Font.Color:=clBlack;
end;


procedure TMainForm.imgAllowEditMouseEnter(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crHandPoint;
    Text50.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgAllowEditMouseLeave(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crDefault;
    Text50.Font.Color:=clBlack;
end;


procedure TMainForm.imgEventLogMouseEnter(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crHandPoint;
    Text51.Font.Color:=TCommon.FontColor;
end;


procedure TMainForm.imgEventLogMouseLeave(Sender: TObject);
begin
    imgAllowEdit.Cursor:=crDefault;
    Text51.Font.Color:=clBlack;
end;


// ----------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS | CLICKS //


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


// ---------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS | PASSWORD EDIT FIEDS //


procedure TMainForm.btnPasswordPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EditPassword.PasswordChar:=#0;
end;


procedure TMainForm.btnPasswordPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EditPassword.PasswordChar:='*';
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //


procedure TMainForm.AppHeaderClick(Sender: TObject);
begin
    if AppHeader.Height = 13 then
    begin
        AppHeader.Height:=57;
        AppHeader.Cursor:=crDefault;
        AppHeader.Color:=clWhite;
        AppHeader.PanelBorders(clWhite, $00E3B268, clWhite, clWhite, clWhite);
    end;
end;


procedure TMainForm.imgHideBarClick(Sender: TObject);
begin
    if AppHeader.Height > 13 then
    begin
        AppHeader.Height:=13;
        AppHeader.Cursor:=crHandPoint;
        AppHeader.Color:=clWhite;
        AppHeader.PanelBorders(clWhite, clWhite, clWhite, clWhite, clWhite);
    end;
end;


procedure TMainForm.txtStartClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet9);
end;


procedure TMainForm.txtReportsClick(Sender: TObject);
begin
    ReportsForm.FSetLastSelection:=MyPages.ActivePage;
    ResetTabsheetButtons;
    txtReports.Font.Style:=[fsBold];
    txtReports.Font.Color:=$006433C9;
    THelpers.WndCall(ReportsForm, TWindowState.Modal);
end;


procedure TMainForm.txtDebtorsClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet1);
end;


procedure TMainForm.txtTrackerClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet4);
end;


procedure TMainForm.txtAddressBookClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet3);
end;


procedure TMainForm.txtOpenItemsClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet2);
end;


procedure TMainForm.txtUnidentifiedClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet6);
end;


procedure TMainForm.txtQueriesClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet5);
end;


procedure TMainForm.txtTablesClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet7);
end;


procedure TMainForm.txtSettingsClick(Sender: TObject);
begin
    SetActiveTabsheet(TabSheet8);
end;


procedure TMainForm.btnLoadAgeViewClick(Sender: TObject);
begin

    // Wait until "Ready" status
    if not (StatBar_TXT1.Caption = TStatusBar.Ready) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please wait until "Ready" status and try again.');
        Exit();
    end;

    if (not(string.IsNullOrEmpty(GroupListBox.Text))) and (not(string.IsNullOrEmpty(GroupListDates.Text))) then
    begin

        // Remember user's choice, automation will follow
        FGroupIdSel:=FGroupList[GroupListBox.ItemIndex, 0];
        FGroupNmSel:=FGroupList[GroupListBox.ItemIndex, 1];
        FAgeDateSel:=GroupListDates.Text;

        // Remove filters
        for var iCNT: integer:=1 to sgAgeView.RowCount - 1 do
            sgAgeView.RowHeights[iCNT]:=sgAgeView.sgRowHeight;

        FilterForm.FilterClearAll();

        // Turn off all timers
        MainForm.SwitchTimers(TurnedOff);

        // Load age view for selected group ID
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);
        MainForm.ClearAgeSummary();

        var Debtors: IDebtors:=TDebtors.Create();
        Debtors.ReadAgeViewAsync(CallOpenItems, TSorting.TMode.Ranges, FGroupIdSel, FAgeDateSel, ReadAgeView_Callback);

    end
    else
    THelpers.MsgCall(TAppMessage.Warn, 'Cannot load selected group.');

end;


procedure TMainForm.btnSortApplyClick(Sender: TObject);
begin

    if
        (
            SortListBox.ItemIndex <> TSorting.TMode.Ranges
        )
    and
        (
            SortListBox.ItemIndex <> TSorting.TMode.FollowUp
        )
    and
        (
            SortListBox.ItemIndex <> TSorting.TMode.Total
        )
    and
        (
            SortListBox.ItemIndex <> TSorting.TMode.Overdue
        )
    then
        Exit();

    if not (StatBar_TXT1.Caption = TStatusBar.Ready) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please wait until "Ready" status and try again.');
        Exit();
    end;

    if (not(string.IsNullOrEmpty(GroupListBox.Text))) and (not(string.IsNullOrEmpty(GroupListDates.Text))) then
    begin

        // Remember user's choice, automation will follow
        FGroupIdSel:=FGroupList[GroupListBox.ItemIndex, 0];
        FGroupNmSel:=FGroupList[GroupListBox.ItemIndex, 1];
        FAgeDateSel:=GroupListDates.Text;

        // Remove filters
        for var iCNT: integer:=1 to sgAgeView.RowCount - 1 do
            sgAgeView.RowHeights[iCNT]:=sgAgeView.sgRowHeight;

        FilterForm.FilterClearAll();

        // Turn off all timers
        SwitchTimers(TurnedOff);

        // Load age view for selected group ID
        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);
        ClearAgeSummary();
        sgAgeView.Freeze(True);

        var Debtors: IDebtors:=TDebtors.Create;
        Debtors.ReadAgeViewAsync(NullParameter, SortListBox.ItemIndex, FGroupIdSel, FAgeDateSel, ReadAgeView_Callback);

    end
    else
    THelpers.MsgCall(TAppMessage.Warn, 'Cannot load selected group.');

end;


procedure TMainForm.btnReloadClick(Sender: TObject);
begin

    if not(FIsConnected) then
    begin
        THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit();
    end;

    // Only administrator is allowed
    if MainForm.FAccessLevel = TUserAccess.Admin then
    begin
        StatBar_TXT1.Caption :=TStatusBar.Downloading;
        MainForm.ClearOpenItemsSummary();
        MainForm.sgOpenItems.Freeze(True);
        var OpenItems: IOpenItems:=TOpenItems.Create();
        OpenItems.ReadOpenItemsAsync(NullParameter, sgOpenItems, sgCompanyData, ReadOpenItems_Callback);
    end
    else
    begin
        StatBar_TXT1.Caption:=TStatusBar.Ready;
        ThreadFileLog.Log('[Open Items]: User have no R/W access, process halted.');
    end;

end;


procedure TMainForm.btnMakeGroupClick(Sender: TObject);

    procedure FieldsState(IsEnabled: boolean);
    begin

        EditGroupName.Enabled:=IsEnabled;
        EditGroupID.Enabled:=IsEnabled;
        btnMakeGroupAge.Enabled:=IsEnabled;

        if IsEnabled then
        begin
            EditGroupName.Text:=FGroupNmSel;
            EditGroupID.Text:=FGroupIdSel;
        end
        else
        begin
            EditGroupName.Text:='';
            EditGroupID.Text:='';
        end;

    end;

begin

    if not(FIsConnected) then
    begin
        THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Exit;
    end;

    if sgOpenItems.RowCount < 2 then
        Exit;

    if MainForm.FAccessLevel <> TUserAccess.Admin then
    begin
        StatBar_TXT1.Caption:='Insufficient UAC level.';
        ThreadFileLog.Log('[Make Group]: User have no R/W access, process halted.');
        Exit();
    end;

    if cbDump.Enabled then
    begin
        cbDump.Enabled:=False;
        FieldsState(False);
        Exit();
    end
    else
    begin
        cbDump.Enabled:=True;
        FieldsState(True);
        Exit();
    end;

end;


procedure TMainForm.btnMakeGroupAgeClick(Sender: TObject);
begin

    if (not(string.IsNullOrEmpty(EditGroupName.Text))) and (not(string.IsNullOrEmpty(EditGroupID.Text))) then
    begin

        // ----------------------------------------------------
        // Start thread with no parameters passed to an object.
        // ----------------------------------------------------

        var SelectedGroupId: string;

        if EditGroupID.Text = FGroupIdSel then SelectedGroupId:=FGroupIdSel
            else if EditGroupID.Text <> '' then SelectedGroupId:=EditGroupID.Text
                 else SelectedGroupId:=FGroupIdSel;

        var Debtors: IDebtors:=TDebtors.Create();

        if not MainForm.cbDump.Checked then
            Debtors.MakeAgeViewSQLAsync(FOSAmount, SelectedGroupId, sgOpenItems, sgCompanyData, MakeAgeViewSQL_Callback)
        else
            Debtors.MakeAgeViewCSVAsync(FOSAmount, SelectedGroupId, sgOpenItems, sgCompanyData, MakeAgeViewCSV_Callback);

    end
    else
    THelpers.MsgCall(TAppMessage.Warn, 'Please enter group name and try again.' + TChars.CRLF + 'If you will use existing one, then it will be overwritten.');

end;


procedure TMainForm.btnOpenABClick(Sender: TObject);
begin

    if FIsConnected then
    begin
        sgAddressBook.SetUpdatedRow(0);

        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Processing, MainForm);
        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);

        var AddressBook: IAddressBook:=TAddressBook.Create();
        AddressBook.OpenAddressBookAsync('', OpenAddressBook_Callback);

    end
    else
    THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');

end;


procedure TMainForm.btnUpdateABClick(Sender: TObject);
begin

    if not(sgAddressBook.Visible) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please open Address Book first.');
        Exit();
    end;

    if FIsConnected then
    begin
        var AddressBook: IAddressBook:=TAddressBook.Create();
        AddressBook.UpdateAddressBookAsync(sgAddressBook, FAbUpdateFields, UpdateAddressBook_Callback);
    end
    else
    THelpers.MsgCall(TAppMessage.Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');

end;


procedure TMainForm.btnCloseABClick(Sender: TObject);
begin

    if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to close Address Book?') = IDYES then
    begin
        sgAddressBook.SetUpdatedRow(0);
        sgAddressBook.ClearAll(2, 1, 1, True);
    end;

end;


procedure TMainForm.btnExportABClick(Sender: TObject);
begin

    if not(sgAddressBook.Visible) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please open Address Book first.');
        Exit();
    end;

    sgAddressBook.ExportCSV(MainForm.CSVExport, '|');

end;


procedure TMainForm.btnFscApproveClick(Sender: TObject);
begin

    if String.IsNullOrEmpty(FSCComment.Text) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please provide mandatory comment before accepting/declining selected query.');
        Exit;
    end;

    var Queries: IQueries:=TQueries.Create();
    Queries.ApproveQuery(sgFSCView.Cells[sgFSCView.ReturnColumn(TQmsLog.Id, 1, 1), sgFSCView.Row].ToInteger, True);

end;


procedure TMainForm.btnFscRejectClick(Sender: TObject);
begin

    if String.IsNullOrEmpty(FSCComment.Text) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please provide mandatory comment before accepting/declining selected query.');
        Exit();
    end;

    var Queries: IQueries:=TQueries.Create();
    Queries.RejectQuery(sgFSCView.Cells[sgFSCView.ReturnColumn(TQmsLog.Id, 1, 1), sgFSCView.Row].ToInteger, True);

end;


procedure TMainForm.btnLbuUpdateClick(Sender: TObject);
begin

    if String.IsNullOrEmpty(LBUComment.Text) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please provide mandatory comment before accepting/declining selected query.');
        Exit();
    end;

    var Queries: IQueries:=TQueries.Create();
    Queries.ApproveQuery(sgLBUView.Cells[sgLBUView.ReturnColumn(TQmsLog.Id, 1, 1), sgLBUView.Row].ToInteger, False);

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

    if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to delete this section? It cannot be undone.') = IDNO then Exit();
    if sgListSection.RowCount = 1 then Exit();

    // Remove given section
    var Settings: ISettings:=TSettings.Create;
    Settings.DeleteSection(sgListSection.Cells[1, sgListSection.Row]);
    Settings.Encode(TAppFiles.Configuration);

    // Remove from string grid
    sgListSection.DeleteRowFrom(1, 1);

    // Re-number
    for var iCNT: integer:= 1 to sgListSection.RowCount do
        sgListSection.Cells[0, iCNT]:=IntToStr(iCNT);

end;


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


procedure TMainForm.imgEventLogClick(Sender: TObject);
begin
    THelpers.WndCall(EventForm, TWindowState.Modeless);
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

    if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to delete this key? It cannot be undone.') = IDNO then Exit();

    // Check for last row
    if sgListValue.RowCount = 1 then Exit();

    // Remove key
    var Settings: ISettings:=TSettings.Create;
    Settings.DeleteKey(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, sgListValue.Row]);
    Settings.Encode(TAppFiles.Configuration);

    // Remove from string grid list
    sgListValue.DeleteRowFrom(1, 1);

    // Re-number
    for var iCNT: integer:= 1 to sgListValue.RowCount do
        sgListValue.Cells[0, iCNT]:=IntToStr(iCNT);

end;


procedure TMainForm.imgUpdateValuesClick(Sender: TObject);
begin

    if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to save all the changes? It cannot be undone.') = IDNO then exit;

    // Check if there is no empty keys
    for var iCNT: integer:= 1 to (sgListValue.RowCount - 1) do
    begin
        if string.IsNullOrEmpty(sgListValue.Cells[1, iCNT]) then
        begin
            THelpers.MsgCall(TAppMessage.Warn, 'Cannot save. At least one key has no label.');
            Exit;
        end;
    end;

    var Settings: ISettings:=TSettings.Create;

    // Save to settings file all the keys and values
    for var iCNT: integer:= 1 to (sgListValue.RowCount - 1) do
        Settings.SetStringValue(sgListSection.Cells[1, sgListSection.Row], sgListValue.Cells[1, iCNT], sgListValue.Cells[2, iCNT]);

    Settings.Encode(TAppFiles.Configuration);

    THelpers.MsgCall(TAppMessage.Info, 'All Keys and its values has been saved successfully.');

end;


procedure TMainForm.btnPassUpdateClick(Sender: TObject);
begin

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

        if EditNewPassword.Text <> EditNewPasswordConfirmation.Text then
        begin
            THelpers.MsgCall(TAppMessage.Warn, 'New password and its confirmation does not match, please re-type it and try again.');
            Exit;
        end;

        var Utilities: IUtilities:=TUtilities.Create();
        Utilities.SetNewPasswordAsync(EditCurrentPassword.Text, EditNewPassword.Text, SetNewPassword_Callback);

    end
    else
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please provide with current password, new password and its confirmation.');
    end;

end;


procedure TMainForm.btnUnlockClick(Sender: TObject);
begin

    if btnUnlock.Caption = 'Lock' then
    begin
        SetSettingsPanel(True);
        Exit;
    end;

    if String.IsNullOrEmpty(EditPassword.Text) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please provide with password.');
        Exit;
    end
    else
    begin
        var Utilities: IUtilities:=TUtilities.Create();
        Utilities.CheckGivenPasswordAsync(EditPassword.Text, CheckGivenPassword_Callback);
    end;

end;


end.

