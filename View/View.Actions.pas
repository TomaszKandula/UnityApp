unit View.Actions;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellApi,
    Winapi.TLHelp32,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.StrUtils,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Grids,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.ComCtrls,
    Vcl.StdCtrls,
    Vcl.Imaging.pngimage,
    Vcl.ImgList,
    Vcl.Imaging.GIFImg,
    Vcl.Clipbrd,
    Data.Win.ADODB,
    Unity.Arrays,
    Unity.Records,
    Unity.Grid,
    Unity.Panel;


type


    TActionsForm = class(TForm)
        OpenItemsGrid: TStringGrid;
        DailyCom: TMemo;
        HistoryGrid: TStringGrid;
        GeneralCom: TMemo;
        zText1: TLabel;
        zText2: TLabel;
        zText7: TLabel;
        zText8: TLabel;
        zText3: TLabel;
        Cust_Name: TLabel;
        Cust_Number: TLabel;
        PanelGrid: TPanel;
        PanelHeader: TPanel;
        Cust_Person: TEdit;
        Cust_Mail: TEdit;
        HistoryPanel: TPanel;
        HistoryTitle: TLabel;
        DailyPanel: TPanel;
        DailyTitle: TLabel;
        GeneralPanel: TPanel;
        GeneralTitle: TLabel;
        Cust_Phone: TComboBox;
        GroupDetails: TGroupBox;
        btnSaveCustDetails: TSpeedButton;
        Cust_MailBack: TShape;
        Cust_PersonBack: TShape;
        Cust_NumberBack: TShape;
        Cust_NameBack: TShape;
        btnEdit: TSpeedButton;
        btnCopyCustName: TSpeedButton;
        btnCopyCustNumber: TSpeedButton;
        btnCopyPerson: TSpeedButton;
        btnCopyEmail: TSpeedButton;
        PanelActions: TPanel;
        MasterPanel: TPanel;
        txtFixedText: TLabel;
        txtTimeDate: TLabel;
        zText9: TLabel;
        Cust_MailGeneral: TEdit;
        Cust_MailGeneralBack: TShape;
        btnCopyGeneralMail: TSpeedButton;
        HistoryGridBorders: TShape;
        OpenItemsGridBorders: TShape;
        DailyComBorders: TShape;
        GeneralComBorders: TShape;
        PanelComments: TPanel;
        SepLine1: TBevel;
        SepLine3: TBevel;
        SepLine2: TBevel;
        SepLine4: TBevel;
        SepLine5: TBevel;
        txtDesc: TLabel;
        imgInfo: TImage;
        imgCoverSaveBtn: TImage;
        GroupOpenItems: TGroupBox;
        LabelOpenAm: TLabel;
        LabelAmount: TLabel;
        ValueOpenAm: TLabel;
        ValueAmount: TLabel;
        GroupEmails: TGroupBox;
        cbUserInCopy: TCheckBox;
        cbCtrlStatusOff: TCheckBox;
        PanelBottom: TPanel;
        txtItem: TLabel;
        btnCallCustomer: TImage;
        btnAutoStatement: TImage;
        btnCustomStatement: TImage;
        btnLogMissingInv: TImage;
        btnLogNow: TImage;
        btnClearFollowUp: TImage;
        btnSetFollowUp: TImage;
        btnBack: TImage;
        btnNext: TImage;
        txtBack: TLabel;
        txtNext: TLabel;
        txtSetFollowUp: TLabel;
        txtClearFollowUp: TLabel;
        txtLogMissingInv: TLabel;
        txtLogNow: TLabel;
        txtCustomStatement: TLabel;
        txtAutoStatement: TLabel;
        txtCallCustomer: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
        procedure HistoryGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure HistoryGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure Cust_PhoneMouseEnter(Sender: TObject);
        procedure Cust_PersonMouseEnter(Sender: TObject);
        procedure Cust_MailMouseEnter(Sender: TObject);
        procedure Cust_MailGeneralMouseEnter(Sender: TObject);
        procedure OpenItemsGridMouseEnter(Sender: TObject);
        procedure HistoryGridMouseEnter(Sender: TObject);
        procedure DailyComMouseEnter(Sender: TObject);
        procedure GeneralComMouseEnter(Sender: TObject);
        procedure DailyComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure GeneralComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnCopyCustNameClick(Sender: TObject);
        procedure btnCopyCustNumberClick(Sender: TObject);
        procedure btnCopyGeneralMailClick(Sender: TObject);
        procedure btnCopyEmailClick(Sender: TObject);
        procedure btnCopyPersonClick(Sender: TObject);
        procedure btnEditClick(Sender: TObject);
        procedure btnSaveCustDetailsClick(Sender: TObject);
        procedure btnBackClick(Sender: TObject);
        procedure btnNextClick(Sender: TObject);
        procedure btnSetFollowUpClick(Sender: TObject);
        procedure btnClearFollowUpClick(Sender: TObject);
        procedure btnLogMissingInvClick(Sender: TObject);
        procedure btnLogNowClick(Sender: TObject);
        procedure btnCustomStatementClick(Sender: TObject);
        procedure btnAutoStatementClick(Sender: TObject);
        procedure btnCallCustomerClick(Sender: TObject);
        procedure btnBackMouseEnter(Sender: TObject);
        procedure btnBackMouseLeave(Sender: TObject);
        procedure btnNextMouseEnter(Sender: TObject);
        procedure btnNextMouseLeave(Sender: TObject);
        procedure btnSetFollowUpMouseEnter(Sender: TObject);
        procedure btnSetFollowUpMouseLeave(Sender: TObject);
        procedure btnClearFollowUpMouseEnter(Sender: TObject);
        procedure btnClearFollowUpMouseLeave(Sender: TObject);
        procedure btnLogMissingInvMouseEnter(Sender: TObject);
        procedure btnLogMissingInvMouseLeave(Sender: TObject);
        procedure btnLogNowMouseEnter(Sender: TObject);
        procedure btnLogNowMouseLeave(Sender: TObject);
        procedure btnCustomStatementMouseEnter(Sender: TObject);
        procedure btnCustomStatementMouseLeave(Sender: TObject);
        procedure btnAutoStatementMouseEnter(Sender: TObject);
        procedure btnAutoStatementMouseLeave(Sender: TObject);
        procedure btnCallCustomerMouseEnter(Sender: TObject);
        procedure btnCallCustomerMouseLeave(Sender: TObject);
    strict private
        const AppButtonTxtNormal = $00555555;
        const AppButtonTxtSelected = $006433C9;
        var FHistoryGrid: boolean;
        var FCUID: string;
        var FSCUID: string;
        var FBranch: string;
        var FBanksHtml: string;
        var FCoCode: string;
        var FCustName: string;
        var FCustNumber: string;
        var FLbuName: string;
        var FLbuAddress: string;
        var FLbuPhone: string;
        var FLbuSendFrom: string;
        var FSrcColumns: TAIntigers;
        var FAbUpdateFields: TAddressBookUpdateFields;
        var FDailyCommentFields: TDailyCommentFields;
        var FGeneralCommentFields: TGeneralCommentFields;
        var FOpenItemsTotal: TOpenItemsTotal;
        var FPayLoad: TAccountStatementPayLoad;
        var FIsDataLoaded: boolean;
        function  GetRunningApps(SearchName: string): boolean;
        procedure GetOpenItems(OpenItemsDest, OpenItemsSrc: TStringGrid);
        procedure UpdateGeneral(var Text: TMemo);
        procedure UpdateHistory(var HistoryGrid: TStringGrid);
        procedure GetFirstComment(var Text: TMemo);
        procedure UpdateCustDetails();
        procedure UpdateCompanyDetails();
        procedure UpdateOpenItems();
        procedure UpdateData();
        procedure InitializePanels();
        procedure InitializeSpeedButtons();
        procedure Initialize();
        procedure ClearAll();
        procedure SetControls();
        procedure MakePhoneCall();
        procedure LoadCustomer(GoNext: boolean);
        procedure ClearFollowUp();
        procedure SaveCustomerDetails();
        procedure SaveGeneralComment();
        procedure SaveDailyComment();
        procedure SendAccountStatement_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
        procedure UpdateAddressBook_Callback(CallResponse: TCallResponse);
        procedure EditGeneralComment_Callback(CallResponse: TCallResponse);
        procedure EditDailyComment_Callback(CallResponse: TCallResponse);
    public
        property CUID: string read FCUID;
        property SCUID: string read FSCUID;
        property Branch: string read FBranch;
        property CoCode: string read FCoCode;
        property CustName: string read FCustName;
        property CustNumber: string read FCustNumber;
        property LbuName: string read FLbuName;
        property LbuAddress: string read FLbuAddress;
        property LbuPhone: string read FLbuPhone;
        property LbuSendFrom: string read FLbuSendFrom;
        property BanksHtml: string read FBanksHtml;
    end;


    function ActionsForm(): TActionsForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Queries,
    View.Calendar,
    View.SendStatement,
    View.PhoneList,
    DbModel{Legacy},
    Sync.Documents,
    Unity.Settings,
    Unity.SessionService,
    Unity.Chars,
    Unity.Helpers,
    Unity.Enums,
    Unity.Sorting,
    Unity.Delimiters,
    Unity.Unknown,
    Unity.Common,
    Unity.EventLogger,
    Async.Utilities,
    Async.Companies,
    Async.AddressBook,
    Async.Comments,
    Async.Statements;


var vActionsForm: TActionsForm;


function ActionsForm(): TActionsForm;
begin
    if not(Assigned(vActionsForm)) then Application.CreateForm(TActionsForm, vActionsForm);
    Result:=vActionsForm;
end;


{$REGION 'LOCAL HELPERS'}


function TActionsForm.GetRunningApps(SearchName: string): boolean;
begin

    Result:=False;

    // Take snapshots of running applications
    var PE: TProcessEntry32; PE.dwSize:=SizeOf(PE);
    var Snap: THandle:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);

    if Snap <> 0 then
    begin
        if Process32First(Snap, PE) then
        begin
            var FileName: string:=string(PE.szExeFile);

            // Go item by item  and stop if matched
            while Process32Next(Snap, PE) do
            begin
                FileName:=string(PE.szExeFile);
                if LowerCase(FileName) = LowerCase(SearchName) then
                begin
                    Result:=True;
                    Break;
                end;
            end;
        end;

        CloseHandle(Snap);
    end;

end;


procedure TActionsForm.UpdateOpenItems();
begin

    txtTimeDate.Caption:=MainForm.FOpenItemsUpdate;
    Cust_Name.Caption  :=CustName;
    Cust_Number.Caption:=CustNumber;

    GetOpenItems(OpenItemsGrid, MainForm.sgOpenItems);
    ValueOpenAm.Caption:=FormatFloat('#,##0.00', FOpenItemsTotal.OpenAm) {+ ' ' + MainForm.tcCURRENCY.Caption};
    ValueAmount.Caption:=FormatFloat('#,##0.00', FOpenItemsTotal.Am) {+ ' ' + MainForm.tcCURRENCY.Caption};

end;


procedure TActionsForm.UpdateData();
begin
    UpdateHistory(HistoryGrid);
    UpdateGeneral(GeneralCom);
    UpdateCustDetails();
    UpdateCompanyDetails();
end;


procedure TActionsForm.GetOpenItems(OpenItemsDest: TStringGrid; OpenItemsSrc: TStringGrid);
begin

    var kCNT:=1;

    FOpenItemsTotal.OpenAm   :=0;
    FOpenItemsTotal.Am       :=0;
    FOpenItemsTotal.OpenCurAm:=0;
    FOpenItemsTotal.CurAm    :=0;

    // Get columns numbers from source open items string grid
    FSrcColumns[0] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.InvoNo);
    FSrcColumns[1] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.Txt);
    FSrcColumns[2] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.AddTxt);
    FSrcColumns[3] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.OpenAm);
    FSrcColumns[4] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.Am);
    FSrcColumns[5] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.OpenCurAm);
    FSrcColumns[6] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.CurAm);
    FSrcColumns[7] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.ISO);
    FSrcColumns[8] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.DueDt);
    FSrcColumns[9] :=OpenItemsSrc.GetCol(DbModel.TOpenitems.ValDt);
    FSrcColumns[10]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.Ctrl);
    FSrcColumns[11]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.PmtStat);
    FSrcColumns[12]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.Ad1);
    FSrcColumns[13]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.Ad2);
    FSrcColumns[14]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.Ad3);
    FSrcColumns[15]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.Pno);
    FSrcColumns[16]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.PArea);
    FSrcColumns[17]:=OpenItemsSrc.GetCol(DbModel.TOpenitems.Cuid);

    // Get headers
    for var iCNT: integer:=Low(FSrcColumns) to High(FSrcColumns) do
    begin

        if FSrcColumns[iCNT] = -100 then
        begin
            THelpers.MsgCall(Warn, 'There are no open items loaded. Please reload it or wait untill auto-load is complete and try again.');
            Close;
            Exit;
        end;

        OpenItemsDest.Cells[iCNT + 1, 0]:=OpenItemsSrc.Cells[FSrcColumns[iCNT], 0];

    end;

    var SourceDBName:=THelpers.GetSourceDBName(CoCode, 'F');
    for var iCNT: integer:=1 to OpenItemsSrc.RowCount - 1 do
    begin

        if
            (OpenItemsSrc.Cells[MainForm.sgOpenItems.GetCol(TOpenitems.CustNo), iCNT] = CustNumber)
        and
            (OpenItemsSrc.Cells[MainForm.sgOpenItems.GetCol(TOpenitems.SourceDBName), iCNT] = SourceDBName)
        then
        begin

            for var jCNT: integer:=Low(FSrcColumns) to High(FSrcColumns) do
                OpenItemsDest.Cells[jCNT + 1, kCNT]:=OpenItemsSrc.Cells[FSrcColumns[jCNT], iCNT];

            FOpenItemsTotal.OpenAm   :=FOpenItemsTotal.OpenAm    + (OpenItemsSrc.Cells[FSrcColumns[3], iCNT]).ToDouble;
            FOpenItemsTotal.Am       :=FOpenItemsTotal.Am        + (OpenItemsSrc.Cells[FSrcColumns[4], iCNT]).ToDouble;
            FOpenItemsTotal.OpenCurAm:=FOpenItemsTotal.OpenCurAm + (OpenItemsSrc.Cells[FSrcColumns[5], iCNT]).ToDouble;
            FOpenItemsTotal.CurAm    :=FOpenItemsTotal.CurAm     + (OpenItemsSrc.Cells[FSrcColumns[6], iCNT]).ToDouble;

            inc(kCNT);
            OpenItemsDest.RowCount:=kCNT;

        end;

    end;

    // Hide helpers columns from string grid
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(DbModel.TOpenitems.Ad1)]  :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(DbModel.TOpenitems.Ad2)]  :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(DbModel.TOpenitems.Ad3)]  :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(DbModel.TOpenitems.Pno)]  :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(DbModel.TOpenitems.PArea)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(DbModel.TOpenitems.Cuid)] :=OpenItemsDest.sgRowHidden;

    // Sort via payment status
    OpenItemsDest.MSort(OpenItemsDest.GetCol(TOpenitems.PmtStat), TDataType.TInteger, True);

end;


procedure TActionsForm.UpdateCustDetails();
begin

    var AddressBook: IAddressBook:=TAddressBook.Create();
    var CustomerDetails: TCustomerDetails;
    CustomerDetails:=AddressBook.GetCustomerDetailsAwaited(SCUID);

    var Phones: string;

    Cust_Person.Text     :=CustomerDetails.CustPerson;
    Cust_MailGeneral.Text:=CustomerDetails.CustMailGen;
    Cust_Mail.Text       :=CustomerDetails.CustMailStat;
    Phones               :=CustomerDetails.CustPhones;

    if (Phones <> '') or (Phones <> ' ') then
    begin
        Cust_Phone.Clear();
        Cust_Phone.Items.Text:=THelpers.Explode(Phones, TDelimiters.Semicolon);
        Cust_Phone.ItemIndex:=0;
    end;

    SetControls();

end;


procedure TActionsForm.UpdateCompanyDetails();
begin

    var Companies: ICompanies:=TCompanies.Create();
    var CompanyDetails: TCompanyDetails;
    CompanyDetails:=Companies.GetCompanyDetailsAwaited(CoCode, Branch);

    FLbuName    :=CompanyDetails.LbuName;
    FLbuAddress :=CompanyDetails.LbuAddress;
    FLbuPhone   :=CompanyDetails.LbuPhone;
    FLbuSendFrom:=CompanyDetails.LbuEmail;
    FBanksHtml  :=CompanyDetails.LbuBanks;

end;


procedure TActionsForm.UpdateHistory(var HistoryGrid: TStringGrid);
begin

    var Comments: IComments:=TComments.Create();
    var ReturnedGrid:=Comments.GetDailyCommentsAwaited(CUID);
    try

        HistoryGrid.SqlColumns:=ReturnedGrid.SqlColumns;
        HistoryGrid.RowCount  :=ReturnedGrid.RowCount;
        HistoryGrid.ColCount  :=ReturnedGrid.ColCount;

        for var iCNT:=0 to ReturnedGrid.RowCount - 1 do
            for var jCNT:=0 to ReturnedGrid.ColCount - 1 do
                HistoryGrid.Cells[jCNT, iCNT]:=ReturnedGrid.Cells[jCNT, iCNT];

        HistoryGrid.ColWidths[HistoryGrid.GetCol(TDailyComment.FixedComment)]:=HistoryGrid.sgRowHidden;
        HistoryGrid.SetColWidth(10, 20, 400);
        FHistoryGrid:=True;
        HistoryGrid.Visible:=FHistoryGrid;

    finally
        if Assigned(ReturnedGrid) then ReturnedGrid.Free();
    end;

end;


procedure TActionsForm.UpdateGeneral(var Text: TMemo);
begin
    var Comments: IComments:=TComments.Create();
    Text.Text:=Comments.GetGeneralCommentAwaited(CUID);
end;


procedure TActionsForm.GetFirstComment(var Text: TMemo);
begin

    if not(Text.Visible) then Exit();
    var GetColumn: integer:=HistoryGrid.GetCol(TDailyComment.FixedComment);
    if GetColumn <> -100 then
        Text.Text:=HistoryGrid.Cells[GetColumn, 1{fixed first row}];

end;


procedure TActionsForm.SetControls();
begin

    // Cover save button if customer is not registered.
    if
        (Cust_Person.Text = TUnknown.NotFound)
    or
        (Cust_Mail.Text = TUnknown.NotFound)
    or
        (Cust_MailGeneral.Text = TUnknown.NotFound)
    then
        imgCoverSaveBtn.Visible:=True
            else
                imgCoverSaveBtn.Visible:=False;

    // Disable text fields if customer is not registered.
    if Cust_Phone.Text = TUnknown.NotFound then
        Cust_Phone.Enabled:=False
            else
                Cust_Phone.Enabled:=True;

    if Cust_Mail.Text = TUnknown.NotFound then
        Cust_Mail.Enabled:=False
            else
                Cust_Mail.Enabled:=True;

    if Cust_Person.Text = TUnknown.NotFound then
        Cust_Person.Enabled:=False
            else
                Cust_Person.Enabled:=True;

    if Cust_MailGeneral.Text = TUnknown.NotFound then
        Cust_MailGeneral.Enabled:=False
            else
                Cust_MailGeneral.Enabled:=True;

end;


procedure TActionsForm.Initialize();
begin
    FCUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCuid), MainForm.sgAgeView.Row]; // to be deleted
    FCustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCustomerName), MainForm.sgAgeView.Row];
    FCustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCustomerNumber), MainForm.sgAgeView.Row]; // important!
    FCoCode    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCoCode), MainForm.sgAgeView.Row]; // important!
    FBranch    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fAgent), MainForm.sgAgeView.Row]; // spit into agent and division
    FSCUID     :=CustNumber + THelpers.CoConvert(CoCode); // to be deleted
end;


procedure TActionsForm.ClearAll();
begin

    Cust_Name.Caption    :=TUnknown.NotFound;
    Cust_Number.Caption  :=TUnknown.NotFound;
    Cust_Person.Text     :=TUnknown.NotFound;
    Cust_Mail.Text       :=TUnknown.NotFound;
    Cust_MailGeneral.Text:=TUnknown.NotFound;

    Cust_Phone.Clear;
    Cust_Phone.Items.Add(TUnknown.NotFound);
    Cust_Phone.ItemIndex:=0;

    DailyCom.Text  :='';
    GeneralCom.Text:='';

    HistoryGrid.ClearAll(2, 1, 1, True);
    OpenItemsGrid.ClearAll(2, 1, 1, True);

end;


procedure TActionsForm.MakePhoneCall();
begin

    if String.IsNullOrEmpty(ActionsForm.Cust_Phone.Text) or String.IsNullOrWhiteSpace(ActionsForm.Cust_Phone.Text) then
    begin
        THelpers.MsgCall(Warn, 'No phone number has been found. Please provide valid phone number and try again.');
        Exit();
    end;

    var Settings: ISettings:=TSettings.Create;

    if not FileExists(Settings.DirApplication + 'LyncCall.exe') then
    begin
        THelpers.MsgCall(Error, TCommon.APPCAPTION + ' cannot find ''lynccall.exe''. Please contact IT support.');
        Exit();
    end;

    if not ActionsForm.GetRunningApps('lync.exe') then
    begin
        THelpers.MsgCall(Error, TCommon.APPCAPTION + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
        Exit();
    end;

    ShellExecute(ActionsForm.Handle, 'open', PChar(Settings.DirApplication + 'LyncCall.exe'), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);

    if ActionsForm.DailyCom.Text = '' then
    begin
        ActionsForm.DailyCom.Text:='Called customer today.';
        SaveDailyComment();
    end
    else
    begin
        ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + TChars.CRLF + 'Called customer today.';
        SaveDailyComment();
    end;

end;


procedure TActionsForm.LoadCustomer(GoNext: boolean);

    function CheckRow(iterator: integer): boolean;
    begin

        Result:=True;
        if
            (MainForm.sgAgeView.RowHeights[iterator] <> -1)
        and
            (MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fOverdue), iterator] <> '0')
        then
        begin
            MainForm.sgAgeView.Row:=iterator;
            Result:=False;
        end;

    end;

begin

    Screen.Cursor:=crSQLWait;

    DailyCom.Text   :='';
    GeneralCom.Text :='';

    // -------------------------------------------------
    // Move grid cursor to next item (skip hidden rows).
    // -------------------------------------------------

    if GoNext then
        for var iCNT: integer:=(MainForm.sgAgeView.Row - 1) Downto 1 do
            if not CheckRow(iCNT) then Break;

    // -----------------------------------------------------
    // Move grid cursor to previous item (skip hidden rows).
    // -----------------------------------------------------

    if not GoNext then
        for var iCNT: integer:=(MainForm.sgAgeView.Row + 1) to MainForm.sgAgeView.RowCount - 1 do
            if not CheckRow(iCNT) then Break;

    // --------------------------------
    // Load data for selected customer.
    // --------------------------------

    THelpers.ExecWithDelay(250, procedure
    begin
        Initialize();
        UpdateOpenItems();
        UpdateData();
        Screen.Cursor:=crDefault;
    end);

end;


procedure TActionsForm.ClearFollowUp();
begin

    if THelpers.MsgCall(Question2, 'Are you sure you want to clear this follow up?') = ID_YES then
    begin

        FGeneralCommentFields.CUID        :=CUID;
        FGeneralCommentFields.FixedComment:=TUnknown.NULL;
        FGeneralCommentFields.FollowUp    :=TChars.SPACE;
        FGeneralCommentFields.Free1       :=TUnknown.NULL;
        FGeneralCommentFields.Free2       :=TUnknown.NULL;
        FGeneralCommentFields.Free3       :=TUnknown.NULL;
        FGeneralCommentFields.EventLog    :=True;

        var Comments: IComments:=TComments.Create();
        Comments.EditGeneralComment(FGeneralCommentFields, EditGeneralComment_Callback);

        MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TGeneralComment.fFollowUp), MainForm.sgAgeView.Row]:='';

    end;

end;


procedure TActionsForm.SaveCustomerDetails();
begin

    FAbUpdateFields.Scuid     :=SCUID;
    FAbUpdateFields.Contact   :=Cust_Person.Text;
    FAbUpdateFields.Email     :=Cust_MailGeneral.Text;
    FAbUpdateFields.Estatement:=Cust_Mail.Text;
    FAbUpdateFields.Phones    :=THelpers.Implode(Cust_Phone.Items, TDelimiters.Semicolon);

    var AddressBook: IAddressBook:=TAddressBook.Create();
    AddressBook.UpdateAddressBookAsync(nil, FAbUpdateFields, UpdateAddressBook_Callback);

end;


procedure TActionsForm.SaveGeneralComment();
begin

    FGeneralCommentFields.CUID        :=CUID;
    FGeneralCommentFields.FixedComment:=GeneralCom.Text;
    FGeneralCommentFields.FollowUp    :=TUnknown.NULL;
    FGeneralCommentFields.Free1       :=TUnknown.NULL;
    FGeneralCommentFields.Free2       :=TUnknown.NULL;
    FGeneralCommentFields.Free3       :=TUnknown.NULL;
    FGeneralCommentFields.EventLog    :=True;

    var Comments: IComments:=TComments.Create();
    Comments.EditGeneralComment(FGeneralCommentFields, EditGeneralComment_Callback);

end;


procedure TActionsForm.SaveDailyComment();
begin

    //FDailyCommentFields.GroupIdSel   :=MainForm.FGroupIdSel;
    //FDailyCommentFields.AgeDateSel   :=MainForm.FAgeDateSel;
    FDailyCommentFields.CUID         :=CUID;
    FDailyCommentFields.Email        :=False;
    FDailyCommentFields.CallEvent    :=False;
    FDailyCommentFields.CallDuration :=0;
    FDailyCommentFields.Comment      :=DailyCom.Text;
    FDailyCommentFields.EmailReminder:=False;
    FDailyCommentFields.EmailAutoStat:=False;
    FDailyCommentFields.EmailManuStat:=False;
    FDailyCommentFields.EventLog     :=True;
    FDailyCommentFields.UpdateGrid   :=True;
    FDailyCommentFields.ExtendComment:=False;

    var Comments: IComments:=TComments.Create();
    Comments.EditDailyComment(FDailyCommentFields, EditDailyComment_Callback);

end;


procedure TActionsForm.InitializePanels();
begin
    {PanelTop.PanelBorders(clWhite, clSkyBlue, clWhite, clWhite, clWhite);}
end;


procedure TActionsForm.InitializeSpeedButtons();
begin
    btnEdit.Glyph.Transparent:=True;
    btnEdit.Glyph.TransparentColor:=clWhite;
    btnSaveCustDetails.Glyph.Transparent:=True;
    btnSaveCustDetails.Glyph.TransparentColor:=clWhite;
end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TActionsForm.SendAccountStatement_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, CallResponse.LastMessage);

end;


procedure TActionsForm.UpdateAddressBook_Callback(CallResponse: TCallResponse);
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


procedure TActionsForm.EditGeneralComment_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log('[EditGeneralComment_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

end;


procedure TActionsForm.EditDailyComment_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log('[EditDailyComment_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    UpdateHistory(HistoryGrid);
    ThreadFileLog.Log('[EditDailyComment_Callback]: Calling "UpdateHistory".');

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TActionsForm.FormCreate(Sender: TObject);
begin

    PanelActions.Borders(clWhite, $00E3B268, clWhite, clWhite, clWhite);

    InitializePanels;
    InitializeSpeedButtons;

    SetLength(FSrcColumns, 19);
    OpenItemsGrid.ColCount:=19;
    OpenItemsGrid.SetRowHeight(OpenItemsGrid.sgRowHeight, 25);

    HistoryGrid.ColCount:=11;
    HistoryGrid.SetRowHeight(OpenItemsGrid.sgRowHeight, 25);
    HistoryGrid.Visible:=False;

    Cust_Name.Caption    :=TUnknown.NotFound;
    Cust_Number.Caption  :=TUnknown.NotFound;
    Cust_Person.Text     :=TUnknown.NotFound;
    Cust_Mail.Text       :=TUnknown.NotFound;
    Cust_MailGeneral.Text:=TUnknown.NotFound;

    Cust_Phone.Clear;
    Cust_Phone.Items.Add(TUnknown.NotFound);
    Cust_Phone.ItemIndex:=0;

    ValueOpenAm.Caption:='';
    ValueAmount.Caption:='';
    txtDesc.Caption    :='';
    DailyCom.Text      :='';
    GeneralCom.Text    :='';
    txtTimeDate.Caption:='';

end;


procedure TActionsForm.FormShow(Sender: TObject);
begin
    {Do nothing}
end;


procedure TActionsForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crSQLWait;
        OpenItemsGrid.Freeze(True);
        HistoryGrid.Freeze(True);

        THelpers.ExecWithDelay(500, procedure
        begin

            Initialize();
            UpdateOpenItems();
            UpdateData();

            OpenItemsGrid.Freeze(False);
            OpenItemsGrid.AutoThumbSize;
            OpenItemsGrid.SetColWidth(10, 20, 400);

            HistoryGrid.Freeze(False);
            HistoryGrid.AutoThumbSize;
            HistoryGrid.SetColWidth(10, 20, 400);

            {GetFirstComment(DailyCom);}
            Screen.Cursor:=crDefault;
            FIsDataLoaded:=True;

        end);

    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TActionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsDataLoaded:=False;
end;


procedure TActionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    ClearAll();
    CanClose:=True;
end;


procedure TActionsForm.FormDestroy(Sender: TObject);
begin
    {Do nothing}
end;


procedure TActionsForm.HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    if gdSelected in State then HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True)
        else HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TCommon.SelectionColor, clBlack, clWhite, True);

end;


procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    if ARow = 0 then Exit;

    OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);

    if (ACol = OpenItemsGrid.GetCol(TOpenitems.OpenCurAm)) or (ACol = OpenItemsGrid.GetCol(TOpenitems.OpenAm))
        or (ACol = OpenItemsGrid.GetCol(TOpenitems.CurAm)) or (ACol = OpenItemsGrid.GetCol(TOpenitems.Am))
        or (ACol = OpenItemsGrid.GetCol(TOpenitems.PmtStat)) then
    begin
        if gdSelected in State then OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;


procedure TActionsForm.HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
    DailyCom.Text:=HistoryGrid.Cells[HistoryGrid.GetCol(TDailyComment.FixedComment), ARow];
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TActionsForm.btnBackClick(Sender: TObject);
begin
    LoadCustomer(true);
end;


procedure TActionsForm.btnNextClick(Sender: TObject);
begin
    LoadCustomer(false);
end;


procedure TActionsForm.btnSetFollowUpClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=DateToDB;
    THelpers.WndCall(CalendarForm, TWindowState.Modal);
end;


procedure TActionsForm.btnClearFollowUpClick(Sender: TObject);
begin
    ClearFollowUp();
end;


procedure TActionsForm.btnLogMissingInvClick(Sender: TObject);
begin
    //QmsForm.IsMissing:=True;
    //THelpers.WndCall(QmsForm, Modal);
end;


procedure TActionsForm.btnLogNowClick(Sender: TObject);
begin
    //QmsForm.IsMissing:=False;
    //THelpers.WndCall(QmsForm, Modal);
end;


procedure TActionsForm.btnCustomStatementClick(Sender: TObject);
begin
    THelpers.WndCall(SendForm, TWindowState.Modal);
end;


procedure TActionsForm.btnAutoStatementClick(Sender: TObject);
begin

    if THelpers.MsgCall(Question2, 'Are you absolutely sure you want to send it, right now?') = IDNO
        then Exit;

    // ---------------------------------------------------------------------
    // UpdateFOpenItemsRefs and UpdateFCtrStatusRefs must be executed before
    // SendAccountStatement is called.
    // ---------------------------------------------------------------------

    MainForm.UpdateFOpenItemsRefs(ActionsForm.OpenItemsGrid);
    MainForm.UpdateFControlStatusRefs(MainForm.sgControlStatus);

    // --------------------------------
    // Prepare PayLoad for the request.
    // --------------------------------

    FPayLoad.Layout        :=TDocMode.Defined;
    FPayLoad.Subject       :='Account Statement';
    FPayLoad.Mess          :='';
    FPayLoad.InvFilter     :=TInvoiceFilter.ShowAllItems;
    FPayLoad.BeginDate     :='';
    FPayLoad.EndDate       :='';
    FPayLoad.OpenItems     :=OpenItemsGrid;
    FPayLoad.CUID          :=CUID;
    FPayLoad.SendFrom      :=LbuSendFrom;
    FPayLoad.MailTo        :=Cust_Mail.Text;
    FPayLoad.CustName      :=CustName;
    FPayLoad.CustNumber    :=CustNumber;
    FPayLoad.LBUName       :=LbuName;
    FPayLoad.LBUAddress    :=LbuAddress;
    FPayLoad.Telephone     :=LbuPhone;
    FPayLoad.BankDetails   :=BanksHtml;
    FPayLoad.Series        :=False;
    FPayLoad.ItemNo        :=0;
    FPayLoad.OpenItems     :=ActionsForm.OpenItemsGrid;
    FPayLoad.OpenItemsRefs :=MainForm.FOpenItemsRefs;
    FPayLoad.ControlStatus :=MainForm.sgControlStatus;
    FPayLoad.CtrlStatusRefs:=MainForm.FCtrlStatusRefs;
    FPayLoad.IsCtrlStatus  :=ActionsForm.cbCtrlStatusOff.Checked;
    FPayLoad.IsUserInCopy  :=ActionsForm.cbUserInCopy.Checked;

    //var Statements: IStatements:=TStatements.Create();
    //Statements.SendAccountStatement(MainForm.FAgeDateSel, FPayLoad, SendAccountStatement_Callback);

end;


procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
    MakePhoneCall();
end;


procedure TActionsForm.btnCopyCustNameClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Name.Caption;
end;


procedure TActionsForm.btnCopyCustNumberClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Number.Caption;
end;


procedure TActionsForm.btnCopyGeneralMailClick(Sender: TObject);
begin
    Clipboard.AsText:=Cust_MailGeneral.Text;
end;


procedure TActionsForm.btnCopyEmailClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Mail.Text;
end;


procedure TActionsForm.btnCopyPersonClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Person.Text;
end;


procedure TActionsForm.btnEditClick(Sender: TObject);
begin
    THelpers.WndCall(PhoneListForm, TWindowState.Modal);
end;


procedure TActionsForm.btnSaveCustDetailsClick(Sender: TObject);
begin
    SaveCustomerDetails();
end;


{$ENDREGION}


{$REGION 'MOUSE EVENTS'}


procedure TActionsForm.Cust_PhoneMouseEnter(Sender: TObject);
begin
    if (Cust_Phone.Enabled) and (Cust_Phone.Visible) then Cust_Phone.SetFocus();
end;


procedure TActionsForm.Cust_PersonMouseEnter(Sender: TObject);
begin
    if (Cust_Person.Enabled) and (Cust_Person.Visible) then Cust_Person.SetFocus();
end;


procedure TActionsForm.Cust_MailMouseEnter(Sender: TObject);
begin
    if (Cust_Mail.Enabled) and (Cust_Mail.Visible) then Cust_Mail.SetFocus();
end;


procedure TActionsForm.Cust_MailGeneralMouseEnter(Sender: TObject);
begin
    if (Cust_MailGeneral.Enabled) and (Cust_MailGeneral.Visible) then Cust_MailGeneral.SetFocus();
end;


procedure TActionsForm.OpenItemsGridMouseEnter(Sender: TObject);
begin
    if (OpenItemsGrid.Enabled) and (OpenItemsGrid.Visible) then OpenItemsGrid.SetFocus();
end;


procedure TActionsForm.HistoryGridMouseEnter(Sender: TObject);
begin
    if (HistoryGrid.Enabled) and (HistoryGrid.Visible) then HistoryGrid.SetFocus();
end;


procedure TActionsForm.DailyComMouseEnter(Sender: TObject);
begin
    if (DailyCom.Enabled) and (DailyCom.Visible) then DailyCom.SetFocus();
end;


procedure TActionsForm.GeneralComMouseEnter(Sender: TObject);
begin
    if (GeneralCom.Enabled) and (GeneralCom.Visible) then GeneralCom.SetFocus();
end;


procedure TActionsForm.OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    OpenItemsGrid.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;


procedure TActionsForm.OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    OpenItemsGrid.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;


procedure TActionsForm.HistoryGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    HistoryGrid.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;


procedure TActionsForm.HistoryGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    HistoryGrid.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;


procedure TActionsForm.btnBackMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Back to previous customer.';
    txtBack.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnBackMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtBack.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnNextMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Load next customer.';
    txtNext.Font.Color:=AppButtonTxtSelected;
end;

procedure TActionsForm.btnNextMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtNext.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnSetFollowUpMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Add follow-up date.';
    txtSetFollowUp.Font.Color:=AppButtonTxtSelected;
end;

procedure TActionsForm.btnSetFollowUpMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtSetFollowUp.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnClearFollowUpMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Remove existing follow-up date.';
    txtClearFollowUp.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnClearFollowUpMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtClearFollowUp.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnLogMissingInvMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Log to QMS missing invoice (require to fill the invoice details).';
    txtLogMissingInv.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnLogMissingInvMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtLogMissingInv.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnLogNowMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Log to QMS selected invoice(s) from the customer open items list.';
    txtLogNow.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnLogNowMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtLogNow.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnCustomStatementMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Send account statement now.';
    txtCustomStatement.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnCustomStatementMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtCustomStatement.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnAutoStatementMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Send custom e-mail with account statement.';
    txtAutoStatement.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnAutoStatementMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtAutoStatement.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnCallCustomerMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Call customer now.';
    txtCallCustomer.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnCallCustomerMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtCallCustomer.Font.Color:=AppButtonTxtNormal;
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TActionsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    {if (Key = VK_F5) and (Shift=[ssCtrl]) then}
end;


procedure TActionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        DailyCom.Lines.Add(TChars.CRLF);
        Exit;
    end;

    if ( (Key = VK_RETURN) and (DailyCom.Text <> '') ) then SaveDailyComment();

end;


procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        GeneralCom.Lines.Add(TChars.CRLF);
        Exit;
    end;

    if ( (Key = VK_RETURN) and (GeneralCom.Text <> '') ) then SaveGeneralComment();

end;


procedure TActionsForm.OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then OpenItemsGrid.CopyCutPaste(TActions.Copy);
end;


procedure TActionsForm.HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then HistoryGrid.CopyCutPaste(TActions.Copy);
end;


procedure TActionsForm.DailyComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then GeneralCom.SetFocus();
end;


procedure TActionsForm.GeneralComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then DailyCom.SetFocus();
end;


{$ENDREGION}


end.

