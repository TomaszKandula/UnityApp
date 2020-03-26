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
    Unity.Records,
    Unity.Grid,
    Unity.Panel,
    Unity.References;


type


    TActionsForm = class(TForm)
        OpenItemsGrid: TStringGrid;
        DailyCom: TMemo;
        DailyComGrid: TStringGrid;
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
        selSendFrom: TComboBox;
        txtSendFrom: TLabel;
        btnAddComment: TSpeedButton;
        btnGoogleIt: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure OpenItemsGridMouseEnter(Sender: TObject);
        procedure DailyComGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
        procedure DailyComGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure DailyComGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure DailyComGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure DailyComGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure DailyComGridMouseEnter(Sender: TObject);
        procedure DailyComMouseEnter(Sender: TObject);
        procedure DailyComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure GeneralComMouseEnter(Sender: TObject);
        procedure GeneralComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Cust_PhoneMouseEnter(Sender: TObject);
        procedure Cust_PersonMouseEnter(Sender: TObject);
        procedure Cust_MailMouseEnter(Sender: TObject);
        procedure Cust_MailGeneralMouseEnter(Sender: TObject);
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
        procedure btnAddCommentClick(Sender: TObject);
        procedure selSendFromSelect(Sender: TObject);
        procedure btnGoogleItClick(Sender: TObject);
    strict private
        const AppButtonTxtNormal = $00555555;
        const AppButtonTxtSelected = $006433C9;
        var FCustDetailsId: integer;
        var FSourceDBName: string;
        var FCustName: string;
        var FCustNumber: Int64;
        var FLbuName: string;
        var FLbuAddress: string;
        var FLbuPhones: string;
        var FLbuSendFrom: string;
        var FLbuBanksHtml: string;
        var FExclusions: TArray<integer>;
        var FSrcColumns: TArray<integer>;
        var FOpenItemsTotal: TOpenItemsTotal;
        var FPayLoad: TAccDocumentPayLoad;
        var FIsDataLoaded: boolean;
        var OpenItemsRefs: TFOpenItemsRefs;
        var CtrlStatusRefs: TFCtrlStatusRefs;
        function  GetRunningApps(SearchName: string): boolean;
        procedure GetOpenItems(OpenItemsDest, OpenItemsSrc: TStringGrid);
        procedure GetFirstComment(var Text: TMemo);
        procedure UpdateOpenItems();
        procedure UpdateDaily();
        procedure UpdateGeneral();
        procedure UpdateCustDetails();
        procedure UpdateCompanyDetails();
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
        procedure SendAccDocumentAsync_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
        procedure UpdateAddressBook_Callback(CallResponse: TCallResponse);
        procedure InsertAddressBook_Callback(ReturnedId: integer; CallResponse: TCallResponse);
        procedure EditGeneralComment_Callback(CallResponse: TCallResponse);
        procedure EditDailyComment_Callback(CallResponse: TCallResponse);
        procedure GetDailyCommentsAsync_Callback(Comments: TArray<TDailyCommentFields>; CallResponse: TCallResponse);
        procedure GetGeneralCommentAsync_Callback(Comments: TGeneralCommentFields; CallResponse: TCallResponse);
        procedure GetCustomerDetailsAsync_Callback(CustDetails: TCustomerDetails; CallResponse: TCallResponse);
        procedure GetCompanyDetailsAsync_Callback(CompanyDetails: TCompanyDetails; CallResponse: TCallResponse);
    public
        property SourceDBName: string read FSourceDBName;
        property CustName: string read FCustName;
        property CustNumber: Int64 read FCustNumber;
        property LbuName: string read FLbuName;
        property LbuAddress: string read FLbuAddress;
        property LbuPhones: string read FLbuPhones;
        property LbuSendFrom: string read FLbuSendFrom;
        property LbuBanksHtml: string read FLbuBanksHtml;
        property Exclusions: TArray<integer> read FExclusions;
    end;


    function ActionsForm(): TActionsForm;


implementation


{$R *.dfm}


uses
    System.NetEncoding,
    System.Generics.Collections,
    View.Main,
    View.Calendar,
    View.SendStatement,
    View.PhoneList,
    Unity.Settings,
    Unity.Service,
    Unity.Constants,
    Unity.Helpers,
    Unity.Enums,
    Unity.Sorting,
    Api.ReturnOpenItems,
    Api.UserDailyCommentsList,
    Api.ReturnCustSnapshots,
    Api.AddressBookList;


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
    Cust_Number.Caption:=CustNumber.ToString();
    GetOpenItems(OpenItemsGrid, MainForm.sgOpenItems);
    ValueOpenAm.Caption:=FormatFloat('#,##0.00', FOpenItemsTotal.OpenAm);
    ValueAmount.Caption:=FormatFloat('#,##0.00', FOpenItemsTotal.Am);
end;


procedure TActionsForm.UpdateData();
begin
    UpdateDaily();
    UpdateGeneral();
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
    FSrcColumns[0] :=OpenItemsSrc.GetCol(TReturnOpenItems._InvoiceNumber);
    FSrcColumns[1] :=OpenItemsSrc.GetCol(TReturnOpenItems._Text);
    FSrcColumns[2] :=OpenItemsSrc.GetCol(TReturnOpenItems._AdditionalText);
    FSrcColumns[3] :=OpenItemsSrc.GetCol(TReturnOpenItems._OpenAmount);
    FSrcColumns[4] :=OpenItemsSrc.GetCol(TReturnOpenItems._Amount);
    FSrcColumns[5] :=OpenItemsSrc.GetCol(TReturnOpenItems._OpenCurAmount);
    FSrcColumns[6] :=OpenItemsSrc.GetCol(TReturnOpenItems._CurAmount);
    FSrcColumns[7] :=OpenItemsSrc.GetCol(TReturnOpenItems._Iso);
    FSrcColumns[8] :=OpenItemsSrc.GetCol(TReturnOpenItems._DueDate);
    FSrcColumns[9] :=OpenItemsSrc.GetCol(TReturnOpenItems._ValueDate);
    FSrcColumns[10]:=OpenItemsSrc.GetCol(TReturnOpenItems._ControlStatus);
    FSrcColumns[11]:=OpenItemsSrc.GetCol(TReturnOpenItems._PmtStatus);
    FSrcColumns[12]:=OpenItemsSrc.GetCol(TReturnOpenItems._Address1);
    FSrcColumns[13]:=OpenItemsSrc.GetCol(TReturnOpenItems._Address2);
    FSrcColumns[14]:=OpenItemsSrc.GetCol(TReturnOpenItems._Address3);
    FSrcColumns[15]:=OpenItemsSrc.GetCol(TReturnOpenItems._PostalNumber);
    FSrcColumns[16]:=OpenItemsSrc.GetCol(TReturnOpenItems._PostalArea);
    FSrcColumns[17]:=OpenItemsSrc.GetCol(TReturnOpenItems._SourceDbName);
    FSrcColumns[18]:=OpenItemsSrc.GetCol(TReturnOpenItems._CustNumber);

    // Get headers
    for var iCNT:=Low(FSrcColumns) to High(FSrcColumns) do
    begin

        if FSrcColumns[iCNT] = -100 then
        begin
            THelpers.MsgCall(Warn, 'There are no open items loaded. Please reload it or wait untill auto-load is complete and try again.');
            Close;
            Exit();
        end;

        OpenItemsDest.Cells[iCNT + 1, 0]:=OpenItemsSrc.Cells[FSrcColumns[iCNT], 0];

    end;

    for var iCNT:=1 to OpenItemsSrc.RowCount - 1 do
    begin

        if
            (OpenItemsSrc.Cells[MainForm.sgOpenItems.GetCol(TReturnOpenItems._CustNumber), iCNT] = CustNumber.ToString())
        and
            (OpenItemsSrc.Cells[MainForm.sgOpenItems.GetCol(TReturnOpenItems._SourceDbName), iCNT] = SourceDBName)
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
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._Address1)]    :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._Address2)]    :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._Address3)]    :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._PostalNumber)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._PostalArea)]  :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._CustNumber)]  :=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.GetCol(TReturnOpenItems._SourceDbName)]:=OpenItemsDest.sgRowHidden;

    // Sort via payment status
    OpenItemsDest.MSort(OpenItemsDest.GetCol(TReturnOpenItems._PmtStatus), TDataType.TInteger, True);

end;


procedure TActionsForm.UpdateCustDetails();
begin
    FCustDetailsId:=0;
    Service.Mediator.AddressBook.GetCustomerDetailsAsync(CustNumber, SourceDBName, GetCustomerDetailsAsync_Callback);
end;


procedure TActionsForm.UpdateCompanyDetails();
begin
    Service.Mediator.Companies.GetCompanyDetailsAsync(SourceDBName, GetCompanyDetailsAsync_Callback);
end;


procedure TActionsForm.UpdateDaily();
begin

    Service.Mediator.Comments.GetDailyCommentsAsync(
        SourceDBName,
        CustNumber,
        Service.SessionData.AliasName,
        GetDailyCommentsAsync_Callback
    );

end;


procedure TActionsForm.UpdateGeneral();
begin

    Service.Mediator.Comments.GetGeneralCommentAsync(
        SourceDBName,
        CustNumber,
        Service.SessionData.AliasName,
        GetGeneralCommentAsync_Callback
    );

end;


procedure TActionsForm.GetFirstComment(var Text: TMemo);
begin

    var GetColumn:=DailyComGrid.GetCol(TUserDailyCommentsList._UserComment);

    if GetColumn <> -100 then
        Text.Text:=DailyComGrid.Cells[GetColumn, 1] else DailyCom.Text:='';

end;


procedure TActionsForm.SetControls();
begin

    // Cover save button if customer is not registered.
    if
        (Cust_Person.Text = 'Not found!')
    or
        (Cust_Mail.Text = 'Not found!')
    or
        (Cust_MailGeneral.Text = 'Not found!')
    then
        imgCoverSaveBtn.Visible:=True else imgCoverSaveBtn.Visible:=False;

    // Disable text fields if customer is not registered.
    if Cust_Phone.Text = 'Not found!' then
        Cust_Phone.Enabled:=False else Cust_Phone.Enabled:=True;

    if Cust_Mail.Text = 'Not found!' then
        Cust_Mail.Enabled:=False else Cust_Mail.Enabled:=True;

    if Cust_Person.Text = 'Not found!' then
        Cust_Person.Enabled:=False else Cust_Person.Enabled:=True;

    if Cust_MailGeneral.Text = 'Not found!' then
        Cust_MailGeneral.Enabled:=False else Cust_MailGeneral.Enabled:=True;

end;


procedure TActionsForm.Initialize();
begin
    FCustName    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TReturnCustSnapshots._CustomerName), MainForm.sgAgeView.Row];
    FCustNumber  :=(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TReturnCustSnapshots._CustomerNumber), MainForm.sgAgeView.Row]).ToInteger();
    FSourceDBName:=(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TReturnCustSnapshots._SourceDbName), MainForm.sgAgeView.Row]);
end;


procedure TActionsForm.ClearAll();
begin

    Cust_Name.Caption    :='Not found!';
    Cust_Number.Caption  :='Not found!';
    Cust_Person.Text     :='Not found!';
    Cust_Mail.Text       :='Not found!';
    Cust_MailGeneral.Text:='Not found!';

    Cust_Phone.Clear;
    Cust_Phone.Items.Add('Not found!');
    Cust_Phone.ItemIndex:=0;
    selSendFrom.Clear();

    DailyCom.Text  :='';
    GeneralCom.Text:='';

    DailyComGrid.ClearAll(2, 1, 1, True);
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

    function CheckRow(Iterator: integer): boolean;
    begin

        Result:=True;
        if
            (MainForm.sgAgeView.RowHeights[Iterator] <> -1)
        and
            (MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TReturnCustSnapshots._Overdue), Iterator] <> '0')
        then
        begin
            MainForm.sgAgeView.Row:=Iterator;
            Result:=False;
        end;

    end;

begin

    Screen.Cursor:=crHourGlass;

    DailyCom.Text  :='';
    GeneralCom.Text:='';

    // -------------------------------------------------
    // Move grid cursor to next item (skip hidden rows).
    // -------------------------------------------------
    if GoNext then
        for var iCNT:=(MainForm.sgAgeView.Row - 1) Downto 1 do
            if not CheckRow(iCNT) then Break;

    // -----------------------------------------------------
    // Move grid cursor to previous item (skip hidden rows).
    // -----------------------------------------------------
    if not GoNext then
        for var iCNT:=(MainForm.sgAgeView.Row + 1) to MainForm.sgAgeView.RowCount - 1 do
            if not CheckRow(iCNT) then Break;

    // --------------------------------
    // Load data for selected customer.
    // --------------------------------
    THelpers.ExecWithDelay(250, procedure
    begin
        Initialize();
        UpdateOpenItems();
        UpdateData();
        //GetFirstComment(DailyCom);
        Screen.Cursor:=crDefault;
    end);

end;


procedure TActionsForm.ClearFollowUp();
begin

    if THelpers.MsgCall(Question2, 'Are you sure you want to clear this follow up?') = ID_YES then
    begin

        var LGeneralCommentFields: TGeneralCommentFields;
        LGeneralCommentFields.SourceDBName  :=SourceDBName;
        LGeneralCommentFields.CustomerNumber:=CustNumber;
        LGeneralCommentFields.FollowUp      :=' ';
        LGeneralCommentFields.Free1         :=String.Empty;
        LGeneralCommentFields.Free2         :=String.Empty;
        LGeneralCommentFields.Free3         :=String.Empty;
        LGeneralCommentFields.UserComment   :=String.Empty;
        LGeneralCommentFields.UserAlias     :=Service.SessionData.AliasName;

        Service.Mediator.Comments.EditGeneralCommentAsync(LGeneralCommentFields, EditGeneralComment_Callback);

        MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TReturnCustSnapshots._FollowUp), MainForm.sgAgeView.Row]:='';
        MainForm.UpdateFollowUps(MainForm.sgAgeView);

    end;

end;


procedure TActionsForm.SaveCustomerDetails();
begin

    if FCustDetailsId = 0 then
    begin

        var CustomerDetails: TCustomerDetails;

        CustomerDetails.SourceDBName   :=SourceDBName;
        CustomerDetails.CustomerNumber :=CustNumber;
        CustomerDetails.CustomerName   :=CustName;
        CustomerDetails.ContactPerson  :=Cust_Person.Text;
        CustomerDetails.RegularEmails  :=Cust_MailGeneral.Text;
        CustomerDetails.StatementEmails:=Cust_Mail.Text;
        CustomerDetails.PhoneNumbers   :=THelpers.Implode(Cust_Phone.Items, TDelimiters.Semicolon);

        Service.Mediator.AddressBook.AddToAddressBookAsync(CustomerDetails, InsertAddressBook_Callback);

    end
    else
    begin

        var CustomerDetails: TCustomerDetails;

        CustomerDetails.Id             :=FCustDetailsId;
        CustomerDetails.ContactPerson  :=Cust_Person.Text;
        CustomerDetails.RegularEmails  :=Cust_MailGeneral.Text;
        CustomerDetails.StatementEmails:=Cust_Mail.Text;
        CustomerDetails.PhoneNumbers   :=THelpers.Implode(Cust_Phone.Items, TDelimiters.Semicolon);

        Service.Mediator.AddressBook.UpdateAddressBookAsync(CustomerDetails, UpdateAddressBook_Callback);

    end;

end;


procedure TActionsForm.SaveGeneralComment();
begin

    var LGeneralCommentFields: TGeneralCommentFields;
    LGeneralCommentFields.SourceDBName  :=SourceDBName;
    LGeneralCommentFields.CustomerNumber:=CustNumber;
    LGeneralCommentFields.FollowUp      :=String.Empty;
    LGeneralCommentFields.Free1         :=String.Empty;
    LGeneralCommentFields.Free2         :=String.Empty;
    LGeneralCommentFields.Free3         :=String.Empty;
    LGeneralCommentFields.UserComment   :=GeneralCom.Text;
    LGeneralCommentFields.UserAlias     :=Service.SessionData.AliasName;

    Service.Mediator.Comments.EditGeneralCommentAsync(LGeneralCommentFields, EditGeneralComment_Callback);

end;


procedure TActionsForm.SaveDailyComment();
begin

    var GetId:=DailyComGrid.Cells[1, DailyComGrid.Row];
    if GetId = '' then Exit();

    var LDailyCommentFields: TDailyCommentFields;
    LDailyCommentFields.CommentId           :=GetId.ToInteger();
    LDailyCommentFields.SourceDBName        :=SourceDBName;
    LDailyCommentFields.CustomerNumber      :=CustNumber;
    LDailyCommentFields.AgeDate             :=MainForm.LoadedAgeDate;
    LDailyCommentFields.CallEvent           :=0;
    LDailyCommentFields.CallDuration        :=0;
    LDailyCommentFields.FixedStatementsSent :=0;
    LDailyCommentFields.CustomStatementsSent:=0;
    LDailyCommentFields.FixedRemindersSent  :=0;
    LDailyCommentFields.CustomRemindersSent :=0;
    LDailyCommentFields.UserComment         :=DailyCom.Text;
    LDailyCommentFields.UserAlias           :=Service.SessionData.AliasName;

    Service.Mediator.Comments.EditDailyCommentAsync(LDailyCommentFields, EditDailyComment_Callback);

end;


procedure TActionsForm.InitializePanels();
begin
    //PanelTop.PanelBorders(clWhite, clSkyBlue, clWhite, clWhite, clWhite);
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


procedure TActionsForm.SendAccDocumentAsync_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
begin

    Screen.Cursor:=crDefault;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, CallResponse.LastMessage);
    UpdateDaily();

end;


procedure TActionsForm.UpdateAddressBook_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Warn, CallResponse.LastMessage);
        Service.Logger.Log('[UpdateAddressBookAsync_Callback]: Adddress Book has thrown an error "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    var Col1:=MainForm.sgAddressBook.GetCol(TAddressBookList._SourceDbName);
    var Col2:=MainForm.sgAddressBook.GetCol(TAddressBookList._CustomerNumber);
    var Col3:=MainForm.sgAddressBook.GetCol(TAddressBookList._ContactPerson);
    var Col4:=MainForm.sgAddressBook.GetCol(TAddressBookList._RegularEmails);
    var Col5:=MainForm.sgAddressBook.GetCol(TAddressBookList._StatementEmails);
    var Col6:=MainForm.sgAddressBook.GetCol(TAddressBookList._PhoneNumbers);

    // Start with 1 to sip the header
    for var Index:=1 to MainForm.sgAddressBook.RowCount - 1 do
    begin

        var SourceDbNm:=MainForm.sgAddressBook.Cells[Col1, Index];
        var CustomerNo:=MainForm.sgAddressBook.Cells[Col2, Index].ToInt64();

        if (SourceDBName = SourceDbNm) and (CustNumber = CustomerNo) then
        begin
            MainForm.sgAddressBook.Cells[Col3, Index]:=Cust_Person.Text;
            MainForm.sgAddressBook.Cells[Col4, Index]:=Cust_MailGeneral.Text;
            MainForm.sgAddressBook.Cells[Col5, Index]:=Cust_Mail.Text;
            MainForm.sgAddressBook.Cells[Col6, Index]:=THelpers.Implode(Cust_Phone.Items, TDelimiters.Semicolon);
            Break;
        end;

    end;

    THelpers.MsgCall(TAppMessage.Info, 'Address Book has been updated.');
    Service.Logger.Log('[UpdateAddressBookAsync_Callback]: Address Book has been updated.');

end;


procedure TActionsForm.InsertAddressBook_Callback(ReturnedId: integer; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Warn, CallResponse.LastMessage);
        Service.Logger.Log('[InsertAddressBook_Callback]: Adddress Book has thrown an error "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    var Col1:=MainForm.sgAddressBook.GetCol(TAddressBookList._SourceDbName);
    var Col2:=MainForm.sgAddressBook.GetCol(TAddressBookList._CustomerNumber);
    var Col3:=MainForm.sgAddressBook.GetCol(TAddressBookList._CustomerName);
    var Col4:=MainForm.sgAddressBook.GetCol(TAddressBookList._ContactPerson);
    var Col5:=MainForm.sgAddressBook.GetCol(TAddressBookList._RegularEmails);
    var Col6:=MainForm.sgAddressBook.GetCol(TAddressBookList._StatementEmails);
    var Col7:=MainForm.sgAddressBook.GetCol(TAddressBookList._PhoneNumbers);

    var RowCount:=MainForm.sgAddressBook.RowCount + 1;
    MainForm.sgAddressBook.RowCount:=RowCount;

    MainForm.sgAddressBook.Cells[Col1, RowCount - 1]:=FSourceDBName;
    MainForm.sgAddressBook.Cells[Col2, RowCount - 1]:=FCustNumber.ToString();
    MainForm.sgAddressBook.Cells[Col3, RowCount - 1]:=FCustName;
    MainForm.sgAddressBook.Cells[Col4, RowCount - 1]:=Cust_Person.Text;
    MainForm.sgAddressBook.Cells[Col5, RowCount - 1]:=Cust_MailGeneral.Text;
    MainForm.sgAddressBook.Cells[Col6, RowCount - 1]:=Cust_Mail.Text;
    MainForm.sgAddressBook.Cells[Col7, RowCount - 1]:=THelpers.Implode(Cust_Phone.Items, TDelimiters.Semicolon);

    FCustDetailsId:=ReturnedId;
    THelpers.MsgCall(TAppMessage.Info, 'New customer has been added successfully.');
    Service.Logger.Log('[InsertAddressBook_Callback]: Address Book has been updated.');

end;


procedure TActionsForm.EditGeneralComment_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[EditGeneralComment_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

end;


procedure TActionsForm.EditDailyComment_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[EditDailyComment_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    UpdateDaily();

end;


procedure TActionsForm.GetDailyCommentsAsync_Callback(Comments: TArray<TDailyCommentFields>; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[GetDailyCommentsAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    var TotalRows:=Length(Comments);
    if TotalRows > 0 then
    begin

        DailyComGrid.RowCount:=TotalRows + 1;

        for var iCNT:=1 to TotalRows do
        begin

            var Col1:=DailyComGrid.GetCol(TUserDailyCommentsList._CommentId);
            var Col2:=DailyComGrid.GetCol(TUserDailyCommentsList._EntryDateTime);
            var Col3:=DailyComGrid.GetCol(TUserDailyCommentsList._AgeDate);
            var Col4:=DailyComGrid.GetCol(TUserDailyCommentsList._UserComment);
            var Col5:=DailyComGrid.GetCol(TUserDailyCommentsList._UserAlias);

            DailyComGrid.Cells[Col1, iCNT]:=Comments[iCNT - 1].CommentId.ToString();
            DailyComGrid.Cells[Col2, iCNT]:=THelpers.FormatDateTime(Comments[iCNT - 1].EntryDateTime, TCalendar.DateTime);
            DailyComGrid.Cells[Col3, iCNT]:=THelpers.FormatDateTime(Comments[iCNT - 1].AgeDate, TCalendar.DateOnly);
            DailyComGrid.Cells[Col4, iCNT]:=Comments[iCNT - 1].UserComment;
            DailyComGrid.Cells[Col5, iCNT]:=Comments[iCNT - 1].UserAlias;

        end;

        DailyComGrid.SetColWidth(10, 20, 400);
        DailyComGrid.MSort(DailyComGrid.GetCol(TUserDailyCommentsList._CommentId), TDataType.TInteger, False);
        ActionsForm.DailyCom.Text:=DailyComGrid.Cells[DailyComGrid.GetCol(TUserDailyCommentsList._UserComment), DailyComGrid.Row];

    end
    else
    begin
        DailyComGrid.ClearAll(2, 1, 1, True);
    end;

end;


procedure TActionsForm.GetGeneralCommentAsync_Callback(Comments: TGeneralCommentFields; CallResponse: TCallResponse);
begin

    if (not CallResponse.IsSucceeded) and (CallResponse.ErrorCode <> 'no_comment_found') then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[GetGeneralCommentAsync_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    GeneralCom.Text:=Comments.UserComment;

end;


procedure TActionsForm.GetCustomerDetailsAsync_Callback(CustDetails: TCustomerDetails; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    var Phones: string;

    FCustDetailsId       :=CustDetails.Id;
    Cust_Person.Text     :=CustDetails.ContactPerson;
    Cust_MailGeneral.Text:=CustDetails.RegularEmails;
    Cust_Mail.Text       :=CustDetails.StatementEmails;
    Phones               :=CustDetails.PhoneNumbers;

    if (Phones <> '') or (Phones <> ' ') then
    begin
        Cust_Phone.Clear();
        Cust_Phone.Items.Text:=THelpers.Explode(Phones, TDelimiters.Semicolon);
        Cust_Phone.ItemIndex:=0;
    end;

    SetControls();

end;


procedure TActionsForm.GetCompanyDetailsAsync_Callback(CompanyDetails: TCompanyDetails; CallResponse: TCallResponse);
begin

    var LbuEmails:=TStringList.Create();
    try

        if not CallResponse.IsSucceeded then
        begin
            THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
            CompanyDetails.Dispose();
            LbuEmails.Free();
            Exit();
        end;

        FLbuName   :=CompanyDetails.LbuName;
        FLbuAddress:=CompanyDetails.LbuAddress;
        FExclusions:=CompanyDetails.Exclusions;

        FLbuPhones   :=THelpers.ArrayStrToString(CompanyDetails.LbuPhones, ';');
        FLbuBanksHtml:=THelpers.BankListToHtml(CompanyDetails.LbuBanks);

        selSendFrom.Clear();
        THelpers.StrArrayToStrings(CompanyDetails.LbuEmails, LbuEmails);
        selSendFrom.Items.AddStrings(LbuEmails);
        if selSendFrom.Items.Count > 0 then
        begin
            selSendFrom.ItemIndex:=0;
            FLbuSendFrom:=selSendFrom.Text;
        end;

    finally
        LbuEmails.Free();
        CompanyDetails.Dispose();
    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TActionsForm.FormCreate(Sender: TObject);
begin

    PanelActions.Borders(clWhite, $00E3B268, clWhite, clWhite, clWhite);

    InitializePanels;
    InitializeSpeedButtons;

    SetLength(FSrcColumns, 20);
    OpenItemsGrid.ColCount:=20;
    OpenItemsGrid.SetRowHeight(OpenItemsGrid.sgRowHeight, 25);

    DailyComGrid.ColCount:=11;
    DailyComGrid.SetRowHeight(OpenItemsGrid.sgRowHeight, 25);
    DailyComGrid.Visible:=True;

    Cust_Name.Caption    :='Not found!';
    Cust_Number.Caption  :='Not found!';
    Cust_Person.Text     :='Not found!';
    Cust_Mail.Text       :='Not found!';
    Cust_MailGeneral.Text:='Not found!';

    Cust_Phone.Clear;
    Cust_Phone.Items.Add('Not found!');
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
    // Empty
end;


procedure TActionsForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crHourGlass;
        OpenItemsGrid.Freeze(True);
        DailyComGrid.Freeze(True);

        MainForm.TimerCustOpenItems.Enabled:=False;
        Service.Logger.Log('[TActionsForm.FormActivate]: Action view has been opened, open items loader is on hold.');

        THelpers.ExecWithDelay(500, procedure
        begin

            DailyComGrid.RowCount:=2;
            DailyComGrid.ColCount:=6;
            DailyComGrid.Cells[1, 0]:=TUserDailyCommentsList._CommentId;
            DailyComGrid.ColWidths[1]:=-1;
            DailyComGrid.Cells[2, 0]:=TUserDailyCommentsList._EntryDateTime;
            DailyComGrid.Cells[3, 0]:=TUserDailyCommentsList._AgeDate;
            DailyComGrid.Cells[4, 0]:=TUserDailyCommentsList._UserComment;
            DailyComGrid.ColWidths[4]:=-1;
            DailyComGrid.Cells[5, 0]:=TUserDailyCommentsList._UserAlias;

            Initialize();
            UpdateOpenItems();
            UpdateData();

            DailyComGrid.Freeze(False);
            DailyComGrid.AutoThumbSize;
            DailyComGrid.SetColWidth(10, 20, 400);

            OpenItemsGrid.Freeze(False);
            OpenItemsGrid.AutoThumbSize;
            OpenItemsGrid.SetColWidth(10, 20, 400);

            GetFirstComment(DailyCom);
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
    MainForm.TimerCustOpenItems.Enabled:=True;
    Service.Logger.Log('[TActionsForm.FormActivate]: Action view has been closed, open items loader is resumed.');
end;


procedure TActionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    ClearAll();
    CanClose:=True;
end;


procedure TActionsForm.FormDestroy(Sender: TObject);
begin
    // Empty
end;


procedure TActionsForm.DailyComGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    if gdSelected in State then DailyComGrid.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True)
        else DailyComGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TCommon.SelectionColor, clBlack, clWhite, True);

end;


procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    if ARow = 0 then Exit();

    OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);

    if (ACol = OpenItemsGrid.GetCol(TReturnOpenItems._OpenCurAmount)) or (ACol = OpenItemsGrid.GetCol(TReturnOpenItems._OpenAmount))
        or (ACol = OpenItemsGrid.GetCol(TReturnOpenItems._CurAmount)) or (ACol = OpenItemsGrid.GetCol(TReturnOpenItems._Amount))
        or (ACol = OpenItemsGrid.GetCol(TReturnOpenItems._PmtStatus)) then
    begin
        if gdSelected in State then OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;


procedure TActionsForm.DailyComGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
    DailyCom.Text:=DailyComGrid.Cells[DailyComGrid.GetCol(TUserDailyCommentsList._UserComment), ARow];
end;


procedure TActionsForm.selSendFromSelect(Sender: TObject);
begin
    FLbuSendFrom:=selSendFrom.Text;
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
    THelpers.WndCall(CalendarForm, TWindowState.Modal, ActionsForm);
end;


procedure TActionsForm.btnClearFollowUpClick(Sender: TObject);
begin
    ClearFollowUp();
end;


procedure TActionsForm.btnLogMissingInvClick(Sender: TObject);
begin
    //QmsForm.IsMissing:=True;
    //THelpers.WndCall(QmsForm, Modal, ActionsForm);
    THelpers.MsgCall(TAppMessage.Warn, 'This feature is disabled in beta version.');
end;


procedure TActionsForm.btnLogNowClick(Sender: TObject);
begin
    //QmsForm.IsMissing:=False;
    //THelpers.WndCall(QmsForm, Modal, ActionsForm);
    THelpers.MsgCall(TAppMessage.Warn, 'This feature is disabled in beta version.');
end;


procedure TActionsForm.btnCustomStatementClick(Sender: TObject);
begin

    if Service.GetUserPermission(TModules.Documents) <> TPermissions.ReadWrite then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    THelpers.WndCall(SendForm, TWindowState.Modal, ActionsForm);

end;


procedure TActionsForm.btnAutoStatementClick(Sender: TObject);
begin

    if Service.GetUserPermission(TModules.Documents) <> TPermissions.ReadWrite then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    if THelpers.MsgCall(Question2, 'Do you want to send it, right now?') = IDNO then Exit();

    OpenItemsRefs.InitWith(ActionsForm.OpenItemsGrid);
    CtrlStatusRefs.InitWith(MainForm.sgControlStatus);

    FPayLoad.Layout        :=TDocMode.Defined;
    FPayLoad.Subject       :='Account Statement';
    FPayLoad.Message       :='';
    FPayLoad.InvFilter     :=TInvoiceFilter.ShowAllItems;
    FPayLoad.BeginDate     :='';
    FPayLoad.EndDate       :='';
    FPayLoad.SendFrom      :=LbuSendFrom;
    FPayLoad.MailTo        :=TArray<string>.Create(Cust_Mail.Text);
    FPayLoad.SourceDBName  :=SourceDBName;
    FPayLoad.CustNumber    :=CustNumber;
    FPayLoad.CustName      :=CustName;
    FPayLoad.LBUName       :=LbuName;
    FPayLoad.LBUAddress    :=LbuAddress;
    FPayLoad.Telephone     :=LbuPhones;
    FPayLoad.BankDetails   :=LbuBanksHtml;
    FPayLoad.Exclusions    :=Exclusions;
    FPayLoad.Series        :=False;
    FPayLoad.ItemNo        :=0;
    FPayLoad.OpenItems     :=ActionsForm.OpenItemsGrid;
    FPayLoad.OpenItemsRefs :=OpenItemsRefs;
    FPayLoad.ControlStatus :=MainForm.sgControlStatus;
    FPayLoad.CtrlStatusRefs:=CtrlStatusRefs;
    FPayLoad.IsCtrlStatus  :=ActionsForm.cbCtrlStatusOff.Checked;
    FPayLoad.IsUserInCopy  :=ActionsForm.cbUserInCopy.Checked;

    Screen.Cursor:=crHourGlass;
    Service.Mediator.Documents.SendAccDocumentAsync(MainForm.LoadedAgeDate, FPayLoad, SendAccDocumentAsync_Callback);

end;


procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin

    if Service.GetUserPermission(TModules.Calling) <> TPermissions.ReadWrite then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'You do not have permission to access this feature.');
        Exit();
    end;

    MakePhoneCall();

end;


procedure TActionsForm.btnGoogleItClick(Sender: TObject);
begin

    var Settings: ISettings:=TSettings.Create();
    var AppParam:='https://google.com/search?q=' + TNetEncoding.URL.Encode(Cust_Name.Caption);

    ShellExecute(
        ActionsForm.Handle,
        'open',
        PChar(Settings.DirApplication + TCommon.UnityReader),
        PChar(AppParam),
        nil,
        SW_SHOWNORMAL
    );

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
    THelpers.WndCall(PhoneListForm, TWindowState.Modal, ActionsForm);
end;


procedure TActionsForm.btnSaveCustDetailsClick(Sender: TObject);
begin
    SaveCustomerDetails();
end;


procedure TActionsForm.btnAddCommentClick(Sender: TObject);
begin

    var LDailyCommentFields: TDailyCommentFields;
    LDailyCommentFields.CommentId           :=0;
    LDailyCommentFields.SourceDBName        :=SourceDBName;
    LDailyCommentFields.CustomerNumber      :=CustNumber;
    LDailyCommentFields.AgeDate             :=MainForm.LoadedAgeDate;
    LDailyCommentFields.CallEvent           :=0;
    LDailyCommentFields.CallDuration        :=0;
    LDailyCommentFields.FixedStatementsSent :=0;
    LDailyCommentFields.CustomStatementsSent:=0;
    LDailyCommentFields.FixedRemindersSent  :=0;
    LDailyCommentFields.CustomRemindersSent :=0;
    LDailyCommentFields.UserComment         :='New action...';
    LDailyCommentFields.UserAlias           :=Service.SessionData.AliasName;

    Service.Mediator.Comments.EditDailyCommentAsync(LDailyCommentFields, EditDailyComment_Callback);

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


procedure TActionsForm.DailyComGridMouseEnter(Sender: TObject);
begin
    if (DailyComGrid.Enabled) and (DailyComGrid.Visible) then DailyComGrid.SetFocus();
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


procedure TActionsForm.DailyComGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    DailyComGrid.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;


procedure TActionsForm.DailyComGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    Handled:=True;
    DailyComGrid.Perform(WM_VSCROLL, SB_LINEUP, 0);
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
    txtDesc.Caption:='Send custom e-mail with account statement.';
    txtCustomStatement.Font.Color:=AppButtonTxtSelected;
end;


procedure TActionsForm.btnCustomStatementMouseLeave(Sender: TObject);
begin
    txtDesc.Caption:='';
    txtCustomStatement.Font.Color:=AppButtonTxtNormal;
end;


procedure TActionsForm.btnAutoStatementMouseEnter(Sender: TObject);
begin
    txtDesc.Caption:='Send account statement now.';
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
    //if (Key = VK_F5) and (Shift=[ssCtrl]) then
end;


procedure TActionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if Service.GetUserPermission(TModules.DailyComment) <> TPermissions.ReadWrite then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'You do not have permission to edit this field.');
        Exit();
    end;

    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        DailyCom.Lines.Add(TChars.CRLF);
        Exit;
    end;

    if ( (Key = VK_RETURN) and (DailyCom.Text <> '') ) then SaveDailyComment();

end;


procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if Service.GetUserPermission(TModules.GeneralComment) <> TPermissions.ReadWrite then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'You do not have permission to edit this field.');
        Exit();
    end;

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


procedure TActionsForm.DailyComGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then DailyComGrid.CopyCutPaste(TActions.Copy);
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

