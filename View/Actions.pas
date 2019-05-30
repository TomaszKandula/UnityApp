unit Actions;


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
    InterposerClasses,
    Helpers;


type


    TActionsForm = class(TForm)
        OpenItemsGrid: TStringGrid;
        DailyCom: TMemo;
        HistoryGrid: TStringGrid;
        btnCallCustomer: TSpeedButton;
        btnNext: TSpeedButton;
        GeneralCom: TMemo;
        zText1: TLabel;
        zText2: TLabel;
        zText7: TLabel;
        zText8: TLabel;
        zText3: TLabel;
        Cust_Name: TLabel;
        Cust_Number: TLabel;
        btnAutoStatement: TSpeedButton;
        PanelGrid: TPanel;
        PanelActions: TPanel;
        PanelHeader: TPanel;
        Cust_Person: TEdit;
        Cust_Mail: TEdit;
        HistoryPanel: TPanel;
        HistoryTitle: TLabel;
        DailyPanel: TPanel;
        DailyTitle: TLabel;
        GeneralPanel: TPanel;
        GeneralTitle: TLabel;
        btnSetFollowUp: TSpeedButton;
        btnClearFollowUp: TSpeedButton;
        btnCustomStatement: TSpeedButton;
        Cust_Phone: TComboBox;
        GroupDetails: TGroupBox;
        btnSaveCustDetails: TSpeedButton;
        Cust_MailBack: TShape;
        Cust_PersonBack: TShape;
        Cust_NumberBack: TShape;
        Cust_NameBack: TShape;
        btnBack: TSpeedButton;
        btnEdit: TSpeedButton;
        btnCopyCustName: TSpeedButton;
        btnCopyCustNumber: TSpeedButton;
        btnCopyPerson: TSpeedButton;
        btnCopyEmail: TSpeedButton;
        PanelStatusBar: TPanel;
        MasterPanel: TPanel;
        Text: TLabel;
        SimpleText: TLabel;
        zText9: TLabel;
        Cust_MailGeneral: TEdit;
        Cust_MailGeneralBack: TShape;
        btnCopyGeneralMail: TSpeedButton;
        btnLogNow: TSpeedButton;
        btnLogMissingInv: TSpeedButton;
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
        SepLine6: TBevel;
        ItemDesc: TLabel;
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
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure btnNextClick(Sender: TObject);
        procedure btnAutoStatementClick(Sender: TObject);
        procedure HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
        procedure HistoryGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure HistoryGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnCallCustomerClick(Sender: TObject);
        procedure DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnSetFollowUpClick(Sender: TObject);
        procedure btnClearFollowUpClick(Sender: TObject);
        procedure btnCustomStatementClick(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure btnSaveCustDetailsClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure btnBackClick(Sender: TObject);
        procedure btnCopyCustNameClick(Sender: TObject);
        procedure btnCopyCustNumberClick(Sender: TObject);
        procedure btnCopyPersonClick(Sender: TObject);
        procedure btnCopyEmailClick(Sender: TObject);
        procedure btnEditClick(Sender: TObject);
        procedure btnCopyGeneralMailClick(Sender: TObject);
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
        procedure btnLogMissingInvClick(Sender: TObject);
        procedure btnLogNowClick(Sender: TObject);
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
        procedure btnSaveCustDetailsMouseEnter(Sender: TObject);
        procedure btnSaveCustDetailsMouseLeave(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    protected
        var SrcColumns:  TAIntigers;
    private
        type TOpenItemsTotal = record
            OpenAm:     double;
            Am:         double;
            OpenCurAm:  double;
            CurAm:      double;
        end;
        var FHistoryGrid:  boolean;
        var FCUID:         string;
        var FSCUID:        string;
        var FBranch:       string;
        var FBanksHtml:    string;
        var FCoCode:       string;
        var FCustName:     string;
        var FCustNumber:   string;
        var FLbuName:      string;
        var FLbuAddress:   string;
        var FLbuPhone:     string;
        var FLbuSendFrom:  string;
        procedure GetAndDisplay;
        function  GetRunningApps(SearchName: string): boolean;
        procedure UpdateOpenItems(OpenItemsDest, OpenItemsSrc: TStringGrid);
        procedure UpdateDetails(CustPerson: TEdit; CustMail: TEdit; CustMailGen: TEdit; CustPhone: TComboBox);
        procedure UpdateGeneral(var Text: TMemo);
        procedure GetFirstComment(var Text: TMemo);
        procedure SetControls;
        procedure Initialize;
        procedure ClearAll;
        procedure MakePhoneCall;
        procedure LoadCustomer(GoNext: boolean);
        procedure ClearFollowUp;
        procedure SaveCustomerDetails;
        procedure SaveGeneralComment;
        procedure SaveDailyComment;
        procedure InitializePanels;
        procedure InitializeSpeedButtons;
    public
        var OpenItemsTotal: TOpenItemsTotal;
        property CUID:         string read FCUID;
        property SCUID:        string read FSCUID;
        property Branch:       string read FBranch;
        property CoCode:       string read FCoCode;
        property CustName:     string read FCustName;
        property CustNumber:   string read FCustNumber;
        property LbuName:      string read FLbuName;
        property LbuAddress:   string read FLbuAddress;
        property LbuPhone:     string read FLbuPhone;
        property LbuSendFrom:  string read FLbuSendFrom;
        property BanksHtml:    string read FBanksHtml;
        procedure UpdateHistory(var Grid: TStringGrid);
    end;


var
    ActionsForm: TActionsForm;


implementation


uses
    Main,
    Qms,
    SqlHandler,
    DbModel,
    Worker,
    Calendar,
    Settings,
    Mailer,
    Transactions,
    Send,
    PhoneList;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


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


procedure TActionsForm.GetAndDisplay;
begin

    Screen.Cursor:=crSQLWait;
    OpenItemsGrid.Freeze(True);
    OpenItemsGrid.ClearAll(2, 1, 1, False);
    HistoryGrid.Freeze(True);
    HistoryGrid.ClearAll(2, 1, 1, False);

    try

        Cust_Name.Caption  :=CustName;
        Cust_Number.Caption:=CustNumber;

        UpdateOpenItems(OpenItemsGrid, MainForm.sgOpenItems);
        UpdateDetails(Cust_Person, Cust_Mail, Cust_MailGeneral, Cust_Phone);
        UpdateHistory(HistoryGrid);
        UpdateGeneral(GeneralCom);

        ValueOpenAm.Caption:=FormatFloat('#,##0.00', OpenItemsTotal.OpenAm) + ' ' + MainForm.tcCURRENCY.Caption;
        ValueAmount.Caption:=FormatFloat('#,##0.00', OpenItemsTotal.Am) + ' ' + MainForm.tcCURRENCY.Caption;

    finally

        OpenItemsGrid.AutoThumbSize;
        OpenItemsGrid.SetColWidth(10, 20, 400);
        OpenItemsGrid.Freeze(False);

        HistoryGrid.AutoThumbSize;
        HistoryGrid.SetColWidth(10, 20, 400);
        HistoryGrid.Freeze(False);

        Screen.Cursor:=crDefault;

    end;

end;


procedure TActionsForm.UpdateOpenItems(OpenItemsDest: TStringGrid; OpenItemsSrc: TStringGrid);
begin

    {TODO -oTomek -cDatabase : Api call for data}

    var kCNT: integer:=1;

    OpenItemsTotal.OpenAm   :=0;
    OpenItemsTotal.Am       :=0;
    OpenItemsTotal.OpenCurAm:=0;
    OpenItemsTotal.CurAm    :=0;

    // Get columns numbers from source open items string grid
    SrcColumns[0] :=OpenItemsSrc.ReturnColumn(TOpenitems.InvoNo,    1, 1);
    SrcColumns[1] :=OpenItemsSrc.ReturnColumn(TOpenitems.Txt,       1, 1);
    SrcColumns[2] :=OpenItemsSrc.ReturnColumn(TOpenitems.AddTxt,    1, 1);
    SrcColumns[3] :=OpenItemsSrc.ReturnColumn(TOpenitems.OpenAm,    1, 1);
    SrcColumns[4] :=OpenItemsSrc.ReturnColumn(TOpenitems.Am,        1, 1);
    SrcColumns[5] :=OpenItemsSrc.ReturnColumn(TOpenitems.OpenCurAm, 1, 1);
    SrcColumns[6] :=OpenItemsSrc.ReturnColumn(TOpenitems.CurAm,     1, 1);
    SrcColumns[7] :=OpenItemsSrc.ReturnColumn(TOpenitems.ISO,       1, 1);
    SrcColumns[8] :=OpenItemsSrc.ReturnColumn(TOpenitems.DueDt,     1, 1);
    SrcColumns[9] :=OpenItemsSrc.ReturnColumn(TOpenitems.ValDt,     1, 1);
    SrcColumns[10]:=OpenItemsSrc.ReturnColumn(TOpenitems.Ctrl,      1, 1);
    SrcColumns[11]:=OpenItemsSrc.ReturnColumn(TOpenitems.PmtStat,   1, 1);

    // Helper columns
    SrcColumns[12]:=OpenItemsSrc.ReturnColumn(TOpenitems.Ad1,       1, 1);
    SrcColumns[13]:=OpenItemsSrc.ReturnColumn(TOpenitems.Ad2,       1, 1);
    SrcColumns[14]:=OpenItemsSrc.ReturnColumn(TOpenitems.Ad3,       1, 1);
    SrcColumns[15]:=OpenItemsSrc.ReturnColumn(TOpenitems.Pno,       1, 1);
    SrcColumns[16]:=OpenItemsSrc.ReturnColumn(TOpenitems.PArea,     1, 1);
    SrcColumns[17]:=OpenItemsSrc.ReturnColumn(TOpenitems.Cuid,      1, 1);

    // Get headers
    for var iCNT: integer:=Low(SrcColumns) to High(SrcColumns) do
    begin

        if SrcColumns[iCNT] = -100 then
        begin
            MainForm.MsgCall(Warn, 'There are no open items loaded. Please reload it or wait untill auto-load is complete and try again.');
            Close;
            Exit;
        end;

        OpenItemsDest.Cells[iCNT + 1, 0]:=OpenItemsSrc.Cells[SrcColumns[iCNT], 0];

    end;

    // Look for the same "CUID" and put it into source grid
    for var iCNT: integer:=1 to OpenItemsSrc.RowCount - 1 do
    begin
        if OpenItemsSrc.Cells[MainForm.sgOpenItems.ReturnColumn(TOpenitems.Cuid, 1, 1), iCNT] = CUID then
        begin

            for var jCNT: integer:=Low(SrcColumns) to High(SrcColumns) do
                OpenItemsDest.Cells[jCNT + 1, kCNT]:=OpenItemsSrc.Cells[SrcColumns[jCNT], iCNT];

            // Aggregate open items
            OpenItemsTotal.OpenAm   :=OpenItemsTotal.OpenAm    + (OpenItemsSrc.Cells[SrcColumns[3], iCNT]).ToDouble;
            OpenItemsTotal.Am       :=OpenItemsTotal.Am        + (OpenItemsSrc.Cells[SrcColumns[4], iCNT]).ToDouble;
            OpenItemsTotal.OpenCurAm:=OpenItemsTotal.OpenCurAm + (OpenItemsSrc.Cells[SrcColumns[5], iCNT]).ToDouble;
            OpenItemsTotal.CurAm    :=OpenItemsTotal.CurAm     + (OpenItemsSrc.Cells[SrcColumns[6], iCNT]).ToDouble;

            inc(kCNT);
            OpenItemsDest.RowCount:=kCNT;

        end;
    end;

    // Hide helpers columns from string grid
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Ad1,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Ad2,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Ad3,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Pno,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.PArea, 1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Cuid,  1, 1)]:=OpenItemsDest.sgRowHidden;

    // Sort via payment status
    OpenItemsDest.MSort(OpenItemsDest.ReturnColumn(TOpenitems.PmtStat, 1, 1), TSorting.IntType, True);

end;


procedure TActionsForm.UpdateDetails(CustPerson: TEdit; CustMail: TEdit; CustMailGen: TEdit; CustPhone: TComboBox); {refactor / async}
begin

    var Tables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    try

        // Get data from Address Book table
        Tables.Columns.Add(TAddressBook.Contact);
        Tables.Columns.Add(TAddressBook.Emails);
        Tables.Columns.Add(TAddressBook.Estatements);
        Tables.Columns.Add(TAddressBook.PhoneNumbers);
        Tables.CustFilter:=TSql.WHERE + TAddressBook.Scuid + TSql.EQUAL + QuotedStr(SCUID);
        Tables.OpenTable(TAddressBook.AddressBook);

        if Tables.DataSet.RecordCount = 1 then
        begin

            var Phones: string;

            CustPerson.Text :=MainForm.OleGetStr(Tables.DataSet.Fields[TAddressBook.Contact].Value);
            CustMailGen.Text:=MainForm.OleGetStr(Tables.DataSet.Fields[TAddressBook.Emails].Value);
            CustMail.Text   :=MainForm.OleGetStr(Tables.DataSet.Fields[TAddressBook.Estatements].Value);
            Phones          :=MainForm.OleGetStr(Tables.DataSet.Fields[TAddressBook.PhoneNumbers].Value);

            if (Phones <> '') or (Phones <> ' ') then
            begin
                CustPhone.Clear;
                CustPhone.Items.Text:=MainForm.Explode(Phones, TDelimiters.Semicolon);
                CustPhone.ItemIndex:=0;
            end;

        end;

        Tables.CleanUp;

        // Get data from Company Data table
        Tables.Columns.Add(TCompanyData.CoName);
        Tables.Columns.Add(TCompanyData.CoAddress);
        Tables.Columns.Add(TCompanyData.TelephoneNumbers);
        Tables.Columns.Add(TCompanyData.SendNoteFrom);
        Tables.Columns.Add(TCompanyData.BankAccounts);
        Tables.CustFilter:=TSql.WHERE + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(CoCode) + TSql._AND + TCompanyData.Branch + TSql.EQUAL + QuotedStr(Branch);
        Tables.OpenTable(TCompanyData.CompanyData);

        if Tables.DataSet.RecordCount = 1 then
        begin
            FLbuName    :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompanyData.CoName].Value);
            FLbuAddress :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompanyData.CoAddress].Value);
            FLbuPhone   :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompanyData.TelephoneNumbers].Value);
            FLbuSendFrom:=MainForm.OleGetStr(Tables.DataSet.Fields[TCompanyData.SendNoteFrom].Value);
            FBanksHtml  :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompanyData.BankAccounts].Value);
        end

    finally
        Tables.Free;
    end;

end;


procedure TActionsForm.UpdateHistory(var Grid: TStringGrid); {refactor / async}
begin

    var DailyText: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        DailyText.Columns.Add(TDailyComment.AgeDate);
        DailyText.Columns.Add(TDailyComment.Stamp);
        DailyText.Columns.Add(TDailyComment.UserAlias);
        DailyText.Columns.Add(TDailyComment.FixedComment);
        DailyText.CustFilter:=TSql.WHERE + TDailyComment.Cuid + TSql.EQUAL + QuotedStr(CUID);
        DailyText.OpenTable(TDailyComment.DailyComment);
        DailyText.DataSet.Sort:=TDailyComment.Stamp + TSql.DESC;

        if not (DailyText.DataSet.EOF) then
        begin
            DailyText.SqlToGrid(Grid, DailyText.DataSet, False, True);
            Grid.ColWidths[Grid.ReturnColumn(TDailyComment.FixedComment, 1, 1)]:=Grid.sgRowHidden;
            Grid.SetColWidth(10, 20, 400);
            FHistoryGrid:=True;
            Grid.Visible:=FHistoryGrid;
        end
        else
        begin
            FHistoryGrid:=False;
        end;

    finally
        DailyText.Free;
    end;

end;


procedure TActionsForm.UpdateGeneral(var Text: TMemo); {refactor / async}
begin

    var GenText: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        GenText.CustFilter:=TSql.WHERE + TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(CUID);
        GenText.OpenTable(TGeneralComment.GeneralComment);

        if not (GenText.DataSet.EOF) then
            Text.Text:=MainForm.OleGetStr(GenText.DataSet.Fields[TGeneralComment.FixedComment].Value);

    finally
        GenText.Free;
    end;

end;


procedure TActionsForm.GetFirstComment(var Text: TMemo);
begin

    if not(Text.Visible) then Exit;
    var GetColumn: integer:=HistoryGrid.ReturnColumn(TDailyComment.FixedComment, 1, 1);
    if GetColumn <> -100 then
        Text.Text:=HistoryGrid.Cells[GetColumn, 1{fixed first row}];

end;


/// <summary>
/// Enable/disable controls.
/// </summary>

procedure TActionsForm.SetControls;
begin

    // Cover save button if customer is not registered.
    if
        (Cust_Person.Text = TNaVariants.NotFound)
    or
        (Cust_Mail.Text = TNaVariants.NotFound)
    or
        (Cust_MailGeneral.Text = TNaVariants.NotFound)
    then
        imgCoverSaveBtn.Visible:=True
            else
                imgCoverSaveBtn.Visible:=False;

    // Disable text fields if customer is not registered.
    if Cust_Phone.Text = TNaVariants.NotFound then
        Cust_Phone.Enabled:=False
            else
                Cust_Phone.Enabled:=True;

    if Cust_Mail.Text = TNaVariants.NotFound then
        Cust_Mail.Enabled:=False
            else
                Cust_Mail.Enabled:=True;

    if Cust_Person.Text = TNaVariants.NotFound then
        Cust_Person.Enabled:=False
            else
                Cust_Person.Enabled:=True;

    if Cust_MailGeneral.Text = TNaVariants.NotFound then
        Cust_MailGeneral.Enabled:=False
            else
                Cust_MailGeneral.Enabled:=True;

end;


procedure TActionsForm.Initialize;
begin
    ClearAll;
    // Assign data for selected row
    FCUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCuid,          1, 1), MainForm.sgAgeView.Row];
    FCustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCustomerName,  1, 1), MainForm.sgAgeView.Row];
    FCustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber,1, 1), MainForm.sgAgeView.Row];
    FCoCode    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCoCode,        1, 1), MainForm.sgAgeView.Row];
    FBranch    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAgent,         1, 1), MainForm.sgAgeView.Row];
    FSCUID     :=CustNumber + MainForm.ConvertCoCode(CoCode, 'F', 3);
end;


procedure TActionsForm.ClearAll;
begin
    Cust_Name.Caption    :=TNaVariants.NotFound;
    Cust_Number.Caption  :=TNaVariants.NotFound;
    Cust_Person.Text     :=TNaVariants.NotFound;
    Cust_Mail.Text       :=TNaVariants.NotFound;
    Cust_MailGeneral.Text:=TNaVariants.NotFound;
    Cust_Phone.Clear;
    Cust_Phone.Items.Add(TNaVariants.NotFound);
    Cust_Phone.ItemIndex:=0;
    DailyCom.Text       :='';
    GeneralCom.Text     :='';
end;


procedure TActionsForm.MakePhoneCall;
begin

    // Check for 'Lynccall.exe'
    var Settings: ISettings:=TSettings.Create;
    if not FileExists(Settings.GetAppDir + TUnityApp.LyncCall) then
    begin
        MainForm.MsgCall(Error, TUnityApp.APPCAPTION + ' cannot find ''lynccall.exe''. Please contact IT support.');
        Exit;
    end;

    // CHeck if Lync/Skype is running
    if not ActionsForm.GetRunningApps('lync.exe') then
    begin
        MainForm.MsgCall(Error, TUnityApp.APPCAPTION + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
        Exit;
    end;

    // Run Lync with given phone number
    ShellExecute(ActionsForm.Handle, 'open', PChar(Settings.GetAppDir + TUnityApp.LyncCall), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);

    if ActionsForm.DailyCom.Text = '' then
    begin
        ActionsForm.DailyCom.Text:='Called customer today.';
        SaveDailyComment;
    end
    else
    begin
        ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + TUChars.CRLF + 'Called customer today.';
        SaveDailyComment;
    end;

end;


procedure TActionsForm.LoadCustomer(GoNext: boolean);

    function CheckRow(iterator: integer): boolean;
    begin
        Result:=True;
        if
            (MainForm.sgAgeView.RowHeights[iterator] <> -1)
        and
            (MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fOverdue, 1, 1), iterator] <> '0')
        then
        begin
            MainForm.sgAgeView.Row:=iterator;
            Result:=False;
        end;
    end;

begin

    // To next (skip hiden)
    if GoNext then
        for var iCNT: integer:=(MainForm.sgAgeView.Row - 1) Downto 1 do
            if not CheckRow(iCNT) then Break;

    // To previous (skip hiden)
    if not GoNext then
        for var iCNT: integer:=(MainForm.sgAgeView.Row + 1) to MainForm.sgAgeView.RowCount - 1 do
            if not CheckRow(iCNT) then Break;

    // Get data
    Initialize;
    try
        GetAndDisplay;
        SetControls;
        HistoryGrid.Visible:=FHistoryGrid;
        GetFirstComment(DailyCom);
        UpdateGeneral(GeneralCom);
    except
        MainForm.MsgCall(Warn, 'Unexpected error has occured. Please close the window and try again.');
    end;

end;


procedure TActionsForm.ClearFollowUp;
begin
    if MainForm.MsgCall(Question2, 'Are you sure you want to clear this follow up?') = ID_YES then
    begin
        TTGeneralComment.Create(
            CUID,
            TNaVariants.NULL,
            TUChars.SPACE,
            TNaVariants.NULL,
            TNaVariants.NULL,
            TNaVariants.NULL,
            True
        );
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1), MainForm.sgAgeView.Row]:='';
    end;
end;


procedure TActionsForm.SaveCustomerDetails;
begin
    TTAddressBook.Create(
        TEnums.TActionTask.adUpdate,
        nil,
        SCUID,
        Cust_Person.Text,
        Cust_Mail.Text,
        Cust_MailGeneral.Text,
        MainForm.Implode(Cust_Phone.Items, TDelimiters.Semicolon),
        ''
    );
end;


procedure TActionsForm.SaveGeneralComment;
begin
    TTGeneralComment.Create(
        CUID,
        GeneralCom.Text,
        TNaVariants.NULL,
        TNaVariants.NULL,
        TNaVariants.NULL,
        TNaVariants.NULL,
        True
    );
end;


/// <summary>
/// Save daily comment into database (use locking thread by default).
/// </summary>

procedure TActionsForm.SaveDailyComment;
begin
    TTDailyComment.Create(
        CUID,
        False,
        False,
        0,
        DailyCom.Text,
        False,
        False,
        False,
        True
    );
end;


procedure TActionsForm.InitializePanels;
begin
    {PanelTop.PanelBorders(clWhite, clSkyBlue, clWhite, clWhite, clWhite);}
end;


/// <summary>
/// Applay transparency for all speed buttons.
/// </summary>

procedure TActionsForm.InitializeSpeedButtons;
begin
    btnEdit.Glyph.Transparent:=True;
    btnEdit.Glyph.TransparentColor:=clWhite;
    btnSaveCustDetails.Glyph.Transparent:=True;
    btnSaveCustDetails.Glyph.TransparentColor:=clWhite;
    btnBack.Glyph.Transparent:=True;
    btnBack.Glyph.TransparentColor:=clWhite;
    btnNext.Glyph.Transparent:=True;
    btnNext.Glyph.TransparentColor:=clWhite;
    btnSetFollowUp.Glyph.Transparent:=True;
    btnSetFollowUp.Glyph.TransparentColor:=clWhite;
    btnClearFollowUp.Glyph.Transparent:=True;
    btnClearFollowUp.Glyph.TransparentColor:=clWhite;
    btnLogNow.Glyph.Transparent:=True;
    btnLogNow.Glyph.TransparentColor:=clWhite;
    btnLogMissingInv.Glyph.Transparent:=True;
    btnLogMissingInv.Glyph.TransparentColor:=clWhite;
    btnCustomStatement.Glyph.Transparent:=True;
    btnCustomStatement.Glyph.TransparentColor:=clWhite;
    btnAutoStatement.Glyph.Transparent:=True;
    btnAutoStatement.Glyph.TransparentColor:=clWhite;
    btnCallCustomer.Glyph.Transparent:=True;
    btnCallCustomer.Glyph.TransparentColor:=clWhite;
    btnCopyCustName.Glyph.Transparent:=True;
    btnCopyCustName.Glyph.TransparentColor:=clWhite;
    btnCopyCustNumber.Glyph.Transparent:=True;
    btnCopyCustNumber.Glyph.TransparentColor:=clWhite;
    btnCopyPerson.Glyph.Transparent:=True;
    btnCopyPerson.Glyph.TransparentColor:=clWhite;
    btnCopyEmail.Glyph.Transparent:=True;
    btnCopyEmail.Glyph.TransparentColor:=clWhite;
    btnCopyGeneralMail.Glyph.Transparent:=True;
    btnCopyGeneralMail.Glyph.TransparentColor:=clWhite;
end;


// --------------------------------------------------------------------------------------------------------------------------------------- MAIN CLASS EVENTS //


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TActionsForm.FormCreate(Sender: TObject);
begin
    SetLength(SrcColumns, 19);
    OpenItemsGrid.ColCount:=19;
    OpenItemsGrid.SetRowHeight(OpenItemsGrid.sgRowHeight, 25);
    HistoryGrid.ColCount:=11;
    HistoryGrid.SetRowHeight(OpenItemsGrid.sgRowHeight, 25);
    HistoryGrid.Visible:=False;
    InitializePanels;
    InitializeSpeedButtons;
    ItemDesc.Caption:='';
end;


procedure TActionsForm.FormShow(Sender: TObject);
begin
    Initialize;
end;


procedure TActionsForm.FormActivate(Sender: TObject);
begin
    if MainForm.IsConnected then
    begin
        GetAndDisplay;
        SimpleText.Caption:=MainForm.OpenItemsUpdate;
        SetControls;
        GetFirstComment(DailyCom);
        UpdateGeneral(GeneralCom);
    end
    else
    begin
        MainForm.MsgCall(Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Close;
    end;
end;


procedure TActionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    CanClose:=True;
end;


procedure TActionsForm.FormDestroy(Sender: TObject);
begin
    // Do nothing
end;


// ---------------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS //


/// <summary>
/// Draw selected row on string grid component.
/// </summary>

procedure TActionsForm.HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TUnityApp.SELCOLOR, clBlack, clWhite, True);
end;


/// <summary>
/// Color numbers and selection on string grid.
/// </summary>

procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    // Skip header
    if ARow = 0 then Exit;

    // Draw selected
    OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TUnityApp.SELCOLOR, clBlack, clWhite, True);

    // Draw certain color
    if
        (
            ACol = OpenItemsGrid.ReturnColumn(TOpenitems.OpenCurAm, 1, 1)
        )
    or
        (
            ACol = OpenItemsGrid.ReturnColumn(TOpenitems.OpenAm, 1, 1)
        )
    or
        (
            ACol = OpenItemsGrid.ReturnColumn(TOpenitems.CurAm, 1, 1)
        )
    or
        (
            ACol = OpenItemsGrid.ReturnColumn(TOpenitems.Am, 1, 1)
        )
    or
        (
            ACol = OpenItemsGrid.ReturnColumn(TOpenitems.PmtStat, 1, 1)
        )
    then
        OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);

end;


/// <summary>
/// Show data when user select item on history string gird.
/// </summary>

procedure TActionsForm.HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
    DailyCom.Text:=HistoryGrid.Cells[HistoryGrid.ReturnColumn(TDailyComment.FixedComment, 1, 1), ARow];
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TActionsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = VK_F5) and (Shift=[ssCtrl]) then GetAndDisplay;
end;


procedure TActionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


/// <summary>
/// Save daily comment on <enter>. Allow to enter empty line when ALT + ENTER is pressed.
/// </summary>

procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if MainForm.AccessLevel = 'RO' then
    begin
        MainForm.MsgCall(Warn, 'You do not have permission to write the comment.');
        Exit;
    end;

    // New line
    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        DailyCom.Lines.Add(TUChars.CRLF);
        Exit;
    end;

    // Save to database
    if ( (Key = VK_RETURN) and (DailyCom.Text <> '') ) then SaveDailyComment;

end;


/// <summary>
/// Save general comment on <enter>. Allow to enter empty line when ALT + ENTER is pressed.
/// </summary>

procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if MainForm.AccessLevel = 'RO' then
    begin
        MainForm.MsgCall(Warn, 'You do not have permission to write the comment.');
        Exit;
    end;

    // New line
    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        GeneralCom.Lines.Add(TUChars.CRLF);
        Exit;
    end;

    // Save to database
    if ( (Key = VK_RETURN) and (GeneralCom.Text <> '') ) then SaveGeneralComment;
    
end;


procedure TActionsForm.OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then OpenItemsGrid.CopyCutPaste(TEnums.TActionTask.adCopy);
end;


procedure TActionsForm.HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then HistoryGrid.CopyCutPaste(adCopy);
end;


procedure TActionsForm.DailyComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then GeneralCom.SetFocus;
end;


procedure TActionsForm.GeneralComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then DailyCom.SetFocus;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


// ---------------------------------------------------------------------------------------------------------------------------------- SET FOCUS ON COMPONENT //


procedure TActionsForm.Cust_PhoneMouseEnter(Sender: TObject);
begin
//    if (Cust_Phone.Enabled) and (Cust_Phone.Visible) then Cust_Phone.SetFocus;
end;


procedure TActionsForm.Cust_PersonMouseEnter(Sender: TObject);
begin
//    if (Cust_Person.Enabled) and (Cust_Person.Visible) then Cust_Person.SetFocus;
end;


procedure TActionsForm.Cust_MailMouseEnter(Sender: TObject);
begin
//    if (Cust_Mail.Enabled) and (Cust_Mail.Visible) then Cust_Mail.SetFocus;
end;


procedure TActionsForm.Cust_MailGeneralMouseEnter(Sender: TObject);
begin
//    if (Cust_MailGeneral.Enabled) and (Cust_MailGeneral.Visible) then Cust_MailGeneral.SetFocus;
end;


procedure TActionsForm.OpenItemsGridMouseEnter(Sender: TObject);
begin
//    if (OpenItemsGrid.Enabled) and (OpenItemsGrid.Visible) then OpenItemsGrid.SetFocus;
end;


procedure TActionsForm.HistoryGridMouseEnter(Sender: TObject);
begin
//    if (HistoryGrid.Enabled) and (HistoryGrid.Visible) then HistoryGrid.SetFocus;
end;


procedure TActionsForm.DailyComMouseEnter(Sender: TObject);
begin
//    if (DailyCom.Enabled) and (DailyCom.Visible) then DailyCom.SetFocus;
end;


procedure TActionsForm.GeneralComMouseEnter(Sender: TObject);
begin
//    if (GeneralCom.Enabled) and (GeneralCom.Visible) then GeneralCom.SetFocus;
end;


// --------------------------------------------------------------------------------------------------------------------------------------- SCROLL BARS MOVES //


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


// ---------------------------------------------------------------------------------------------------------------------------------- MOUSE HOOVER | ACTIONS //


procedure TActionsForm.btnBackMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Back to previous customer.';
end;


procedure TActionsForm.btnBackMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnNextMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Load next customer.';
end;


procedure TActionsForm.btnNextMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnSetFollowUpMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Add follow-up date.';
end;


procedure TActionsForm.btnSetFollowUpMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnClearFollowUpMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Remove existing follow-up date.';
end;


procedure TActionsForm.btnClearFollowUpMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnLogMissingInvMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='QMS: Log missing invoice.';
end;


procedure TActionsForm.btnLogMissingInvMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnLogNowMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='QMS: Log selected invoice(s).';
end;


procedure TActionsForm.btnLogNowMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnCustomStatementMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Send custom e-mail with account statement.';
end;


procedure TActionsForm.btnCustomStatementMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnAutoStatementMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Send account statement now.';
end;


procedure TActionsForm.btnAutoStatementMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnCallCustomerMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Call customer now.';
end;


procedure TActionsForm.btnCallCustomerMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


procedure TActionsForm.btnSaveCustDetailsMouseEnter(Sender: TObject);
begin
    ItemDesc.Caption:='Save changes in customer details.';
end;


procedure TActionsForm.btnSaveCustDetailsMouseLeave(Sender: TObject);
begin
    ItemDesc.Caption:='';
end;


// ---------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS | ACTIONS //


procedure TActionsForm.btnEditClick(Sender: TObject);
begin
    MainForm.WndCall(PhoneListForm, Helpers.TWindows.TState.Modal);
end;


procedure TActionsForm.btnSaveCustDetailsClick(Sender: TObject);
begin
    SaveCustomerDetails;
end;


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
    CalendarForm.CalendarMode:=cfDateToDB;
    MainForm.WndCall(CalendarForm, Helpers.TWindows.TState.Modal);
end;


procedure TActionsForm.btnClearFollowUpClick(Sender: TObject);
begin
    ClearFollowUp;
end;


procedure TActionsForm.btnCustomStatementClick(Sender: TObject);
begin
    MainForm.WndCall(SendForm, Helpers.TWindows.TState.Modal);
end;


procedure TActionsForm.btnAutoStatementClick(Sender: TObject);
begin

    if MainForm.MsgCall(Question2, 'Are you absolutely sure you want to send it, right now?') = IDNO
        then Exit;

    MainForm.UpdateOpenItemsRefs(OpenItemsGrid);
    MainForm.UpdateControlStatusRefs(MainForm.sgControlStatus);
    TTSendAccountStatement.Create(
        TDocuments.TMode.maDefined,
        'Account Statement',
        '',
        TInvoiceFilter.ShowAllItems,
        '',
        '',
        OpenItemsGrid,
        CUID,
        LbuSendFrom,
        Cust_Mail.Text,
        CustName,
        CustNumber,
        LbuName,
        LbuAddress,
        LbuPhone,
        BanksHtml
    );

end;


procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
    MakePhoneCall;
end;


// ------------------------------------------------------------------------------------------------------------------------ BUTTON CALLS | COPY TO CLIPBOARD //


procedure TActionsForm.btnCopyCustNameClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Name.Caption;
end;


procedure TActionsForm.btnCopyCustNumberClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Number.Caption;
end;


procedure TActionsForm.btnCopyPersonClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Person.Text;
end;


procedure TActionsForm.btnCopyEmailClick(Sender: TObject);
begin
    ClipBoard.AsText:=Cust_Mail.Text;
end;


procedure TActionsForm.btnCopyGeneralMailClick(Sender: TObject);
begin
    Clipboard.AsText:=Cust_MailGeneral.Text;
end;


// -------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS | QMS //


procedure TActionsForm.btnLogMissingInvClick(Sender: TObject);
begin
    QmsForm.IsMissing:=True;
    MainForm.WndCall(QmsForm, Modal);
end;


procedure TActionsForm.btnLogNowClick(Sender: TObject);
begin
    QmsForm.IsMissing:=False;
    MainForm.WndCall(QmsForm, Modal);
end;


end.

