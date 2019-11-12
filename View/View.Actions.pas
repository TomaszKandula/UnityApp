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
    Unity.Grid;


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
    strict private
        var FHistoryGrid:          boolean;
        var FCUID:                 string;
        var FSCUID:                string;
        var FBranch:               string;
        var FBanksHtml:            string;
        var FCoCode:               string;
        var FCustName:             string;
        var FCustNumber:           string;
        var FLbuName:              string;
        var FLbuAddress:           string;
        var FLbuPhone:             string;
        var FLbuSendFrom:          string;
        var FSrcColumns:           TAIntigers;
        var FAbUpdateFields:       TAddressBookUpdateFields;
        var FDailyCommentFields:   TDailyCommentFields;
        var FGeneralCommentFields: TGeneralCommentFields;
        var FOpenItemsTotal:       TOpenItemsTotal;
        var FPayLoad:              TAccountStatementPayLoad;
        procedure GetAndDisplay;
        function  GetRunningApps(SearchName: string): boolean;
        procedure UpdateOpenItems(OpenItemsDest, OpenItemsSrc: TStringGrid);
        procedure UpdateCustDetails();
        procedure UpdateCompanyDetails();
        procedure UpdateGeneral(var Text: TMemo);
        procedure UpdateHistory(var HistoryGrid: TStringGrid);
        procedure GetFirstComment(var Text: TMemo);
        procedure SetControls();
        procedure Initialize();
        procedure ClearAll();
        procedure MakePhoneCall();
        procedure LoadCustomer(GoNext: boolean);
        procedure ClearFollowUp();
        procedure SaveCustomerDetails();
        procedure SaveGeneralComment();
        procedure SaveDailyComment();
        procedure InitializePanels();
        procedure InitializeSpeedButtons();
        procedure SendAccountStatement_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
        procedure UpdateAddressBook_Callback(CallResponse: TCallResponse);
        procedure EditGeneralComment_Callback(CallResponse: TCallResponse);
        procedure EditDailyComment_Callback(CallResponse: TCallResponse);
        procedure GetCustomerDetails_Callback(CustPerson: string; CustMailGen: string; CustMailStat: string; CustPhones: string; CallResponse: TCallResponse);
        procedure GetCompanyDetails_Callback(LbuName: string; LbuAddress: string; LbuPhone: string; LbuEmail: string; BanksData: string; CallResponse: TCallResponse);
    public
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
    Async.AddressBook,
    Async.Comments,
    Async.Statements;


var vActionsForm: TActionsForm;


function ActionsForm(): TActionsForm;
begin
    if not(Assigned(vActionsForm)) then Application.CreateForm(TActionsForm, vActionsForm);
    Result:=vActionsForm;
end;


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


procedure TActionsForm.GetAndDisplay();
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
        UpdateCustDetails();
        UpdateCompanyDetails();
        UpdateHistory(HistoryGrid);
        UpdateGeneral(GeneralCom);

        ValueOpenAm.Caption:=FormatFloat('#,##0.00', FOpenItemsTotal.OpenAm) + ' ' + MainForm.tcCURRENCY.Caption;
        ValueAmount.Caption:=FormatFloat('#,##0.00', FOpenItemsTotal.Am) + ' ' + MainForm.tcCURRENCY.Caption;

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

    var kCNT: integer:=1;

    FOpenItemsTotal.OpenAm   :=0;
    FOpenItemsTotal.Am       :=0;
    FOpenItemsTotal.OpenCurAm:=0;
    FOpenItemsTotal.CurAm    :=0;

    // Get columns numbers from source open items string grid
    FSrcColumns[0] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.InvoNo,    1, 1);
    FSrcColumns[1] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Txt,       1, 1);
    FSrcColumns[2] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.AddTxt,    1, 1);
    FSrcColumns[3] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.OpenAm,    1, 1);
    FSrcColumns[4] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Am,        1, 1);
    FSrcColumns[5] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.OpenCurAm, 1, 1);
    FSrcColumns[6] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.CurAm,     1, 1);
    FSrcColumns[7] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.ISO,       1, 1);
    FSrcColumns[8] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.DueDt,     1, 1);
    FSrcColumns[9] :=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.ValDt,     1, 1);
    FSrcColumns[10]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Ctrl,      1, 1);
    FSrcColumns[11]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.PmtStat,   1, 1);

    // Helper columns
    FSrcColumns[12]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Ad1,       1, 1);
    FSrcColumns[13]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Ad2,       1, 1);
    FSrcColumns[14]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Ad3,       1, 1);
    FSrcColumns[15]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Pno,       1, 1);
    FSrcColumns[16]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.PArea,     1, 1);
    FSrcColumns[17]:=OpenItemsSrc.ReturnColumn(DbModel.TOpenitems.Cuid,      1, 1);

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

    // Look for the same "CUID" and put it into source grid
    for var iCNT: integer:=1 to OpenItemsSrc.RowCount - 1 do
    begin

        if OpenItemsSrc.Cells[MainForm.sgOpenItems.ReturnColumn(TOpenitems.Cuid, 1, 1), iCNT] = CUID then
        begin

            for var jCNT: integer:=Low(FSrcColumns) to High(FSrcColumns) do
                OpenItemsDest.Cells[jCNT + 1, kCNT]:=OpenItemsSrc.Cells[FSrcColumns[jCNT], iCNT];

            // Aggregate open items
            FOpenItemsTotal.OpenAm   :=FOpenItemsTotal.OpenAm    + (OpenItemsSrc.Cells[FSrcColumns[3], iCNT]).ToDouble;
            FOpenItemsTotal.Am       :=FOpenItemsTotal.Am        + (OpenItemsSrc.Cells[FSrcColumns[4], iCNT]).ToDouble;
            FOpenItemsTotal.OpenCurAm:=FOpenItemsTotal.OpenCurAm + (OpenItemsSrc.Cells[FSrcColumns[5], iCNT]).ToDouble;
            FOpenItemsTotal.CurAm    :=FOpenItemsTotal.CurAm     + (OpenItemsSrc.Cells[FSrcColumns[6], iCNT]).ToDouble;

            inc(kCNT);
            OpenItemsDest.RowCount:=kCNT;

        end;

    end;

    // Hide helpers columns from string grid
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(DbModel.TOpenitems.Ad1,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(DbModel.TOpenitems.Ad2,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(DbModel.TOpenitems.Ad3,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(DbModel.TOpenitems.Pno,   1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(DbModel.TOpenitems.PArea, 1, 1)]:=OpenItemsDest.sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(DbModel.TOpenitems.Cuid,  1, 1)]:=OpenItemsDest.sgRowHidden;

    // Sort via payment status
    OpenItemsDest.MSort(OpenItemsDest.ReturnColumn(TOpenitems.PmtStat, 1, 1), TSorting.TDataType.TInteger, True);

end;


procedure TActionsForm.UpdateCustDetails();
begin
    var AddressBook: IAddressBook:=TAddressBook.Create();
    AddressBook.GetCustomerDetailsAsync(SCUID, GetCustomerDetails_Callback);
end;


procedure TActionsForm.UpdateCompanyDetails();
begin
    var Utilities: IUtilities:=TUtilities.Create();
    Utilities.GetCompanyDetailsAsync(CoCode, Branch, GetCompanyDetails_Callback);
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

        HistoryGrid.ColWidths[HistoryGrid.ReturnColumn(TDailyComment.FixedComment, 1, 1)]:=HistoryGrid.sgRowHidden;
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

    if not(Text.Visible) then Exit;
    var GetColumn: integer:=HistoryGrid.ReturnColumn(TDailyComment.FixedComment, 1, 1);
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
    ClearAll();
    FCUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCuid,          1, 1), MainForm.sgAgeView.Row];
    FCustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCustomerName,  1, 1), MainForm.sgAgeView.Row];
    FCustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCustomerNumber,1, 1), MainForm.sgAgeView.Row];
    FCoCode    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCoCode,        1, 1), MainForm.sgAgeView.Row];
    FBranch    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAgent,         1, 1), MainForm.sgAgeView.Row];
    FSCUID     :=CustNumber + THelpers.ConvertCoCode(CoCode, 'F', 3);
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

    DailyCom.Text       :='';
    GeneralCom.Text     :='';

end;


procedure TActionsForm.MakePhoneCall();
begin

    // Check for 'Lynccall.exe'
    var Settings: ISettings:=TSettings.Create;
    if not FileExists(Settings.DirApplication + 'LyncCall.exe') then
    begin
        THelpers.MsgCall(Error, TCommon.APPCAPTION + ' cannot find ''lynccall.exe''. Please contact IT support.');
        Exit;
    end;

    // CHeck if Lync/Skype is running
    if not ActionsForm.GetRunningApps('lync.exe') then
    begin
        THelpers.MsgCall(Error, TCommon.APPCAPTION + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
        Exit;
    end;

    // Run Lync with given phone number
    ShellExecute(ActionsForm.Handle, 'open', PChar(Settings.DirApplication + 'LyncCall.exe'), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);

    if ActionsForm.DailyCom.Text = '' then
    begin
        ActionsForm.DailyCom.Text:='Called customer today.';
        SaveDailyComment;
    end
    else
    begin
        ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + TChars.CRLF + 'Called customer today.';
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
        HistoryGrid.Visible:=FHistoryGrid;
        {GetFirstComment(DailyCom);}
    except
        THelpers.MsgCall(Warn, 'Unexpected error has occured. Please close the window and try again.');
    end;

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

        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1), MainForm.sgAgeView.Row]:='';

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

    FDailyCommentFields.GroupIdSel   :=MainForm.FGroupIdSel;
    FDailyCommentFields.AgeDateSel   :=MainForm.FAgeDateSel;
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


// ----------------------------------------------------------------------------------------------------------------------------------------------- CALLBACKS //


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


procedure TActionsForm.GetCustomerDetails_Callback(CustPerson: string; CustMailGen: string; CustMailStat: string; CustPhones: string; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log(CallResponse.LastMessage);
        Exit();
    end;

    var Phones: string;

    Cust_Person.Text     :=CustPerson;
    Cust_MailGeneral.Text:=CustMailGen;
    Cust_Mail.Text       :=CustMailStat;
    Phones               :=CustPhones;

    if (Phones <> '') or (Phones <> ' ') then
    begin
        Cust_Phone.Clear();
        Cust_Phone.Items.Text:=THelpers.Explode(Phones, TDelimiters.Semicolon);
        Cust_Phone.ItemIndex:=0;
    end;

    SetControls();

end;


procedure TActionsForm.GetCompanyDetails_Callback(LbuName: string; LbuAddress: string; LbuPhone: string; LbuEmail: string; BanksData: string; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log(CallResponse.LastMessage);
        Exit();
    end;

    FLbuName    :=LbuName;
    FLbuAddress :=LbuAddress;
    FLbuPhone   :=LbuPhone;
    FLbuSendFrom:=LbuEmail;
    FBanksHtml  :=BanksData;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TActionsForm.FormCreate(Sender: TObject);
begin
    SetLength(FSrcColumns, 19);
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
    Initialize();
end;


procedure TActionsForm.FormActivate(Sender: TObject);
begin

    if MainForm.FIsConnected then
    begin
        GetAndDisplay();
        SimpleText.Caption:=MainForm.FOpenItemsUpdate;
        GetFirstComment(DailyCom);
    end
    else
    begin
        THelpers.MsgCall(Error, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Close;
    end;

end;


procedure TActionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    CanClose:=True;
end;


procedure TActionsForm.FormDestroy(Sender: TObject);
begin
    {Do nothing}
end;


// ---------------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS //


procedure TActionsForm.HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TCommon.SelectionColor, clBlack, clWhite, True);
end;


procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    // Skip header
    if ARow = 0 then Exit;

    // Draw selected
    OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clWhite, TCommon.SelectionColor, clBlack, clWhite, True);

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
    begin
        if gdSelected in State then OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clWhite)
            else OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);
    end;

end;


procedure TActionsForm.HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
    DailyCom.Text:=HistoryGrid.Cells[HistoryGrid.ReturnColumn(TDailyComment.FixedComment, 1, 1), ARow];
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TActionsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = VK_F5) and (Shift=[ssCtrl]) then GetAndDisplay();
end;


procedure TActionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if MainForm.FAccessLevel = 'RO' then
    begin
        THelpers.MsgCall(Warn, 'You do not have permission to write the comment.');
        Exit;
    end;

    // New line
    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        DailyCom.Lines.Add(TChars.CRLF);
        Exit;
    end;

    // Save to database
    if ( (Key = VK_RETURN) and (DailyCom.Text <> '') ) then SaveDailyComment();

end;


procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    if MainForm.FAccessLevel = 'RO' then
    begin
        THelpers.MsgCall(Warn, 'You do not have permission to write the comment.');
        Exit;
    end;

    // New line
    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        GeneralCom.Lines.Add(TChars.CRLF);
        Exit;
    end;

    // Save to database
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
    THelpers.WndCall(PhoneListForm, TWindowState.Modal);
end;


procedure TActionsForm.btnSaveCustDetailsClick(Sender: TObject);
begin
    SaveCustomerDetails();
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
    CalendarForm.FCalendarMode:=DateToDB;
    THelpers.WndCall(CalendarForm, TWindowState.Modal);
end;


procedure TActionsForm.btnClearFollowUpClick(Sender: TObject);
begin
    ClearFollowUp();
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

    var Statements: IStatements:=TStatements.Create();
    Statements.SendAccountStatement(MainForm.FAgeDateSel, FPayLoad, SendAccountStatement_Callback);

end;


procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
    MakePhoneCall();
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
    THelpers.WndCall(QmsForm, Modal);
end;


procedure TActionsForm.btnLogNowClick(Sender: TObject);
begin
    QmsForm.IsMissing:=False;
    THelpers.WndCall(QmsForm, Modal);
end;


end.

