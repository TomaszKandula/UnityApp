
 {$I .\Include\Header.inc}

unit Actions;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,  Dialogs, Grids, Buttons, ExtCtrls, ComCtrls, StdCtrls, ADODB, StrUtils,
    ShellApi, TLHelp32, pngimage, ImgList, GIFImg, Clipbrd, InterposerClasses, Arrays;

type

    /// <summary>
    ///     View form class with helpers for Actions window.
    /// </summary>

    TActionsForm = class(TForm)
        OpenItemsGrid: TStringGrid;
        DailyCom: TMemo;
        HistoryGrid: TStringGrid;
        btnCallCustomer: TSpeedButton;
        btnNext: TSpeedButton;
        GeneralCom: TMemo;
        Text4: TLabel;
        Text5: TLabel;
        Text6: TLabel;
        Text7: TLabel;
        Text8: TLabel;
        Cust_Name: TLabel;
        Cust_Number: TLabel;
        btnAutoStatement: TSpeedButton;
        PanelMiddle: TPanel;
        PanelBottom: TPanel;
        PanelTop: TPanel;
        Cust_Person: TEdit;
        Cust_Mail: TEdit;
        ButtonPanel: TPanel;
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
        GroupCustomerDetails: TGroupBox;
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
        TextSave: TLabel;
        MasterPanel: TPanel;
        Text: TLabel;
        SimpleText: TLabel;
        ImgLoadingWindow: TImage;
        btnQMStoggle: TSpeedButton;
        Text9: TLabel;
        Cust_MailGeneral: TEdit;
        Cust_MailGeneralBack: TShape;
        btnCopyGeneralMail: TSpeedButton;
        PanelQMS: TPanel;
        cbStatusQms: TComboBox;
        btnSelectAll: TSpeedButton;
        btnLogNow: TSpeedButton;
        textStatus: TLabel;
        btnLogMissingInv: TSpeedButton;
        HistoryGridBorders: TShape;
        OpenItemsGridBorders: TShape;
        DailyComBorders: TShape;
        GeneralComBorders: TShape;
        procedure FormCreate(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure btnNextClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnAutoStatementClick(Sender: TObject);
        procedure HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
        procedure HistoryGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure HistoryGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
        procedure HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnCallCustomerClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
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
        procedure btnQMStoggleClick(Sender: TObject);
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
    private
        var FHistoryGrid: boolean;
    public
        var CUID       :  string;
        var SCUID      :  string;
        var CoCode     :  string;
        var CustName   :  string;
        var CustNumber :  string;
        var SrcColumns :  TIntigers;
    published
        function  GetRunningApps(SearchName: string): boolean;
        procedure GetData;
        procedure UpdateOpenItems(OpenItemsDest, OpenItemsSrc: TStringGrid);
        procedure UpdateDetails(CustPerson: TEdit; CustMail: TEdit; CustMailGen: TEdit; CustPhone: TComboBox);
        procedure UpdateHistory(var Grid: TStringGrid);
        procedure UpdateGeneral(Text: TMemo);
        procedure SetControls;
        procedure Initialize;
        procedure ClearAll;
        procedure MakePhoneCall;
        procedure LoadCustomer(Direction: integer);
        procedure ClearFollowUp;
        procedure SaveCustomerDetails;
        procedure SaveGeneralComment;
        procedure SaveDailyComment;
        procedure InitializePanels;
        procedure InitializeSpeedButtons;
        procedure LoadingAnimation(GIFImage: TImage; State: integer);
    end;

var
    ActionsForm: TActionsForm;


implementation


uses
    Main, SQL, Model, Worker, Calendar, Settings, Mailer, Transactions, Send, PhoneList;

{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Check if application is running.
/// </summary>

function TActionsForm.GetRunningApps(SearchName: string): boolean;
var
    PE:        TProcessEntry32;
    Snap:      THandle;
    FileName:  string;
begin

    Result:=False;

    // Take snapshots of running applications
    PE.dwSize:=SizeOf(PE);
    Snap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);

    if Snap <> 0 then
    begin
        if Process32First(Snap, PE) then
        begin
            FileName:=string(PE.szExeFile);

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

/// <summary>
///     Get all relevant data.
/// </summary>

procedure TActionsForm.GetData;
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

/// <summary>
///     Get open items from source.
/// </summary>

procedure TActionsForm.UpdateOpenItems(OpenItemsDest: TStringGrid; OpenItemsSrc: TStringGrid);
var
    iCNT  : integer;
    jCNT  : integer;
    kCNT  : integer;
begin

    kCNT:=1;

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
    SrcColumns[17]:=OpenItemsSrc.ReturnColumn(TOpenitems.CUID,      1, 1);

    // Get headers
    for iCNT:=Low(SrcColumns) to High(SrcColumns) do
        OpenItemsDest.Cells[iCNT + 1, 0]:=OpenItemsSrc.Cells[SrcColumns[iCNT], 0];

    // Look for the same "CUID" and put it into source grid
    for iCNT:=1 to OpenItemsSrc.RowCount - 1 do
    begin
        if OpenItemsSrc.Cells[MainForm.sgOpenItems.ReturnColumn(TOpenitems.CUID, 1, 1), iCNT] = CUID then
        begin

            for jCNT:=Low(SrcColumns) to High(SrcColumns) do
                OpenItemsDest.Cells[jCNT + 1, kCNT]:=OpenItemsSrc.Cells[SrcColumns[jCNT], iCNT];

            inc(kCNT);
            OpenItemsDest.RowCount:=kCNT;

        end;
    end;

    // Hide hepers columns
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Ad1,   1, 1)]:=sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Ad2,   1, 1)]:=sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Ad3,   1, 1)]:=sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.Pno,   1, 1)]:=sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.PArea, 1, 1)]:=sgRowHidden;
    OpenItemsDest.ColWidths[OpenItemsDest.ReturnColumn(TOpenitems.CUID,  1, 1)]:=sgRowHidden;

    // Sort via payment status
    OpenItemsDest.MSort(OpenItemsDest.ReturnColumn(TOpenitems.PmtStat, 1, 1), sdtINTEGER, True);

end;

/// <summary>
///     Get customer details.
/// </summary>

procedure TActionsForm.UpdateDetails(CustPerson: TEdit; CustMail: TEdit; CustMailGen: TEdit; CustPhone: TComboBox);
var
    AddrBook  : TDataTables;
    Phones    : string;
begin

    AddrBook:=TDataTables.Create(MainForm.DbConnect);
    try
        AddrBook.Columns.Add(TAddressBook.CONTACT);
        AddrBook.Columns.Add(TAddressBook.EMAILS);
        AddrBook.Columns.Add(TAddressBook.ESTATEMENTS);
        AddrBook.Columns.Add(TAddressBook.PHONE_NUMBERS);
        AddrBook.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
        AddrBook.OpenTable(TblAddressbook);

        if AddrBook.DataSet.RecordCount = 1 then
        begin
            CustPerson.Text :=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.CONTACT].Value);
            CustMailGen.Text:=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.EMAILS].Value);
            CustMail.Text   :=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.ESTATEMENTS].Value);
            Phones          :=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.PHONE_NUMBERS].Value);

            if (Phones <> '') or (Phones <> ' ') then
            begin
                CustPhone.Clear;
                CustPhone.Items.Text:=MainForm.Explode(Phones, deSemicolon);
                CustPhone.ItemIndex:=0;
            end;
        end;

    finally
        AddrBook.Free;
    end;

end;

/// <summary>
///     Refresh history of daily comments.
/// </summary>

procedure TActionsForm.UpdateHistory(var Grid: TStringGrid);
var
    DailyText: TDataTables;
begin

    DailyText:=TDataTables.Create(MainForm.DbConnect);
    try
        DailyText.Columns.Add(TDaily.AGEDATE);
        DailyText.Columns.Add(TDaily.STAMP);
        DailyText.Columns.Add(TDaily.USER_ALIAS);
        DailyText.Columns.Add(TDaily.FIXCOMMENT);
        DailyText.CustFilter:=WHERE + TDaily.CUID + EQUAL + QuotedStr(CUID);
        DailyText.OpenTable(TblDaily);
        DailyText.DataSet.Sort:=TDaily.STAMP + DESC;

        if not (DailyText.DataSet.EOF) then
        begin
            DailyText.SqlToGrid(Grid, DailyText.DataSet, False, True);
            Grid.ColWidths[Grid.ReturnColumn(TDaily.FIXCOMMENT, 1, 1)]:=sgRowHidden;
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

/// <summary>
///     Refresh general comment column.
/// </summary>

procedure TActionsForm.UpdateGeneral(Text: TMemo);
var
    GenText: TDataTables;
begin
    GenText:=TDataTables.Create(MainForm.DbConnect);

    try
        GenText.CustFilter:=WHERE + TGeneral.CUID + EQUAL + QuotedStr(CUID);
        GenText.OpenTable(TblGeneral);

        if not (GenText.DataSet.EOF) then
            Text.Text:=MainForm.OleGetStr(GenText.DataSet.Fields[TGeneral.FIXCOMMENT].Value);

    finally
        GenText.Free;
    end;

end;

/// <summary>
///     Enable/disable controls.
/// </summary>

procedure TActionsForm.SetControls;
begin

    // Disable save button if customer is not registered.
    if
        (Cust_Person.Text = unNotFound)
    or
        (Cust_Mail.Text = unNotFound)
    or
        (Cust_MailGeneral.Text = unNotFound)
    then
        btnSaveCustDetails.Enabled:=False else btnSaveCustDetails.Enabled:=True;

    // Disable text fields if customer is not registered.
    if Cust_Phone.Text = unNotFound then
        Cust_Phone.Enabled:=False
            else
                Cust_Phone.Enabled:=True;

    if Cust_Mail.Text = unNotFound then
        Cust_Mail.Enabled:=False
            else
                Cust_Mail.Enabled:=True;

    if Cust_Person.Text = unNotFound then
        Cust_Person.Enabled:=False
            else
                Cust_Person.Enabled     :=True;

    if Cust_MailGeneral.Text = unNotFound then
        Cust_MailGeneral.Enabled:=False
            else
                Cust_MailGeneral.Enabled:=True;

end;

/// <summary>
///     Prepare for new data load.
/// </summary>

procedure TActionsForm.Initialize;
begin
    ClearAll;
    // Assign data for selected row
    CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), MainForm.sgAgeView.Row];
    CustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), MainForm.sgAgeView.Row];
    CustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), MainForm.sgAgeView.Row];
    CoCode    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), MainForm.sgAgeView.Row];
    SCUID     :=CustNumber + MainForm.ConvertName(CoCode, 'F', 3);
end;

/// <summary>
///     Clear all details displayed on Action window.
/// </summary>

procedure TActionsForm.ClearAll;
begin
    Cust_Name.Caption    :=unNotFound;
    Cust_Number.Caption  :=unNotFound;
    Cust_Person.Text     :=unNotFound;
    Cust_Mail.Text       :=unNotFound;
    Cust_MailGeneral.Text:=unNotFound;
    Cust_Phone.Clear;
    Cust_Phone.Items.Add(unNotFound);
    Cust_Phone.ItemIndex:=0;
    DailyCom.Text       :='';
    GeneralCom.Text     :='';
end;

/// <summary>
///     Make phone call.
/// </summary>

procedure TActionsForm.MakePhoneCall;
var
    Settings:  ISettings;
begin

    Settings:=TSettings.Create;

    // CHECK FOR 'LYNCCALL.EXE'
    if not FileExists(Settings.GetAppDir + LyncCall) then
    begin
        MainForm.MsgCall(mcError, APPCAPTION + ' cannot find ''lynccall.exe''. Please contact IT support.');
        Exit;
    end;

    // CHECK IF LYNC/SKYPE IS RUNNING
    if not ActionsForm.GetRunningApps('lync.exe') then
    begin
        MainForm.MsgCall(mcError, APPCAPTION + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
        Exit;
    end;

    // RUN LYNC WITH GIVEN PHONE NUMBER
    ShellExecute(ActionsForm.Handle, 'open', PChar(Settings.GetAppDir + LyncCall), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);

    if ActionsForm.DailyCom.Text = '' then
    begin
        ActionsForm.DailyCom.Text:='Called customer today.';
        SaveDailyComment;
    end
    else
    begin
        ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + CRLF + 'Called customer today.';
        SaveDailyComment;
    end;

end;

/// <summary>
///     oad next or previous customer.
/// </summary>

procedure TActionsForm.LoadCustomer(Direction: integer);

    var
        iCNT:  integer;

    // NESTED METHOD
    function CheckRow(iterator: integer): boolean;
    begin
        Result:=True;
        if
            (MainForm.sgAgeView.RowHeights[iterator] <> -1)
        and
            (MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fOVERDUE, 1, 1), iterator] <> '0')
        then
        begin
            MainForm.sgAgeView.Row:=iterator;
            Result:=False;
        end;
    end;

begin

    // To next (skip hiden)
    if Direction = loadPrev then
        for iCNT:=(MainForm.sgAgeView.Row - 1) Downto 1 do
            if not CheckRow(iCNT) then Break;

    // To previous (skip hiden)
    if Direction = loadNext then
        for iCNT:=(MainForm.sgAgeView.Row + 1) to MainForm.sgAgeView.RowCount - 1 do
            if not CheckRow(iCNT) then Break;

    // Get data
    Initialize;
    try
        GetData;
        SetControls;
        HistoryGrid.Visible:=FHistoryGrid;

    except
        MainForm.MsgCall(mcWarn, 'Unexpected error has occured. Please close the window and try again.');
    end;

end;

/// <summary>
///     Clear follow-up from given customer.
/// </summary>

procedure TActionsForm.ClearFollowUp;
begin
    if MainForm.MsgCall(mcQuestion2, 'Are you sure you want to clear this follow up?') = ID_YES then
    begin
        TTGeneralComment.Create(
            CUID,
            strNULL,
            SPACE,
            strNULL,
            strNULL,
            True
        );
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneral.fFOLLOWUP, 1, 1), MainForm.sgAgeView.Row]:='';
    end;
end;

/// <summary>
///     Save customer details in Address Book.
/// </summary>

procedure TActionsForm.SaveCustomerDetails;
begin
    TTAddressBook.Create(
        adUpdate,
        nil,
        SCUID,
        Cust_Person.Text,
        Cust_Mail.Text,
        Cust_MailGeneral.Text,
        MainForm.Implode(Cust_Phone.Items, deSemicolon),
        ''
    );
end;

/// <summary>
///     Save general comment into database.
/// </summary>

procedure TActionsForm.SaveGeneralComment;
begin
    TTGeneralComment.Create(
        CUID,
        GeneralCom.Text,
        strNULL,
        strNULL,
        strNULL,
        True
    );
end;

/// <summary>
///     Save daily comment into database (use locking thread by default).
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

/// <summary>
///     Draw panels borders.
/// </summary>

procedure TActionsForm.InitializePanels;
begin
    PanelTop.PanelBorders(clWhite, clSkyBlue, clWhite, clWhite, clWhite);
end;

/// <summary>
///     Applay transparency for all speed buttons.
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

    btnCustomStatement.Glyph.Transparent:=True;
    btnCustomStatement.Glyph.TransparentColor:=clWhite;

    btnAutoStatement.Glyph.Transparent:=True;
    btnAutoStatement.Glyph.TransparentColor:=clWhite;

    btnCallCustomer.Glyph.Transparent:=True;
    btnCallCustomer.Glyph.TransparentColor:=clWhite;
end;

/// <summary>
///     Animation during data loading.
/// </summary>

procedure TActionsForm.LoadingAnimation(GIFImage: TImage; State: Integer);
begin
    if State = AnimationON then
    begin
        MasterPanel.Visible:=False;
        ImgLoadingWindow.Visible:=True;
        ActionsForm.DoubleBuffered:=False;
        (GIFImage.Picture.Graphic as TGIFImage).Animate:=True;
    end;

    if State = AnimationOFF then
    begin
        ActionsForm.DoubleBuffered:=True;
        (GIFImage.Picture.Graphic as TGIFImage).Animate:=False;
        ImgLoadingWindow.Visible:=False;
        MasterPanel.Visible:=True;

        HistoryGrid.Visible:=FHistoryGrid;

    end;
end;


// --------------------------------------------------------------------------------------------------------------------------------------- MAIN CLASS EVENTS //


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TActionsForm.FormCreate(Sender: TObject);
var
    Settings: ISettings;
begin

    Settings:=TSettings.Create;
    ActionsForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_ACTIONS', APPCAPTION);

    SetLength(SrcColumns, 19);

    OpenItemsGrid.ColCount:=19;
    OpenItemsGrid.SetRowHeight(sgRowHeight, 25);

    HistoryGrid.ColCount:=11;
    HistoryGrid.SetRowHeight(sgRowHeight, 25);

    InitializePanels;
    InitializeSpeedButtons;

    //...



end;

procedure TActionsForm.FormShow(Sender: TObject);
begin
    Initialize;
end;

procedure TActionsForm.FormActivate(Sender: TObject);
begin
    LoadingAnimation(ImgLoadingWindow, AnimationON);
    if MainForm.IsConnected then
    begin
        GetData;
        SimpleText.Caption:=MainForm.OpenItemsUpdate;
        SetControls;
        LoadingAnimation(ImgLoadingWindow, AnimationOFF);
    end
    else
    begin
        MainForm.MsgCall(mcError, 'The connection with SQL Server database is lost. Please contact your network administrator.');
        Close;
    end;
end;

procedure TActionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    CanClose:=True;
end;

procedure TActionsForm.FormDestroy(Sender: TObject);
begin
    MasterPanel.Visible:=False;
    ImgLoadingWindow.Visible:=True;
end;


// ---------------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS //


/// <summary>
///     Draw selected row on string grid component.
/// </summary>

procedure TActionsForm.HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

/// <summary>
///     Color numbers and selection on string grid.
/// </summary>

procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

    // Skip heaer
    if ARow = 0 then Exit;

    // Draw selected
    OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);

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
///     Show data when user select item on history string gird.
/// </summary>

procedure TActionsForm.HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
    DailyCom.Text:=HistoryGrid.Cells[HistoryGrid.ReturnColumn(TDaily.FIXCOMMENT, 1, 1), ARow];
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TActionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = ESC then Close;
end;

/// <summary>
///     Save daily comment on <enter>. Allow to enter empty line when ALT + ENTER is pressed.
/// </summary>

procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // New line
    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        DailyCom.Lines.Add(CRLF);
        Exit;
    end;

    // Save to database
    if (Key = VK_RETURN) then
    begin

        if DailyCom.Text <> '' then
            SaveDailyComment;

    end;
end;

/// <summary>
///     Save general comment on <enter>. Allow to enter empty line when ALT + ENTER is pressed.
/// </summary>

procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

    // New line
    if ( (Key = VK_RETURN) and (Shift=[ssALT]) ) or ( (Key = VK_RETURN) and (Shift=[ssShift]) ) then
    begin
        GeneralCom.Lines.Add(CRLF);
        Exit;
    end;

    // Save to database
    if (Key = VK_RETURN) then
    begin

        if GeneralCom.Text <> '' then
            SaveGeneralComment;

    end;

end;

/// <summary>
///     String grid content to clipboard.
/// </summary>

procedure TActionsForm.OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then OpenItemsGrid.CopyCutPaste(adCopy);
end;

/// <summary>
///     String grid content to clipboard.
/// </summary>

procedure TActionsForm.HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then HistoryGrid.CopyCutPaste(adCopy);
end;

/// <summary>
///     Set focus on General Comment edit box.
/// </summary>

procedure TActionsForm.DailyComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then GeneralCom.SetFocus;
end;

/// <summary>
///     Set focus on Daily Comment edit box.
/// </summary>

procedure TActionsForm.GeneralComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then DailyCom.SetFocus;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


// ---------------------------------------------------------------------------------------------------------------------------------- SET FOCUS ON COMPONENT //


procedure TActionsForm.Cust_PhoneMouseEnter(Sender: TObject);
begin
    if (Cust_Phone.Enabled) and (Cust_Phone.Visible) then Cust_Phone.SetFocus;
end;

procedure TActionsForm.Cust_PersonMouseEnter(Sender: TObject);
begin
    if (Cust_Person.Enabled) and (Cust_Person.Visible) then Cust_Person.SetFocus;
end;

procedure TActionsForm.Cust_MailMouseEnter(Sender: TObject);
begin
    if (Cust_Mail.Enabled) and (Cust_Mail.Visible) then Cust_Mail.SetFocus;
end;

procedure TActionsForm.Cust_MailGeneralMouseEnter(Sender: TObject);
begin
    if (Cust_MailGeneral.Enabled) and (Cust_MailGeneral.Visible) then Cust_MailGeneral.SetFocus;
end;

procedure TActionsForm.OpenItemsGridMouseEnter(Sender: TObject);
begin
    if (OpenItemsGrid.Enabled) and (OpenItemsGrid.Visible) then OpenItemsGrid.SetFocus;
end;

procedure TActionsForm.HistoryGridMouseEnter(Sender: TObject);
begin
    if (HistoryGrid.Enabled) and (HistoryGrid.Visible) then HistoryGrid.SetFocus;
end;

procedure TActionsForm.DailyComMouseEnter(Sender: TObject);
begin
    if (DailyCom.Enabled) and (DailyCom.Visible) then DailyCom.SetFocus;
end;

procedure TActionsForm.GeneralComMouseEnter(Sender: TObject);
begin
    if (GeneralCom.Enabled) and (GeneralCom.Visible) then GeneralCom.SetFocus;
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


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //


procedure TActionsForm.btnEditClick(Sender: TObject);
begin
    MainForm.WndCall(PhoneListForm, 0);
end;

procedure TActionsForm.btnSaveCustDetailsClick(Sender: TObject);
begin
    SaveCustomerDetails;
end;

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

procedure TActionsForm.btnQMStoggleClick(Sender: TObject);
begin
    if PanelQMS.Visible then
    begin
        PanelBottom.Visible:=True;
        PanelQMS.Visible:=False;
        OpenItemsGrid.Options:=OpenItemsGrid.Options - [goRowSelect];
    end
    else
    begin
        PanelBottom.Visible:=False;
        PanelQMS.Visible:=True;
        OpenItemsGrid.Options:=OpenItemsGrid.Options + [goRowSelect];
    end;
end;

procedure TActionsForm.btnBackClick(Sender: TObject);
begin
    LoadCustomer(loadPrev);
end;

procedure TActionsForm.btnNextClick(Sender: TObject);
begin
    LoadCustomer(loadNext);
end;

procedure TActionsForm.btnSetFollowUpClick(Sender: TObject);
begin
    CalendarForm.CalendarMode:=cfDateToDB;
    MainForm.WndCall(CalendarForm, 0);
end;

procedure TActionsForm.btnClearFollowUpClick(Sender: TObject);
begin
    ClearFollowUp;
end;

procedure TActionsForm.btnCustomStatementClick(Sender: TObject);
begin
    MainForm.WndCall(SendForm, 0);
end;

procedure TActionsForm.btnAutoStatementClick(Sender: TObject);
begin

    if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure you want to send it, right now?') = IDNO
        then Exit;

    MainForm.UpdateOpenItemsRefs(OpenItemsGrid);
    MainForm.UpdateControlStatusRefs(MainForm.sgControlStatus);
    TTSendAccountStatement.Create(
        maDefined,
        'Account Statement',
        '',
        '',
        False,
        OpenItemsGrid,
        SCUID,
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAGENT,           1, 1), MainForm.sgAgeView.Row]
    );

end;

procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
    MakePhoneCall;
end;


end.
