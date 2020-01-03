unit View.MassMailer;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ComCtrls,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    Vcl.Imaging.pngimage,
    Unity.Grid,
    Unity.Panel,
    Unity.ListView,
    Unity.Records,
    Unity.References;


type


    TMassMailerForm = class(TForm)
        PanelBottom: TPanel;
        Text_Warn: TLabel;
        btnCancel: TSpeedButton;
        btnSendEmail: TSpeedButton;
        PanelClient: TPanel;
        Shape_Banks: TShape;
        Shape_Business: TShape;
        Shape_Customer: TShape;
        Shape_Footer: TShape;
        Shape_Invoices: TShape;
        Shape_Terms: TShape;
        Text_Banks: TLabel;
        Text_Business: TLabel;
        Text_Customer: TLabel;
        Text_Footer: TLabel;
        Text_Invoices: TLabel;
        Text_Terms: TLabel;
        Text2: TLabel;
        PanelMessage: TPanel;
        Text_Message: TMemo;
        PanelList: TPanel;
        CustomerList: TListView;
        PanelSubject: TPanel;
        Text3: TLabel;
        PanelEmailContainer: TPanel;
        Text_Subject: TEdit;
        PanelOption: TPanel;
        Shape_Dates: TShape;
        Shape_Options: TShape;
        Text_Begin: TLabel;
        Text_End: TLabel;
        Text_Options: TLabel;
        ValBeginDate: TLabel;
        ValEndDate: TLabel;
        btnBeginDate: TSpeedButton;
        btnEndDate: TSpeedButton;
        cbNonOverdue: TCheckBox;
        cbOverdueOnly: TCheckBox;
        cbShowAll: TCheckBox;
        DueDateLabel: TLabel;
        ImgCover: TImage;
        btnDelBegin: TSpeedButton;
        btnDelEnd: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnBeginDateClick(Sender: TObject);
        procedure btnEndDateClick(Sender: TObject);
        procedure cbShowAllClick(Sender: TObject);
        procedure cbOverdueOnlyClick(Sender: TObject);
        procedure cbNonOverdueClick(Sender: TObject);
        procedure CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure btnDelBeginClick(Sender: TObject);
        procedure btnDelEndClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
    strict private
        var OpenItemsRefs: TFOpenItemsRefs;
        var CtrlStatusRefs: TFCtrlStatusRefs;
        var FPayLoad: TAccDocumentPayLoad;
        var FItemCount: integer;
        var FIsDataLoaded:  boolean;
        function  GetEmailAddress(Scuid: string): string;
        procedure SetEmailAddresses(List: TListView);
        procedure UpdateCompanyData(Source: TListView);
        procedure ExecuteMailer();
        procedure LoadFromGrid();
    public
        property ItemCount: integer read FItemCount write FItemCount;
        procedure SendAccDocumentAsyncs_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
    end;


    function MassMailerForm(): TMassMailerForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Calendar,
    View.Actions,
    Handler.Sql{Legacy},
    DbModel{Legacy},
    Unity.Enums,
    Unity.Helpers,
    Unity.Settings,
    Unity.Constants,
    Unity.EventLogger,
    Unity.SessionService,
    Async.Utilities,
    Async.Companies,
    Async.AddressBook,
    Async.Documents;


var vMassMailerForm: TMassMailerForm;


function MassMailerForm(): TMassMailerForm;
begin
    if not(Assigned(vMassMailerForm)) then Application.CreateForm(TMassMailerForm, vMassMailerForm);
    Result:=vMassMailerForm;
end;


{$REGION 'LOCAL HELPERS'}


procedure TMassMailerForm.LoadFromGrid();
begin

    var Item: TListItem;
    CustomerList.Clear();

    if (MainForm.sgAgeView.Selection.Top - MainForm.sgAgeView.Selection.Bottom) = 0 then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please select more than one customer.');
        Exit();
    end;

    // Many customers
    for var iCNT: integer:=MainForm.sgAgeView.Selection.Top to MainForm.sgAgeView.Selection.Bottom do
    begin

        if MainForm.sgAgeView.RowHeights[iCNT] <> MainForm.sgAgeView.sgRowHidden then
        begin

            Item:=MassMailerForm.CustomerList.Items.Add();
            Item.Caption:=IntToStr(iCNT);
            Item.SubItems.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCustomerName), iCNT]);
            Item.SubItems.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCustomerNumber), iCNT]);
            Item.SubItems.Add('No');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add('n/a');
            Item.SubItems.Add('n/a');
            Item.SubItems.Add('n/a');
            Item.SubItems.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCoCode), iCNT]);
            Item.SubItems.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fAgent), iCNT]);
            Item.SubItems.Add(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCuid), iCNT]);
            Item.SubItems.Add(
                MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCustomerNumber), iCNT] +
                THelpers.CoConvert(MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCoCode), iCNT])
            );

            Item.SubItems.Add('empty');

        end;

    end;

end;


function TMassMailerForm.GetEmailAddress(Scuid: string): string;
begin
    var AddressBook: IAddressBook:=TAddressBook.Create();
    var CustomerDetails: TCustomerDetails;
    CustomerDetails:=AddressBook.GetCustomerDetailsAwaited(Scuid);
    Result:=CustomerDetails.CustMailStat;
end;


procedure TMassMailerForm.SetEmailAddresses(List: TListView);
begin

    var EmailAddress: string;

    if List.Items.Count > 0 then
    begin
        for var iCNT: integer:=0 to List.Items.Count - 1 do
        begin
            EmailAddress:=GetEmailAddress(List.Items[iCNT].SubItems[11]);

            if not(string.IsNullOrEmpty(EmailAddress)) then
                List.Items[iCNT].SubItems[4]:=EmailAddress

        end;
    end;

end;


procedure TMassMailerForm.UpdateCompanyData(Source: TListView);

    function InputText(Text: string): string;
    begin
        if String.IsNullOrEmpty(Text) then
            Result:=TUnknown.NotFound
        else
            Result:=Text;
    end;

begin

    if Source.Items.Count > 0 then
    begin

        for var iCNT: integer:=0 to Source.Items.Count - 1 do
        begin

            var CoCode: string:=Source.Items[iCNT].SubItems[8];
            var Branch: string:=Source.Items[iCNT].SubItems[9];

//            var Companies: ICompanies:=TCompanies.Create();
//            var CompanyDetails: TCompanyDetails;
//            CompanyDetails:=Companies.GetCompanyDetailsAwaited(CoCode, Branch);
//
//            Source.Items[iCNT].SubItems[5] :=InputText(CompanyDetails.LbuName);
//            Source.Items[iCNT].SubItems[6] :=InputText(CompanyDetails.LbuAddress);
//            Source.Items[iCNT].SubItems[7] :=InputText(CompanyDetails.LbuPhone);
//            Source.Items[iCNT].SubItems[3] :=InputText(CompanyDetails.LbuEmail);
//            Source.Items[iCNT].SubItems[12]:=InputText(CompanyDetails.LbuBanks);

        end;

    end;

end;


procedure TMassMailerForm.ExecuteMailer();
begin

    if (String.IsNullOrEmpty(Text_Subject.Text)) or (String.IsNullOrEmpty(Text_Message.Text))
    then
    begin
        THelpers.MsgCall(Warn, 'Cannot send incomplete form. Please re-check it and try again.');
        Exit();
    end;

    if THelpers.MsgCall(Question2, 'Do you want to send it now?') = IDNO then Exit();

    var InvFilter: TInvoiceFilter:=TInvoiceFilter.ShowAllItems;

    if cbShowAll.Checked     then InvFilter:=TInvoiceFilter.ShowAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TInvoiceFilter.ReminderOvd;
    if cbNonOverdue.Checked  then InvFilter:=TInvoiceFilter.ReminderNonOvd;

    // -----------------------------------
    // Get item count for sendable emails.
    // -----------------------------------
    for var iCNT:=0 to CustomerList.Items.Count - 1 do
        if CustomerList.Items[iCNT].SubItems[4] <> 'Not found!' then
            ItemCount:=ItemCount + 1;

    // ---------------------------------------
    // Prepare custom message to the customer.
    // ---------------------------------------
    var MessStr:=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);

    // -----------------------------------------------------------------------------------------------------
    // We have to always pre-sort Open Items list via Due Date before sending account statement or reminder.
    // This is necessary to ensure that the HTML generator will generate list for the customer.
    // -----------------------------------------------------------------------------------------------------
    MainForm.sgOpenItems.MSort(MainForm.sgOpenItems.GetCol(TOpenitems.PmtStat), TDataType.TFloat, True);

    OpenItemsRefs.InitWith(MainForm.sgOpenItems);
    CtrlStatusRefs.InitWith(MainForm.sgControlStatus);

    FPayLoad.Layout        :=TDocMode.Defined;
    FPayLoad.Subject       :=Text_Subject.Text;
    FPayLoad.Message       :=MessStr;
    FPayLoad.InvFilter     :=InvFilter;
    FPayLoad.BeginDate     :=ValBeginDate.Caption;
    FPayLoad.EndDate       :=ValEndDate.Caption;
    FPayLoad.MailerList    :=MassMailerForm.CustomerList;
    FPayLoad.OpenItems     :=MainForm.sgOpenItems;
    FPayLoad.OpenItemsRefs :=OpenItemsRefs;
    FPayLoad.ControlStatus :=MainForm.sgControlStatus;
    FPayLoad.CtrlStatusRefs:=CtrlStatusRefs;

    Screen.Cursor:=crHourGlass;
    var Documents: IDocuments:=TDocuments.Create();
    Documents.SendAccDocumentsAsync(MainForm.LoadedAgeDate, FPayLoad, SendAccDocumentAsyncs_Callback);

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TMassMailerForm.SendAccDocumentAsyncs_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
begin

    Screen.Cursor:=crDefault;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    if CallResponse.LastMessage <> 'Processed.' then
    begin

        if ProcessingItemNo > -1 then
        begin
            CustomerList.Items[ProcessingItemNo].SubItems[2]:='Sent';
            ItemCount:=MassMailerForm.ItemCount - 1;
        end

    end
    else if CallResponse.LastMessage = 'Processed.' then
    begin
        THelpers.MsgCall(TAppMessage.Info, 'All listed items have been processed.');
    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TMassMailerForm.FormCreate(Sender: TObject);
begin

    var lsColumns: TListColumn;

    // Row number from Age View
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Lp';
    lsColumns.Width  :=40;

    // From Age View (0)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer name';
    lsColumns.Width  :=150;

    // From Age View (1)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer number';
    lsColumns.Width  :=100;

    // Own indicator 4 (2)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Is sent?';
    lsColumns.Width  :=80;

    // From Company Data 2 (3)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send from';
    lsColumns.Width  :=100;

    // From Address Book 3 (4)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send to';
    lsColumns.Width  :=100;

    // From Company Data (5)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU name';
    lsColumns.Width  :=80;

    // From Company Data 6
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU address';
    lsColumns.Width  :=150;

    // From Compant Data (7)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU telephone';
    lsColumns.Width  :=100;

    // From Age View (8)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU number';
    lsColumns.Width  :=80;

    // From Age View (9)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU agent';
    lsColumns.Width  :=80;

    // From Age View (10)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='CUID';
    lsColumns.Width  :=80;

    // Assembled from Age View data (11)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='SCUID';
    lsColumns.Width  :=80;

    // From Company Data (12)
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='BanksHtml';
    lsColumns.Width  :=0;

    PanelEmailContainer.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSubject.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelMessage.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);

    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';

end;


procedure TMassMailerForm.FormShow(Sender: TObject);
begin
    {Do nothing}
end;


procedure TMassMailerForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crHourGlass;
        MainForm.UpdateStatusBar(TStatusBar.Processing);

        THelpers.ExecWithDelay(500, procedure
        begin

            // ----------------------------------------------------------------------------------------
            // Before the form is shown to the user, get all email addresses from database.
            // This may take some time, so we display busy cursor. We also switch off open items timer,
            // so it wil not interfere when user sends the data.
            // ----------------------------------------------------------------------------------------
            LoadFromGrid();
            SetEmailAddresses(CustomerList);
            UpdateCompanyData(CustomerList);

            MainForm.TimerCustOpenItems.Enabled:=False;
            ThreadFileLog.Log('[TMassMailerForm.FormActivate]: Mass mailer opened, open items loader is now on hold.');

            Text_Subject.SetFocus();
            FIsDataLoaded:=True;
            Screen.Cursor:=crDefault;
            MainForm.UpdateStatusBar(TStatusBar.Ready);

        end);

    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TMassMailerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    // ------------------------------------------------
    // Turn on disabled timer for open items scanner,
    // so open items will not change during processing.
    // ------------------------------------------------
    FIsDataLoaded:=False;
    CustomerList.Clear();
    MainForm.TimerCustOpenItems.Enabled:=True;
    ThreadFileLog.Log('[TMassMailerForm.FormClose]: Mass mailer closed, open items loader is now enabled back again.');
end;


procedure TMassMailerForm.FormDestroy(Sender: TObject);
begin
    {Do nonthing}
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TMassMailerForm.btnBeginDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    THelpers.WndCall(CalendarForm, Modal);
    ValBeginDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TMassMailerForm.btnEndDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    THelpers.WndCall(CalendarForm, Modal);
    ValEndDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TMassMailerForm.cbShowAllClick(Sender: TObject);
begin

    if cbShowAll.Checked then
    begin
        cbOverdueOnly.Checked:=False;
        cbNonOverdue.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if
        not(cbShowAll.Checked)
    and
        not(cbOverdueOnly.Checked)
    and
        not(cbNonOverdue.Checked)
    then
        cbShowAll.Checked:=True;

end;


procedure TMassMailerForm.cbOverdueOnlyClick(Sender: TObject);
begin

    if cbOverdueOnly.Checked then
    begin
        cbShowAll.Checked:=False;
        cbNonOverdue.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if
        not(cbShowAll.Checked)
    and
        not(cbOverdueOnly.Checked)
    and
        not(cbNonOverdue.Checked)
    then
        cbOverdueOnly.Checked:=True;

end;


procedure TMassMailerForm.cbNonOverdueClick(Sender: TObject);
begin

    if cbNonOverdue.Checked then
    begin
        cbShowAll.Checked:=False;
        cbOverdueOnly.Checked:=False;
        ImgCover.Visible:=False;
    end;

    if
        not(cbShowAll.Checked)
    and
        not(cbOverdueOnly.Checked)
    and
        not(cbNonOverdue.Checked)
    then
        cbNonOverdue.Checked:=True;

end;


procedure TMassMailerForm.btnDelBeginClick(Sender: TObject);
begin
    ValBeginDate.Caption:='';
end;


procedure TMassMailerForm.btnDelEndClick(Sender: TObject);
begin
    ValEndDate.Caption:='';
end;


procedure TMassMailerForm.btnSendEmailClick(Sender: TObject);
begin
    ExecuteMailer();
end;


procedure TMassMailerForm.btnCancelClick(Sender: TObject);
begin
    Close();
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TMassMailerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


procedure TMassMailerForm.Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus();
end;


procedure TMassMailerForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbShowAll.SetFocus();
end;


procedure TMassMailerForm.cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbOverdueOnly.SetFocus();
end;


procedure TMassMailerForm.cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbNonOverdue.SetFocus();
end;


procedure TMassMailerForm.cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then CustomerList.SetFocus();
end;


procedure TMassMailerForm.CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Subject.SetFocus();
end;


{$ENDREGION}


end.

