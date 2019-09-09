unit View.MassMailer;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined the same as callback
// signature. All views must use Lazy Initialization pattern.
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
    Unity.Records;


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
    protected
        var FFields: TSendAccountStatementFields;
    private
        var FThreadCount: integer;
        function  GetEmailAddress(Scuid: string): string;
        procedure SetEmailAddresses(List: TListView);
        procedure UpdateCompanyData(Source: TListView);
        procedure ExecuteMailer;
    public
        property ThreadCount: integer read FThreadCount write FThreadCount;

        // ------------------
        // Callbacks methods.
        // ------------------

        //...

    end;


    function MassMailerForm: TMassMailerForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Calendar,
    View.Actions,
    View.AwaitScreen,
    Handler.Sql,
    DbModel,
    Unity.Enums,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.Sql,
    Unity.StatusBar,
    Unity.Unknown,
    Unity.Chars,
    Async.Statements;


var vMassMailerForm: TMassMailerForm;


function MassMailerForm: TMassMailerForm;
begin
    if not(Assigned(vMassMailerForm)) then Application.CreateForm(TMassMailerForm, vMassMailerForm);
    Result:=vMassMailerForm;
end;


// -------------------------------------------------------------------------------------------------------------------------------------- START UP & RELEASE //


procedure TMassMailerForm.FormCreate(Sender: TObject);
begin

    var lsColumns: TListColumn;

    // Setup List View component
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Lp';              // Row number from Age View
    lsColumns.Width  :=40;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer name';   // From Age View 0
    lsColumns.Width  :=150;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer number'; // From Age View 1
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Is sent?';        // Own indicator 4 (2)
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send from';       // From Company Data 2 (3)
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send to';         // From Address Book 3 (4)
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU name';        // From Company Data 5
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU address';     // From Company Data 6
    lsColumns.Width  :=150;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU telephone';   // From Compant Data 7
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU number';      // From Age View 8
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU agent';       // From Age View 9
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='CUID';            // From Age View 10
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='SCUID';           // Assembled from Age View data 11
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='BanksHtml';       // From Company Data 12
    lsColumns.Width  :=0;

    // Draw panel borders
    PanelEmailContainer.PanelBorders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSubject.PanelBorders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelMessage.PanelBorders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);

    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';

end;


procedure TMassMailerForm.FormDestroy(Sender: TObject);
begin
    //
end;


/// <summary>
/// Before the form is shown to the user, get all email addresses from database. This may take some time, so we display busy cursor.
/// We also switch off open items timer, so it wil not interfere when user sends the data.
/// </summary>

procedure TMassMailerForm.FormShow(Sender: TObject);
begin

    // Set focus on subject field
    Text_Subject.SetFocus;

    // Display busy cursor and change status
    Screen.Cursor:=crSQLWait;
    THelpers.ExecMessage(False, TMessaging.TWParams.StatusBar, TStatusBar.Processing, MainForm);

    // Get data
    SetEmailAddresses(CustomerList);
    UpdateCompanyData(CustomerList);

    // Default
    Screen.Cursor:=crDefault;
    THelpers.ExecMessage(False, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

    // Turn off open items timer
    MainForm.OILoader.Enabled:=False;

    // Log it to event log. As long as Mass Mailer is opened, we do not process
    // any open items/age view snapshots
    MainForm.FAppEvents.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Mass mailer opened, open items loader is now on hold.');

end;


/// <summary>
/// Turn on disabled timer for open items scanner.
/// </summary>

procedure TMassMailerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainForm.OILoader.Enabled:=True;
    MainForm.FAppEvents.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Mass mailer closed, open items loader is now enabled back again.');
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


function TMassMailerForm.GetEmailAddress(Scuid: string): string;
begin

    Result:='';

    var Database: TDataTables:=TDataTables.Create(MainForm.FDbConnect);
    try
        Database.Columns.Add(TAddressBook.Estatements);
        Database.CustFilter:=TSql.WHERE + TAddressBook.Scuid + TSql.EQUAL + QuotedStr(Scuid);
        Database.OpenTable(TAddressBook.AddressBook);

        if Database.DataSet.RecordCount > 0 then
            Result:=THelpers.OleGetStr(Database.DataSet.Fields[TAddressBook.Estatements].Value)

    finally
        Database.Free;
    end;

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
begin

    var Tables: TDataTables:=TDataTables.Create(MainForm.FDbConnect);
    try

        if Source.Items.Count > 0 then
        begin

            Tables.Columns.Add(TCompanyData.CoName);
            Tables.Columns.Add(TCompanyData.CoAddress);
            Tables.Columns.Add(TCompanyData.TelephoneNumbers);
            Tables.Columns.Add(TCompanyData.SendNoteFrom);
            Tables.Columns.Add(TCompanyData.BankAccounts);

            for var iCNT: integer:=0 to Source.Items.Count - 1 do
            begin

                var CoCode: string:=Source.Items[iCNT].SubItems[8];
                var Branch: string:=Source.Items[iCNT].SubItems[9];

                Tables.ClearSQL;
                Tables.CustFilter:=TSql.WHERE + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(CoCode) + TSql._AND + TCompanyData.Branch + TSql.EQUAL + QuotedStr(Branch);
                Tables.OpenTable(TCompanyData.CompanyData);

                // Always add to the lists
                if Tables.DataSet.RecordCount = 1 then
                begin
                    Source.Items[iCNT].SubItems[5] :=THelpers.OleGetStr(Tables.DataSet.Fields[TCompanyData.CoName].Value);
                    Source.Items[iCNT].SubItems[6] :=THelpers.OleGetStr(Tables.DataSet.Fields[TCompanyData.CoAddress].Value);
                    Source.Items[iCNT].SubItems[7] :=THelpers.OleGetStr(Tables.DataSet.Fields[TCompanyData.TelephoneNumbers].Value);
                    Source.Items[iCNT].SubItems[3] :=THelpers.OleGetStr(Tables.DataSet.Fields[TCompanyData.SendNoteFrom].Value);
                    Source.Items[iCNT].SubItems[12]:=THelpers.OleGetStr(Tables.DataSet.Fields[TCompanyData.BankAccounts].Value);
                end
                else
                begin
                    Source.Items[iCNT].SubItems[5] :=TUnknown.NotFound;
                    Source.Items[iCNT].SubItems[6] :=TUnknown.NotFound;
                    Source.Items[iCNT].SubItems[7] :=TUnknown.NotFound;
                    Source.Items[iCNT].SubItems[3] :=TUnknown.NotFound;
                    Source.Items[iCNT].SubItems[12]:=TUnknown.NotFound;
                end;

            end;
        end;

    finally
        Tables.Free;
    end;

end;


/// <summary>
/// Execute worker thread to process the listed emails. Show busy window in main thread.
/// </summary>

procedure TMassMailerForm.ExecuteMailer;
begin

    // Check fields
    if
        (
            string.IsNullOrEmpty(Text_Subject.Text)
        )
    or
        (
            string.IsNullOrEmpty(Text_Message.Text)
        )
    then
    begin
        THelpers.MsgCall(Warn, 'Cannot send incomplete form. Please re-check it and try again.');
        Exit;
    end;

    // Ask user, they may press the button by mistake
    if THelpers.MsgCall(Question2, 'Are you absolutely sure you want to send it, right now?') = IDNO
        then
            Exit;

    // Filtering options
    var InvFilter: TInvoiceFilter:=TInvoiceFilter.ShowAllItems;
    if cbShowAll.Checked     then InvFilter:=TInvoiceFilter.ShowAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TInvoiceFilter.ReminderOvd;
    if cbNonOverdue.Checked  then InvFilter:=TInvoiceFilter.ReminderNonOvd;

    // Get item count for sendable emails
    for var iCNT: integer:=0 to CustomerList.Items.Count - 1 do
        if CustomerList.Items[iCNT].SubItems[4] <> 'Not found!' then
            ThreadCount:=ThreadCount + 1;

    // Prepare custom message to the customer
    var MessStr: string:=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);

    /// <remarks>
    /// We have to always pre-sort Open Items list via Due Date before sending account statement or reminder.
    /// This is necessary to ensure that the HTML generator will make sorted list for the customer.
    /// </remarks>

    // Sort Open Items via Due Date
    MainForm.sgOpenItems.MSort(MainForm.sgOpenItems.ReturnColumn(TOpenitems.PmtStat, 1 , 1), 2, True);

    /// <remarks>
    /// Update column references, as they depend on view from SQL which may be changed at runtime.
    /// </remarks>

    MainForm.UpdateFOpenItemsRefs(MainForm.sgOpenItems);
    MainForm.UpdateFControlStatusRefs(MainForm.sgControlStatus);

    FFields.Layout    :=TDocMode.Defined;
    FFields.Subject   :=Text_Subject.Text;
    FFields.Mess      :=MessStr;
    FFields.InvFilter :=InvFilter;
    FFields.BeginDate :=ValBeginDate.Caption;
    FFields.EndDate   :=ValEndDate.Caption;
    FFields.OpenItems :=MainForm.sgOpenItems;
    FFields.MailerList:=MassMailerForm.CustomerList;

    var Statements: IStatements:=TStatements.Create();
    Statements.SendAccountStatements(FFields);

    // Display await window
    THelpers.WndCall(AwaitForm, TWindowState.Modal);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


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
    ExecuteMailer;
end;


procedure TMassMailerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TMassMailerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


procedure TMassMailerForm.Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus;
end;


procedure TMassMailerForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbShowAll.SetFocus;
end;


procedure TMassMailerForm.cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbOverdueOnly.SetFocus;
end;


procedure TMassMailerForm.cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbNonOverdue.SetFocus;
end;


procedure TMassMailerForm.cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then CustomerList.SetFocus;
end;


procedure TMassMailerForm.CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Subject.SetFocus;
end;


end.

