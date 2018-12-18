
{$I .\Include\Header.inc}

unit MassMailer;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ComCtrls,
    StdCtrls,
    ExtCtrls,
    Buttons,
    InterposerClasses;


type

    /// <summary>
    ///     View form class with helpers for mass mailer (separate window). Alow user to send emails to a selected customers with open items.
    /// </summary>

    TViewMailerForm = class(TForm)
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
        cbAddOverdue: TCheckBox;
        PanelMessage: TPanel;
        Text_Message: TMemo;
        PanelList: TPanel;
        CustomerList: TListView;
        PanelSubject: TPanel;
        Text3: TLabel;
        PanelEmailContainer: TPanel;
        Text_Subject: TEdit;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbAddOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure EmailListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    private
        var FThreadCount: integer;
        function  GetEmailAddress(Scuid: string): string;
        procedure SetEmailAddresses(List: TListView);
        procedure UpdateCompanyData(Source: TListView);
        procedure ExecuteMailer;
    public
        property ThreadCount: integer read FThreadCount write FThreadCount;
    end;


var
    ViewMailerForm: TViewMailerForm;


implementation


uses
    Main,
    Settings,
    SQL,
    Model,
    Worker,
    Actions,
    Await;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Get customer email address for given SCUID value.
/// </summary>

function TViewMailerForm.GetEmailAddress(Scuid: string): string;
var
    Database: TDataTables;
begin

    Result:='';

    Database:=TDataTables.Create(MainForm.DbConnect);

    try
        Database.Columns.Add(TAddressBook.ESTATEMENTS);
        Database.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(Scuid);
        Database.OpenTable(TblAddressbook);

        if Database.DataSet.RecordCount > 0 then
            Result:=MainForm.OleGetStr(Database.DataSet.Fields[TAddressBook.ESTATEMENTS].Value)

    finally
        Database.Free;
    end;

end;


/// <summary>
///     Set email address used to send email.
/// </summary>

procedure TViewMailerForm.SetEmailAddresses(List: TListView);
var
    EmailAddress: string;
    iCNT: integer;
begin

    if List.Items.Count > 0 then
    begin
        for iCNT:=0 to List.Items.Count - 1 do
        begin
            EmailAddress:=GetEmailAddress(List.Items[iCNT].SubItems[11]);

            if not(string.IsNullOrEmpty(EmailAddress)) then
                List.Items[iCNT].SubItems[3]:=EmailAddress

        end;
    end;

end;


/// <summary>
///
/// </summary>

procedure TViewMailerForm.UpdateCompanyData(Source: TListView);
var
    Tables:  TDataTables;
    iCNT:    integer;
    CoCode:  string;
    Branch:  string;
begin

    Tables:=TDataTables.Create(MainForm.DbConnect);
    try

        if Source.Items.Count > 0 then
        begin

            Tables.Columns.Add(TCompany.CONAME);
            Tables.Columns.Add(TCompany.COADDRESS);
            Tables.Columns.Add(TCompany.Telephone);
            Tables.Columns.Add(TCompany.SEND_NOTE_FROM);
            Tables.Columns.Add(TCompany.BANKDETAILS);

            for iCNT:=0 to Source.Items.Count - 1 do
            begin
                CoCode:=Source.Items[iCNT].SubItems[8];
                Branch:=Source.Items[iCNT].SubItems[9];

                Tables.ClearSQL;
                Tables.CustFilter:=WHERE + TCompany.CO_CODE + EQUAL + QuotedStr(CoCode) + _AND + TCompany.BRANCH + EQUAL + QuotedStr(Branch);
                Tables.OpenTable(TblCompany);

                // Always add to the lists
                if Tables.DataSet.RecordCount = 1 then
                begin
                    Source.Items[iCNT].SubItems[5] :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.CONAME].Value);
                    Source.Items[iCNT].SubItems[6] :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.COADDRESS].Value);
                    Source.Items[iCNT].SubItems[7] :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.Telephone].Value);
                    Source.Items[iCNT].SubItems[2] :=MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.SEND_NOTE_FROM].Value);
                    Source.Items[iCNT].SubItems[12]:=MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.BANKDETAILS].Value);
                end
                else
                begin
                    Source.Items[iCNT].SubItems[5] :=unNotFound;
                    Source.Items[iCNT].SubItems[6] :=unNotFound;
                    Source.Items[iCNT].SubItems[7] :=unNotFound;
                    Source.Items[iCNT].SubItems[2] :=unNotFound;
                    Source.Items[iCNT].SubItems[12]:=unNotFound;
                end;

            end;
        end;

    finally
        Tables.Free;
    end;

end;


/// <summary>
///     Execute worker thread to process the listed emails. Show busy window in main thread.
/// </summary>

procedure TViewMailerForm.ExecuteMailer;
var
    iCNT:    integer;
    MessStr: string;
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
        MainForm.MsgCall(mcWarn, 'Cannot send incomplete form. Please re-check it and try again.');
        Exit;
    end;

    // Ask user, they may press the button by mistake
    if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure you want to send it, right now?') = IDNO
        then
            Exit;

    // Get item count for sendable emails
    for iCNT:=0 to CustomerList.Items.Count - 1 do
        if CustomerList.Items[iCNT].SubItems[3] <> 'Not found!' then
            ThreadCount:=ThreadCount + 1;

    // Prepare custom message to the customer
    MessStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

    /// <remarks>
    ///     We have to always pre-sort Open Items list via Due Date before sending account statement or reminder.
    ///     This is necessary to ensure that the HTML generator will make sorted list for the customer.
    /// </remarks>

    // Sort Open Items via Due Date
    MainForm.sgOpenItems.MSort(MainForm.sgOpenItems.ReturnColumn(TOpenitems.PmtStat, 1 , 1), 2, True);

    // Process listed customers in worker thread
    TTSendAccountStatements.Create(
        Text_Subject.Text,
        MessStr,
        cbAddOverdue.Checked,
        MainForm.sgOpenItems,
        ViewMailerForm.CustomerList
    );

    // Display await window
    MainForm.WndCall(AwaitForm, stModal);
end;


// -------------------------------------------------------------------------------------------------------------------------------------- START UP & RELEASE //


/// <summary>
///     Prepare window caption, window panels and list view object, so it can be later used as we do not make
///     change in columns at runtime.
/// </summary>

procedure TViewMailerForm.FormCreate(Sender: TObject);
var
    Settings:   ISettings;
    lsColumns:  TListColumn;
begin

    // Set window caption
    Settings:=TSettings.Create;
    ViewMailerForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_MASSMAILER', APPCAPTION);

    // List View
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
    lsColumns.Caption:='Send from';       // From Company Data 2
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send to';         // From Address Book 3
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Is sent?';        // Own indicator 4
    lsColumns.Width  :=80;
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
    PanelEmailContainer.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelSubject.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelMessage.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);

end;


/// <summary>
///
/// </summary>

procedure TViewMailerForm.FormDestroy(Sender: TObject);
begin
    //
end;


/// <summary>
///     Before the form is shown to the user, get all email addresses from database. This may take some time, so we display busy cursor.
///     We also switch off open items timer, so it wil not interfere when user sends the data.
/// </summary>

procedure TViewMailerForm.FormShow(Sender: TObject);
begin
    // Set focus on subject field
    Text_Subject.SetFocus;

    // Display busy cursor and change status
    Screen.Cursor:=crSQLWait;
    MainForm.ExecMessage(False, mcStatusBar, stProcessing);

    // Get data
    SetEmailAddresses(CustomerList);
    UpdateCompanyData(CustomerList);

    // Default
    Screen.Cursor:=crDefault;
    MainForm.ExecMessage(False, mcStatusBar, stReady);

    // Turn off open items timer
    MainForm.OILoader.Enabled:=False;

    // Log it to event log. As long as Mass Mailer is opened, we do not process
    // any open items/age view snapshots
    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Mass mailer opened, open items loader is now on hold.');
end;


/// <summary>
///     Turn on disabled timer for open items scanner.
/// </summary>

procedure TViewMailerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainForm.OILoader.Enabled:=True;
    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Mass mailer closed, open items loader is now enabled back again.');
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


procedure TViewMailerForm.btnSendEmailClick(Sender: TObject);
begin
    ExecuteMailer;
end;


procedure TViewMailerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TViewMailerForm.Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus;
end;


procedure TViewMailerForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbAddOverdue.SetFocus;
end;


procedure TViewMailerForm.cbAddOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//    if Key = VK_TAB then EmailList.SetFocus;
end;


procedure TViewMailerForm.EmailListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Subject.SetFocus;
end;


end.

