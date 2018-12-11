
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
    ///     Helper class encapsulating company data fields. Once SetCompanyData method is called, it scans
    ///     Source and get relevant fields into separate StringLists that can be later accessed and used elsewhere.
    /// </summary>

    TCompanyData = class(TObject)
    {$TYPEINFO ON}
    private
        var FLbuName:      TStringList;
        var FLbuAddress:   TStringList;
        var FLbuPhone:     TStringList;
        var FLbuSendFrom:  TStringList;
        var FLbuBanksHtml: TStringList;
        procedure ClearAll;
    public
        property LbuName:      TStringList read FLbuName      write FLbuName;
        property LbuAddress:   TStringList read FLbuAddress   write FLbuAddress;
        property LbuPhone:     TStringList read FLbuPhone     write FLbuPhone;
        property LbuSendFrom:  TStringList read FLbuSendFrom  write FLbuSendFrom;
        property LbuBanksHtml: TStringList read FLbuBanksHtml write FLbuBanksHtml;
        constructor Create;
        destructor  Destroy; override;
        procedure   SetCompanyData(Source: TListView);
    end;

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
        procedure ExecuteMailer;
    public
        var CompanyData: TCompanyData;
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


// ---------------------------------------------------------------------------------------------------------------------------------- METHODS | HELPER CLASS //


/// <summary>
///
/// </summary>

constructor TCompanyData.Create;
begin
    if not(Assigned(FLbuName))      then FLbuName.Create;
    if not(Assigned(FLbuAddress))   then FLbuAddress.Create;
    if not(Assigned(FLbuPhone))     then FLbuPhone.Create;
    if not(Assigned(FLbuSendFrom))  then FLbuSendFrom.Create;
    if not(Assigned(FLbuBanksHtml)) then FLbuBanksHtml.Create;
end;


/// <summary>
///
/// </summary>

destructor TCompanyData.Destroy;
begin
    if Assigned(FLbuName)      then FLbuName.Free;
    if Assigned(FLbuAddress)   then FLbuAddress.Free;
    if Assigned(FLbuPhone)     then FLbuPhone.Free;
    if Assigned(FLbuSendFrom)  then FLbuSendFrom.Free;
    if Assigned(FLbuBanksHtml) then FLbuBanksHtml.Free;
    inherited;
end;


/// <summary>
///
/// </summary>

procedure TCompanyData.ClearAll;
begin
    FLbuName.Clear;
    FLbuAddress.Clear;
    FLbuPhone.Clear;
    FLbuSendFrom.Clear;
    FLbuBanksHtml.Clear;
end;


/// <summary>
///
/// </summary>

procedure TCompanyData.SetCompanyData(Source: TListView);
var
    Tables:  TDataTables;
    iCNT:    integer;
    CoCode:  string;
    Branch:  string;
begin
    ClearAll;

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
                CoCode:=Source.Items[iCNT].SubItems[7];
                Branch:=Source.Items[iCNT].SubItems[8];

                Tables.ClearSQL;
                Tables.CustFilter:=WHERE + TCompany.CO_CODE + EQUAL + QuotedStr(CoCode) + _AND + TCompany.BRANCH + EQUAL + QuotedStr(Branch);
                Tables.OpenTable(TblCompany);

                // Always add to the lists
                if Tables.DataSet.RecordCount = 1 then
                begin
                    LbuName.Add(MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.CONAME].Value));
                    LbuAddress.Add(MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.COADDRESS].Value));
                    LbuPhone.Add(MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.Telephone].Value));
                    LbuSendFrom.Add(MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.SEND_NOTE_FROM].Value));
                    LbuBanksHtml.Add(MainForm.OleGetStr(Tables.DataSet.Fields[TCompany.BANKDETAILS].Value));
                end
                else
                begin
                    LbuName.Add(unNotFound);
                    LbuAddress.Add(unNotFound);
                    LbuPhone.Add(unNotFound);
                    LbuSendFrom.Add(unNotFound);
                    LbuBanksHtml.Add(unNotFound);
                end;

                // Update source fields
                Source.Items[iCNT].SubItems[0]:=LbuName.Strings[iCNT];
                Source.Items[iCNT].SubItems[5]:=LbuAddress.Strings[iCNT];
                Source.Items[iCNT].SubItems[6]:=LbuPhone.Strings[iCNT];
                Source.Items[iCNT].SubItems[2]:=LbuSendFrom.Strings[iCNT];

            end;
        end;

    finally
        Tables.Free;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------ HELPERS | THIS CLASS //


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
            EmailAddress:=GetEmailAddress(List.Items[iCNT].SubItems[10]);

            if not(string.IsNullOrEmpty(EmailAddress)) then
                List.Items[iCNT].SubItems[3]:=EmailAddress

        end;
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

    // Check if EmailList contains "noreply" emailbox (default). if so, we should not allow to send from such email address.
//    if AnsiPos(EmailList.Items[EmailList.ItemIndex], 'noreply') > 0 then
//    begin
//        MainForm.MsgCall(mcWarn, 'Cannot send e-mail from "noreply" email box. Please select other email address and try again.');
//        Exit;
//    end;

    // Ask user, they may press the button by mistake
    if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure you want to send it, right now?') = IDNO
        then
            Exit;

    // Get item count for sendable emails
    for iCNT:=0 to CustomerList.Items.Count - 1 do
        if CustomerList.Items[iCNT].SubItems[2] <> 'Not Found' then
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
//    TTSendAccountStatements.Create(
//        Text_Subject.Text,
//        strNull,
//        MessStr,
//        cbAddOverdue.Checked,
//        MainForm.sgOpenItems,
//        MainForm.sgAgeView,
//        ViewMailerForm.CustomerList
//    );

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

    // Initialize string list
    if not(Assigned(CompanyData)) then CompanyData:=TCompanyData.Create;

    // Set window caption
    Settings:=TSettings.Create;
    ViewMailerForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_MASSMAILER', APPCAPTION);

    // List View
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Lp';              // Row number from Age View
    lsColumns.Width  :=40;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer Name';   // From Age View
    lsColumns.Width  :=150;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer Number'; // From Age View
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send from:';      // From Company Data
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send To:';        // From Address Book
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Is sent?';        // Own indicator
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU address';     // From Company Data
    lsColumns.Width  :=150;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LBU telephone';   // From Compant Data
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Co Code';         // From Age View
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Branch';          // From Age View
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='CUID';            // From Age View
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='SCUID';           // Assembled from Age View data
    lsColumns.Width  :=80;

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
    if Assigned(CompanyData) then CompanyData.Free;
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
    CompanyData.SetCompanyData(CustomerList);

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

