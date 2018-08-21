
{$I .\Include\Header.inc}

unit MassMailer;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, InterposerClasses;

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
        Text1: TLabel;
        Text2: TLabel;
        cbAddOverdue: TCheckBox;
        PanelMessage: TPanel;
        Text_Message: TMemo;
        PanelSalutation: TPanel;
        Text_Salut: TMemo;
        PanelList: TPanel;
        CustomerList: TListView;
        PanelSubject: TPanel;
        Text_Subject: TMemo;
        Text3: TLabel;
        EmailList: TComboBox;
        PanelEmailFrom: TPanel;
        Text4: TLabel;
        PanelEmailContainer: TPanel;
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    public
        var ThreadCount: integer;
    private
        function  GetEmailAddress(Scuid: string): string;
        procedure SetEmailAddresses(List: TListView);
        procedure GetEmailList(List: TComboBox);
    end;

var
    ViewMailerForm: TViewMailerForm;


implementation


uses
    Main, Settings, SQL, Model, Worker, Actions, Await;

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
        Database.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + Scuid;
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
    EmailAddress:   string;
    iCNT:           integer;
begin

    if List.Items.Count > 0 then
    begin
        for iCNT:=0 to List.Items.Count - 1 do
        begin
            EmailAddress:=GetEmailAddress(List.Items[iCNT].SubItems[0]);

            if not(string.IsNullOrEmpty(EmailAddress)) then
                List.Items[iCNT].SubItems[2]:=EmailAddress

        end;
    end;

end;

/// <summary>
///     Get list of emails addresses that can be use to send message to the selected recipients.
/// </summary>

procedure TViewMailerForm.GetEmailList(List: TComboBox);
var
    Database: TDataTables;
    CoCode1:  string;
    CoCode2:  string;
    CoCode3:  string;
    CoCode4:  string;
begin

    // Get Co Codes that are opened (age view snapshot)
    if MainForm.tcCOCODE1.Caption <> 'n/a' then CoCode1:=MainForm.tcCOCODE1.Caption;
    if MainForm.tcCOCODE2.Caption <> 'n/a' then CoCode2:=MainForm.tcCOCODE2.Caption;
    if MainForm.tcCOCODE3.Caption <> 'n/a' then CoCode3:=MainForm.tcCOCODE3.Caption;
    if MainForm.tcCOCODE4.Caption <> 'n/a' then CoCode4:=MainForm.tcCOCODE4.Caption;

    Database:=TDataTables.Create(MainForm.DbConnect);
    try
        Database.Columns.Add(DISTINCT + TCompany.SEND_NOTE_FROM);
        Database.CustFilter:=WHERE +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE1) +
                             _OR +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE2) +
                             _OR +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE3) +
                             _OR +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE4);
        Database.OpenTable(TblCompany);
        if not(Database.DataSet.RecordCount = 0) then
            Database.SqlToSimpleList(List, Database.DataSet)
                else
                    MainForm.MsgCall(mcWarn, 'Cannot find assigned email address to your organisation. Please contact Unity IT administrator.');
    finally
        Database.Free;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


/// <summary>
///     Prepare window caption, window panels and list view object, so it can be later used as we do not make
///     change in columns at runtime.
/// </summary>

procedure TViewMailerForm.FormCreate(Sender: TObject);
var
    Settings:   ISettings;
    lsColumns:  TListColumn;
begin

    Settings:=TSettings.Create;
    ViewMailerForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_MASSMAILER', APPCAPTION);

    // INITIALIZE LIST VIEW
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Lp';
    lsColumns.Width  :=40;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Scuid';
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer Name';
    lsColumns.Width  :=150;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='To';
    lsColumns.Width  :=100;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Is sent?';
    lsColumns.Width  :=100;

    // Draw panel borders
    PanelEmailContainer.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelEmailFrom.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelSubject.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelSalutation.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelMessage.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);

end;

/// <summary>
///     Before the form is shown to the user, get all email addresses from database. This may take some time, so we display busy cursor.
///     We also switch off open items timer, so it wil not interfere when user sends the data.
/// </summary>

procedure TViewMailerForm.FormShow(Sender: TObject);
begin
    // Display busy cursor and change status
    Screen.Cursor:=crSQLWait;
    MainForm.ExecMessage(False, mcStatusBar, stProcessing);

    // Get all necessary data
    GetEmailList(EmailList);
    EmailList.ItemIndex:=0;
    SetEmailAddresses(CustomerList);

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


/// <summary>
///     Execute worker thread to process the listed emails. Show busy window in main thread.
/// </summary>

procedure TViewMailerForm.btnSendEmailClick(Sender: TObject);
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
            string.IsNullOrEmpty(Text_Salut.Text)
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
        if CustomerList.Items[iCNT].SubItems[2] <> 'Not Found' then
            Inc(ThreadCount);

    // Prepare custom message to the customer
    MessStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

    // Process listed customers in worker thread
    TTSendAccountStatements.Create(
        Text_Subject.Text,
        Text_Salut.Text,
        MessStr,
        cbAddOverdue.Checked,
        MainForm.sgOpenItems,
        MainForm.sgAgeView,
        ViewMailerForm.CustomerList
    );

    // Display await window
    MainForm.WndCall(AwaitForm, stModal);

end;


procedure TViewMailerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


end.
