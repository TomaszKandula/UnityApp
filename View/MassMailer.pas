
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

procedure TViewMailerForm.FormShow(Sender: TObject);
begin
    GetEmailList(EmailList);
    EmailList.ItemIndex:=0;
    SetEmailAddresses(CustomerList);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


/// <summary>
///     We loop through the list and send emails if email address is found. We execute TTSendAccountStatement with "Series" parameter set to true,
///     this ensures that each time worker thread execute send method, it will return windows message with processed row (if successfull).
/// </summary>

procedure TViewMailerForm.btnSendEmailClick(Sender: TObject);
var
    iCNT:    integer;
    TempStr: string;
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

    if CustomerList.Items.Count > 0 then
    begin

        // Update column references
        MainForm.UpdateOpenItemsRefs(MainForm.sgOpenItems);

        // Send concurrently
        for iCNT:=0 to CustomerList.Items.Count - 1 do
        begin

            if CustomerList.Items[iCNT].SubItems[2] <> 'Not Found' then
            begin

                TempStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

                TTSendAccountStatement.Create(
                    True,
                    maCustom,
                    Text_Subject.Text,
                    Text_Salut.Text,
                    TempStr,
                    cbAddOverdue.Checked,
                    MainForm.sgOpenItems,
                    CustomerList.Items[iCNT].SubItems[0],
                    MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), StrToInt(CustomerList.Items[iCNT].Caption)],
                    MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), StrToInt(CustomerList.Items[iCNT].Caption)],
                    MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), StrToInt(CustomerList.Items[iCNT].Caption)],
                    MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), StrToInt(CustomerList.Items[iCNT].Caption)],
                    MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAGENT,           1, 1), StrToInt(CustomerList.Items[iCNT].Caption)],
                    iCNT
                );

                // Log started thread
                Inc(ThreadCount);

            end;

        end;

        /// <remarks>
        ///     Display message window, it will be dismissed once all worker threads ends.
        /// </remarks>

        MainForm.WndCall(AwaitForm, stModal);

    end;

end;

procedure TViewMailerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


end.
