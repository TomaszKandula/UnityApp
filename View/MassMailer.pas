
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
    private
        function  GetEmailAddress(Scuid: string): string;
        procedure SetEmailAddresses(List: TListView);
        procedure GetEmailList(List: TComboBox);
        procedure SendAccountStatement(Series: boolean; Layout: integer; Subject: string; Salut: string; Mess: string; IsOverdue: boolean; SCUID: string; Row: integer); // to be removed!!!
    end;

var
    ViewMailerForm: TViewMailerForm;


implementation


uses
    Main, Settings, SQL, Model, Worker;

{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Get customer email address for given SCUID value.
/// </summary>

function TViewMailerForm.GetEmailAddress(Scuid: string): string;
var
    Database: TDataTables;
begin

    Database:=TDataTables.Create(MainForm.DbConnect);

    try
        Database.Columns.Add(TAddressBook.EMAILS);
        Database.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + Scuid;
        Database.OpenTable(TblAddressbook);
        Result:=MainForm.OleGetStr(Database.DataSet.Fields[TAddressBook.EMAILS].Value);
    finally
        Database.Free;
    end;

end;

/// <summary>
///     Set email address used to send email.
/// </summary>

procedure TViewMailerForm.SetEmailAddresses(List: TListView);
var
    iCNT: integer;
begin

    if List.Items.Count > 0 then
    begin
        for iCNT:=0 to List.Items.Count - 1 do
        begin
            List.Items[iCNT].SubItems[2]:=GetEmailAddress(List.Items[iCNT].SubItems[0]);
        end;
    end;

end;

/// <summary>
///     Get email list.
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
        Database.SqlToSimpleList(List, Database.DataSet);
    finally
        Database.Free;
    end;

end;

// to be removed - temporary!
procedure TViewMailerForm.SendAccountStatement(Series: boolean; Layout: integer; Subject: string; Salut: string; Mess: string; IsOverdue: boolean; SCUID: string; Row: integer);
begin
    TTSendAccountStatement.Create(
        Series,
        Layout,
        Subject,
        Salut,
        Mess,
        IsOverdue,
        MainForm.sgOpenItems,
        SCUID,
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAGENT,           1, 1), Row]
    );
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TViewMailerForm.FormCreate(Sender: TObject);
var
    Settings:   ISettings;
    lsColumns:  TListColumn;
begin

    Settings:=TSettings.Create;
    ViewMailerForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_SENDMAIL', APPCAPTION);

    // INITIALIZE LIST VIEW
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LP';
    lsColumns.Width  :=40;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='CUID';
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer Name';
    lsColumns.Width  :=150;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='To';
    lsColumns.Width  :=100;
    //lsColumns:=CustomerList.Columns.Add;
    //lsColumns.Caption:='Is sent?';
    //lsColumns.Width  :=100;

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


procedure TViewMailerForm.btnSendEmailClick(Sender: TObject); // refactor!!!!
var
    iCNT:    integer;
    TempStr: string;
begin

    if CustomerList.Items.Count > 0 then
    begin

        for iCNT:=0 to CustomerList.Items.Count - 1 do
        begin

            if CustomerList.Items[iCNT].SubItems[2] <> '' then
            begin

                TempStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

                if cbAddOverdue.Checked then
                    SendAccountStatement(True, maCustom, Text_Subject.Text, Text_Salut.Text, TempStr, True, CustomerList.Items[iCNT].SubItems[0], StrToInt(CustomerList.Items[iCNT].Caption))
                        else
                            SendAccountStatement(True, maCustom, Text_Subject.Text, Text_Salut.Text, TempStr, False, CustomerList.Items[iCNT].SubItems[0], StrToInt(CustomerList.Items[iCNT].Caption));

                MainForm.ExecMessage(False, mcInfo, 'Process has been run successfully!');

            end;

        end;

    end;

end;

procedure TViewMailerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


end.
