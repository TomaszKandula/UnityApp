
{$I \Include\Header.inc}

unit Send;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, InterposerClasses;


type

    /// <summary>
    ///     View form class for custom statement.
    /// </summary>

    TSendForm = class(TForm)
        btnCancel: TSpeedButton;
        btnSendEmail: TSpeedButton;
        Text_Salut: TMemo;
        Text1: TLabel;
        Text2: TLabel;
        Text_Message: TMemo;
        Shape_Customer: TShape;
        Shape_Invoices: TShape;
        Shape_Business: TShape;
        Shape_Terms: TShape;
        Shape_Banks: TShape;
        Shape_Footer: TShape;
        Text_Business: TLabel;
        Text_Customer: TLabel;
        Text_Invoices: TLabel;
        Text_Footer: TLabel;
        Text_Banks: TLabel;
        Text_Terms: TLabel;
        Text_Warn: TLabel;
        cbAddOverdue: TCheckBox;
        PanelSalutation: TPanel;
        PanelMessage: TPanel;
        PanelClient: TPanel;
        PanelBottom: TPanel;
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    end;

var
    SendForm: TSendForm;


implementation


uses
    Main, Settings, Worker, Actions, Model;

{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TSendForm.FormCreate(Sender: TObject);
var
    Settings: ISettings;
begin
    Settings:=TSettings.Create;
    SendForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_SEND', APPCAPTION);
    PanelSalutation.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelMessage.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //

/// <summary>
///     Send statement with open items.
/// </summary>

procedure TSendForm.btnSendEmailClick(Sender: TObject);
var
    TempStr: string;
begin

    if (Text_Salut.Text = '') or (Text_Message.Text = '') then
    begin
        MainForm.MsgCall(mcWarn, 'Please provide with custom message and salutation.');
        Exit;
    end;

    if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure you want to send it, right now?') = IDNO then
        Exit;

    TempStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

    MainForm.UpdateOpenItemsRefs(ActionsForm.OpenItemsGrid);
    TTSendAccountStatement.Create(
        maCustom,
        'Account Statement',
        Text_Salut.Text,
        TempStr,
        cbAddOverdue.Checked,
        ActionsForm.OpenItemsGrid,
        ActionsForm.SCUID,
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), MainForm.sgAgeView.Row],
        MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAGENT,           1, 1), MainForm.sgAgeView.Row]
    );

    Close;

end;

procedure TSendForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TSendForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = ESC then Close;
end;


end.

