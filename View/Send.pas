
{$I \Include\Header.inc}

unit Send;


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
    StdCtrls,
    Buttons,
    ExtCtrls,
    InterposerClasses;


type

    /// <summary>
    ///     View form class for custom statement.
    /// </summary>

    TSendForm = class(TForm)
        btnCancel: TSpeedButton;
        btnSendEmail: TSpeedButton;
        Text_Custom_Message: TLabel;
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
        PanelMessage: TPanel;
        PanelClient: TPanel;
        PanelBottom: TPanel;
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure Text_SalutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbAddOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
    private
        procedure ExecuteMailer;
    end;


var
    SendForm: TSendForm;


implementation


uses
    Main,
    Settings,
    Worker,
    Actions,
    Model;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Send statement with open items.
/// </summary>

procedure TSendForm.ExecuteMailer;
var
    TempStr: string;
begin
    if String.IsNullOrEmpty(Text_Message.Text) then
    begin
        MainForm.MsgCall(mcWarn, 'Please provide with custom message and salutation.');
        Exit;
    end;

    if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure you want to send it, right now?') = IDNO then
        Exit;

    TempStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

    /// <remarks>
    ///     UpdateOpenItemsRefs and UpdateControlStatusRefs must be executed before TTSendAccountStatement is called!
    /// </remarks>

    MainForm.UpdateOpenItemsRefs(ActionsForm.OpenItemsGrid);
    MainForm.UpdateControlStatusRefs(MainForm.sgControlStatus);
    TTSendAccountStatement.Create(
        maCustom,
        'Account Statement',
        TempStr,
        cbAddOverdue.Checked,
        ActionsForm.OpenItemsGrid,
        ActionsForm.CUID,
        ActionsForm.Lbu_SendFrom.Caption,
        ActionsForm.CustName,
        ActionsForm.CustNumber,
        ActionsForm.Lbu_Name.Caption,
        ActionsForm.Lbu_Address.Caption,
        ActionsForm.Lbu_Phone.Caption,
        ActionsForm.BanksHtml
    );

    Close;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TSendForm.FormCreate(Sender: TObject);
var
    Settings: ISettings;
begin
    Settings:=TSettings.Create;
    SendForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_SEND', APPCAPTION);
    PanelMessage.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


procedure TSendForm.FormShow(Sender: TObject);
begin
    Text_Message.SetFocus;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


procedure TSendForm.btnSendEmailClick(Sender: TObject);
begin
    ExecuteMailer;
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


procedure TSendForm.Text_SalutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus;
end;


procedure TSendForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbAddOverdue.SetFocus;
end;


procedure TSendForm.cbAddOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus;
end;


end.

