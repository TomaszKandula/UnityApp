unit View.SendStatement;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views except MainForm use Lazy Loading design pattern.
// ------------------------------------------------------------------------------

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
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.Imaging.pngimage,
    Unity.Interposer,
    Unity.Records;


type


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
        cbShowAll: TCheckBox;
        PanelMessage: TPanel;
        PanelClient: TPanel;
        PanelBottom: TPanel;
        Shape_Options: TShape;
        Text_Options: TLabel;
        cbOverdueOnly: TCheckBox;
        cbNonOverdue: TCheckBox;
        PanelOption: TPanel;
        btnBeginDate: TSpeedButton;
        btnDelBegin: TSpeedButton;
        btnDelEnd: TSpeedButton;
        btnEndDate: TSpeedButton;
        DueDateLabel: TLabel;
        ImgCover: TImage;
        Shape_Dates: TShape;
        Text_Begin: TLabel;
        Text_End: TLabel;
        ValBeginDate: TLabel;
        ValEndDate: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure Text_SalutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure cbShowAllClick(Sender: TObject);
        procedure cbOverdueOnlyClick(Sender: TObject);
        procedure cbNonOverdueClick(Sender: TObject);
        procedure cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnBeginDateClick(Sender: TObject);
        procedure btnEndDateClick(Sender: TObject);
        procedure btnDelBeginClick(Sender: TObject);
        procedure btnDelEndClick(Sender: TObject);
    private
        var Fields: TSendAccountStatementFields;
        procedure ExecuteMailer;
    end;


    function SendForm: TSendForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Calendar,
    View.Actions,
    Settings,
    DbModel,
    Unity.Statics,
    Unity.Enums,
    Async.Statements;


var vSendForm: TSendForm;


function SendForm: TSendForm;
begin
    if not(Assigned(vSendForm)) then Application.CreateForm(TSendForm, vSendForm);
    Result:=vSendForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


procedure TSendForm.ExecuteMailer;
begin

    if String.IsNullOrEmpty(Text_Message.Text) then
    begin
        MainForm.MsgCall(Warn, 'Please provide custom message and salutation.');
        Exit;
    end;

    if MainForm.MsgCall(Question2, 'Are you absolutely sure you want to send it, right now?') = IDNO then
        Exit;

    var InvFilter: TInvoiceFilter:=TInvoiceFilter.ShowAllItems;
    if cbShowAll.Checked     then InvFilter:=TInvoiceFilter.ShowAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TInvoiceFilter.ReminderOvd;
    if cbNonOverdue.Checked  then InvFilter:=TInvoiceFilter.ReminderNonOvd;

    var TempStr: string:=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);

    /// <remarks>
    /// UpdateOpenItemsRefs and UpdateControlStatusRefs must be executed before TTSendAccountStatement is called.
    /// </remarks>
    MainForm.UpdateOpenItemsRefs(ActionsForm.OpenItemsGrid);
    MainForm.UpdateControlStatusRefs(MainForm.sgControlStatus);

    Fields.Layout     :=TDocMode.Custom;
    Fields.Subject    :='Account Statement';
    Fields.Mess       :=TempStr;
    Fields.InvFilter  :=InvFilter;
    Fields.BeginDate  :=ValBeginDate.Caption;
    Fields.EndDate    :=ValEndDate.Caption;
    Fields.OpenItems  :=ActionsForm.OpenItemsGrid;
    Fields.CUID       :=ActionsForm.CUID;
    Fields.SendFrom   :=ActionsForm.LbuSendFrom;
    Fields.MailTo     :=ActionsForm.Cust_Mail.Text;
    Fields.CustName   :=ActionsForm.CustName;
    Fields.CustNumber :=ActionsForm.CustNumber;
    Fields.LBUName    :=ActionsForm.LbuName;
    Fields.LBUAddress :=ActionsForm.LbuAddress;
    Fields.Telephone  :=ActionsForm.LbuPhone;
    Fields.BankDetails:=ActionsForm.BanksHtml;
    Fields.Series     :=False;
    Fields.ItemNo     :=0;

    var Statements: IStatements:=TStatements.Create();
    Statements.SendAccountStatement(Fields);

    Close;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TSendForm.FormCreate(Sender: TObject);
begin
    PanelMessage.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    cbShowAll.Checked:=True;
    cbOverdueOnly.Checked:=False;
    cbNonOverdue.Checked:=False;
    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';
end;


procedure TSendForm.FormShow(Sender: TObject);
begin
    Text_Message.SetFocus;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


procedure TSendForm.cbShowAllClick(Sender: TObject);
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


procedure TSendForm.cbOverdueOnlyClick(Sender: TObject);
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


procedure TSendForm.cbNonOverdueClick(Sender: TObject);
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


procedure TSendForm.btnDelBeginClick(Sender: TObject);
begin
    ValBeginDate.Caption:='';
end;


procedure TSendForm.btnDelEndClick(Sender: TObject);
begin
    ValEndDate.Caption:='';
end;


procedure TSendForm.btnBeginDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    MainForm.WndCall(CalendarForm, TWindowState.Modal);
    ValBeginDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TSendForm.btnEndDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    MainForm.WndCall(CalendarForm, Modal);
    ValEndDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


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
    if Key = Char(VK_ESCAPE) then Close;
end;


procedure TSendForm.Text_SalutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus;
end;


procedure TSendForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbShowAll.SetFocus;
end;


procedure TSendForm.cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbOverdueOnly.SetFocus;
end;


procedure TSendForm.cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbNonOverdue.SetFocus;
end;


procedure TSendForm.cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus;
end;


end.

