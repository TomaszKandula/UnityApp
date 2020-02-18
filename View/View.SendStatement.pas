unit View.SendStatement;

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
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.Imaging.pngimage,
    Unity.Grid,
    Unity.Panel,
    Unity.Records,
    Unity.References;


type


    TSendForm = class(TForm)
        btnCancel: TSpeedButton;
        btnSendEmail: TSpeedButton;
        Text_Custom_Message: TLabel;
        Text_Message: TMemo;
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
    strict private
        var OpenItemsRefs: TFOpenItemsRefs;
        var CtrlStatusRefs: TFCtrlStatusRefs;
        var FPayLoad: TAccDocumentPayLoad;
        procedure ExecuteMailer();
        procedure SendAccDocumentAsync_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
    end;


    function SendForm(): TSendForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Calendar,
    View.Actions,
    Unity.Enums,
    Unity.Constants,
    Unity.Helpers,
    Unity.Settings,
    Unity.Service;


var vSendForm: TSendForm;


function SendForm(): TSendForm;
begin
    if not(Assigned(vSendForm)) then Application.CreateForm(TSendForm, vSendForm);
    Result:=vSendForm;
end;


{$REGION 'LOCAL HELPERS'}


procedure TSendForm.ExecuteMailer();
begin

    if String.IsNullOrEmpty(Text_Message.Text) then
    begin
        THelpers.MsgCall(Warn, 'Please provide custom message and salutation.');
        Exit();
    end;

    if THelpers.MsgCall(Question2, 'Do you want to send it, right now?') = IDNO then Exit();

    var InvFilter: TInvoiceFilter:=TInvoiceFilter.ShowAllItems;

    if cbShowAll.Checked     then InvFilter:=TInvoiceFilter.ShowAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TInvoiceFilter.ReminderOvd;
    if cbNonOverdue.Checked  then InvFilter:=TInvoiceFilter.ReminderNonOvd;

    var TempStr:=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);

    OpenItemsRefs.InitWith(ActionsForm.OpenItemsGrid);
    CtrlStatusRefs.InitWith(MainForm.sgControlStatus);

    FPayLoad.Layout        :=TDocMode.Custom;
    FPayLoad.Subject       :='Account Statement';
    FPayLoad.Message       :=TempStr;
    FPayLoad.InvFilter     :=InvFilter;
    FPayLoad.BeginDate     :=ValBeginDate.Caption;
    FPayLoad.EndDate       :=ValEndDate.Caption;
    FPayLoad.SendFrom      :=ActionsForm.LbuSendFrom;
    FPayLoad.MailTo        :=TArray<string>.Create(ActionsForm.Cust_Mail.Text);
    FPayLoad.SourceDBName  :=ActionsForm.SourceDBName;
    FPayLoad.CustName      :=ActionsForm.CustName;
    FPayLoad.CustNumber    :=ActionsForm.CustNumber;
    FPayLoad.LBUName       :=ActionsForm.LbuName;
    FPayLoad.LBUAddress    :=ActionsForm.LbuAddress;
    FPayLoad.Telephone     :=ActionsForm.LbuPhones;
    FPayLoad.BankDetails   :=ActionsForm.LbuBanksHtml;
    FPayLoad.Exclusions    :=ActionsForm.Exclusions;
    FPayLoad.Series        :=False;
    FPayLoad.ItemNo        :=0;
    FPayLoad.OpenItems     :=ActionsForm.OpenItemsGrid;
    FPayLoad.OpenItemsRefs :=OpenItemsRefs;
    FPayLoad.ControlStatus :=MainForm.sgControlStatus;
    FPayLoad.CtrlStatusRefs:=CtrlStatusRefs;
    FPayLoad.IsCtrlStatus  :=ActionsForm.cbCtrlStatusOff.Checked;
    FPayLoad.IsUserInCopy  :=ActionsForm.cbUserInCopy.Checked;

    Screen.Cursor:=crHourGlass;
    Service.Mediator.Documents.SendAccDocumentAsync(MainForm.LoadedAgeDate, FPayLoad, SendAccDocumentAsync_Callback);

    Close();

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TSendForm.SendAccDocumentAsync_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
begin

    Screen.Cursor:=crDefault;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, CallResponse.LastMessage);

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TSendForm.FormCreate(Sender: TObject);
begin
    PanelMessage.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    cbShowAll.Checked:=True;
    cbOverdueOnly.Checked:=False;
    cbNonOverdue.Checked:=False;
    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';
end;


procedure TSendForm.FormShow(Sender: TObject);
begin
    Text_Message.SetFocus();
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TSendForm.cbShowAllClick(Sender: TObject);
begin

    if cbShowAll.Checked then
    begin
        cbOverdueOnly.Checked:=False;
        cbNonOverdue.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and not(cbNonOverdue.Checked)
        then cbShowAll.Checked:=True;

end;


procedure TSendForm.cbOverdueOnlyClick(Sender: TObject);
begin

    if cbOverdueOnly.Checked then
    begin
        cbShowAll.Checked:=False;
        cbNonOverdue.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and not(cbNonOverdue.Checked)
        then cbOverdueOnly.Checked:=True;

end;


procedure TSendForm.cbNonOverdueClick(Sender: TObject);
begin

    if cbNonOverdue.Checked then
    begin
        cbShowAll.Checked:=False;
        cbOverdueOnly.Checked:=False;
        ImgCover.Visible:=False;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and not(cbNonOverdue.Checked)
        then cbNonOverdue.Checked:=True;

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
    THelpers.WndCall(CalendarForm, TWindowState.Modal);
    ValBeginDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TSendForm.btnEndDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    THelpers.WndCall(CalendarForm, Modal);
    ValEndDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TSendForm.btnSendEmailClick(Sender: TObject);
begin
    ExecuteMailer();
end;


procedure TSendForm.btnCancelClick(Sender: TObject);
begin
    Close();
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TSendForm.Text_SalutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus();
end;


procedure TSendForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbShowAll.SetFocus();
end;


procedure TSendForm.cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbOverdueOnly.SetFocus();
end;


procedure TSendForm.cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbNonOverdue.SetFocus();
end;


procedure TSendForm.cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus();
end;


procedure TSendForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

