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
    Api.SentDocument;


type


    TSendForm = class(TForm)
        btnCancel: TSpeedButton;
        btnSendEmail: TSpeedButton;
        Text_Custom_Message: TLabel;
        Text_Message: TMemo;
        Text_Warn: TLabel;
        PanelMessage: TPanel;
        PanelClient: TPanel;
        PanelBottom: TPanel;
        PanelOption: TPanel;
        btnBeginDate: TSpeedButton;
        btnDelBegin: TSpeedButton;
        btnDelEnd: TSpeedButton;
        btnEndDate: TSpeedButton;
        ImgCover: TImage;
        Text_Begin: TLabel;
        Text_End: TLabel;
        ValBeginDate: TLabel;
        ValEndDate: TLabel;
        GroupFiltering: TGroupBox;
        cbShowAll: TRadioButton;
        cbOverdueOnly: TRadioButton;
        GroupOptions: TGroupBox;
        cbNonOverdue: TRadioButton;
        cbNotDueOnly: TRadioButton;
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
        procedure cbNotDueOnlyClick(Sender: TObject);
        procedure cbNotDueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    strict private
        procedure ExecuteMailer();
        procedure SendAccountDocument_Callback(PayLoad: TSentDocument);
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
    Unity.Service,
    Api.SendDocument;


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
        THelpers.MsgCall(SendForm.Handle, TAppMessage.Warn, 'Please provide custom message and salutation.');
        Exit();
    end;

    if THelpers.MsgCall(SendForm.Handle, TAppMessage.Question2, 'Do you want to send it, right now?') = IDNO then Exit();

    var InvFilter: string;

    if cbShowAll.Checked     then InvFilter:=TFilterType.StatementAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TFilterType.ReminderOverdueAll;
    if cbNonOverdue.Checked  then InvFilter:=TFilterType.ReminderOverdueRange;
    if cbNotDueOnly.Checked  then InvFilter:=TFilterType.StatementAllFuture;

    var BeginDate:='';
    var EndDate:='';

    if String.IsNullOrEmpty(ValBeginDate.Caption) then BeginDate:=DateToStr(Now()) else BeginDate:=ValBeginDate.Caption;
    if String.IsNullOrEmpty(ValEndDate.Caption) then EndDate:=DateToStr(Now()) else EndDate:=ValEndDate.Caption;

    var FPayLoad:=TSendDocument.Create(1);

    FPayLoad.LayoutType     :=TLayoutType.Custom;
    FPayLoad.ReportedAgeDate:=MainForm.LoadedAgeDate;
    FPayLoad.Subject        :=String.Empty;
    FPayLoad.Message        :=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);
    FPayLoad.UserEmail      :=Service.SessionData.EmailAddress;
    FPayLoad.InvoiceFilter  :=InvFilter;
    FPayLoad.BeginDate      :=BeginDate;
    FPayLoad.EndDate        :=EndDate;
    FPayLoad.IsCtrlStatus  :=not ActionsForm.cbCtrlStatusOff.Checked;
    FPayLoad.IsUserInCopy  :=ActionsForm.cbUserInCopy.Checked;
    FPayLoad.IsSourceInCopy:=ActionsForm.cbIncludeSource.Checked;

    FPayLoad.Documents[0].CustomerNumber:=ActionsForm.CustNumber;
    FPayLoad.Documents[0].SourceDbName  :=ActionsForm.SourceDBName;
    FPayLoad.Documents[0].SendFrom      :=ActionsForm.LbuSendFrom;
    FPayLoad.Documents[0].EmailTo       :=ActionsForm.Cust_Mail.Text;

    Service.Mediator.Documents.SendAccountDocumentAsync(FPayLoad, SendAccountDocument_Callback);
    SendForm.Enabled:=False;
    Screen.Cursor:=crHourGlass;

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TSendForm.SendAccountDocument_Callback(PayLoad: TSentDocument);
begin

    Close();
    Screen.Cursor:=crDefault;

    if not PayLoad.IsSucceeded then
    begin
        THelpers.MsgCall(ActionsForm.Handle, TAppMessage.Error, PayLoad.Error.ErrorDesc);
        Exit();
    end;

    SendForm.Enabled:=True;
    THelpers.MsgCall(SendForm.Handle, TAppMessage.Info, 'Action has been executed successfully!');

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TSendForm.FormCreate(Sender: TObject);
begin
    PanelMessage.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    cbShowAll.Checked:=True;
    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';
end;


procedure TSendForm.FormShow(Sender: TObject);
begin
    if not SendForm.Enabled then SendForm.Enabled:=True;
    Text_Message.SetFocus();
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TSendForm.cbShowAllClick(Sender: TObject);
begin
    if cbShowAll.Checked then ImgCover.Visible:=True;
end;


procedure TSendForm.cbOverdueOnlyClick(Sender: TObject);
begin
    if cbOverdueOnly.Checked then ImgCover.Visible:=True;
end;


procedure TSendForm.cbNonOverdueClick(Sender: TObject);
begin
    if cbNonOverdue.Checked then ImgCover.Visible:=False;
end;


procedure TSendForm.cbNotDueOnlyClick(Sender: TObject);
begin
    if cbNotDueOnly.Checked then ImgCover.Visible:=True;
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
    if Key = VK_TAB then cbNotDueOnly.SetFocus();
end;


procedure TSendForm.cbNotDueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus();
end;


procedure TSendForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

