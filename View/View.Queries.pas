unit View.Queries;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
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
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Unity.Grid,
    Unity.Enums;


type


    TQmsForm = class(TForm)//do not use it until refactored completly!
        MainFrame: TGroupBox;
        BottomPanel: TPanel;
        btnLog: TSpeedButton;
        btnCancel: TSpeedButton;
        MainPanel: TPanel;
        StatusList: TComboBox;
        StatusLabel: TLabel;
        MissingInvoiceBox: TGroupBox;
        StatusPanel: TPanel;
        EditInvoiceNo: TLabeledEdit;
        EditOpenAmount: TLabeledEdit;
        EditAmount: TLabeledEdit;
        EditOpenCurrAm: TLabeledEdit;
        EditDueDate: TLabeledEdit;
        EditValDate: TLabeledEdit;
        EditQueryReason: TLabeledEdit;
        EditLogType: TLabeledEdit;
        EditCurrAmount: TLabeledEdit;
        EditUserAlias: TLabeledEdit;
        EditStamp: TLabeledEdit;
        CurrencyList: TComboBox;
        Currency: TLabel;
        bgEdit1: TShape;
        bgEdit2: TShape;
        bgEdit3: TShape;
        bgEdit4: TShape;
        bgEdit5: TShape;
        bgEdit6: TShape;
        bgEdit7: TShape;
        bgEdit8: TShape;
        bgEdit9: TShape;
        bgEdit10: TShape;
        bgEdit11: TShape;
        btnAddDueDate: TSpeedButton;
        btnAddValDate: TSpeedButton;
        QueryDesc: TMemo;
        QueryDescBorders: TShape;
        QueryDescLabel: TLabel;
        LbuEmailAddress: TLabeledEdit;
        bgEdit: TShape;
        OpenDlgBox: TOpenDialog;
        btnAddAttcahement: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnLogClick(Sender: TObject);
        procedure StatusListSelect(Sender: TObject);
        procedure btnAddDueDateClick(Sender: TObject);
        procedure btnAddValDateClick(Sender: TObject);
        procedure EditOpenAmountKeyPress(Sender: TObject; var Key: Char);
        procedure EditOpenCurrAmKeyPress(Sender: TObject; var Key: Char);
        procedure EditCurrAmountKeyPress(Sender: TObject; var Key: Char);
        procedure EditAmountKeyPress(Sender: TObject; var Key: Char);
        procedure btnAddAttcahementClick(Sender: TObject);
    end;


    function QmsForm(): TQmsForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Actions,
    View.Calendar,
    Unity.Constants,
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService;


var vQmsForm: TQmsForm;


function QmsForm(): TQmsForm;
begin
    if not(Assigned(vQmsForm)) then Application.CreateForm(TQmsForm, vQmsForm);
    Result:=vQmsForm;
end;


procedure TQmsForm.FormCreate(Sender: TObject);
begin
    {Do nothing}
end;


procedure TQmsForm.FormDestroy(Sender: TObject);
begin
    {Do nothing}
end;


procedure TQmsForm.FormShow(Sender: TObject);
begin
    {Do nothing}
end;


procedure TQmsForm.StatusListSelect(Sender: TObject);
begin
    EditQueryReason.Text:=StatusList.Items[StatusList.ItemIndex];
end;


procedure TQmsForm.btnAddAttcahementClick(Sender: TObject);
begin
    //
end;


procedure TQmsForm.btnAddDueDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=TCalendar.GetDate;
    THelpers.WndCall(CalendarForm, TWindowState.Modal);
    if CalendarForm.FSelectedDate <> TDtFormat.NullDate then EditDueDate.Text:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TQmsForm.btnAddValDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=TCalendar.GetDate;
    THelpers.WndCall(CalendarForm, Modal);
    if CalendarForm.FSelectedDate <> TDtFormat.NullDate then EditValDate.Text:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TQmsForm.btnCancelClick(Sender: TObject);
begin
    Close();
end;


procedure TQmsForm.btnLogClick(Sender: TObject);
begin
    //
end;


procedure TQmsForm.EditAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9',  TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditCurrAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditOpenAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditOpenCurrAmKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


end.

