unit View.UserFeedback;

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
    SYstem.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.ComCtrls,
    Vcl.Imaging.pngimage,
    Unity.Enums,
    Unity.Panel,
    Unity.Records;


type


    TFeedbackForm = class(TForm)
        ReportMemo: TMemo;
        btnSendReport: TSpeedButton;
        txtWords: TLabel;
        TotalWords: TLabel;
        PanelClient: TPanel;
        PanelReportMemo: TPanel;
        PanelBottom: TPanel;
        ImageGrip: TImage;
        PanelText: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnSendReportClick(Sender: TObject);
    strict private
        procedure SendFeedbackAsync_Callback(CallResponse: TCallResponse);
    public
        var FSetLastSelection: TTabSheet;
    end;


    function FeedbackForm(): TFeedbackForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Service,
    Unity.Helpers;


var vFeedbackForm: TFeedbackForm;


function FeedbackForm(): TFeedbackForm;
begin
    if not(Assigned(vFeedbackForm)) then Application.CreateForm(TFeedbackForm, vFeedbackForm);
    Result:=vFeedbackForm;
end;


{$REGION 'CALLBACKS'}


procedure TFeedbackForm.SendFeedbackAsync_Callback(CallResponse: TCallResponse);
begin

    PanelClient.Enabled:=True;
    Screen.Cursor:=crDefault;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, 'Your feedback has been sent successfully!');

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TFeedbackForm.FormCreate(Sender: TObject);
begin
    PanelReportMemo.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TFeedbackForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainForm.SetActiveTabsheet(FSetLastSelection);
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TFeedbackForm.btnSendReportClick(Sender: TObject);
begin

    if ReportMemo.Text = '' then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Cannot send empty report. Please write what feels right and then send.');
        Exit();
    end;

    PanelClient.Enabled:=False;
    Screen.Cursor:=crHourGlass;

    Service.Mediator.Mailer.SendFeedbackAsync(ReportMemo.Text, SendFeedbackAsync_Callback);

end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TFeedbackForm.ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    TotalWords.Caption:=IntToStr(THelpers.WordCount(ReportMemo.Text)) + ' / ' + IntToStr(ReportMemo.MaxLength);
end;


procedure TFeedbackForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

