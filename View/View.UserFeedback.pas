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
    Unity.Enums,
    Unity.Grid,
    Unity.Panel,
    Unity.Records, Vcl.Imaging.pngimage;


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
        procedure ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnSendReportClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    strict private
        function WordCount(const InputStr: string): cardinal;
        procedure SendFeedbackAsync_Callback(CallResponse: TCallResponse);
    public
        var FSetLastSelection: TTabSheet;
    end;


    function FeedbackForm(): TFeedbackForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Async.Utilities,
    Unity.Helpers;


const
    ToSkip = [#0..#32, '.', ',', ';', '[', ']', '(', ')', '{', '}'];


var vFeedbackForm: TFeedbackForm;


function FeedbackForm(): TFeedbackForm;
begin
    if not(Assigned(vFeedbackForm)) then Application.CreateForm(TFeedbackForm, vFeedbackForm);
    Result:=vFeedbackForm;
end;


{$REGION 'LOCAL HELPERS'}


function TFeedbackForm.WordCount(const InputStr: string): cardinal;
begin

    Result:=0;

    var TextLength: integer:=Length(InputStr);
    var FindWord:   boolean:=False;

    // Count words in TMemo component while user is typing
    for var iCNT: integer:=1 to TextLength do
    begin

        if not (CharInSet(InputStr[iCNT], ToSkip)) then
        begin

            if not FindWord then
            begin
                FindWord:=True;
                Inc(Result);
            end;

        end
        else
        begin
            FindWord:=False;
        end;

    end;

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TFeedbackForm.SendFeedbackAsync_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, 'Report has been sent successfully!');

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

    var Utilities: IUtilities:=TUtilities.Create();
    Utilities.SendFeedbackAsync(ReportMemo.Text, SendFeedbackAsync_Callback);

end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TFeedbackForm.ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    TotalWords.Caption:=IntToStr(WordCount(ReportMemo.Text)) + ' / ' + IntToStr(ReportMemo.MaxLength);
end;


procedure TFeedbackForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

