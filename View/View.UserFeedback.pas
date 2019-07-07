unit View.UserFeedback;

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
    SYstem.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Unity.Interposer;


type


    TFeedbackForm = class(TForm)
        ReportMemo: TMemo;
        btnSendReport: TSpeedButton;
        Text2: TLabel;
        TotalWords: TLabel;
        btnCancel: TSpeedButton;
        PanelClient: TPanel;
        PanelReportMemo: TPanel;
        PanelArea: TPanel;
        PanelBottom: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnSendReportClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    public
        function WordCount(const InputStr: string): cardinal;
        function SendReport: boolean;
    end;


    function FeedbackForm: TFeedbackForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Settings,
    Sync.Documents,
    Async.Utilities,
    Unity.Statics;


var vFeedbackForm: TFeedbackForm;


const
    ToSkip = [#0..#32, '.', ',', ';', '[', ']', '(', ')', '{', '}'];


function FeedbackForm: TFeedbackForm;
begin
    if not(Assigned(vFeedbackForm)) then Application.CreateForm(TFeedbackForm, vFeedbackForm);
    Result:=vFeedbackForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Count words in TMemo component while user is writing.
/// </summary>

function TFeedbackForm.WordCount(const InputStr: string): cardinal;
begin

    Result:=0;

    var TextLength: integer:=Length(InputStr);
    var FindWord:   boolean:=False;

    // Count if word is found
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
                FindWord:=False;
    end;

end;


function TFeedbackForm.SendReport: boolean;
begin

    Result:=False;

    if ReportMemo.Text = '' then
    begin
        MainForm.MsgCall(TCommon.TMessage.Warn, 'Cannot send empty report. Please write what feels right and then send.');
        Exit;
    end;

    var Settings: ISettings:=TSettings.Create;
    var Mail: IDocument:=TDocument.Create;

    var AppName: string:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'VALUE', '');
    var AppVer: string:=TCommon.GetBuildInfoAsString;

    // Get and set email details
    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM  then
    begin
        Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'FROM', '');
        Mail.MailTo :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'TO', '');
        Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'REPLY-TO', '');
    end;

    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then
    begin
        Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'FROM', '');
        Mail.MailTo :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'TO', '');
        Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'REPLY-TO', '');
    end;

    Mail.MailFrom   :=Mail.XMailer;
    Mail.MailCc     :=MainForm.WinUserName + '@' + Settings.GetStringValue(TConfigSections.ApplicationDetails, 'MAIL_DOMAIN', '');
    Mail.MailBcc    :='';
    Mail.MailSubject:='Unity - User feedback (' + UpperCase(MainForm.WinUserName) + ')';

    // Plain text to HTML using template
    var Transfer: string:=ReportMemo.Text;
    Transfer:=StringReplace(Transfer, TChars.CRLF, '<br>', [rfReplaceAll]);

    var HTMLBody: string:=Mail.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SENDFEEDBACK', '') + '.html');
    HTMLBody:=StringReplace(HTMLBody, '{TEXT_HOLER}',  Transfer,       [rfReplaceAll]);
    HTMLBody:=StringReplace(HTMLBody, '{APPNAME}',     AppName,        [rfReplaceAll]);
    HTMLBody:=StringReplace(HTMLBody, '{BUILD}',       AppVer,         [rfReplaceAll]);
    HTMLBody:=StringReplace(HTMLBody, '{REPORT_DATE}', DateToStr(Now), [rfReplaceAll]);
    HTMLBody:=StringReplace(HTMLBody, '{REPORT_TIME}', TimeToStr(Now), [rfReplaceAll]);

    Mail.MailBody:=HTMLBody;
    Result:=Mail.SendNow;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TFeedbackForm.FormCreate(Sender: TObject);
begin
    PanelReportMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


procedure TFeedbackForm.btnSendReportClick(Sender: TObject);
begin
    var Utilities: IUtilities:=TUtilities.Create;
    Utilities.SendUserFeedback();
end;

procedure TFeedbackForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TFeedbackForm.ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    TotalWords.Caption:=IntToStr(WordCount(ReportMemo.Text)) + ' / ' + IntToStr(ReportMemo.MaxLength);
end;


procedure TFeedbackForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


end.

