unit SendFeedback;


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
    InterposerClasses;


type


    TReportForm = class(TForm)
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


var
    ReportForm: TReportForm;


implementation


uses
    Main,
    Mailer,
    Settings,
    Worker,
    Helpers;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Count words in TMemo component while user is writing.
/// </summary>

function TReportForm.WordCount(const InputStr: string): cardinal;
const
    ToSkip = [#0..#32, '.', ',', ';', '[', ']', '(', ')', '{', '}'];
var
    iCNT:       integer;
    TextLength: integer;
    FindWord:   boolean;
begin

    Result:=0;
    TextLength:=Length(InputStr);
    FindWord:=False;

    // Count if word is found
    for iCNT:=1 to TextLength do
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


/// <summary>
/// Send feedback with user comments.
/// </summary>

function TReportForm.SendReport: boolean;
var
    Mail:      TMailer;
    Settings:  ISettings;
    Doc:       TDocument;
    HTMLBody:  string;
    Transfer:  string;
    AppName:   string;
    AppVer:    string;
begin

    Result:=False;

    if ReportMemo.Text = '' then
    begin
        MainForm.MsgCall(TCommon.TMsgTypes.Warn, 'Cannot send empty report. Please write what feels right and then send.');
        Exit;
    end;

    Settings:=TSettings.Create;
    Mail  :=TMailer.Create;
    Doc   :=TDocument.Create;

    try

        AppName:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'VALUE', '');
        AppVer :=GetBuildInfoAsString;

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
        Transfer:=ReportMemo.Text;
        Transfer:=StringReplace(Transfer, TUChars.CRLF, '<br>', [rfReplaceAll]);
        HTMLBody:=Doc.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SENDFEEDBACK', '') + '.html');
        HTMLBody:=StringReplace(HTMLBody, '{TEXT_HOLER}',  Transfer,       [rfReplaceAll]);
        HTMLBody:=StringReplace(HTMLBody, '{APPNAME}',     AppName,        [rfReplaceAll]);
        HTMLBody:=StringReplace(HTMLBody, '{BUILD}',       AppVer,         [rfReplaceAll]);
        HTMLBody:=StringReplace(HTMLBody, '{REPORT_DATE}', DateToStr(Now), [rfReplaceAll]);
        HTMLBody:=StringReplace(HTMLBody, '{REPORT_TIME}', TimeToStr(Now), [rfReplaceAll]);

        // Assign and send
        Mail.MailBody:=HTMLBody;
        Result:=Mail.SendNow;

    finally
        Mail.Free;
        Doc.Free;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TReportForm.FormCreate(Sender: TObject);
begin
    PanelReportMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


procedure TReportForm.btnSendReportClick(Sender: TObject);
begin
    TTSendUserFeedback.Create;
end;

procedure TReportForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TReportForm.ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    TotalWords.Caption:=IntToStr(WordCount(ReportMemo.Text)) + ' / ' + IntToStr(ReportMemo.MaxLength);
end;


procedure TReportForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


end.

