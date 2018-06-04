{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit ReportBug;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Main;

{ ------------------------------------------------------------------ ! MAIN CLASS ! ------------------------------------------------------------------------- }
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
    procedure ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnSendReportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    function WordCount(const InputStr: string): cardinal;
    function SendReport: boolean;
  end;

var
  ReportForm: TReportForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Mailer, Settings, Worker;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------- ! EXECUTE ON CREATE ! ----------------------------------------------------------------------- }
procedure TReportForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { ----------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
    ReportForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_BUGREPORT', APPNAME);
  finally
    AppSettings.Free;
  end;
  { PANEL BORDERS }
  PanelReportMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- SHOW WORD COUNT }
function TReportForm.WordCount(const InputStr: string): cardinal;
const
  ToSkip = [#0..#32, '.', ',', ';', '[', ']', '(', ')', '{', '}'];
var
  iCNT:       integer;
  TextLength: integer;
  FindWord:   boolean;
begin
  { INITIALIZE }
  Result:=0;
  TextLength:=Length(InputStr);
  FindWord:=False;
  { COUNT IF WORD IS FOUND }
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

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SEND EMAIL }
function TReportForm.SendReport: boolean;
var
  Mail:      TMailer;
  AppSet:    TSettings;
  Doc:       TDocument;
  HTMLBody:  string;
  Transfer:  string;
  AppName:   string;
  AppVer:    string;
begin
  Result:=False;
  { QUIT IF EMPTY }
  if ReportMemo.Text = '' then
  begin
    MainForm.MsgCall(mcWarn, 'Cannot send empty report. Please write what feels right and then send.');
    Exit;
  end;
  { PROCEED }
  AppSet:=TSettings.Create;
  Mail  :=TMailer.Create;
  Doc   :=TDocument.Create;
  try
    { GET APP NAME AND VERSION }
    AppName         :=AppSet.TMIG.ReadString(ApplicationDetails, 'VALUE', '');
    AppVer          :=GetBuildInfoAsString;
    { SET EMAIL DETAILS }
    if AppSet.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerNTLM  then
    begin
      Mail.XMailer    :=AppSet.TMIG.ReadString(MailerNTLM, 'FROM', '');
      Mail.MailTo     :=AppSet.TMIG.ReadString(MailerNTLM, 'TO', '');
      Mail.MailRt     :=AppSet.TMIG.ReadString(MailerNTLM, 'REPLY-TO', '');
    end;
    if AppSet.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerBASIC then
    begin
      Mail.XMailer    :=AppSet.TMIG.ReadString(MailerBASIC, 'FROM', '');
      Mail.MailTo     :=AppSet.TMIG.ReadString(MailerBASIC, 'TO', '');
      Mail.MailRt     :=AppSet.TMIG.ReadString(MailerBASIC, 'REPLY-TO', '');
    end;
    Mail.MailFrom   :=Mail.XMailer;
    Mail.MailCc     :=MainForm.WinUserName + '@' + AppSet.TMIG.ReadString(ApplicationDetails, 'MAIL_DOMAIN', '');
    Mail.MailBcc    :='';
    Mail.MailSubject:='Unity Bug Report from User: ' + UpperCase(MainForm.WinUserName);
    { PLAIN TEXT TO HTML TEMPLATE }
    Transfer        :=ReportMemo.Text;
    Transfer        :=StringReplace(Transfer, CRLF, HTML_BR, [rfReplaceAll]);
    HTMLBody        :=Doc.LoadTemplate(AppSet.FLayoutDir + AppSet.TMIG.ReadString(VariousLayouts, 'BUGREPORT', '') + '.html');
    HTMLBody        :=StringReplace(HTMLBody, '{TEXT_HOLER}',  Transfer,       [rfReplaceAll]);
    HTMLBody        :=StringReplace(HTMLBody, '{APPNAME}',     AppName,        [rfReplaceAll]);
    HTMLBody        :=StringReplace(HTMLBody, '{BUILD}',       AppVer,         [rfReplaceAll]);
    HTMLBody        :=StringReplace(HTMLBody, '{REPORT_DATE}', DateToStr(Now), [rfReplaceAll]);
    HTMLBody        :=StringReplace(HTMLBody, '{REPORT_TIME}', TimeToStr(Now), [rfReplaceAll]);
    { ASSIGN PREPARED HTML }
    Mail.MailBody   :=HTMLBody;
    { SEND }
    Result:=Mail.SendNow;
  finally
    AppSet.Free;
    Mail.Free;
    Doc.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- SHOW WORD COUNT }
procedure TReportForm.ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  TotalWords.Caption:=IntToStr(WordCount(ReportMemo.Text)) + ' / ' + IntToStr(ReportMemo.MaxLength);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- SEND MEMO }
procedure TReportForm.btnSendReportClick(Sender: TObject);
begin
  TTSendBugReport.Create;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TReportForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TReportForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.
