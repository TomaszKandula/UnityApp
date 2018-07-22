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
unit Send;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Main;

{ ----------------------------------------------------------------- ! MAIN CLASS ! -------------------------------------------------------------------------- }
type
  TSendForm = class(TForm)
    btnCancel: TSpeedButton;
    btnSendEmail: TSpeedButton;
    Text_Salut: TMemo;
    Text1: TLabel;
    Text2: TLabel;
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
    PanelSalutation: TPanel;
    PanelMessage: TPanel;
    PanelClient: TPanel;
    PanelBottom: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSendEmailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  end;

var
  SendForm: TSendForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Settings, Actions;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TSendForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { ----------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
    SendForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_SENDMAIL', APPNAME);
  finally
    AppSettings.Free;
  end;
  { PANELS BORDERS }
  PanelSalutation.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelMessage.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

{ ----------------------------------------------------------------- ! MOUSE EVENTS ! ------------------------------------------------------------------------ }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SEND EMAIL }
procedure TSendForm.btnSendEmailClick(Sender: TObject);
var
  TempStr: string;
begin

  if (Text_Salut.Text = '') or (Text_Message.Text = '') then
  begin
    MainForm.MsgCall(mcWarn, 'Please provide with custom message and salutation.');
    Exit;
  end;

  if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure you want to send it, right now?') = IDNO then Exit;

  TempStr:=StringReplace(Text_Message.Text, CRLF, HTML_BR, [rfReplaceAll]);

  if cbAddOverdue.Checked then
    ActionsForm.SendAccountStatement(maCustom, Text_Salut.Text, TempStr, True)
      else
        ActionsForm.SendAccountStatement(maCustom, Text_Salut.Text, TempStr, False);

  Close;

end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TSendForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ --------------------------------------------------------------- ! KEYBOARD EVENTS ! ----------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TSendForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.

