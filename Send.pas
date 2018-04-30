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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

{ ------------------------------------------------------------------ ! MAIN CLASS ! ------------------------------------------------------------------------- }
type
  TSendForm = class(TForm)
    AppMain: TShape;
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
    procedure btnCancelClick(Sender: TObject);
    procedure btnSendEmailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  end;

var
  SendForm: TSendForm;

{ -------------------------------------------------------------- ! IMPLEMENTATION ZONE ! -------------------------------------------------------------------- }

implementation

uses
  Main, Settings, Actions;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------- ! EXECUTE ON CREATE ! ----------------------------------------------------------------------- }
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
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SEND EMAIL }
procedure TSendForm.btnSendEmailClick(Sender: TObject);
begin
  { ASK USER BEFORE SENDING THE EMAIL }
  if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure that you really want it to be sent, right now?') = IDNO then Exit;
  ActionsForm.SendAccountStatement(maCustom, Text_Salut.Text, Text_Message.Text);
  ActionsForm.RegisterAction;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TSendForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TSendForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.
