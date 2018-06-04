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
unit PhoneList;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, StdCtrls, Main;

{ ---------------------------------------------------------------- ! MAIN CLASS ! --------------------------------------------------------------------------- }
type
  TPhoneListForm = class(TForm)
    PhoneList: TMemo;
    btnCancel: TSpeedButton;
    PanelPhoneList: TPanel;
    btnSubmit: TSpeedButton;
    PanelMemo: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSubmitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PhoneListKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
  public
  end;

var
  PhoneListForm: TPhoneListForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

{$R *.dfm}

uses
  Actions, Settings;

{ ############################################################ ! MAIN THREAD EVENTS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TPhoneListForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    ActionsForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_PHONES', APPNAME);
  finally
    AppSettings.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  PanelMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TPhoneListForm.FormShow(Sender: TObject);
begin
  PhoneList.Lines.Clear;
  PhoneList.Lines.AddStrings(ActionsForm.Cust_Phone.Items);
end;

{ ------------------------------------------------------------- ! BUTTONS EVENTS ! -------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- SUBMIT }
procedure TPhoneListForm.btnSubmitClick(Sender: TObject);
begin
  ActionsForm.Cust_Phone.Items.Clear;
  ActionsForm.Cust_Phone.Items.AddStrings(PhoneList.Lines);
  if ActionsForm.Cust_Phone.Items.Count > 0 then ActionsForm.Cust_Phone.ItemIndex:=0;
  Close;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- CANCEL }
procedure TPhoneListForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ ------------------------------------------------------------ ! KEYBOARD EVENTS ! -------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------- RESTRICT KEY INPUT }
procedure TPhoneListForm.PhoneListKeyPress(Sender: TObject; var Key: Char);
const
  MyChars = ['0'..'9', #8, #10, #13];
begin
  if not CharInSet(Key, MyChars) then Key:=#0;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TPhoneListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.
