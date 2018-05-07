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
unit TicketList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, Main;

{ ------------------------------------------------------------------ ! MAIN CLASS ! ------------------------------------------------------------------------- }
type
  TTicketForm = class(TForm)
    AppMain: TShape;
    TicketList: TListBox;
    btnSelect: TSpeedButton;
    btnCancel: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
  private
  public
  end;

var
  TicketForm: TTicketForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Settings, Supplier;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------- ! EXECUTE ON CREATE ! ----------------------------------------------------------------------- }
procedure TTicketForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { ----------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
    TicketForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_TICKETLIST', APPNAME);
  finally
    AppSettings.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- PROCESS GIVEN TICKET }
procedure TTicketForm.btnSelectClick(Sender: TObject);
begin
//
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TTicketForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
