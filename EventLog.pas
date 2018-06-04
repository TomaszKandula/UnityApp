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
unit EventLog;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, pngimage, Main;

{ ---------------------------------------------------------------- ! MAIN CLASS ! --------------------------------------------------------------------------- }
type
  TEventForm = class(TForm)
    EventMemo: TMemo;
    PanelEventMemo: TPanel;
    PanelClient: TPanel;
    PanelBottom: TPanel;
    ImageGrip: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    procedure LoadEventLog;
  end;

var
  EventForm: TEventForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Settings;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ --------------------------------------------------------------------------------------------------------------------------------------- LOAD EVENT LOG FILE }
procedure TEventForm.LoadEventLog;
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    try
      EventMemo.Lines.LoadFromFile(AppSettings.FPathEventLog);
    except
      on E: Exception do
        EventMemo.Lines.Text:='Cannot load event log file. Error has been thorwn: ' + E.Message;
    end;
  finally
    AppSettings.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TEventForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    EventForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_EVENTLOG', APPNAME);
  finally
    AppSettings.Free;
  end;
  { PANEL BORDERS }
  PanelEventMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TEventForm.FormShow(Sender: TObject);
begin
  LoadEventLog;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ REFRESH EVENT LOG [F5] }
procedure TEventForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F5 then
  begin
    LoadEventLog;
    SendMessage(EventMemo.Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TEventForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.
