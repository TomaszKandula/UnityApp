{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Update;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, GIFImg, Gauges;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TUpdateForm = class(TForm)
    Background: TShape;
    MainText1: TLabel;
    MainText2: TLabel;
    Text1: TLabel;
    Text3: TLabel;
    BottomLine: TBevel;
    Text2: TLabel;
    Progress: TGauge;
    procedure FormShow(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  UpdateForm: TUpdateForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

{$R *.dfm}

{ ---------------------------------------------------------------- ! APPERANCE ! ---------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------------------- WINDOW SHADOW }
procedure TUpdateForm.CreateParams(var Params: TCreateParams);
const CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  with Params do begin
    Style:=WS_POPUP;
    WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- START ANIMATION }
procedure TUpdateForm.FormShow(Sender: TObject);
begin
  { DO NOTHING }
end;

end.
