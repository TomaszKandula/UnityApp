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
unit Splash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Gauges, StdCtrls, pngimage;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TSplashForm = class(TForm)
    Background: TShape;
    MainText2: TLabel;
    BottomLine: TBevel;
    Progress: TGauge;
    Text2: TLabel;
    Text3: TLabel;
    MainText1: TLabel;
    ProgressBar: TShape;
    Status: TLabel;
    Text4: TLabel;
    ImageDB2: TImage;
    ProgressText: TLabel;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  SplashForm: TSplashForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

{$R *.dfm}

{ ---------------------------------------------------------------- ! APPERANCE ! ---------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------------------- WINDOW SHADOW }
procedure TSplashForm.CreateParams(var Params: TCreateParams);
const CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  with Params do begin
    Style:=WS_POPUP;
    WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
  end;
end;

end.
