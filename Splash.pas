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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Gauges, StdCtrls, pngimage, Main;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TSplashForm = class(TForm)
    MainText2: TLabel;
    ProgressBar: TGauge;
    TextFooterA: TLabel;
    TextFooter2B: TLabel;
    MainText1: TLabel;
    TextStatus: TLabel;
    SubText: TLabel;
    TextProgress: TLabel;
    ShapeProgressBar: TShape;
    ShapeBackground: TShape;
    procedure FormCreate(Sender: TObject);
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

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TSplashForm.FormCreate(Sender: TObject);
begin
  { DO NOTHING }
end;

end.
