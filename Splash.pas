{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
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
    Text1: TLabel;
    Text2: TLabel;
    Text3: TLabel;
    MainText1: TLabel;
    ProgressBar: TShape;
    Status: TLabel;
    Text4: TLabel;
    ImageDB2: TImage;
    ProgressText: TLabel;
    BtnMinimize: TLabel;
    procedure BtnMinimizeClick(Sender: TObject);
    procedure BtnMinimizeMouseEnter(Sender: TObject);
    procedure BtnMinimizeMouseLeave(Sender: TObject);
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

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------------------- MINIMIZE FORM }
procedure TSplashForm.BtnMinimizeClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_MINIMIZE);
end;

{ --------------------------------------------------------------- ! MOUSE EVENTS ! -------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------------------------------------------------------------------- ON MOUSE ENTER }
procedure TSplashForm.BtnMinimizeMouseEnter(Sender: TObject);
begin
  BtnMinimize.Color     :=clSkyBlue;
  BtnMinimize.Font.Color:=clWhite;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- ON MOUSE LEAVE }
procedure TSplashForm.BtnMinimizeMouseLeave(Sender: TObject);
begin
  BtnMinimize.Color     :=clWhite;
  BtnMinimize.Font.Color:=clGray;
end;

end.
