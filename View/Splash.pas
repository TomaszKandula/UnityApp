
{$I .\Include\Header.inc}

unit Splash;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.Samples.Gauges,
    Vcl.StdCtrls,
    Vcl.Imaging.pngimage,
    Main;


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
        CentreText: TLabel;
        FireMonkey: TImage;
    protected
        {procedure CreateParams(var Params: TCreateParams); override;}
    end;


var
  SplashForm: TSplashForm;


implementation


{$R *.dfm}


// ----------------------------------------------------------------------------------------------------------------------------------------------- APPERANCE //


//procedure TSplashForm.CreateParams(var Params: TCreateParams);
//begin
//
//    inherited CreateParams(Params);
//
//    with Params do begin
//        Style:=WS_POPUP;
//        WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
//    end;
//
//end;


end.

