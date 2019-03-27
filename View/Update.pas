
{$I .\Include\Header.inc}

unit Update;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    GIFImg,
    Gauges;


type

    /// <summary>
    ///     View form class shown during update process. This window requires to be updated to display changed content.
    /// </summary>

    TUpdateForm = class(TForm)
        Background: TShape;
        MainText1: TLabel;
        MainText2: TLabel;
        Text1: TLabel;
        Text4: TLabel;
        Text3: TLabel;
        Progress: TGauge;
        ShapeProgressBar: TShape;
        Text2: TLabel;
        txtProgress: TLabel;
    protected
//        procedure CreateParams(var Params: TCreateParams); override;
    end;


var
    UpdateForm: TUpdateForm;


implementation


{$R *.dfm}


// ----------------------------------------------------------------------------------------------------------------------------------------------- APPERANCE //


//procedure TUpdateForm.CreateParams(var Params: TCreateParams);
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

