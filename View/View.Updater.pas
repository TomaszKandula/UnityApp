unit View.Updater;


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
    Vcl.Samples.Gauges,
    Vcl.StdCtrls,
    Vcl.ExtCtrls;


type

    TFormUpdater = class(TForm)
        Background: TShape;
        MainText1: TLabel;
        MainText2: TLabel;
        Progress: TGauge;
        ShapeProgressBar: TShape;
        Text1: TLabel;
        Text2: TLabel;
        Text3: TLabel;
        Text4: TLabel;
        txtProgress: TLabel;
    protected
        procedure CreateParams(var Params: TCreateParams); override;
    private

    public

    end;


var FormUpdater: TFormUpdater;


implementation



{$R *.dfm}


// ----------------------------------------------------------------------------------------------------------------------------------------------- APPERANCE //


procedure TFormUpdater.CreateParams(var Params: TCreateParams);
begin

    inherited CreateParams(Params);

    with Params do begin
        Style:=WS_POPUP;
        WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
    end;

end;


end.

