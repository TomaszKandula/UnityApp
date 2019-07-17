unit View.StartupScreen;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views use Lazy Initialization pattern.
// ------------------------------------------------------------------------------

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
    Vcl.Imaging.pngimage;


type


    TStartupForm = class(TForm)
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
        Flutter: TImage;
    protected
    FlutterText: TLabel;
        procedure CreateParams(var Params: TCreateParams); override;
    end;


    function StartupForm: TStartupForm;


implementation


{$R *.dfm}


var
    VStartupForm: TStartupForm;


function StartupForm: TStartupForm;
begin
    if not(Assigned(VStartupForm)) then Application.CreateForm(TStartupForm, VStartupForm);
    Result:=VStartupForm;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------------- APPERANCE //


procedure TStartupForm.CreateParams(var Params: TCreateParams);
begin

    inherited CreateParams(Params);

    with Params do begin
        Style:=WS_POPUP;
        WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
    end;

end;


end.

