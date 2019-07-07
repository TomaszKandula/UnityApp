unit View.AwaitScreen;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views except MainForm use Lazy Loading design pattern.
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
    Vcl.StdCtrls,
    Vcl.Imaging.GIFImg,
    Vcl.ExtCtrls,
    Unity.Interposer;


type

    /// <summary>
    /// Allow to display busy status to the user during processing any "heavy duty task".
    /// </summary>

    /// <remarks>
    /// We do not allow user to close the window. It is opened and closed by external event.
    /// </remarks>

    TAwaitForm = class(TForm)
        WaitImage: TImage;
        PanelAwaitForm: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    end;


    function AwaitForm: TAwaitForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.MassMailer;


var vAwaitForm: TAwaitForm;


function AwaitForm: TAwaitForm;
begin
    if not(Assigned(vAwaitForm)) then Application.CreateForm(TAwaitForm, vAwaitForm);
    Result:=vAwaitForm;
end;


procedure TAwaitForm.FormCreate(Sender: TObject);
begin
    PanelAwaitForm.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


procedure TAwaitForm.FormShow(Sender: TObject);
begin

    // Make sure Await window is always displayed in the centre of main form
    AwaitForm.Top :=MainForm.Top  + (MainForm.Height div 2) - (AwaitForm.Height div 2);
    AwaitForm.Left:=MainForm.Left + (MainForm.Width  div 2) - (AwaitForm.Width  div 2);

    (WaitImage.Picture.Graphic as TGIFImage).Animate:=True;

end;


procedure TAwaitForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    (WaitImage.Picture.Graphic as TGIFImage).Animate:=False;
end;


/// <remarks>
/// <ALT> + <F4> combination for window close is disabled.
/// </remarks>

procedure TAwaitForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if
    (
        Key=VK_F4
    )
    and
    (
        Shift=[ssALT]
    )
    then
        Key:=0;
end;


end.

