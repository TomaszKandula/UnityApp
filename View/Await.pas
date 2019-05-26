unit Await;


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
    InterposerClasses;


type

    /// <summary>
    /// Allow to display busy status to the user during processing any "heavy duty task".
    /// </summary>

    /// <remarks>
    /// We do not allow user to close the window. It is opened and closed by external event.
    /// </remarks>

    TAwaitForm = class(TForm)
        WaitImage: TImage;
        WaitText: TLabel;
        PanelAwaitForm: TPanel;
        PanelText: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    end;


var
    AwaitForm: TAwaitForm;


implementation


uses
    MassMailer;


{$R *.dfm}


/// <summary>
/// Create window with panel borders set to blue. Please note that this window does not have standard
/// window shadow due to borderless type.
/// </summary>

procedure TAwaitForm.FormCreate(Sender: TObject);
begin
    PanelAwaitForm.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


/// <summary>
/// Perform animation when window is about to be shown.
/// </summary>

procedure TAwaitForm.FormShow(Sender: TObject);
begin
    (WaitImage.Picture.Graphic as TGIFImage).Animate:=True;
end;


/// <summary>
/// If window is about to be closed, we disable GIF animation.
/// </summary>

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

