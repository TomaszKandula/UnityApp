
{$I .\Include\Header.inc}

unit Await;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
    Vcl.Imaging.GIFImg, Vcl.ExtCtrls;

type

    /// <summary>
    ///     Allow to display busy status to the user during processing any "heavy duty task".
    /// </summary>

    TAwaitForm = class(TForm)
        WaitImage: TImage;
        WaitText: TLabel;
    AwaitThdCheck: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AwaitThdCheckTimer(Sender: TObject);
    end;

var
    AwaitForm: TAwaitForm;


implementation

uses
    MassMailer;

{$R *.dfm}


procedure TAwaitForm.FormShow(Sender: TObject);
begin
    (WaitImage.Picture.Graphic as TGIFImage).Animate:=True;
end;

procedure TAwaitForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    (WaitImage.Picture.Graphic as TGIFImage).Animate:=False;
end;

procedure TAwaitForm.AwaitThdCheckTimer(Sender: TObject);
begin
    if ViewMailerForm.ThreadCount = 0 then
        Close;
end;


end.
