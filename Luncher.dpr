program Luncher;


uses
    Vcl.Forms,
    View.Luncher in 'View\View.Luncher.pas';


{$R *.res}


begin
    Application.Initialize;
    Application.MainFormOnTaskbar:=False;
    LuncherForm.Show();
    Application.Run;
end.
