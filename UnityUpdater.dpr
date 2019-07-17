program UnityUpdater;


// ----------------------------------------
// UnityUpdater for Unity application
// Copyright (C) 2018-2019 Tomasz Kandula
// VCL/Win32 application for Windows 7 & 10
// ----------------------------------------


uses
    Vcl.Forms,
    View.Updater in 'View\View.Updater.pas' {FormUpdater};


{$R *.res}


begin
    Application.Initialize;
    Application.MainFormOnTaskbar:=True;
    Application.CreateForm(TFormUpdater, FormUpdater);
    Application.Run;
end.

