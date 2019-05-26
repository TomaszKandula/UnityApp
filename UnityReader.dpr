program UnityReader;


{$SetPEFlags $0020}


uses
    Winapi.Windows,
    Vcl.Forms,
    uCEFApplication,
    Reader in 'View\Reader.pas' {FormReader};


{$R *.res}


var
    URLparam:     string;
    WindowHandle: HWND;


begin

    // Find Unity running
    WindowHandle:=FindWindow(nil, PChar('Unity'));

    if not(IsWindow(WindowHandle)) then
    begin
        Application.MessageBox(PChar('Cannot find Unity opened. Process has been stopped.'), PChar('Unity Reader'), MB_OK + MB_ICONWARNING);
        ExitProcess(0);
    end;

    // Require application parameter
    if not(ParamCount = 0) then URLparam:=ParamStr(1);

    // Initialize Chromium application
    GlobalCEFApp:=TCefApplication.Create;
    GlobalCEFApp.BrowserSubprocessPath:='SubProcess.exe';

    // Start Unity Reader in main thread
    if GlobalCEFApp.StartMainProcess then
    begin
        Application.Initialize;
        Application.MainFormOnTaskbar:=True;
        Application.Title:='Unity Reader';
        Application.CreateForm(TFormReader, FormReader);
        if not(URLparam = '') then FormReader.input:=URLparam;
        Application.Run;
    end;

    GlobalCEFApp.Free;

end.

