{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
program UnityReader;

{$SetPEFlags $0020}  { LARGE ADDRESS AWARE | USING MORE THAN 3GB FOR 32-BIT PROGRAM }

uses
  Windows,
  Forms,
  uCEFApplication,
  Reader in 'Reader.pas';

{$R *.res}

var
  URLparam:      string;
  WindowHandle:  HWND;

begin

  { FIND UNITY RUNNING }
  WindowHandle:=FindWindow(nil, PChar('Unity'));
  if not(IsWindow(WindowHandle)) then
  begin
    Application.MessageBox(PChar('Cannot find Unity opened. Process has been stopped.'), PChar('Unity Reader'), MB_OK + MB_ICONWARNING);
    ExitProcess(0);
  end;

  { GET APPLICATION PARAMETER }
  if not(ParamCount = 0) then URLparam:=ParamStr(1);

  { INITIATE CHROMIUM APPLICATION }
  GlobalCEFApp:=TCefApplication.Create;
  GlobalCEFApp.BrowserSubprocessPath:='SubProcess.exe';

  { START UNITY READER IF IN MAIN THREAD }
  if GlobalCEFApp.StartMainProcess then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar:=True;
    Application.Title:='Unity Reader';
    Application.CreateForm(TFormReader, FormReader);
    if not(URLparam = '') then FormReader.input:=URLparam;
    Application.Run;
  end;

  { RELEASE CHROMIUM }
  GlobalCEFApp.Free;
end.
