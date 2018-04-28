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

begin
  if ParamCount = 0 then ExitProcess(0) else { PASS PARAMETER }
  GlobalCEFApp:=TCefApplication.Create;
  if GlobalCEFApp.StartSubProcess then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar:=True;
    Application.CreateForm(TFormReader, FormReader);
    Application.Run;
  end;
  GlobalCEFApp.Free;
end.
