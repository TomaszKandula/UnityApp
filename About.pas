{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ShellAPI, IniFiles, pngimage;

{ ------------------------------------------------------------------ ! MAIN CLASS ! ------------------------------------------------------------------------- }
type
  TAboutForm = class(TForm)
    AppMain: TShape;
    Shape1: TShape;
    Shape2: TShape;
    btn_Close: TButton;
    Shape3: TShape;
    Shape4: TShape;
    Text12: TLabel;
    Text13: TLabel;
    Text03: TLabel;
    Text05: TLabel;
    Text07: TLabel;
    LineHor3: TBevel;
    Text08: TLabel;
    Text10: TLabel;
    Text11: TLabel;
    Text06: TLabel;
    Text01: TLabel;
    Text02: TLabel;
    Text04: TLabel;
    txt_VER: TLabel;
    txt_EDT: TLabel;
    txt_INQ: TLabel;
    txt_LIC: TLabel;
    txt_ITS: TLabel;
    txt_SYS: TLabel;
    txt_MEM: TLabel;
    txt_USG: TLabel;
    Text14: TLabel;
    txt_STA: TLabel;
    Text15: TLabel;
    txt_WEB: TLabel;
    LineHor2: TBevel;
    LineHor1: TBevel;
    Text16: TLabel;
    txt_PRO: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image4: TImage;
    TechLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btn_CloseClick(Sender: TObject);
    procedure txt_INQClick(Sender: TObject);
    procedure txt_ITSClick(Sender: TObject);
    procedure txt_WEBClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    function Is64BitOS: Boolean;
  end;

{ ----------------------------------------------------------- ! GROUP OF DATA FIELDS ! ---------------------------------------------------------------------- }
type
  TMemoryStatusEx = packed record
    dwLength:                 DWORD;
    dwMemoryLoad:             DWORD;
    ullTotalPhys:             Int64;
    ullAvailPhys:             Int64;
    ullTotalPageFile:         Int64;
    ullAvailPageFile:         Int64;
    ullTotalVirtual:          Int64;
    ullAvailVirtual:          Int64;
    ullAvailExtendedVirtual:  Int64;
  end;

var
  AboutForm: TAboutForm;

{ ----------------------------------------------------------- ! IMPLEMENTATION ZONE ! ----------------------------------------------------------------------- }

implementation

uses
  Main, Settings;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------------------- 64-BIT CHECK }
function TAboutForm.Is64BitOS: Boolean;
type
  TIsWow64Process = function(Handle:THandle; var IsWow64 : BOOL) : BOOL; stdcall;
var
  hKernel32:       integer;
  IsWow64Process:  TIsWow64Process;
  IsWow64:         BOOL;
begin
  { WE CAN CHECK IF THE OPERATING SYSTEM IS 64-BIT BY CHECKING WHETHER              }
  { WE ARE RUNNING UNDER WOW64 (WE ARE 32-BIT CODE). WE MUST CHECK IF THIS          }
  { FUNCTION IS IMPLEMENTED BEFORE WE CALL IT, BECAUSE SOME OLDER VERSIONS          }
  { OF KERNEL32.DLL (eg. WINDOWS 2000) DON'T KNOW ABOUT IT.                         }
  { SEE "ISWOW64Process", http://msdn.microsoft.com/en-us/library/ms684139.aspx     }
  Result:=false;
  hKernel32:=LoadLibrary('kernel32.dll');
  if (hKernel32 = 0) then RaiseLastOSError;
  @IsWow64Process:=GetProcAddress(hkernel32, 'IsWow64Process');
  if Assigned(IsWow64Process) then begin
    IsWow64:=false;
    if (IsWow64Process(GetCurrentProcess, IsWow64)) then
    begin
      Result:=IsWow64;
    end
    else RaiseLastOSError;
  end;
  FreeLibrary(hKernel32);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- MEMOREY CHECK (ABOVE 4 GB) }
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall;
type
  TFNGlobalMemoryStatusEx = function(var msx: TMemoryStatusEx): BOOL; stdcall;
var
  FNGlobalMemoryStatusEx: TFNGlobalMemoryStatusEx;
begin
  FNGlobalMemoryStatusEx:= TFNGlobalMemoryStatusEx(GetProcAddress(GetModuleHandle(kernel32),'GlobalMemoryStatusEx'));
  if not Assigned(FNGlobalMemoryStatusEx) then
  begin
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result:=false;
  end
  else
    Result:=FNGlobalMemoryStatusEx(lpBuffer);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TAboutForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  if FileExists(AppSettings.FPathLicence) then
  begin
    try
      AboutForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_ABOUT', APPNAME);
      if AppSettings.Decode(LicData, True) then
      begin
        { WRITE OUTPUT }
        txt_VER.Caption:=GetBuildInfoAsString;
        txt_EDT.Caption:=AppSettings.TMIL.ReadString('VERSION', 'Edition', 'n/a');
        txt_LIC.Caption:=AppSettings.TMIL.ReadString('LICENCE', 'Type',    'n/a');
        txt_STA.Caption:=AppSettings.TMIL.ReadString('LICENCE', 'Status',  'n/a');
        txt_INQ.Caption:=AppSettings.TMIL.ReadString('DETAILS', 'Email1',  'n/a');
        txt_ITS.Caption:=AppSettings.TMIL.ReadString('DETAILS', 'Email2',  'n/a');
        txt_WEB.Caption:=AppSettings.TMIL.ReadString('DETAILS', 'WebAddr', 'n/a');
        txt_PRO.Caption:=AppSettings.TMIL.ReadString('DETAILS', 'Author',  'n/a');
      end;
    finally
      AppSettings.Free;
    end;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TAboutForm.FormShow(Sender: TObject);
var
  mem_32:  TMemoryStatus;
  mem_64:  TMemoryStatusEx;
begin
  { SYSTEM DATA }
  if Is64BitOs=false then
  begin
    mem_32.dwLength:=sizeof(mem_32);
    GlobalMemoryStatus(mem_32);
    txt_SYS.Caption:=GetOSVer(1) + ' (32-bit)';
    txt_MEM.Caption:=formatfloat('## ###', (mem_32.dwTotalPhys DIV 1048576)) + ' MB';
    txt_USG.Caption:=formatfloat('## ###', ((mem_32.dwTotalPhys-mem_32.dwAvailPhys) DIV 1048576)) + ' MB';
  end
    else
      begin
        mem_64.dwLength:=sizeof(mem_64);
        GlobalMemoryStatusEx(mem_64);
        txt_SYS.Caption:=GetOSVer(1) + ' (64-bit)';
        txt_MEM.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys) DIV 1048576)) + ' MB';
        txt_USG.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys-mem_64.ullAvailPhys) DIV 1048576)) + ' MB';
      end;
end;

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- CLOSE }
procedure TAboutForm.btn_CloseClick(Sender: TObject);
begin
  close;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- LINK 1 | E-MAIL }
procedure TAboutForm.txt_INQClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, nil, PChar('mailto: ' + txt_INQ.Caption), nil, nil, SW_NORMAL);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- LINK 2 | E-MAIL }
procedure TAboutForm.txt_ITSClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, nil, PChar('mailto: ' + txt_ITS.Caption), nil, nil, SW_NORMAL);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- LINK 3 | WWW }
procedure TAboutForm.txt_WEBClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(txt_WEB.Caption), nil, nil, SW_SHOWNORMAL);
end;

end.
