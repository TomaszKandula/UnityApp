
{$I .\Include\Header.inc}

unit About;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellAPI,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.IniFiles,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    Vcl.Imaging.pngimage;


type


    TAboutForm = class(TForm)
        AppMain: TShape;
        Shape1: TShape;
        Shape2: TShape;
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
        btnClose: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure txtINQClick(Sender: TObject);
        procedure txtITSClick(Sender: TObject);
        procedure txtWEBClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure btnCloseClick(Sender: TObject);
    protected
        function Is64BitOS: Boolean;
    end;

    TMemoryStatusEx = packed record
        dwLength:                DWORD;
        dwMemoryLoad:            DWORD;
        ullTotalPhys:            Int64;
        ullAvailPhys:            Int64;
        ullTotalPageFile:        Int64;
        ullAvailPageFile:        Int64;
        ullTotalVirtual:         Int64;
        ullAvailVirtual:         Int64;
        ullAvailExtendedVirtual: Int64;
    end;


var
    AboutForm: TAboutForm;


implementation


uses
    Main,
    Settings;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Check if machine run on a 64-bit CPU.
/// </summary>

function TAboutForm.Is64BitOS: Boolean;
type
    TIsWow64Process = function(Handle:THandle; var IsWow64 : BOOL) : BOOL; stdcall;
var
    hKernel32:       integer;
    IsWow64Process:  TIsWow64Process;
    IsWow64:         BOOL;
begin

    /// <remarks>
    /// We can check if the operating system is 64-bit by checking whether
    /// we are running under wow64 (we are 32-bit code). We must check if this
    /// function is implemented before we call it, because some older versions
    /// of kernel32.dll (eg. Windows 2000) don't know about it.
    /// </remarks>
    /// <see cref="http://msdn.microsoft.com/en-us/library/ms684139.aspx"/>
    Result:=false;
    hKernel32:=LoadLibrary('kernel32.dll');

    if (hKernel32 = 0) then
        RaiseLastOSError;

    @IsWow64Process:=GetProcAddress(hkernel32, 'IsWow64Process');

    if Assigned(IsWow64Process) then
    begin
        IsWow64:=false;
        if (IsWow64Process(GetCurrentProcess, IsWow64)) then
        begin
            Result:=IsWow64;
        end
            else
                RaiseLastOSError;
    end;

    FreeLibrary(hKernel32);

end;


/// <summary>
/// Check if machine have a memory above 4GB.
/// </summary>

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


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


/// <summary>
/// Get data from application settings file and licence file.
/// </summary>

procedure TAboutForm.FormCreate(Sender: TObject);
var
    Settings: ISettings;
begin

    Settings:=TSettings.Create;
    if FileExists(Settings.GetPathLicenceLic) then
    begin
        if Settings.Decode(LicData, True) then
        begin
            txt_VER.Caption:=GetBuildInfoAsString;
            txt_EDT.Caption:=Settings.GetLicenceValue('VERSION', 'Edition');
            txt_LIC.Caption:=Settings.GetLicenceValue('LICENCE', 'Type');
            txt_STA.Caption:=Settings.GetLicenceValue('LICENCE', 'Status');
            txt_INQ.Caption:=Settings.GetLicenceValue('DETAILS', 'Email1');
            txt_ITS.Caption:=Settings.GetLicenceValue('DETAILS', 'Email2');
            txt_WEB.Caption:=Settings.GetLicenceValue('DETAILS', 'WebAddr');
            txt_PRO.Caption:=Settings.GetLicenceValue('DETAILS', 'Author');
        end;
    end;

end;


/// <summary>
/// Update memory information on window show.
/// </summary>

procedure TAboutForm.FormShow(Sender: TObject);
var
    mem_32:  TMemoryStatus;
    mem_64:  TMemoryStatusEx;
begin
    if Is64BitOs=false then
    begin
        mem_32.dwLength:=sizeof(mem_32);
        GlobalMemoryStatus(mem_32);
        txt_SYS.Caption:=GetOSVer(OSName) + ' (32-bit)';
        txt_MEM.Caption:=formatfloat('## ###', (mem_32.dwTotalPhys DIV 1048576)) + ' MB';
        txt_USG.Caption:=formatfloat('## ###', ((mem_32.dwTotalPhys-mem_32.dwAvailPhys) DIV 1048576)) + ' MB';
    end
    else
    begin
        mem_64.dwLength:=sizeof(mem_64);
        GlobalMemoryStatusEx(mem_64);
        txt_SYS.Caption:=GetOSVer(OSName) + ' (64-bit)';
        txt_MEM.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys) DIV 1048576)) + ' MB';
        txt_USG.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys-mem_64.ullAvailPhys) DIV 1048576)) + ' MB';
    end;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TAboutForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //


procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TAboutForm.txtINQClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, nil, PChar('mailto: ' + txt_INQ.Caption), nil, nil, SW_NORMAL);
end;


procedure TAboutForm.txtITSClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, nil, PChar('mailto: ' + txt_ITS.Caption), nil, nil, SW_NORMAL);
end;


procedure TAboutForm.txtWEBClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(txt_WEB.Caption), nil, nil, SW_SHOWNORMAL);
end;


end.

