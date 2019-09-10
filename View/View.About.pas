unit View.About;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined the same as callback
// signature. All views must use Lazy Initialization pattern.
// --------------------------------------------------------------------------------------

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
        Shape2: TShape;
        Shape3: TShape;
        Shape4: TShape;
        Text12: TLabel;
        Text13: TLabel;
        Text03: TLabel;
        Text05: TLabel;
        Text07: TLabel;
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
        Text16: TLabel;
        txt_PRO: TLabel;
        btnClose: TSpeedButton;
        Shape1: TShape;
        Shape5: TShape;
        Shape6: TShape;
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


    function AboutForm: TAboutForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Enums,
    Unity.Common,
    Unity.Settings,
    Unity.Utilities;


type
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

    TIsWow64Process = function(Handle:THandle; var IsWow64: BOOL): BOOL; stdcall;
    TFNGlobalMemoryStatusEx = function(var msx: TMemoryStatusEx): BOOL; stdcall;


var
    VAboutForm: TAboutForm;


function AboutForm: TAboutForm;
begin
    if not(Assigned(VAboutForm)) then Application.CreateForm(TAboutForm, VAboutForm);
    Result:=VAboutForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


function TAboutForm.Is64BitOS: Boolean;
begin

    /// <remarks>
    /// We can check if the operating system is 64-bit by checking whether
    /// we are running under wow64 (we are 32-bit code). We must check if this
    /// function is implemented before we call it, because some older versions
    /// of kernel32.dll (eg. Windows 2000) don't know about it.
    /// </remarks>
    /// <see cref="http://msdn.microsoft.com/en-us/library/ms684139.aspx"/>
    Result:=false;

    var hKernel32: integer:=LoadLibrary('kernel32.dll');
    if (hKernel32 = 0) then
        RaiseLastOSError;

    var IsWow64Process: TIsWow64Process;
    var IsWow64: BOOL;
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


function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall;
begin

    var FNGlobalMemoryStatusEx: TFNGlobalMemoryStatusEx:=TFNGlobalMemoryStatusEx(GetProcAddress(GetModuleHandle(kernel32),'GlobalMemoryStatusEx'));

    if not Assigned(FNGlobalMemoryStatusEx) then
    begin
        SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
        Result:=false;
    end
    else
        Result:=FNGlobalMemoryStatusEx(lpBuffer);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TAboutForm.FormCreate(Sender: TObject);
begin

    var Settings: ISettings:=TSettings.Create;
    if FileExists(Settings.PathLicence) then
    begin

        if Settings.Decode(TAppFiles.Licence, True) then
        begin
            txt_VER.Caption:=TCore.GetBuildInfoAsString;
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


procedure TAboutForm.FormShow(Sender: TObject);
begin

    if Is64BitOs=false then
    begin
        var mem_32: TMemoryStatus;
        mem_32.dwLength:=sizeof(mem_32);
        GlobalMemoryStatus(mem_32);
        txt_SYS.Caption:=TCore.GetOSVer(True) + ' (32-bit)';
        txt_MEM.Caption:=formatfloat('## ###', (mem_32.dwTotalPhys DIV 1048576)) + ' MB';
        txt_USG.Caption:=formatfloat('## ###', ((mem_32.dwTotalPhys-mem_32.dwAvailPhys) DIV 1048576)) + ' MB';
    end
    else
    begin
        var mem_64: TMemoryStatusEx;
        mem_64.dwLength:=sizeof(mem_64);
        GlobalMemoryStatusEx(mem_64);
        txt_SYS.Caption:=TCore.GetOSVer(True) + ' (64-bit)';
        txt_MEM.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys) DIV 1048576)) + ' MB';
        txt_USG.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys-mem_64.ullAvailPhys) DIV 1048576)) + ' MB';
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TAboutForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Char(VK_ESCAPE) then Close;
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

