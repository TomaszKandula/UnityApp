unit View.About;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
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
    Vcl.ComCtrls,
    Vcl.Imaging.pngimage,
    Unity.Panel;


type


    TAboutForm = class(TForm)
        txtCopyright: TLabel;
        txtLegalNote: TLabel;
        txtVersion: TLabel;
        txtEmail: TLabel;
        txtSupport: TLabel;
        txtSystem: TLabel;
        txtTotMem: TLabel;
        txtMemUse: TLabel;
        txtLicence: TLabel;
        valVersion: TLabel;
        valEmail: TLabel;
        valLicence: TLabel;
        valSupport: TLabel;
        valSystem: TLabel;
        valTotMem: TLabel;
        valMemUse: TLabel;
        txtStatus: TLabel;
        valStatus: TLabel;
        txtWebsite: TLabel;
        valWebsite: TLabel;
        txtDeveloper: TLabel;
        valDeveloper: TLabel;
        txtSubtitle: TLabel;
        PanelHeader: TPanel;
        PanelContent: TPanel;
        PanelFooter: TPanel;
        versionGroupBox: TGroupBox;
        developersGroupBox: TGroupBox;
        helpGroupBox: TGroupBox;
        infoGroupBox: TGroupBox;
        userGroupBox: TGroupBox;
        txtDisplayName: TLabel;
        txtDepartment: TLabel;
        txtUserEmail: TLabel;
        valDisplayName: TLabel;
        valUserEmail: TLabel;
        valDepartment: TLabel;
        txtUserNumber: TLabel;
        valUserNumber: TLabel;
        txtAliasName: TLabel;
        valAliasName: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure txtINQClick(Sender: TObject);
        procedure txtITSClick(Sender: TObject);
        procedure txtWEBClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    strict private
        function Is64BitOS(): Boolean;
    public
        var FSetLastSelection: TTabSheet;
    end;


    function AboutForm(): TAboutForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.SessionService,
    Unity.Helpers,
    Unity.Enums,
    Unity.Constants,
    Unity.Settings;


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


function AboutForm(): TAboutForm;
begin
    if not(Assigned(VAboutForm)) then Application.CreateForm(TAboutForm, VAboutForm);
    Result:=VAboutForm;
end;


{$REGION 'LOCAL HELPERS'}


function TAboutForm.Is64BitOS(): Boolean;
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
    if (hKernel32 = 0) then RaiseLastOSError();

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
        RaiseLastOSError();

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


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TAboutForm.FormCreate(Sender: TObject);
begin

    PanelHeader.Borders(clWhite,  $00E3B268, clWhite, clWhite, clWhite);
    PanelContent.Borders(clWhite, clWhite,   clWhite, clWhite, clWhite);
    PanelFooter.Borders(clWhite,  clWhite,   clWhite, clWhite, clWhite);

    var Settings: ISettings:=TSettings.Create();
    if FileExists(Settings.PathLicence) then
    begin

        if Settings.Decode(TAppFiles.Licence, True) then
        begin
            valVersion.Caption  :=THelpers.GetBuildInfoAsString;
            valLicence.Caption  :=Settings.GetLicenceValue('LICENCE', 'Type');
            valStatus.Caption   :=Settings.GetLicenceValue('LICENCE', 'Status');
            valEmail.Caption    :=Settings.GetLicenceValue('DETAILS', 'Email1');
            valSupport.Caption  :=Settings.GetLicenceValue('DETAILS', 'Email2');
            valWebsite.Caption  :=Settings.GetLicenceValue('DETAILS', 'WebAddr');
            valDeveloper.Caption:=Settings.GetLicenceValue('DETAILS', 'Author');
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
        valSystem.Caption:=THelpers.GetOSVer(True) + ' (32-bit)';
        valTotMem.Caption:=formatfloat('## ###', (mem_32.dwTotalPhys DIV 1048576)) + ' MB';
        valMemUse.Caption:=formatfloat('## ###', ((mem_32.dwTotalPhys-mem_32.dwAvailPhys) DIV 1048576)) + ' MB';
    end
    else
    begin
        var mem_64: TMemoryStatusEx;
        mem_64.dwLength:=sizeof(mem_64);
        GlobalMemoryStatusEx(mem_64);
        valSystem.Caption:=THelpers.GetOSVer(True) + ' (64-bit)';
        valTotMem.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys) DIV 1048576)) + ' MB';
        valMemUse.Caption:=formatfloat('## ###', ((mem_64.ullTotalPhys-mem_64.ullAvailPhys) DIV 1048576)) + ' MB';
    end;

    valDisplayName.Caption:=SessionService.SessionData.DisplayName;
    valDepartment.Caption :=SessionService.SessionData.Department;
    valUserEmail.Caption  :=SessionService.SessionData.EmailAddress;
    valUserNumber.Caption :=SessionService.SessionData.UnityUserId.ToString();
    valAliasName.Caption  :=SessionService.SessionData.AliasName;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainForm.SetActiveTabsheet(FSetLastSelection);
end;


{$ENDREGION}


{$REGION 'MOUSE EVENTS'}


procedure TAboutForm.txtINQClick(Sender: TObject);
begin
    ShellExecute(Self.Handle, nil, PChar('mailto: ' + valEmail.Caption), nil, nil, SW_NORMAL);
end;


procedure TAboutForm.txtITSClick(Sender: TObject);
begin
    ShellExecute(Self.Handle, nil, PChar('mailto: ' + valSupport.Caption), nil, nil, SW_NORMAL);
end;


procedure TAboutForm.txtWEBClick(Sender: TObject);
begin
    ShellExecute(Handle, 'open', PChar(valWebsite.Caption), nil, nil, SW_SHOWNORMAL);
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TAboutForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

