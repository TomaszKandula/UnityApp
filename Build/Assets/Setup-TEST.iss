#define MyAppName "Unity Platform Test"
#define MyAppVersion "2.0.1.2010"
#define MyAppPublisher "Tomasz Kandula/DFDS Polska Sp. z o.o."
#define MyAppURL "http://www.dfds.com/"
#define MyAppExeName "Unity.exe"
#define MyAppLuncher "Luncher.exe"

[Setup]
;NOTE: The value of AppId uniquely identifies this application.
;Do not use the same AppId value in installers for other applications.
;(To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{ABB1DBBF-271B-456F-8DD7-3EC1EC4B68C4}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={#MyAppName}
DisableDirPage=yes
DisableProgramGroupPage=yes
OutputDir=I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\Tools\_Setup
OutputBaseFilename=Setup32_Build2210_Test
SetupIconFile=I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Unity_Mutlisize_64.ico
UninstallFilesDir={commonpf}\{#MyAppName}
Compression=lzma
SolidCompression=yes
PrivilegesRequired=admin

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Icons]
Name: "{commonstartup}\{#MyAppName}"; Filename: "{commonpf}\{#MyAppName}\{#MyAppLuncher}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{commonpf}\{#MyAppName}\{#MyAppLuncher}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{commonpf}\{#MyAppName}\{#MyAppLuncher}"; Tasks: desktopicon

[Dirs]
;Public application folders (ProgramData folder)
Name: "{commonappdata}\{#MyAppName}\layouts";
Name: "{commonappdata}\{#MyAppName}\sessions";

[Files]
;BIN
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Config.bin"; DestDir: "{commonappdata}\{#MyAppName}"; DestName: "Config.bin"; Flags: ignoreversion
;INF
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Luncher.inf"; DestDir: "{commonpf}\{#MyAppName}"; DestName: "Luncher.inf"; Flags: ignoreversion
;EXE
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Luncher.exe"; DestDir: "{commonpf}\{#MyAppName}";  Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\LyncCall.exe"; DestDir: "{commonappdata}\{#MyAppName}"; Flags: ignoreversion
;DLL
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Microsoft.Lync.Controls.dll"; DestDir: "{commonappdata}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Microsoft.Lync.Controls.Framework.dll"; DestDir: "{commonappdata}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Microsoft.Lync.Model.dll"; DestDir: "{commonappdata}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Microsoft.Lync.Utilities.dll"; DestDir: "{commonappdata}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Microsoft.Office.Uc.dll"; DestDir: "{commonappdata}\{#MyAppName}"; Flags: ignoreversion
;ASSETS SUBFOLDERS
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\assets\*"; DestDir: "{commonappdata}\{#MyAppName}\assets"; Flags: ignoreversion recursesubdirs createallsubdirs

[UninstallDelete]
Type: files; Name: "{commonappdata}\{#MyAppName}\Config.cfg"
