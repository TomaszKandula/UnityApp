#define MyAppName "Unity Platform Test"
#define MyAppVersion "2.0.1.2010"
#define MyAppPublisher "Tomasz Kandula/DFDS Polska Sp. z o.o."
#define MyAppURL "http://www.dfds.com/"
#define MyAppExeName "Unity.exe"

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
OutputBaseFilename=Setup32_build2010_Test
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
Name: "{commonstartup}\{#MyAppName}"; Filename: "{commonpf}\{#MyAppName}\{#MyAppExeName}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{commonpf}\{#MyAppName}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{commonpf}\{#MyAppName}\{#MyAppExeName}"; Tasks: desktopicon

[Dirs]
;Public application folders (ProgramData folder)
Name: "{commonappdata}\{#MyAppName}\layouts";
Name: "{commonappdata}\{#MyAppName}\sessions";

[Files]

;CFG
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\$programdata\Config.bin.TEST"; DestDir: "{commonappdata}\{#MyAppName}"; DestName: "Config.bin"; Flags: ignoreversion

;INF
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Unity.inf.TEST"; DestDir: "{commonpf}\{#MyAppName}"; DestName: "Unity.inf"; Flags: ignoreversion

;EXE
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Unity.exe"; DestDir: "{commonpf}\{#MyAppName}";  Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\UnityReader.exe"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\SubProcess.exe"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\LyncCall.exe"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;LIC
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Unity.lic"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;PAK
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\cef.pak"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\cef_100_percent.pak"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\cef_200_percent.pak"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\cef_extensions.pak"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\devtools_resources.pak"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;DLL
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\chrome_elf.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\d3dcompiler_47.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\libcef.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\libEGL.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\libGLESv2.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Microsoft.Lync.Controls.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Microsoft.Lync.Controls.Framework.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Microsoft.Lync.Model.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Microsoft.Lync.Utilities.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Microsoft.Office.Uc.dll"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;DAT
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\icudtl.dat"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;BIN
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\natives_blob.bin"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\snapshot_blob.bin"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\v8_context_snapshot.bin"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;LIB
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\cef_sandbox.lib"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\libcef.lib"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;DRC
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\Unity.drc"; DestDir: "{commonpf}\{#MyAppName}"; Flags: ignoreversion

;SUBFOLDERS
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\assets\*"; DestDir: "{commonpf}\{#MyAppName}\assets"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\locales\*"; DestDir: "{commonpf}\{#MyAppName}\locales"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "I:\Projects\Software Projects\Embarcadero Delphi\Unity Project\UnityApp\Build\Win32\Bin\swiftshader\*"; DestDir: "{commonpf}\{#MyAppName}\swiftshader"; Flags: ignoreversion recursesubdirs createallsubdirs

[UninstallDelete]
Type: files; Name: "{commonappdata}\{#MyAppName}\Config.cfg"
