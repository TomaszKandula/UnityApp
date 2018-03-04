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
program Unity;

{$SetPEFlags $0020}  { LARGE ADDRESS AWARE }

uses
  Forms,
  Windows,
  Messages,
  Classes,
  SysUtils,
  StdCtrls,
  INIFiles,
  Coder,
  CRC32u,
  Splash in 'Splash.pas',
  Main in 'Main.pas',
  Filter in 'Filter.pas',
  Tracker in 'Tracker.pas',
  Actions in 'Actions.pas',
  Calendar in 'Calendar.pas',
  Invoices in 'Invoices.pas',
  About in 'About.pas',
  Search in 'Search.pas',
  SQL in 'SQL.pas',
  Model in 'Model.pas',
  Worker in 'Worker.pas',
  Settings in 'Settings.pas';

type
  DWord = 0..$FFFFFFFF;
  TDwmIsCompositionEnabledFunc = function(out pfEnabled: BOOL): HRESULT; stdcall;

(* NOTE: CONSTANTS ARE DEFINED IN "MAIN" VIA "COMMON.INC" *)

var
  FileCON:        string;
  FileLIC:        string;
  StrWrite:       string;
  iCNT:           integer;
  FL:             TFileStream;
  IsEnabled:      BOOL;
  ModuleHandle:   HMODULE;
  IsAreoOn:       TDwmIsCompositionEnabledFunc;
  IsAeroEnabled:  boolean;
  Mutex:          integer;
  WndRect:        TRect;
  AppSettings:    TSettings;

{$R *.res}

{ ------------------------------------------------------------ ! ADDITIONAL RESOURCES ! --------------------------------------------------------------------- }

{$R 'binres.res' 'binres.rc'}

{ LIST:                                                       }
{  10 RCDATA "08_Makefile\\general.def"    GENERAL SETTINGS   }
{  20 RCDATA "08_Makefile\\logon.def"      USER CONFIG FILE   }
{  30 RCDATA "08_Makefile\\libeay32.dll"   DLL FOR SYNAPSE    }
{  40 RCDATA "08_Makefile\\ssleay32.dll"   DLL FOR SYNAPSE    }
{  50 RCDATA "08_Makefile\\vsinit.dll"     DLL FOR SYNAPSE    }
{  60 RCDATA "08_Makefile\\logon.log"      USER LOG FILE      }

{ ------------------------------------------------------------------------------------------------------------------ EXTRACT GIVEN RESOURCE FILE BY ID NUMBER }
function Unpack(ItemID: integer; FileName: string; mode: integer): boolean;
var
  RS:  TResourceStream;
begin
  Result:=False;
  RS:=TResourceStream.CreateFromID(hInstance, ItemID, RT_RCDATA);
  try
    RS.Position:=0;
    if mode = 1 then DeleteFile(FileName);
    try
      RS.SaveToFile(FileName);
    except
      on E: Exception do
      begin
        Application.MessageBox(PCHar('Cannot extract file from resource container. Exception has been thrown: ' + E.Message), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
        Exit;
      end;
    end;
    Result:=True;
  finally
    RS.free;
  end;
end;

{ UPDATE 'STATUS' ON SPLASH SCREEN                                        }
{   NOTE1: SPLASH SCREEN MUST BE ALREADY INITIALIZED                      }
{   NOTE2: SLEEP WILL FREEZE APPLICATION & NO MESSAGES WILL BE PROCESSED  }

{ ------------------------------------------------------------------------------------------------------------------------------ SHOW STATUS ON SPLASH SCREEN }
procedure Status(Task: integer; Total: integer; Time: integer; Text: string; TextMode: boolean);
begin
  SplashForm.Status.Caption:=Text;
  SplashForm.Progress.Progress:=Trunc((Task / Total) * 100);
  SplashForm.ProgressText.Caption:=IntToStr(SplashForm.Progress.Progress) + '%';
  SplashForm.Update;
  Sleep(Time);
  if TextMode = True then LogText(AppSettings.FPathLog, Text);
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  { ---------------------------------------------------------------------------------------------------------------------------- ONLY ONE COPY RUNNING PER PC }
  Mutex:=CreateMutex(nil, True, 'UnityAppication2018');
  if (Mutex = 0) OR (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    Application.MessageBox(PCHar('Unity is already running. You can only have one instance at a time.'), PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
    Exit;
  end;

  { READ ALL SETTINGS }
  AppSettings:=TSettings.Create(APPNAME);

  { ---------------------------------------------------------------------------------------------------------------------------------- CHECK FOR LICENCE FILE }
  if not FileExists(AppSettings.FPathLicence) then
  begin
    LogText(AppSettings.FPathLog, 'Cannot find licence file (' + LicenceFile + '). Application terminated.');
    Application.MessageBox(PCHar('Cannot find licence file (' + LicenceFile + '). Program will be closed. Please contact IT Support.'),
                           PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
    Exit;
  end;
  { -------------------------------------------------------------------------------------------------------------- WINDOWS VERSION CHECK - WINDOWS 7 & HIGHER }
  if not StrToInt(GetOSVer(0)) >= 61 then
  begin
    { SAVE IT INO LOG FILE }
    try
      LogText(AppSettings.FPathLog, 'Program must be run under Windows 7 or higher. Application terminated.');
    except
      { DO NOTHING, WE SHOW MESSAGE BOX AND QUIT ANYWAY }
    end;
    Application.MessageBox(PCHar('Program must be run under Windows 7 or higher. ' + APPCAPTION + ' will be closed.'),
                           PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
    Exit;
  end;
  { --------------------------------------------------------------------------------------------------------------------------------- AREO CHECK - MUST BE ON }
  IsAeroEnabled:=False;
  ModuleHandle :=LoadLibrary('dwmapi.dll');
  if ModuleHandle <> 0 then
  begin
    try
      @IsAreoOn:=GetProcAddress(ModuleHandle, 'DwmIsCompositionEnabled');
      if Assigned(IsAreoOn) then
        if IsAreoOn(IsEnabled) = S_OK then
          IsAeroEnabled:=IsEnabled;
    finally
      FreeLibrary(ModuleHandle);
    end;
  end;
  if IsAeroEnabled = False then
  begin
    { SAVE IT INO LOG FILE }
    try
      LogText(AppSettings.FPathLog, 'Areo is not enabled. Application terminated.');
    except
      { DO NOTHING, WE SHOW MESSAGE BOX AND QUIT ANYWAY }
    end;
    Application.MessageBox(PChar('Aero is not enabled. ' + APPCAPTION + ' will be closed.'), PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
    Exit;
  end;

  { ------------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN START }
  SplashForm:=TSplashForm.Create(nil);
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
  { MAKE IT CENTRED ON THE SCREEN }
  SplashForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (SplashForm.Height div 2);
  SplashForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (SplashForm.Width  div 2);
  { FADE IN AND UPDATE }
  AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
  SplashForm.Update;

  { ---- START ---- }

  { --------------------------------------------------------------------------------------------------------------- CHECK CONFIG FILES & DEPLOY IF NECCESSARY }
  try
    LogText(AppSettings.FPathLog, 'Start checking resources files and configuration files.');
    Status(1, AllTasks, DelayStd, 'Checking ' + ExtractFileName(AppSettings.LogFile) + '... OK.', True);
    { IF WE CANNOT SAVE INTO LOG FILE, THEN SOMETHING IS NOT RIGHT }
  except
    { ---------------------------------------------------------------------------------------------------------------------- EXTRACT DEFAULT FILES IF MISSING }
    Status(1, AllTasks, DelayErr, 'Checking ' + ExtractFileName(AppSettings.FPathLog) + '... not found! Extracting new log file...', False);
    if Unpack(60, AppSettings.FPathLog, 0) = True then
    begin
      { PUT USER LOGON NAME TO LOG FILE (@ EOF) }
      FL:=TFileStream.Create(AppSettings.FPathLog, fmOpenWrite);
      try
        StrWrite:=AppSettings.WinUserName + '.' + #13#10 + #13#10;
        { GO TO EOF }
        FL.Position:=FL.Size;
        { WRITE BYTE BY BYTE }
        for iCNT:=1 to length(StrWrite) do FL.Write(StrWrite[iCNT], 1);
      finally
        FL.Free;
      end;
      Status(1, AllTasks, DelayStd, 'Checking ' + ExtractFileName(AppSettings.FPathLog) + '... extracted. OK.', True);
    end
    else
    begin
      Status(1, AllTasks, DelayErr, 'Cannot extract ' + ExtractFileName(AppSettings.FPathLog) + '..., unexpected error!', False);
      Application.MessageBox(PChar('Cannot create log file. ' + APPCAPTION + ' will be closed.'), PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
      Exit;
    end;
  end;

  { ----------------------------------------------------------------------------------------------------------------------------- CHECK IF <LOGON>.CFG EXISTS }
  Status(2, AllTasks, DelayStd, 'Checking ' + ExtractFileName(AppSettings.FPathLogon) + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }

  if FileExists(AppSettings.FPathLogon) then
  begin
    Status(2, AllTasks, DelayStd, 'Checking ' + ExtractFileName(AppSettings.FPathLogon) + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if not (AppSettings.Decode(UserConfig, False)) then
    begin
      Status(2, AllTasks, DelayErr, 'Checking ' + ExtractFileName(AppSettings.FPathLogon) + '... corrupted! Extracting default file...', True);
      if Unpack(20, AppSettings.FPathLogon, 1) = False then Status(2, AllTasks, DelayErr, 'Cannot extract ' + ExtractFileName(AppSettings.FPathLogon) + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(2, AllTasks, DelayErr, 'Checking ' + ExtractFileName(AppSettings.FPathLogon) + '... not found! Extracting default file...', True);
    if Unpack(20, AppSettings.FPathLogon, 0) = False then Status(2, AllTasks, DelayErr, 'Cannot extract ' + ExtractFileName(AppSettings.FPathLogon) + '..., unexpected error!', True);
  end;

  { ----------------------------------------------------------------------------------------------------------------------------- CHECK IF GENERAL.CFG EXISTS }
  Status(3, AllTasks, DelayStd, 'Checking ' + ConfigFile + '...', True);
  { ---------------------------------------------------------------------------------------------- IF EXIST, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppSettings.FPathConfig) then
  begin
    Status(3, AllTasks, DelayStd, 'Checking ' + ConfigFile + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if not (AppSettings.Decode(AppConfig, False)) then
    begin
      Status(3, AllTasks, DelayErr, 'Checking ' + ConfigFile + '... corrupted! Extracting default file...', True);
      if Unpack(10, AppSettings.FPathConfig, 1) = False then Status(3, AllTasks, DelayErr, 'Cannot extract ' + ConfigFile + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(3, AllTasks, DelayErr, 'Checking ' + ConfigFile + '... not found! Extracting default file...', True);
    if Unpack(10, AppSettings.FPathConfig, 0) = False then Status(3, AllTasks, DelayStd, 'Cannot extract ' + ConfigFile + '..., unexpected error!', True);
  end;

  { ------------------------------------------------------------------------------------------------------------------ CHECK DLL FILES & DEPLOY IF NECCESSARY }
  { ---------------------------------------------------------------------------------------------------------------------------- CHECK IF LIBEAY32.DLL EXISTS }
  Status(4, AllTasks, DelayStd, 'Checking ' + DLL1 + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppSettings.AppDir + DLL1) then
  begin
    Status(4, AllTasks, DelayStd, 'Checking ' + DLL1 + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if CRC32DLL1 <> CRC32File(AppSettings.AppDir + DLL1) then
    begin
      Status(4, AllTasks, DelayErr, 'Checking ' + DLL1 + '... corrupted! Extracting default file...', True);
      if Unpack(30, AppSettings.AppDir + DLL1, 1) = False then Status(4, AllTasks, DelayErr, 'Cannot extract ' + DLL1 + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else begin
    Status(4, AllTasks, DelayErr, 'Checking ' + DLL1 + '... not found! Extracting default file...', True);
    if Unpack(30, AppSettings.AppDir + DLL1, 0) = False then Status(4, AllTasks, DelayErr, 'Cannot extract ' + DLL1 + '..., unexpected error!', True);
  end;

  { ---------------------------------------------------------------------------------------------------------------------------- CHECK IF SSLEAY32.DLL EXISTS }
  Status(5, AllTasks, DelayStd, 'Checking ' + DLL2 + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppSettings.AppDir + DLL2) then
  begin
    Status(5, AllTasks, DelayStd, 'Checking ' + DLL2 + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if CRC32DLL2 <> CRC32File(AppSettings.AppDir + DLL2) then
    begin
      Status(5, AllTasks, DelayErr, 'Checking ' + DLL2 + '... corrupted! Extracting default file...', True);
      if Unpack(40, AppSettings.AppDir + DLL2, 1) = False then Status(5, AllTasks, DelayErr, 'Cannot extract ' + DLL2 + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(5, AllTasks, DelayErr, 'Checking ' + DLL2 + '... not found! Extracting default file...', True);
    if Unpack(40, AppSettings.AppDir + DLL2, 0) = False then Status(5, AllTasks, DelayErr, 'Cannot extract ' + DLL2 + '..., unexpected error!', True);
  end;

  { ------------------------------------------------------------------------------------------------------------------------------ CHECK IF VSINIT.DLL EXISTS }
  Status(6, AllTasks, DelayStd, 'Checking ' + DLL3 + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppSettings.AppDir + DLL3) then
  begin
    Status(6, AllTasks, DelayStd, 'Checking ' + DLL3 + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if CRC32DLL3 <> CRC32File(AppSettings.AppDir + DLL3) then
    begin
      Status(6, AllTasks, DelayErr, 'Checking ' + DLL3 + '... corrupted! Extracting default file...', True);
      if Unpack(50, AppSettings.AppDir + DLL3, 1) = False then Status(6, AllTasks, DelayErr, 'Cannot extract ' + DLL3 + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(6, AllTasks, DelayErr, 'Checking ' + DLL3 + '... not found! Extracting default file...', True);
    if Unpack(50, AppSettings.AppDir + DLL3, 0) = False then Status(6, AllTasks, DelayErr, 'Cannot extract ' + DLL3 + '..., unexpected error!', True);
  end;

  { ------------------------------------------------------------------------------------------------------------------------------------ MICROSOFT ASSEMBLIES }

  { --------------------------------------------------------------------------------------------------------------------------- "Microsoft.Lync.Controls.dll" }
  Status(7, AllTasks, DelayStd, 'Checking ' + DLL4 + '...', True);
  if FileExists(AppSettings.AppDir + DLL4) then
  begin
    Status(7, AllTasks, DelayStd, 'Checking ' + DLL4 + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + DLL4 + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  { ----------------------------------------------------------------------------------------------------------------- "Microsoft.Lync.Controls.Framework.dll" }
  Status(8, AllTasks, DelayStd, 'Checking ' + DLL5 + '...', True);
  if FileExists(AppSettings.AppDir + DLL5) then
  begin
    Status(8, AllTasks, DelayStd, 'Checking ' + DLL5 + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + DLL5 + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  { ------------------------------------------------------------------------------------------------------------------------------ "Microsoft.Lync.Model.dll" }
  Status(9, AllTasks, DelayStd, 'Checking ' + DLL6 + '...', True);
  if FileExists(AppSettings.AppDir + DLL6) then
  begin
    Status(9, AllTasks, DelayStd, 'Checking ' + DLL6 + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + DLL6 + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  { -------------------------------------------------------------------------------------------------------------------------- "Microsoft.Lync.Utilities.dll" }

  Status(10, AllTasks, DelayStd, 'Checking ' + DLL7 + '...', True);
  if FileExists(AppSettings.AppDir + DLL7) then
  begin
    Status(10, AllTasks, DelayStd, 'Checking ' + DLL7 + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + DLL7 + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  { ------------------------------------------------------------------------------------------------------------------------------- "Microsoft.Office.Uc.dll" }

  Status(11, AllTasks, DelayStd, 'Checking ' + DLL8 + '...', True);
  if FileExists(AppSettings.AppDir + DLL8) then
  begin
    Status(11, AllTasks, DelayStd, 'Checking ' + DLL8 + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + DLL8 + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  { ----------------------------------------------------------------------------------------------------------------------------- EXTERNAL HELPER APPLICATION }
  { ------------------------------------------------------------------------------------------------------------------------------------------ "LyncCall.exe" }

  Status(12, AllTasks, DelayStd, 'Checking ' + LyncCall + '...', True);
  if FileExists(AppSettings.AppDir + LyncCall) then
  begin
    Status(12, AllTasks, DelayStd, 'Checking ' + LyncCall + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + LyncCall + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  LogText(AppSettings.FPathLog, 'End of checking resource files and configuration files.');
  LogText(AppSettings.FPathLog, 'Create Forms and execute their methods...');

  { ---- END ---- }

  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Status(13, AllTasks, 50, 'Application initialization... connecting with SQL server..., please wait.', False);
  Application.Initialize;
  Application.Title:=APPCAPTION;
  Application.MainFormOnTaskbar:=False;

  { ---------------------------------------------------------------------------------------------------------------------------------------- CREATE ALL FORMS }

  (* NOTE: ALL FORMS MUST HAVE VISIBE PARAMETER SET TO FALSE *)

  { MAIN FORM }
  Application.CreateForm(TMainForm, MainForm);
  LogText(AppSettings.FPathLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

  { OTHER WINFORMS }
  Status(14, AllTasks, 400, 'Application initialization... WinForms loading, please wait.', False);
  Application.CreateForm(TAboutForm,    AboutForm);    LogText(AppSettings.FPathLog, '[GUI] ''AboutForm'' ......... has been created.');
  Application.CreateForm(TSearchForm,   SearchForm);   LogText(AppSettings.FPathLog, '[GUI] ''SearchForm'' ........ has been created.');
  Application.CreateForm(TFilterForm,   FilterForm);   LogText(AppSettings.FPathLog, '[GUI] ''FilterForm'' ........ has been created.');
  Application.CreateForm(TTrackerForm,  TrackerForm);  LogText(AppSettings.FPathLog, '[GUI] ''TrackerForm'' ....... has been created.');
  Application.CreateForm(TActionsForm,  ActionsForm);  LogText(AppSettings.FPathLog, '[GUI] ''ActionsForm'' ....... has been created.');
  Application.CreateForm(TCalendarForm, CalendarForm); LogText(AppSettings.FPathLog, '[GUI] ''CalendarForm'' ...... has been created.');
  Application.CreateForm(TInvoicesForm, InvoicesForm); LogText(AppSettings.FPathLog, '[GUI] ''InvoicesForm'' ...... has been created.');

  { SPLASH SCREEN - 100% }
  Status(15, AllTasks, 900, 'Application initialization... done.', False);

  { --------------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN END }
  AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
  Sleep(150);
  SplashForm.Free;

  { ----------------------------------------------------------------------------------------------------------------------- SETUP SAVED WINDOW STATE AND SHOW }
  if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
  if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
  if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;
  MainForm.Show;

  { ----------------------------------------------------------------------------------------------------------------------------------------------------- RUN }
  LogText(AppSettings.FPathLog, 'Initialization is completed. Application is running.');
  FreeAndNil(AppSettings);
  Application.MainFormOnTaskbar:=True;
  Application.Run;

  (* BREAKS THE MESSAGE LOOPS IN APPLICATION.RUN CLASS *)
  Application.Terminate;
end.
