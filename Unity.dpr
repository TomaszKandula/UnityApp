{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
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
  ShellApi,
  IOUtils,
  INIFiles,
  CRC32u,
  SynZip,
  SynZipFiles,
  Update in 'Update.pas',
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
  Settings in 'Settings.pas',
  Database in 'Database.pas',
  UAC in 'UAC.pas',
  Mailer in 'Mailer.pas',
  AgeView in 'AgeView.pas',
  Transactions in 'Transactions.pas',
  ReportBug in 'ReportBug.pas',
  Colors in 'Colors.pas',
  EventLog in 'EventLog.pas';

type
  DWord = 0..$FFFFFFFF;
  TDwmIsCompositionEnabledFunc = function(out pfEnabled: BOOL): HRESULT; stdcall;

(* NOTE: CONSTANTS ARE DEFINED IN "MAIN" VIA "COMMON.INC" *)

var
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
  UnityFiles:     TLists;
  MsAssemblies:   TStrings;

{$R *.res}

{ ------------------------------------------------------------ ! ADDITIONAL RESOURCES ! --------------------------------------------------------------------- }

{$R 'binres.res' 'binres.rc'}

{ LIST:                                                      }
{  10 RCDATA "__Makefile\\config.cfg"     GENERAL SETTINGS   }
{  30 RCDATA "__Makefile\\libeay32.dll"   DLL FOR SYNAPSE    }
{  40 RCDATA "__Makefile\\ssleay32.dll"   DLL FOR SYNAPSE    }
{  50 RCDATA "__Makefile\\vsinit.dll"     DLL FOR SYNAPSE    }
{  60 RCDATA "__Makefile\\logon.log"      EVENT LOG FILE     }

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
        Application.MessageBox(
                                PCHar('Cannot extract file from resource container. Exception has been thrown: ' + E.Message),
                                PChar(APPCAPTION), MB_OK + MB_ICONERROR
                              );
        Exit;
      end;
    end;
    Result:=True;
  finally
    RS.free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ SHOW STATUS ON SPLASH SCREEN }

(* NOTE! IT REQUIRES "SPLASHFORM" TO BE INITIALIZED *)

procedure Status(Task: integer; Total: integer; Time: integer; Text: string; TextMode: boolean);
begin
  SplashForm.Status.Caption:=Text;
  SplashForm.Progress.Progress:=Trunc((Task / Total) * 100);
  SplashForm.ProgressText.Caption:=IntToStr(SplashForm.Progress.Progress) + '%';
  SplashForm.Update;
  Sleep(Time);
  if TextMode = True then LogText(AppSettings.FPathEventLog, Text);
end;

{ ------------------------------------------------------------------------------------------------------------------------ UPDATE ALL FILES FROM RELEASE PACK }

(* NOTE! IT REQUIRES "UPDATEFORM" TO BE INITIALIZED *)

function UnzippReleaseFile(FileName: string; DestDir: string; EventLogPath: string): boolean;
var
  iCNT:    integer;
  ZipR:    TZipReader;
  FS:      TFileStream;
  Zipped:  string;
begin
  FS:=nil;
  ZipR:=TZipReader.Create(FileName);
  LogText(EventLogPath, 'New update package has been found, updating files...');
  try
    for iCNT:=0 to ZipR.Count - 1 do
    begin
      Zipped:=ZipR.Entry[iCNT].ZipName;
      try
        RenameFile(DestDir + Zipped, Zipped + '.del');
        FS:=TFileStream.Create(DestDir + Zipped, fmCreate);
        ZipR.GetData(iCNT, FS);
        UpdateForm.Progress.Progress:=Trunc(((iCNT + 1)/ZipR.Count) * 100);
        UpdateForm.Update;
        Sleep(DelayStd);
        LogText(EventLogPath, '... ' + Zipped + '. Status: ' + BoolToStr(DeleteFile(DestDir + Zipped + '.del'), False) + '.');
      finally
        FS.Free;
      end;
    end;
    Result:=True;
    LogText(EventLogPath, 'Old files with status (0) will be rmoved by new instance.');
  finally
    ZipR.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------- DELETE FILES FOLLOWING GIVEN PATTERN }
procedure DeleteFilesMatchingPattern(const directory, pattern: string; EventLogPath: string);
var
  FileName: string;
  Check:    cardinal;
begin
  Check:=0;
  for FileName in TDirectory.GetFiles(directory, pattern) do
  begin
    TFile.Delete(FileName);
    LogText(EventLogPath, 'File "' + FileName + '" has been removed.');
    Inc(Check);
  end;
  if Check > 0 then LogText(EventLogPath, 'Cleaning folder after last update has been done (' + IntToStr(Check) + ' items removed).');
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin

  { DEBUG LINE }
  ReportMemoryLeaksOnShutdown:=DebugHook <> 0;

  { ----------------------------------------------------------------------------------------------------------------------------------- ONLY ONE RUNNING COPY }
  Mutex:=CreateMutex(nil, True, CurrentMutex);
  if (Mutex = 0) OR (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    Application.MessageBox(
                            PCHar('Unity is already running. You can only have one instance at a time.'),
                            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
                          );
    Exit;
  end;

  { --------------------------------------------------------------------------------------------------------------------------------- ACCESS ALL THE SETTINGS }
  AppSettings:=TSettings.Create;

  { ------------------------------------------------------------------------------------------------------------------------------------ CHECK EVENT LOG FILE }
  if FileExists(AppSettings.FPathEventLog) then
  begin
    LogText(AppSettings.FPathEventLog, 'Starting application...');
  end
  else
  begin
    { ----------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
    if Unpack(60, AppSettings.FPathEventLog, LeaveAsIs) = True then
    begin
      { PUT USER LOGON NAME TO LOG FILE (@ EOF) }
      FL:=TFileStream.Create(AppSettings.FPathEventLog, fmOpenWrite);
      try
        StrWrite:=AppSettings.FWinUserName + '.' + CRLF + CRLF;
        FL.Position:=FL.Size;
        for iCNT:=1 to length(StrWrite) do FL.Write(StrWrite[iCNT], 1);
      finally
        FL.Free;
      end;
      LogText(AppSettings.FPathEventLog, 'Starting application...');
    end
    else
    begin
      Application.MessageBox(
                              PChar('Cannot create log file. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONWARNING
                            );
      ExitProcess(0);
    end;
  end;

  { -------------------------------------------------------------------------------------------------------------------------------------- CHECK FOR PASSOWRD }
  if AppSettings.TMIG.ReadString(Password, 'HASH', '') = '' then
  begin
    Application.MessageBox(
                            PCHar('No master password has been found. Program will be closed. Please contact IT Support.'),
                            PChar(APPCAPTION), MB_OK + MB_ICONERROR
                          );
    LogText(AppSettings.FPathEventLog, '[Critical Error]: No master password has been found. Application terminated.');
    ExitProcess(0);
  end;

  { ---------------------------------------------------------------------------------------------------------------------------------- CHECK FOR LICENCE FILE }
  if not FileExists(AppSettings.FPathLicence) then
  begin
    Application.MessageBox(
                            PCHar('Cannot find licence file (' + LicenceFile + '). Program will be closed. Please contact IT Support.'),
                            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
                          );
    LogText(AppSettings.FPathEventLog, '[Critical Error]: No licence file has been found. Application terminated.');
    ExitProcess(0);
  end;

  { -------------------------------------------------------------------------------------------------------------- WINDOWS VERSION CHECK - WINDOWS 7 & HIGHER }
  if not StrToInt(GetOSVer(0)) >= 61 then
  begin
    { SAVE IT INTO LOG FILE }
    try
      LogText(AppSettings.FPathEventLog, 'Program must be run under Windows 7 or higher. Application will be closed.');
    except
      { DO NOTHING, WE SHOW MESSAGE BOX AND QUIT ANYWAY }
    end;
    Application.MessageBox(
                            PCHar('Program must be run under Windows 7 or higher. ' + APPCAPTION + ' will be closed.'),
                            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
                          );
    LogText(AppSettings.FPathEventLog, '[Critical Error]: Invalid Operating System. Application terminated.');
    ExitProcess(0);
  end;

  { ----------------------------------------------------------------------------------------------------------- AREO CHECK MUST BE TURNED ON | WINDOWS 7 ONLY }
  if StrToInt(GetOSVer(0)) = 61 then
  begin
    { INITIALIZE }
    IsAeroEnabled:=False;
    ModuleHandle :=LoadLibrary(PChar(DWMI));
    { CHECK }
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
    { TERMINATE IF NOT SWITCHED ON }
    if IsAeroEnabled = False then
    begin
      try
        LogText(AppSettings.FPathEventLog, 'Areo composition must be enabled. Program will be closed. Please change Windows settings.');
      except
        { DO NOTHING, WE SHOW MESSAGE BOX AND QUIT ANYWAY }
      end;
      Application.MessageBox(
                              PChar('Aero is not enabled. ' + APPCAPTION + ' will be closed.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONWARNING
                            );
      LogText(AppSettings.FPathEventLog, '[Critical Error]: Areo composition is deisabled. Application terminated.');
      ExitProcess(0);
    end;
  end;

  { ------------------------------------------------------------------------------------------------------------------ PERFORM UPDATE IF NEW RELEASE IS FOUND }
  if FileExists(AppSettings.FPathRelease) then
  begin
    if AppSettings.FRelFileDateTime > AppSettings.FReleaseDateTime then
    begin
      { UPDATE SCREEN }
      UpdateForm:=TUpdateForm.Create(nil);
      SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
      UpdateForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (UpdateForm.Height div 2);
      UpdateForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (UpdateForm.Width  div 2);
      AnimateWindow(UpdateForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
      UpdateForm.Update;
      { REPLACE FILES AND SAVE DATE&TIME }
      UnzippReleaseFile(AppSettings.FPathRelease, AppSettings.FAppDir, AppSettings.FPathEventLog);
      AppSettings.FReleaseDateTime:=AppSettings.FRelFileDateTime;
      { OPEN NEW COPY AND CLOSE CURRENT INSTANCE }
      ShellExecute(Application.Handle, 'open', PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
      UpdateForm.Free;
      AppSettings.Free;
      ExitProcess(0);
    end
    else
    begin
      { CLEANING UP FROM PREVIOUS UPDATE }
      DeleteFilesMatchingPattern(AppSettings.FAppDir, '*.del', AppSettings.FPathEventLog);
    end;
  end;

  { ---- START ---- }

  SplashForm:=TSplashForm.Create(nil);
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
  SplashForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (SplashForm.Height div 2);
  SplashForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (SplashForm.Width  div 2);
  AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
  SplashForm.Update;

  { ---------------------------------------------------------------------------------------------------------------------------------------- CHECK CONFIG.CFG }
  Status(1, AllTasks, DelayStd, 'Checking ' + ConfigFile + '...', True);
  if FileExists(AppSettings.FPathAppCfg) then
  begin
    Status(1, AllTasks, DelayStd, 'Checking ' + ConfigFile + '... CRC32.', True);
    if not (AppSettings.Decode(AppConfig, False)) then
    begin
      Status(1, AllTasks, DelayErr, 'Checking ' + ConfigFile + '... corrupted! Extracting default file...', True);
      if Unpack(10, AppSettings.FPathAppCfg, DeleteOld) = False then Status(2, AllTasks, DelayErr, 'Cannot extract ' + ConfigFile + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(1, AllTasks, DelayErr, 'Checking ' + ConfigFile + '... not found! Extracting default file...', True);
    if Unpack(10, AppSettings.FPathAppCfg, LeaveAsIs) = False then Status(1, AllTasks, DelayStd, 'Cannot extract ' + ConfigFile + '..., unexpected error!', True);
  end;

  { ---------------------------------------------------------------------------------------------------------------------------------------------- OTHER FILES}

  (* CRCR3 CHECK *)

  SetLength(UnityFiles, 3, 3);
  UnityFiles[0, 0]:=DLL1;  UnityFiles[0, 1]:='30';  UnityFiles[0, 2]:=IntToStr(CRC32DLL1);
  UnityFiles[1, 0]:=DLL2;  UnityFiles[1, 1]:='40';  UnityFiles[1, 2]:=IntToStr(CRC32DLL2);
  UnityFiles[2, 0]:=DLL3;  UnityFiles[2, 1]:='50';  UnityFiles[2, 2]:=IntToStr(CRC32DLL3);

  for iCNT:=0 to High(UnityFiles) - 1 do
  begin
    Status(iCNT + 2, AllTasks, DelayStd, 'Checking ' + UnityFiles[iCNT, 0] + '...', True);
    if FileExists(AppSettings.FPathAppCfg) then
    begin
      Status(iCNT + 2, AllTasks, DelayStd, 'Checking ' + UnityFiles[iCNT, 0] + '... CRC32.', True);
      { CRC32 CHECK, EXTRACT DEFAULT FILE ON ERROR }
      if UnityFiles[iCNT, 2] <> IntToStr(CRC32File(AppSettings.FAppDir + UnityFiles[iCNT, 0])) then
      begin
        Status(iCNT + 2, AllTasks, DelayErr, 'Checking ' + UnityFiles[iCNT, 0] + '... corrupted! Extracting default file...', True);
        if Unpack(StrToInt(UnityFiles[iCNT, 1]), AppSettings.FPathAppCfg, DeleteOld) = False then Status(iCNT, AllTasks, DelayErr, 'Cannot extract ' + UnityFiles[iCNT, 0] + '..., unexpected error!', True);
      end;
    end
    else
    begin
      Status(iCNT + 2, AllTasks, DelayErr, 'Checking ' + UnityFiles[iCNT, 0] + '... not found! Extracting default file...', True);
      if Unpack(StrToInt(UnityFiles[iCNT, 1]), AppSettings.FPathAppCfg, LeaveAsIs) = False then Status(iCNT, AllTasks, DelayStd, 'Cannot extract ' + UnityFiles[iCNT, 0] + '..., unexpected error!', True);
    end;
  end;

  (* CHECK IF EXISTS *)

  SetLength(MsAssemblies, 6);
  MsAssemblies[0]:=DLL4;
  MsAssemblies[1]:=DLL5;
  MsAssemblies[2]:=DLL6;
  MsAssemblies[3]:=DLL7;
  MsAssemblies[4]:=DLL8;
  MsAssemblies[5]:=LyncCall;

  for iCNT:=0 to High(MsAssemblies) - 1 do
  begin
    Status(iCNT + 5, AllTasks, DelayStd, 'Checking ' + MsAssemblies[iCNT] + '...', True);
    if FileExists(AppSettings.FAppDir + DLL4) then
    begin
      Status(iCNT + 5, AllTasks, DelayStd, 'Checking ' + MsAssemblies[iCNT] + '... OK.', True);
    end
    else
    begin
      Application.MessageBox(
                              PCHar('Cannot find ' + MsAssemblies[iCNT] + '. Please reinstall application or contact IT support.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONERROR
                            );
      ExitProcess(0);
    end;
  end;

  LogText(AppSettings.FPathEventLog, 'End of checking resource and configuration files.');
  LogText(AppSettings.FPathEventLog, 'Create Forms and execute their methods...');

  { ---- END ---- }

  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Status(11, AllTasks, 50, 'Application initialization... connecting with SQL server..., please wait.', False);
  Application.Initialize;
  Application.Title:=APPCAPTION;
  Application.MainFormOnTaskbar:=False;

  { ---------------------------------------------------------------------------------------------------------------------------------------- CREATE ALL FORMS }

  (* NOTE: ALL FORMS MUST HAVE VISIBE PARAMETER SET TO FALSE *)

  { MAIN FORM }
  Application.CreateForm(TMainForm, MainForm);
  LogText(AppSettings.FPathEventLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

  { OTHER WINFORMS }
  Status(12, AllTasks, 400, 'Application initialization... WinForms loading, please wait.', False);
  Application.CreateForm(TAboutForm,    AboutForm);    LogText(AppSettings.FPathEventLog, '[GUI] ''AboutForm'' ......... has been created.');
  Application.CreateForm(TEventForm,    EventForm);    LogText(AppSettings.FPathEventLog, '[GUI] ''EventForm'' ......... has been created.');
  Application.CreateForm(TColorsForm,   ColorsForm);   LogText(AppSettings.FPathEventLog, '[GUI] ''ColorsForm'' ........ has been created.');
  Application.CreateForm(TReportForm,   ReportForm);   LogText(AppSettings.FPathEventLog, '[GUI] ''ReportForm'' ........ has been created.');
  Application.CreateForm(TSearchForm,   SearchForm);   LogText(AppSettings.FPathEventLog, '[GUI] ''SearchForm'' ........ has been created.');
  Application.CreateForm(TFilterForm,   FilterForm);   LogText(AppSettings.FPathEventLog, '[GUI] ''FilterForm'' ........ has been created.');
  Application.CreateForm(TTrackerForm,  TrackerForm);  LogText(AppSettings.FPathEventLog, '[GUI] ''TrackerForm'' ....... has been created.');
  Application.CreateForm(TActionsForm,  ActionsForm);  LogText(AppSettings.FPathEventLog, '[GUI] ''ActionsForm'' ....... has been created.');
  Application.CreateForm(TCalendarForm, CalendarForm); LogText(AppSettings.FPathEventLog, '[GUI] ''CalendarForm'' ...... has been created.');
  Application.CreateForm(TInvoicesForm, InvoicesForm); LogText(AppSettings.FPathEventLog, '[GUI] ''InvoicesForm'' ...... has been created.');

  { SPLASH SCREEN - 100% }
  Status(13, AllTasks, 900, 'Application initialization... done.', False);

  { --------------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN END }
  AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
  Sleep(150);
  SplashForm.Free;

  { ----------------------------------------------------------------------------------------------------------------------- SETUP SAVED WINDOW STATE AND SHOW }
  if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
  if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
  if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;
  LogText(AppSettings.FPathEventLog, 'Initialization is completed. Application is running.');
  AppSettings.Free;

  { ----------------------------------------------------------------------------------------------------------------------------------------------------- RUN }
  MainForm.Show;
  Application.MainFormOnTaskbar:=True;
  Application.Run;

  (* BREAKS THE MESSAGE LOOPS IN APPLICATION.RUN CLASS *)
  Application.Terminate;
end.
