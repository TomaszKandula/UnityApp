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
  EventLog in 'EventLog.pas',
  Send in 'Send.pas';

type
  DWord = 0..$FFFFFFFF;
  TDwmIsCompositionEnabledFunc = function(out pfEnabled: boolean): HRESULT; stdcall;

(* NOTE: CONSTANTS ARE DEFINED IN "MAIN" VIA "COMMON.INC" *)

var
  StrWrite:         string;
  iCNT:             integer;
  FL:               TFileStream;
  IsEnabled:        Boolean;
  ModuleHandle:     HMODULE;
  IsAreoOn:         TDwmIsCompositionEnabledFunc;
  IsAeroEnabled:    boolean;
  Mutex:            integer;
  WndRect:          TRect;
  AppSettings:      TSettings;
  MsAssemblies:     TStrings;
  FileDateTime:     TDateTime;
  ReleaseDateTime:  TDateTime;
  PathRelease:      string;
  PathEventLog:     string;
  PathAppDir:       string;
  WinUserName:      string;
  RegSettings:      TFormatSettings;

{$R *.res}

{ ------------------------------------------------------------ ! ADDITIONAL RESOURCES ! --------------------------------------------------------------------- }

{$R 'binres.res' 'binres.rc'}

{ LIST:                                                          }
{  10 RCDATA "__Makefile\\config.cfg" DEFAULT GENERAL SETTINGS   }
{  60 RCDATA "__Makefile\\logon.log"  PRE-DEFINED EVENT LOG FILE }

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

procedure Status(Task: integer; Total: integer; Time: integer; Text: string; TextMode: boolean; EventLogPath: string);
begin
  SplashForm.Status.Caption:=Text;
  SplashForm.Progress.Progress:=Trunc((Task / Total) * 100);
  SplashForm.ProgressText.Caption:=IntToStr(SplashForm.Progress.Progress) + '%';
  SplashForm.Update;
  Sleep(Time);
  if TextMode = True then LogText(EventLogPath, Text);
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
      finally
        FS.Free;
      end;
    end;
    Result:=True;
    LogText(EventLogPath, 'Old files will be removed by new instance.');
  finally
    ZipR.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------- DELETE FILES FOLLOWING GIVEN PATTERN }
procedure DeleteFilesMatchingPattern(const Directory: string; const Pattern: string; EventLogPath: string);
var
  FileName: string;
  Check:    cardinal;
begin
  Check:=0;
  for FileName in TDirectory.GetFiles(Directory, Pattern) do
  begin
    TFile.Delete(FileName);
    LogText(EventLogPath, 'File "' + FileName + '" has been removed.');
    Inc(Check);
  end;
  if Check > 0 then LogText(EventLogPath, 'Cleaning folder after previous update has been done (' + IntToStr(Check) + ' items removed).');
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- DEBUG LINE }
  ReportMemoryLeaksOnShutdown:=DebugHook <> 0;
  { -------------------------------------------------------------------------------------------------------------------------------------- ALLOW ONE INSTANCE }
  Mutex:=CreateMutex(nil, True, CurrentMutex);
  if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    Application.MessageBox(
                            PCHar(APPCAPTION + ' is already running. You can only have one instance at a time.'),
                            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
                          );
    ExitProcess(0);
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------------- SETUP FORMATS }
  {$WARN SYMBOL_PLATFORM OFF}
  RegSettings:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
  {$WARN SYMBOL_PLATFORM OFF}
  RegSettings.CurrencyDecimals    :=4;
  RegSettings.DateSeparator       :='-';
  RegSettings.ShortDateFormat     :='yyyy-mm-dd';
  RegSettings.LongDateFormat      :='yyyy-mm-dd';
  RegSettings.TimeSeparator       :=':';
  RegSettings.TimeAMString        :='AM';
  RegSettings.TimePMString        :='PM';
  RegSettings.ShortTimeFormat     :='hh:mm:ss';
  RegSettings.LongTimeFormat      :='hh:mm:ss';
  FormatSettings                  :=RegSettings;
  Application.UpdateFormatSettings:=False;
  { ------------------------------------------------------------------------------------------------------- READ CURRENT CONFIG.CFG BEFORE ANY UPDATE ATTEMPT }
  AppSettings:=TSettings.Create;
  try
    { INITIALIZE }
    FileDateTime   :=NULLDATE;
    ReleaseDateTime:=NULLDATE;
    PathRelease    :='';
    PathEventLog   :='';
    PathAppDir     :='';
    WinUserName    :='';
    { EXTRACT DEFAULT CONFIG.CFG IF MISSING }
    if AppSettings.GetLastError = 404 then
    begin
      if Unpack(10, AppSettings.FPathAppCfg, DeleteOld) then
        AppSettings.ConfigToMemory
          else
          begin
            Application.MessageBox(
                                    PCHar('Cannot extract missing configuration file. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                                  );
            AppSettings.Free;
            ExitProcess(0);
          end;
    end;
    { PROCEED OTHERWISE }
    if AppSettings.GetLastError = 0 then
    begin
      FileDateTime   :=AppSettings.FRelFileDateTime;
      ReleaseDateTime:=AppSettings.FReleaseDateTime;
      PathRelease    :=AppSettings.FPathRelease;
      PathEventLog   :=AppSettings.FPathEventLog;
      PathAppDir     :=AppSettings.FAppDir;
      WinUserName    :=AppSettings.FWinUserName;
    end;
  finally
    AppSettings.Free;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------ CHECK EVENT LOG FILE }
  if FileExists(PathEventLog) then
  begin
    LogText(PathEventLog, 'Starting application...');
  end
  else
  begin
    { ----------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
    if Unpack(60, PathEventLog, LeaveAsIs) then
    begin
      { PUT USER LOGON NAME TO LOG FILE (@ EOF) }
      FL:=TFileStream.Create(PathEventLog, fmOpenWrite);
      try
        StrWrite:=WinUserName + '.' + CRLF + CRLF;
        FL.Position:=FL.Size;
        for iCNT:=1 to length(StrWrite) do FL.Write(StrWrite[iCNT], 1);
      finally
        FL.Free;
      end;
      LogText(PathEventLog, 'Starting application...');
    end
    else
    begin
      Application.MessageBox(
                              PChar('Cannot create log file. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONERROR
                            );
      ExitProcess(0);
    end;
  end;

  { ---------------------------------------------------------------------------------------------------- FORCE CLEAN UP REMAINING FILES AFTER PREVIOUS UPDATE }
  DeleteFilesMatchingPattern(PathAppDir, '*.del', PathEventLog);

  { ------------------------------------------------------------------------------------------------------------------ PERFORM UPDATE IF NEW RELEASE IS FOUND }
  if FileExists(PathRelease) then
  begin
    if FileDateTime > ReleaseDateTime then
    begin
      { UPDATE SCREEN }
      UpdateForm:=TUpdateForm.Create(nil);
      SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
      UpdateForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (UpdateForm.Height div 2);
      UpdateForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (UpdateForm.Width  div 2);
      AnimateWindow(UpdateForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
      UpdateForm.Update;
      { UNZIP NEW FILES | NOTE! CONFIG.CFG MAY BE ALSO UPDATED }
      UnzippReleaseFile(PathRelease, PathAppDir, PathEventLog);
      { UPDATE DATE AND TIME OF NEW RELEASE }
      AppSettings:=TSettings.Create;
      try
        try
          AppSettings.FReleaseDateTime:=FileDateTime;
        except
          on E: Exception do
          begin
            Application.MessageBox(
                                    PChar('Cannot finalize automatic update. ' + APPCAPTION + ' will be closed. Please contact IT support. Error has been thrown: ' + E.Message),
                                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                                  );
            LogText(AppSettings.FPathEventLog, '[Critical Error]: Cannot finalize automatic update. Error has been thrown: ' + E.Message);
            ExitProcess(0);
          end;
        end;
      finally
        AppSettings.Free;
      end;
      { OPEN NEW COPY AND CLOSE CURRENT INSTANCE }
      ShellExecute(Application.Handle, 'open', PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
      UpdateForm.Free;
      ExitProcess(0);
    end;
  end;

  { ---- START  ---- }

  SplashForm:=TSplashForm.Create(nil);
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
  SplashForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (SplashForm.Height div 2);
  SplashForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (SplashForm.Width  div 2);
  AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
  SplashForm.Update;

  { ---------------------------------------------------------------------------------------------------------- RE-OPEN SETTINGS FILE AND PERFORM OTHER CHECKS }
  AppSettings:=TSettings.Create;
  try
    { ----------------------------------------------------------------------------------------------------------------------------- CHECK FOR MASTER PASSOWRD }
    if AppSettings.TMIG.ReadString(Password, 'HASH', '') = '' then
    begin
      Status(1, AllTasks, DelayStd, 'Checking master password... failed!', True, AppSettings.FPathEventLog);
      Application.MessageBox(
                              PCHar('No master password has been found. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONERROR
                            );
      LogText(AppSettings.FPathEventLog, '[Critical Error]: No master password has been found. Application has been terminated.');
      AppSettings.Free;
      ExitProcess(0);
    end
    else
    begin
      Status(1, AllTasks, DelayStd, 'Checking master password... OK.', True, AppSettings.FPathEventLog);
    end;
    { -------------------------------------------------------------------------------------------------------------------------------- CHECK FOR LICENCE FILE }
    if not FileExists(AppSettings.FPathLicence) then
    begin
      Status(2, AllTasks, DelayStd, 'Checking licence file... failed!', True, AppSettings.FPathEventLog);

      (* CHECK HERE ".LICX" FILE IN CASE OF UNITY IS SHAREWARE/LIMITED COMMERCIAL APPLICATION *)

      Application.MessageBox(
                              PCHar('Cannot find licence file (' + LicenceFile + '). ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONERROR
                            );
      LogText(AppSettings.FPathEventLog, '[Critical Error]: No licence file has been found. Application has been terminated.');
      AppSettings.Free;
      ExitProcess(0);
    end
    else
    begin
      Status(2, AllTasks, DelayStd, 'Checking licence file... OK.', True, AppSettings.FPathEventLog);
    end;
    { ------------------------------------------------------------------------------------------------------------ WINDOWS VERSION CHECK - WINDOWS 7 & HIGHER }
    if not StrToInt(GetOSVer(0)) >= 61 then
    begin
      Status(3, AllTasks, DelayStd, 'Checking operating system version... failed!', True, AppSettings.FPathEventLog);
      Application.MessageBox(
                              PCHar(APPCAPTION + ' must be run under Windows 7 or higher. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                              PChar(APPCAPTION), MB_OK + MB_ICONERROR
                            );
      LogText(AppSettings.FPathEventLog, '[Critical Error]: Invalid Operating System. Application has been terminated.');
      AppSettings.Free;
      ExitProcess(0);
    end
    else
    begin
      Status(3, AllTasks, DelayStd, 'Checking operating system version... OK.', True, AppSettings.FPathEventLog);
    end;
    { --------------------------------------------------------------------------------------------------------- AREO CHECK MUST BE TURNED ON | WINDOWS 7 ONLY }
    if StrToInt(GetOSVer(0)) = 61 then
    begin
      { INITIALIZE }
      Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... ', True, AppSettings.FPathEventLog);
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
        Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... disabled!', True, AppSettings.FPathEventLog);
        Application.MessageBox(
                                PChar('Aero is not enabled. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                                PChar(APPCAPTION), MB_OK + MB_ICONERROR
                              );
        LogText(AppSettings.FPathEventLog, '[Critical Error]: Areo composition is deisabled. Application has been terminated.');
        AppSettings.Free;
        ExitProcess(0);
      end;
    end
    else
    begin
      Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... OK.', True, AppSettings.FPathEventLog);
    end;
    { ------------------------------------------------------------------------------------------------------------------------------ CHECK CONFIG.CFG | CRC32 }
    Status(5, AllTasks, DelayStd, 'CRC32 check: ' + ConfigFile + '...', True, AppSettings.FPathEventLog);
    if not (AppSettings.Decode(AppConfig, False)) then
    begin
      Status(5, AllTasks, DelayErr, 'CRC32 check: ' + ConfigFile + '... corrupted! Extracting default file...', True, AppSettings.FPathEventLog);
      try
        Unpack(10, AppSettings.FPathAppCfg, DeleteOld);
      except
        on E: Exception do
        begin
          Application.MessageBox(
                                  PChar('Cannot extract config.cfg. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                                  PChar(APPCAPTION), MB_OK + MB_ICONERROR
                                );
          Status(5, AllTasks, DelayErr, 'CRC32 check: ' + ConfigFile + '..., unexpected error!', True, AppSettings.FPathEventLog);
          LogText(AppSettings.FPathEventLog, '[Critical Error]: Cannot extract "config.cfg" from resources. Error has been thrown: ' + E.Message);
          AppSettings.Free;
          ExitProcess(0);
        end;
      end;
    end
    else
    begin
      Status(5, AllTasks, DelayStd, 'CRC32 check: ' + ConfigFile + '...OK.', True, AppSettings.FPathEventLog);
    end;
    { ----------------------------------------------------------------------------------------------------------- CHECK ASSEMBLIES AND LYNCCALL.EXE IF EXISTS }
    SetLength(MsAssemblies, 6);
    MsAssemblies[0]:=DLL1;
    MsAssemblies[1]:=DLL2;
    MsAssemblies[2]:=DLL3;
    MsAssemblies[3]:=DLL4;
    MsAssemblies[4]:=DLL5;
    MsAssemblies[5]:=LyncCall;
    for iCNT:=0 to High(MsAssemblies) - 1 do
    begin
      Status(iCNT + 6, AllTasks, DelayStd, 'Checking ' + MsAssemblies[iCNT] + '...', True, AppSettings.FPathEventLog);
      if FileExists(AppSettings.FAppDir + MsAssemblies[iCNT]) then
      begin
        Status(iCNT + 6, AllTasks, DelayStd, 'Checking ' + MsAssemblies[iCNT] + '... OK.', True, AppSettings.FPathEventLog);
      end
      else
      begin
        Application.MessageBox(
                                PCHar('Cannot find ' + MsAssemblies[iCNT] + '. Please reinstall application and/or contact IT support.'),
                                PChar(APPCAPTION), MB_OK + MB_ICONERROR
                              );
        AppSettings.Free;
        ExitProcess(0);
      end;
    end;
    LogText(AppSettings.FPathEventLog, 'End of checking resource and configuration files.');
  finally
    AppSettings.Free;
  end;

  { ---- END ---- }

  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  AppSettings:=TSettings.Create;
  try
    Status(12, AllTasks, 50, 'Application initialization... connecting with SQL server..., please wait.', False, AppSettings.FPathEventLog);
    Application.Initialize;
    Application.Title:=APPCAPTION;
    Application.MainFormOnTaskbar:=False;

    { -------------------------------------------------------------------------------------------------------------------------------------- CREATE ALL FORMS }

    (* NOTE: ALL FORMS MUST HAVE VISIBE PARAMETER SET TO FALSE *)

    { MAIN FORM }
    Application.CreateForm(TMainForm, MainForm);
    LogText(AppSettings.FPathEventLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

    { OTHER WINFORMS }
    Status(13, AllTasks, 400, 'Application initialization... VCL forms loading, please wait.', False, AppSettings.FPathEventLog);
    Application.CreateForm(TSendForm,     SendForm);     LogText(AppSettings.FPathEventLog, '[GUI] ''SendForm'' .......... has been created.');
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
    Status(14, AllTasks, 900, 'Application initialization... done.', False, AppSettings.FPathEventLog);

    { ------------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN END }
    AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
    Sleep(150);
    SplashForm.Free;

    { --------------------------------------------------------------------------------------------------------------------- SETUP SAVED WINDOW STATE AND SHOW }
    if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
    if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
    if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;
    LogText(AppSettings.FPathEventLog, 'Initialization is completed. Application is running.');
  finally
    AppSettings.Free;
  end;

  { ----------------------------------------------------------------------------------------------------------------------------------------------------- RUN }
  MainForm.Show;
  Application.MainFormOnTaskbar:=True;
  Application.Run;

  (* BREAKS THE MESSAGE LOOPS IN APPLICATION.RUN CLASS *)
  Application.Terminate;
end.
