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
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
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
  Splash   in 'Splash.pas',
  Main     in 'Main.pas',
  Filter   in 'Filter.pas',
  Tracker  in 'Tracker.pas',
  Actions  in 'Actions.pas',
  Calendar in 'Calendar.pas',
  Invoices in 'Invoices.pas',
  About    in 'About.pas',
  Search   in 'Search.pas',
  SQL      in 'SQL.pas',
  Model    in 'Model.pas',
  Worker   in 'Worker.pas';

type
  DWord = 0..$FFFFFFFF;
  TDwmIsCompositionEnabledFunc = function(out pfEnabled: BOOL): HRESULT; stdcall;

const
  DelayStd:       integer = 50;               { SLOW DOWN A BIT TO SHOW MESSAGE  }
  DelayErr:       integer = 750;              { SLOW DOWN SIGNIFICANTLY FOR USER }
  AllTasks:       integer = 15;               { TOTAL NUMBER OF ALL BASIC TASKS  }
  DecryptKey:     integer = 429496;
  CRC32DLL1:      DWord   = 3118596872;
  CRC32DLL2:      DWord   = 3608491174;
  CRC32DLL3:      DWord   = 4084329229;
  APPCAPTION:     string  = 'Unity';
  LyncCall:       string  = 'LyncCall.exe';
  UserFolder:     string  = '\users\';
  DLL1:           string  = 'libeay32.dll';
  DLL2:           string  = 'ssleay32.dll';
  DLL3:           string  = 'vsinit.dll';
  LIC:            string  = 'Unity.lic';

  { MICROSOFT ASSEMBLIES FOR LYNC / SKYPE FOR BUSINESS }

  DLL4:           string = 'Microsoft.Lync.Controls.dll';
  DLL5:           string = 'Microsoft.Lync.Controls.Framework.dll';
  DLL6:           string = 'Microsoft.Lync.Model.dll';
  DLL7:           string = 'Microsoft.Lync.Utilities.dll';
  DLL8:           string = 'Microsoft.Office.Uc.dll';

var
  FileGEN:        string;
  FileLOG:        string;
  FileCON:        string;
  FileLIC:        string;
  AppDir:         string;
  UserDir:        string;
  UserLog:        string;
  strWrite:       string;
  Error:          integer;
  iCNT:           integer;
  FL:             TFileStream;
  IsEnabled:      BOOL;
  ModuleHandle:   HMODULE;
  IsAreoOn:       TDwmIsCompositionEnabledFunc;
  IsAeroEnabled:  boolean;
  Mutex:          integer;
  WndRect:        TRect;

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
function Unpack(ItemID: integer; DirName: string; FileName: string; mode: integer): boolean;
var
  RS:  TResourceStream;
begin
  Result:=False;
  if not (AnsiPos('\', DirName) = Length(DirName)) then DirName:=DirName + '\';
  RS:=TResourceStream.CreateFromID(hInstance, ItemID, RT_RCDATA);
  try
    RS.Position:=0;
    if mode = 1 then DeleteFile(DirName + FileName);
    try
      RS.SaveToFile(DirName + FileName);
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
  if TextMode = True then LogText(UserDir + FileCON, Text);
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  { ---------------------------------------------------------------------------------------------------------------------------- ONLY ONE COPY RUNNING PER PC }
  Mutex:=CreateMutex(nil, True, 'UnityAppication2018');
  if (Mutex = 0) OR (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    Application.MessageBox(PCHar('Unity is already opened. You can only run one copy at a time.'), PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
    Exit;
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------------- DIRECTORIES }
  AppDir :=ExtractFileDir(Application.ExeName)  + '\';
  UserDir:=ExtractFileDir(Application.ExeName) + UserFolder;
  { ----------------------------------------------------------------------------------------------------------------------------------------- USER LOGON NAME }
  UserLog:=Trim(LowerCase(GetEnvironmentVariable('username')));
  { ------------------------------------------------------------------------------------------------------------------ <LOGON>.LOG | <LOGON>.CFG | CONFIG.CFG }
  FileCON:=UserLog + '.log';    { USER LOG FILE WITH EVENTS & ERRORS, THIS IS PLAIN TEXT FILE }
  FileLOG:=UserLog + '.cfg';
  FileGEN:='config.cfg';
  FileLIC:=AppDir + LIC;
  { ---------------------------------------------------------------------------------------------------------------------------------- CHECK FOR LICENCE FILE }
  if not FileExists(FileLIC) then
  begin
    LogText(UserDir + FileCON, 'Cannot find licence file (' + LIC + '). Application terminated.');
    Application.MessageBox(PCHar('Cannot find licence file (' + LIC + '). Program will be closed. Please contact IT Support.'),
                           PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
    Exit;
  end;
  { -------------------------------------------------------------------------------------------------------------- WINDOWS VERSION CHECK - WINDOWS 7 & HIGHER }
  { MESSAGE BOX FLAGS                                                                                 }
  { ------------------------------------------------------------------------------------------------- }
  { BUTTONS:              |                        |  ICONS:                |                         }
  { ----------------------|------------------------|------------------------|------------------------ }
  {   MB_OK               | OK                     |    MB_ICONEXCLAMATION  | WARNING                 }
  {   MB_OKCANCEL         | OK, CANCEL             |    MB_ICONWARNING      | WARNING                 }
  {   MB_ABORTRETRYIGNORE | ABORT, RETRY, IGNORE   |    MB_ICONINFORMATION  | INFORMING               }
  {   MB_YESNOCANCEL      | YES, NO, CANCEL        |    MB_ICONASTERISK     | INFORMING               }
  {   MB_YESNO            | YES, NO                |    MB_ICONQUESTION     | ASKING                  }
  {   MB_RETRYCANCEL      | RETRY, CANCEL          |    MB_ICONSTOP         | ERROR                   }
  {   MB_HELP             | OK, HELP               |    MB_ICONERROR        | ERROR                   }
  {                       |                        |    MB_ICONHAND         | ERROR                   }
  { ------------------------------------------------------------------------------------------------- }
  { DEFAULTS:             |                        |  RETURNS:              |                         }
  { ----------------------|------------------------|------------------------|------------------------ }
  {   MB_DEFBUTTON1       | 1ST BUTTON             |    IDOK, IDCANCEL,     |                         }
  {   MB_DEFBUTTON2       | 2ND BUTTON             |    IDYES, IDNO         |                         }
  {   MB_DEFBUTTON3       | 3RD BUTTON             |    IDABORT, IDRETRY,   |                         }
  {   MB_DEFBUTTON4       | 4TH BUTTON             |    IDIGNORE            |                         }
  { ------------------------------------------------------------------------------------------------- }
  if not StrToInt(GetOSVer(0)) >= 61 then
  begin
    { SAVE IT INO LOG FILE }
    try
      LogText(UserDir + FileCON, 'Program must be run under Windows 7 or higher. Application terminated.');
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
      LogText(UserDir + FileCON, 'Areo is not enabled. Application terminated.');
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
    LogText(UserDir + FileCON, 'Start checking resources files and configuration files.');
    Status(1, AllTasks, DelayStd, 'Checking ' + FileCON + '... OK.', True);
    { IF WE CANNOT SAVE INTO LOG FILE, THEN SOMETHING IS NOT RIGHT }
  except
    { ---------------------------------------------------------------------------------------------------------------------- EXTRACT DEFAULT FILES IF MISSING }
    Status(1, AllTasks, DelayErr, 'Checking ' + FileCON + '... not found! Extracting new log file...', False);
    if Unpack(60, UserDir, FileCON, 0) = True then
    begin
      { PUT USER LOGON NAME TO LOG FILE (@ EOF) }
      FL:=TFileStream.Create(UserDir + FileCON, fmOpenWrite);
      try
        strWrite:=UserLog + '.' + #13#10 + #13#10;
        { GO TO EOF }
        FL.Position:=FL.Size;
        { WRITE BYTE BY BYTE }
        for iCNT:=1 to length(strWrite) do FL.Write(strWrite[iCNT], 1);
      finally
        FL.Free;
      end;
      Status(1, AllTasks, DelayStd, 'Checking ' + FileCON + '... extracted. OK.', True);
    end
    else
    begin
      Status(1, AllTasks, DelayErr, 'Cannot extract ' + FileCON + '..., unexpected error!', False);
      Application.MessageBox(PChar('Cannot create log file. ' + APPCAPTION + ' will be closed.'), PChar(APPCAPTION), MB_OK + MB_ICONWARNING);
      Exit;
    end;
  end;

  { ----------------------------------------------------------------------------------------------------------------------------- CHECK IF <LOGON>.CFG EXISTS }
  Status(2, AllTasks, DelayStd, 'Checking ' + FileLOG + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(UserDir + FileLOG) then
  begin
    Status(2, AllTasks, DelayStd, 'Checking ' + FileLOG + '... CRC32.', True);
    { CRC32 CHECK HERE }
    Decode(UserDir + FileLOG, DecryptKey, False, Error, Settings.TMIP);
    { IF ERROR, THEN EXTRACT DEFAULT }
    if Error <> 0 then
    begin
      Status(2, AllTasks, DelayErr, 'Checking ' + FileLOG + '... corrupted! Extracting default file...', True);
      if Unpack(20, UserDir, FileLOG, 1) = False then Status(2, AllTasks, DelayErr, 'Cannot extract ' + FileLOG + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(2, AllTasks, DelayErr, 'Checking ' + FileLOG + '... not found! Extracting default file...', True);
    if Unpack(20, UserDir, FileLOG, 0) = False then Status(2, AllTasks, DelayErr, 'Cannot extract ' + FileLOG + '..., unexpected error!', True);
  end;

  { ----------------------------------------------------------------------------------------------------------------------------- CHECK IF GENERAL.CFG EXISTS }
  Status(3, AllTasks, DelayStd, 'Checking ' + FileGEN + '...', True);
  { ---------------------------------------------------------------------------------------------- IF EXIST, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppDir + FileGEN) then
  begin
    Status(3, AllTasks, DelayStd, 'Checking ' + FileGEN + '... CRC32.', True);
    { CRC32 CHECK HERE }
    Decode(AppDir + FileGEN, DecryptKey, False, Error, Settings.TMIG);
    { IF ERROR, THEN EXTRACT DEFAULT }
    if Error <> 0 then
    begin
      Status(3, AllTasks, DelayErr, 'Checking ' + FileGEN + '... corrupted! Extracting default file...', True);
      if Unpack(10, AppDir, FileGEN, 1) = False then Status(3, AllTasks, DelayErr, 'Cannot extract ' + FileGEN + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(3, AllTasks, DelayErr, 'Checking ' + FileGEN + '... not found! Extracting default file...', True);
    if Unpack(10, AppDir, FileGEN, 0) = False then Status(3, AllTasks, DelayStd, 'Cannot extract ' + FileGEN + '..., unexpected error!', True);
  end;

  { ------------------------------------------------------------------------------------------------------------------ CHECK DLL FILES & DEPLOY IF NECCESSARY }
  { ---------------------------------------------------------------------------------------------------------------------------- CHECK IF LIBEAY32.DLL EXISTS }
  Status(4, AllTasks, DelayStd, 'Checking ' + DLL1 + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppDir + DLL1) then
  begin
    Status(4, AllTasks, DelayStd, 'Checking ' + DLL1 + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if CRC32DLL1 <> CRC32File(AppDir + DLL1) then
    begin
      Status(4, AllTasks, DelayErr, 'Checking ' + DLL1 + '... corrupted! Extracting default file...', True);
      if Unpack(30, AppDir, DLL1, 1) = False then Status(4, AllTasks, DelayErr, 'Cannot extract ' + DLL1 + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else begin
    Status(4, AllTasks, DelayErr, 'Checking ' + DLL1 + '... not found! Extracting default file...', True);
    if Unpack(30, AppDir, DLL1, 0) = False then Status(4, AllTasks, DelayErr, 'Cannot extract ' + DLL1 + '..., unexpected error!', True);
  end;

  { ---------------------------------------------------------------------------------------------------------------------------- CHECK IF SSLEAY32.DLL EXISTS }
  Status(5, AllTasks, DelayStd, 'Checking ' + DLL2 + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppDir + DLL2) then
  begin
    Status(5, AllTasks, DelayStd, 'Checking ' + DLL2 + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if CRC32DLL2 <> CRC32File(AppDir + DLL2) then
    begin
      Status(5, AllTasks, DelayErr, 'Checking ' + DLL2 + '... corrupted! Extracting default file...', True);
      if Unpack(40, AppDir, DLL2, 1) = False then Status(5, AllTasks, DelayErr, 'Cannot extract ' + DLL2 + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(5, AllTasks, DelayErr, 'Checking ' + DLL2 + '... not found! Extracting default file...', True);
    if Unpack(40, AppDir, DLL2, 0) = False then Status(5, AllTasks, DelayErr, 'Cannot extract ' + DLL2 + '..., unexpected error!', True);
  end;

  { ------------------------------------------------------------------------------------------------------------------------------ CHECK IF VSINIT.DLL EXISTS }
  Status(6, AllTasks, DelayStd, 'Checking ' + DLL3 + '...', True);
  { IF EXISTS, READ LAST 8 BYTES (CRC32 CHECKSUM) AND VERIFY IT }
  if FileExists(AppDir + DLL3) then
  begin
    Status(6, AllTasks, DelayStd, 'Checking ' + DLL3 + '... CRC32.', True);
    { CRC32 CHECK HERE }
    { IF ERROR, THEN EXTRACT DEFAULT }
    if CRC32DLL3 <> CRC32File(AppDir + DLL3) then
    begin
      Status(6, AllTasks, DelayErr, 'Checking ' + DLL3 + '... corrupted! Extracting default file...', True);
      if Unpack(50, AppDir, DLL3, 1) = False then Status(6, AllTasks, DelayErr, 'Cannot extract ' + DLL3 + '..., unexpected error!', True);
    end;
  end
  { ------------------------------------------------------------------------------------------------------------------------------- OTHERWISE EXTRACT DEFAULT }
  else
  begin
    Status(6, AllTasks, DelayErr, 'Checking ' + DLL3 + '... not found! Extracting default file...', True);
    if Unpack(50, AppDir, DLL3, 0) = False then Status(6, AllTasks, DelayErr, 'Cannot extract ' + DLL3 + '..., unexpected error!', True);
  end;

  { ------------------------------------------------------------------------------------------------------------------------------------ MICROSOFT ASSEMBLIES }

  { --------------------------------------------------------------------------------------------------------------------------- "Microsoft.Lync.Controls.dll" }
  Status(7, AllTasks, DelayStd, 'Checking ' + DLL4 + '...', True);
  if FileExists(AppDir + DLL4) then
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
  if FileExists(AppDir + DLL5) then
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
  if FileExists(AppDir + DLL6) then
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
  if FileExists(AppDir + DLL7) then
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
  if FileExists(AppDir + DLL8) then
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
  if FileExists(AppDir + LyncCall) then
  begin
    Status(12, AllTasks, DelayStd, 'Checking ' + LyncCall + '... OK.', True);
  end
  else
  begin
    Application.MessageBox(PCHar('Cannot find ' + LyncCall + '. Please reinstall application or contact IT support.'), PChar(APPCAPTION), MB_OK + MB_ICONERROR);
    Exit;
  end;

  LogText(UserDir + FileCON, 'End of checking resource files and configuration files.');
  LogText(UserDir + FileCON, 'Create Forms and execute their methods...');

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
  LogText(UserDir + FileCON, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

  { OTHER WINFORMS }
  Status(14, AllTasks, 400, 'Application initialization... WinForms loading, please wait.', False);
  Application.CreateForm(TAboutForm,    AboutForm);    LogText(UserDir + FileCON, '[GUI] ''AboutForm'' ......... has been created.');
  Application.CreateForm(TSearchForm,   SearchForm);   LogText(UserDir + FileCON, '[GUI] ''SearchForm'' ........ has been created.');
  Application.CreateForm(TFilterForm,   FilterForm);   LogText(UserDir + FileCON, '[GUI] ''FilterForm'' ........ has been created.');
  Application.CreateForm(TTrackerForm,  TrackerForm);  LogText(UserDir + FileCON, '[GUI] ''TrackerForm'' ....... has been created.');
  Application.CreateForm(TActionsForm,  ActionsForm);  LogText(UserDir + FileCON, '[GUI] ''ActionsForm'' ....... has been created.');
  Application.CreateForm(TCalendarForm, CalendarForm); LogText(UserDir + FileCON, '[GUI] ''CalendarForm'' ...... has been created.');
  Application.CreateForm(TInvoicesForm, InvoicesForm); LogText(UserDir + FileCON, '[GUI] ''InvoicesForm'' ...... has been created.');

  { SPLASH SCREEN - 100% }
  Status(15, AllTasks, 900, 'Application initialization... done.', False);

  { --------------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN END }
  AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
  Sleep(150);
  SplashForm.Free;

  { ----------------------------------------------------------------------------------------------------------------------- SETUP SAVED WINDOW STATE AND SHOW }
  if Settings.TMIG.ReadString(Settings.ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
  if Settings.TMIG.ReadString(Settings.ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
  if Settings.TMIG.ReadString(Settings.ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;
  MainForm.Show;

  { ----------------------------------------------------------------------------------------------------------------------------------------------------- RUN }
  LogText(UserDir + FileCON, 'Initialization is completed. Application is running.');
  Application.MainFormOnTaskbar:=True;
  Application.Run;

  (* BREAKS THE MESSAGE LOOPS IN APPLICATION.RUN CLASS *)
  Application.Terminate;
end.
