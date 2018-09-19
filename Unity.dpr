
{$I .\Include\Header.inc}

program Unity;

/// <remarks>
///     Large address aware - allow 32-bit program to use more than 2GB (up to 4GB). If still not enough, then compile as a 64-bit.
/// </remarks>>

{$SetPEFlags $0020}

uses
    Forms,
    Windows,
    Messages,
    Classes,
    SysUtils,
    StrUtils,
    StdCtrls,
    ShellApi,
    IOUtils,
    INIFiles,
    CRC32u,
    uCEFApplication,
    System.Types,
    System.Zip,
    Model in 'Model\Model.pas',
    SQL in 'Model\SQL.pas',
    Arrays in 'Extensions\Arrays.pas',
    InterposerClasses in 'Extensions\InterposerClasses.pas',
    AgeView in 'Logic\AgeView.pas',
    Database in 'Logic\Database.pas',
    Mailer in 'Logic\Mailer.pas',
    Settings in 'Logic\Settings.pas',
    Transactions in 'Logic\Transactions.pas',
    UAC in 'Logic\UAC.pas',
    Worker in 'Logic\Worker.pas',
    Internet in 'Logic\Internet.pas',
    ThreadUtilities in 'Logic\ThreadUtilities.pas',
    EventLogger in 'Logic\EventLogger.pas',
    About in 'View\About.pas' {AboutForm},
    Actions in 'View\Actions.pas' {ActionsForm},
    Calendar in 'View\Calendar.pas' {CalendarForm},
    Colors in 'View\Colors.pas' {ColorsForm},
    EventLog in 'View\EventLog.pas' {EventForm},
    Filter in 'View\Filter.pas' {FilterForm},
    Invoices in 'View\Invoices.pas' {InvoicesForm},
    Main in 'View\Main.pas' {MainForm},
    PhoneList in 'View\PhoneList.pas' {PhoneListForm},
    SendFeedback in 'View\SendFeedback.pas' {ReportForm},
    AVSearch in 'View\AVSearch.pas' {SearchForm},
    Send in 'View\Send.pas' {SendForm},
    Splash in 'View\Splash.pas' {SplashForm},
    Tracker in 'View\Tracker.pas' {TrackerForm},
    Update in 'View\Update.pas' {UpdateForm},
    MassMailer in 'View\MassMailer.pas' {ViewMailerForm},
    ABSearch in 'View\ABSearch.pas' {ViewSearchForm},
    Await in 'View\Await.pas' {AwaitForm};

type
    DWord = 0..$FFFFFFFF;
    TDwmIsCompositionEnabledFunc = function(out pfEnabled: boolean): HRESULT; stdcall;

    /// <remarks>
    ///     Application constants are defined in main view throught "common.inc" file.
    /// </remarks>>

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
    Settings:         ISettings;
    LogText:          TThreadFileLog;
    Connection:       IConnectivity;
    Assemblies:       TStrings;
    ChromiumExit:     boolean;
    ReleaseNumber:    cardinal;
    PathReleasePak:   string;
    PathReleaseMan:   string;
    PathEventLog:     string;
    PathAppDir:       string;
    PackageDir:       string;
    WinUserName:      string;
    Manifest:         string;
    RegSettings:      TFormatSettings;

{$R *.res}
{$R 'binres.res' 'binres.rc'}


// ------------------------------------------------------------------------------------------------------------------------------------------ HELPER METHODS //

/// <summary>
///     Allocate assemblies names that we are looing for.
///     TODO: Subject for CRC32.
/// </summary>

procedure InitAssembliesRef;
begin
    SetLength(Assemblies, 6);
    Assemblies[0]:=DLL1;
    Assemblies[1]:=DLL2;
    Assemblies[2]:=DLL3;
    Assemblies[3]:=DLL4;
    Assemblies[4]:=DLL5;
    Assemblies[5]:=LyncCall;
end;

/// <summary>
///     Extract given source file by provided ID number.
/// </summary>
/// <param name="ItemID">Integer.</param>
/// <param name="FileName">String.</param>
/// <param name="mode">Integer.</param>
/// <returns>Boolean. Set to true if succeed.</returns>
/// <remarks>
///     10 RCDATA "Makefile\\config.cfg" default setting file.
///     60 RCDATA "Makefile\\logon.log"  pre-defined event log file.
/// </remarks>

function Unpack(ItemID: integer; FileName: string; mode: integer): boolean;
var
    RS:  TResourceStream;
begin
    Result:=False;
    RS:=TResourceStream.CreateFromID(hInstance, ItemID, RT_RCDATA);

    try
        RS.Position:=0;
        if mode = 1 then
            DeleteFile(FileName);

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

/// <summary>
///     Update splash form message and loading status.
/// </summary>
/// <remarks>
///     It requires "Splash Form" to be already initialized.
/// </remarks>

procedure Status(Task: integer; Total: integer; Time: integer; Text: string; TextMode: boolean; EventLogPath: string);
begin

    SplashForm.TextStatus.Caption  :=Text;
    SplashForm.ProgressBar.Progress:=Trunc((Task / Total) * 100);
    SplashForm.TextProgress.Caption:=IntToStr(SplashForm.ProgressBar.Progress) + '%';

    SplashForm.Update;
    Sleep(Time);

    if TextMode = True then
        LogText.Log(EventLogPath, Text);

end;

/// <summary>
///     Unpack "Release.pak" file.
/// </summary>
/// <remarks>
///     It requires "Update Form" to be already initialized.
/// </remarks>

function UnzippReleaseFile(FileName: string; DestDir: string; EventLogPath: string): boolean;
var
    iCNT:      integer;
    ZipRead:   TZipFile;
    Zipped:    string;
    FullPath:  string;
begin

    Result:=False;

    ZipRead:=TZipFile.Create;
    try
        try
            ZipRead.Open(FileName, zmRead);
            LogText.Log(EventLogPath, '[Automatic updater]: New update package has been found, updating files...');

            for iCNT:=0 to ZipRead.FileCount - 1 do
            begin
                Zipped:=ZipRead.FileName[iCNT];
                FullPath:=DestDir + Zipped;

                // Rename old files
                if not(string.IsNullOrEmpty(ExtractFileName(FullPath))) then
                    RenameFile(FullPath, Zipped + '.del');

                // Extract and create any folder if missing
                ZipRead.Extract(iCNT, DestDir, True);

                // Update screen
                UpdateForm.Progress.Progress:=Trunc( ( (iCNT + 1) / ZipRead.FileCount ) * 100 );
                UpdateForm.txtProgress.Caption:='Extracting: ' + Zipped + '.';
                UpdateForm.Update;

            end;

            Result:=True;
            LogText.Log(EventLogPath, '[Automatic updater]: Old files will be removed by new instance.');

        except
            on E: Exception do
                LogText.Log(EventLogPath, '[Automatic updater]: Unexpected error has been thrown: ' + E.Message);
        end;

    finally
        ZipRead.Free;
        DeleteFile(FileName);
    end;

end;

/// <summary>
///     During the update, some files cannot be overwritten or removed, thus we change the name and copy new file(s) into the very same place.
///     This methods remove all of the "leftovers" from given folder.
/// </summary>
/// <param name="Directory">String, source.</param>
/// <param name="Pattern">String, indicate what to remove, eg. *.png.</param>
/// <param name="EventLogPath">String, points to event log.</param>

procedure DeleteFilesMatchingPattern(const Directory: string; const Pattern: string; EventLogPath: string);
var
    FileName: string;
    Check:    cardinal;
begin
    Check:=0;

    for FileName in TDirectory.GetFiles(Directory, Pattern) do
    begin
        TFile.Delete(FileName);
        LogText.Log(EventLogPath, '[Automatic updater]: File "' + FileName + '" has been removed.');
        Inc(Check);
    end;

    if Check > 0 then
        LogText.Log(EventLogPath, '[Automatic updater]: Cleaning folder after previous update has been done (' + IntToStr(Check) + ' items removed).');

end;

/// <summary>
///     Parse content of manifest file and return requested Key value.
/// </summary>

function GetManifestValue(Key: string; Source: string): string;

    // Nested method

    function GetValue(Key: string): string;
    var
        iCNT:     integer;
        StartPos: integer;
    begin

        StartPos:=AnsiPos(Key, Source);

        for iCNT:=StartPos to Length(Source) do
            if Source[iCNT] = CR then
                Break;

        Result:=MidStr(Source, StartPos, (iCNT - StartPos)).Replace(Key, '').ToLower;

    end;

    // Main block

begin
    Result:=GetValue('$' + Key + '=');
end;


// -------------------------------------------------------------------------------------------------------------------------------------- MAIN PROGRAM BLOCK //


begin

    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown:=DebugHook <> 0;
    {$WARN SYMBOL_PLATFORM ON}

    /// <summary>
    ///     We allow only one instance of running program (no sessions).
    /// </summary>

    Mutex:=CreateMutex(nil, True, CurrentMutex);
    if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
        Application.MessageBox(
            PCHar(APPCAPTION + ' is already running. You can only have one instance at a time.'),
            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
        );
        ExitProcess(0);
    end;

    /// <summary>
    ///     Setup formats to user local settings.
    /// </summary>

    {$WARN SYMBOL_PLATFORM OFF}
    RegSettings:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}

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

    /// <summary>
    ///     Initialize interfaced objects for internet methods and settings file operations and event logging.
    /// </summary>
    /// <remarks>
    ///     Because LogText is re-introduced in MainForm, it has to be free before MainForm initialization.
    /// </remarks>

    Connection:=TConnectivity.Create;
    Settings  :=TSettings.Create;
    LogText   :=TThreadFileLog.Create;

    /// <summary>
    ///     Check internet connection.
    /// </summary>

    if not(Connection.IsInternetPresent) then
    begin
        Application.MessageBox(
            PCHar(APPCAPTION + ' cannot work off-line. Please check Internet connection or contact your network administrator. Program will be closed.'),
            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
        );
        ExitProcess(0);
    end;

    /// <summary>
    ///     Get all necessary settings from configuration file before any possible update.
    /// </summary>

    ReleaseNumber:=0;

    // Extract default config.cfg if it is missing
    if Settings.GetLastError = 404 then
    begin
        if Unpack(10, Settings.GetPathAppCfg, DeleteOld) then
            Settings.ConfigToMemory
        else
        begin
            Application.MessageBox(
                PCHar('Cannot extract missing configuration file. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                PChar(APPCAPTION), MB_OK + MB_ICONERROR
            );
            ExitProcess(0);
        end;
    end;

    // Proceed otherwise
    if Settings.GetLastError = 0 then
    begin
        ReleaseNumber  :=Settings.ReleaseNumber;
        PathReleasePak :=Settings.GetReleasePakURL;
        PathReleaseMan :=Settings.GetReleaseManURL;
        PathEventLog   :=Settings.GetPathEventLog;
        PathAppDir     :=Settings.GetAppDir;
        PackageDir     :=Settings.GetPackageDir;
        WinUserName    :=Settings.GetWinUserName;
    end;

    /// <summary>
    ///     Check event log file. If it is missing, then unpack from EXE resources default file.
    /// </summary>

    if FileExists(PathEventLog) then
    begin
        LogText.Log(PathEventLog, 'Starting application...');
    end
    else
    begin
        // Otherwise extract default
        if Unpack(60, PathEventLog, LeaveAsIs) then
        begin
            // Put user logon name to log file (@ eof)
            FL:=TFileStream.Create(PathEventLog, fmOpenWrite);
            try
                StrWrite:=WinUserName + '.' + CRLF + CRLF;
                FL.Position:=FL.Size;

                for iCNT:=1 to length(StrWrite) do
                    FL.Write(StrWrite[iCNT], 1);

            finally
                FL.Free;
            end;
            LogText.Log(PathEventLog, 'Starting application...');
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

    // Force clean up remaining files after previous update
    DeleteFilesMatchingPattern(PathAppDir, '*.del', PathEventLog);
    DeleteFilesMatchingPattern(PathAppDir + 'locales\', '*.del', PathEventLog);
    DeleteFilesMatchingPattern(PathAppDir + 'swiftshader\', '*.del', PathEventLog);

    // Check directories
    if not(DirectoryExists(Settings.GetLayoutDir)) then
        CreateDir(Settings.GetLayoutDir);

    if not(DirectoryExists(Settings.GetPackageDir)) then
        CreateDir(Settings.GetPackageDir);

    /// <summary>
    ///     Check manifest and update if application release number is lower.
    /// </summary>

    Manifest:=Connection.GetResponseText(Settings.GetReleaseManURL);
    if ( GetManifestValue('Status', Manifest) = 'update' ) and ( GetManifestValue('Release', Manifest) > IntToStr(ReleaseNumber) ) then
    begin

        // Update screen
        UpdateForm:=TUpdateForm.Create(nil);
        SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
        UpdateForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (UpdateForm.Height div 2);
        UpdateForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (UpdateForm.Width  div 2);
        AnimateWindow(UpdateForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
        UpdateForm.Update;

        // Update message for the user
        UpdateForm.txtProgress.Caption:='Downloading...';
        UpdateForm.Update;

        // Get package from website
        if Connection.Download(PathReleasePak, PackageDir + ReleaseFile) then
        begin
            Settings:=nil;
            // Unzip the content, update settings file and execute new release
            if UnzippReleaseFile(PackageDir + ReleaseFile, PathAppDir, PathEventLog) then
            begin
                Settings:=TSettings.Create;
                Settings.ReleaseDateTime:=Now;
                Settings.ReleaseNumber:=StrToInt(GetManifestValue('Release', Manifest));
                ShellExecute(Application.Handle, seOpen, PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
            end
            else
            begin
                Application.MessageBox(
                    PChar('Cannot unpack files. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                );
            end;
        end
        else
        begin
            Application.MessageBox(
                PChar('Cannot download new release package. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                PChar(APPCAPTION), MB_OK + MB_ICONERROR
            );
        end;

        // Exit
        UpdateForm.Free;
        ExitProcess(0);

    end;

    /// <remarks>
    ///     Splash screen requires updating to display the changed content.
    /// </remarks>

    SplashForm:=TSplashForm.Create(nil);
    SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
    SplashForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (SplashForm.Height div 2);
    SplashForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (SplashForm.Width  div 2);
    AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
    SplashForm.Update;

    /// <summary>
    ///     Check password. It is hashed with BCrypt and cannot be shorter than 60 characters.
    /// </summary>

    if Length(Settings.GetStringValue(PasswordSection, 'HASH', '')) < 60 then
    begin
        Status(1, AllTasks, DelayStd, 'Checking master password... failed!', True, Settings.GetPathEventLog);
        Application.MessageBox(
            PCHar('Invalid master password has been found. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
            PChar(APPCAPTION), MB_OK + MB_ICONERROR
        );
        LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Invaid master password has been found. Application has been terminated.');
        ExitProcess(0);
    end
    else
    begin
        Status(1, AllTasks, DelayStd, 'Checking master password... OK.', True, Settings.GetPathEventLog);
    end;

    /// <summary>
    ///     Check if licence file exists.
    /// </summary>
    /// <remarks>
    ///     TODO: Extend this by adding CRC32 check.
    /// </remarks>

    if not FileExists(Settings.GetPathLicenceLic) then
    begin
        Status(2, AllTasks, DelayStd, 'Checking licence file... failed!', True, Settings.GetPathEventLog);

        /// <remarks>
        ///     Check here ".LIC" file in case of Unity is shareware/limited commercial application.
        /// </remarks>

        Application.MessageBox(
            PCHar('Cannot find licence file (' + LicenceFile + '). ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
            PChar(APPCAPTION), MB_OK + MB_ICONERROR
        );
        LogText.Log(Settings.GetPathEventLog, '[Critical Error]: No licence file has been found. Application has been terminated.');
        ExitProcess(0);
    end
    else
    begin
        Status(2, AllTasks, DelayStd, 'Checking licence file... OK.', True, Settings.GetPathEventLog);
    end;

    /// <summary>
    ///     Check Windows version. We allow only Windows 7 (with Aero) or Windows 10 (and above).
    /// </summary>

    if not StrToInt(GetOSVer(OSNumber)) >= 61 then
    begin
        Status(3, AllTasks, DelayStd, 'Checking operating system version... failed!', True, Settings.GetPathEventLog);
        Application.MessageBox(
            PCHar(APPCAPTION + ' must be run under Windows 7 or higher. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
            PChar(APPCAPTION), MB_OK + MB_ICONERROR
        );
        LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Invalid Operating System. Application has been terminated.');
        ExitProcess(0);
    end
    else
    begin
        Status(3, AllTasks, DelayStd, 'Checking operating system version... OK.', True, Settings.GetPathEventLog);
    end;

    /// <summary>
    ///     Areo must be enabled if program runs under Windows 7.
    /// </summary>

    if StrToInt(GetOSVer(OSNumber)) = 61 then
    begin

        // Initialize
        Status(4, AllTasks, DelayStd, 'Checking Windows 7 Aero composition... ', True, Settings.GetPathEventLog);
        IsAeroEnabled:=False;
        ModuleHandle :=LoadLibrary(PChar(DWMI));

        // Check
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

        // Terminate if not switched on
        if IsAeroEnabled = False then
        begin
            Status(4, AllTasks, DelayStd, 'Checking Windows 7 Aero composition... disabled!', True, Settings.GetPathEventLog);
            Application.MessageBox(
                PChar('Aero is not enabled. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                PChar(APPCAPTION), MB_OK + MB_ICONERROR
            );
            LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Aero composition is disabled. Application has been terminated.');
            ExitProcess(0);
        end;
    end
    else
    begin
        Status(4, AllTasks, DelayStd, 'Checking Windows 7 Aero composition... OK.', True, Settings.GetPathEventLog);
    end;

   /// <summary>
   ///      Check configuration file and deploy default if corrupted.
   /// </summary>

   Status(5, AllTasks, DelayStd, 'CRC32 check: ' + ConfigFile + '...', True, Settings.GetPathEventLog);

   if not (Settings.Decode(AppConfig, False)) then
    begin
        Status(5, AllTasks, DelayErr, 'CRC32 check: ' + ConfigFile + '... corrupted! Extracting default file...', True, Settings.GetPathEventLog);
        try
            Unpack(10, Settings.GetPathAppCfg, DeleteOld);
        except
            on E: Exception do
            begin
                Application.MessageBox(
                    PChar('Cannot extract config.cfg. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                );
                Status(5, AllTasks, DelayErr, 'CRC32 check: ' + ConfigFile + '..., unexpected error!', True, Settings.GetPathEventLog);
                LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Cannot extract "config.cfg" from resources. Error has been thrown: ' + E.Message);
                ExitProcess(0);
            end;
        end;
    end
    else
    begin
        Status(5, AllTasks, DelayStd, 'CRC32 check: ' + ConfigFile + '...OK.', True, Settings.GetPathEventLog);
    end;

    /// <summary>
    ///     Check if assemlbies exists in main folder. All assemlbies must be present to run the program. We exclude from this list
    ///     Chromium assemblies because it is not needed to run key features. Chromium is primarly used to display Tableau reports and
    ///     Unity Info web page. This can be also displayed by external web browser.
    /// </summary>
    /// <remarks>
    ///     We do not check CRC32 correctness.
    /// </remarks>

    InitAssembliesRef;

    for iCNT:=0 to High(Assemblies) - 1 do
    begin
        Status(iCNT + 6, AllTasks, DelayStd, 'Checking ' + Assemblies[iCNT] + '...', True, Settings.GetPathEventLog);

        if FileExists(Settings.GetAppDir + Assemblies[iCNT]) then
        begin
            Status(iCNT + 6, AllTasks, DelayStd, 'Checking ' + Assemblies[iCNT] + '... OK.', True, Settings.GetPathEventLog);
        end
        else
        begin
            Application.MessageBox(
                PCHar('Cannot find ' + Assemblies[iCNT] + '. Please re-install application and/or contact IT support.'),
                PChar(APPCAPTION), MB_OK + MB_ICONERROR
            );
            ExitProcess(0);
        end;
    end;

    LogText.Log(Settings.GetPathEventLog, 'End of checking resource and configuration files.');

    /// <summary>
    ///     Synchronise all layouts listed in settings file.
    /// </summary>

    Status(12, AllTasks, DelayStd, 'Synchronising layouts...', True, Settings.GetPathEventLog);

    for iCNT:=0 to Settings.LayoutLists.Count - 1 do
    begin
        if Connection.Download(Settings.GetLayoutsURL + Settings.LayoutLists.Strings[iCNT], Settings.GetLayoutDir + Settings.LayoutLists.Strings[iCNT]) then
        begin
            LogText.Log(Settings.GetPathEventLog, Settings.LayoutLists.Strings[iCNT] + '... synchronised.');
            Status(12, AllTasks, DelayStd, 'Synchronising layouts... ' + Settings.LayoutLists.Strings[iCNT] + ' [synchronised].', False, Settings.GetPathEventLog);
        end
        else
        begin
            LogText.Log(Settings.GetPathEventLog, Settings.LayoutLists.Strings[iCNT] + '... failed to download.');
            Status(12, AllTasks, DelayStd, 'Synchronising layouts... ' + Settings.LayoutLists.Strings[iCNT] + ' [failed].', False, Settings.GetPathEventLog);
        end;
    end;

    /// <summary>
    ///     Initialize Chromium object before Chromium component is created within MainForm.
    /// </summary>
    /// <see cref="https://www.briskbard.com/index.php?lang=en&pageid=cef"/>
    /// <remarks>
    ///     GlobalCEFApp is an instance of the TCEFApplication class an it simpliefies the Chromium initialization.
    /// </remarks>

    Status(13, AllTasks, DelayStd, 'Chromium initialization...', True, Settings.GetPathEventLog);
    GlobalCEFApp:=TCefApplication.Create;
    ChromiumExit:=False;

    try

        /// <summary>
        ///     Do not run Chromium inside Unity application, all HTML rendering should be subprocessed.
        /// </summary>

        Status(14, AllTasks, DelayStd, 'Chromium initialization: assigning sub process...', True, Settings.GetPathEventLog);
        GlobalCEFApp.BrowserSubprocessPath:='SubProcess.exe';

        /// <summary>
        ///     Because TApplication should be only initialized and run in the main process, we call GlobalCEFApp.StartMainProcess to check
        ///     if we have main thread running. If not, we exit the program.
        /// </summary>

        Status(15, AllTasks, DelayStd, 'Chromium initialization: starting main process...', True, Settings.GetPathEventLog);

        try

            /// <remarks>
            ///     Setup framework directory, explicitly to an absolute value. It ensures correct initialization.
            /// </remarks>

            GlobalCEFApp.FrameworkDirPath:=PathAppDir;
            Status(16, AllTasks, DelayStd, 'Chromium initialization: setup Framework...', True, Settings.GetPathEventLog);

            /// <remarks>
            ///     Setup resources directory, explicitly to an absolute value. It ensures correct initialization.
            /// </remarks>

            GlobalCEFApp.ResourcesDirPath:=PathAppDir;
            Status(17, AllTasks, DelayStd, 'Chromium initialization: setup Resources...', True, Settings.GetPathEventLog);

            /// <remarks>
            ///     Setup locales directory, explicitly to an absolute value. It ensures correct initialization.
            /// </remarks>

            GlobalCEFApp.LocalesDirPath:=PathAppDir + 'locales';
            Status(18, AllTasks, DelayStd, 'Chromium initialization: setup Locales...', True, Settings.GetPathEventLog);

            /// <remarks>
            ///     Set the current application directory before loading the CEF3 libraries to avoid "CEF3 binaries missing !" error.
            /// </remarks>

            GlobalCEFApp.SetCurrentDir:=True;

            if not(GlobalCEFApp.StartMainProcess) then
            begin
                Application.MessageBox(
                    PChar('Cannot detect main thread running. Program will be closed. Please contact IT support.'),
                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                );
                ChromiumExit:=True;
            end;
        except
            on E: Exception do
                Status(19, AllTasks, DelayStd, 'Chromium initialization failed, message received: ' + E.Message, False, Settings.GetPathEventLog);
        end;
    finally

        if not(ChromiumExit) then
        begin
            Status(19, AllTasks, DelayStd, 'Chromium initialization: GlobalCEFApp.StartMainProcess returned true.', True, Settings.GetPathEventLog)
        end
        else
        begin
            Status(19, AllTasks, DelayStd, 'Chromium initialization failed, GlobalCEFApp.StartMainProcess returned false, no exception has been thrown.', False, Settings.GetPathEventLog);
            ExitProcess(0);
        end;

    end;

    /// <summary>
    ///     Start the application.
    /// </summary>

    Status(20, AllTasks, 50, 'Application initialization... ', False, Settings.GetPathEventLog);
    Application.Initialize;
    Application.Title:=APPCAPTION;
    Application.MainFormOnTaskbar:=False;

    /// <remarks>
    ///     Release logger object. It will be re-introduced during MainForm initialization.
    /// </remarks>

    LogText.Free;

    /// <summary>
    ///     Load Main Form and execute all of its initialization methods.
    /// </summary>
    /// <remarks>
    ///      Visible parameter must be set to false to prevent from showing the main application window while splash screen is still on.
    /// </remarks>

    Application.CreateForm(TMainForm, MainForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

    /// <summary>
    ///     Load all other forms and execute initialization methods. Similarly to MainForm, visible parameter of the forms must be set to false
    ///     to prevent showing up windows.
    /// </summary>
    /// <remarks>
    ///     Use Status method with TextMode parameter set to False. It allows to log different text and to display different text during loading procedure.
    /// </remarks>

    Application.CreateForm(TAboutForm, AboutForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''AboutForm'' has been created.');
    Status(21, AllTasks, 1, 'Application initialization: [VCL] AboutForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TSendForm, SendForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''SendForm'' has been created.');
    Status(22, AllTasks, 1, 'Application initialization: [VCL] SendForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TEventForm, EventForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''EventForm'' has been created.');
    Status(23, AllTasks, 1, 'Application initialization: [VCL] EventForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TColorsForm, ColorsForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ColorsForm'' has been created.');
    Status(24, AllTasks, 1, 'Application initialization: [VCL] ColorsForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TReportForm, ReportForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ReportForm'' has been created.');
    Status(25, AllTasks, 1, 'Application initialization: [VCL] ReportForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TFilterForm, FilterForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''FilterForm'' has been created.');
    Status(26, AllTasks, 1, 'Application initialization: [VCL] FilterForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TSearchForm, SearchForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''SearchForm'' has been created.');
    Status(27, AllTasks, 1, 'Application initialization: [VCL] SearchForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TTrackerForm, TrackerForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''TrackerForm'' has been created.');
    Status(28, AllTasks, 1, 'Application initialization: [VCL] TrackerForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TActionsForm, ActionsForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ActionsForm'' has been created.');
    Status(29, AllTasks, 1, 'Application initialization: [VCL] ActionsForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TCalendarForm, CalendarForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''CalendarForm'' has been created.');
    Status(30, AllTasks, 1, 'Application initialization: [VCL] CalendarForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TInvoicesForm, InvoicesForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''InvoicesForm'' has been created.');
    Status(31, AllTasks, 1, 'Application initialization: [VCL] InvoicesForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TPhoneListForm, PhoneListForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''PhoneListForm'' has been created.');
    Status(32, AllTasks, 1, 'Application initialization: [VCL] PhoneListForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TViewSearchForm, ViewSearchForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ViewSearchForm'' has been created.');
    Status(33, AllTasks, 1, 'Application initialization: [VCL] ViewSearchForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TViewMailerForm, ViewMailerForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ViewMailerForm'' has been created.');
    Status(34, AllTasks, 1, 'Application initialization: [VCL] ViewMailerForm has been loaded.', False, Settings.GetPathEventLog);

    Application.CreateForm(TAwaitForm, AwaitForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''AwaitForm'' has been created.');
    Status(35, AllTasks, 1, 'Application initialization: [VCL] AwaitForm has been loaded.', False, Settings.GetPathEventLog);

    // Splash screen - 100%
    Status(36, AllTasks, 500, 'Application is initialized.', False, Settings.GetPathEventLog);

    AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
    Sleep(150);
    SplashForm.Free;

    /// <summary>
    ///     Setup last window position and show it to the user.
    /// </summary>

    if Settings.GetStringValue(ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
    if Settings.GetStringValue(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
    if Settings.GetStringValue(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;
    MainForm.LogText.Log(Settings.GetPathEventLog, 'Initialization is completed. Application is running.');
    MainForm.Show;

    /// <remarks>
    ///     Show taskbar icon.
    /// </remarks>

    Application.MainFormOnTaskbar:=True;
    Application.Run;

    /// <summary>
    ///     Destroy Chromium object when application.run exits.
    /// </summary>

    GlobalCEFApp.Free;

    /// <remarks>
    ///     Breaks the message loop in application.run class.
    /// </remarks>

    Application.Terminate;

end.
