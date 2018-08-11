
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
    SynZip,
    SynZipFiles,
    System.Types,
    Model in 'Model\Model.pas',
    SQL in 'Model\SQL.pas',
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
    Arrays in 'Extensions\Arrays.pas',
    InterposerClasses in 'Extensions\InterposerClasses.pas',
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
    ABSearch in 'View\ABSearch.pas' {ViewSearchForm};

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
    MsAssemblies:     TStrings;
    ReleaseNumber:    cardinal;
    PathReleasePak:   string;
    PathReleaseMan:   string;
    PathEventLog:     string;
    PathAppDir:       string;
    WinUserName:      string;
    Manifest:         string;
    RegSettings:      TFormatSettings;

{$R *.res}
{$R 'binres.res' 'binres.rc'}


// ------------------------------------------------------------------------------------------------------------------------------------------ HELPER METHODS //


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
    ZipR:      TZipReader;
    FS:        TFileStream;
    Zipped:    string;
    FullPath:  string;
begin
    ZipR:=TZipReader.Create(FileName);
    LogText.Log(EventLogPath, 'New update package has been found, updating files...');

    try
        for iCNT:=0 to ZipR.Count - 1 do
        begin
            Zipped:=string(ZipR.Entry[iCNT].ZipName);
            FullPath:=DestDir + Zipped;

            // Check if we have path to file or path to folder
            if ExtractFileName(FullPath) <> '' then
            begin
                RenameFile(FullPath, Zipped + '.del');
                FS:=TFileStream.Create(FullPath, fmCreate);
                try
                    ZipR.GetData(iCNT, FS);
                    UpdateForm.Progress.Progress:=Trunc( ( (iCNT + 1) / ZipR.Count ) * 100 );
                    UpdateForm.Update;
                    Sleep(DelayStd);
                finally
                    FS.Free;
                end;
            end

            // Otherwise create given folder so later we can extract file(s) there
            else
            begin
                CreateDir(FullPath);
            end;
        end;

        Result:=True;
        LogText.Log(EventLogPath, 'Old files will be removed by new instance.');
    finally
        ZipR.Free;
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
        LogText.Log(EventLogPath, 'File "' + FileName + '" has been removed.');
        Inc(Check);
    end;

    if Check > 0 then
        LogText.Log(EventLogPath, 'Cleaning folder after previous update has been done (' + IntToStr(Check) + ' items removed).');

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
    ///     We allow only one instance of running program.
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
        WinUserName    :=Settings.GetWinUserName;
    end;

    /// <summary>
    ///     Check event log file.
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

        // Get package from website
        if Connection.Download(PathReleasePak, Settings.GetPackageDir + ReleaseFile) then
        begin
            // Unzip the content, update settings file and execute new release
            if UnzippReleaseFile(Settings.GetPackageDir + ReleaseFile, PathAppDir, PathEventLog) then
            begin
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
    ///     Licence file.
    /// </summary>

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
    ///     Check Windows version. We allow only Windows 7 (with Areo) or Windows 10 (and above).
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
        Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... ', True, Settings.GetPathEventLog);
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
            Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... disabled!', True, Settings.GetPathEventLog);
            Application.MessageBox(
                PChar('Aero is not enabled. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                PChar(APPCAPTION), MB_OK + MB_ICONERROR
            );
            LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Areo composition is disabled. Application has been terminated.');
            ExitProcess(0);
        end;
    end
    else
    begin
        Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... OK.', True, Settings.GetPathEventLog);
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
    ///     Check if assemlbies exists in main folder. All assemlbies must be present to run the program. We exclude from the list
    ///     assemblies related to Chromium.
    /// </summary>
    /// <remarks>
    ///     We do not check CRC32 correctness.
    /// </remarks>

    SetLength(MsAssemblies, 6);
    MsAssemblies[0]:=DLL1;
    MsAssemblies[1]:=DLL2;
    MsAssemblies[2]:=DLL3;
    MsAssemblies[3]:=DLL4;
    MsAssemblies[4]:=DLL5;
    MsAssemblies[5]:=LyncCall;

    for iCNT:=0 to High(MsAssemblies) - 1 do
    begin
        Status(iCNT + 6, AllTasks, DelayStd, 'Checking ' + MsAssemblies[iCNT] + '...', True, Settings.GetPathEventLog);

        if FileExists(Settings.GetAppDir + MsAssemblies[iCNT]) then
        begin
            Status(iCNT + 6, AllTasks, DelayStd, 'Checking ' + MsAssemblies[iCNT] + '... OK.', True, Settings.GetPathEventLog);
        end
        else
        begin
            Application.MessageBox(
                PCHar('Cannot find ' + MsAssemblies[iCNT] + '. Please reinstall application and/or contact IT support.'),
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
            LogText.Log(Settings.GetPathEventLog, Settings.LayoutLists.Strings[iCNT] + '... synchronised.')
                else
                    LogText.Log(Settings.GetPathEventLog, Settings.LayoutLists.Strings[iCNT] + '... failed to download.');
    end;

    /// <remarks>
    ///     Release logger object. It will be re-introduced after MainForm initialization.
    /// </remarks>

    LogText.Free;

    /// <summary>
    ///     Start the application.
    /// </summary>

    Status(13, AllTasks, 50, 'Application initialization... ', False, Settings.GetPathEventLog);
    Application.Initialize;
    Application.Title:=APPCAPTION;
    Application.MainFormOnTaskbar:=False;

    /// <remarks>
    ///      All forms must have parameter "visible" set to false.
    /// </remarks>

    // Main form (view) load
    Application.CreateForm(TMainForm, MainForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

    // Other forms (views)
    Status(14, AllTasks, 400, 'Application initialization: VCL forms loading, please wait.', False, Settings.GetPathEventLog);
    Application.CreateForm(TAboutForm,       AboutForm);       MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''AboutForm'' ......... has been created.');
    Application.CreateForm(TSendForm,        SendForm);        MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''SendForm'' .......... has been created.');
    Application.CreateForm(TEventForm,       EventForm);       MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''EventForm'' ......... has been created.');
    Application.CreateForm(TColorsForm,      ColorsForm);      MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ColorsForm'' ........ has been created.');
    Application.CreateForm(TReportForm,      ReportForm);      MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ReportForm'' ........ has been created.');
    Application.CreateForm(TFilterForm,      FilterForm);      MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''FilterForm'' ........ has been created.');
    Application.CreateForm(TSearchForm,      SearchForm);      MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''SearchForm'' ........ has been created.');
    Application.CreateForm(TTrackerForm,     TrackerForm);     MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''TrackerForm'' ....... has been created.');
    Application.CreateForm(TActionsForm,     ActionsForm);     MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ActionsForm'' ....... has been created.');
    Application.CreateForm(TCalendarForm,    CalendarForm);    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''CalendarForm'' ...... has been created.');
    Application.CreateForm(TInvoicesForm,    InvoicesForm);    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''InvoicesForm'' ...... has been created.');
    Application.CreateForm(TPhoneListForm,   PhoneListForm);   MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''PhoneListForm'' ..... has been created.');
    Application.CreateForm(TViewSearchForm,  ViewSearchForm);  MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ViewSearchForm'' .... has been created.');
    Application.CreateForm(TViewMailerForm,  ViewMailerForm);  MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] ''ViewMailerForm'' .... has been created.');

    // Splash screen - 100%
    Status(15, AllTasks, 900, 'Application is initialized.', False, Settings.GetPathEventLog);

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

    /// <remarks>
    ///     Breaks the message loop in application.run class.
    /// </remarks>

    Application.Terminate;

end.
