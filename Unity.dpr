
/// <para>
///     Filename: Unity.dpr
/// </para>

{$I \Include\Header.inc}

program Unity;

/// <remarks>
///     Large address aware (using more than 3GB for 32-bit program), or compile as a 64-bit.
/// </remarks>

{$SetPEFlags $0020}

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
    System.Types,

    // Database model and handler

    Model in 'Model\Model.pas',
    SQL   in 'Model\SQL.pas',

    // Business logic

    AgeView      in 'Logic\AgeView.pas',
    Database     in 'Logic\Database.pas',
    Mailer       in 'Logic\Mailer.pas',
    Settings     in 'Logic\Settings.pas',
    Transactions in 'Logic\Transactions.pas',
    UAC          in 'Logic\UAC.pas',
    Worker       in 'Logic\Worker.pas',

    // Views

    About      in 'View\About.pas'      {AboutForm},
    Actions    in 'View\Actions.pas'    {ActionsForm},
    Calendar   in 'View\Calendar.pas'   {CalendarForm},
    Colors     in 'View\Colors.pas'     {ColorsForm},
    EventLog   in 'View\EventLog.pas'   {EventForm},
    Filter     in 'View\Filter.pas'     {FilterForm},
    Invoices   in 'View\Invoices.pas'   {InvoicesForm},
    Main       in 'View\Main.pas'       {MainForm},
    PhoneList  in 'View\PhoneList.pas'  {PhoneListForm},
    ReportBug  in 'View\ReportBug.pas'  {ReportForm},
    AVSearch   in 'View\AVSearch.pas'   {SearchForm},
    Send       in 'View\Send.pas'       {SendForm},
    Splash     in 'View\Splash.pas'     {SplashForm},
    Tracker    in 'View\Tracker.pas'    {TrackerForm},
    Update     in 'View\Update.pas'     {UpdateForm},
    MassMailer in 'View\MassMailer.pas' {ViewMailerForm},
    ABSearch   in 'View\ABSearch.pas'   {ViewSearchForm};

type
    DWord = 0..$FFFFFFFF;
    TDwmIsCompositionEnabledFunc = function(out pfEnabled: boolean): HRESULT; stdcall;

/// <remarks>
///     Appication constants are defined in main view throught common.inc file.
/// </remarks>

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

// ------------------------------------------------------------------------------------------------------------------------------------ ADDITIONAL RESOURCES //

{$R 'binres.res' 'binres.rc'}

/// <summary>
///     Extract given source file by provided ID number.
/// </summary>
/// <param name="ItemID">Integer.</param>
/// <param name="FileName">String.</param>
/// <param name="mode">Integer.</param>
/// <returns>Boolean. Set to true if succeed.</returns>
/// <remarks>
///     10 RCDATA "__Makefile\\config.cfg" default setting file.
///     60 RCDATA "__Makefile\\logon.log"  pre-defined event log file.
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

// ---------------------------------------------------------------------------------------------------------------------------- SHOW STATUS ON SPLASH SCREEN //

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
        LogText(EventLogPath, Text);

end;

// ---------------------------------------------------------------------------------------------------------------------- UPDATE ALL FILES FROM RELEASE PACK //

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
    LogText(EventLogPath, 'New update package has been found, updating files...');

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
                    UpdateForm.Progress.Progress:=Trunc(((iCNT + 1)/ZipR.Count) * 100);
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
        LogText(EventLogPath, 'Old files will be removed by new instance.');
    finally
        ZipR.Free;
    end;

end;

// ------------------------------------------------------------------------------------------------------------------- DELETE FILES FOLLOWING GIVEN PATTERN //

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
        LogText(EventLogPath, 'File "' + FileName + '" has been removed.');
        Inc(Check);
    end;

    if Check > 0 then
        LogText(EventLogPath, 'Cleaning folder after previous update has been done (' + IntToStr(Check) + ' items removed).');

end;


// -------------------------------------------------------------------------------------------------------------------------------------- MAIN PROGRAM BLOCK //

begin

    {$WARN SYMBOL_PLATFORM OFF}

    ReportMemoryLeaksOnShutdown:=DebugHook <> 0;

    {$WARN SYMBOL_PLATFORM ON}

    // ---------------------------------------------------------------------------------------------------------------------------------- ALLOW ONE INSTANCE //

    Mutex:=CreateMutex(nil, True, CurrentMutex);

    if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
        Application.MessageBox(
            PCHar(APPCAPTION + ' is already running. You can only have one instance at a time.'),
            PChar(APPCAPTION), MB_OK + MB_ICONWARNING
        );
        ExitProcess(0);
    end;

    // --------------------------------------------------------------------------------------------------------------------------------------- SETUP FORMATS //

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

    // --------------------------------------------------------------------------------------------------- READ CURRENT CONFIG.CFG BEFORE ANY UPDATE ATTEMPT //

    AppSettings:=TSettings.Create;

    try
        // Initialize
        FileDateTime   :=NULLDATE;
        ReleaseDateTime:=NULLDATE;
        PathRelease    :='';
        PathEventLog   :='';
        PathAppDir     :='';
        WinUserName    :='';

        // Extract default config.cfg if missing
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

        // Proceed otherwise
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

    // -------------------------------------------------------------------------------------------------------------------------------- CHECK EVENT LOG FILE //

    if FileExists(PathEventLog) then
    begin
        LogText(PathEventLog, 'Starting application...');
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

    // Force clean up remaining files after previous update
    DeleteFilesMatchingPattern(PathAppDir, '*.del', PathEventLog);

    // -------------------------------------------------------------------------------------------------------------- PERFORM UPDATE IF NEW RELEASE IS FOUND //

    if FileExists(PathRelease) then
    begin
        if FileDateTime > ReleaseDateTime then
        begin

            // Update screen
            UpdateForm:=TUpdateForm.Create(nil);
            SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
            UpdateForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (UpdateForm.Height div 2);
            UpdateForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (UpdateForm.Width  div 2);
            AnimateWindow(UpdateForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
            UpdateForm.Update;

            // Unzipp all files

            /// <remarks>
            ///     Config.cfg may be also update.
            /// </remarks>

            UnzippReleaseFile(PathRelease, PathAppDir, PathEventLog);

            // Update date and time of new release
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

            // Open new copy and close current instance
            ShellExecute(Application.Handle, seOpen, PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
            UpdateForm.Free;
            ExitProcess(0);

        end;

    end;

    // --------------------------------------------------------------------------------------------------------------------------------- START SPLASH SCREEN //

    SplashForm:=TSplashForm.Create(nil);

    SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);

    SplashForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (SplashForm.Height div 2);
    SplashForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (SplashForm.Width  div 2);

    AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
    SplashForm.Update;

    // ------------------------------------------------------------------------------------------------------ RE-OPEN SETTINGS FILE AND PERFORM OTHER CHECKS //

    AppSettings:=TSettings.Create;
    try

        // ----------------------------------------------------------------------------------------------------------------------- CHECK FOR MASTER PASSOWRD //

        if AppSettings.TMIG.ReadString(PasswordSection, 'HASH', '') = '' then
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

        // -------------------------------------------------------------------------------------------------------------------------- CHECK FOR LICENCE FILE //

        if not FileExists(AppSettings.FPathLicence) then
        begin
            Status(2, AllTasks, DelayStd, 'Checking licence file... failed!', True, AppSettings.FPathEventLog);

            /// <remarks>
            ///     Check here ".LICX" file in case of Unity is shareware/limited commercial application.
            /// </remarks>

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

        // ------------------------------------------------------------------------------------------------------ WINDOWS VERSION CHECK - WINDOWS 7 & HIGHER //

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

        // --------------------------------------------------------------------------------------------------- AREO CHECK MUST BE TURNED ON | WINDOWS 7 ONLY //

        if StrToInt(GetOSVer(0)) = 61 then
        begin

            // Initialize
            Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... ', True, AppSettings.FPathEventLog);
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
                Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... disabled!', True, AppSettings.FPathEventLog);
                Application.MessageBox(
                    PChar('Aero is not enabled. ' + APPCAPTION + ' will be closed. Please contact IT Support.'),
                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                );
                LogText(AppSettings.FPathEventLog, '[Critical Error]: Areo composition is disabled. Application has been terminated.');
                AppSettings.Free;
                ExitProcess(0);
            end;
        end
        else
        begin
            Status(4, AllTasks, DelayStd, 'Checking Windows 7 Areo composition... OK.', True, AppSettings.FPathEventLog);
        end;

        // ------------------------------------------------------------------------------------------------------------------------ CHECK CONFIG.CFG | CRC32 //

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

        // ----------------------------------------------------------------------------------------------------- CHECK ASSEMBLIES AND LYNCCALL.EXE IF EXISTS //

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

    // ------------------------------------------------------------------------------------------------------------------------------------------------- END //

    // ------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE //

    AppSettings:=TSettings.Create;
    try
        Status(12, AllTasks, 50, 'Application initialization... connecting with SQL server..., please wait.', False, AppSettings.FPathEventLog);
        Application.Initialize;
        Application.Title:=APPCAPTION;
        Application.MainFormOnTaskbar:=False;

        // -------------------------------------------------------------------------------------------------------------------------------- CREATE ALL FORMS //

        /// <remarks>
        ///      All forms must have visible parameters set to false.
        /// </remarks>

        // Main form (view) load
        Application.CreateForm(TMainForm, MainForm);
        LogText(AppSettings.FPathEventLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + IntToStr(MainThreadID) + '.');

        // Other forms (views)
        Status(13, AllTasks, 400, 'Application initialization... VCL forms loading, please wait.', False, AppSettings.FPathEventLog);
        Application.CreateForm(TAboutForm,       AboutForm);       LogText(AppSettings.FPathEventLog, '[GUI] ''AboutForm'' ......... has been created.');
        Application.CreateForm(TSendForm,        SendForm);        LogText(AppSettings.FPathEventLog, '[GUI] ''SendForm'' .......... has been created.');
        Application.CreateForm(TEventForm,       EventForm);       LogText(AppSettings.FPathEventLog, '[GUI] ''EventForm'' ......... has been created.');
        Application.CreateForm(TColorsForm,      ColorsForm);      LogText(AppSettings.FPathEventLog, '[GUI] ''ColorsForm'' ........ has been created.');
        Application.CreateForm(TReportForm,      ReportForm);      LogText(AppSettings.FPathEventLog, '[GUI] ''ReportForm'' ........ has been created.');
        Application.CreateForm(TFilterForm,      FilterForm);      LogText(AppSettings.FPathEventLog, '[GUI] ''FilterForm'' ........ has been created.');
        Application.CreateForm(TTrackerForm,     TrackerForm);     LogText(AppSettings.FPathEventLog, '[GUI] ''TrackerForm'' ....... has been created.');
        Application.CreateForm(TActionsForm,     ActionsForm);     LogText(AppSettings.FPathEventLog, '[GUI] ''ActionsForm'' ....... has been created.');
        Application.CreateForm(TSearchForm,      SearchForm);      LogText(AppSettings.FPathEventLog, '[GUI] ''SearchForm'' ........ has been created.');
        Application.CreateForm(TCalendarForm,    CalendarForm);    LogText(AppSettings.FPathEventLog, '[GUI] ''CalendarForm'' ...... has been created.');
        Application.CreateForm(TInvoicesForm,    InvoicesForm);    LogText(AppSettings.FPathEventLog, '[GUI] ''InvoicesForm'' ...... has been created.');
        Application.CreateForm(TPhoneListForm,   PhoneListForm);   LogText(AppSettings.FPathEventLog, '[GUI] ''PhoneListForm'' ..... has been created.');
        Application.CreateForm(TViewSearchForm,  ViewSearchForm);  LogText(AppSettings.FPathEventLog, '[GUI] ''ViewSearchForm'' .... has been created.');
        Application.CreateForm(TViewMailerForm,  ViewMailerForm);  LogText(AppSettings.FPathEventLog, '[GUI] ''ViewMailerForm'' .... has been created.');

        // Splash screen - 100%
        Status(14, AllTasks, 900, 'Application initialization... done.', False, AppSettings.FPathEventLog);

        // ------------------------------------------------------------------------------------------------------------------------------- SPLASH SCREEN END //

        AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
        Sleep(150);
        SplashForm.Free;

        // --------------------------------------------------------------------------------------------------------------- SETUP SAVED WINDOW STATE AND SHOW //

        if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
        if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
        if AppSettings.TMIG.ReadString(ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;
        LogText(AppSettings.FPathEventLog, 'Initialization is completed. Application is running.');

    finally
        AppSettings.Free;
    end;

    // ------------------------------------------------------------------------------------------------------------------------------------------------- RUN //

    MainForm.Show;
    Application.MainFormOnTaskbar:=True;
    Application.Run;

    /// <remarks>
    ///     Breaks the message loop in application.run class.
    /// </remarks>

    Application.Terminate;

end.
