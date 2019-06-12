program Unity;


{$SetPEFlags $0020}


uses
    System.Classes,
    System.SysUtils,
    System.StrUtils,
    System.IOUtils,
    System.INIFiles,
    System.Types,
    System.Zip,
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellApi,
    Vcl.Forms,
    Vcl.StdCtrls,
    CRC32u,
    uCEFApplication,
    DbModel                  in 'Model\DbModel.pas'{Legacy code/to be removed},
    Customer.AddressBook     in 'Model\Json\RawTables\Customer.AddressBook.pas',
    Customer.ControlStatus   in 'Model\Json\RawTables\Customer.ControlStatus.pas',
    Customer.Snapshots       in 'Model\Json\RawTables\Customer.Snapshots.pas',
    Customer.TrackerData     in 'Model\Json\RawTables\Customer.TrackerData.pas',
    Customer.TrackerInvoices in 'Model\Json\RawTables\Customer.TrackerInvoices.pas',
    Erp.AccountType          in 'Model\Json\RawTables\Erp.AccountType.pas',
    Erp.CustomerGroup        in 'Model\Json\RawTables\Erp.CustomerGroup.pas',
    Erp.Group3               in 'Model\Json\RawTables\Erp.Group3.pas',
    Erp.PaidInfo             in 'Model\Json\RawTables\Erp.PaidInfo.pas',
    Erp.PaymentTerms         in 'Model\Json\RawTables\Erp.PaymentTerms.pas',
    Erp.Person               in 'Model\Json\RawTables\Erp.Person.pas',
    Erp.PersonResponsible    in 'Model\Json\RawTables\Erp.PersonResponsible.pas',
    Erp.SalesResponsible     in 'Model\Json\RawTables\Erp.SalesResponsible.pas',
    InterposerClasses        in 'Extensions\InterposerClasses.pas'{Too lazy to build own components},
    Helpers                  in 'Extensions\Helpers.pas',
    SqlHandler               in 'Logic\SqlHandler.pas',
    DbHandler                in 'Logic\DbHandler.pas',
    AgeView                  in 'Logic\AgeView.pas',
    Mailer                   in 'Logic\Mailer.pas',
    Settings                 in 'Logic\Settings.pas',
    Transactions             in 'Logic\Transactions.pas',
    UAC                      in 'Logic\UAC.pas',
    Worker                   in 'Logic\Worker.pas',
    Internet                 in 'Logic\Internet.pas',
    ThreadUtilities          in 'Logic\ThreadUtilities.pas',
    EventLogger              in 'Logic\EventLogger.pas',
    About                    in 'View\About.pas' {AboutForm},
    Actions                  in 'View\Actions.pas' {ActionsForm},
    Calendar                 in 'View\Calendar.pas' {CalendarForm},
    Colors                   in 'View\Colors.pas' {ColorsForm},
    EventLog                 in 'View\EventLog.pas' {EventForm},
    Filter                   in 'View\Filter.pas' {FilterForm},
    Invoices                 in 'View\Invoices.pas' {InvoicesForm},
    Main                     in 'View\Main.pas' {MainForm},
    PhoneList                in 'View\PhoneList.pas' {PhoneListForm},
    SendFeedback             in 'View\SendFeedback.pas' {ReportForm},
    AVSearch                 in 'View\AVSearch.pas' {SearchForm},
    Send                     in 'View\Send.pas' {SendForm},
    Splash                   in 'View\Splash.pas' {SplashForm},
    Tracker                  in 'View\Tracker.pas' {TrackerForm},
    Update                   in 'View\Update.pas' {UpdateForm},
    MassMailer               in 'View\MassMailer.pas' {ViewMailerForm},
    ABSearch                 in 'View\ABSearch.pas' {ViewSearchForm},
    Await                    in 'View\Await.pas' {AwaitForm},
    Qms                      in 'View\Qms.pas' {QmsForm};


type
    TDwmIsCompositionEnabledFunc = function(out pfEnabled: boolean): HRESULT; stdcall;


const
    DWMI = 'dwmapi.dll';


{$R *.res}
{$R 'binres.res' 'binres.rc'}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Extract given source file by provided ID number.
/// </summary>
/// <param name="ItemID">Integer</param>
/// <param name="FileName">String</param>
/// <param name="ShouldStay">Boolean</param>
/// <returns>Boolean, true if succeed</returns>
/// <remarks>
/// 10 RCDATA "Makefile\\config.cfg" default setting file.
/// 60 RCDATA "Makefile\\logon.log"  pre-defined event log file.
/// </remarks>

function Unpack(ItemID: integer; FileName: string; ShouldStay: boolean): boolean;
begin

    Result:=False;

    var RS: TResourceStream:=TResourceStream.CreateFromID(hInstance, ItemID, RT_RCDATA);
    try
        RS.Position:=0;
        if not ShouldStay then
            DeleteFile(PChar(FileName));

        try
            RS.SaveToFile(FileName);
        except
            on E: Exception do
            begin
                Application.MessageBox(
                    PCHar('Cannot extract file from resource container. Exception has been thrown: ' + E.Message),
                    PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
                );
                Exit;
            end;
        end;

        Result:=True;

    finally
        RS.free;
    end;

end;


procedure Status(Task: integer; Total: integer; Time: integer; Text: string; TextMode: boolean; EventLogPath: string; var LogText: TThreadFileLog);
begin

    if not Assigned(SplashForm) then Exit;

    SplashForm.TextStatus.Caption  :=Text;
    SplashForm.ProgressBar.Progress:=Trunc((Task / Total) * 100);
    SplashForm.TextProgress.Caption:=IntToStr(SplashForm.ProgressBar.Progress) + '%';

    SplashForm.Update;
    Sleep(Time);

    if (TextMode = True) and (Assigned(LogText)) then LogText.Log(EventLogPath, Text);

end;


function UnzippLayoutsFile(FileName: string; DestDir: string; EventLogPath: string; var LogText: TThreadFileLog): boolean;
begin

    Result:=False;
    if not Assigned(LogText) then Exit;

    var ZipRead: TZipFile:=TZipFile.Create;
    try

        try
            ZipRead.Open(FileName, zmRead);
            LogText.Log(EventLogPath, '[Unity]: unzipping layouts...');

            for var iCNT: integer:=0 to ZipRead.FileCount - 1 do
            begin

                var Zipped:   string:=ZipRead.FileName[iCNT];
                var FullPath: string:=DestDir + Zipped;

                // Extract and create any folder if missing
                ZipRead.Extract(iCNT, DestDir, True);
                LogText.Log(EventLogPath, 'Extracting: ' + Zipped + '.');

            end;

            Result:=True;

        except
            on E: Exception do
                LogText.Log(EventLogPath, '[Unity]: Unexpected error has been thrown: ' + E.Message);
        end;

    finally
        ZipRead.Free;
        DeleteFile(PChar(FileName));
    end;

end;


function UnzippReleaseFile(FileName: string; DestDir: string; EventLogPath: string; var LogText: TThreadFileLog): boolean;
begin

    Result:=False;
    if not Assigned(LogText) then Exit;

    var ZipRead: TZipFile:=TZipFile.Create;
    try
        try
            ZipRead.Open(FileName, zmRead);
            LogText.Log(EventLogPath, '[Automatic updater]: New update package has been found, updating files...');

            for var iCNT: integer:=0 to ZipRead.FileCount - 1 do
            begin

                var Zipped:   string:=ZipRead.FileName[iCNT];
                var FullPath: string:=DestDir + Zipped;

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
        DeleteFile(PChar(FileName));
    end;

end;


/// <summary>
/// During the update, some files cannot be overwritten or removed, thus we change the name and copy new file(s) into the very same place.
/// This methods remove all of the "leftovers" from given folder.
/// </summary>
/// <param name="Directory">Source</param>
/// <param name="Pattern">Indicate what to remove, example: *.png</param>
/// <param name="EventLogPath">Points to event log</param>

procedure DeleteFilesMatchingPattern(const Directory: string; const Pattern: string; EventLogPath: string; var LogText: TThreadFileLog);
begin

    if not Assigned(LogText) then Exit;

    var FileName: string;
    var Check: cardinal:=0;

    for FileName in TDirectory.GetFiles(Directory, Pattern) do
    begin
        TFile.Delete(FileName);
        LogText.Log(EventLogPath, '[Automatic updater]: File "' + FileName + '" has been removed.');
        Inc(Check);
    end;

    if Check > 0 then
        LogText.Log(EventLogPath, '[Automatic updater]: Cleaning folder after previous update has been done (' + Check.ToString + ' items removed).');

end;


/// <summary>
/// Parse content of manifest file and return requested Key value.
/// </summary>

function GetManifestValue(Key: string; Source: string): string;

    function GetValue(Key: string): string;
    begin

        var StartPos: integer:=AnsiPos(Key, Source);

        for var iCNT: integer:=StartPos to Length(Source) do
        if Source[iCNT] = TChars.CR then
        begin
            Result:=MidStr(Source, StartPos, (iCNT - StartPos)).Replace(Key, '').ToLower;
            Break;
        end;

    end;

begin
    Result:=GetValue('$' + Key + '=');
end;


// ---------------------------------------------------------------------------------------------------------------------------------------------------- MAIN //


begin

    {$WARN SYMBOL_PLATFORM OFF} {Windows only}
    ReportMemoryLeaksOnShutdown:=DebugHook <> 0;
    {$WARN SYMBOL_PLATFORM ON}

    /// <summary>
    /// We allow only one instance of running program (no sessions).
    /// </summary>

    var Mutex: integer:=CreateMutex(nil, True, TCommon.CurrentMutex);
    if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
        Application.MessageBox(
            PCHar(TCommon.AppCaption + ' is already running. You can only have one instance at a time.'),
            PChar(TCommon.AppCaption), MB_OK + MB_ICONWARNING
        );
        ExitProcess(0);
    end;

    /// <summary>
    /// Setup formats to user local settings.
    /// </summary>

    {$WARN SYMBOL_PLATFORM OFF} {Windows only}
    var RegSettings: TFormatSettings:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}

    RegSettings.CurrencyDecimals    :=4;
    RegSettings.DateSeparator       :='-';
    RegSettings.ShortDateFormat     :='yyyy-mm-dd';
    RegSettings.LongDateFormat      :='yyyy-mm-dd';
    RegSettings.TimeSeparator       :=':';
    RegSettings.TimeAMString        :='AM';
    RegSettings.TimePMString        :='PM';
    RegSettings.ShortTimeFormat     :='hh:mm tt';
    RegSettings.LongTimeFormat      :='hh:mm:ss';
    FormatSettings                  :=RegSettings;
    Application.UpdateFormatSettings:=False;

    /// <summary>
    /// Initialize interfaced objects for internet methods and settings file operations and event logging.
    /// </summary>
    /// <remarks>
    /// Because LogText is re-introduced in MainForm, it has to be free before MainForm initialization.
    /// </remarks>

    var Connection: IConnectivity:=TConnectivity.Create;
    var Settings: ISettings:=TSettings.Create;
    var LogText: TThreadFileLog:=TThreadFileLog.Create;

    /// <summary>
    /// Check internet connection.
    /// </summary>

    if not(Connection.IsInternetPresent) then
    begin
        Application.MessageBox(
            PCHar(TCommon.AppCaption + ' cannot work off-line. Please check Internet connection or contact your network administrator. Program will be closed.'),
            PChar(TCommon.AppCaption), MB_OK + MB_ICONWARNING
        );
        ExitProcess(0);
    end;

    /// <summary>
    /// Get all necessary settings from configuration file before any possible update.
    /// </summary>

    var ReleaseNumber: cardinal:=0;

    // Extract default config.cfg if it is missing
    if Settings.GetLastError = 404 then
    begin
        if Unpack(10, Settings.GetPathAppCfg, false) then
            Settings.ConfigToMemory
        else
        begin
            Application.MessageBox(
                PCHar('Cannot extract missing configuration file. ' + TCommon.AppCaption + ' will be closed. Please contact IT support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );
            ExitProcess(0);
        end;
    end;

    var PathReleasePak: string;
    var PathReleaseMan: string;
    var PathEventLog:   string;
    var PathAppDir:     string;
    var PackageDir:     string;
    var WinUserName:    string;

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
    /// Check event log file. If it is missing, then unpack from EXE resources default file.
    /// </summary>

    if FileExists(PathEventLog) then
    begin
        LogText.Log(PathEventLog, 'Starting application...');
    end
    else
    {Otherwise extract default}
    begin

        if Unpack(60, PathEventLog, true) then
        begin
            // Put user logon name to log file (@ eof)
            var FL: TFileStream:=TFileStream.Create(PathEventLog, fmOpenWrite);
            try

                var StrWrite: string:=WinUserName + '.' + TChars.CRLF + TChars.CRLF;
                FL.Position:=FL.Size;

                for var iCNT: integer:=1 to length(StrWrite) do
                    FL.Write(StrWrite[iCNT], 1);

            finally
                FL.Free;
            end;

            LogText.Log(PathEventLog, 'Starting application...');

        end
        else
        begin

            Application.MessageBox(
                PChar('Cannot create log file. ' + TCommon.AppCaption + ' will be closed. Please contact IT support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );

            ExitProcess(0);

        end;
    end;

    // Force clean up remaining files after previous update
    DeleteFilesMatchingPattern(PathAppDir, '*.del', PathEventLog, LogText);
    DeleteFilesMatchingPattern(PathAppDir + 'locales\', '*.del', PathEventLog, LogText);
    DeleteFilesMatchingPattern(PathAppDir + 'swiftshader\', '*.del', PathEventLog, LogText);

    // Check directories
    if not(DirectoryExists(Settings.GetLayoutDir)) then CreateDir(Settings.GetLayoutDir);
    if not(DirectoryExists(Settings.GetPackageDir)) then CreateDir(Settings.GetPackageDir);

    /// <summary>
    /// Check manifest and update if application release number is lower.
    /// </summary>

    var WndRect: TRect;
    var Manifest: string:=Connection.GetResponseText(Settings.GetReleaseManURL);
    if ( GetManifestValue('Status', Manifest) = 'update' ) and ( GetManifestValue('Release', Manifest) > ReleaseNumber.ToString ) then
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
        if Connection.Download(PathReleasePak, PackageDir + TCommon.ReleaseFile) then
        begin
            Settings:=nil;
            // Unzip the content, update settings file and execute new release
            if UnzippReleaseFile(PackageDir + TCommon.ReleaseFile, PathAppDir, PathEventLog, LogText) then
            begin
                Settings:=TSettings.Create;
                Settings.ReleaseDateTime:=Now;
                Settings.ReleaseNumber:=StrToInt(GetManifestValue('Release', Manifest));
                ShellExecute(Application.Handle, 'open', PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
            end
            else
            begin
                Application.MessageBox(
                    PChar('Cannot unpack files. ' + TCommon.AppCaption + ' will be closed. Please contact IT support.'),
                    PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
                );
            end;
        end
        else
        begin
            Application.MessageBox(
                PChar('Cannot download new release package. ' + TCommon.AppCaption + ' will be closed. Please contact IT support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );
        end;

        UpdateForm.Free;
        ExitProcess(0);

    end;

    /// <remarks>
    /// Splash screen requires updating to display the changed content.
    /// </remarks>

    SplashForm:=TSplashForm.Create(nil);
    SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
    SplashForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (SplashForm.Height div 2);
    SplashForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (SplashForm.Width  div 2);
    AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
    SplashForm.Update;

    /// <summary>
    /// Check password. It is hashed with BCrypt and cannot be shorter than 60 characters.
    /// </summary>

    if Length(Settings.GetStringValue(TConfigSections.PasswordSection, 'HASH', '')) < 60 then
    begin

        Status(1, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking master password... failed!', True, Settings.GetPathEventLog, LogText);

        Application.MessageBox(
            PCHar('Invalid master password has been found. ' + TCommon.AppCaption + ' will be closed. Please contact IT Support.'),
            PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
        );

        LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Invaid master password has been found. Application has been terminated.');
        ExitProcess(0);

    end
    else
    begin
        Status(1, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking master password... OK.', True, Settings.GetPathEventLog, LogText);
    end;

    /// <summary>
    /// Check here ".LIC" file in case of Unity is shareware/limited commercial application.
    /// </summary>

    {TODO -oTomek -cGeneral : Extend this by adding CRC32 check}

    if not FileExists(Settings.GetPathLicenceLic) then
    begin

        Status(2, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking licence file... failed!', True, Settings.GetPathEventLog, LogText);

        Application.MessageBox(
            PCHar('Cannot find licence file (' + TCommon.LicenceFile + '). ' + TCommon.AppCaption + ' will be closed. Please contact IT Support.'),
            PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
        );
        LogText.Log(Settings.GetPathEventLog, '[Critical Error]: No licence file has been found. Application has been terminated.');
        ExitProcess(0);

    end
    else
    begin
        Status(2, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking licence file... OK.', True, Settings.GetPathEventLog, LogText);
    end;

    /// <summary>
    /// Check Windows version. We allow only Windows 7 (with Aero) or Windows 10 (and above).
    /// </summary>

    if not TCommon.GetOSVer(False).ToInteger >= 61 then
    begin

        Status(3, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking operating system version... failed!', True, Settings.GetPathEventLog, LogText);

        Application.MessageBox(
            PCHar(TCommon.AppCaption + ' must be run under Windows 7 or higher. ' + TCommon.AppCaption + ' will be closed. Please contact IT Support.'),
            PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
        );

        LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Invalid Operating System. Application has been terminated.');
        ExitProcess(0);

    end
    else
    begin
        Status(3, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking operating system version... OK.', True, Settings.GetPathEventLog, LogText);
    end;

    /// <summary>
    /// Areo must be enabled if program runs under Windows 7.
    /// </summary>

    if TCommon.GetOSVer(False).ToInteger = 61 then
    begin

        Status(4, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking Windows 7 Aero composition... ', True, Settings.GetPathEventLog, LogText);

        var IsAreoOn: TDwmIsCompositionEnabledFunc;
        var IsEnabled: boolean:=False;
        var IsAeroEnabled: boolean:=False;
        var ModuleHandle: HMODULE:=LoadLibrary(PChar(DWMI));

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

            Status(4, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking Windows 7 Aero composition... disabled!', True, Settings.GetPathEventLog, LogText);

            Application.MessageBox(
                PChar('Aero is not enabled. ' + TCommon.AppCaption + ' will be closed. Please contact IT Support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );

            LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Aero composition is disabled. Application has been terminated.');
            ExitProcess(0);

        end;
    end
    else
    begin
        Status(4, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking Windows 7 Aero composition... OK.', True, Settings.GetPathEventLog, LogText);
    end;

   /// <summary>
   /// Check configuration file and deploy default if corrupted.
   /// </summary>

   Status(5, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'CRC32 check: ' + TCommon.ConfigFile + '...', True, Settings.GetPathEventLog, LogText);

   if not (Settings.Decode(TCommon.TFiles.AppConfig, False)) then
    begin

        Status(5, TSplashScreen.AllTasks, TSplashScreen.DelayErr, 'CRC32 check: ' + TCommon.ConfigFile + '... corrupted! Extracting default file...', True, Settings.GetPathEventLog, LogText);

        try
            Unpack(10, Settings.GetPathAppCfg, false);
        except
            on E: Exception do
            begin

                Application.MessageBox(
                    PChar('Cannot extract config.cfg. ' + TCommon.AppCaption + ' will be closed. Please contact IT Support.'),
                    PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
                );

                Status(5, TSplashScreen.AllTasks, TSplashScreen.DelayErr, 'CRC32 check: ' + TCommon.ConfigFile + '..., unexpected error!', True, Settings.GetPathEventLog, LogText);

                LogText.Log(Settings.GetPathEventLog, '[Critical Error]: Cannot extract "config.cfg" from resources. Error has been thrown: ' + E.Message);
                ExitProcess(0);

            end;
        end;
    end
    else
    begin
        Status(5, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'CRC32 check: ' + TCommon.ConfigFile + '...OK.', True, Settings.GetPathEventLog, LogText);
    end;

    /// <summary>
    /// Check if all assemlbies exists in main folder. All of them must be present to run the program. We exclude from this list
    /// Chromium assemblies because it is not needed to run key features. Chromium is primarly used to display Tableau reports and
    /// Unity Info web page. In rare case, this can be also displayed by external web browser.
    /// </summary>

    var Assemblies: TAStrings;
    SetLength(Assemblies, 6);
    Assemblies[0]:=TLyncLib.LyncControls;
    Assemblies[1]:=TLyncLib.LyncFramework;
    Assemblies[2]:=TLyncLib.LyncModel;
    Assemblies[3]:=TLyncLib.LyncUtils;
    Assemblies[4]:=TLyncLib.OfficeUc;
    Assemblies[5]:=TLyncLib.LyncCall;

    for var iCNT: integer:=0 to High(Assemblies) - 1 do
    begin
        Status(iCNT + 6, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking ' + Assemblies[iCNT] + '...', True, Settings.GetPathEventLog, LogText);

        if FileExists(Settings.GetAppDir + Assemblies[iCNT]) then
        begin
            Status(iCNT + 6, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Checking ' + Assemblies[iCNT] + '... OK.', True, Settings.GetPathEventLog, LogText);
        end
        else
        begin

            Application.MessageBox(
                PCHar('Cannot find ' + Assemblies[iCNT] + '. Please re-install application and/or contact IT support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );

            ExitProcess(0);

        end;
    end;

    LogText.Log(Settings.GetPathEventLog, 'End of checking resource and configuration files.');

    /// <summary>
    /// Synchronise all layouts listed in settings file.
    /// </summary>

    Status(12, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Synchronising layouts...', True, Settings.GetPathEventLog, LogText);
    Connection.Download(Settings.GetLayoutsURL + TCommon.LayoutPak, Settings.GetLayoutDir + TCommon.LayoutPak);
    UnzippLayoutsFile(Settings.GetLayoutDir + TCommon.LayoutPak, Settings.GetLayoutDir, PathEventLog, LogText);

    /// <summary>
    /// Initialize Chromium object before Chromium component is created within MainForm.
    /// </summary>
    /// <see cref="https://www.briskbard.com/index.php?lang=en&pageid=cef"/>
    /// <remarks>
    /// GlobalCEFApp is an instance of the TCEFApplication class an it simpliefies the Chromium initialization.
    /// </remarks>

    Status(13, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization...', True, Settings.GetPathEventLog, LogText);
    GlobalCEFApp:=TCefApplication.Create;
    var ChromiumExit: boolean:=False;

    try

        /// <summary>
        /// Do not run Chromium inside Unity application, all HTML rendering should be subprocessed.
        /// </summary>

        Status(14, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization: assigning sub process...', True, Settings.GetPathEventLog, LogText);
        GlobalCEFApp.BrowserSubprocessPath:='SubProcess.exe';

        /// <summary>
        /// Because TApplication should be only initialized and run in the main process, we call GlobalCEFApp.StartMainProcess to check
        /// if we have main thread running. If not, we exit the program.
        /// </summary>

        Status(15, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization: starting main process...', True, Settings.GetPathEventLog, LogText);

        try

            /// <remarks>
            /// Setup framework directory, explicitly to an absolute value. It ensures correct initialization.
            /// </remarks>

            GlobalCEFApp.FrameworkDirPath:=PathAppDir;
            Status(16, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization: setup Framework...', True, Settings.GetPathEventLog, LogText);

            /// <remarks>
            /// Setup resources directory, explicitly to an absolute value. It ensures correct initialization.
            /// </remarks>

            GlobalCEFApp.ResourcesDirPath:=PathAppDir;
            Status(17, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization: setup Resources...', True, Settings.GetPathEventLog, LogText);

            /// <remarks>
            /// Setup locales directory, explicitly to an absolute value. It ensures correct initialization.
            /// </remarks>

            GlobalCEFApp.LocalesDirPath:=PathAppDir + 'locales';
            Status(18, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization: setup Locales...', True, Settings.GetPathEventLog, LogText);

            /// <remarks>
            /// Set the current application directory before loading the CEF3 libraries to avoid "CEF3 binaries missing !" error.
            /// </remarks>

            GlobalCEFApp.SetCurrentDir:=True;

            if not(GlobalCEFApp.StartMainProcess) then
            begin

                Application.MessageBox(
                    PChar('Cannot detect main thread running. Program will be closed. Please contact IT support.'),
                    PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
                );

                ChromiumExit:=True;

            end;
        except
            on E: Exception do
                Status(19, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization failed, message received: ' + E.Message, False, Settings.GetPathEventLog, LogText);
        end;
    finally

        if not(ChromiumExit) then
        begin
            Status(19, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization: GlobalCEFApp.StartMainProcess returned true.', True, Settings.GetPathEventLog, LogText)
        end
        else
        begin
            Status(19, TSplashScreen.AllTasks, TSplashScreen.DelayStd, 'Chromium initialization failed, GlobalCEFApp.StartMainProcess returned false, no exception has been thrown.', False, Settings.GetPathEventLog, LogText);
            ExitProcess(0);
        end;

    end;


    // ----------------------------------------------------------------------------------------------------------------------------------------------------- //


    Status(20, TSplashScreen.AllTasks, 50, 'Application initialization... ', False, Settings.GetPathEventLog, LogText);
    Application.Initialize;
    Application.Title:=TCommon.AppCaption;
    Application.MainFormOnTaskbar:=False;

    /// <remarks>
    /// Release logger object. It will be re-introduced during MainForm initialization.
    /// </remarks>

    LogText.Free;

    /// <summary>
    /// Load Main Form and execute all of its initialization methods.
    /// </summary>
    /// <remarks>
    /// Visible parameter must be set to false to prevent from showing the main application window while splash screen is still on.
    /// </remarks>

    Application.CreateForm(TMainForm, MainForm);
    MainForm.LogText.Log(Settings.GetPathEventLog, '[GUI] Initialization methods executed within main thread, ''MainForm'' has been created. Main process thread ID = ' + MainThreadID.ToString + '.');

    /// <summary>
    /// Load all other forms and execute initialization methods. Similarly to MainForm, visible parameter of the forms must be set to false
    /// to prevent showing up windows.
    /// </summary>

    Application.CreateForm(TAboutForm, AboutForm);
    Application.CreateForm(TSendForm, SendForm);
    Application.CreateForm(TEventForm, EventForm);
    Application.CreateForm(TColorsForm, ColorsForm);
    Application.CreateForm(TReportForm, ReportForm);
    Application.CreateForm(TFilterForm, FilterForm);
    Application.CreateForm(TSearchForm, SearchForm);
    Application.CreateForm(TTrackerForm, TrackerForm);
    Application.CreateForm(TActionsForm, ActionsForm);
    Application.CreateForm(TCalendarForm, CalendarForm);
    Application.CreateForm(TInvoicesForm, InvoicesForm);
    Application.CreateForm(TPhoneListForm, PhoneListForm);
    Application.CreateForm(TViewSearchForm, ViewSearchForm);
    Application.CreateForm(TViewMailerForm, ViewMailerForm);
    Application.CreateForm(TAwaitForm, AwaitForm);
    Application.CreateForm(TQmsForm, QmsForm);


    // ----------------------------------------------------------------------------------------------------------------------------------------------------- //


    Status(21, TSplashScreen.AllTasks, 500, 'Application is initialized.', False, Settings.GetPathEventLog, LogText);
    AnimateWindow(SplashForm.Handle, 500, AW_BLEND or AW_HIDE);
    Sleep(150);
    SplashForm.Free;

    if Settings.GetStringValue(TConfigSections.ApplicationDetails,  'WINDOW_STATE', '') = 'wsNormal'    then MainForm.WindowState:=wsNormal;
    if Settings.GetStringValue(TConfigSections.ApplicationDetails,  'WINDOW_STATE', '') = 'wsMaximized' then MainForm.WindowState:=wsMaximized;
    if Settings.GetStringValue(TConfigSections.ApplicationDetails,  'WINDOW_STATE', '') = 'wsMinimized' then MainForm.WindowState:=wsMinimized;

    MainForm.LogText.Log(Settings.GetPathEventLog, 'Initialization is completed. Application is running.');
    MainForm.Show;
    Application.MainFormOnTaskbar:=True;
    Application.Run;
    GlobalCEFApp.Free;
    Application.Terminate;

end.

