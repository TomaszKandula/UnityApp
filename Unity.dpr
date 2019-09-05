program Unity;


// ----------------------------------------
// Unity for Debt Management application
// Copyright (C) 2018-2019 Tomasz Kandula
// VCL/Win32 application for Windows 7 & 10
// ----------------------------------------


{$SetPEFlags $0020}


uses
    System.Classes,
    System.SysUtils,
    System.StrUtils,
    System.DateUtils,
    System.IOUtils,
    System.INIFiles,
    System.Types,
    System.Zip,
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellApi,
    Vcl.Forms,
    Vcl.StdCtrls,
    uCEFApplication,
    Unity.ChkListBox            in 'Extensions\Unity.ChkListBox.pas'{Component extension},
    Unity.Edit                  in 'Extensions\Unity.Edit.pas'{Component extension},
    Unity.Panel                 in 'Extensions\Unity.Panel.pas'{Component extension},
    Unity.Shape                 in 'Extensions\Unity.Shape.pas'{Component extension},
    Unity.Grid                  in 'Extensions\Unity.Grid.pas'{Component extension},
    Unity.ListView              in 'Extensions\Unity.ListView.pas'{Component extension},
    Unity.ComboBox              in 'Extensions\Unity.ComboBox.pas'{Component extension},
    Unity.Crc32                 in 'Helpers\Unity.Crc32.pas',
    Unity.Enums                 in 'Helpers\Types\Unity.Enums.pas',
    Unity.Arrays                in 'Helpers\Types\Unity.Arrays.pas',
    Unity.Records               in 'Helpers\Types\Unity.Records.pas',
    Unity.AdoDb                 in 'Helpers\Statics\Unity.AdoDb.pas',
    Unity.Chars                 in 'Helpers\Statics\Unity.Chars.pas',
    Unity.Delimiters            in 'Helpers\Statics\Unity.Delimiters.pas',
    Unity.DateTimeFormats       in 'Helpers\Statics\Unity.DateTimeFormats.pas',
    Unity.Messaging             in 'Helpers\Statics\Unity.Messaging.pas',
    Unity.NCSI                  in 'Helpers\Statics\Unity.NCSI.pas',
    Unity.Qms                   in 'Helpers\Statics\Unity.Qms.pas',
    Unity.RestAuth              in 'Helpers\Statics\Unity.RestAuth.pas',
    Unity.RiskClass             in 'Helpers\Statics\Unity.RiskClass.pas',
    Unity.SplashScreen          in 'Helpers\Statics\Unity.SplashScreen.pas',
    Unity.StatusBar             in 'Helpers\Statics\Unity.StatusBar.pas',
    Unity.Unknown               in 'Helpers\Statics\Unity.Unknown.pas',
    Unity.Sql                   in 'Helpers\Statics\Unity.Sql.pas',
    Unity.Helpers               in 'Helpers\Statics\Unity.Helpers.pas',
    Unity.Common                in 'Helpers\Statics\Unity.Common.pas',
    Unity.Sorting               in 'Helpers\Statics\Unity.Sorting.pas',
    Unity.Filtering             in 'Helpers\Statics\Unity.Filtering.pas',
    Unity.UserAccess            in 'Helpers\Statics\Unity.UserAccess.pas',
    Unity.UserSid               in 'Helpers\Statics\Unity.UserSid.pas',
    Unity.Utilities             in 'Helpers\Statics\Unity.Utilities.pas',
    DbModel                     in 'Model\DbModel.pas'{Legacy code/to be removed},
    Customer.AddressBook        in 'Model\Json\RawTables\Customer.AddressBook.pas',
    Customer.ControlStatus      in 'Model\Json\RawTables\Customer.ControlStatus.pas',
    Customer.Snapshots          in 'Model\Json\RawTables\Customer.Snapshots.pas',
    Customer.TrackerData        in 'Model\Json\RawTables\Customer.TrackerData.pas',
    Customer.TrackerInvoices    in 'Model\Json\RawTables\Customer.TrackerInvoices.pas',
    Erp.AccountType             in 'Model\Json\RawTables\Erp.AccountType.pas',
    Erp.CustomerGroup           in 'Model\Json\RawTables\Erp.CustomerGroup.pas',
    Erp.Group3                  in 'Model\Json\RawTables\Erp.Group3.pas',
    Erp.PaidInfo                in 'Model\Json\RawTables\Erp.PaidInfo.pas',
    Erp.PaymentTerms            in 'Model\Json\RawTables\Erp.PaymentTerms.pas',
    Erp.Person                  in 'Model\Json\RawTables\Erp.Person.pas',
    Erp.PersonResponsible       in 'Model\Json\RawTables\Erp.PersonResponsible.pas',
    Erp.SalesResponsible        in 'Model\Json\RawTables\Erp.SalesResponsible.pas',
    Handler.Sql                 in 'Logic\AccessLayer\Handler.Sql.pas'{Legacy code/to be removed},
    Handler.Database            in 'Logic\AccessLayer\Handler.Database.pas'{Legacy code/to be removed},
    Handler.Account             in 'Logic\AccessLayer\Handler.Account.pas',
    Handler.Connection          in 'Logic\AccessLayer\Handler.Connection.pas'{Legacy code/to be removed},
    Handler.Rest                in 'Logic\AccessLayer\Handler.Rest.pas',
    AgeView                     in 'Logic\BusinessLayer\AgeView.pas'{Legacy code/to be removed},
    Transactions                in 'Logic\BusinessLayer\Transactions.pas'{Legacy code/to be removed},
    Sync.Mailer                 in 'Logic\BusinessLayer\Sync.Mailer.pas',
    Sync.Documents              in 'Logic\BusinessLayer\Sync.Documents.pas',
    Async.Utilities             in 'Logic\BusinessLayer\Async.Utilities.pas',
    Async.Tracker               in 'Logic\BusinessLayer\Async.Tracker.pas',
    Async.Queries               in 'Logic\BusinessLayer\Async.Queries.pas',
    Async.AddressBook           in 'Logic\BusinessLayer\Async.AddressBook.pas',
    Async.Debtors               in 'Logic\BusinessLayer\Async.Debtors.pas',
    Async.OpenItems             in 'Logic\BusinessLayer\Async.OpenItems.pas',
    Async.Comments              in 'Logic\BusinessLayer\Async.Comments.pas',
    Async.Statements            in 'Logic\BusinessLayer\Async.Statements.pas',
    Unity.Settings              in 'Logic\Configuration\Unity.Settings.pas',
    Unity.ThreadUtilities       in 'Logic\Logger\Unity.ThreadUtilities.pas',
    Unity.EventLogger           in 'Logic\Logger\Unity.EventLogger.pas',
    View.Main                   in 'View\View.Main.pas' {MainForm},
    View.About                  in 'View\View.About.pas' {AboutForm},
    View.Actions                in 'View\View.Actions.pas' {ActionsForm},
    View.AwaitScreen            in 'View\View.AwaitScreen.pas' {AwaitForm},
    View.Calendar               in 'View\View.Calendar.pas' {CalendarForm},
    View.ColorPicker            in 'View\View.ColorPicker.pas' {ColorsForm},
    View.EventLog               in 'View\View.EventLog.pas' {EventForm},
    View.UserFeedback           in 'View\View.UserFeedback.pas' {FeedbackForm},
    View.GridFilter             in 'View\View.GridFilter.pas' {FilterForm},
    View.GridSearch             in 'View\View.GridSearch.pas' {GridSearchForm},
    View.InvoiceList            in 'View\View.InvoiceList.pas' {InvoicesForm},
    View.MassMailer             in 'View\View.MassMailer.pas' {MassMailerForm},
    View.PhoneList              in 'View\View.PhoneList.pas' {PhoneListForm},
    View.Queries                in 'View\View.Queries.pas' {QmsForm},
    View.SendStatement          in 'View\View.SendStatement.pas' {SendForm},
    View.StartupScreen          in 'View\View.StartupScreen.pas' {StartupForm},
    View.SqlSearch              in 'View\View.SqlSearch.pas' {SqlSearchForm},
    View.InvoiceTracker         in 'View\View.InvoiceTracker.pas' {TrackerForm};


{$R *.res}
{$R 'binres.res' 'binres.rc'}


begin

    {$WARN SYMBOL_PLATFORM OFF}
    {Microsoft Windows only}
    ReportMemoryLeaksOnShutdown:=DebugHook <> 0;
    var RegSettings: TFormatSettings:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}

    // --------------------------------
    // Setup application date and time.
    // --------------------------------

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

    // -------------------------------------------
    // Open settings file and decode its contents.
    // -------------------------------------------

    var Settings: ISettings:=TSettings.Create();
    if not Settings.CheckConfigFile then
    begin

        var LastErrorMsg: string;
        if TCommon.Unpack(10, Settings.PathConfig, false, LastErrorMsg) then Settings.ConfigToMemory
        else begin

            Application.MessageBox(
                PCHar(LastErrorMsg + TCommon.AppCaption + ' will be closed. Please contact IT support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );

            ExitProcess(0);

        end;

    end;

    // --------------------------------------------------
    // Set the new session file and return its full path.
    // --------------------------------------------------

    var SessionEventLog:=Settings.MakeNewSessionFile(Settings.NewSessioId);

    // -------------------------------------------------------------------------------------------------------
    // Initialize Chromium object before Chromium component is created within MainForm.
    // <see cref="https://www.briskbard.com/index.php?lang=en&pageid=cef"/>
    // GlobalCEFApp is an instance of the TCEFApplication class an it simpliefies the Chromium initialization.
    // -------------------------------------------------------------------------------------------------------

    GlobalCEFApp:=TCefApplication.Create;
    var ChromiumExit: boolean:=False;

    try

        // ----------------------------------------------------------------------------------------
        // Do not run Chromium inside Unity application, all HTML rendering should be subprocessed.
        // ----------------------------------------------------------------------------------------

        GlobalCEFApp.BrowserSubprocessPath:='SubProcess.exe';
        var PathAppDir: string:=ExtractFileDir(Application.ExeName) + '\';

        try

            // ---------------------------------------------------------------------------------------------------------------------------
            // Because TApplication should be only initialized and run in the main process, we call GlobalCEFApp.StartMainProcess to check
            // if we have main thread running. If not, we exit the program.
            // Setup framework directory, explicitly to an absolute value. It ensures correct initialization.
            // ---------------------------------------------------------------------------------------------------------------------------

            GlobalCEFApp.FrameworkDirPath:=PathAppDir;

            // ----------------------------------------------------------------------------------------------
            // Setup resources directory, explicitly to an absolute value. It ensures correct initialization.
            // ----------------------------------------------------------------------------------------------

            GlobalCEFApp.ResourcesDirPath:=PathAppDir;

            // --------------------------------------------------------------------------------------------
            // Setup locales directory, explicitly to an absolute value. It ensures correct initialization.
            // --------------------------------------------------------------------------------------------

            GlobalCEFApp.LocalesDirPath:=PathAppDir + 'locales';

            // -----------------------------------------------------------------------------------------------------------------
            // Set the current application directory before loading the CEF3 libraries to avoid "CEF3 binaries missing !" error.
            // -----------------------------------------------------------------------------------------------------------------

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
                Application.MessageBox(
                    PChar('Chromium initialization failed, message received: ' + E.Message),
                    PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
                );
        end;

    finally

        if ChromiumExit then
        begin
            Application.MessageBox(
                PChar('Chromium initialization failed, GlobalCEFApp.StartMainProcess returned false, no exception has been thrown.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );
            ExitProcess(0);
        end;

    end;

    Application.Initialize;
    Application.Title:=TCommon.AppCaption;
    Application.MainFormOnTaskbar:=True;

    // ----------------------------------------------------------
    // Call startup view to display splash screen and process the
    // initial loading of settings and database support tables.
    // It will automatically handle main application window.
    // ----------------------------------------------------------

    StartupForm.SetSessionLog(SessionEventLog);
    StartupForm.Show();

    Application.Run;
    GlobalCEFApp.Free;

end.

