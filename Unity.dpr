program Unity;


// ====================================================================== //
// Application full name:  Unity Platform.                                //
// Application shortname:  Unity.                                         //
// Major version name:     Cadiz.                                         //
// ---------------------------------------------------------------------- //
// Introduced:             Mid 2016 (concept).                            //
// First test release:     2017-07-10.                                    //
// Last stable release:    2019-08-01 (production).                       //
// ---------------------------------------------------------------------- //
// IDE:                    RAD Studio (2019).                             //
// Language:               Delphi* 10.3.2.                                //
// ---------------------------------------------------------------------- //
// Target system:          Windows 10 application.                        //
// Framework used:         VCL (created by Borland).                      //
// Require web services:   Yes, cloud based (Azure).                      //
// Active Directory:       Yes, via web service.                          //
// Chromium engine:        CEF4Delphi.                                    //
// ---------------------------------------------------------------------- //
// Copyright (C) 2016-2019 Tomasz Kandula/DFDS Polska sp. z o.o.          //
// All rights reserved.                                                   //
// ====================================================================== //
// *Delphi language in version 10.3.x uses inline variables               //
// and therefore it is not backward compatibile with older versions.      //
// ====================================================================== //
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        //
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     //
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. //
// IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR      //
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,  //
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR  //
// OTHER DEALINGS IN THE SOFTWARE.                                        //
// ====================================================================== //


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
    Unity.ChkListBox            in 'Extensions\Unity.ChkListBox.pas',
    Unity.Edit                  in 'Extensions\Unity.Edit.pas',
    Unity.Panel                 in 'Extensions\Unity.Panel.pas',
    Unity.Shape                 in 'Extensions\Unity.Shape.pas',
    Unity.Grid                  in 'Extensions\Unity.Grid.pas',
    Unity.ListView              in 'Extensions\Unity.ListView.pas',
    Unity.ComboBox              in 'Extensions\Unity.ComboBox.pas',
    Unity.Crc32                 in 'Helpers\Unity.Crc32.pas',
    Unity.Enums                 in 'Helpers\Types\Unity.Enums.pas',
    Unity.Arrays                in 'Helpers\Types\Unity.Arrays.pas',
    Unity.Records               in 'Helpers\Types\Unity.Records.pas',
	Unity.References            in 'Helpers\Types\Unity.References.pas',
    Unity.AdoDb                 in 'Helpers\Statics\Unity.AdoDb.pas',
    Unity.Chars                 in 'Helpers\Statics\Unity.Chars.pas',
    Unity.Delimiters            in 'Helpers\Statics\Unity.Delimiters.pas',
    Unity.DateTimeFormats       in 'Helpers\Statics\Unity.DateTimeFormats.pas',
    Unity.Qms                   in 'Helpers\Statics\Unity.Qms.pas',
    Unity.StatusBar             in 'Helpers\Statics\Unity.StatusBar.pas',
    Unity.Unknown               in 'Helpers\Statics\Unity.Unknown.pas',
    Unity.Sql                   in 'Helpers\Statics\Unity.Sql.pas',
    Unity.Helpers               in 'Helpers\Statics\Unity.Helpers.pas',
    Unity.Common                in 'Helpers\Statics\Unity.Common.pas',
    Unity.Sorting               in 'Helpers\Statics\Unity.Sorting.pas',
    Unity.Filtering             in 'Helpers\Statics\Unity.Filtering.pas',
    DbModel                     in 'Model\DbModel.pas',
	Api.ErrorHandler            in 'Model\Json\Api.ErrorHandler.pas',
	Api.UserSessionAdd          in 'Model\Json\Requests\Api.UserSessionAdd.pas',
	Api.UserSessionAdded        in 'Model\Json\Responses\Api.UserSessionAdded.pas',
	Api.UserSessionChecked      in 'Model\Json\Responses\Api.UserSessionChecked.pas',
    Handler.Sql                 in 'Logic\AccessLayer\Handler.Sql.pas',
    Handler.Database            in 'Logic\AccessLayer\Handler.Database.pas',
    Unity.RestWrapper           in 'Logic\AccessLayer\Unity.RestWrapper.pas',
    Sync.Mailer                 in 'Logic\BusinessLayer\Sync.Mailer.pas',
    Sync.Documents              in 'Logic\BusinessLayer\Sync.Documents.pas',
	Async.Accounts              in 'Logic\BusinessLayer\Async.Accounts.pas',
    Async.Utilities             in 'Logic\BusinessLayer\Async.Utilities.pas',
    Async.Tracker               in 'Logic\BusinessLayer\Async.Tracker.pas',
    Async.Queries               in 'Logic\BusinessLayer\Async.Queries.pas',
    Async.AddressBook           in 'Logic\BusinessLayer\Async.AddressBook.pas',
    Async.Debtors               in 'Logic\BusinessLayer\Async.Debtors.pas',
    Async.OpenItems             in 'Logic\BusinessLayer\Async.OpenItems.pas',
    Async.Comments              in 'Logic\BusinessLayer\Async.Comments.pas',
    Async.Statements            in 'Logic\BusinessLayer\Async.Statements.pas',
	Async.InvoiceTracker        in 'Logic\BusinessLayer\Async.InvoiceTracker.pas',
	Async.Companies             in 'Logic\BusinessLayer\Async.Companies.pas',
	Async.Mailer                in 'Logic\BusinessLayer\Async.Mailer.pas',
    Async.GeneralTables         in 'Logic\BusinessLayer\Async.GeneralTables.pas',
	Unity.Settings              in 'Logic\Configuration\Unity.Settings.pas',
    Unity.SessionService        in 'Logic\Configuration\Unity.SessionService.pas',
    Unity.ThreadUtilities       in 'Logic\Logger\Unity.ThreadUtilities.pas',
    Unity.EventLogger           in 'Logic\Logger\Unity.EventLogger.pas',
    View.BusyScreen             in 'View\View.BusyScreen.pas' {BusyForm},
	View.Main                   in 'View\View.Main.pas' {MainForm},
    View.About                  in 'View\View.About.pas' {AboutForm},
    View.Actions                in 'View\View.Actions.pas' {ActionsForm},
    View.Calendar               in 'View\View.Calendar.pas' {CalendarForm},
    View.ColorPicker            in 'View\View.ColorPicker.pas' {ColorsForm},
    View.EventLog               in 'View\View.EventLog.pas' {EventForm},
    View.UserFeedback           in 'View\View.UserFeedback.pas' {FeedbackForm},
    View.GridFilter             in 'View\View.GridFilter.pas' {FilterForm},
    View.GridSearch             in 'View\View.GridSearch.pas' {GridSearchForm},
    View.InvoiceList            in 'View\View.InvoiceList.pas' {InvoicesForm},
    View.MassMailer             in 'View\View.MassMailer.pas' {MassMailerForm},
    View.PhoneList              in 'View\View.PhoneList.pas' {PhoneListForm},
	View.Reports                in 'View\View.Reports.pas' {ReportsForm},
    View.Queries                in 'View\View.Queries.pas' {QmsForm},
    View.SendStatement          in 'View\View.SendStatement.pas' {SendForm},
    View.Startup                in 'View\View.Startup.pas' {StartupForm},
    View.SqlSearch              in 'View\View.SqlSearch.pas' {SqlSearchForm},
    View.InvoiceTracker         in 'View\View.InvoiceTracker.pas' {TrackerForm},
	View.CompanyList            in 'View\View.CompanyList.pas' {CompanyListForm};


{$R *.res}


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

    // ------------------------------------------------------
    // Open settings file, decode its content and Initialize
    // new session service that holds session data throughout
    // the application lifetime.
    // ------------------------------------------------------
    var Settings: ISettings:=TSettings.Create();
    if not Settings.CheckConfigFile then
    begin

        var LastErrorMsg: string;
        if THelpers.Unpack(10, Settings.PathConfig, false, LastErrorMsg) then Settings.ConfigToMemory()
        else begin

            Application.MessageBox(
                PCHar(LastErrorMsg + TCommon.AppCaption + ' will be closed. Please contact IT support.'),
                PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
            );

            ExitProcess(0);

        end;

    end;

    Settings.MakeNewSessionId();
    SessionService.InitializeSession(Settings.NewSessionId, Settings.MakeNewSessionFile(Settings.NewSessionId));

    // -------------------------------------------------------------------------------------------------------
    // Initialize Chromium object before Chromium component is created within MainForm.
    // <see cref="https://www.briskbard.com/index.php?lang=en&pageid=cef"/>
    // <see cref="https://github.com/salvadordf/CEF4Delphi" />
    // GlobalCEFApp is an instance of the TCEFApplication class an it simpliefies the Chromium initialization.
    // -------------------------------------------------------------------------------------------------------
    GlobalCEFApp:=TCefApplication.Create();
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

            // -----------------------------------------------------------------
            // Setup Coockies and Cache folders to allow store web browser data.
            // -----------------------------------------------------------------
            GlobalCEFApp.PersistSessionCookies:=False; // Do not store coockies without expiry/validity date!
            GlobalCEFApp.Cache  :=PathAppDir + 'cache';
            GlobalCEFApp.Cookies:=PathAppDir + 'coockies';

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

    Application.Initialize();
    Application.Title:=TCommon.AppCaption;
    Application.MainFormOnTaskbar:=True;

    // -------------------------------------------------------------------
    // Call startup view to display splash screen and process loading of
    // various assets and/or data (including database supporting tables).
    // It will automatically handle main user window.
    // Warning! Main application window (mian form) is View.Startup, and
    // therefore the user main form (View.Main) must have taskbar icon
    // re-assigned to it, so it can act as a main window of the Windows
    // application.
    // -------------------------------------------------------------------
    StartupForm.SetSessionLog(SessionService.SessionLog);
    StartupForm.Show();

    Application.Run;{Starts event loop}
    DestroySessionService();
    GlobalCEFApp.Free();

end.

