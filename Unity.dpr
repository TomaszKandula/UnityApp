program Unity;

// ====================================================================== //
// Application full name:  Unity Platform.                                //
// Application shortname:  Unity.                                         //
// Major version name:     Callisto.                                      //
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
// Require web services:   Yes, cloud based (Microsoft Azure).            //
// Active Directory:       Yes, via web service.                          //
// Chromium engine:        CEF4Delphi / embedded, sub-processed           //
// ---------------------------------------------------------------------- //
// Copyright (C) 2017-2020 Tomasz Kandula/DFDS Polska sp. z o.o.          //
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
    Unity.ChkListBox in 'Extensions\Unity.ChkListBox.pas',
    Unity.Edit in 'Extensions\Unity.Edit.pas',
    Unity.Panel in 'Extensions\Unity.Panel.pas',
    Unity.Shape in 'Extensions\Unity.Shape.pas',
    Unity.Grid in 'Extensions\Unity.Grid.pas',
    Unity.ListView in 'Extensions\Unity.ListView.pas',
    Unity.ComboBox in 'Extensions\Unity.ComboBox.pas',
    Unity.Crc32 in 'Helpers\Unity.Crc32.pas',
    Unity.Enums in 'Helpers\Unity.Enums.pas',
    Unity.Records in 'Helpers\Unity.Records.pas',
    Unity.References in 'Helpers\Unity.References.pas',
    Unity.Constants in 'Helpers\Unity.Constants.pas',
    Unity.Sorting in 'Helpers\Unity.Sorting.pas',
    Unity.Helpers in 'Helpers\Unity.Helpers.pas',
    Unity.Types in 'Helpers\Unity.Types.pas',
    Unity.Mediator in 'Logic\Unity.Mediator.pas',
    Unity.Service in 'Logic\Unity.Service.pas',
    Unity.RestWrapper in 'Logic\Communication\Unity.RestWrapper.pas',
    Unity.Settings in 'Logic\Configuration\Unity.Settings.pas',
    Unity.EventLogger in 'Logic\Logger\Unity.EventLogger.pas',
    Unity.ThreadUtilities in 'Logic\Logger\Unity.ThreadUtilities.pas',
	Api.FreeFields in 'Model\Api.FreeFields.pas',
    Api.MetaData in 'Model\Api.MetaData.pas',
    Api.ErrorHandler in 'Model\Api.ErrorHandler.pas',
    Api.BankDetails in 'Model\Api.BankDetails.pas',
    Api.RegisteredEmails in 'Model\Api.RegisteredEmails.pas',
    Api.StatementData in 'Model\Api.StatementData.pas',
    Api.CompanyCodesList in 'Model\Requests\Api.CompanyCodesList.pas',
    Api.UserSessionAdd in 'Model\Requests\Api.UserSessionAdd.pas',
    Api.UserCompanySelection in 'Model\Requests\Api.UserCompanySelection.pas',
    Api.UserSessionLogs in 'Model\Requests\Api.UserSessionLogs.pas',
    Api.SendEmail in 'Model\Requests\Api.SendEmail.pas',
    Api.UserGeneralCommentAdd in 'Model\Requests\Api.UserGeneralCommentAdd.pas',
    Api.UserGeneralCommentUpdate in 'Model\Requests\Api.UserGeneralCommentUpdate.pas',
    Api.UserDailyCommentAdd in 'Model\Requests\Api.UserDailyCommentAdd.pas',
    Api.UserDailyCommentUpdate in 'Model\Requests\Api.UserDailyCommentUpdate.pas',
    Api.LogSentDocument in 'Model\Requests\Api.LogSentDocument.pas',
    Api.AddressBookAdd in 'Model\Requests\Api.AddressBookAdd.pas',
    Api.AddressBookUpdate in 'Model\Requests\Api.AddressBookUpdate.pas',
    Api.UserCustSnapshotList in 'Model\Requests\Api.UserCustSnapshotList.pas',
    Api.UserSessionAdded in 'Model\Responses\Api.UserSessionAdded.pas',
	Api.UpdateFreeFields in 'Model\Requests\Api.UpdateFreeFields.pas',
    Api.UserSessionChecked in 'Model\Responses\Api.UserSessionChecked.pas',
    Api.UserCompanyList in 'Model\Responses\Api.UserCompanyList.pas',
    Api.UserRatingAdd in 'Model\Requests\Api.UserRatingAdd.pas',
    Api.UserRatingUpdate in 'Model\Requests\Api.UserRatingUpdate.pas',
    Api.UserCompaniesUpdated in 'Model\Responses\Api.UserCompaniesUpdated.pas',
    Api.CustSortingOptions in 'Model\Responses\Api.CustSortingOptions.pas',
    Api.UserSessionLogsSaved in 'Model\Responses\Api.UserSessionLogsSaved.pas',
    Api.CompanyData in 'Model\Responses\Api.CompanyData.pas',
    Api.CompanyEmailsList in 'Model\Responses\Api.CompanyEmailsList.pas',
    Api.SentEmail in 'Model\Responses\Api.SentEmail.pas',
    Api.UserGeneralCommentAdded in 'Model\Responses\Api.UserGeneralCommentAdded.pas',
    Api.UserGeneralCommentUpdated in 'Model\Responses\Api.UserGeneralCommentUpdated.pas',
    Api.UserGeneralComment in 'Model\Responses\Api.UserGeneralComment.pas',
    Api.UserDailyCommentAdded in 'Model\Responses\Api.UserDailyCommentAdded.pas',
    Api.UserDailyCommentUpdated in 'Model\Responses\Api.UserDailyCommentUpdated.pas',
    Api.UserDailyCommentsList in 'Model\Responses\Api.UserDailyCommentsList.pas',
    Api.UserDailyCommentCheck in 'Model\Responses\Api.UserDailyCommentCheck.pas',
    Api.LoggedSentDocument in 'Model\Responses\Api.LoggedSentDocument.pas',
    Api.AddressBookList in 'Model\Responses\Api.AddressBookList.pas',
    Api.AddressBookItem in 'Model\Responses\Api.AddressBookItem.pas',
    Api.AddressBookAdded in 'Model\Responses\Api.AddressBookAdded.pas',
    Api.AddressBookItemDel in 'Model\Responses\Api.AddressBookItemDel.pas',
    Api.UserRating in 'Model\Responses\Api.UserRating.pas',
    Api.UserRatingAdded in 'Model\Responses\Api.UserRatingAdded.pas',
    Api.UserRatingUpdated in 'Model\Responses\Api.UserRatingUpdated.pas',
    Api.ReturnCompanies in 'Model\Responses\Api.ReturnCompanies.pas',
    Api.ReturnAccountType in 'Model\Responses\Api.ReturnAccountType.pas',
    Api.ReturnPaidInfo in 'Model\Responses\Api.ReturnPaidInfo.pas',
    Api.ReturnControlStatus in 'Model\Responses\Api.ReturnControlStatus.pas',
    Api.ReturnPersonResponsible in 'Model\Responses\Api.ReturnPersonResponsible.pas',
    Api.ReturnSalesResponsible in 'Model\Responses\Api.ReturnSalesResponsible.pas',
    Api.ReturnPaymentTerms in 'Model\Responses\Api.ReturnPaymentTerms.pas',
    Api.ReturnCustomerGroup in 'Model\Responses\Api.ReturnCustomerGroup.pas',
    Api.ReturnSsisData in 'Model\Responses\Api.ReturnSsisData.pas',
    Api.ReturnOpenItems in 'Model\Responses\Api.ReturnOpenItems.pas',
    Api.ReturnCustSnapshots in 'Model\Responses\Api.ReturnCustSnapshots.pas',
    Api.TokenGranted in 'Model\Responses\Api.TokenGranted.pas',
    Api.AddressBookUpdated in 'Model\Responses\Api.AddressBookUpdated.pas',
    Api.UserGeneralCommentCheck in 'Model\Responses\Api.UserGeneralCommentCheck.pas',
    Api.ReturnClientInfo in 'Model\Responses\Api.ReturnClientInfo.pas',
    Sync.Mailer in 'Logic\Business\Sync.Mailer.pas',
    Sync.Document in 'Logic\Business\Sync.Document.pas',
    Async.Accounts in 'Logic\Business\Async.Accounts.pas',
    Async.Utilities in 'Logic\Business\Async.Utilities.pas',
    Async.AddressBook in 'Logic\Business\Async.AddressBook.pas',
    Async.Debtors in 'Logic\Business\Async.Debtors.pas',
    Async.OpenItems in 'Logic\Business\Async.OpenItems.pas',
    Async.Comments in 'Logic\Business\Async.Comments.pas',
    Async.Documents in 'Logic\Business\Async.Documents.pas',
    Async.Companies in 'Logic\Business\Async.Companies.pas',
    Async.Mailer in 'Logic\Business\Async.Mailer.pas',
    Async.GeneralTables in 'Logic\Business\Async.GeneralTables.pas',
    View.BusyScreen in 'View\View.BusyScreen.pas',
    View.Main in 'View\View.Main.pas',
    View.About in 'View\View.About.pas',
    View.Actions in 'View\View.Actions.pas',
    View.Calendar in 'View\View.Calendar.pas',
    View.ColorPicker in 'View\View.ColorPicker.pas',
    View.EventLog in 'View\View.EventLog.pas',
    View.UserFeedback in 'View\View.UserFeedback.pas',
    View.GridFilter in 'View\View.GridFilter.pas',
    View.GridSearch in 'View\View.GridSearch.pas',
    View.MassMailer in 'View\View.MassMailer.pas',
    View.PhoneList in 'View\View.PhoneList.pas',
    View.Reports in 'View\View.Reports.pas',
    View.SendStatement in 'View\View.SendStatement.pas',
    View.Startup in 'View\View.Startup.pas',
    View.CompanyList in 'View\View.CompanyList.pas',
    View.RateApp in 'View\View.RateApp.pas';


{$R *.res}


begin

    {$WARN SYMBOL_PLATFORM OFF}
    // Microsoft Windows only
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
    var PathAppDir :=Service.Settings.DirApplication;
    var PathHomeDir:=Service.Settings.DirRoaming;
    if not Service.Settings.CheckConfigFile then
    begin
        Application.MessageBox(
            PCHar('Cannot find config.cfg. ' + TCommon.AppCaption + ' will be closed. Please contact IT support or reinstall the application.'),
            PChar(TCommon.AppCaption), MB_OK + MB_ICONERROR
        );
        ExitProcess(0);
    end;

    Service.Settings.MakeNewSessionId();
    Service.InitializeSession(
        Service.Settings.NewSessionId,
        Service.Settings.MakeNewSessionFile(Service.Settings.NewSessionId)
    );

    // ----------------------------------------------------------------
    // Clear cache directory content if previously ordered by the user.
    // ----------------------------------------------------------------
    if Service.Settings.GetStringValue(TConfigSections.ApplicationDetails, 'CLEAR_CACHE_AT_STARTUP', '') = 'yes' then
    begin
        THelpers.RemoveAllInFolder(PathAppDir + 'cache', '*.*');
        Service.Settings.SetStringValue(TConfigSections.ApplicationDetails, 'CLEAR_CACHE_AT_STARTUP', 'no');
        Service.Settings.Encode(TAppFiles.Configuration);
    end;

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
            // Note: Do place those folders in Home path.
            // -----------------------------------------------------------------
            GlobalCEFApp.PersistSessionCookies:=False; // Do not store coockies without expiry/validity date!
            GlobalCEFApp.Cache  :=PathHomeDir + 'cache';
            GlobalCEFApp.Cookies:=PathHomeDir + 'coockies';

            // -----------------------------------------------------------------------------------------------------------------
            // Set the current application directory before loading the CEF3 libraries to avoid "CEF3 binaries missing !" error.
            // -----------------------------------------------------------------------------------------------------------------
            GlobalCEFApp.SetCurrentDir:=True;

            if not(GlobalCEFApp.StartMainProcess) then
            begin

                Application.MessageBox(
                    PChar('Chromium cannot detect main thread running. Program will be closed. Please contact IT support.'),
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

    // ------------------------------------------------------------------
    // Call startup view to display splash screen and process loading of
    // various assets and/or data (including database supporting tables).
    // It will automatically handle main user window.
    // Warning! Main application window (mian form) is View.Startup, and
    // therefore the user main form (View.Main) must have taskbar icon
    // re-assigned to it, so it can act as a main window of the Windows
    // application.
    // ------------------------------------------------------------------
    StartupForm.SetSessionLog(Service.SessionLog);
    StartupForm.Show();

    Application.Run;
    UnloadService();
    GlobalCEFApp.Free();

end.

