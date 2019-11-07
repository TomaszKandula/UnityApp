unit Async.Utilities;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    System.Diagnostics,
    System.Win.ComObj,
    System.SyncObjs,
    System.Threading,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.ComCtrls,
    Vcl.Dialogs,
    Data.Win.ADODB,
    Data.DB,
    Handler.Sql,
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    /// <summary>
    /// Callback signature (delegate) for getting results for checking SQL connection.
    /// </summary>
    TCheckServerConn = procedure(IsConnected: boolean; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for getting results from sending user email with feedback message.
    /// </summary>
    TSendUserFeedback = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for getting results from exporting data grid to Excel file.
    /// </summary>
    TExcelExport = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for getting results from updating general tables.
    /// </summary>
    TGeneralTables = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for getting results from checking supplied local administrator password.
    /// </summary>
    TCheckGivenPassword = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for getting results from setting new local administrator password.
    /// </summary>
    TSetNewPassword = procedure(CallResponse: TCallResponse) of object;


    IUtilities = interface(IInterface)
    ['{0B054CF4-86F7-4770-957B-3026BE491B5A}']

        /// <summary>
        /// Async. checking connection with SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckServerConnAsync(IsConnected: boolean; Callback: TCheckServerConn);

        /// <summary>
        /// Async. sending email to CI Team with user feedback.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);

        /// <summary>
        /// Allow to async. export data grid to Excel file.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ExcelExportAsync(GroupId: string; AgeDate: string; FileName: string; Callback: TExcelExport);

        /// <summary>
        /// Allow to async. load general tables to provided TStringGrids. This method can be executed without waiting to complete
        /// the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GeneralTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGeneralTables; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);

        /// <summary>
        /// Allow to async. check provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);

        /// <summary>
        /// Allow to async. setup newly provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);

    end;


    TUtilities = class(TInterfacedObject, IUtilities)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Async. checking connection with SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckServerConnAsync(IsConnected: boolean; Callback: TCheckServerConn);

        /// <summary>
        /// Async. sending email to CI Team with user feedback.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);

        /// <summary>
        /// Allow to async. export data grid to Excel file.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ExcelExportAsync(GroupId: string; AgeDate: string; FileName: string; Callback: TExcelExport);

        /// <summary>
        /// Allow to async. load general tables to provided TStringGrids. This method can be executed without waiting to complete
        /// the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GeneralTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGeneralTables; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);

        /// <summary>
        /// Allow to async. check provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);

        /// <summary>
        /// Allow to async. setup newly provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);

    end;


implementation


uses
    Handler.Database,
    Handler.Account,
    Unity.Helpers,
    Unity.Messaging,
    Unity.Settings,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Common,
    Unity.Utilities,
    Sync.Documents,
    Bcrypt,
    DbModel;


// ------------------------------------
// Check connection with SQL Server
// *Remove when SQL is replaced by API.
// ------------------------------------

procedure TUtilities.CheckServerConnAsync(IsConnected: boolean; Callback: TCheckServerConn);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var FIsConnected: boolean;
        var CallResponse: TCallResponse;
        try

            var DataBase: TDataBase:=TDataBase.Create(False);
            try

                if (not(IsConnected)) and (DataBase.Check = 0) then
                begin
                    FIsConnected:=True;
                    CallResponse.IsSucceeded:=True;
                    CallResponse.LastMessage:='[CheckServerConnAsync]: Connection with SQL Server database has been re-established.';
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

                if DataBase.Check <> 0 then
                begin
                    FIsConnected:=False;
                    CallResponse.IsSucceeded:=True;
                    CallResponse.LastMessage:='[CheckServerConnAsync]: Connection with SQL Server database has been lost, waiting to reconnect... .';
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            finally
                DataBase.Free;
            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckServerConnAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(FIsConnected, CallResponse);
        end);

    end);

    NewTask.Start;

end;


// -------------------------------
// Send user feedback to predefine
// email address in settings file.
// -------------------------------

procedure TUtilities.SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var Settings: ISettings:=TSettings.Create();
            var Mail: IDocument:=TDocument.Create();

            var AppName: string:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'VALUE', '');
            var AppVer: string:=TCore.GetBuildInfoAsString;

            // --------------------------
            // Get and set email details.
            // --------------------------

            if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM  then
            begin
                Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'FROM',     '');
                Mail.MailTo :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'TO',       '');
                Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'REPLY-TO', '');
            end;

            if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then
            begin
                Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'FROM',     '');
                Mail.MailTo :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'TO',       '');
                Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'REPLY-TO', '');
            end;

            Mail.MailFrom   :=Mail.XMailer;
            Mail.MailCc     :=SessionService.SessionUser + '@' + Settings.GetStringValue(TConfigSections.ApplicationDetails, 'MAIL_DOMAIN', '');
            Mail.MailBcc    :='';
            Mail.MailSubject:='Unity - User feedback (' + UpperCase(SessionService.SessionUser) + ')';

            // ----------------------------------
            // Plain text to HTML using template.
            // ----------------------------------

            var Transfer: string:=Text;
            Transfer:=StringReplace(Transfer, TChars.CRLF, '<br>', [rfReplaceAll]);

            var HTMLBody: string:=Mail.LoadTemplate(Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE4', ''), False);
            HTMLBody:=StringReplace(HTMLBody, '{TEXT_HOLER}',  Transfer,       [rfReplaceAll]);
            HTMLBody:=StringReplace(HTMLBody, '{APPNAME}',     AppName,        [rfReplaceAll]);
            HTMLBody:=StringReplace(HTMLBody, '{BUILD}',       AppVer,         [rfReplaceAll]);
            HTMLBody:=StringReplace(HTMLBody, '{REPORT_DATE}', DateToStr(Now), [rfReplaceAll]);
            HTMLBody:=StringReplace(HTMLBody, '{REPORT_TIME}', TimeToStr(Now), [rfReplaceAll]);

            Mail.MailBody:=HTMLBody;

            if Mail.SendNow then
            begin
                CallResponse.IsSucceeded:=True;
                CallResponse.LastMessage:='[SendFeedbackAsync]: User feedback has been sent.';
                ThreadFileLog.Log(CallResponse.LastMessage);
            end
            else
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendFeedbackAsync]: Cannot send email. Please contact IT Support.';
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendFeedbackAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start;

end;


// --------------------------------------
// Generate Excel report asynchronously
// to not to block application usability.
// --------------------------------------

procedure TUtilities.ExcelExportAsync(GroupId: string; AgeDate: string; FileName: string; Callback: TExcelExport);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var Temp: TStringGrid:=TStringGrid.Create(nil);
            try
                Temp.ToExcel('Age Report', FileName, GroupId, AgeDate, SessionService.FDbConnect);
            finally
                Temp.Free;
            end;

            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ExcelExportAsync]: Cannot execute. Error has been thrown: ' + CallResponse.LastMessage;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start;

end;


// --------------------------------
// Load async. given general table.
// --------------------------------

procedure TUtilities.GeneralTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGeneralTables; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                DataTables.CleanUp;

                if not(string.IsNullOrEmpty(Columns)) then
                    DataTables.Columns.Text:=Columns;

                if not(string.IsNullOrEmpty(Conditions)) then
                    DataTables.CustFilter:=Conditions;

                if DataTables.OpenTable(TableName) then
                    DataTables.SqlToGrid(DestGrid, DataTables.DataSet, False, True);

            finally
                DataTables.Free;
            end;

            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GeneralTablesAsync]: Cannot execute. Error has been thrown: ' + CallResponse.LastMessage;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


// ------------------------
// Check supplied password.
// ------------------------

procedure TUtilities.CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var ReHashed: boolean;
            var Settings: ISettings:=TSettings.Create;
            var Hash: string:=Settings.GetStringValue(TConfigSections.PasswordSection, 'HASH', '');

            if Hash = '' then
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckGivenPassword]: Missing hash value, check settings file. Please contact IT Support.';
                ThreadFileLog.Log(CallResponse.LastMessage);
            end
            else
            begin

                CallResponse.IsSucceeded:=TBcrypt.CheckPassword(Password, Hash, ReHashed);

                if CallResponse.IsSucceeded then
                begin
                    CallResponse.LastMessage:='[CheckGivenPassword]: Administrator password has been validaded.';
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end
                else
                begin
                    CallResponse.LastMessage:='[CheckGivenPassword]: Incorrect password, please re-type it and try again';
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[CheckGivenPassword]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


// -------------------------------------------------------------
// Set new administrator password. This is local admin password,
// that works only for given program installed on local machine.
// -------------------------------------------------------------

procedure TUtilities.SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            if (CurrentPassword = String.Empty) or (NewPassword = String.Empty) then
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='Please provide current password and new password.';
                ThreadFileLog.Log('[SetNewPassword]: User have not provided current password/new password.');
            end
            else
            begin

                // -----------------------
                // Check current password.
                // -----------------------

                var ReHashed: boolean;
                var Settings: ISettings:=TSettings.Create;
                var Hash: string:=Settings.GetStringValue(TConfigSections.PasswordSection, 'HASH', '');

                if TBcrypt.CheckPassword(CurrentPassword, Hash, ReHashed) then
                begin

                    // ------------------------------
                    // Setup newly provided password.
                    // ------------------------------

                    var HashPasswd: string:=TBcrypt.HashPassword(NewPassword);

                    Settings.SetStringValue(TConfigSections.PasswordSection, 'HASH', HashPasswd);
                    Settings.Encode(TAppFiles.Configuration);

                    CallResponse.IsSucceeded:=True;
                    ThreadFileLog.Log('[SetNewPassword]: New administrator password has been setup.');

                end
                else
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='Incorrect password, please re-type it and try again.';
                    ThreadFileLog.Log('[SetNewPassword]: provided current password is incorrect.');
                end;

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SetNewPassword]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

