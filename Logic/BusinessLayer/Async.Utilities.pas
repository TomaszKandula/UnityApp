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


    // --------------------
    // Callback signatures.
    // --------------------

    TCheckServerConn    = procedure(IsConnected: boolean; LastError: TLastError) of object;
    TSendUserFeedback   = procedure(LastError: TLastError) of object;
    TExcelExport        = procedure(LastError: TLastError) of object;
    TGeneralTables      = procedure(LastError: TLastError) of object;
    TCheckGivenPassword = procedure(LastError: TLastError) of object;
    TSetNewPassword     = procedure(LastError: TLastError) of object;

    IUtilities = interface(IInterface)
    ['{0B054CF4-86F7-4770-957B-3026BE491B5A}']

        // ----------------
        // Exposed methods.
        // ----------------

        procedure CheckServerConnAsync(IsConnected: boolean; Callback: TCheckServerConn);
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);
        procedure ExcelExportAsync(GroupId: string; AgeDate: string; FileName: string; Callback: TExcelExport);
        procedure GeneralTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGeneralTables; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);

    end;

    TUtilities = class(TInterfacedObject, IUtilities)
    {$TYPEINFO ON}
    public
        procedure CheckServerConnAsync(IsConnected: boolean; Callback: TCheckServerConn);
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);
        procedure ExcelExportAsync(GroupId: string; AgeDate: string; FileName: string; Callback: TExcelExport);
        procedure GeneralTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGeneralTables; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);
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
        var LastError: TLastError;
        try

            var DataBase: TDataBase:=TDataBase.Create(False);
            try

                if (not(IsConnected)) and (DataBase.Check = 0) then
                begin
                    FIsConnected:=True;
                    LastError.IsSucceeded:=True;
                    LastError.ErrorMessage:='[CheckServerConnAsync]: Connection with SQL Server database has been re-established.';
                    ThreadFileLog.Log(LastError.ErrorMessage);
                end;

                if DataBase.Check <> 0 then
                begin
                    FIsConnected:=False;
                    LastError.IsSucceeded:=True;
                    LastError.ErrorMessage:='[CheckServerConnAsync]: Connection with SQL Server database has been lost, waiting to reconnect... .';
                    ThreadFileLog.Log(LastError.ErrorMessage);
                end;

            finally
                DataBase.Free;
            end;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[CheckServerConnAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(FIsConnected, LastError);
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

        var LastError: TLastError;
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
                LastError.IsSucceeded:=True;
                LastError.ErrorMessage:='[SendFeedbackAsync]: User feedback has been sent.';
                ThreadFileLog.Log(LastError.ErrorMessage);
            end
            else
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[SendFeedbackAsync]: Cannot send email. Please contact IT Support.';
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[SendFeedbackAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
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

        var LastError: TLastError;
        try

            var Temp: TStringGrid:=TStringGrid.Create(nil);
            try
                Temp.ToExcel('Age Report', FileName, GroupId, AgeDate, SessionService.FDbConnect);
            finally
                Temp.Free;
            end;

            LastError.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[ExcelExportAsync]: Cannot execute. Error has been thrown: ' + LastError.ErrorMessage;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
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

        var LastError: TLastError;
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

            LastError.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[GeneralTablesAsync]: Cannot execute. Error has been thrown: ' + LastError.ErrorMessage;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
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

        var LastError: TLastError;
        try

            var ReHashed: boolean;
            var Settings: ISettings:=TSettings.Create;
            var Hash: string:=Settings.GetStringValue(TConfigSections.PasswordSection, 'HASH', '');

            if Hash = '' then
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[CheckGivenPassword]: Missing hash value, check settings file. Please contact IT Support.';
                ThreadFileLog.Log(LastError.ErrorMessage);
            end
            else
            begin

                LastError.IsSucceeded:=TBcrypt.CheckPassword(Password, Hash, ReHashed);

                if LastError.IsSucceeded then
                begin
                    LastError.ErrorMessage:='[CheckGivenPassword]: Administrator password has been validaded.';
                    ThreadFileLog.Log(LastError.ErrorMessage);
                end
                else
                begin
                    LastError.ErrorMessage:='[CheckGivenPassword]: Incorrect password, please re-type it and try again';
                    ThreadFileLog.Log(LastError.ErrorMessage);
                end;

            end;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[CheckGivenPassword]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
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

        var LastError: TLastError;
        try

            if (CurrentPassword = String.Empty) or (NewPassword = String.Empty) then
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='Please provide current password and new password.';
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

                    LastError.IsSucceeded:=True;
                    ThreadFileLog.Log('[SetNewPassword]: New administrator password has been setup.');

                end
                else
                begin
                    LastError.IsSucceeded:=False;
                    LastError.ErrorMessage:='Incorrect password, please re-type it and try again.';
                    ThreadFileLog.Log('[SetNewPassword]: provided current password is incorrect.');
                end;

            end;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[SetNewPassword]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
        end);

    end);

    NewTask.Start();

end;


end.

