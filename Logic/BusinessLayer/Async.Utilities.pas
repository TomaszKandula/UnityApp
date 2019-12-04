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
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    /// <summary>
    /// Callback signature for getting results from sending user email with feedback message.
    /// </summary>
    TSendUserFeedback = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results from exporting data grid to Excel file.
    /// </summary>
    TExcelExport = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results from updating general tables.
    /// </summary>
    TGeneralTables = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results from query basic company data.
    /// </summary>
    TGetCompanyDetails = procedure(LbuName: string; LbuAddress: string; LbuPhone: string; LbuEmail: string; BanksData: string; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results from checking supplied local administrator password.
    /// </summary>
    TCheckGivenPassword = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for getting results from setting new local administrator password.
    /// </summary>
    TSetNewPassword = procedure(CallResponse: TCallResponse) of object;


    IUtilities = interface(IInterface)
    ['{0B054CF4-86F7-4770-957B-3026BE491B5A}']

        /// <summary>
        /// Async. sending email to CI Team with user feedback.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);

        /// <summary>
        /// Allow to async. export data grid to Excel file. Requires installed Microsopft Excel 2013 or higher.
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
        /// Allow to load async. some company data like name, address, phone etc. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyDetailsAwaited(CoCode: string; Branch: string): TCompanyDetails;

        /// <summary>
        /// Allow to load async. list of emails for given CoCodes. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList);

        /// <summary>
        /// Allow to async. check provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);

        /// <summary>
        /// Allow to async. setup newly provided local administrator password that works only for given program installed on local machine.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);

        /// <summary>
        /// Allow to load async. list of company codes assigned to the current user. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetCompanyCodesAwaited(var SelectedCoCodes: TStringList);

        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetSortingOptionsAwaited(var SortingOptions: TStringList);

        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;

    end;


    TUtilities = class(TInterfacedObject, IUtilities)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Async. sending email to CI Team with user feedback.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);

        /// <summary>
        /// Allow to async. export data grid to Excel file. Requires installed Microsopft Excel 2013 or higher.
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
        /// Allow to load async. some company data like name, address, phone etc. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyDetailsAwaited(CoCode: string; Branch: string): TCompanyDetails;

        /// <summary>
        /// Allow to load async. list of emails for given CoCodes. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList);

        /// <summary>
        /// Allow to async. check provided local administrator password.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure CheckGivenPasswordAsync(Password: string; Callback: TCheckGivenPassword);

        /// <summary>
        /// Allow to async. setup newly provided local administrator password that works only for given program installed on local machine.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SetNewPasswordAsync(CurrentPassword: string; NewPassword: string; Callback: TSetNewPassword);

        /// <summary>
        /// Allow to load async. list of company codes assigned to the current user. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetCompanyCodesAwaited(var SelectedCoCodes: TStringList);

        /// <summary>
        /// Allow to load async. list of sorting options available for aging report. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetSortingOptionsAwaited(var SortingOptions: TStringList);

        /// <summary>
        /// Allow to write async. user logs to database. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveUserLogsAwaited(): TCallResponse;

    end;


implementation


uses
    Handler.Database{Legacy}, //remove
    Handler.Sql{Legacy}, //remove
    Unity.Sql{Legacy}, //remove
    Unity.Helpers,
    Unity.Settings,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Common,
    Unity.DateTimeFormats,
    Sync.Documents,
    Bcrypt,
    DbModel{Legacy}; //remove


procedure TUtilities.SendFeedbackAsync(Text: string; Callback: TSendUserFeedback); // replace with rest and move to Async.Mailer / IMailer
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var Settings: ISettings:=TSettings.Create();
            var Mail: IDocument:=TDocument.Create();

            var AppName: string:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'VALUE', '');
            var AppVer: string:=THelpers.GetBuildInfoAsString;

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
            Mail.MailCc     :=SessionService.SessionData.EmailAddress;
            Mail.MailBcc    :='';
            Mail.MailSubject:='Unity - User feedback (' + UpperCase(SessionService.SessionData.AliasName) + ')';

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


procedure TUtilities.GeneralTablesAsync(
    TableName: string; DestGrid: TStringGrid; Callback: TGeneralTables; Columns: string = '';
    Conditions: string = ''; WaitToComplete: boolean = False); // replace with REST, move to IGeneralTables
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


function TUtilities.GetCompanyDetailsAwaited(CoCode: string; Branch: string): TCompanyDetails; // replace with rest, move to ICompanies
begin

    var CompanyDetails: TCompanyDetails;
    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LbuName:    string;
        var LbuAddress: string;
        var LbuPhone:   string;
        var LbuEmail:   string;
        var BanksData:  string;

        var CallResponse: TCallResponse;
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                DataTables.Columns.Add(TCompanyData.CoName);
                DataTables.Columns.Add(TCompanyData.CoAddress);
                DataTables.Columns.Add(TCompanyData.TelephoneNumbers);
                DataTables.Columns.Add(TCompanyData.SendNoteFrom);
                DataTables.Columns.Add(TCompanyData.BankAccounts);
                DataTables.CustFilter:=TSql.WHERE + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(CoCode) + TSql._AND + TCompanyData.Branch + TSql.EQUAL + QuotedStr(Branch);
                DataTables.OpenTable(TCompanyData.CompanyData);

                if DataTables.DataSet.RecordCount = 1 then
                begin
                    CompanyDetails.LbuName   :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.CoName].Value);
                    CompanyDetails.LbuAddress:=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.CoAddress].Value);
                    CompanyDetails.LbuPhone  :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.TelephoneNumbers].Value);
                    CompanyDetails.LbuEmail  :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.SendNoteFrom].Value);
                    CompanyDetails.LbuBanks  :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.BankAccounts].Value);
                end;

                CallResponse.IsSucceeded:=True;

            except
                on E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CompanyDetails;

end;


procedure TUtilities.GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList); // replace with rest, move to ICompanies
begin

    if (not SourceList.Count > 0) or (not Assigned(TargetList)) then Exit();

    var EmailList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                try

                    DataTables.Columns.Add(TSql.DISTINCT + TCompanyData.SendNoteFrom);

                    var CoCodeList: string;
                    for var iCNT:=0 to SourceList.Count - 1 do
                    begin

                        if iCNT < (SourceList.Count - 1) then
                            CoCodeList:=CoCodeList + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(SourceList.Strings[iCNT]) + TSql._OR
                        else
                            CoCodeList:=CoCodeList + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(SourceList.Strings[iCNT]);

                    end;

                    DataTables.CustFilter:=TSql.WHERE + CoCodeList;
                    DataTables.OpenTable(TCompanyData.CompanyData);

                    while not DataTables.DataSet.EOF do
                    begin
                        EmailList.Add(DataTables.DataSet.Fields[0].Value);
                        DataTables.DataSet.MoveNext;
                     end;

                except
                    on E: Exception do
                        ThreadFileLog.Log('[GetCompanyEmailsAwaited]: Cannot execute. Error has been thrown: ' + E.Message);
                end;

            finally
                DataTables.Free();
            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        TargetList.AddStrings(EmailList);

    finally
        EmailList.Free();
    end;

end;


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


procedure TUtilities.GetCompanyCodesAwaited(var SelectedCoCodes: TStringList); // replace with rest, move to IAccounts, rename to "user company list"
begin

    var TempList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            var StringGrid:=TStringGrid.Create(nil);
            try

                DataTables.StrSQL:='select distinct CoCode, CoName from Customer.CompanyData';
                DataTables.SqlToGrid(StringGrid, DataTables.ExecSQL, False, False);

                for var iCNT:=1{Skip header} to StringGrid.RowCount - 1 do
                    TempList.Add(StringGrid.Cells[1{CoCode}, iCNT] + ' - ' + StringGrid.Cells[2{CoName}, iCNT]);

            finally
                DataTables.Free();
                StringGrid.Free();
            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        SelectedCoCodes.AddStrings(TempList);

    finally
        TempList.Free();
    end;

end;


procedure TUtilities.GetSortingOptionsAwaited(var SortingOptions: TStringList); // replace with rest, move to IAccounts, rename user sorting option
begin

    var TempList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            var StringGrid:=TStringGrid.Create(nil);
            try

                DataTables.StrSQL:='select ModeDesc from Customer.SortingOptions';
                DataTables.SqlToGrid(StringGrid, DataTables.ExecSQL, False, False);

                for var iCNT:=1{Skip header} to StringGrid.RowCount - 1 do
                    TempList.Add(StringGrid.Cells[1{ModeDesc}, iCNT]);

            finally
                DataTables.Free();
                StringGrid.Free();
            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        SortingOptions.AddStrings(TempList);

    finally
        TempList.Free();
    end;

end;


function TUtilities.SaveUserLogsAwaited(): TCallResponse; // replace with rest and move to IAccounts
begin

    var NewCallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                var Today: string:=FormatDateTime(TDateTimeFormats.DateTimeFormat, Now);

                DataTables.Columns.Add(TUnityEventLogs.UserAlias);
                DataTables.Columns.Add(TUnityEventLogs.DateTimeStamp);
                DataTables.Columns.Add(TUnityEventLogs.AppEventLog);
                DataTables.Columns.Add(TUnityEventLogs.AppName);
                DataTables.Values.Add(SessionService.SessionData.AliasName.ToUpper);
                DataTables.Values.Add(Today);
                DataTables.Values.Add(THelpers.LoadFileToStr(ThreadFileLog.LogFileName));
                DataTables.Values.Add('Unity Platform');
                DataTables.InsertInto(TUnityEventLogs.UnityEventLogs, True);

                NewCallResponse.IsSucceeded:=True;

            except
                on E: Exception do
                begin
                    NewCallResponse.IsSucceeded:=False;
                    NewCallResponse.LastMessage:='[SaveUserLogsAwaited]: ' + E.Message;
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewCallResponse;

end;


end.

