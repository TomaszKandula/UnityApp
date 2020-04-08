unit Async.Documents;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.SysUtils,
    Unity.Types,
    Unity.Records;


type


    IDocuments = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']
        /// <summary>
        /// Allow to async. send single account statemnent/reminder. It requires to pass payload with invoice data. Note that method
        /// can be executed async. without waiting to complete the task, thus it can be executed many times in parallel.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Please note that the AgeDate is required argument, this is the age report date that user put comment for.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure SendAccDocumentAsync(AgeDate: string; PayLoad: TAccDocumentPayLoad; Callback: TSendAccDocument; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to async. send many account statemnents/reminders. It requires to pass payload with invoice data. It uses SendAccountStatement
        /// method so it can be also executed async. without waiting to complete the task, thus it allows parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Please note that the AgeDate is required argument, this is the age report date that user put comment for.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure SendAccDocumentsAsync(AgeDate: string; PayLoad: TAccDocumentPayLoad; Callback: TSendAccDocuments);
        /// <summary>
        /// Allow to async. post newly sent document (statement/reminder) in the database history table. It requires to pass payload with document details.
        /// There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function LogSentDocumentAwaited(PayLoad: TSentDocument): TCallResponse;
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TDocuments = class(TInterfacedObject, IDocuments)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure SendAccDocumentAsync(AgeDate: string; PayLoad: TAccDocumentPayLoad; Callback: TSendAccDocument; WaitToComplete: boolean = False); virtual;
        procedure SendAccDocumentsAsync(AgeDate: string; PayLoad: TAccDocumentPayLoad; Callback: TSendAccDocuments); virtual;
        function LogSentDocumentAwaited(PayLoad: TSentDocument): TCallResponse; virtual;
    end;


implementation


uses
    System.Classes,
    System.Threading,
    System.Generics.Collections,
    REST.Types,
    REST.Json,
    Unity.Constants,
    Unity.Enums,
    Unity.Helpers,
    Unity.Service,
    Api.LogSentDocument,
    Api.LoggedSentDocument,
    Sync.Document,
    Async.Comments;


constructor TDocuments.Create();
begin
end;


destructor TDocuments.Destroy();
begin
    inherited;
end;


procedure TDocuments.SendAccDocumentAsync(AgeDate: string; PayLoad: TAccDocumentPayLoad; Callback: TSendAccDocument; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ProcessingItemNo:=-1;
        var CallResponse: TCallResponse;
        try

            var Statement: IDocument:=TDocument.Create();
            Statement.MailSubject   :=PayLoad.Subject + ' - ' + PayLoad.CustName + ' - ' + PayLoad.CustNumber.ToString();
            Statement.Exclusions    :=PayLoad.Exclusions;
            Statement.MailFrom      :=PayLoad.SendFrom;
            Statement.MailTo        :=PayLoad.MailTo;
            Statement.SourceDBName  :=PayLoad.SourceDBName;
            Statement.CustNumber    :=PayLoad.CustNumber;
            Statement.CustName      :=PayLoad.CustName;
            Statement.LBUName       :=PayLoad.LBUName;
            Statement.LBUAddress    :=PayLoad.LBUAddress;
            Statement.Telephone     :=PayLoad.Telephone;
            Statement.BankDetails   :=PayLoad.BankDetails;
            Statement.CustMessage   :=PayLoad.Message;
            Statement.InvFilter     :=PayLoad.InvFilter;
            Statement.BeginWith     :=PayLoad.BeginDate;
            Statement.EndWith       :=PayLoad.EndDate;
            Statement.OpenItems     :=PayLoad.OpenItems;
            Statement.OpenItemsRefs :=PayLoad.OpenItemsRefs;
            Statement.ControlStatus :=PayLoad.ControlStatus;
            Statement.CtrlStatusRefs:=PayLoad.CtrlStatusRefs;

            // ------------------------------------------------------
            // Load either fixed template or customizable template.
            // Where param name "FLayout":
            //   - maDefined for fully pre-defined template.
            //   - maCustom for customised template.
            // It requires FSalut, FMess and FSubject to be provided.
            // ------------------------------------------------------
            if PayLoad.Layout = TDocMode.Defined then
                Statement.HTMLLayout:=Statement.LoadTemplate(
                    Service.Settings.DirLayouts + Service.Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''),
                    PayLoad.IsCtrlStatus
                );

            if PayLoad.Layout = TDocMode.Custom then
                Statement.HTMLLayout:=Statement.LoadTemplate(
                    Service.Settings.DirLayouts + Service.Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''),
                    PayLoad.IsCtrlStatus
                );

            if Statement.SendDocument(PayLoad.IsUserInCopy) then
            begin

                var DocumentData: TSentDocument;
                DocumentData.SourceDBName      :=PayLoad.SourceDBName;
                DocumentData.ReportedCustomer  :=PayLoad.CustNumber;
                DocumentData.ReportedAggrAmount:=Statement.TotalAmountAggr;
                DocumentData.ReportedAgeDate   :=AgeDate;
                DocumentData.PreservedEmail    :=Statement.MailBody;
                DocumentData.DocumentType      :=Statement.DocumentType;

                LogSentDocumentAwaited(DocumentData);

                var Comments: IComments:=TComments.Create();
                Comments.UpdateDailyCommentAwaited(PayLoad.SourceDBName, PayLoad.CustNumber, AgeDate);

                // -------------------------------------------------------
                // We send either single email (customized by the user) or
                // executed by mass mailer (multiple emails).
                // -------------------------------------------------------
                case PayLoad.Series of

                    True:
                    begin
                        ProcessingItemNo:=PayLoad.ItemNo;
                        CallResponse.IsSucceeded:=True;
                        CallResponse.LastMessage:='Item processed.';
                    end;

                    False:
                    begin
                        CallResponse.IsSucceeded:=True;
                        CallResponse.LastMessage:='Account Statement has been sent successfully!';
                    end;

                end;

            end
            else if not(PayLoad.Series) then
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendAccountStatement]: Account Statement cannot be sent. Please contact IT support.';
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendAccountStatement]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ProcessingItemNo, CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAll(NewTask);

end;


procedure TDocuments.SendAccDocumentsAsync(AgeDate: string; PayLoad: TAccDocumentPayLoad; Callback: TSendAccDocuments);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        PayLoad.Series:=True;
        PayLoad.ItemNo:=0;

        var CallResponse: TCallResponse;
        try

            for var iCNT:=0 to PayLoad.MailerList.Items.Count - 1 do
            begin

                if PayLoad.MailerList.Items[iCNT].SubItems[4] <> 'Not found!' then
                begin

                    var EmailsTo:=PayLoad.MailerList.Items[iCNT].SubItems[4];
                    var ListEmailsTo:=THelpers.StringToArray(EmailsTo, ';');

                    PayLoad.CustNumber  :=(PayLoad.MailerList.Items[iCNT].SubItems[0]).ToInteger();
                    PayLoad.CustName    :=PayLoad.MailerList.Items[iCNT].SubItems[1];
                    PayLoad.SendFrom    :=PayLoad.MailerList.Items[iCNT].SubItems[3];
                    PayLoad.MailTo      :=ListEmailsTo;
                    PayLoad.SourceDBName:=PayLoad.MailerList.Items[iCNT].SubItems[5];
                    PayLoad.BankDetails :=PayLoad.MailerList.Items[iCNT].SubItems[6];
                    PayLoad.LBUName     :=PayLoad.MailerList.Items[iCNT].SubItems[7];
                    PayLoad.LBUAddress  :=PayLoad.MailerList.Items[iCNT].SubItems[8];
                    PayLoad.Telephone   :=PayLoad.MailerList.Items[iCNT].SubItems[9];
                    PayLoad.Exclusions  :=THelpers.StringToArrayInt(PayLoad.MailerList.Items[iCNT].SubItems[10], ';');
                    PayLoad.ItemNo      :=iCNT;

                    SendAccDocumentAsync(AgeDate, PayLoad, Callback, True);

                end;

            end;

            CallResponse.IsSucceeded:=True;
            CallResponse.LastMessage:='Processed.';
            Service.Logger.Log('[SendAccountStatements]: Listed items have been processed successfully!');

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendAccountStatements]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(PayLoad.ItemNo, CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TDocuments.LogSentDocumentAwaited(PayLoad: TSentDocument): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'documents/'
            + PayLoad.SourceDBName
            + '/'
            + PayLoad.DocumentType;
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[LogSentDocumentAwaited]: Executing POST ' + Rest.ClientBaseURL);

        var LogSentDocument:=TLogSentDocument.Create();
        try

            LogSentDocument.UserAlias         :=Service.SessionData.AliasName;
            LogSentDocument.ReportedCustomer  :=PayLoad.ReportedCustomer;
            LogSentDocument.ReportedAggrAmount:=PayLoad.ReportedAggrAmount;
            LogSentDocument.ReportedAgeDate   :=PayLoad.ReportedAgeDate;
            LogSentDocument.PreservedEmail    :=PayLoad.PreservedEmail;
            LogSentDocument.DocumentType      :=PayLoad.DocumentType;

            Rest.CustomBody:=TJson.ObjectToJsonString(LogSentDocument);

        finally
            LogSentDocument.Free();
        end;

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var LoggedSentDocument:=TJson.JsonToObject<TLoggedSentDocument>(Rest.Content);
                try

                    CallResponse.IsSucceeded:=LoggedSentDocument.IsSucceeded;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[LogSentDocumentAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    LoggedSentDocument.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[LogSentDocumentAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[LogSentDocumentAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[LogSentDocumentAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[LogSentDocumentAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


end.

