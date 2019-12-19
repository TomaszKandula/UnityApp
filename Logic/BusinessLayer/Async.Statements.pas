unit Async.Statements;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.SysUtils,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for returning information from sending single account statement.
    /// </summary>
    TSendAccountStatement = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for returning information from sending many account statements.
    /// </summary>
    TSendAccountStatements = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;


    IStatements = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']

        /// <summary>
        /// Allow to async. send single account statemnent. It requires to pass payload with invoice data. Note that method
        /// can be executed async. without waiting to complete the task, thus it can be executed many times in parallel.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Please note that the AgeDate is required argument, this is the age report date that user put comment for.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure SendAccountStatement(AgeDate: string; PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatement; WaitToComplete: boolean = False);

        /// <summary>
        /// Allow to async. send many account statemnents. It requires to pass payload with invoice data. It uses SendAccountStatement
        /// method so it can be also executed async. without waiting to complete the task, thus it allows parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Please note that the AgeDate is required argument, this is the age report date that user put comment for.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure SendAccountStatements(AgeDate: string; PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatements);

    end;


    TStatements = class(TInterfacedObject, IStatements)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Allow to async. send single account statemnent. It requires to pass payload with invoice data. Note that method
        /// can be executed async. without waiting to complete the task, thus it can be executed many times in parallel.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Please note that the AgeDate is required argument, this is the age report date that user put comment for.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure SendAccountStatement(AgeDate: string; PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatement; WaitToComplete: boolean = False);

        /// <summary>
        /// Allow to async. send many account statemnents. It requires to pass payload with invoice data. It uses SendAccountStatement
        /// method so it can be also executed async. without waiting to complete the task, thus it allows parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Please note that the AgeDate is required argument, this is the age report date that user put comment for.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure SendAccountStatements(AgeDate: string; PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatements);

    end;


implementation


uses
    System.Classes,
    System.Threading,
    System.Generics.Collections,
    Unity.Enums,
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Sync.Documents,
    Async.Comments;


procedure TStatements.SendAccountStatement(AgeDate: string; PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatement; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ProcessingItemNo:=-1;
        var CallResponse: TCallResponse;
        try

            var Settings: ISettings:=TSettings.Create();
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
                    Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''),
                    PayLoad.IsCtrlStatus
                );

            if PayLoad.Layout = TDocMode.Custom then
                Statement.HTMLLayout:=Statement.LoadTemplate(
                    Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''),
                    PayLoad.IsCtrlStatus
                );

            if Statement.SendDocument(PayLoad.IsUserInCopy) then
            begin

                // save statement details in history table
                // ...

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
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendAccountStatement]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
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


procedure TStatements.SendAccountStatements(AgeDate: string; PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatements);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        PayLoad.Series:=True;
        PayLoad.ItemNo:=0;

        var CallResponse: TCallResponse;
        try

            for var iCNT: integer:=0 to PayLoad.MailerList.Items.Count - 1 do
            begin

                if PayLoad.MailerList.Items[iCNT].SubItems[4] <> 'Not found!' then
                begin

//                    PayLoad.CUID       :=PayLoad.MailerList.Items[iCNT].SubItems[10]; // cuid
//                    PayLoad.SendFrom   :=PayLoad.MailerList.Items[iCNT].SubItems[3];  // send from
//                    PayLoad.MailTo     :=TArray<string>.Create(PayLoad.MailerList.Items[iCNT].SubItems[4]);  // send to
//                    PayLoad.CustName   :=PayLoad.MailerList.Items[iCNT].SubItems[0];  // cust name
//                    PayLoad.CustNumber :=PayLoad.MailerList.Items[iCNT].SubItems[1];  // cust number
//                    PayLoad.LBUName    :=PayLoad.MailerList.Items[iCNT].SubItems[5];  // lbu name
//                    PayLoad.LBUAddress :=PayLoad.MailerList.Items[iCNT].SubItems[6];  // lbu address
//                    PayLoad.Telephone  :=PayLoad.MailerList.Items[iCNT].SubItems[7];  // lbu phone
//                    PayLoad.BankDetails:=PayLoad.MailerList.Items[iCNT].SubItems[12]; // bank html
//                    PayLoad.ItemNo     :=iCNT;

                    SendAccountStatement(AgeDate, PayLoad, Callback, True);

                end;

            end;

            CallResponse.IsSucceeded:=True;
            CallResponse.LastMessage:='Processed.';
            ThreadFileLog.Log('[SendAccountStatements]: Listed items have been processed successfully!');

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendAccountStatements]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(PayLoad.ItemNo, CallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

