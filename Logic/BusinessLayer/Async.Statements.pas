unit Async.Statements;

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


    TSendAccountStatement  = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;
    TSendAccountStatements = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;


    IStatements = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']
        procedure SendAccountStatement(PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatement; WaitToComplete: boolean = False);
        procedure SendAccountStatements(PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatements);
    end;


    TStatements = class(TInterfacedObject, IStatements)
    {$TYPEINFO ON}
    public
        procedure SendAccountStatement(PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatement; WaitToComplete: boolean = False);
        procedure SendAccountStatements(PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatements);
    end;


implementation


uses
    Handler.Database,
    Handler.Account,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.EventLogger,
    Sync.Documents,
    Async.Comments,
    DbModel;


// ---------------------------------------------
// Send single account statement asynchronously.
// ---------------------------------------------

procedure TStatements.SendAccountStatement(PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatement; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ProcessingItemNo:=-1;
        var CallResponse: TCallResponse;
        try

            var Settings: ISettings:=TSettings.Create();
            var Statement: IDocument:=TDocument.Create();

            Statement.Exclusions:=TArray<Integer>.Create(514, 9999); // Warning! Data should be taken from database. To be change after DB is restructured.
            Statement.MailSubject:=PayLoad.Subject + ' - ' + PayLoad.CustName + ' - ' + PayLoad.CustNumber;

            Statement.CUID       :=PayLoad.CUID;
            Statement.MailFrom   :=PayLoad.SendFrom;
            Statement.MailTo     :=PayLoad.MailTo;
            Statement.CustName   :=PayLoad.CustName;
            Statement.LBUName    :=PayLoad.LBUName;
            Statement.LBUAddress :=PayLoad.LBUAddress;
            Statement.Telephone  :=PayLoad.Telephone;
            Statement.BankDetails:=PayLoad.BankDetails;
            Statement.CustMess   :=PayLoad.Mess;
            Statement.InvFilter  :=PayLoad.InvFilter;
            Statement.BeginWith  :=PayLoad.BeginDate;
            Statement.EndWith    :=PayLoad.EndDate;

            // ----------------------------------------------------------
            // Assign source of open items and control statuses alongside
            // with theirs column references.
            // ----------------------------------------------------------

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
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''), PayLoad.IsCtrlStatus);

            if PayLoad.Layout = TDocMode.Custom then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''), PayLoad.IsCtrlStatus);

            if Statement.SendDocument(PayLoad.IsUserInCopy) then
            begin

                var FDailyCommentFields: TDailyCommentFields;
                FDailyCommentFields.CUID         :=PayLoad.CUID;
                FDailyCommentFields.Email        :=False;
                FDailyCommentFields.CallEvent    :=False;
                FDailyCommentFields.CallDuration :=0;
                FDailyCommentFields.Comment      :='New communication has been sent to the customer.';
                FDailyCommentFields.UpdateGrid   :=not PayLoad.Series;
                FDailyCommentFields.EmailReminder:=False;
                FDailyCommentFields.EventLog     :=False;
                FDailyCommentFields.ExtendComment:=True;

                // ----------------------------------------------------------------------
                // Register sent email either as manual statement or automatic statement.
                // ----------------------------------------------------------------------

                if PayLoad.Layout = TDocMode.Defined then
                begin

                    FDailyCommentFields.EmailAutoStat:=True;
                    FDailyCommentFields.EmailManuStat:=False;

                    var Comments: IComments:=TComments.Create();
                    Comments.EditDailyComment(FDailyCommentFields, nil);

                end;

                if PayLoad.Layout = TDocMode.Custom then
                begin

                    FDailyCommentFields.EmailAutoStat:=False;
                    FDailyCommentFields.EmailManuStat:=True;

                    var Comments: IComments:=TComments.Create();
                    Comments.EditDailyComment(FDailyCommentFields, nil);

                end;

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
            Callback(ProcessingItemNo, CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAll(NewTask);

end;


// ---------------------------------------
// Send account statements asynchronously.
// ---------------------------------------

procedure TStatements.SendAccountStatements(PayLoad: TAccountStatementPayLoad; Callback: TSendAccountStatements);
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

                    PayLoad.CUID       :=PayLoad.MailerList.Items[iCNT].SubItems[10]; // cuid
                    PayLoad.SendFrom   :=PayLoad.MailerList.Items[iCNT].SubItems[3];  // send from
                    PayLoad.MailTo     :=PayLoad.MailerList.Items[iCNT].SubItems[4];  // send to
                    PayLoad.CustName   :=PayLoad.MailerList.Items[iCNT].SubItems[0];  // cust name
                    PayLoad.CustNumber :=PayLoad.MailerList.Items[iCNT].SubItems[1];  // cust number
                    PayLoad.LBUName    :=PayLoad.MailerList.Items[iCNT].SubItems[5];  // lbu name
                    PayLoad.LBUAddress :=PayLoad.MailerList.Items[iCNT].SubItems[6];  // lbu address
                    PayLoad.Telephone  :=PayLoad.MailerList.Items[iCNT].SubItems[7];  // lbu phone
                    PayLoad.BankDetails:=PayLoad.MailerList.Items[iCNT].SubItems[12]; // bank html
                    PayLoad.ItemNo     :=iCNT;

                    SendAccountStatement(PayLoad, Callback, True);

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
            Callback(PayLoad.ItemNo, CallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

