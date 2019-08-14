unit Async.Statements;

// ----------------------------------
// Application logic, business layer.
// Can be referenced by anyone.
// Cannot hold references to View.
// ----------------------------------

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


    IStatements = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']
        procedure SendAccountStatement(Fields: TSendAccountStatementFields; WaitToComplete: boolean = False);
        procedure SendAccountStatements(Fields: TSendAccountStatementFields);
    end;


    TStatements = class(TInterfacedObject, IStatements)
    {$TYPEINFO ON}
    public
        procedure SendAccountStatement(Fields: TSendAccountStatementFields; WaitToComplete: boolean = False);
        procedure SendAccountStatements(Fields: TSendAccountStatementFields);
    end;


implementation


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Database,
    Handler.Account,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Sync.Documents,
    Async.Comments,
    DbModel,
    AgeView,
    Transactions;


// ------------------------------------
//
// ------------------------------------

procedure TStatements.SendAccountStatement(Fields: TSendAccountStatementFields; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Settings: ISettings:=TSettings.Create;
        var Statement: IDocument:=TDocument.Create;

        Statement.CUID       :=Fields.CUID;
        Statement.MailFrom   :=Fields.SendFrom;
        Statement.MailTo     :=Fields.MailTo;
        Statement.CustName   :=Fields.CustName;
        Statement.LBUName    :=Fields.LBUName;
        Statement.LBUAddress :=Fields.LBUAddress;
        Statement.Telephone  :=Fields.Telephone;
        Statement.BankDetails:=Fields.BankDetails;
        Statement.CustMess   :=Fields.Mess;
        Statement.OpenItems  :=Fields.OpenItems;
        Statement.InvFilter  :=Fields.InvFilter;
        Statement.BeginWith  :=Fields.BeginDate;
        Statement.EndWith    :=Fields.EndDate;

        // Warning! Data should be taken from database. To be change after DB is restructured.
        Statement.Exclusions:=TArray<Integer>.Create(514, 9999);

        Statement.MailSubject:=Fields.Subject + ' - ' + Fields.CustName + ' - ' + Fields.CustNumber;

        // ------------------------------------------------------
        // Load either fixed template or customizable template.
        // Where param name "FLayout":
        //   - maDefined for fully pre-defined template.
        //   - maCustom for customised template.
        // It requires FSalut, FMess and FSubject to be provided.
        // ------------------------------------------------------

        if Fields.Layout = TDocMode.Defined then
            Statement.HTMLLayout:=Statement.LoadTemplate(Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''));

        if Fields.Layout = TDocMode.Custom then
            Statement.HTMLLayout:=Statement.LoadTemplate(Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''));

        if Statement.SendDocument then
        begin

            var FDailyCommentFields: TDailyCommentFields;
            FDailyCommentFields.CUID         :=Fields.CUID;
            FDailyCommentFields.Email        :=False;
            FDailyCommentFields.CallEvent    :=False;
            FDailyCommentFields.CallDuration :=0;
            FDailyCommentFields.Comment      :='New communication has been sent to the customer.';
            FDailyCommentFields.UpdateGrid   :=not Fields.Series;
            FDailyCommentFields.EmailReminder:=False;
            FDailyCommentFields.EventLog     :=False;
            FDailyCommentFields.ExtendComment:=True;

            /// <summary>
            /// Register sent email either as manual statement or automatic statement.
            /// </summary>

            if Fields.Layout = TDocMode.Defined then
            begin

                FDailyCommentFields.EmailAutoStat:=True;
                FDailyCommentFields.EmailManuStat:=False;

                var Comments: IComments:=TComments.Create();
                Comments.EditDailyComment(FDailyCommentFields);

            end;

            if Fields.Layout = TDocMode.Custom then
            begin

                FDailyCommentFields.EmailAutoStat:=False;
                FDailyCommentFields.EmailManuStat:=True;

                var Comments: IComments:=TComments.Create();
                Comments.EditDailyComment(FDailyCommentFields);

            end;

            /// <remarks>
            /// Either single email (manual by user) or executed by mass mailer (multiple emails).
            /// </remarks>

            if Fields.Series then
                THelpers.ExecMessage(False, TMessaging.TWParams.MailerReportItem, Fields.ItemNo.ToString, MainForm)
            else
                THelpers.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Account Statement has been sent successfully!', MainForm);

        end
        else if not(Fields.Series) then
            THelpers.ExecMessage(False, TMessaging.TWParams.MessageError, 'Account Statement cannot be sent. Please contact IT support.', MainForm)

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAll(NewTask);

end;


// ------------------------------------
//
// ------------------------------------

procedure TStatements.SendAccountStatements(Fields: TSendAccountStatementFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        Fields.Series:=True;
        Fields.ItemNo:=0;

        for var iCNT: integer:=0 to Fields.MailerList.Items.Count - 1 do
        begin

            if Fields.MailerList.Items[iCNT].SubItems[4] <> 'Not found!' then
            begin

                Fields.CUID       :=Fields.MailerList.Items[iCNT].SubItems[10]; // cuid
                Fields.SendFrom   :=Fields.MailerList.Items[iCNT].SubItems[3];  // send from
                Fields.MailTo     :=Fields.MailerList.Items[iCNT].SubItems[4];  // send to
                Fields.CustName   :=Fields.MailerList.Items[iCNT].SubItems[0];  // cust name
                Fields.CustNumber :=Fields.MailerList.Items[iCNT].SubItems[1];  // cust number
                Fields.LBUName    :=Fields.MailerList.Items[iCNT].SubItems[5];  // lbu name
                Fields.LBUAddress :=Fields.MailerList.Items[iCNT].SubItems[6];  // lbu address
                Fields.Telephone  :=Fields.MailerList.Items[iCNT].SubItems[7];  // lbu phone
                Fields.BankDetails:=Fields.MailerList.Items[iCNT].SubItems[12]; // bank html
                Fields.ItemNo:=iCNT;

                SendAccountStatement(Fields, True);

            end;

        end;

        THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);

    end);

    NewTask.Start();

end;


end.

