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
    Unity.Interposer,
    Unity.Statics,
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
    Unity.Settings,
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
            Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''));

        if Fields.Layout = TDocMode.Custom then
            Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''));

        if Statement.SendDocument then
        begin

            var DailyCommentFields: TDailyCommentFields;
            DailyCommentFields.CUID         :=Fields.CUID;
            DailyCommentFields.Email        :=False;
            DailyCommentFields.CallEvent    :=False;
            DailyCommentFields.CallDuration :=0;
            DailyCommentFields.Comment      :='New communication has been sent to the customer.';
            DailyCommentFields.UpdateGrid   :=not Fields.Series;
            DailyCommentFields.EmailReminder:=False;
            DailyCommentFields.EventLog     :=False;
            DailyCommentFields.ExtendComment:=True;

            /// <summary>
            /// Register sent email either as manual statement or automatic statement.
            /// </summary>

            if Fields.Layout = TDocMode.Defined then
            begin

                DailyCommentFields.EmailAutoStat:=True;
                DailyCommentFields.EmailManuStat:=False;

                var Comments: IComments:=TComments.Create();
                Comments.EditDailyComment(DailyCommentFields);

            end;

            if Fields.Layout = TDocMode.Custom then
            begin

                DailyCommentFields.EmailAutoStat:=False;
                DailyCommentFields.EmailManuStat:=True;

                var Comments: IComments:=TComments.Create();
                Comments.EditDailyComment(DailyCommentFields);

            end;

            /// <remarks>
            /// Either single email (manual by user) or executed by mass mailer (multiple emails).
            /// </remarks>

            if Fields.Series then
                MainForm.ExecMessage(False, TMessaging.TWParams.MailerReportItem, Fields.ItemNo.ToString)
            else
                MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Account Statement has been sent successfully!');

        end
        else if not(Fields.Series) then
            MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Account Statement cannot be sent. Please contact IT support.')

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

        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);

    end);

    NewTask.Start();

end;


end.

