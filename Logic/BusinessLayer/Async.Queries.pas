unit Async.Queries;

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
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


//type


//    IQueries = interface(IInterface)
//    ['{1F7A4D27-E3C0-42B6-AC36-BABEC2A0F97D}']
//        procedure UpdateQmsViewFsc(Source: TStringGrid);
//        procedure UpdateQmsViewLbu(Source: TStringGrid);
//        procedure ShowItemDetails(ItemId: integer; FscView: boolean);
//        procedure InitializeQms;
//        procedure UpdateStatus(DbItemId: integer; Status: string; Grid: TStringGrid; FscView: boolean);
//        procedure ApproveQuery(DbItemId: integer; FscView: boolean);
//        procedure RejectQuery(DbItemId: integer; FscView: boolean);
//    end;
//
//
//    TQueries = class(TInterfacedObject, IQueries)
//    {$TYPEINFO ON}
//    private
//
//    public
//        procedure UpdateQmsViewFsc(Source: TStringGrid);
//        procedure UpdateQmsViewLbu(Source: TStringGrid);
//        procedure ShowItemDetails(ItemId: integer; FscView: boolean);
//        procedure InitializeQms;
//        procedure UpdateStatus(DbItemId: integer; Status: string; Grid: TStringGrid; FscView: boolean);
//        procedure ApproveQuery(DbItemId: integer; FscView: boolean);
//        procedure RejectQuery(DbItemId: integer; FscView: boolean);
//    end;


implementation


//uses
//    View.Main,           // remove!
//    View.Actions,        // remove!
//    Handler.Database,
//    Unity.Sql,
//    Unity.Chars,
//    Unity.Helpers,
//    Unity.Settings,
//    Unity.SessionService,
//    Sync.Documents,
//    DbModel;


//procedure TQueries.UpdateQmsViewFsc(Source: TStringGrid); {refactor / async}
//begin
//
//    var Tables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
//    try
//        Tables.StrSQL:=TSql.SELECT                     +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.Id          + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.InvoNo      + TChars.COMMA +
//                           TCurrencies.Currencies + TChars.POINT + TCurrencies.Iso     + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.LogType     + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.QueryStatus + TChars.COMMA +
//                           TQmsReasons.QmsReasons + TChars.POINT + TQmsLog.QueryReason + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.Receiver    + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.UserAlias   + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.Stamp       + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.QueryUid    +
//                       TSql.FROM                  +
//                           TQmsLog.QmsLog         +
//                       TSql.LEFT_JOIN             +
//                           TQmsReasons.QmsReasons +
//                       TSql._ON                   +
//                           TQmsReasons.QmsReasons +
//                       TChars.POINT              +
//                           TQmsReasons.Id         +
//                       TSql.EQUAL                 +
//                           TQmsLog.QmsLog         +
//                       TChars.POINT              +
//                           TQmsLog.QueryReason    +
//                       TSql.LEFT_JOIN             +
//                           TCurrencies.Currencies +
//                       TSql._ON                   +
//                           TCurrencies.Currencies +
//                       TChars.POINT              +
//                           TCurrencies.Id         +
//                       TSql.EQUAL                 +
//                           TQmsLog.QmsLog         +
//                       TChars.POINT              +
//                           TQmsLog.ISO;
//        Tables.SqlToGrid(Source, Tables.ExecSQL, False, True);
//        Source.SetColWidth(10, 20, 400);
//        Source.SetRowHeight(Source.sgRowHeight, 25);
//    finally
//        Tables.Free;
//    end;
//
//end;
//
//
//procedure TQueries.UpdateQmsViewLbu(Source: TStringGrid); {refactor / async}
//begin
//
//    var Tables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
//    try
//        Tables.StrSQL:=TSql.SELECT                     +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.Id          + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.InvoNo      + TChars.COMMA +
//                           TCurrencies.Currencies + TChars.POINT + TCurrencies.Iso     + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.LogType     + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.QueryStatus + TChars.COMMA +
//                           TQmsReasons.QmsReasons + TChars.POINT + TQmsLog.QueryReason + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.Receiver    + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.UserAlias   + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.Stamp       + TChars.COMMA +
//                           TQmsLog.QmsLog         + TChars.POINT + TQmsLog.QueryUid    +
//                       TSql.FROM                       +
//                           TQmsLog.QmsLog         +
//                       TSql.LEFT_JOIN                  +
//                           TQmsReasons.QmsReasons +
//                       TSql._ON                        +
//                           TQmsReasons.QmsReasons +
//                       TChars.POINT                      +
//                           TQmsReasons.Id         +
//                       TSql.EQUAL                      +
//                           TQmsLog.QmsLog         +
//                       TChars.POINT                      +
//                           TQmsLog.QueryReason    +
//                       TSql.LEFT_JOIN                  +
//                           TCurrencies.Currencies +
//                       TSql._ON                        +
//                           TCurrencies.Currencies +
//                       TChars.POINT                      +
//                           TCurrencies.Id         +
//                       TSql.EQUAL                      +
//                           TQmsLog.QmsLog         +
//                       TChars.POINT                      +
//                           TQmsLog.ISO;
//        Tables.SqlToGrid(Source, Tables.ExecSQL, False, True);
//        Source.SetColWidth(10, 20, 400);
//        Source.SetRowHeight(Source.sgRowHeight, 25);
//    finally
//        Tables.Free;
//    end;
//
//end;
//
//
//procedure TQueries.ShowItemDetails(ItemId: integer; FscView: boolean); {refactor / async}
//begin
//
//    var Tables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
//    try
//        Tables.Columns.Add(TQmsLog.OpenAm);
//        Tables.Columns.Add(TQmsLog.Am);
//        Tables.Columns.Add(TQmsLog.OpenCurAm);
//        Tables.Columns.Add(TQmsLog.CurAm);
//        Tables.Columns.Add(TQmsLog.DueDt);
//        Tables.Columns.Add(TQmsLog.ValDt);
//        Tables.Columns.Add(TQmsLog.QueryDesc);
//        Tables.Columns.Add(TQmsLog.FscComment);
//        Tables.CustFilter:=TSql.WHERE + TQmsLog.Id + TSql.EQUAL + QuotedStr(ItemId.ToString);
//        Tables.OpenTable(TQmsLog.QmsLog);
//
//        if FscView then
//        begin
//            MainForm.ValueOpAmountFsc.Caption:=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.OpenAm].Value);
//            MainForm.ValueAmountFsc.Caption  :=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.Am].Value);
//            MainForm.ValueOpAmCurrFsc.Caption:=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.OpenCurAm].Value);
//            MainForm.ValueAmCurrFsc.Caption  :=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.CurAm].Value);
//            MainForm.ValueDueDtFsc.Caption   :=Tables.DataSet.Fields[TQmsLog.DueDt].Value;
//            MainForm.ValueValDtFsc.Caption   :=Tables.DataSet.Fields[TQmsLog.ValDt].Value;
//            MainForm.FscQueryDesc.Clear;
//            MainForm.FscQueryDesc.Text       :=Tables.DataSet.Fields[TQmsLog.QueryDesc].Value;
//        end
//        else
//        begin
//            MainForm.ValueOpAmountLbu.Caption:=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.OpenAm].Value);
//            MainForm.ValueAmountLbu.Caption  :=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.Am].Value);
//            MainForm.ValueOpAmCurrLbu.Caption:=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.OpenCurAm].Value);
//            MainForm.ValueAmCurrLbu.Caption  :=FormatFloat('#,##0.00', Tables.DataSet.Fields[TQmsLog.CurAm].Value);
//            MainForm.ValueDueDtLbu.Caption   :=Tables.DataSet.Fields[TQmsLog.DueDt].Value;
//            MainForm.ValueValDtLbu.Caption   :=Tables.DataSet.Fields[TQmsLog.ValDt].Value;
//            MainForm.LbuQueryDesc.Clear;
//            MainForm.LbuQueryDesc.Text       :=Tables.DataSet.Fields[TQmsLog.QueryDesc].Value;
//        end;
//
//    finally
//        Tables.Free;
//    end;
//
//end;
//
//
//procedure TQueries.InitializeQms;
//begin
//    MainForm.ValueOpAmountFsc.Caption:='0,00';
//    MainForm.ValueAmountFsc.Caption  :='0,00';
//    MainForm.ValueOpAmCurrFsc.Caption:='0,00';
//    MainForm.ValueAmCurrFsc.Caption  :='0,00';
//    MainForm.ValueDueDtFsc.Caption   :='n/a';
//    MainForm.ValueValDtFsc.Caption   :='n/a';
//    MainForm.FscQueryDesc.Clear;
//    MainForm.ValueOpAmountLbu.Caption:='0,00';
//    MainForm.ValueAmountLbu.Caption  :='0,00';
//    MainForm.ValueOpAmCurrLbu.Caption:='0,00';
//    MainForm.ValueAmCurrLbu.Caption  :='0,00';
//    MainForm.ValueDueDtLbu.Caption   :='n/a';
//    MainForm.ValueValDtLbu.Caption   :='n/a';
//    MainForm.LbuQueryDesc.Clear;
//end;
//
//
//procedure TQueries.UpdateStatus(DbItemId: integer; Status: string; Grid: TStringGrid; FscView: boolean); {refactor}
//begin
//
//    // Update database
//    var Tables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
//    try
//        var Conditions: string:=TQmsLog.Id + TSql.EQUAL + QuotedStr(DbItemId.ToString);
//        Tables.Columns.Add(TQmsLog.QueryStatus);
//        Tables.Values.Add(Status);
//        Tables.UpdateRecord(TQmsLog.QmsLog, True, Conditions);
//    finally
//        Tables.Free;
//    end;
//
//    // Update grids
//    Grid.Cells[Grid.ReturnColumn(TQmsLog.QueryStatus, 1, 1), Grid.Row]:=Status;
//    var QueryReason: string:=Grid.Cells[Grid.ReturnColumn(TQmsLog.QueryReason, 1, 1), Grid.Row];
//    var LbuEmail:    string:=Grid.Cells[Grid.ReturnColumn(TQmsLog.Receiver, 1, 1), Grid.Row];
//    var QueryUid:    string:=Grid.Cells[Grid.ReturnColumn(TQmsLog.QueryUid, 1, 1), Grid.Row];
//
//    // Send an email
//    var Settings: ISettings:=TSettings.Create;
//    var Mailer: IDocument:=TDocument.Create;
//
//    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM  then
//    begin
//        Mailer.XMailer:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'FROM', '');
//        Mailer.MailRt :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'REPLY-TO', '');
//    end;
//
//    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then
//    begin
//        Mailer.XMailer:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'FROM', '');
//        Mailer.MailRt :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'REPLY-TO', '');
//    end;
//
//    var InvoiceNo: string:=Grid.Cells[Grid.ReturnColumn(TQmsLog.InvoNo, 1, 1) , Grid.Row];
//
//    var Comment: string;
//
//    if FscView then Comment:=MainForm.FSCComment.Text;
//    if not FscView then Comment:=MainForm.LbuComment.Text;
//
//    Mailer.MailFrom   :=Mailer.XMailer;
//    Mailer.MailTo     :=LbuEmail;
//    Mailer.MailCc     :=SessionService.SessionUser + '@' + Settings.GetStringValue(TConfigSections.ApplicationDetails, 'MAIL_DOMAIN', '');
//    Mailer.MailBcc    :='';
//    Mailer.MailSubject:='Unity [QMS]: Query status has been changed';
//
//    Mailer.MailBody:='<p>Query status has been changed by ' +
//                     UpperCase(SessionService.SessionUser)  +
//                     ' with comment: '                      + Comment     +
//                     '.</p><p>Invoice number: '             + InvoiceNo   +
//                     '.</p><p>Query status: '               + Status      +
//                     '.</p><p>Query reason: '               + QueryReason +
//                     '.</p><p>Query UID: '                  + QueryUid;
//    Mailer.SendNow;
//
//end;
//
//
//procedure TQueries.ApproveQuery(DbItemId: integer; FscView: boolean); {refactor / async}
//begin
//
//    // FSC actions
//    if FscView then
//    begin
//
//        // Check item status
//        var ItemStatus: string:=MainForm.sgFSCview.Cells[MainForm.sgFSCview.ReturnColumn(TQmsLog.QueryStatus, 1, 1), MainForm.sgFSCview.Row];
//
//        if ItemStatus = 'OPEN' then
//        begin
//
//            // Allow to resolve query
//            if THelpers.MsgCall(TAppMessage.Question2, 'Are you sure you want to resolve this query?') = IDYES then
//            begin
//                UpdateStatus(DbItemId, 'RESOLVED', MainForm.sgFSCview, FscView);
//                MainForm.FSCComment.Clear;
//                THelpers.MsgCall(TAppMessage.Info, 'Query has been resolved!');
//            end;
//
//        end
//        else
//        if ItemStatus = 'PENDING' then
//        begin
//            UpdateStatus(DbItemId, 'RESOLVED', MainForm.sgFSCview, FscView);
//            MainForm.FSCComment.Clear;
//            THelpers.MsgCall(TAppMessage.Info, 'Query has been resolved!');
//        end
//        else
//        begin
//            THelpers.MsgCall(TAppMessage.Warn, 'You can only resolve queries that are either pending or open.');
//        end;
//
//    end
//    // LBU actions
//    else
//    begin
//
//        // Check item status
//        var ItemStatus: string:=MainForm.sgLBUview.Cells[MainForm.sgLBUview.ReturnColumn(TQmsLog.QueryStatus, 1, 1) , MainForm.sgLBUview.Row];
//
//        if ItemStatus = 'OPEN' then
//        begin
//            UpdateStatus(DbItemId, 'PENDING', MainForm.sgLBUview, FscView);
//            MainForm.LbuComment.Clear;
//            THelpers.MsgCall(TAppMessage.Info, 'Query has been resolved!');
//        end
//        else
//        begin
//            THelpers.MsgCall(TAppMessage.Warn, 'You can only update open queries.');
//        end;
//
//    end;
//
//end;
//
//
//procedure TQueries.RejectQuery(DbItemId: integer; FscView: boolean); {refactor / async}
//begin
//
//    if FscView then
//    begin
//
//        // Check item status
//        var ItemStatus: string:=MainForm.sgFSCview.Cells[MainForm.sgFSCview.ReturnColumn(TQmsLog.QueryStatus, 1, 1) , MainForm.sgFSCview.Row];
//
//        if ItemStatus = 'PENDING' then
//        begin
//            UpdateStatus(DbItemId, 'OPEN', MainForm.sgFSCview, FscView);
//            MainForm.FscComment.Clear;
//        end;
//
//    end;
//
//end;


end.

