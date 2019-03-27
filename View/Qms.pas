
{$I .\Include\Header.inc}

unit Qms;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    InterposerClasses;


type

    /// <summary>
    ///
    /// </summary>

    TQmsForm = class(TForm)
        MainFrame: TGroupBox;
        BottomPanel: TPanel;
        btnLog: TSpeedButton;
        btnCancel: TSpeedButton;
        MainPanel: TPanel;
        StatusList: TComboBox;
        StatusLabel: TLabel;
        MissingInvoiceBox: TGroupBox;
        StatusPanel: TPanel;
        EditInvoiceNo: TLabeledEdit;
        EditOpenAmount: TLabeledEdit;
        EditAmount: TLabeledEdit;
        EditOpenCurrAm: TLabeledEdit;
        EditDueDate: TLabeledEdit;
        EditValDate: TLabeledEdit;
        EditQueryReason: TLabeledEdit;
        EditLogType: TLabeledEdit;
        EditCurrAmount: TLabeledEdit;
        EditUserAlias: TLabeledEdit;
        EditStamp: TLabeledEdit;
        CurrencyList: TComboBox;
        Currency: TLabel;
        bgEdit1: TShape;
        bgEdit2: TShape;
        bgEdit3: TShape;
        bgEdit4: TShape;
        bgEdit5: TShape;
        bgEdit6: TShape;
        bgEdit7: TShape;
        bgEdit8: TShape;
        bgEdit9: TShape;
        bgEdit10: TShape;
        bgEdit11: TShape;
        btnAddDueDate: TSpeedButton;
        btnAddValDate: TSpeedButton;
        QueryDesc: TMemo;
        QueryDescBorders: TShape;
        QueryDescLabel: TLabel;
        LbuEmailAddress: TLabeledEdit;
        bgEdit: TShape;
        OpenDlgBox: TOpenDialog;
        btnAddAttcahement: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnLogClick(Sender: TObject);
        procedure StatusListSelect(Sender: TObject);
        procedure btnAddDueDateClick(Sender: TObject);
        procedure btnAddValDateClick(Sender: TObject);
        procedure EditOpenAmountKeyPress(Sender: TObject; var Key: Char);
        procedure EditOpenCurrAmKeyPress(Sender: TObject; var Key: Char);
        procedure EditCurrAmountKeyPress(Sender: TObject; var Key: Char);
        procedure EditAmountKeyPress(Sender: TObject; var Key: Char);
        procedure btnAddAttcahementClick(Sender: TObject);
    private
        var FIsMissing:  boolean;
        var FFileName:   string;
        var FLogQueryId: string;
    public
        property  LogQueryId: string  read FLogQueryId;
        property  IsMissing:  boolean read FIsMissing write FIsMissing;
        property  FileName:   string  read FFileName  write FFileName;
        function  InsertMissingInvoice: boolean;
        function  InsertCurrentInvoices(Source: TStringGrid): boolean;
        function  ValidateFields: cardinal;
        procedure ClearAll;
        procedure AddAttachement;
        function  ReturnCurrencyId(ISO: string): cardinal;
        function  ReturnQueryReasonId(QueryReason: string): cardinal;
        function  SendNotification(LbuEmail: string): boolean;
    end;


var
    QmsForm: TQmsForm;


implementation


uses
    Main,
    Mailer,
    SQL,
    Model,
    Worker,
    Actions,
    Calendar,
    Settings;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


function TQmsForm.InsertMissingInvoice: boolean;  // make async!!! refactor  // only missing invoice
var
    Tables: TDataTables;
    QueryUid: TGUID;
begin

    Result:=False;
    if not(IsMissing) then Exit;

    Tables:=TDataTables.Create(MainForm.DbConnect);
    try
        // Generate GUID for new entry
        CreateGUID(QueryUid);

        // Define target columns
        Tables.Columns.Add(TQmsLog.InvoNo);
        Tables.Columns.Add(TQmsLog.OpenAm);
        Tables.Columns.Add(TQmsLog.Am);
        Tables.Columns.Add(TQmsLog.OpenCurAm);
        Tables.Columns.Add(TQmsLog.CurAm);
        Tables.Columns.Add(TQmsLog.ISO);
        Tables.Columns.Add(TQmsLog.DueDt);
        Tables.Columns.Add(TQmsLog.ValDt);
        Tables.Columns.Add(TQmsLog.LogType);
        Tables.Columns.Add(TQmsLog.QueryReason);
        Tables.Columns.Add(TQmsLog.QueryDesc);
        Tables.Columns.Add(TQmsLog.QueryStatus);
        Tables.Columns.Add(TQmsLog.FscComment);
        Tables.Columns.Add(TQmsLog.LbuComment);
        Tables.Columns.Add(TQmsLog.Receiver);
        Tables.Columns.Add(TQmsLog.UserAlias);
        Tables.Columns.Add(TQmsLog.Stamp);
        Tables.Columns.Add(TQmsLog.QueryUid);

        // Assign values
        Tables.Values.Add(EditInvoiceNo.Text);
        Tables.Values.Add(EditOpenAmount.Text);
        Tables.Values.Add(EditAmount.Text);
        Tables.Values.Add(EditOpenCurrAm.Text);
        Tables.Values.Add(EditCurrAmount.Text);
        Tables.Values.Add(ReturnCurrencyId(CurrencyList.Items[CurrencyList.ItemIndex]).ToString);
        Tables.Values.Add(EditDueDate.Text);
        Tables.Values.Add(EditValDate.Text);
        Tables.Values.Add(EditLogType.Text);
        Tables.Values.Add(ReturnQueryReasonId(EditQueryReason.Text).ToString);
        Tables.Values.Add(QueryDesc.Text);
        Tables.Values.Add(qrOPEN);
        Tables.Values.Add(String.Empty);
        Tables.Values.Add(String.Empty);
        Tables.Values.Add(LbuEmailAddress.Text);
        Tables.Values.Add(EditUserAlias.Text);
        Tables.Values.Add(EditStamp.Text);
        Tables.Values.Add(QueryUid.ToString);

        if Tables.InsertInto(TQmsLog.QmsLog, ttExplicit) then
        begin
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Missing invoice has been logged successfully.');
            MainForm.MsgCall(mcInfo, 'Missing invoice has been logged successfully.');
            FLogQueryId:=QueryUid.ToString;
            Result:=True;
            CLearAll;
            Close;
        end
        else
        begin
            FLogQueryId:=String.Empty;
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot log missing invoice.');
            MainForm.MsgCall(mcError, 'Cannot log missing invoice to database. Please check the log and contact IT support.');
        end;

    finally
        Tables.Free;
    end;

end;


/// <summary>
///
/// </summary>

function TQmsForm.InsertCurrentInvoices(Source: TStringGrid): boolean; // make async !!! // only current invoices (one or more)
var
    Tables:     TDataTables;
    TempData:   TStringGrid;
    QueryUid:   TGUID;
    iCNT:       integer;
    jCNT:       integer;
    UserFormat: TFormatSettings;
    DueDate:    TDate;
    ValDate:    TDate;
    OpenAm:     string;
    Am:         string;
    OpenCurAm:  string;
    CurAm:      string;
begin

    Result:=False;
    if (Source = nil) {or (Source.Selection.Bottom - Source.Selection.Top = 0)} then Exit;

    {$WARN SYMBOL_PLATFORM OFF}
    UserFormat:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}

    Tables:=TDataTables.Create(MainForm.DbConnect);
    TempData:=TStringGrid.Create(nil);
    try
        // Generate GUID for new entry
        CreateGUID(QueryUid);

        // Define target columns
        Tables.Columns.Add(TQmsLog.InvoNo);
        Tables.Columns.Add(TQmsLog.OpenAm);
        Tables.Columns.Add(TQmsLog.Am);
        Tables.Columns.Add(TQmsLog.OpenCurAm);
        Tables.Columns.Add(TQmsLog.CurAm);
        Tables.Columns.Add(TQmsLog.ISO);
        Tables.Columns.Add(TQmsLog.DueDt);
        Tables.Columns.Add(TQmsLog.ValDt);
        Tables.Columns.Add(TQmsLog.LogType);
        Tables.Columns.Add(TQmsLog.QueryReason);
        Tables.Columns.Add(TQmsLog.QueryDesc);
        Tables.Columns.Add(TQmsLog.QueryStatus);
        Tables.Columns.Add(TQmsLog.FscComment);
        Tables.Columns.Add(TQmsLog.LbuComment);
        Tables.Columns.Add(TQmsLog.Receiver);
        Tables.Columns.Add(TQmsLog.UserAlias);
        Tables.Columns.Add(TQmsLog.Stamp);
        Tables.Columns.Add(TQmsLog.QueryUid);

        // Move the data from open items grid in ActionLog window into the TempGrid
        TempData.RowCount:=(Source.Selection.Bottom - Source.Selection.Top) + 1;
        TempData.ColCount:=Tables.Columns.Count;

        jCNT:=Source.Selection.Top;
        for iCNT:=0 to TempData.RowCount do
        begin

            DueDate:=StrToDate(Source.Cells[Source.ReturnColumn(TOpenitems.DueDt, 1, 1), jCNT], UserFormat);
            ValDate:=StrToDate(Source.Cells[Source.ReturnColumn(TOpenitems.ValDt, 1, 1), jCNT], UserFormat);
            OpenAm:=Source.Cells[Source.ReturnColumn(TOpenitems.OpenAm, 1, 1), jCNT];
            Am:=Source.Cells[Source.ReturnColumn(TOpenitems.Am, 1, 1), jCNT];
            OpenCurAm:=Source.Cells[Source.ReturnColumn(TOpenitems.OpenCurAm, 1, 1), jCNT];
            CurAm:=Source.Cells[Source.ReturnColumn(TOpenitems.CurAm, 1, 1), jCNT];

            TempData.Cells[0,  iCNT]:=Source.Cells[Source.ReturnColumn(TOpenitems.InvoNo, 1, 1), jCNT];
            TempData.Cells[1,  iCNT]:=StringReplace(OpenAm, ',', '.', [rfReplaceAll]);
            TempData.Cells[2,  iCNT]:=StringReplace(Am, ',', '.', [rfReplaceAll]);
            TempData.Cells[3,  iCNT]:=StringReplace(OpenCurAm, ',', '.', [rfReplaceAll]);
            TempData.Cells[4,  iCNT]:=StringReplace(CurAm, ',', '.', [rfReplaceAll]);
            TempData.Cells[5,  iCNT]:=ReturnCurrencyId(Source.Cells[Source.ReturnColumn(TOpenitems.ISO, 1, 1), jCNT]).ToString;
            TempData.Cells[6,  iCNT]:=DateToStr(DueDate, FormatSettings);
            TempData.Cells[7,  iCNT]:=DateToStr(ValDate, FormatSettings);
            TempData.Cells[8,  iCNT]:=EditLogType.Text;
            TempData.Cells[9,  iCNT]:=ReturnQueryReasonId(EditQueryReason.Text).ToString;
            TempData.Cells[10, iCNT]:=QueryDesc.Text;
            TempData.Cells[11, iCNT]:=qrOPEN;
            TempData.Cells[12, iCNT]:=String.Empty;
            TempData.Cells[13, iCNT]:=String.Empty;
            TempData.Cells[14, iCNT]:=LbuEmailAddress.Text;
            TempData.Cells[15, iCNT]:=EditUserAlias.Text;
            TempData.Cells[16, iCNT]:=EditStamp.Text;
            TempData.Cells[17, iCNT]:=QueryUid.ToString;
            Inc(jCNT);
        end;

        // Send to server
        if Tables.InsertInto(TQmsLog.QmsLog, ttExplicit, TempData, nil, False) then
        begin
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Provided informatin has been logged successfully.');
            MainForm.MsgCall(mcInfo, 'Provided information has been logged successfully.');
            Result:=True;
            FLogQueryId:=QueryUid.ToString;
            Close;
        end
        else
        begin
            FLogQueryId:=String.Empty;
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot log current invoice(s).');
            MainForm.MsgCall(mcError, 'Cannot log current invoice(s) to database. Please check the log and contact IT support.');
        end;
    finally
        Tables.Free;
        TempData.Free;
    end;

end;


/// <summary>
///
/// </summary>

function TQmsForm.ValidateFields: cardinal;
var
    Checks: integer;
begin

    Checks:=0;

    if String.IsNullOrEmpty(LbuEmailAddress.Text) then Inc(Checks);
    if String.IsNullOrEmpty(QueryDesc.Text)       then Inc(Checks);

    if IsMissing then
    begin
        if String.IsNullOrEmpty(EditInvoiceNo.Text)  then Inc(Checks);
        if String.IsNullOrEmpty(EditOpenAmount.Text) then Inc(Checks);
        if String.IsNullOrEmpty(EditAmount.Text)     then Inc(Checks);
        if String.IsNullOrEmpty(EditOpenCurrAm.Text) then Inc(Checks);
        if String.IsNullOrEmpty(EditCurrAmount.Text) then Inc(Checks);
        if String.IsNullOrEmpty(EditDueDate.Text)    then Inc(Checks);
        if String.IsNullOrEmpty(EditValDate.Text)    then Inc(Checks);
    end;

    Result:=Checks;

end;


/// <summary>
///
/// </summary>

procedure TQmsForm.ClearAll;
begin
    EditInvoiceNo.Text:='';
    EditOpenAmount.Text:='';
    EditAmount.Text:='';
    EditOpenCurrAm.Text:='';
    EditCurrAmount.Text:='';
    EditDueDate.Text:='';
    EditValDate.Text:='';
    LbuEmailAddress.Text:='';
    QueryDesc.Text:='';
end;


/// <summary>
///
/// </summary>

procedure TQmsForm.AddAttachement;
begin
    if not(String.IsNullOrEmpty(FileName)) then OpenDlgBox.InitialDir:=ExtractFilePath(FileName);

    if OpenDlgBox.Execute then
    begin
        FileName:=OpenDlgBox.FileName;
        MainForm.MsgCall(mcInfo, 'The attachement has been added successfully!');
    end
    else
    begin
        FileName:='';
        MainForm.MsgCall(mcWarn, 'No attachement present.');
    end;
end;


/// <summary>
///
/// </summary>

function TQmsForm.ReturnCurrencyId(ISO: string): cardinal; // make async !!!
var
    Tables: TDataTables;
begin
    Result:=0;
    Tables:=TDataTables.Create(MainForm.DbConnect);
    try
        try
            Tables.CleanUp;
            Tables.CustFilter:=WHERE + TCurrencies.Iso + EQUAL + QuotedStr(ISO);
            Tables.OpenTable(TCurrencies.Currencies);
            if Tables.DataSet.RecordCount = 1 then
                Result:=Tables.DataSet.Fields[TCurrencies.Id].Value;
        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot execute SQL, error has been thrown: ' + E.Message);
                MainForm.MsgCall(mcWarn, 'Unexpected error has occured: ' + E.Message + '. Please contact IT Support.');
            end;
        end;
    finally
        Tables.Free;
    end;
end;


/// <summary>
///
/// </summary>

function TQmsForm.ReturnQueryReasonId(QueryReason: string): cardinal; // make async !!!
var
    Tables: TDataTables;
begin
    Result:=0;
    Tables:=TDataTables.Create(MainForm.DbConnect);
    try
        try
            Tables.CustFilter:=WHERE + TQmsReasons.QueryReason + EQUAL + QuotedStr(QueryReason);
            Tables.OpenTable(TQmsReasons.QmsReasons);
            if Tables.DataSet.RecordCount = 1 then
                Result:=Tables.DataSet.Fields[TQmsReasons.Id].Value;
        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot execute SQL, error has been thrown: ' + E.Message);
                MainForm.MsgCall(mcWarn, 'Unexpected error has occured: ' + E.Message + '. Please contact IT Support.');
            end;
        end;
    finally
        Tables.Free;
    end;
end;


/// <summary>
///
/// </summary>

function TQmsForm.SendNotification(LbuEmail: string): boolean; // make async !!! refactor!!!
var
    Mail:     TMailer;
    Settings: ISettings;
begin

    Settings:=TSettings.Create;
    Mail:=TMailer.Create;
    try

        // Get and set email details
        if Settings.GetStringValue(MailerSetup, 'ACTIVE', '') = MailerNTLM  then
        begin
            Mail.XMailer:=Settings.GetStringValue(MailerNTLM, 'FROM', '');
            //Mail.MailTo :=Settings.GetStringValue(MailerNTLM, 'TO', '');
            Mail.MailRt :=Settings.GetStringValue(MailerNTLM, 'REPLY-TO', '');
        end;

        if Settings.GetStringValue(MailerSetup, 'ACTIVE', '') = MailerBASIC then
        begin
            Mail.XMailer:=Settings.GetStringValue(MailerBASIC, 'FROM', '');
            //Mail.MailTo :=Settings.GetStringValue(MailerBASIC, 'TO', '');
            Mail.MailRt :=Settings.GetStringValue(MailerBASIC, 'REPLY-TO', '');
        end;

        Mail.MailFrom   :=Mail.XMailer;
        Mail.MailTo     :=LbuEmail;
        Mail.MailCc     :=MainForm.WinUserName + '@' + Settings.GetStringValue(ApplicationDetails, 'MAIL_DOMAIN', '');
        Mail.MailBcc    :='';
        Mail.MailSubject:='Unity [QMS]: New query';

        Mail.MailBody:='New query has been logged by ' +
                       UpperCase(MainForm.WinUserName) +
                       ' with comment: ' + QueryDesc.Lines.Text +
                       '. Query reason: ' + EditQueryReason.Text +
                       '. Query UID: ' +
                       LogQueryId + '.';

        Mail.Attachments.Add(FileName);
        Result:=Mail.SendNow;

    finally
        Mail.Free;
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------- MAIN CLASS EVENTS //


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TQmsForm.FormCreate(Sender: TObject);
begin
    // Do nothing
end;


/// <summary>
///
/// </summary>

procedure TQmsForm.FormShow(Sender: TObject); // make it async!!! // refactor!!!
var
    Tables: TDataTables;
begin

    Screen.Cursor:=crHourGlass;

    //
    if IsMissing then
    begin
        MissingInvoiceBox.Enabled:=True;
        MissingInvoiceBox.Visible:=True;
        QmsForm.Height:=680;
        StatusLabel.Caption:='Log missing invoice with status:';
    end
    else
    begin
        MissingInvoiceBox.Enabled:=False;
        MissingInvoiceBox.Visible:=False;
        QmsForm.Height:=340;
        StatusLabel.Caption:='Log selected invoice(s) with status:';
    end;

    //
    Tables:=TDataTables.Create(MainForm.DbConnect);
    try
        try
            Tables.Columns.Add(TQmsReasons.QueryReason);
            Tables.OpenTable(TQmsReasons.QmsReasons);
            Tables.SqlToSimpleList(StatusList, Tables.DataSet);
            if StatusList.Items.Count > 0 then
            begin
                StatusList.ItemIndex:=0;
                EditQueryReason.Text:=StatusList.Items[StatusList.ItemIndex];
            end;

            if IsMissing then
            begin
                Tables.CleanUp;
                Tables.Columns.Add(TCurrencies.Iso);
                Tables.OpenTable(TCurrencies.Currencies);
                Tables.SqlToSimpleList(CurrencyList, Tables.DataSet);
                if CurrencyList.Items.Count > 0 then CurrencyList.ItemIndex:=0;
            end;

        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot execute SQL, error has been thrown: ' + E.Message);
                MainForm.MsgCall(mcWarn, 'Unexpected error has occured: ' + E.Message + '. Please contact IT Support.');
            end;
        end;
    finally
        Tables.Free;
    end;

    //
    if IsMissing then EditLogType.Text:='Missing' else EditLogType.Text:='Existing';
    EditUserAlias.Text:=MainForm.WinUserName;
    EditStamp.Text:=DateTimeToStr(Now);

    Screen.Cursor:=crDefault;

end;


/// <summary>
///
/// </summary>

procedure TQmsForm.FormDestroy(Sender: TObject);
begin
    //
end;


// --------------------------------------------------------------------------------------------------------------------------------------- COMPONENTS EVENTS //


procedure TQmsForm.StatusListSelect(Sender: TObject);
begin
    EditQueryReason.Text:=StatusList.Items[StatusList.ItemIndex];
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //


/// <summary>
///
/// </summary>

procedure TQmsForm.btnAddAttcahementClick(Sender: TObject);
begin
    AddAttachement;
end;


/// <summary>
///
/// </summary>

procedure TQmsForm.btnAddDueDateClick(Sender: TObject);
begin
    CalendarForm.CalendarMode:=cfGetDate;
    MainForm.WndCall(CalendarForm, stModal);
    if CalendarForm.SelectedDate <> NULLDATE then EditDueDate.Text:=DateToStr(CalendarForm.SelectedDate);
end;


/// <summary>
///
/// </summary>

procedure TQmsForm.btnAddValDateClick(Sender: TObject);
begin
    CalendarForm.CalendarMode:=cfGetDate;
    MainForm.WndCall(CalendarForm, stModal);
    if CalendarForm.SelectedDate <> NULLDATE then EditValDate.Text:=DateToStr(CalendarForm.SelectedDate);
end;


/// <summary>
///
/// </summary>

procedure TQmsForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


/// <summary>
///
/// </summary>

procedure TQmsForm.btnLogClick(Sender: TObject);
begin

    if ValidateFields > 0 then
    begin
        MainForm.MsgCall(mcWarn, 'Please provide all required fields.');
        Exit;
    end;

    Screen.Cursor:=crHourGlass;

    if IsMissing then
        if InsertMissingInvoice then SendNotification(LbuEmailAddress.Text);

    if not(IsMissing) then
        if InsertCurrentInvoices(ActionsForm.OpenItemsGrid) then SendNotification(LbuEmailAddress.Text);

    ClearAll;
    Screen.Cursor:=crDefault;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TQmsForm.EditAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', POINT, BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditCurrAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', POINT, BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditOpenAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', POINT, BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditOpenCurrAmKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', POINT, BACKSPACE])) then Key:=#0;
end;


end.
