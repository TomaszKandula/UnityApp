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
    Unity.Interposer,
    Unity.Enums;


type


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


    function QmsForm: TQmsForm;


implementation


{$R *.dfm}


uses
    Main,
    Sync.Documents,
    SqlHandler,
    DbModel,
    Worker,
    Actions,
    Calendar,
    Settings,
    Unity.Statics;


var vQmsForm: TQmsForm;


function QmsForm: TQmsForm;
begin
    if not(Assigned(vQmsForm)) then Application.CreateForm(TQmsForm, vQmsForm);
    Result:=vQmsForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


function TQmsForm.InsertMissingInvoice: boolean;  {refactor / async}
begin

    Result:=False;
    if not(IsMissing) then Exit;

    var Tables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        // Generate GUID for new entry
        var QueryUid: TGUID;
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
        Tables.Values.Add(TQms.Open);
        Tables.Values.Add(String.Empty);
        Tables.Values.Add(String.Empty);
        Tables.Values.Add(LbuEmailAddress.Text);
        Tables.Values.Add(EditUserAlias.Text);
        Tables.Values.Add(EditStamp.Text);
        Tables.Values.Add(QueryUid.ToString);

        if Tables.InsertInto(TQmsLog.QmsLog, True) then
        begin
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Missing invoice has been logged successfully.');
            MainForm.MsgCall(TCommon.TMessage.Info, 'Missing invoice has been logged successfully.');
            FLogQueryId:=QueryUid.ToString;
            Result:=True;
            CLearAll;
            Close;
        end
        else
        begin
            FLogQueryId:=String.Empty;
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot log missing invoice.');
            MainForm.MsgCall(TCommon.TMessage.Error, 'Cannot log missing invoice to database. Please check the log and contact IT support.');
        end;

    finally
        Tables.Free;
    end;

end;


function TQmsForm.InsertCurrentInvoices(Source: TStringGrid): boolean; {refactor / async}
begin

    Result:=False;
    if Source = nil then Exit;

    {$WARN SYMBOL_PLATFORM OFF}
    var UserFormat: TFormatSettings:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}

    var Tables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    var TempData: TStringGrid:=TStringGrid.Create(nil);
    try
        // Generate GUID for new entry
        var QueryUid: TGUID;
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

        var jCNT: integer:=Source.Selection.Top;
        for var iCNT: integer:=0 to TempData.RowCount do
        begin

            // Get and convert from user format to application format accepted by MSSQL
            var DueDate: TDate:=StrToDate(Source.Cells[Source.ReturnColumn(TOpenitems.DueDt, 1, 1), jCNT], UserFormat);
            var ValDate: TDate:=StrToDate(Source.Cells[Source.ReturnColumn(TOpenitems.ValDt, 1, 1), jCNT], UserFormat);
            var DueDateStr: string:=DateToStr(DueDate, FormatSettings);
            var ValDateStr: string:=DateToStr(ValDate, FormatSettings);

            // Get amounts
            var OpenAm:    string:=Source.Cells[Source.ReturnColumn(TOpenitems.OpenAm, 1, 1), jCNT];
            var Am:        string:=Source.Cells[Source.ReturnColumn(TOpenitems.Am, 1, 1), jCNT];
            var OpenCurAm: string:=Source.Cells[Source.ReturnColumn(TOpenitems.OpenCurAm, 1, 1), jCNT];
            var CurAm:     string:=Source.Cells[Source.ReturnColumn(TOpenitems.CurAm, 1, 1), jCNT];

            // Replace decimal separator to point required by MSSQL
            OpenAm   :=StringReplace(OpenAm, ',', '.', [rfReplaceAll]);
            Am       :=StringReplace(Am, ',', '.', [rfReplaceAll]);
            OpenCurAm:=StringReplace(OpenCurAm, ',', '.', [rfReplaceAll]);
            CurAm    :=StringReplace(CurAm, ',', '.', [rfReplaceAll]);

            TempData.Cells[0,  iCNT]:=Source.Cells[Source.ReturnColumn(TOpenitems.InvoNo, 1, 1), jCNT];
            TempData.Cells[1,  iCNT]:=OpenAm;
            TempData.Cells[2,  iCNT]:=Am;
            TempData.Cells[3,  iCNT]:=OpenCurAm;
            TempData.Cells[4,  iCNT]:=CurAm;
            TempData.Cells[5,  iCNT]:=ReturnCurrencyId(Source.Cells[Source.ReturnColumn(TOpenitems.ISO, 1, 1), jCNT]).ToString;
            TempData.Cells[6,  iCNT]:=DueDateStr;
            TempData.Cells[7,  iCNT]:=ValDateStr;
            TempData.Cells[8,  iCNT]:=EditLogType.Text;
            TempData.Cells[9,  iCNT]:=ReturnQueryReasonId(EditQueryReason.Text).ToString;
            TempData.Cells[10, iCNT]:=QueryDesc.Text;
            TempData.Cells[11, iCNT]:=TQms.Open;
            TempData.Cells[12, iCNT]:=String.Empty;
            TempData.Cells[13, iCNT]:=String.Empty;
            TempData.Cells[14, iCNT]:=LbuEmailAddress.Text;
            TempData.Cells[15, iCNT]:=EditUserAlias.Text;
            TempData.Cells[16, iCNT]:=EditStamp.Text;
            TempData.Cells[17, iCNT]:=QueryUid.ToString;
            Inc(jCNT);
        end;

        // Send to server
        if Tables.InsertInto(TQmsLog.QmsLog, True, TempData, nil, False) then
        begin
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Provided informatin has been logged successfully.');
            MainForm.MsgCall(Info, 'Provided information has been logged successfully.');
            Result:=True;
            FLogQueryId:=QueryUid.ToString;
            Close;
        end
        else
        begin
            FLogQueryId:=String.Empty;
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot log current invoice(s).');
            MainForm.MsgCall(Error, 'Cannot log current invoice(s) to database. Please check the log and contact IT support.');
        end;
    finally
        Tables.Free;
        TempData.Free;
    end;

end;


function TQmsForm.ValidateFields: cardinal;
begin

    var Checks: integer:=0;

    if String.IsNullOrEmpty(LbuEmailAddress.Text) then Inc(Checks);
    if String.IsNullOrEmpty(QueryDesc.Text)       then Inc(Checks);

    if (IsMissing) and (String.IsNullOrEmpty(EditInvoiceNo.Text))  then Inc(Checks);
    if (IsMissing) and (String.IsNullOrEmpty(EditOpenAmount.Text)) then Inc(Checks);
    if (IsMissing) and (String.IsNullOrEmpty(EditAmount.Text))     then Inc(Checks);
    if (IsMissing) and (String.IsNullOrEmpty(EditOpenCurrAm.Text)) then Inc(Checks);
    if (IsMissing) and (String.IsNullOrEmpty(EditCurrAmount.Text)) then Inc(Checks);
    if (IsMissing) and (String.IsNullOrEmpty(EditDueDate.Text))    then Inc(Checks);
    if (IsMissing) and (String.IsNullOrEmpty(EditValDate.Text))    then Inc(Checks);

    Result:=Checks;

end;


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


procedure TQmsForm.AddAttachement;
begin
    if not(String.IsNullOrEmpty(FileName)) then OpenDlgBox.InitialDir:=ExtractFilePath(FileName);

    if OpenDlgBox.Execute then
    begin
        FileName:=OpenDlgBox.FileName;
        MainForm.MsgCall(Info, 'The attachement has been added successfully!');
    end
    else
    begin
        FileName:='';
        MainForm.MsgCall(Warn, 'No attachement present.');
    end;
end;


function TQmsForm.ReturnCurrencyId(ISO: string): cardinal; {refactor / async}
begin

    Result:=0;

    var Tables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        try
            Tables.CleanUp;
            Tables.CustFilter:=TSql.WHERE + TCurrencies.Iso + TSql.EQUAL + QuotedStr(ISO);
            Tables.OpenTable(TCurrencies.Currencies);
            if Tables.DataSet.RecordCount = 1 then
                Result:=Tables.DataSet.Fields[TCurrencies.Id].Value;
        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot execute SQL, error has been thrown: ' + E.Message);
                MainForm.MsgCall(Warn, 'Unexpected error has occured: ' + E.Message + '. Please contact IT Support.');
            end;
        end;
    finally
        Tables.Free;
    end;

end;


function TQmsForm.ReturnQueryReasonId(QueryReason: string): cardinal; {refactor / async}
begin

    Result:=0;

    var Tables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        try
            Tables.CustFilter:=TSql.WHERE + TQmsReasons.QueryReason + TSql.EQUAL + QuotedStr(QueryReason);
            Tables.OpenTable(TQmsReasons.QmsReasons);
            if Tables.DataSet.RecordCount = 1 then
                Result:=Tables.DataSet.Fields[TQmsReasons.Id].Value;
        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: [QMS] Cannot execute SQL, error has been thrown: ' + E.Message);
                MainForm.MsgCall(Warn, 'Unexpected error has occured: ' + E.Message + '. Please contact IT Support.');
            end;
        end;
    finally
        Tables.Free;
    end;

end;


function TQmsForm.SendNotification(LbuEmail: string): boolean; {refactor / async}
begin

    var Settings: ISettings:=TSettings.Create;
    var Mail: IDocument:=TDocument.Create;

    // Get and set email details
    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM  then
    begin
        Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'FROM', '');
        Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'REPLY-TO', '');
    end;

    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then
    begin
        Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'FROM', '');
        Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'REPLY-TO', '');
    end;

    Mail.MailFrom   :=Mail.XMailer;
    Mail.MailTo     :=LbuEmail;
    Mail.MailCc     :=MainForm.WinUserName + '@' + Settings.GetStringValue(TConfigSections.ApplicationDetails, 'MAIL_DOMAIN', '');
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

end;


// --------------------------------------------------------------------------------------------------------------------------------------- MAIN CLASS EVENTS //


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TQmsForm.FormCreate(Sender: TObject);
begin
    // Do nothing
end;


procedure TQmsForm.FormDestroy(Sender: TObject);
begin
    // Do nothing
end;


procedure TQmsForm.FormShow(Sender: TObject); {refactor / async}
begin

    Screen.Cursor:=crHourGlass;

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

    var Tables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
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
                MainForm.MsgCall(Warn, 'Unexpected error has occured: ' + E.Message + '. Please contact IT Support.');
            end;
        end;
    finally
        Tables.Free;
    end;

    if IsMissing then EditLogType.Text:='Missing' else EditLogType.Text:='Existing';
    EditUserAlias.Text:=MainForm.WinUserName;
    EditStamp.Text:=DateTimeToStr(Now);

    Screen.Cursor:=crDefault;

end;


// --------------------------------------------------------------------------------------------------------------------------------------- COMPONENTS EVENTS //


procedure TQmsForm.StatusListSelect(Sender: TObject);
begin
    EditQueryReason.Text:=StatusList.Items[StatusList.ItemIndex];
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //


procedure TQmsForm.btnAddAttcahementClick(Sender: TObject);
begin
    AddAttachement;
end;


procedure TQmsForm.btnAddDueDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=TCalendar.GetDate;
    MainForm.WndCall(CalendarForm, TWindowState.Modal);
    if CalendarForm.FSelectedDate <> TDateTimeFormats.NullDate then EditDueDate.Text:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TQmsForm.btnAddValDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=TCalendar.GetDate;
    MainForm.WndCall(CalendarForm, Modal);
    if CalendarForm.FSelectedDate <> TDateTimeFormats.NullDate then EditValDate.Text:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TQmsForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


procedure TQmsForm.btnLogClick(Sender: TObject);
begin

    if ValidateFields > 0 then
    begin
        MainForm.MsgCall(Warn, 'Please provide all required fields.');
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
    if not(CharInSet(Key, ['0'..'9',  TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditCurrAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditOpenAmountKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


procedure TQmsForm.EditOpenCurrAmKeyPress(Sender: TObject; var Key: Char);
begin
    if not(CharInSet(Key, ['0'..'9', TChars.POINT, TChars.BACKSPACE])) then Key:=#0;
end;


end.
