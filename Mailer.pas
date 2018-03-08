{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Mailer;

interface

uses
  Main, SysUtils, Windows, Messages, StdCtrls, Classes, StrUtils, Variants, CDO_TLB, blcksock, smtpsend { MODIFIED FROM ORIGINAL }, pop3send, ssl_openssl, synautil,
  synacode, mimemess { MODIFIED FROM ORIGINAL };


{ -------------------------------------------------------------- ! MAILER CLASS ! --------------------------------------------------------------------------- }
type                                                      (* RUN IN WORKER THREAD ONLY *)
  TMailer = class { BASE CLASS }
  {$TYPEINFO ON}
  private
    pidThd     : integer;
    pXMailer   : string;
    pmFrom     : string;
    pmTo       : string;
    pmCc       : string;
    pmBcc      : string;
    pmRt       : string;
    pmSubject  : string;
    pmBody     : string;
    pmLogo     : string;
  public
    property    idThd       : integer read pidThd    write pidThd;
    property    XMailer     : string  read pXMailer  write pXMailer;
    property    MailFrom    : string  read pmFrom    write pmFrom;
    property    MailTo      : string  read pmTo      write pmTo;
    property    MailCc      : string  read pmCc      write pmCc;
    property    MailBcc     : string  read pmBcc     write pmBcc;
    property    MailRt      : string  read pmRt      write pmRt;
    property    MailSubject : string  read pmSubject write pmSubject;
    property    MailBody    : string  read pmBody    write pmBody;
    property    Logo        : string  read pmLogo    write pmLogo;
  published
    constructor Create;
    function    SendCDOSYS : boolean;
    function    SendSynapse: boolean;
    function    SendNow    : boolean;
  end;

{ ---------------------------------------------------------- ! INVOICE TRACKER CLASS ! ---------------------------------------------------------------------- }
type                                                       (* RUN IN WORKER THREAD ONLY *)
  TInvoiceTracker = class(TMailer)
  {$TYPEINFO ON}
  published
    constructor Create;
    procedure   Scanner(idThd: integer);
    procedure   Refresh(var SG: TStringGrid; Param: string);
  end;

implementation

uses
  Settings, Database, ADODB, Tracker;

{ ############################################################## ! MAILER CLASS ! ########################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- MAILER }
constructor TMailer.Create;
begin
  (* INITIALIZE PRIVATE VARIABES *)
  pidThd   :=0;
  pXMailer :='';
  pmFrom   :='';
  pmTo     :='';
  pmCc     :='';
  pmBcc    :='';
  pmRt     :='';
  pmSubject:='';
  pmBody   :='';
  pmLogo   :='';
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- SEND USING CDOSYS | NTLM }
function TMailer.SendCDOSYS: boolean;
var
  AppSettings:  TSettings;
  CdoMessage:   CDO_TLB.IMessage;
  Schema:       string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  CdoMessage:=CDO_TLB.CoMessage.Create;
  { -------------------------------------------------------------------------------------------------------------------------------- ASSIGN | CANNOT BE EMPTY }
  CdoMessage.From:=MailFrom;
  CdoMessage.To_ :=MailTo;
  CdoMessage.CC  :=MailCc;
  { ---------------------------------------------------------------------------------------------------------------- OPTIONAL | CAN BE EMPTY, THEN WE SKIP IT }
  if MailBcc <> '' then CdoMessage.BCC    :=MailBcc;
  if MailRt  <> '' then CdoMessage.ReplyTo:=MailRt;
  { ------------------------------------------------------------------------------------------------------------------------------------- SUBJECT & HTML BODY }
  CdoMessage.Subject :=MailSubject;
  CdoMessage.HTMLBody:=MailBody;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- CONFIGURE }
  Schema:='http://schemas.microsoft.com/cdo/configuration/';
  AppSettings:=TSettings.Create;
  try
    CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=2; (* SEND THE MESSAGE USING THE NETWORK *)
    CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=AppSettings.TMIG.ReadString(MailerCDOSYS, 'SMTP', '');
    CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=AppSettings.TMIG.ReadString(MailerCDOSYS, 'PORT', '');
    CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=2; (* NTLM *)
    CdoMessage.Configuration.Fields.item[Schema + 'NNTPAccountName' ].Value:=XMailer;
    CdoMessage.Configuration.Fields.update;
    { ---------------------------------------------------------------------------------------------------------------------------------------------------- SEND }
    try
      CdoMessage.BodyPart.Charset:='utf-8';
      CdoMessage.Send;
      Result:=True;
    except
      on E: Exception do
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Could not send an e-mail. Error message thrown: ' + E.Message);
    end;
  finally
    AppSettings.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- SEND USING SYNAPSE }
function TMailer.SendSynapse;
var
  AppSettings:  TSettings;
  Email:        TSMTPSend;
  MailContent:  TStringList;
  Msg:          TMimeMess;
  SendTo:       TStringList;
  SendCc:       TStringList;
  SendBc:       TStringList;
  Delimiter:    char;
  iCNT:         integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result     :=False;
  Delimiter  :=';';
  SendTo     :=TStringList.Create;
  SendCc     :=TStringList.Create;
  SendBc     :=TStringList.Create;
  MailContent:=TStringList.Create;
  Msg        :=TMimeMess.Create;
  Email      :=TSMTPSend.Create;
  AppSettings:=TSettings.Create;
  try
    try
      { ------------------------------------------------------------------------------------------------------------------------ ADD PRE-PREPARED E-MAIL BODY }
      MailContent.Add(MailBody);
      { -------------------------------------------------------------------------------------------------------------------------------------- E-MAIL HEADERS }
      Msg.Header.From    :=XMailer;
      Msg.Header.Priority:=mp_High;
      Msg.Header.Subject :=MailSubject;
      Msg.Header.XMailer :=XMailer;
      if MailRt <> '' then Msg.Header.ReplyTo:=MailRt;

      (* SCAN FOR ALL EMAILS DELIMINATED BY SEMI-COLON          *)
      (* AND ADD THEM TO 'MAIL TO', 'CARBON COPY' AND           *)
      (* 'BLIND CARBON COPY' RESPECTIVELY                       *)
      (* ALL ARRAYS WILL BE MERGED LATER AND WILL BE ADDED TO   *)
      (* SMTP BEFORE SEND AN EMAIL                              *)

      { -------------------------------------------------------------------------------------------------------------------------------------------------- TO }
      SendTo.Delimiter      :=Delimiter;
      SendTo.StrictDelimiter:=False;
      SendTo.DelimitedText  :=MailTo;
      for iCNT:=0 to SendTo.Count - 1 do Msg.Header.ToList.Add(SendTo[iCNT]);
      { ----------------------------------------------------------------------------------------------------------------------------------------- CARBON COPY }
      if MailCc <> '' then
      begin
        SendCc.Delimiter      :=Delimiter;
        SendCc.StrictDelimiter:=False;
        SendCC.DelimitedText  :=MailCc;
        for iCNT:=0 to SendCc.Count - 1 do Msg.Header.CCList.Add(SendCc[iCNT]);
      end;
      { ----------------------------------------------------------------------------------------------------------------------------------- BLIND CARBON COPY }
      if MailBcc <> '' then
      begin
        SendBc.Delimiter      :=Delimiter;
        SendBc.StrictDelimiter:=False;
        SendBC.DelimitedText  :=MailBcc;
      end;
      { ------------------------------------------------------------------------------------------------------------------------------------------ SETTING UP }
      Msg.AddPartHTML(MailContent, nil);
      Msg.EncodeMessage;
      { ------------------------------------------------------------------------------------------------------------------------ EMAIL CREDENTIALS AND SERVER }
      Email.UserName  :=AppSettings.TMIG.ReadString(MailerSynapse, 'USERNAME', '');
      Email.Password  :=AppSettings.TMIG.ReadString(MailerSynapse, 'PASSWORD', '');
      Email.TargetHost:=AppSettings.TMIG.ReadString(MailerSynapse, 'SMTP', '');
      Email.TargetPort:=AppSettings.TMIG.ReadString(MailerSynapse, 'PORT', '');
      { ---------------------------------------------------------------------------------------------------------------------------------------- TLS OVER SSL }
      if AppSettings.TMIG.ReadBool(MailerSynapse, 'TLS', False) = True then
      begin
        Email.AutoTLS:=True;
        Email.FullSSL:=False;
      end;
      { ---------------------------------------------------------------------------------------------------------------------------------------- SSL OVER TLS }
      if AppSettings.TMIG.ReadBool(MailerSynapse, 'SSL', True) = True then
      begin
        Email.AutoTLS:=False;
        Email.FullSSL:=True;
      end;
      { -------------------------------------------------------------------------------------------------------------------------------------- SENDING E-MAIL }
      if Email.Login then
      begin
        if Email.AuthDone then
        begin
          Email.MailFrom(MailFrom, length(MailFrom));

          (* ADD ALL RECIPIENTS REGARDLESS IF BCC OR NOT *)
          (* BCC CANNOT BE INCLUDED IN HEADERS           *)
          (* ADD ONE PER 'MAILTO' FUNCTION               *)

          for iCNT:=0 to SendTo.Count - 1 do Email.MailTo(SendTo[iCNT]);
          for iCNT:=0 to SendCc.Count - 1 do Email.MailTo(SendCc[iCNT]);
          for iCNT:=0 to SendBc.Count - 1 do Email.MailTo(SendBc[iCNT]);
          { --------------------------------------------------------------------------------------------------------------------------------- SEND AND LOGOUT }
          if Email.MailData(Msg.Lines) then
          begin
            Result:=True;
            Email.Logout;
          end;
        end;
      end;
    except
      on E: Exception do
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Could not send an e-mail. Error message thrown: ' + E.Message);
    end;
  finally
    MailContent.Free;
    AppSettings.Free;
    Msg.Free;
    Email.Free;
    SendTo.Free;
    SendCc.Free;
    SendBc.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SEND E-MAIL }
function TMailer.SendNow: boolean;  (* ASYNC *)
var
  AppSettings: TSettings;
begin
  Result:=False;
  AppSettings:=TSettings.Create;
  try
    (* NTLM AUTHENTICATE ONLY *)
    if AppSettings.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerCDOSYS then Result:=SendCDOSYS;
    (* REQUIRE USERNAME AND PASSWORD, SSL/TLS *)
    if AppSettings.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerSYNAPSE then Result:=SendSynapse;
  finally
    AppSettings.Free;
  end;
end;

{ ########################################################## ! INVOICE TRACKER CLASS ! ###################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TInvoiceTracker.Create;
begin
  (* DO NOTHING *)
end;

{ ----------------------------------------------------------------------------------------------------------------------------- SCAN INVOICES AND SEND EMAILS }
(* IT REQUIRES PRE-FORMATTED HTML LAYOUT. THE PREPARED PART WITH THE HTML TABLE THAT CARRY INVOICES REPLACE TAG '{INVOICE_LIST}' IN THE LAYOUT FILE *)
procedure TInvoiceTracker.Scanner(idThd: integer);  (* ASYNC *)

{ ------------------------------------------------------ ! COMMON VARIABLES AND CONSTANTS ! ----------------------------------------------------------------- }

var
  { COUNTERS }
  iCNT:        integer;
  jCNT:        integer;
  Rem1:        integer;
  Rem2:        integer;
  Rem3:        integer;
  Leg0:        integer;
  { HTML FOR BODY LAYOUT }
  HTMLTable:   string;
  HTMLRow:     string;
  HTMLTemp:    string;
  HTMLRem1:    string;
  HTMLRem2:    string;
  HTMLRem3:    string;
  HTMLLega:    string;
  { FILES AND EMAILS }
  EmailFr:     string;
  EmailTo:     string;
  EmailCc:     string;
  EmailBc:     string;
  EmailRt:     string;
  EmailSub:    string;
  EmailBody:   string;
  { CUSTOMER DETAILS }
  CustName:    string;
  CustAddr:    string;
  { EXCLUSIONS }
  REM_EX1:     string;
  REM_EX2:     string;
  REM_EX3:     string;
  REM_EX4:     string;
  REM_EX5:     string;
  { DATA FOR COMPARISION }
  CoCode:      string;
  Branch:      string;
  CUID:        string;
  Reminder1:   string;
  Reminder2:   string;
  Reminder3:   string;
  Reminder4:   string;
  { HTML DATA WITH POPULLATED TABLES PER REMINDER }
  Table1:      string;    { REMINDER 1 INVOICES   }
  Table2:      string;    { REMINDER 2 INVOICES   }
  Table3:      string;    { REMINDER 3 INVOICES   }
  Table4:      string;    { LEGAL ACTION INVOICES }

{ -------------------------------------------------------------- ! COMMON METHODS ! ------------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------ MOVE OPEN ITEMS DATA TO HTML TABLE }
procedure OpenItemsToHtmlTable(var HtmlReminder: string; var ReminderCount: integer; var SG: TStringGrid; ActualRow: integer);
begin
  HTMLTemp:=HTMLRow;
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[11, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[4,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[12, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[8,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[9,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[5,  ActualRow], [rfReplaceAll]);
  HtmlReminder:=HtmlReminder + HTMLTemp;
  inc(ReminderCount);
end;
{ ---------------------------------------------------------------------------------------------------------------------------------- SEND EMAIL WITH REMINDER }
procedure SendReminderEmail(ReminderNumber: integer; var RemCount: integer; var TableNum: string; var HTMLRemNum: string; var SG: TStringGrid; ActualRow: integer);
var
  SL:           TStringList;
  BankDetails:  string;
  AppSettings:  TSettings;
begin
  BankDetails:=MainForm.sgInvoiceTracker.Cells[24, iCNT];
  SL:=TStringList.Create;
  AppSettings:=TSettings.Create;
  try
    { -------------------------------------------------------------------------------------------------------------------------------------- READ LAYOUT FORM }
    if ReminderNumber = 4 then SL.LoadFromFile(AppSettings.LayoutDir + 'fm_notify.html')
      else
        SL.LoadFromFile(AppSettings.LayoutDir + SG.Cells[6, ActualRow] + '_' + IntToStr(ReminderNumber) + '.html');
    { ------------------------------------------------------------------------------------------------------------------------------------ FILL WITH THE DATA }
    TableNum  :=StringReplace(TableNum,  '{ROWS}',         HTMLRemNum, [rfReplaceAll]);
    EmailBody :=StringReplace(SL.Text,   '{INVOICE_LIST}', TableNum,   [rfReplaceAll]);
    EmailBody :=StringReplace(EmailBody, '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
    EmailBody :=StringReplace(EmailBody, '{BANKS}',        BankDetails,[rfReplaceAll]);
    { ------------------------------------------------------------------------------------------------------------------------------------- E-MAIL PARANETERS }
    if ReminderNumber < 4 then EmailTo:=SG.Cells[9,  ActualRow];
    if ReminderNumber = 4 then EmailTo:=SG.Cells[11, ActualRow];
    EmailFr   :=MainForm.sgInvoiceTracker.Cells[8,  iCNT];
    EmailCc   :=EmailFr;
    EmailBc   :='';
    EmailRt   :='';
    EmailSub  :='Unpaid invoices (' + CustName + '): Reminder ' + IntToStr(ReminderNumber) + ' / 3';
    { ------------------------------------------------------------------------------------------------------ RESET THE INVOICE COUNTER AND HTML TABLE CONTENT }
    RemCount  :=0;
    TableNum  :=HTMLTable;
    HTMLRemNum:='';
    { -------------------------------------------------------------------------------------------------------------------------------------------- SEND EMAIL }
    XMailer    :=EmailFr;
    MailFrom   :=EmailFr;
    MailTo     :=EmailTo;
    MailCc     :=EmailCc;
    MailBcc    :=EmailBc;
    MailRt     :=EmailRt;
    MailSubject:=EmailSub;
    MailBody   :=EmailBody;
    Self.idThd :=idThd;
    SendNow;
    { ------------------------------------------------------------------------------------------------------------------------------------------- DEBUG LINES }
    //SL.Text:=EmailBody;
    //SL.SaveToFile('e:\projects\test.html_' + IntToStr(ReminderNumber) + '.html');
  finally
    AppSettings.Free;
    SL.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------- LOG INVOICE WITH INVOICE TRACKER LIST }
procedure AddInvoiceToList(var CUID: string; InvoiceNumber: string; ReminderNumber: integer);
var
  AppSettings:  TSettings;
  DataBase:     TDataBase;
  Stamp:        string;
  Query:        TADOQuery;
  StrSQL:       string;
  Return:       integer;
begin
    { -------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
    Stamp:=DateTimeToStr(Now);
    Query:=TADOQuery.Create(nil);
    Query.Connection:=MainForm.FDbConnect;
    AppSettings:=TSettings.Create;
    DataBase:=TDataBase.Create(False);
    try
      { ------------------------------------------------------------------------------------------------------------------------------- LOG TO 'TBL_INVOICES' }
      Query.SQL.Clear;
      StrSQL:='INSERT INTO tbl_invoices (SK, CUID, INVOICENO, INVOICESTATE, STAMP) ' +
              'VALUES ( (SELECT ID FROM tbl_tracker WHERE CUID = ' + QuotedStr(CUID) + '), ' + QuotedStr(CUID) + ', ' + QuotedStr(InvoiceNumber) + ', ' + QuotedStr(IntToStr(ReminderNumber)) + ', ' + QuotedStr(Stamp) + ' )';
      Query.SQL.Text:=StrSQL;
      Query.ExecSQL;
      { ---------------------------------------------------------------------------------------------------------------------------------- LOG TO 'TBL_DAILY' }
      Query.SQL.Clear;
      StrSQL:='SELECT EMAIL FROM tbl_daily WHERE CUID = ' + QuotedStr(CUID) + ' AND AGEDATE = ' + QuotedStr(MainForm.GroupListDates.Text);
      Query.SQL.Text:=StrSQL;
      Query.Open;
      { ------------------------------------------------------------------------------------------------------------------------------------- UPDATE EXISTING }
      if Query.RecordCount = 1 then
      begin
        { CHECK WHAT VALUE WE HAVE }
        Query.Recordset.MoveFirst;
        Return:=Query.Recordset.Fields[0].Value;
        Inc(Return);
        Query.Close;
        { MAKE NEW SQL }
        Query.SQL.Clear;
        StrSQL:='UPDATE tbl_daily SET EMAIL = ' + QuotedStr(IntToStr(Return)) +
                ' WHERE CUID = '                + QuotedStr(CUID) +
                ' AND GROUP_ID = '              + QuotedStr(MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 0]) +
                ' AND AGEDATE = '               + QuotedStr(MainForm.GroupListDates.Text);
        Query.SQL.Text:=StrSQL;
        Query.ExecSQL;
      end else
        { -------------------------------------------------------------------------------------------------------------------------------- LOG EMAIL REMINDER }
        if Query.RecordCount = 0 then
        begin
          Query.Close;
          Query.SQL.Clear;
          { MAKE NEW SQL }
          StrSQL:='INSERT INTO tbl_daily (GROUP_ID, CUID, AGEDATE, STAMP, USER_ALIAS, EMAIL) VALUES ('
                       + QuotedStr(MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 0]) + ', '
                       + QuotedStr(CUID) + ', '
                       + QuotedStr(MainForm.GroupListDates.Text) + ', '
                       + QuotedStr(Stamp) + ', '
                       + QuotedStr(UpperCase(MainForm.FUserName)) + ', '
                       + QuotedStr('1') + ')';
          Query.SQL.Text:=StrSQL;
          { APPLY PARAMETERS }
          Query.ExecSQL;
        end;
    finally
      DataBase.Free;
      AppSettings.Free;
      Query.Free;
    end;
end;

{ --------------------------------------------------------------------------------------------------------- CHECK IF GIVEN REMINDER WAS SENT FOR THIS INVOICE }
function CheckInvoiceState(InvNum: string; SearchState: integer): boolean;
var
  Query:   TADOQuery;
  StrSQL:  string;
begin
  { INITIALIZE }
  Result:=False;
  Query:=TADOQuery.Create(nil);
  Query.Connection:=MainForm.FDbConnect;
  if SearchState = 0 then StrSQL:='SELECT INVOICESTATE FROM tbl_invoices WHERE INVOICENO = ' + QuotedStr(InvNum)
    else
      StrSQL:='SELECT INVOICESTATE FROM tbl_invoices WHERE INVOICENO = :uParam1 AND INVOICESTATE = ' + QuotedStr(IntToStr(SearchState));
  try
    { EXCUTE }
    Query.SQL.Clear;
    Query.SQL.Add(StrSQL);
    Query.Open;
    if Query.RecordCount = 0  then Result:=False;
    if SearchState = 0 then
      { WE CAN HAVE MORE THAN ONE INVOICE STATES }
      if Query.RecordCount >= 1 then Result:=True;
    if not SearchState = 0 then
      { WE CAN HAVE JUST ONE INVOICE STATE, THE ONE WE LOOK FOR }
      if Query.RecordCount = 1 then Result:=True;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{ --------------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------- }

begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Rem1:=0;
  Rem2:=0;
  Rem3:=0;
  Leg0:=0;
  SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Processing reminders...')));
  { -------------------------------------------------------------------------------------------------------- HTML TABLE WITH COLUMNS AND PLACEHOLDER FOR ROWS }
  HTMLTable:='<table class="data">'                   +#13#10+
             '<!-- HEADERS -->'                       +#13#10+
             '<tr>'                                   +#13#10+
             '  <!-- COLUMNS -->'                     +#13#10+
             '  <th class="col1">Invoice No.:</th>'   +#13#10+
             '  <th class="col2">Invoice Date:</th>'  +#13#10+
             '  <th class="col3">Due date:</th>'      +#13#10+
             '  <th class="col4">Currency:</th>'      +#13#10+
             '  <th class="col5">Amount:</th>'        +#13#10+
             '  <th class="col6">O/S Amount:</th>'    +#13#10+
             '</tr>'                                  +#13#10+
             '  <!-- ROWS WITH DATA -->'              +#13#10+
             '{ROWS}'                                 +#13#10+
             '</table>';
  { ------------------------------------------------------------------------------------------- HTML ROW FORM TO BE FILLED WITH DATA AND PUT INTO 'HTMLTABLE' }
  HTMLRow:='<tr>'                                     +#13#10+
           '  <td class="col1">{INV_NUM}</td>'        +#13#10+
           '  <td class="col2">{INV_DAT}</td>'        +#13#10+
           '  <td class="col3">{DUE_DAT}</td>'        +#13#10+
           '  <td class="col4">{INV_CUR}</td>'        +#13#10+
           '  <td class="col5">{INV_AMT}</td>'        +#13#10+
           '  <td class="col6">{INV_OSA}</td>'        +#13#10+
           '</tr>'                                    +#13#10;
  { WE CAN PROCEED IF OPEN ITEMS AND INVOICE TRACKER ARE NOT EMPTY }
  if (MainForm.sgOpenItems.RowCount > 1) and (MainForm.sgInvoiceTracker.RowCount > 1) then
  begin
    { ---------------------------------------------------------------------------------------------------------------------------------------- PREPARE TABLES }
    Table1:=HTMLTable;
    Table2:=HTMLTable;
    Table3:=HTMLTable;
    Table4:=HTMLTable;
    { ------------------------------------------------------------------------------------ MASTER LOOP - GO THROUGH ALL CUSTOMERS LOGGED WITH INVOICE TRACKER }
    for iCNT:=1 to MainForm.sgInvoiceTracker.ColCount - 1 do
    begin
      { DATA FOR COMPARISION }
      CUID     :=MainForm.sgInvoiceTracker.Cells[2,  iCNT];
      Reminder1:=MainForm.sgInvoiceTracker.Cells[12, iCNT];
      Reminder2:=MainForm.sgInvoiceTracker.Cells[13, iCNT];
      Reminder3:=MainForm.sgInvoiceTracker.Cells[14, iCNT];
      Reminder4:=MainForm.sgInvoiceTracker.Cells[15, iCNT];
      CoCode   :=MainForm.sgInvoiceTracker.Cells[4,  iCNT];
      Branch   :=MainForm.sgInvoiceTracker.Cells[5,  iCNT];
      REM_EX1  :=MainForm.sgInvoiceTracker.Cells[19, iCNT];
      REM_EX2  :=MainForm.sgInvoiceTracker.Cells[20, iCNT];
      REM_EX3  :=MainForm.sgInvoiceTracker.Cells[21, iCNT];
      REM_EX4  :=MainForm.sgInvoiceTracker.Cells[22, iCNT];
      REM_EX5  :=MainForm.sgInvoiceTracker.Cells[23, iCNT];
      { CUSTOMER NAME }
      CustName :=MainForm.sgInvoiceTracker.Cells[3,  iCNT];
      { -------------------------------------------------------------------------------------------------------------- LOCAL LOOP - GO THROUGH ALL OPEN ITEMS }
      for jCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
      begin
        { COMPARE 'CUID' IN BOTH TABLES }
        if (CUID = MainForm.sgOpenItems.Cells[37, jCNT])

        and

           { NOTE: WE EXCLUDE INVOICES WITH 'CONTROL STATUS' THAT }
           {       IS DIFFERENT THAN GIVEN NUMBER IN THE TABLE    }

           (

           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX1) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX2) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX3) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX4) or
           ( MainForm.sgOpenItems.Cells[20, jCNT] <> REM_EX5)

           )

        then
        begin

          { BUILD HTML CODE FOR CUSTOMER ADDRESS }
          CustAddr:='<p class="p"><b>' + CustName + '</b><br />' +#13#10;
          { ADD ADDRESS FIELD IF NOT EMPTY }
          if (MainForm.sgOpenItems.Cells[21, jCNT] <> '') and (MainForm.sgOpenItems.Cells[21, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[21, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[22, jCNT] <> '') and (MainForm.sgOpenItems.Cells[22, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[22, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[23, jCNT] <> '') and (MainForm.sgOpenItems.Cells[23, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[23, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[24, jCNT] <> '') and (MainForm.sgOpenItems.Cells[24, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[24, jCNT] + '<br />' +#13#10;
          if (MainForm.sgOpenItems.Cells[25, jCNT] <> '') and (MainForm.sgOpenItems.Cells[25, jCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[25, jCNT] + '<br />' +#13#10;
          CustAddr:=CustAddr + '</p>' +#13#10;
          { COMPARE 'PAYMENT STATUS' AND REQUIREMENTS FOR REMINDER 1..3 AND LEGAL }
          { REMINDER 1 }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder1 then
          begin
            { CHECK IF IT IS NOT ON INVOICE LIST, ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 0) = False then
            begin
              OpenItemsToHtmlTable(HTMLRem1, Rem1, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 1);
            end;
          end;
          { REMINDER 2 }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder2 then
          begin
            { CHECK IF IT IS ON INVOICE LIST MARKED AS '1', ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 1) then
            begin
              OpenItemsToHtmlTable(HTMLRem2, Rem2, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 2);
            end;
          end;
          { REMINDER 3 }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder3 then
          begin
            { CHECK IF IT IS ON INVOICE LIST MARKED AS '2', ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 2) then
            begin
              OpenItemsToHtmlTable(HTMLRem3, Rem3, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 3);
            end;
          end;
          { REMINDER 4 | LEGAL ACTION }
          if MainForm.sgOpenItems.Cells[33, jCNT] = '-' + Reminder4 then
          begin
            { CHECK IF IT IS ON INVOICE LIST MARKED AS '3', ADD TO HTML TABLE AND PUT ON THE LIST }
            if CheckInvoiceState(MainForm.sgOpenItems.Cells[11, jCNT], 3) then
            begin
              OpenItemsToHtmlTable(HTMLLega, Leg0, MainForm.sgOpenItems, jCNT);
              AddInvoiceToList(CUID, MainForm.sgOpenItems.Cells[11, jCNT], 4);
            end;
          end;
        end;
      end;
      { ------------------------------------------------------------------------------------------------------------------------------- PROCESS THE REMINDERS }
      if Rem1 > 0 then SendReminderEmail(1, Rem1, Table1, HTMLRem1, MainForm.sgInvoiceTracker, iCNT);
      if Rem2 > 0 then SendReminderEmail(2, Rem2, Table2, HTMLRem2, MainForm.sgInvoiceTracker, iCNT);
      if Rem3 > 0 then SendReminderEmail(3, Rem3, Table3, HTMLRem3, MainForm.sgInvoiceTracker, iCNT);
      if Leg0 > 0 then SendReminderEmail(4, Leg0, Table4, HTMLLega, MainForm.sgInvoiceTracker, iCNT);
    end;
  end;
  SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
end;

{ ----------------------------------------------------------------------------------------------------------------------------- REFRESH INVOICE TRACKER TABLE }
procedure TInvoiceTracker.Refresh(var SG: TStringGrid; Param: string);
var
  AppSettings: TSettings;
  { SQL }
  Query:       TADOQuery;
  StrSQL:      array of string;
  Columns:     string;
  Params:      string;
  AddWhere:    string;
  { COUNTERS }
  iCNT:      integer;
  jCNT:      integer;
  { CURRENT DATE/TIME }
  Stamp:  string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------- DISABLE DRAWING 'STRINGGRID' }
  with SG do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=MainForm.FDbConnect;
  Query.SQL.Clear;
  Stamp:=DateTimeToStr(Now);
  SetLength(StrSQL, 2);
  AppSettings:=TSettings.Create;
  { --------------------------------------------------------------------------------------------------------------------------------------------- CHECK PARAM }
  if not (Param = 'ADD') and not (Param = 'REMOVE') then
  begin
    { ---------------------------------------------------------------------------------------------------------------------------------- BUILD SQL EXPRESSION }
    if Param <> 'ALL' then AddWhere:=' where tbl_tracker.user_alias = ' + QuotedStr(Param);
    StrSQL[0]:='SELECT '                                                      +
                 'tbl_tracker.user_alias       as ''User Alias'','            +
                 'tbl_tracker.cuid             as ''CUID'','                  +
                 'tbl_tracker.custname         as ''Customer Name'','         +
                 'tbl_tracker.co_code          as ''Co Code'','               +
                 'tbl_tracker.branch           as ''Agent'','                 +
                 'tbl_tracker.layout           as ''Applied Layout'','        +
                 'tbl_tracker.stamp            as ''Created'','               +
                 'tbl_company.send_note_from   as ''Send From'','             +
                 'tbl_addressbook.emails       as ''Reminder To'','           +
                 'tbl_addressbook.estatements  as ''Statement To'','          +
                 'tbl_company.legalto          as ''Notification To'','       +
                 'tbl_company.reminder1        as ''Timing (Reminder 1)'','   +
                 'tbl_company.reminder2        as ''Timing (Reminder 2)'','   +
                 'tbl_company.reminder3        as ''Timing (Reminder 3)'','   +
                 'tbl_company.legalaction      as ''Timing (Legal Note)'','   +
                 'tbl_company.first_statement  as ''1st Statement'','         +
                 'tbl_company.second_statement as ''2nd Statement'','         +
                 'tbl_company.stat_except      as ''Statement exception'','   +
                 'tbl_company.rem_ex1          as ''Exception (1)'','         +
                 'tbl_company.rem_ex2          as ''Exception (2)'','         +
                 'tbl_company.rem_ex3          as ''Exception (3)'','         +
                 'tbl_company.rem_ex4          as ''Exception (4)'','         +
                 'tbl_company.rem_ex5          as ''Exception (5)'','         +
                 'tbl_company.bankdetails      as ''Bank Account'','          +
                 'tbl_company.coname           as ''Co Name'','               +
                 'tbl_company.coaddress        as ''Co Address'','            +
                 'tbl_company.vatno            as ''VAT number'','            +
                 'tbl_company.telephone        as ''Phone number'' '          +
               'FROM '                                                        +
                 'tbl_tracker '                                               +
               'LEFT JOIN '                                                   +
                 'tbl_addressbook on tbl_tracker.cuid = tbl_addressbook.cuid '+
               'LEFT JOIN '                                                   +
                 'tbl_company on (tbl_tracker.co_code = tbl_company.co_code and tbl_tracker.branch = tbl_company.branch)' + AddWhere + ' ORDER BY tbl_tracker.co_code ASC ';
    Query.SQL.Text:=StrSQL[0];
    { ----------------------------------------------------------------------------------------------------------------------------- CONNECT, READ AND DISPLAY }
    try
      Query.Open;
      if Query.RecordCount > 0 then
      begin
        SG.ClearAll(2, 1, 1, False);
        iCNT:=0;
        SG.RowCount:=Query.RecordCount + 1;
        SG.ColCount:=Query.Recordset.Fields.Count + 1;
        while not Query.Recordset.EOF do
        begin
          for jCNT:=0 to Query.Recordset.Fields.Count - 1 do
          begin
            if iCNT = 0 then SG.Cells[jCNT + 1, iCNT]:=Query.Recordset.Fields[jCNT].Name;
            SG.Cells[jCNT + 1, iCNT + 1]:=VarToStr(Query.Recordset.Fields[jCNT].Value);
          end;
          Query.Recordset.MoveNext;
          inc(iCNT);
        end;
      end;
      Query.Close;
      SG.SetColWidth(10, 20);
    finally
      Query.Free;
      if not SG.Enabled then SG.Enabled:=True;
    end;
  end;
  { --------------------------------------------------------------------------------------------------------------------------------------------- CHECK PARAM }
  if (Param = 'ADD') or (Param = 'REMOVE') then
  begin
    { ADD ITEM TO INVOICE TRACKER LIST }
    if Param = 'ADD' then
    begin
      { --------------------------------------------------------------------------------------------------- CHECK IF SELECTED CUSTOMER IS ALREADY ON THE LIST }
      StrSQL[0]:='SELECT user_alias FROM tbl_tracker WHERE CUID = ' + QuotedStr(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1), MainForm.sgAgeView.Row]);
      Query.SQL.Add(StrSQL[0]);
      try
        Query.Open;
        { ALREADY EXISTS }
        if Query.RecordCount > 0 then SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('This customer is already on the Invoice Tracker list.')));
        { NOT ON THE LIST }
        if Query.RecordCount = 0 then
        begin
          { ---------------------------------------------------------------------------------------------------------------------------- BUILD SQL EXPRESSION }
          Query.SQL.Clear;
          Columns:='USER_ALIAS, CUID, CO_CODE, BRANCH, CUSTNAME, LAYOUT, STAMP';
          Params:=QuotedStr(UpperCase(MainForm.FUserName)) + ', ' +
                  QuotedStr(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID',   1, 1),  MainForm.sgAgeView.Row]) + ', ' +
                  QuotedStr(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CO CODE', 1, 1), MainForm.sgAgeView.Row]) + ',' +
                  QuotedStr(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('AGENT',   1, 1), MainForm.sgAgeView.Row]) + ', ' +
                  QuotedStr(MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUSTOMER NAME', 1, 1), MainForm.sgAgeView.Row]) + ', ' +
                  QuotedStr(TrackerForm.LayoutList.Text) + ', ' +
                  QuotedStr(Stamp);

          StrSQL[0]:='INSERT INTO tbl_tracker (' + Columns + ') VALUES (' + Params + ')';
          Query.SQL.Add(StrSQL[0]);
          { ----------------------------------------------------------------------------------------------------------------------------------------- EXECUTE }
          try
            Query.ExecSQL;
          finally
            Query.Close;
            if not SG.Enabled then SG.Enabled:=True;
          end;
          SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Customer has been added to the Invoice Tracker.')));
        end;
      finally
        Query.Free;
      end;
    end;
    { ---------------------------------------------------------------------------------------------------------------------------------- REMOVE FROM THE LIST }
    { 'TBL_TRACKER' IS RELATED WITH 'TBL_INVOICES'. WE USE ONE-TO-MANY RELATIONSHIP. TBL_TRACKER CONTAINS WITH CUSTOMER 'UNDER WATCH' WHILE 'TBL_INVOICES'    }
    { CONTAINS WITH INVOICES THAT HAS BEEN ACTUALLY SENT (IF ANY) THEREFORE, WE MUST REMOVE ITEMS FROM 'TBL_INVOICES' AND THEN 'TBL_TRACKER', NOT THE         }
    { OTHER WAY. IF THERE IS NO INVOICES ON THE 'TBL_INVOICES' LIST, DELETE STATEMENT RETURNS ONLY '0 ROWS AFFECTED'.                                         }
    if Param = 'REMOVE' then
    begin
      StrSQL[0]:='DELETE FROM tbl_invoices WHERE CUID = ' + QuotedStr(SG.Cells[2, SG.Row]);
      StrSQL[1]:='DELETE FROM tbl_tracker WHERE CUID = ' + QuotedStr(SG.Cells[2, SG.Row]);
      try
        for iCNT:=0 to 1 do
        begin
          Query.SQL.Clear;
          Query.SQL.Add(StrSQL[iCNT]);
          Query.ExecSQL;
        end;
      finally
        Query.Free;
        { REMOVE FROM 'STRINGGRID' }
        SG.DeleteRowFrom(1, 1);
      end;
    end;
  end;
  { ----------------------------------------------------------------------------------------------------------------------------- ENABLE DRAWING 'STRINGGRID' }
  AppSettings.Free;
  with SG do SendMessage(Handle, WM_SETREDRAW, 1, 0);
  SG.Repaint;
end;

end.
