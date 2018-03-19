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
  Main, Model, Settings, SysUtils, Windows, Messages, StdCtrls, Classes, StrUtils, Variants, CDO_TLB,
  blcksock, smtpsend { MODIFIED FROM ORIGINAL }, pop3send, ssl_openssl, synautil, synacode, mimemess { MODIFIED FROM ORIGINAL };


{ -------------------------------------------------------------- ! MAILER CLASS ! --------------------------------------------------------------------------- }
type
  TMailer = class(TSettings)
  {$TYPEINFO ON}
  private
    var pidThd     : integer;
    var pXMailer   : string;
    var pmFrom     : string;
    var pmTo       : string;
    var pmCc       : string;
    var pmBcc      : string;
    var pmRt       : string;
    var pmSubject  : string;
    var pmBody     : string;
    var pmLogo     : string;
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
    function    SendCDOSYS : boolean;
    function    SendSynapse: boolean;
    function    SendNow    : boolean;
  end;

{ ------------------------------------------------------------- ! STATEMENT CLASS ! ------------------------------------------------------------------------- }
type
  TStatement = class(TMailer)                             // refactor! remove tight coupling!
  {$TYPEINFO ON}
  private
    { UNUSED }
  public
    { HTML FOR INVOICE LIST }
    var HTMLTable:   string;
    var HTMLTemp:    string;
    var HTMLRow:     string;
    var HTMLStat:    string;
    var CustAddr:    string;
    var CustName:    string;
    { REAY EMAIL BODY }
    var BankDetails: string;
    var LBUAddress:  string;
    var Telephone:   string;
    var SL:          TStringList;
    var CUID:        string;
  published
    procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: integer);
    function  RetriveEmails(InvoiceGrid: TStringGrid; AgeGrid: TStringGrid): boolean;
    function  SendStatement: boolean;
  end;

{ -------------------------------------------------------------- ! REMINDER CLASS ! ------------------------------------------------------------------------- }
(*
type
  TReminder = class(TMailer)
  {$TYPEINFO ON}
  private
    { UNUSED }
  public
    { UNUSED }
  published
    function CheckInvoiceState: boolean;
    function RegisterReminder: boolean;
    function SendReminder: boolean;
  end;
*)

{ ----------------------------------------------------------- ! IMPLEMENTATION ZONE ! ----------------------------------------------------------------------- }

implementation

uses
  ADODB, Tracker, Actions;

{ ############################################################# ! STATEMENT CLASS ! ######################################################################### }

{ --------------------------------------------------------------------------------------------------------------------------------- CONVERT DATA LINE TO HTML }
procedure TStatement.OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
begin
  HTMLTemp:=HTMLRow;
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[1,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[10, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[9,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[8,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[5,  ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[4,  ActualRow], [rfReplaceAll]);
  HtmlStatement:=HtmlStatement + HTMLTemp;
end;

{ ---------------------------------------------------------------------------------------------------------------------------- READ EMAIL DETAILS AND ADDRESS }
function TStatement.RetriveEmails(InvoiceGrid: TStringGrid; AgeGrid: TStringGrid): boolean;
var
  iCNT: integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  MailFrom:='';
  MailTo:='';
  { REFRESH INVOICE TRACKER }
  TrackerForm.UserAlias:=MainForm.FUserName;
  TrackerForm.Show;
  { --------------------------------------------------------------------------------------- LOOK FOR EMAILS AND BANK DETAILS ON THE INVOICE TRACKER COMPONENT }
  if InvoiceGrid.RowCount > 1 then
    for iCNT:=1 to InvoiceGrid.RowCount - 1 do
      if CUID = InvoiceGrid.Cells[2, iCNT] then
      begin
        MailFrom   :=InvoiceGrid.Cells[8,  iCNT];
        MailTo     :=InvoiceGrid.Cells[10, iCNT];
        BankDetails:=InvoiceGrid.Cells[24, iCNT];
        LBUAddress :=InvoiceGrid.Cells[25, iCNT] + #13#10 + '<br>' +
                     InvoiceGrid.Cells[26, iCNT] + #13#10 + '<br>' +
                     InvoiceGrid.Cells[27, iCNT];
        Telephone  :=InvoiceGrid.Cells[28, iCNT];
        if (MailFrom <> '') and (MailTo <> '') then Result:=True;
        Break;
      end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
function TStatement.SendStatement: boolean;
var
  iCNT: integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  CUID    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,          1, 1), MainForm.sgAgeView.Row];
  CustName:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), MainForm.sgAgeView.Row];

  if (ActionsForm.Cust_Mail.Text = '') or (ActionsForm.Cust_Mail.Text = ' ') or (ActionsForm.Cust_Mail.Text = unNotFound) then
  begin
    MainForm.MsgCall(mcWarn, 'Statement cannot be sent. There is no e-mail provided.');
    Exit;
  end;

  HTMLTable:=CommonHTMLTable;
  HTMLRow  :=CommonHTMLRow;

  SL:=TStringList.Create;

  if ActionsForm.OpenItemsGrid.RowCount > 2 then
  begin

    { --------------------------------------------------------------------------------------------------------------------------- SKIP FIRST ROW BEING HEADER }
    for iCNT:=1 to ActionsForm.OpenItemsGrid.RowCount - 1 do
      if StrToFloatDef(ActionsForm.OpenItemsGrid.Cells[5, iCNT], 0) <> 0 then OpenItemsToHtmlTable(HTMLStat, ActionsForm.OpenItemsGrid, iCNT);

    { ---------------------------------------------------------------------------------------------------------------- GET CUID POSITION FROM OPEN ITEMS LIST }
    for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
      if MainForm.sgOpenItems.Cells[37, iCNT] = CUID then break;

    { ------------------------------------------------------------------------------------------------------------------ BUILD HTML CODE FOR CUSTOMER ADDRESS }
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' +#13#10;

    { ------------------------------------------------------------------------------------------------------------------------ ADD ADDRESS FIELD IF NOT EMPTY }
    if (MainForm.sgOpenItems.Cells[20, iCNT] <> '') and (MainForm.sgOpenItems.Cells[20, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[20, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[21, iCNT] <> '') and (MainForm.sgOpenItems.Cells[21, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[21, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[22, iCNT] <> '') and (MainForm.sgOpenItems.Cells[22, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[22, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[23, iCNT] <> '') and (MainForm.sgOpenItems.Cells[23, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[23, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[24, iCNT] <> '') and (MainForm.sgOpenItems.Cells[24, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[24, iCNT] + '<br />' +#13#10;

    CustAddr:=CustAddr + '</p>' +#13#10;

    { ----------------------------------------------------------------------------------------------------------------------------------- FILL THE STATEMENT  }
    try
      { --------------------------------------------------------------------------------------------------------------------------------------------- PREPARE }
      SL.LoadFromFile(LayoutDir + 'statement.html');
      { ----------------------------------------------------------------------------------------------------------------------- SEND AN E-MAIL WITH STATEMENT }
      if RetriveEmails(MainForm.sgInvoiceTracker, MainForm.sgAgeView) then
      begin
        { HTML BODY }
        HTMLTable:=StringReplace(HTMLTable, '{ROWS}',         HTMLStat,   [rfReplaceAll]);
        MailBody :=StringReplace(SL.Text,   '{INVOICE_LIST}', HTMLTable,  [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,  '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,  '{BANKS}',        BankDetails,[rfReplaceAll]);
        MailBody :=StringReplace(MailBody,  '{ADDR_LBU}',     LBUAddress, [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,  '{EMAIL}',        MailFrom,   [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,  '{TEL}',          Telephone,  [rfReplaceAll]);
        { EMAILS }
        XMailer    :=MailFrom;
        MailCc     :=MailFrom;
        MailBcc    :='';
        MailRt     :='';
        MailSubject:='Account Statement (' + CustName + ')';
(*
        if SendNow then
        begin
          MainForm.MsgCall(mcInfo, 'Statement has been sent successfully!')
        end
          else
            MainForm.MsgCall(mcWarn, 'Cannot send statement from e-mail address: ' + MailFrom + '. Please contact IT support.');
*)
        { ----------------------------------------------------------------------------------------------------------------------- DEBUG LINES | DO NOT DELETE }
        SL.Text:=MailBody;
        SL.SaveToFile('e:\test.html');
      end
        else
          MainForm.MsgCall(mcWarn, 'Cannot send statement from e-mail address: ' + MailFrom + '. Please make sure that this customer is registered on Invoice Tracker list.');
    finally
      SL.Free;
    end;
  end;
end;


{ ############################################################## ! MAILER CLASS ! ########################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------- SEND USING CDOSYS | NTLM }
function TMailer.SendCDOSYS: boolean;
var
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
  CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=2; (* SEND THE MESSAGE USING THE NETWORK *)
  CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=TMIG.ReadString(MailerCDOSYS, 'SMTP', '');
  CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=TMIG.ReadString(MailerCDOSYS, 'PORT', '');
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
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot send an e-mail. Error message has been thrown: ' + E.Message);
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- SEND USING SYNAPSE }
function TMailer.SendSynapse;
var
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
      Email.UserName  :=TMIG.ReadString(MailerSynapse, 'USERNAME', '');
      Email.Password  :=TMIG.ReadString(MailerSynapse, 'PASSWORD', '');
      Email.TargetHost:=TMIG.ReadString(MailerSynapse, 'SMTP', '');
      Email.TargetPort:=TMIG.ReadString(MailerSynapse, 'PORT', '');
      { ---------------------------------------------------------------------------------------------------------------------------------------- TLS OVER SSL }
      if TMIG.ReadBool(MailerSynapse, 'TLS', False) = True then
      begin
        Email.AutoTLS:=True;
        Email.FullSSL:=False;
      end;
      { ---------------------------------------------------------------------------------------------------------------------------------------- SSL OVER TLS }
      if TMIG.ReadBool(MailerSynapse, 'SSL', True) = True then
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
    { ------------------------------------------------------------------------------------------------------------------------------------------ UNINITIALIZE }
    MailContent.Free;
    Msg.Free;
    Email.Free;
    SendTo.Free;
    SendCc.Free;
    SendBc.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- SEND E-MAIL }
function TMailer.SendNow: boolean;
begin
  Result:=False;
  (* NTLM AUTHENTICATE ONLY *)
  if TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerCDOSYS then Result:=SendCDOSYS;
  (* REQUIRE USERNAME AND PASSWORD, SSL/TLS *)
  if TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerSYNAPSE then Result:=SendSynapse;
end;

end.
