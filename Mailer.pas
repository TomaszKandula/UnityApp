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
  TStatement = class(TMailer)
  {$TYPEINFO ON}
  private
    { UNUSED }
  public
    { HTML BUILDER }
    var HTMLTable:   string;
    var HTMLTemp:    string;
    var HTMLRow:     string;
    var HTMLStat:    string;
    var BankDetails: string;
    var LBUAddress:  string;
    var Telephone:   string;
    var CustAddr:    string;
    var CustName:    string;
    var CUID:        string;
    var CoCode:      string;
    var Branch:      string;
    var SL:          TStringList;
    var Grid:        TStringGrid;
  published
    constructor Create(AgeGrid: TStringGrid);
    destructor  Destroy; override;
    function    GetData: boolean;
    procedure   BuildHTML;
    function    SendStatement: boolean;
  end;

{ ----------------------------------------------------------- ! IMPLEMENTATION ZONE ! ----------------------------------------------------------------------- }

implementation

uses
  ADODB, Tracker, Actions;

{ ############################################################# ! STATEMENT CLASS ! ######################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TStatement.Create(AgeGrid: TStringGrid);
begin
  { LOAD ACCOUNT STATEMENT LAYOUT }
  SL:=TStringList.Create;
  SL.LoadFromFile(LayoutDir + 'statements.html');
  { PREPARE HTML TEMPLATES }
  HTMLTable:=CommonHTMLTable;
  HTMLRow  :=CommonHTMLRow;
  { READ KEY CUSTOMER AND COMPANY INFORMATION }
  Grid:=AgeGrid;
  try
    CUID    :=Grid.Cells[Grid.ReturnColumn(TSnapshots.fCUID,          1, 1), Grid.Row];
    CustName:=Grid.Cells[Grid.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), Grid.Row];
    CoCode  :=Grid.Cells[Grid.ReturnColumn(TSnapshots.fCO_CODE,       1, 1), Grid.Row];
    Branch  :=Grid.Cells[Grid.ReturnColumn(TSnapshots.fAGENT,         1, 1), Grid.Row];
  finally
    Grid.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TStatement.Destroy;
begin
  SL.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------------------------------------------------------- READ EMAIL DETAILS AND ADDRESS }
function TStatement.GetData: boolean;
var
  DataBase: TDataTables;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result  :=False;
  MailFrom:='';
  MailTo  :='';
  DataBase:=TDataTables.Create(MainForm.FDbConnect);
  try
    { GET "EMAIL TO" AND "ADDRESS" FROM ADDRESSBOOK }
    DataBase.CustFilter:=WHERE + TAddressBook.CUID + EQUAL + QuotedStr(CUID);
    DataBase.OpenTable(TblAddressbook);
    if DataBase.DataSet.RecordCount = 1 then
    begin
      MailTo  :=DataBase.DataSet.Fields[TAddressBook.ESTATEMENTS].Value;
      CustAddr:=DataBase.DataSet.Fields[TAddressBook.CUSTADDR].Value;
    end;
    { GET "EMAIL FROM" AND "BANK ACCOUNT" FROM COMPANY TABLE }
    DataBase.CustFilter:=WHERE +
                           TCompany.CO_CODE +
                         EQUAL +
                           QuotedStr(CoCode) +
                         _AND +
                           TCompany.BRANCH +
                         EQUAL +
                           QuotedStr(Branch);
    DataBase.OpenTable(TblCompany);
    if DataBase.DataSet.RecordCount = 1 then
    begin
      MailFrom:=DataBase.DataSet.Fields[TCompany.SEND_NOTE_FROM].Value;
      BankDetails:=DataBase.DataSet.Fields[TCompany.BANKDETAILS].Value;
      LBUAddress:=DataBase.DataSet.Fields[TCompany.COADDRESS].Value;
    end;
    Result:=True;
  finally
    DataBase.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- MAKE HTML }
procedure TStatement.BuildHTML;

  (* COMMON VARIABLES *)

  var
    iCNT: integer;

  (* NESTED METHOD *)

  procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
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

  (* MAIN BLOCK *)

begin

    //build
    { ------------------------------------------------------------------------------------------------------------------------------------------ HTML BUILDER }
    for iCNT:=1 to ActionsForm.OpenItemsGrid.RowCount - 1 do
      if StrToFloatDef(ActionsForm.OpenItemsGrid.Cells[5, iCNT], 0) <> 0 then OpenItemsToHtmlTable(HTMLStat, ActionsForm.OpenItemsGrid, iCNT);
    //build
    { ------------------------------------------------------------------------------------------------------------------ BUILD HTML CODE FOR CUSTOMER ADDRESS }
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' +#13#10;
    CustAddr:=CustAddr + '</p>' +#13#10;




end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
function TStatement.SendStatement: boolean;
var
  iCNT: integer;
begin
  if GetData then
  begin
    BuildHTML;
    HTMLTable:=StringReplace(HTMLTable, '{ROWS}',         HTMLStat,   [rfReplaceAll]);
    MailBody :=StringReplace(SL.Text,   '{INVOICE_LIST}', HTMLTable,  [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,  '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,  '{BANKS}',        BankDetails,[rfReplaceAll]);
    MailBody :=StringReplace(MailBody,  '{ADDR_LBU}',     LBUAddress, [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,  '{EMAIL}',        MailFrom,   [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,  '{TEL}',          Telephone,  [rfReplaceAll]);
    XMailer    :=MailFrom;
    MailCc     :=MailFrom;
    MailBcc    :='';
    MailRt     :='';
    MailSubject:='Account Statement (' + CustName + ')';
    Result:=SendNow;
    //SL.Text:=MailBody;
    //SL.SaveToFile('e:\test.html');
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
