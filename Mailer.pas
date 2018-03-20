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
  TMailer = class
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
  TDocument = class(TMailer)
  {$TYPEINFO ON}
  private
    { UNUSED }
  public
    var HTMLTable:   string;
    var HTMLTemp:    string;
    var HTMLRow:     string;
    var HTMLStat:    string;
    var HTMLLayout:  string;
    var BankDetails: string;
    var LBUAddress:  string;
    var Telephone:   string;
    var CustAddr:    string;
    var CustName:    string;
    var CUID:        string;
    var CoCode:      string;
    var Branch:      string;
    var REM_EX1:     string;
    var REM_EX2:     string;
    var REM_EX3:     string;
    var REM_EX4:     string;
    var REM_EX5:     string;
    var SourceGrid:  TStringGrid;
    var OpenItems:   TStringGrid;
    var DocType:     integer;
  published
    procedure   SaveOutput(FileName: string);
    function    LoadTemplate(FileName: string): string;
    function    GetData: boolean;
    procedure   BuildHTML;
    function    SendDocument: boolean;
  end;

{ ----------------------------------------------------------- ! IMPLEMENTATION ZONE ! ----------------------------------------------------------------------- }

implementation

uses
  ADODB, Tracker, Actions;

{ ############################################################## ! MAILER CLASS ! ########################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------- SEND USING CDOSYS | NTLM }
function TMailer.SendCDOSYS: boolean;
var
  CdoMessage:   CDO_TLB.IMessage;
  Schema:       string;
  AppSettings:  TSettings;
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
  AppSettings:=TSettings.Create;
  try
    Schema:='http://schemas.microsoft.com/cdo/configuration/';
    CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=2; (* SEND THE MESSAGE USING THE NETWORK *)
    CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=AppSettings.TMIG.ReadString(MailerCDOSYS, 'SMTP', '');
    CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=AppSettings.TMIG.ReadString(MailerCDOSYS, 'PORT', '');
    CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=2; (* NTLM *)
    CdoMessage.Configuration.Fields.item[Schema + 'NNTPAccountName' ].Value:=XMailer;
    CdoMessage.Configuration.Fields.update;
  finally
    AppSettings.Free;
  end;
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
  AppSettings:  TSettings;
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
    { ------------------------------------------------------------------------------------------------------------------------------------------ UNINITIALIZE }
    AppSettings.Free;
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

{ ############################################################## ! DOCUMENT CLASS ! ######################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------- LOAD TEMPLATE FROM FILE }
function TDocument.LoadTemplate(FileName: string): string;
var
  SL: TStringList;
begin
  { LOAD LAYOUT }
  SL:=TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result:=SL.Text;
  finally
    SL.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------- SAVE GENERATED EMAIL BODY TO FILE }
procedure TDocument.SaveOutput(FileName: string);
var
  SL: TStringList;
begin
  SL:=TStringList.Create;
  try
    SL.Text:=MailBody;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------- READ EMAIL DETAILS AND ADDRESS }
function TDocument.GetData: boolean;
var
  DataBase: TDataTables;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  MailFrom:='';
  MailTo  :='';
  DataBase:=TDataTables.Create(MainForm.FDbConnect);
  try
    { GET "EMAIL TO" FROM ADDRESSBOOK }
    DataBase.CustFilter:=WHERE + TAddressBook.CUID + EQUAL + QuotedStr(CUID);
    DataBase.OpenTable(TblAddressbook);
    if DataBase.DataSet.RecordCount = 1 then MailTo:=DataBase.DataSet.Fields[TAddressBook.ESTATEMENTS].Value;
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
procedure TDocument.BuildHTML;

  (* COMMON VARIABLES *)

  var
    iCNT: integer;
    Pos:  integer;

  (* NESTED METHOD *)

  procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
  begin
    HTMLTemp:=HTMLRow;
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[10, ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[26, ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[11, ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[7,  ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[9,  ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[5,  ActualRow], [rfReplaceAll]);
    HtmlStatement:=HtmlStatement + HTMLTemp;
  end;

  (* MAIN BLOCK *)

begin

    Pos:=0;

    HTMLTable:=CommonHTMLTable;
    HTMLRow  :=CommonHTMLRow;

    { OPEN ITEMS TO HTML TABLE }
    for iCNT:=1 to OpenItems.RowCount - 1 do
    begin
      if OpenItems.Cells[37, iCNT] = CUID then
      begin
        if Pos = 0 then Pos:=iCNT;

        { STATEMENT CONDITIONS }
        if DocType = dcStatement then
        begin
          if StrToFloatDef(OpenItems.Cells[5, iCNT], 0) <> 0 then
            OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
        end;

        { REMINDER CONDITIONS }
        if DocType = dcReminder then
        begin

           { NOTE: WE EXCLUDE INVOICES WITH 'CONTROL STATUS' THAT         }
           {       IS DIFFERENT THAN GIVEN NUMBER IN THE COMPANY TABLE    }

           if ( ( OpenItems.Cells[19, iCNT] <> REM_EX1) or
                ( OpenItems.Cells[19, iCNT] <> REM_EX2) or
                ( OpenItems.Cells[19, iCNT] <> REM_EX3) or
                ( OpenItems.Cells[19, iCNT] <> REM_EX4) or
                ( OpenItems.Cells[19, iCNT] <> REM_EX5)
              )
           and
             (
               StrToFloatDef(OpenItems.Cells[5, iCNT], 0) > 0
             )
           then
             OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

        end;

      end;
    end;

    { BUILD CUSTOMER ADDRESS FIELD }
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + CRLF;
    if (OpenItems.Cells[20, Pos] <> '') and (OpenItems.Cells[20, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[20, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[21, Pos] <> '') and (OpenItems.Cells[21, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[21, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[22, Pos] <> '') and (OpenItems.Cells[22, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[22, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[23, Pos] <> '') and (OpenItems.Cells[23, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[23, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[24, Pos] <> '') and (OpenItems.Cells[24, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[24, Pos] + '<br />' + CRLF;
    CustAddr:=CustAddr + '</p>' + CRLF;

end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
function TDocument.SendDocument;
begin
  Result:=False;
  if GetData then
  begin
    { BUILD HTML }
    BuildHTML;
    { PUT DATA INTO PLACEHOLDERS }
    HTMLTable:=StringReplace(HTMLTable,  '{ROWS}',         HTMLStat,   [rfReplaceAll]);
    MailBody :=StringReplace(HTMLLayout, '{INVOICE_LIST}', HTMLTable,  [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{BANKS}',        BankDetails,[rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{ADDR_LBU}',     LBUAddress, [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{EMAIL}',        MailFrom,   [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{TEL}',          Telephone,  [rfReplaceAll]);
    { ASSIGN AND SEND }
    XMailer    :=MailFrom;
    MailCc     :=MailFrom;
    MailBcc    :='';
    MailRt     :='';
    //Result:=SendNow;
    { DEBUG LINE }
    SaveOutput('e:\test.html');
  end;
end;

end.
