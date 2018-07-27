{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Mailer;

interface

uses
  Main, Model, Settings, SysUtils, Windows, Messages, StdCtrls, Classes, StrUtils, Variants, CDO_TLB;

{ -------------------------------------------------------------- ! MAILER CLASS ! --------------------------------------------------------------------------- }
type
  TMailer = class
  {$TYPEINFO ON}
  public
    var idThd       : integer;
    var XMailer     : string;
    var MailFrom    : string;
    var MailTo      : string;
    var MailCc      : string;
    var MailBcc     : string;
    var MailRt      : string;
    var MailSubject : string;
    var MailBody    : string;
    var Logo        : string;
  published
    function  SendEmail(oauth: integer) : boolean;
    function  SendNow: boolean;
  end;

{ ------------------------------------------------------------- ! STATEMENT CLASS ! ------------------------------------------------------------------------- }
type
  TDocument = class(TMailer)
  {$TYPEINFO ON}
  public
    var HTMLTable:   string;
    var HTMLTemp:    string;
    var HTMLRow:     string;
    var HTMLStat:    string;
    var HTMLLayout:  string;
    var BankDetails: string;
    var LBUName:     string;
    var LBUAddress:  string;
    var Telephone:   string;
    var CustAddr:    string;
    var CustName:    string;
    var CUID:        string;
    var SCUID:       string;
    var CoCode:      string;
    var Branch:      string;
    var REM_EX1:     string;
    var REM_EX2:     string;
    var REM_EX3:     string;
    var REM_EX4:     string;
    var REM_EX5:     string;
    var CustSalut:   string;
    var CustMess:    string;
    var IsOverdue:   boolean;
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
function TMailer.SendEmail(oauth: integer): boolean;
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

    if oauth = auNTLM then
    begin
      CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=cdoSendUsingPort;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=cdoNTLM;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=AppSettings.TMIG.ReadString(MailerNTLM, 'SMTP', '');
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=AppSettings.TMIG.ReadString(MailerNTLM, 'PORT', '');
    end;

    if oauth = auBASIC then
    begin
      CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=cdoSendUsingPort;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=cdoBasic;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=AppSettings.TMIG.ReadString(MailerBASIC, 'SMTP', '');
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=AppSettings.TMIG.ReadString(MailerBASIC, 'PORT', '');
      CdoMessage.Configuration.Fields.item[Schema + 'sendusername'    ].Value:=AppSettings.TMIG.ReadString(MailerBASIC, 'USERNAME', '');
      CdoMessage.Configuration.Fields.item[Schema + 'sendpassword'    ].Value:=AppSettings.TMIG.ReadString(MailerBASIC, 'PASSWORD', '');
      CdoMessage.Configuration.Fields.item[Schema + 'smtpusessl'      ].Value:=AppSettings.TMIG.ReadString(MailerBASIC, 'SSL', '');
    end;

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
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: E-mail has been sent successfully.');
  except
    on E: Exception do
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot send an e-mail. Error message has been thrown: ' + E.Message);
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
    if AppSettings.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerNTLM  then Result:=SendEmail(auNTLM);
    if AppSettings.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerBASIC then Result:=SendEmail(auBASIC);
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
  MailFrom :='';
  MailTo   :='';
  Telephone:='';
  DataBase:=TDataTables.Create(MainForm.DbConnect);
  try
    { GET "MAILTO" }
    DataBase.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
    DataBase.OpenTable(TblAddressbook);
    if DataBase.DataSet.RecordCount = 1 then MailTo:=DataBase.DataSet.Fields[TAddressBook.ESTATEMENTS].Value;
    { GET: "MAILFROM", "BANKS", "LBU ADDRESS" AND "TELEPHONE" }
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
      MailFrom   :=DataBase.DataSet.Fields[TCompany.SEND_NOTE_FROM].Value;
      BankDetails:=DataBase.DataSet.Fields[TCompany.BANKDETAILS].Value;
      LBUName    :=DataBase.DataSet.Fields[TCompany.CONAME].Value;
      LBUAddress :=DataBase.DataSet.Fields[TCompany.COADDRESS].Value;
      Telephone  :=DataBase.DataSet.Fields[TCompany.Telephone].Value;
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
    Col1: integer;
    Col2: integer;
    Col3: integer;
    Col4: integer;
    Col5: integer;

  (* NESTED METHOD *)

  procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
  begin
    HTMLTemp:=HTMLRow;
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[SG.ReturnColumn(TOpenitems.InvoNo,    1, 1), ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[SG.ReturnColumn(TOpenitems.ValDt,     1, 1), ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[SG.ReturnColumn(TOpenitems.DueDt,     1, 1), ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[SG.ReturnColumn(TOpenitems.ISO,       1, 1), ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[SG.ReturnColumn(TOpenitems.CurAm,     1, 1), ActualRow], [rfReplaceAll]);
    HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[SG.ReturnColumn(TOpenitems.OpenCurAm, 1, 1), ActualRow], [rfReplaceAll]);
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
      if OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.CUID, 1, 1), iCNT] = CUID then
      begin
        if Pos = 0 then Pos:=iCNT;

        { STATEMENT CONDITIONS }
        if DocType = dcStatement then
        begin
          if not(IsOverdue) then
          begin
            { ALL ITEMS }
            if StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.OpenAm, 1, 1), iCNT], 0) <> 0 then
              { MAKE }
              OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
          end
          else
          begin
            { ONLY OVERDUE ITEMS }
            if
              (
                StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.OpenAm, 1, 1), iCNT], 0) <> 0
              )
            and
              (
                StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.PmtStat, 1, 1), iCNT], 0) < 0
              )
            then
              { MAKE }
              OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
          end;
        end;

        { REMINDER CONDITIONS }
        if DocType = dcReminder then
        begin

           { NOTE: WE EXCLUDE INVOICES WITH 'CONTROL STATUS' THAT         }
           {       IS DIFFERENT THAN GIVEN NUMBER IN THE COMPANY TABLE    }

           if ( ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX1) or
                ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX2) or
                ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX3) or
                ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX4) or
                ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX5)
              )
           and
             ( { ONLY UNPAID INVOICES }
               StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.OpenAm, 1, 1), iCNT], 0) > 0
             )
           then
             { MAKE }
             OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

        end;

      end;
    end;

    Col1:=OpenItems.ReturnColumn(TOpenitems.Ad1,   1, 1);
    Col2:=OpenItems.ReturnColumn(TOpenitems.Ad2,   1, 1);
    Col3:=OpenItems.ReturnColumn(TOpenitems.Ad3,   1, 1);
    Col4:=OpenItems.ReturnColumn(TOpenitems.Pno,   1, 1);
    Col5:=OpenItems.ReturnColumn(TOpenitems.PArea, 1, 1);

    { BUILD CUSTOMER ADDRESS FIELD }
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + CRLF;
    if (OpenItems.Cells[Col1, Pos] <> '') and (OpenItems.Cells[Col1, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col1, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col2, Pos] <> '') and (OpenItems.Cells[Col2, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col2, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col3, Pos] <> '') and (OpenItems.Cells[Col3, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col3, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col4, Pos] <> '') and (OpenItems.Cells[Col4, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col4, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col5, Pos] <> '') and (OpenItems.Cells[Col5, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col5, Pos] + '<br />' + CRLF;
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
    MailBody :=StringReplace(MailBody,   '{NAME_LBU}',     LBUName,    [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{ADDR_LBU}',     LBUAddress, [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{EMAIL}',        MailFrom,   [rfReplaceAll]);
    MailBody :=StringReplace(MailBody,   '{TEL}',          Telephone,  [rfReplaceAll]);
    { CUSTOM SALUTATION AND MESSAGE }
    if CustSalut <> '' then MailBody:=StringReplace(MailBody, '{SALUT}', CustSalut, [rfReplaceAll]);
    if CustMess  <> '' then MailBody:=StringReplace(MailBody, '{TEXT}',  CustMess,  [rfReplaceAll]);
    { ASSIGN AND SEND }
    XMailer  :=MailFrom;
    MailCc   :=MailFrom;
    MailBcc  :='';
    MailRt   :='';
    Result   :=SendNow;
    { DEBUG LINES }
    //SaveOutput('d:\test.html'); Result:=True;
  end;
end;

end.
