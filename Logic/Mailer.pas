
{$I .\Include\Header.inc}

unit Mailer;

interface

uses
    SQL, Model, Settings, SysUtils, Windows, Messages, StdCtrls, Classes, StrUtils, Variants, CDO_TLB, InterposerClasses;

type

    /// <summary>
    ///     Base class responsible for sending email (using CDOSYS).
    /// </summary>

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

    /// <summary>
    ///     Class responsible for building account statement or reminder (generate HTML) and send it via emial.
    /// </summary>

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


implementation


uses
    Main, ADODB, Tracker, Actions;


// -------------------------------------------------------------------------------------------------------------------------------------------- MAILER CLASS //


/// <summary>
///     Send email through CDOSYS.
/// </summary>

function TMailer.SendEmail(oauth: integer): boolean;
var
    CdoMessage: CDO_TLB.IMessage;
    Schema:     string;
    Settings:   ISettings;
begin

    Result:=False;
    CdoMessage:=CDO_TLB.CoMessage.Create;
    CdoMessage.From:=MailFrom;
    CdoMessage.To_ :=MailTo;
    CdoMessage.CC  :=MailCc;

    if MailBcc <> '' then CdoMessage.BCC    :=MailBcc;
    if MailRt  <> '' then CdoMessage.ReplyTo:=MailRt;

    CdoMessage.Subject :=MailSubject;
    CdoMessage.HTMLBody:=MailBody;

    // Configure
    Settings:=TSettings.Create;

    Schema:='http://schemas.microsoft.com/cdo/configuration/';

    if oauth = auNTLM then
    begin
      CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=cdoSendUsingPort;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=cdoNTLM;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=Settings.GetStringValue(MailerNTLM, 'SMTP', '');
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=Settings.GetStringValue(MailerNTLM, 'PORT', '');
    end;

    if oauth = auBASIC then
    begin
      CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=cdoSendUsingPort;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=cdoBasic;
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=Settings.GetStringValue(MailerBASIC, 'SMTP', '');
      CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=Settings.GetStringValue(MailerBASIC, 'PORT', '');
      CdoMessage.Configuration.Fields.item[Schema + 'sendusername'    ].Value:=Settings.GetStringValue(MailerBASIC, 'USERNAME', '');
      CdoMessage.Configuration.Fields.item[Schema + 'sendpassword'    ].Value:=Settings.GetStringValue(MailerBASIC, 'PASSWORD', '');
      CdoMessage.Configuration.Fields.item[Schema + 'smtpusessl'      ].Value:=Settings.GetStringValue(MailerBASIC, 'SSL', '');
    end;

    CdoMessage.Configuration.Fields.item[Schema + 'NNTPAccountName' ].Value:=XMailer;
    CdoMessage.Configuration.Fields.update;

    try
        CdoMessage.BodyPart.Charset:='utf-8';
        CdoMessage.Send;
        Result:=True;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: E-mail has been sent successfully.');
    except
        on E: Exception do
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot send an e-mail. Error message has been thrown: ' + E.Message);
    end;

end;

/// <summary>
///     Send routine.
/// </summary>

function TMailer.SendNow: boolean;
var
    Settings: ISettings;
begin
    Result:=False;
    Settings:=TSettings.Create;

    if Settings.GetStringValue(MailerSetup, 'ACTIVE', '') = MailerNTLM then
        Result:=SendEmail(auNTLM);

    if Settings.GetStringValue(MailerSetup, 'ACTIVE', '') = MailerBASIC then
        Result:=SendEmail(auBASIC);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------ DOCUMENT CLASS //


/// <summary>
///     Lead template from file.
/// </summary>

function TDocument.LoadTemplate(FileName: string): string;
var
    SL: TStringList;
begin
    SL:=TStringList.Create;
    try
        SL.LoadFromFile(FileName);
        Result:=SL.Text;
    finally
        SL.Free;
    end;
end;

/// <summary>
///     Save generated email body to a file.
/// </summary>

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

/// <summary>
///     Get necessary details for sending email.
/// </summary>

function TDocument.GetData: boolean;
var
    DataBase: TDataTables;
begin

    MailFrom :='';
    MailTo   :='';
    Telephone:='';

    DataBase:=TDataTables.Create(MainForm.DbConnect);
    try

        // skip mailto if serios "on"

        // Get "MAILTO"
        DataBase.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
        DataBase.OpenTable(TblAddressbook);
        if DataBase.DataSet.RecordCount = 1 then MailTo:=DataBase.DataSet.Fields[TAddressBook.ESTATEMENTS].Value;

        // Get "Mail from", "Banks", "LBU address" AND "Telephone"
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

/// <summary>
///     Generate HTML code containing open items.
/// </summary>

procedure TDocument.BuildHTML;

    // Common variables

    var
        iCNT:       integer;
        Pos:        integer;
        Col1:       integer;
        Col2:       integer;
        Col3:       integer;
        Col4:       integer;
        Col5:       integer;
        CuidCol:    integer;

    // Nested method

    procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
    begin
        HTMLTemp:=HTMLRow;
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[SG.ReturnColumn(TOpenitems.InvoNo,    1, 1), ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[SG.ReturnColumn(TOpenitems.ValDt,     1, 1), ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[SG.ReturnColumn(TOpenitems.DueDt,     1, 1), ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[SG.ReturnColumn(TOpenitems.ISO,       1, 1), ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[SG.ReturnColumn(TOpenitems.CurAm,     1, 1), ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[SG.ReturnColumn(TOpenitems.OpenCurAm, 1, 1), ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_CRL}', SG.Cells[SG.ReturnColumn(TOpenitems.Ctrl,      1, 1), ActualRow], [rfReplaceAll]);
        HtmlStatement:=HtmlStatement + HTMLTemp;
    end;

    // Main block

begin

    // Get all column numbers

    CuidCol:=OpenItems.ReturnColumn(TOpenitems.CUID, 1, 1);


    Pos:=0;

    HTMLTable:=CommonHTMLTable;
    HTMLRow  :=CommonHTMLRow;

    // Open items to HTML table
    for iCNT:=1 to OpenItems.RowCount - 1 do
    begin
        if OpenItems.Cells[CuidCol, iCNT] = CUID then
        //if OpenItems.Cells[34, iCNT] = CUID then
        begin

            if Pos = 0 then Pos:=iCNT;

            // Statement conditions
            if DocType = dcStatement then
            begin
                if not(IsOverdue) then
                begin
                    // All items
                    if StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.OpenAm, 1, 1), iCNT], 0) <> 0 then
                    // Generate HTML table
                    OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
                end
                else
                begin
                    // Allow only overdue invoices
                    if
                    (
                        StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.OpenAm, 1, 1), iCNT], 0) <> 0
                    )
                    and
                    (
                        StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.PmtStat, 1, 1), iCNT], 0) < 0
                    )
                    then
                    // Make
                    OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
                end;
            end;

            // Reminder conditions
            if DocType = dcReminder then
            begin

                /// <remarks>
                ///     We exclude invoices with 'control status' that is different than given number in the comapny table.
                /// </remarks>

                if ( ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX1) or
                     ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX2) or
                     ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX3) or
                     ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX4) or
                     ( OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.Ctrl, 1, 1), iCNT] <> REM_EX5)
                   )
                and
                  (
                    // Only unpaid invoices
                    StrToFloatDef(OpenItems.Cells[OpenItems.ReturnColumn(TOpenitems.OpenAm, 1, 1), iCNT], 0) > 0
                  )
                then

                // Make
                OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

            end;

        end;

    end;

    Col1:=OpenItems.ReturnColumn(TOpenitems.Ad1,   1, 1);
    Col2:=OpenItems.ReturnColumn(TOpenitems.Ad2,   1, 1);
    Col3:=OpenItems.ReturnColumn(TOpenitems.Ad3,   1, 1);
    Col4:=OpenItems.ReturnColumn(TOpenitems.Pno,   1, 1);
    Col5:=OpenItems.ReturnColumn(TOpenitems.PArea, 1, 1);

    // Build customer address field
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + CRLF;
    if (OpenItems.Cells[Col1, Pos] <> '') and (OpenItems.Cells[Col1, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col1, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col2, Pos] <> '') and (OpenItems.Cells[Col2, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col2, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col3, Pos] <> '') and (OpenItems.Cells[Col3, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col3, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col4, Pos] <> '') and (OpenItems.Cells[Col4, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col4, Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[Col5, Pos] <> '') and (OpenItems.Cells[Col5, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[Col5, Pos] + '<br />' + CRLF;
    CustAddr:=CustAddr + '</p>' + CRLF;

end;

/// <summary>
///     Sen statement/reminder on given email address.
/// </summary>

function TDocument.SendDocument;

var RAND: integer;

begin
    Result:=False;

    if GetData then
    begin

        BuildHTML;

        // Put data into placeholders
        HTMLTable:=StringReplace(HTMLTable,  '{ROWS}',         HTMLStat,   [rfReplaceAll]);
        MailBody :=StringReplace(HTMLLayout, '{INVOICE_LIST}', HTMLTable,  [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,   '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,   '{BANKS}',        BankDetails,[rfReplaceAll]);
        MailBody :=StringReplace(MailBody,   '{NAME_LBU}',     LBUName,    [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,   '{ADDR_LBU}',     LBUAddress, [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,   '{EMAIL}',        MailFrom,   [rfReplaceAll]);
        MailBody :=StringReplace(MailBody,   '{TEL}',          Telephone,  [rfReplaceAll]);

        // Custom salutation and the message
        if CustSalut <> '' then MailBody:=StringReplace(MailBody, '{SALUT}', CustSalut, [rfReplaceAll]);
        if CustMess  <> '' then MailBody:=StringReplace(MailBody, '{TEXT}',  CustMess,  [rfReplaceAll]);

        XMailer  :=MailFrom;
        MailCc   :=MailFrom;
        MailBcc  :='';
        MailRt   :='';
        //Result   :=SendNow;

        // Debug lines
        RAND:=Random(100000);
        SaveOutput('i:\test' + IntToStr(RAND) + '.html');
        Result:=True;

    end;

end;


end.
