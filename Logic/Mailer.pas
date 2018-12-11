
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
    private
        var FidThd       : integer;
        var FXMailer     : string;
        var FMailFrom    : string;
        var FMailTo      : string;
        var FMailCc      : string;
        var FMailBcc     : string;
        var FMailRt      : string;
        var FMailSubject : string;
        var FMailBody    : string;
        var FLogo        : string;
    public
        property idThd       : integer read FidThd       write FidThd;
        property XMailer     : string  read FXMailer     write FXMailer;
        property MailFrom    : string  read FMailFrom    write FMailFrom;
        property MailTo      : string  read FMailTo      write FMailTo;
        property MailCc      : string  read FMailCc      write FMailCc;
        property MailBcc     : string  read FMailBcc     write FMailBcc;
        property MailRt      : string  read FMailRt      write FMailRt;
        property MailSubject : string  read FMailSubject write FMailSubject;
        property MailBody    : string  read FMailBody    write FMailBody;
        property Logo        : string  read FLogo        write FLogo;
    published
        function  SendEmail(oauth: integer) : boolean;
        function  SendNow: boolean;
    end;

    /// <summary>
    ///     Class responsible for building account statement or reminder (generate HTML) and send it via emial.
    /// </summary>

    TDocument = class(TMailer)
    {$TYPEINFO ON}
    protected
        var HTMLStat:   string;
    private
        var FHTMLTable:   string;
        var FHTMLTemp:    string;
        var FHTMLRow:     string;
        var FHTMLLayout:  string;
        var FCustName:    string;
        var FCustAddr:    string;
        var FLBUName:     string;
        var FLBUAddress:  string;
        var FTelephone:   string;
        var FBankDetails: string;
        var FCustMess:    string;
        var FIsOverdue:   boolean;
        var FSourceGrid:  TStringGrid;
        var FDocType:     integer;
        var FCUID:        string;
        var FREM_EX1:     string;
        var FREM_EX2:     string;
        var FREM_EX3:     string;
        var FREM_EX4:     string;
        var FREM_EX5:     string;
    public
        var OpenItems:  TStringGrid;
        property HTMLTable:   string      read FHTMLTable   write FHTMLTable;
        property HTMLTemp:    string      read FHTMLTemp    write FHTMLTemp;
        property HTMLRow:     string      read FHTMLRow     write FHTMLRow;
        property HTMLLayout:  string      read FHTMLLayout  write FHTMLLayout;
        property CustName:    string      read FCustName    write FCustName;
        property CustAddr:    string      read FCustAddr    write FCustAddr;
        property LBUName:     string      read FLBUName     write FLBUName;
        property LBUAddress:  string      read FLBUAddress  write FLBUAddress;
        property Telephone:   string      read FTelephone   write FTelephone;
        property BankDetails: string      read FBankDetails write FBankDetails;
        property CustMess:    string      read FCustMess    write FCustMess;
        property IsOverdue:   boolean     read FIsOverdue   write FIsOverdue;
        property SourceGrid:  TStringGrid read FSourceGrid  write FSourceGrid;
        property DocType:     integer     read FDocType     write FDocType;
        property CUID:        string      read FCUID        write FCUID;
        property REM_EX1:     string      read FREM_EX1     write FREM_EX1;
        property REM_EX2:     string      read FREM_EX2     write FREM_EX2;
        property REM_EX3:     string      read FREM_EX3     write FREM_EX3;
        property REM_EX4:     string      read FREM_EX4     write FREM_EX4;
        property REM_EX5:     string      read FREM_EX5     write FREM_EX5;
    published
        procedure SaveOutput(FileName: string);
        function  LoadTemplate(FileName: string): string;
        procedure BuildHTML;
        function  SendDocument: boolean;
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
///     Generate HTML code containing open items.
/// </summary>

procedure TDocument.BuildHTML;

    // Common variables

    var
        iCNT:           integer;
        Pos:            integer;

    // Nested methods

    // Replace status code to text (short code description)
    function StatusCodeToText(TextCode: string; Source: TStringGrid): string;
    var
        iCNT: integer;
    begin

        for iCNT:=1 to Source.RowCount do
        begin

            if Source.Cells[MainForm.ControlStatusRefs.Code, iCNT] = TextCode then
            begin
                Result:=Source.Cells[MainForm.ControlStatusRefs.Text, iCNT];
                Break;
            end;

        end;

    end;

    // Move open items to HTML template
    procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
    var
        CurAmount: string;
        Amount:    string;
    begin

        // Get outstanding amounts
        CurAmount:=SG.Cells[MainForm.OpenItemsRefs.CurAmCol, ActualRow];
        Amount   :=SG.Cells[MainForm.OpenItemsRefs.OpenCurAmCol, ActualRow];

        // Format number (from 1000,00 to 1 000,00)
        CurAmount:=FormatFloat('#,##0.00', StrToFloat(CurAmount));
        Amount   :=FormatFloat('#,##0.00', StrToFloat(Amount));

        // Generate HTML
        HTMLTemp:=HTMLRow;
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[MainForm.OpenItemsRefs.InvoNoCol, ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[MainForm.OpenItemsRefs.ValDtCol,  ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[MainForm.OpenItemsRefs.DueDtCol,  ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[MainForm.OpenItemsRefs.ISOCol,    ActualRow], [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', CurAmount, [rfReplaceAll]);
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', Amount,    [rfReplaceAll]);

        // Replace control status number to description
        HTMLTemp:=StringReplace(
            HTMLTemp,
            '{INV_CRL}',
            StatusCodeToText(SG.Cells[MainForm.OpenItemsRefs.CtrlCol, ActualRow], MainForm.sgControlStatus),
            [rfReplaceAll]
        );

        HtmlStatement:=HtmlStatement + HTMLTemp;
    end;

    // Main block

begin

    Pos:=0;

    // Get templates
    HTMLTable:=CommonHTMLTable;
    HTMLRow  :=CommonHTMLRow;

    // Open items to HTML table
    for iCNT:=1 to OpenItems.RowCount - 1 do
    begin
        if OpenItems.Cells[MainForm.OpenItemsRefs.CuidCol, iCNT] = CUID then
        begin

            if Pos = 0 then Pos:=iCNT;

            // Statement conditions
            if DocType = dcStatement then
            begin

                // Exclude items with control status indicated by field "ReminderException5"
                if OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> '514' {REM_EX5} then    // REFACTOR!!!
                begin

                    if not(IsOverdue) then
                    begin
                        // All items
                        if StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) <> 0 then
                            // Generate HTML table
                            OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
                    end
                    else
                    begin
                        // Allow only overdue invoices
                        if
                        (
                            StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) <> 0
                        )
                        and
                        (
                            StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.PmtStatCol, iCNT], 0) < 0
                        )
                        then
                        // Make
                        OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
                    end;

                end;

            end;

            // Reminder conditions
            if DocType = dcReminder then
            begin

                /// <remarks>
                ///     We exclude invoices with 'control status' that is different than given number in the comapny table.
                /// </remarks>

                if ( ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX1) or
                     ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX2) or
                     ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX3) or
                     ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX4) or
                     ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX5)
                   )
                and
                  (
                    // Only unpaid invoices
                    StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) > 0
                  )
                then

                // Make
                OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

            end;

        end;

    end;

    // Build customer address field
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,Pos] + '<br />' + CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, Pos] + '<br />' + CRLF;
    CustAddr:=CustAddr + '</p>' + CRLF;

end;

/// <summary>
///     Sen statement/reminder on given email address.
/// </summary>

function TDocument.SendDocument;
var RAND: integer; (* DEBUG *)
begin
    //Result:=False;

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

    // Custom template title (statement or reminder)
    if IsOverdue then
        MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER', [rfReplaceAll])
            else
                MailBody:=StringReplace(MailBody, '{TITLE}', 'STATEMENT', [rfReplaceAll]);

    // Custom salutation and the message
    if CustMess  <> '' then MailBody:=StringReplace(MailBody, '{TEXT}',  CustMess,  [rfReplaceAll]);

    XMailer  :=MailFrom;
    MailCc   :=MailFrom;
    MailBcc  :='';
    MailRt   :='';
    //Result   :=SendNow;

    (* DEBUG *)
    RAND:=Random(100000);
    SaveOutput('I:\temp\test' + IntToStr(RAND) + '.html');
    Result:=True;

end;


end.
