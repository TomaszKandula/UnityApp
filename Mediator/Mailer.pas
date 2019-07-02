unit Mailer;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    System.StrUtils,
    System.Variants,
    System.Generics.Collections,
    Vcl.StdCtrls,
    CDO_TLB,
    InterposerClasses,
    Helpers,
    Statics;


type


    IMailer = Interface(IInterface)
    ['{3D803B98-BE4F-49A4-A2B5-7F323772E5B4}']
        function SendNow: boolean;
    end;


    TMailer = class(TInterfacedObject, IMailer)
    {$TYPEINFO ON}
    private
        var FidThd:       integer;
        var FXMailer:     string;
        var FMailFrom:    string;
        var FMailTo:      string;
        var FMailCc:      string;
        var FMailBcc:     string;
        var FMailRt:      string;
        var FMailSubject: string;
        var FMailBody:    string;
        var FAttachments: TList<string>;
    public
        type TAuthTypes = (cdoAnonymous, cdoBasic, cdoNTLM);
        type TAuthUsing = (cdoNone, cdoSendUsingPickup, cdoSendUsingPort, cdoSendUsingExchange);
        property idThd:       integer       read FidThd       write FidThd;
        property XMailer:     string        read FXMailer     write FXMailer;
        property MailFrom:    string        read FMailFrom    write FMailFrom;
        property MailTo:      string        read FMailTo      write FMailTo;
        property MailCc:      string        read FMailCc      write FMailCc;
        property MailBcc:     string        read FMailBcc     write FMailBcc;
        property MailRt:      string        read FMailRt      write FMailRt;
        property MailSubject: string        read FMailSubject write FMailSubject;
        property MailBody:    string        read FMailBody    write FMailBody;
        property Attachments: TList<string> read FAttachments write FAttachments;
        function SendEmail(OAuth: TAuthTypes): boolean;
        function SendNow: boolean;
        constructor Create;
        destructor Destroy; override;
    end;


    IDocument = Interface(IInterface)
    ['{C3D66D48-891B-438B-9EB6-F53B62E2FCAD}']
        function LoadTemplate(FileName: string; CrlStatVisible: boolean = true): string;
        function SendDocument: boolean;
    End;


    TDocument = class(TMailer, IDocument)
    {$TYPEINFO ON}
    protected
        var HTMLStat: string;
    private
        var FHTMLTable:       string;
        var FHTMLTemp:        string;
        var FHTMLRow:         string;
        var FHTMLLayout:      string;
        var FCustName:        string;
        var FCustAddr:        string;
        var FLBUName:         string;
        var FLBUAddress:      string;
        var FTelephone:       string;
        var FBankDetails:     string;
        var FCustMess:        string;
        var FInvFilter:       TInvoiceFilter;
        var FBeginWith:       string;
        var FEndWith:         string;
        var FSourceGrid:      TStringGrid;
        var FCUID:            string;
        var FREM_EX1:         string;
        var FREM_EX2:         string;
        var FREM_EX3:         string;
        var FREM_EX4:         string;
        var FREM_EX5:         string;
        var FCommonHTMLTable: string;
        var FCommonHTMLRow:   string;
        procedure SaveOutput(FileName: string);
        function  BuildHTML: integer;
    public
        var OpenItems:  TStringGrid;
        property HTMLTable:   string         read FHTMLTable   write FHTMLTable;
        property HTMLTemp:    string         read FHTMLTemp    write FHTMLTemp;
        property HTMLRow:     string         read FHTMLRow     write FHTMLRow;
        property HTMLLayout:  string         read FHTMLLayout  write FHTMLLayout;
        property CustName:    string         read FCustName    write FCustName;
        property CustAddr:    string         read FCustAddr    write FCustAddr;
        property LBUName:     string         read FLBUName     write FLBUName;
        property LBUAddress:  string         read FLBUAddress  write FLBUAddress;
        property Telephone:   string         read FTelephone   write FTelephone;
        property BankDetails: string         read FBankDetails write FBankDetails;
        property CustMess:    string         read FCustMess    write FCustMess;
        property InvFilter:   TInvoiceFilter read FInvFilter   write FInvFilter;
        property BeginWith:   string         read FBeginWith   write FBeginWith;
        property EndWith:     string         read FEndWith     write FEndWith;
        property SourceGrid:  TStringGrid    read FSourceGrid  write FSourceGrid;
        property CUID:        string         read FCUID        write FCUID;
        property REM_EX1:     string         read FREM_EX1     write FREM_EX1;
        property REM_EX2:     string         read FREM_EX2     write FREM_EX2;
        property REM_EX3:     string         read FREM_EX3     write FREM_EX3;
        property REM_EX4:     string         read FREM_EX4     write FREM_EX4;
        property REM_EX5:     string         read FREM_EX5     write FREM_EX5;
        function  LoadTemplate(FileName: string; CrlStatVisible: boolean = true): string;
        function  SendDocument: boolean;
        constructor Create;
        destructor Destroy; override;
    end;


implementation


uses
    Data.Win.ADODB,
    Main,
    Tracker,
    Actions,
    SQL,
    DbModel,
    Settings;


// -------------------------------------------------------------------------------------------------------------------------------------------- MAILER CLASS //


constructor TMailer.Create;
begin
     FAttachments:=TList<string>.Create;
end;


destructor TMailer.Destroy;
begin
    if Assigned(FAttachments) then FAttachments.Free;
end;


function TMailer.SendEmail(OAuth: TMailer.TAuthTypes): boolean;
begin

    Result:=False;

    var CdoMessage: CDO_TLB.IMessage:=CDO_TLB.CoMessage.Create;
    CdoMessage.From:=MailFrom;
    CdoMessage.To_ :=MailTo;
    CdoMessage.CC  :=MailCc;

    if MailBcc <> '' then CdoMessage.BCC    :=MailBcc;
    if MailRt  <> '' then CdoMessage.ReplyTo:=MailRt;

    CdoMessage.Subject :=MailSubject;
    CdoMessage.HTMLBody:=MailBody;

    // Configure
    var Settings: ISettings:=TSettings.Create;
    var Schema: string:='http://schemas.microsoft.com/cdo/configuration/';

    if oauth = TAuthTypes.cdoNTLM then
    begin
        CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=TAuthUsing.cdoSendUsingPort;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=TAuthTypes.cdoNTLM;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'SMTP', '');
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'PORT', '');
    end;

    if oauth = TAuthTypes.cdoBasic then
    begin
        CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=TAuthUsing.cdoSendUsingPort;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=TAuthTypes.cdoBasic;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'SMTP', '');
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'PORT', '');
        CdoMessage.Configuration.Fields.item[Schema + 'sendusername'    ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'USERNAME', '');
        CdoMessage.Configuration.Fields.item[Schema + 'sendpassword'    ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'PASSWORD', '');
        CdoMessage.Configuration.Fields.item[Schema + 'smtpusessl'      ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'SSL', '');
    end;

    CdoMessage.Configuration.Fields.item[Schema + 'NNTPAccountName' ].Value:=XMailer;
    CdoMessage.Configuration.Fields.update;

    try

        // Add attachments (if any)
        if Attachments.Count > 0 then
        begin
            for var iCNT: integer:=0 to Attachments.Count - 1 do
                CdoMessage.AddAttachment(Attachments.Items[iCNT],'','');
        end;

        CdoMessage.BodyPart.Charset:='utf-8';
        CdoMessage.Send;
        Result:=True;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + idThd.ToString + ']: E-mail has been sent successfully.');

    except
        on E: Exception do
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + idThd.ToString + ']: Cannot send an e-mail. Error message has been thrown: ' + E.Message);
    end;

end;


function TMailer.SendNow: boolean;
begin
    Result:=False;
    var Settings: ISettings:=TSettings.Create;
    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM then Result:=SendEmail(TAuthTypes.cdoNTLM);
    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then Result:=SendEmail(TAuthTypes.cdoBasic);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ DOCUMENT CLASS //


constructor TDocument.Create;
begin
    inherited;
end;


destructor TDocument.Destroy;
begin
    inherited;
end;


function TDocument.LoadTemplate(FileName: string; CrlStatVisible: boolean = true): string;
begin

    var KeyName: string;
    var Settings: ISettings:=TSettings.Create;
    var SL: TStringList:=TStringList.Create;
    try

        // Upload main template
        SL.LoadFromFile(FileName);
        Result:=SL.Text;

        // Upload main table
        KeyName:='SERVICE%TBL';
        if not ActionsForm.cbCtrlStatusOff.Checked then KeyName:=KeyName.Replace('%', '1')
            else KeyName:=KeyName.Replace('%', '2');

        var HtmlTablePath: string:=Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        SL.LoadFromFile(HtmlTablePath);
        FCommonHTMLTable:=SL.Text;

        // Upload main row
        KeyName:='SERVICE%ROW';
        if not ActionsForm.cbCtrlStatusOff.Checked then KeyName:=KeyName.Replace('%', '1')
            else KeyName:=KeyName.Replace('%', '2');

        var HtmlRowPath: string:=Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        SL.LoadFromFile(HtmlRowPath);
        FCommonHTMLRow:=SL.Text;

    finally
        SL.Free;
    end;

end;


procedure TDocument.SaveOutput(FileName: string);
begin

    var SL: TStringList:=TStringList.Create;
    try
        SL.Text:=MailBody;
        SL.SaveToFile(FileName);
    finally
        SL.Free;
    end;

end;


function TDocument.BuildHTML: integer;

    var Pos:   integer;
    var Items: integer;

    function StatusCodeToText(TextCode: string; Source: TStringGrid): string;
    begin

        // Replace status code to text (short code description)
        for var iCNT: integer:=1 to Source.RowCount do
        begin

            if Source.Cells[MainForm.ControlStatusRefs.Code, iCNT] = TextCode then
            begin
                Result:=Source.Cells[MainForm.ControlStatusRefs.Text, iCNT];
                Break;
            end;

        end;

    end;

    procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
    begin

        // Move open items to HTML template
        var CurAmount: string;
        var Amount:    string;

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

        /// <remarks>
        /// Text on the invoice may be very long (but not more than 200 chars), decision was put hard limit of 32 chars.
        /// </remarks>
        HTMLTemp:=StringReplace(HTMLTemp, '{INV_TXT}', LeftStr(SG.Cells[MainForm.OpenItemsRefs.Text, ActualRow], 32), [rfReplaceAll]);

        // Replace control status number to description
        HTMLTemp:=StringReplace(
            HTMLTemp,
            '{INV_CRL}',
            StatusCodeToText(SG.Cells[MainForm.OpenItemsRefs.CtrlCol, ActualRow], MainForm.sgControlStatus),
            [rfReplaceAll]
        );

        HtmlStatement:=HtmlStatement + HTMLTemp;
        Inc(Items);

    end;


begin

    var LocalFrmt: TFormatSettings;
    var UnityFrmt: TFormatSettings;

    /// <remarks>
    /// We have two different date time format. First used by application is always the same:
    ///     Time: hh:mm:ss
    ///     Date: yyyy-mm-dd
    /// Second is local user format settings in Windows system that may vary. Data that comes from
    /// SQL database will be displayed accordingly to local user settings, so any conversion from
    /// string must use local settings and use application settings if further calculation is
    /// done on the data.
    /// </remarks>
    {$WARN SYMBOL_PLATFORM OFF} { Windows only }
    LocalFrmt:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}
    UnityFrmt:=FormatSettings;

    Pos:=0;
    Items:=0;

    // Get templates
    HTMLTable:=FCommonHTMLTable;
    HTMLRow  :=FCommonHTMLRow;

    // Open items to HTML table
    for var iCNT: integer:=1 to OpenItems.RowCount - 1 do
    begin
        if OpenItems.Cells[MainForm.OpenItemsRefs.CuidCol, iCNT] = CUID then
        begin

            if Pos = 0 then Pos:=iCNT;

            // Exclude items with control status indicated by field "ReminderException5"
            if OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX5 then
            begin

                // Include all items (account statement)
                if InvFilter = TInvoiceFilter.ShowAllItems then
                begin

                    if
                        (
                            StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) <> 0
                        )
                    then
                    begin

                        if not(String.IsNullOrEmpty(BeginWith)) and not(String.IsNullOrEmpty(EndWith)) then
                        begin
                            if
                                (
                                    StrToDate(OpenItems.Cells[MainForm.OpenItemsRefs.DueDtCol, iCNT], LocalFrmt) >= StrToDate(BeginWith, UnityFrmt)
                                )
                            and
                                (
                                    StrToDate(OpenItems.Cells[MainForm.OpenItemsRefs.DueDtCol, iCNT], LocalFrmt) <= StrToDate(EndWith, UnityFrmt)
                                )
                            then
                                OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

                        end
                        else
                            OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

                    end;

                end;

            end;

            // We exclude invoices with 'control status' that is different than given number in the comapny table
            if ( ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX1) and
                 ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX2) and
                 ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX3) and
                 ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX4) and
                 ( OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT] <> REM_EX5)
               )
            then
            begin
                // Include only overdue items (payment reminder)
                if InvFilter = TInvoiceFilter.ReminderOvd then
                begin
                    if
                        (
                            StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) <> 0
                        )
                    and
                        (
                            StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.PmtStatCol, iCNT], 0) < 0
                        )
                    then
                        OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);
                end;

                // Include all items with given due date range
                if InvFilter = TInvoiceFilter.ReminderNonOvd then
                begin

                    if
                        (
                            StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) <> 0
                        )
                    and
                        (
                            StrToDate(OpenItems.Cells[MainForm.OpenItemsRefs.DueDtCol, iCNT], LocalFrmt) >= StrToDate(BeginWith, UnityFrmt)
                        )
                    and
                        (
                            StrToDate(OpenItems.Cells[MainForm.OpenItemsRefs.DueDtCol, iCNT], LocalFrmt) <= StrToDate(EndWith, UnityFrmt)
                        )
                    then
                        OpenItemsToHtmlTable(HTMLStat, OpenItems, iCNT);

                end;

            end;

        end;

    end;

    // Build customer address field
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + TChars.CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,Pos] + '<br />' + TChars.CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,Pos] + '<br />' + TChars.CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,Pos] + '<br />' + TChars.CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,Pos] + '<br />' + TChars.CRLF;
    if (OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, Pos] <> '') and (OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, Pos] <> ' ') then CustAddr:=CustAddr + OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, Pos] + '<br />' + TChars.CRLF;
    CustAddr:=CustAddr + '</p>' + TChars.CRLF;

    Result:=Items;

end;


function TDocument.SendDocument;
//var RAND: integer; (* DEBUG *)
begin

    // Do not send if we have no items (due to selected due date range by the user)
    Result:=False;
    if BuildHTML = 0 then Exit;

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
    case InvFilter of
        TInvoiceFilter.ReminderOvd:    MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
        TInvoiceFilter.ReminderNonOvd: MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
        TInvoiceFilter.ShowAllItems:   MailBody:=StringReplace(MailBody, '{TITLE}', 'STATEMENT', [rfReplaceAll]);
    end;

    // Custom salutation and the message
    if CustMess  <> '' then MailBody:=StringReplace(MailBody, '{TEXT}',  CustMess,  [rfReplaceAll]);

    // Put user in CC
    var Settings: ISettings:=TSettings.Create;
    if ActionsForm.cbUserInCopy.Checked then
        MailBcc:=MainForm.WinUserName + '@' + Settings.GetStringValue(TConfigSections.ApplicationDetails, 'MAIL_DOMAIN', '')
    else
        MailBcc:='';

    XMailer:=MailFrom;
    MailCc :=MailFrom;
    MailRt :='';
    Result :=SendNow;

    (* DEBUG *)
//    RAND:=Random(100000);
//    SaveOutput('I:\temp\' + IntToStr(RAND) + '.html');
//    Result:=True;

end;


end.

