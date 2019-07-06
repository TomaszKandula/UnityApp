unit Sync.Documents;


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
    Vcl.Grids,
    CDO_TLB,
    Unity.Statics,
    Unity.Enums,
    Unity.Arrays,
    Unity.Interposer,
    Sync.Mailer;


type


    IDocument = Interface(IMailer)
    ['{C3D66D48-891B-438B-9EB6-F53B62E2FCAD}']

        // ----------------------------
        // Undisclosed getters/setters.
        // ----------------------------

        procedure SetHTMLTable(NewValue:       string);
        procedure SetHTMLTemp(NewValue:        string);
        procedure SetHTMLRow(NewValue:         string);
        procedure SetHTMLLayout(NewValue:      string);
        procedure SetCustName(NewValue:        string);
        procedure SetCustAddr(NewValue:        string);
        procedure SetLBUName(NewValue:         string);
        procedure SetLBUAddress(NewValue:      string);
        procedure SetTelephone(NewValue:       string);
        procedure SetBankDetails(NewValue:     string);
        procedure SetCustMess(NewValue:        string);
        procedure SetInvFilter(NewValue:       TInvoiceFilter);
        procedure SetBeginWith(NewValue:       string);
        procedure SetEndWith(NewValue:         string);
        procedure SetOpenItems(NewValue:       TStringGrid);
        procedure SetSourceGrid(NewValue:      TStringGrid);
        procedure SetCUID(NewValue:            string);
        procedure SetExclusions(NewValue:      TArray<integer>);
        procedure SetCommonHTMLTable(NewValue: string);
        procedure SetCommonHTMLRow(NewValue:   string);
        function  GetHTMLTable:       string;
        function  GetHTMLTemp:        string;
        function  GetHTMLRow:         string;
        function  GetHTMLLayout:      string;
        function  GetCustName:        string;
        function  GetCustAddr:        string;
        function  GetLBUName:         string;
        function  GetLBUAddress:      string;
        function  GetTelephone:       string;
        function  GetBankDetails:     string;
        function  GetCustMess:        string;
        function  GetInvFilter:       TInvoiceFilter;
        function  GetBeginWith:       string;
        function  GetEndWith:         string;
        function  GetOpenItems:       TStringGrid;
        function  GetSourceGrid:      TStringGrid;
        function  GetCUID:            string;
        function  GetExclusions:      TArray<integer>;
        function  GetCommonHTMLTable: string;
        function  GetCommonHTMLRow:   string;

        // ----------------------------
        // Exposed properties.
        // ----------------------------

        property HTMLTable:   string          read GetHTMLTable;
        property HTMLTemp:    string          read GetHTMLTemp;
        property HTMLRow:     string          read GetHTMLRow;
        property HTMLLayout:  string          read GetHTMLLayout  write SetHTMLLayout;
        property CustName:    string          read GetCustName    write SetCustName;
        property CustAddr:    string          read GetCustAddr    write SetCustAddr;
        property LBUName:     string          read GetLBUName     write SetLBUName;
        property LBUAddress:  string          read GetLBUAddress  write SetLBUAddress;
        property Telephone:   string          read GetTelephone   write SetTelephone;
        property BankDetails: string          read GetBankDetails write SetBankDetails;
        property CustMess:    string          read GetCustMess    write SetCustMess;
        property InvFilter:   TInvoiceFilter  read GetInvFilter   write SetInvFilter;
        property BeginWith:   string          read GetBeginWith   write SetBeginWith;
        property EndWith:     string          read GetEndWith     write SetEndWith;
        property OpenItems:  TStringGrid      read GetOpenItems   write SetOpenItems;
        property SourceGrid:  TStringGrid     read GetSourceGrid  write SetSourceGrid;
        property CUID:        string          read GetCUID        write SetCUID;
        property Exclusions:  TArray<integer> read GetExclusions  write SetExclusions;

        // ----------------------------
        // Exposed methods.
        // ----------------------------

        function LoadTemplate(FileName: string): string;
        function SendDocument: boolean;

    end;


    TDocument = class(TMailer, IDocument)
    {$TYPEINFO ON}
    protected
        var FHTMLStat: string;
        var FPos:      integer;
        var FItems:    integer;
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
        var FOpenItems:       TStringGrid;
        var FSourceGrid:      TStringGrid;
        var FCUID:            string;
        var FExclusions:      TArray<integer>;
        var FCommonHTMLTable: string;
        var FCommonHTMLRow:   string;
        procedure SetHTMLTable(NewValue:       string);
        procedure SetHTMLTemp(NewValue:        string);
        procedure SetHTMLRow(NewValue:         string);
        procedure SetHTMLLayout(NewValue:      string);
        procedure SetCustName(NewValue:        string);
        procedure SetCustAddr(NewValue:        string);
        procedure SetLBUName(NewValue:         string);
        procedure SetLBUAddress(NewValue:      string);
        procedure SetTelephone(NewValue:       string);
        procedure SetBankDetails(NewValue:     string);
        procedure SetCustMess(NewValue:        string);
        procedure SetInvFilter(NewValue:       TInvoiceFilter);
        procedure SetBeginWith(NewValue:       string);
        procedure SetEndWith(NewValue:         string);
        procedure SetOpenItems(NewValue:       TStringGrid);
        procedure SetSourceGrid(NewValue:      TStringGrid);
        procedure SetCUID(NewValue:            string);
        procedure SetExclusions(NewValue:      TArray<integer>);
        procedure SetCommonHTMLTable(NewValue: string);
        procedure SetCommonHTMLRow(NewValue:   string);
        function  GetHTMLTable:       string;
        function  GetHTMLTemp:        string;
        function  GetHTMLRow:         string;
        function  GetHTMLLayout:      string;
        function  GetCustName:        string;
        function  GetCustAddr:        string;
        function  GetLBUName:         string;
        function  GetLBUAddress:      string;
        function  GetTelephone:       string;
        function  GetBankDetails:     string;
        function  GetCustMess:        string;
        function  GetInvFilter:       TInvoiceFilter;
        function  GetBeginWith:       string;
        function  GetEndWith:         string;
        function  GetOpenItems:       TStringGrid;
        function  GetSourceGrid:      TStringGrid;
        function  GetCUID:            string;
        function  GetExclusions:      TArray<integer>;
        function  GetCommonHTMLTable: string;
        function  GetCommonHTMLRow:   string;
        procedure SaveOutput(FileName: string);
        function  BuildHTML: integer;
        function  StatusCodeToText(TextCode: string; Source: TStringGrid): string;
        procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
    public
        property HTMLTable:   string          read GetHTMLTable;
        property HTMLTemp:    string          read GetHTMLTemp;
        property HTMLRow:     string          read GetHTMLRow;
        property HTMLLayout:  string          read GetHTMLLayout  write SetHTMLLayout;
        property CustName:    string          read GetCustName    write SetCustName;
        property CustAddr:    string          read GetCustAddr    write SetCustAddr;
        property LBUName:     string          read GetLBUName     write SetLBUName;
        property LBUAddress:  string          read GetLBUAddress  write SetLBUAddress;
        property Telephone:   string          read GetTelephone   write SetTelephone;
        property BankDetails: string          read GetBankDetails write SetBankDetails;
        property CustMess:    string          read GetCustMess    write SetCustMess;
        property InvFilter:   TInvoiceFilter  read GetInvFilter   write SetInvFilter;
        property BeginWith:   string          read GetBeginWith   write SetBeginWith;
        property EndWith:     string          read GetEndWith     write SetEndWith;
        property OpenItems:  TStringGrid      read GetOpenItems   write SetOpenItems;
        property SourceGrid:  TStringGrid     read GetSourceGrid  write SetSourceGrid;
        property CUID:        string          read GetCUID        write SetCUID;
        property Exclusions:  TArray<integer> read GetExclusions  write SetExclusions;
        function  LoadTemplate(FileName: string): string;
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


// -------------------------------
// Initialize.
// -------------------------------

constructor TDocument.Create;
begin
    inherited;
    { Empty }
end;


// -------------------------------
// Release object from memory.
// -------------------------------

destructor TDocument.Destroy;
begin
    inherited;
    { Empty }
end;


// -------------------------------
// Load template html file.
// -------------------------------

function TDocument.LoadTemplate(FileName: string): string;
begin

    // -----------------------------------------------------------------------------
    // Template is made out of two files that defines HTML table and single row.
    // Note: we have two different tables,with Control Status column and without it.
    // -----------------------------------------------------------------------------

    var KeyName: string;
    var Settings: ISettings:=TSettings.Create;
    var SL: TStringList:=TStringList.Create;
    try

        // ------------------------
        // Upload table definition.
        // ------------------------

        SL.LoadFromFile(FileName);
        Result:=SL.Text;

        KeyName:='SERVICE%TBL';
        if not ActionsForm.cbCtrlStatusOff.Checked then KeyName:=KeyName.Replace('%', '1')
            else KeyName:=KeyName.Replace('%', '2');

        var HtmlTablePath: string:=Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        SL.LoadFromFile(HtmlTablePath);
        FCommonHTMLTable:=SL.Text;

        // ----------------------
        // Upload row definition.
        // ----------------------

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


// -------------------------------
// Save the email as HTML file.
// -------------------------------

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


// ------------------------------------------
// Replace status code with text description.
// ------------------------------------------

function TDocument.StatusCodeToText(TextCode: string; Source: TStringGrid): string;
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


// -----------------------------
// Generate HTML for given item.
// -----------------------------

procedure TDocument.OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
begin

    // Get outstanding amounts
    var CurAmount: string:=SG.Cells[MainForm.OpenItemsRefs.CurAmCol, ActualRow];
    var Amount:    string:=SG.Cells[MainForm.OpenItemsRefs.OpenCurAmCol, ActualRow];

    // Change format number from 1000,00 to 1 000,00
    CurAmount:=FormatFloat('#,##0.00', StrToFloat(CurAmount));
    Amount   :=FormatFloat('#,##0.00', StrToFloat(Amount));

    // Generate HTML
    FHTMLTemp:=HTMLRow;
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_NUM}', SG.Cells[MainForm.OpenItemsRefs.InvoNoCol, ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_DAT}', SG.Cells[MainForm.OpenItemsRefs.ValDtCol,  ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{DUE_DAT}', SG.Cells[MainForm.OpenItemsRefs.DueDtCol,  ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_CUR}', SG.Cells[MainForm.OpenItemsRefs.ISOCol,    ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_AMT}', CurAmount, [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_OSA}', Amount,    [rfReplaceAll]);

    // Text on the invoice may be very long (but not more than 200 chars),
    // the decision was to put hard limit of 32 chars.
    var Text: string:=SG.Cells[MainForm.OpenItemsRefs.Text, ActualRow];
    var Code: string:=SG.Cells[MainForm.OpenItemsRefs.CtrlCol, ActualRow];
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_TXT}', LeftStr(Text, 32), [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_CRL}', StatusCodeToText(Code, MainForm.sgControlStatus), [rfReplaceAll]);

    HtmlStatement:=HtmlStatement + FHTMLTemp;
    Inc(FItems);

end;


// ------------------------------------
// Build document for given open items.
// ------------------------------------

function TDocument.BuildHTML: integer;
begin

    // ------------------------------------------------------------------------------------------
    // We have two different date time format. First used by application that is always the same:
    //     Time: hh:mm:ss
    //     Date: yyyy-mm-dd
    // Second is local user format settings in Windows system that may vary. Data that comes from
    // SQL database will be displayed accordingly to local user settings, so any conversion from
    // string must use local settings and use application settings if further calculation is
    // done on the data.
    // ------------------------------------------------------------------------------------------

    var LocalFrmt: TFormatSettings;
    var UnityFrmt: TFormatSettings;

    {$WARN SYMBOL_PLATFORM OFF} { Windows only }
    LocalFrmt:=TFormatSettings.Create(LOCALE_USER_DEFAULT);
    {$WARN SYMBOL_PLATFORM ON}
    UnityFrmt:=FormatSettings;

    // ---------------------------------------------
    // Build HTML with all open items for given CUID
    // number and control statuses different than
    // those defined by FExclusion array.
    // ---------------------------------------------

    FPos:=0;
    FItems:=0;

    FHTMLTable:=FCommonHTMLTable;
    FHTMLRow  :=FCommonHTMLRow;

    for var iCNT: integer:=1 to OpenItems.RowCount - 1 do
    begin

        var CtrlStatus: integer:=(OpenItems.Cells[MainForm.OpenItemsRefs.CtrlCol, iCNT]).ToInteger();

        if (OpenItems.Cells[MainForm.OpenItemsRefs.CuidCol, iCNT] = CUID) and (not TArrayUtils<integer>.Contains(CtrlStatus, FExclusions)) then
        begin

            if FPos = 0 then FPos:=iCNT;

            case InvFilter of

                // --------------------------------------
                // Include all items (account statement).
                // --------------------------------------

                TInvoiceFilter.ShowAllItems:
                begin

                    if (StrToFloatDef(OpenItems.Cells[MainForm.OpenItemsRefs.OpenAmCol, iCNT], 0) <> 0) then
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
                                OpenItemsToHtmlTable(FHTMLStat, FOpenItems, iCNT);

                        end
                        else
                            OpenItemsToHtmlTable(FHTMLStat, FOpenItems, iCNT);

                    end;

                end;

                // ----------------------------------------------
                // Include only overdue items (payment reminder).
                // ----------------------------------------------

                TInvoiceFilter.ReminderOvd:
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
                        OpenItemsToHtmlTable(FHTMLStat, FOpenItems, iCNT);

                end;

                // --------------------------------------------
                // Include all items with given due date range.
                // --------------------------------------------

                TInvoiceFilter.ReminderNonOvd:
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
                        OpenItemsToHtmlTable(FHTMLStat, FOpenItems, iCNT);

                end;

            end;

        end;

    end;

    // -----------------------------
    // Build customer address field.
    // -----------------------------

    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + TChars.CRLF;

    var AddrFld1: string:=OpenItems.Cells[MainForm.OpenItemsRefs.Ad1Col,   FPos];
    var AddrFld2: string:=OpenItems.Cells[MainForm.OpenItemsRefs.Ad2Col,   FPos];
    var AddrFld3: string:=OpenItems.Cells[MainForm.OpenItemsRefs.Ad3Col,   FPos];
    var PoCode:   string:=OpenItems.Cells[MainForm.OpenItemsRefs.PnoCol,   FPos];
    var PoArea:   string:=OpenItems.Cells[MainForm.OpenItemsRefs.PAreaCol, FPos];

    if not String.IsNullOrWhiteSpace(AddrFld1) then CustAddr:=CustAddr + AddrFld1 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(AddrFld2) then CustAddr:=CustAddr + AddrFld2 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(AddrFld3) then CustAddr:=CustAddr + AddrFld3 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(PoCode)   then CustAddr:=CustAddr + PoCode   + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(PoArea)   then CustAddr:=CustAddr + PoArea   + '<br />' + TChars.CRLF;

    CustAddr:=CustAddr + '</p>' + TChars.CRLF;

    Result:=FItems;

end;


// -----------------------------------
// Prepare and send account statement.
// -----------------------------------

function TDocument.SendDocument;
var RAND: integer; (* DEBUG *)
begin

    // Do not send if we have no items (due to selected due date range by the user)
    Result:=False;
    if BuildHTML = 0 then Exit;

    // Put data into placeholders
    FHTMLTable:=StringReplace(FHTMLTable,  '{ROWS}',         FHTMLStat,    [rfReplaceAll]);
    MailBody  :=StringReplace(FHTMLLayout, '{INVOICE_LIST}', FHTMLTable,   [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{ADDR_DATA}',    CustAddr,     [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{BANKS}',        BankDetails,  [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{NAME_LBU}',     LBUName,      [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{ADDR_LBU}',     LBUAddress,   [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{EMAIL}',        MailFrom,     [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{TEL}',          Telephone,    [rfReplaceAll]);

    // Custom template title (statement or reminder)
    case InvFilter of

        TInvoiceFilter.ReminderOvd:    MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
        TInvoiceFilter.ReminderNonOvd: MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
        TInvoiceFilter.ShowAllItems:   MailBody:=StringReplace(MailBody, '{TITLE}', 'STATEMENT', [rfReplaceAll]);

    end;

    // Custom salutation and the message
    if CustMess <> '' then MailBody:=StringReplace(MailBody, '{TEXT}', CustMess, [rfReplaceAll]);

    // Put user in CC
    var Settings: ISettings:=TSettings.Create;
    if ActionsForm.cbUserInCopy.Checked then
        MailBcc:=MainForm.WinUserName + '@' + Settings.GetStringValue(TConfigSections.ApplicationDetails, 'MAIL_DOMAIN', '')
    else
        MailBcc:='';

    XMailer:=MailFrom;
    MailCc :=MailFrom;
    MailRt :='';
    //Result :=SendNow;

    (* DEBUG *)
    RAND:=Random(100000);
    SaveOutput('I:\temp\' + IntToStr(RAND) + '.html');
    Result:=True;

end;


// -------------------------------
// Property's getters.
// -------------------------------

function TDocument.GetHTMLTable: string;
begin
    Result:=FHTMLTable;
end;


function TDocument.GetHTMLTemp: string;
begin
    Result:=FHTMLTemp;
end;


function TDocument.GetHTMLRow: string;
begin
    Result:=FHTMLRow;
end;


function TDocument.GetHTMLLayout: string;
begin
    Result:=FHTMLLayout;
end;


function TDocument.GetCustName: string;
begin
    Result:=FCustName;
end;


function TDocument.GetCustAddr: string;
begin
    Result:=FCustAddr;
end;


function TDocument.GetLBUName: string;
begin
    Result:=FLBUName;
end;


function TDocument.GetLBUAddress: string;
begin
    Result:=FLBUAddress;
end;


function TDocument.GetTelephone: string;
begin
    Result:=FTelephone;
end;


function TDocument.GetBankDetails: string;
begin
    Result:=FBankDetails;
end;


function TDocument.GetCustMess: string;
begin
    Result:=FCustMess;
end;


function TDocument.GetInvFilter: TInvoiceFilter;
begin
    Result:=FInvFilter;
end;


function TDocument.GetBeginWith: string;
begin
    Result:=FBeginWith;
end;


function TDocument.GetEndWith: string;
begin
    Result:=FEndWith;
end;


function TDocument.GetOpenItems: TStringGrid;
begin
    Result:=FOpenItems;
end;


function TDocument.GetSourceGrid: TStringGrid;
begin
    Result:=FSourceGrid;
end;


function TDocument.GetCUID: string;
begin
    Result:=FCUID;
end;


function TDocument.GetExclusions: TArray<integer>;
begin
    Result:=FExclusions;
end;


function TDocument.GetCommonHTMLTable: string;
begin
    Result:=FCommonHTMLTable;
end;


function TDocument.GetCommonHTMLRow: string;
begin
    Result:=FCommonHTMLRow;
end;


// -------------------------------
// Property's setters.
// -------------------------------

procedure TDocument.SetHTMLTable(NewValue: string);
begin
    FHTMLTable:=NewValue;
end;


procedure TDocument.SetHTMLTemp(NewValue: string);
begin
    FHTMLTemp:=NewValue;
end;


procedure TDocument.SetHTMLRow(NewValue: string);
begin
    FHTMLRow:=NewValue;
end;


procedure TDocument.SetHTMLLayout(NewValue: string);
begin
    FHTMLLayout:=NewValue;
end;


procedure TDocument.SetCustName(NewValue: string);
begin
    FCustName:=NewValue;
end;


procedure TDocument.SetCustAddr(NewValue: string);
begin
    FCustAddr:=NewValue;
end;


procedure TDocument.SetLBUName(NewValue: string);
begin
    FLBUName:=NewValue;
end;


procedure TDocument.SetLBUAddress(NewValue: string);
begin
    FLBUAddress:=NewValue;
end;


procedure TDocument.SetTelephone(NewValue: string);
begin
    FTelephone:=NewValue;
end;


procedure TDocument.SetBankDetails(NewValue: string);
begin
    FBankDetails:=NewValue;
end;


procedure TDocument.SetCustMess(NewValue: string);
begin
    FCustMess:=NewValue;
end;


procedure TDocument.SetInvFilter(NewValue: TInvoiceFilter);
begin
    FInvFilter:=NewValue;
end;


procedure TDocument.SetBeginWith(NewValue: string);
begin
    FBeginWith:=NewValue;
end;


procedure TDocument.SetEndWith(NewValue: string);
begin
    FEndWith:=NewValue;
end;


procedure TDocument.SetOpenItems(NewValue: TStringGrid);
begin
    FOpenItems:=NewValue;
end;


procedure TDocument.SetSourceGrid(NewValue: TStringGrid);
begin
    FSourceGrid:=NewValue;
end;


procedure TDocument.SetCUID(NewValue: string);
begin
    FCUID:=NewValue;
end;


procedure TDocument.SetExclusions(NewValue: TArray<integer>);
begin
    FExclusions:=NewValue;
end;


procedure TDocument.SetCommonHTMLTable(NewValue: string);
begin
    FCommonHTMLTable:=NewValue;
end;


procedure TDocument.SetCommonHTMLRow(NewValue: string);
begin
    FCommonHTMLRow:=NewValue;
end;


end.

