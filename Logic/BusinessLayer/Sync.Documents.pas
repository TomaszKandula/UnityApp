unit Sync.Documents;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

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
    Unity.Enums,
    Unity.Arrays,
    Unity.Grid,
    Unity.Records,
    Unity.References,
    Sync.Mailer;


type


    /// <summary>
    /// This interface provides methods and properties for setting up and sending new document (account statement or reminder)
    /// using HTML layout with defined tables for invoice list.
    /// </summary>
    IDocument = Interface(IMailer)
    ['{C3D66D48-891B-438B-9EB6-F53B62E2FCAD}']

        /// <summary>
        /// Setup new HTML table.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetHTMLTable(NewValue: string);

        /// <summary>
        /// Setup new HTML document template.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetHTMLTemp(NewValue: string);

        /// <summary>
        /// Setup new HTML row definition for HTML table row.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetHTMLRow(NewValue: string);

        /// <summary>
        /// Setup new HTML layout from local file.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetHTMLLayout(NewValue: string);

        /// <summary>
        /// Setup new customer name.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCustName(NewValue: string);

        /// <summary>
        /// Setup new customer address.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCustAddr(NewValue: string);

        /// <summary>
        /// Setup new LBU (Local Business Unit) name.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetLBUName(NewValue: string);

        /// <summary>
        /// Setup new LBU (Local Business Unit) address.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetLBUAddress(NewValue: string);

        /// <summary>
        /// Setup new Company telephone number.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetTelephone(NewValue: string);

        /// <summary>
        /// Setup new company bank account number with SWIFT/BIC/SORT CODE.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetBankDetails(NewValue: string);

        /// <summary>
        /// Setup new custom message to the end-customer.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCustMess(NewValue: string);

        /// <summary>
        /// Setup new document type based on invoices that should be included, example:
        /// if all invoices are presented, document becomes an account statement.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetInvFilter(NewValue: TInvoiceFilter);

        /// <summary>
        /// Setup new starting date for due date of the invoice (filtering purposes).
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetBeginWith(NewValue: string);

        /// <summary>
        /// Setup new ending date for due date of the invoice (filtering purposes).
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetEndWith(NewValue: string);

        /// <summary>
        /// Setup new open items list.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetOpenItems(NewValue: TStringGrid);

        /// <summary>
        /// Setup new CUID - customer unique identifier.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCUID(NewValue: string);

        /// <summary>
        /// Setup new list of invoices control statuses that should be excluded from placing onto document.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetExclusions(NewValue: TArray<integer>);

        /// <summary>
        /// Setup new common part of the HTML table.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCommonHTMLTable(NewValue: string);

        /// <summary>
        /// Setup new common part of the HTML table row.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCommonHTMLRow(NewValue: string);

        /// <summary>
        /// Setup new open items column references (source).
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetOpenItemsRefs(NewValue: TFOpenItemsRefs);

        /// <summary>
        /// Setup new control status column references (source).
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetCtrlStatusRefs(NewValue: TFCtrlStatusRefs);

        /// <summary>
        /// Setup new source of control status data.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetControlStatus(NewValue: TStringGrid);

        /// <summary>
        /// Returns HTML table.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetHTMLTable(): string;

        /// <summary>
        /// Returns HTML templarte.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetHTMLTemp(): string;

        /// <summary>
        /// Returns HTML table row.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetHTMLRow(): string;

        /// <summary>
        /// Returns HTML layout loaded from file.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetHTMLLayout(): string;

        /// <summary>
        /// Returns customer name.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCustName(): string;

        /// <summary>
        /// Returns customer address.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCustAddr(): string;

        /// <summary>
        /// Returns LBU (Local Business Unit) name.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetLBUName(): string;

        /// <summary>
        /// Returns LBU (Local Business Unit) address.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetLBUAddress(): string;

        /// <summary>
        /// Returns Company telephone number.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetTelephone(): string;

        /// <summary>
        /// Get company bank account with SWIFT/BIC/SORT CODE numbers.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetBankDetails(): string;

        /// <summary>
        /// Get setup custom message to the customer.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCustMess(): string;

        /// <summary>
        /// Get document type based on invoice selection.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetInvFilter(): TInvoiceFilter;

        /// <summary>
        /// Get starting date for invoice due date filter.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetBeginWith(): string;

        /// <summary>
        /// Get ending date for invoice due date filter.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetEndWith(): string;

        /// <summary>
        /// Get open items (invoice list).
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetOpenItems(): TStringGrid;

        /// <summary>
        /// Get customer unique identifier.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCUID(): string;

        /// <summary>
        /// Get setup control status numbers that defines excluded invoices from document.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetExclusions(): TArray<integer>;

        /// <summary>
        /// Get common HTML table part.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCommonHTMLTable(): string;

        /// <summary>
        /// Get common HTML table row part.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCommonHTMLRow(): string;

        /// <summary>
        /// Get open items column references.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetOpenItemsRefs(): TFOpenItemsRefs;

        /// <summary>
        /// get control statuses column references.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetCtrlStatusRefs(): TFCtrlStatusRefs;

        /// <summary>
        /// Get control statuses list.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        function GetControlStatus: TStringGrid;

        /// <summary>
        /// HTML table holder.
        /// </summary>
        property HTMLTable: string read GetHTMLTable;

        /// <summary>
        /// HTML template holder.
        /// </summary>
        property HTMLTemp: string read GetHTMLTemp;

        /// <summary>
        /// HTML table row holder.
        /// </summary>
        property HTMLRow: string read GetHTMLRow;

        /// <summary>
        /// Open items holder.
        /// </summary>
        property OpenItems: TStringGrid read GetOpenItems write SetOpenItems;

        /// <summary>
        /// Open items column references holder.
        /// </summary>
        property OpenItemsRefs: TFOpenItemsRefs read GetOpenItemsRefs write SetOpenItemsRefs;

        /// <summary>
        /// Control status holder.
        /// </summary>
        property ControlStatus: TStringGrid read GetControlStatus write SetControlStatus;

        /// <summary>
        /// Control status column references holder.
        /// </summary>
        property CtrlStatusRefs: TFCtrlStatusRefs read GetCtrlStatusRefs write SetCtrlStatusRefs;

        /// <summary>
        /// HTML layout holder loaded from file.
        /// </summary>
        property HTMLLayout: string read GetHTMLLayout write SetHTMLLayout;

        /// <summary>
        /// Customer name.
        /// </summary>
        property CustName: string read GetCustName write SetCustName;

        /// <summary>
        /// Customer address.
        /// </summary>
        property CustAddr: string read GetCustAddr write SetCustAddr;

        /// <summary>
        /// LBU (Local Business Unit) name.
        /// </summary>
        property LBUName: string read GetLBUName write SetLBUName;

        /// <summary>
        /// LBU (Local Business Unit) address.
        /// </summary>
        property LBUAddress: string read GetLBUAddress write SetLBUAddress;

        /// <summary>
        /// Company telephone holder.
        /// </summary>
        property Telephone: string read GetTelephone write SetTelephone;

        /// <summary>
        /// Company bank details that includes SWIFT/BIC/SORT CODE.
        /// </summary>
        property BankDetails: string read GetBankDetails write SetBankDetails;

        /// <summary>
        /// Custom message to a customer.
        /// </summary>
        property CustMess: string read GetCustMess write SetCustMess;

        /// <summary>
        /// Document type based on invoice selection (ex. all invoices implies account statement, overdue invoices makes reminder).
        /// </summary>
        property InvFilter: TInvoiceFilter read GetInvFilter write SetInvFilter;

        /// <summary>
        /// Starting date for invoice due date filter.
        /// </summary>
        property BeginWith: string read GetBeginWith write SetBeginWith;

        /// <summary>
        /// Ending date for invoice due date filter.
        /// </summary>
        property EndWith: string read GetEndWith write SetEndWith;

        /// <summary>
        /// Customer unique identifier.
        /// </summary>
        property CUID: string read GetCUID write SetCUID;

        /// <summary>
        /// List of excluded invoices based on control status value.
        /// </summary>
        property Exclusions: TArray<integer> read GetExclusions write SetExclusions;

        /// <summary>
        /// Allow to load HTML template from the file. It can return HTML layout with or without control status column.
        /// </summary>
        function LoadTemplate(FileName: string; IsCtrlStatus: boolean): string;

        /// <summary>
        /// Method that allows to send document via email.
        /// </summary>
        function SendDocument(IsUserInCopy: boolean): boolean;

    end;


    TDocument = class(TMailer, IDocument)
    {$TYPEINFO ON}
    strict private
        var FHTMLStat: string;
        var FPos: integer;
        var FItems: integer;
        var FHTMLTable: string;
        var FHTMLTemp: string;
        var FHTMLRow: string;
        var FHTMLLayout: string;
        var FCustName: string;
        var FCustAddr: string;
        var FLBUName: string;
        var FLBUAddress: string;
        var FTelephone: string;
        var FBankDetails: string;
        var FCustMess:        string;
        var FInvFilter: TInvoiceFilter;
        var FBeginWith: string;
        var FEndWith: string;
        var FCUID: string;
        var FExclusions: TArray<integer>;
        var FCommonHTMLTable: string;
        var FCommonHTMLRow: string;
        var FOpenItems: TStringGrid;
        var FOpenItemsRefs: TFOpenItemsRefs;
        var FControlStatus: TStringGrid;
        var FCtrlStatusRefs: TFCtrlStatusRefs;
        procedure SetHTMLTable(NewValue: string);
        procedure SetHTMLTemp(NewValue: string);
        procedure SetHTMLRow(NewValue: string);
        procedure SetHTMLLayout(NewValue: string);
        procedure SetCustName(NewValue: string);
        procedure SetCustAddr(NewValue: string);
        procedure SetLBUName(NewValue: string);
        procedure SetLBUAddress(NewValue: string);
        procedure SetTelephone(NewValue: string);
        procedure SetBankDetails(NewValue: string);
        procedure SetCustMess(NewValue: string);
        procedure SetInvFilter(NewValue: TInvoiceFilter);
        procedure SetBeginWith(NewValue: string);
        procedure SetEndWith(NewValue: string);
        procedure SetOpenItems(NewValue: TStringGrid);
        procedure SetCUID(NewValue: string);
        procedure SetExclusions(NewValue: TArray<integer>);
        procedure SetCommonHTMLTable(NewValue: string);
        procedure SetCommonHTMLRow(NewValue: string);
        procedure SetOpenItemsRefs(NewValue: TFOpenItemsRefs);
        procedure SetCtrlStatusRefs(NewValue: TFCtrlStatusRefs);
        procedure SetControlStatus(NewValue: TStringGrid);
        function  GetHTMLTable: string;
        function  GetHTMLTemp: string;
        function  GetHTMLRow: string;
        function  GetHTMLLayout: string;
        function  GetCustName: string;
        function  GetCustAddr: string;
        function  GetLBUName: string;
        function  GetLBUAddress: string;
        function  GetTelephone: string;
        function  GetBankDetails: string;
        function  GetCustMess: string;
        function  GetInvFilter: TInvoiceFilter;
        function  GetBeginWith: string;
        function  GetEndWith: string;
        function  GetOpenItems: TStringGrid;
        function  GetCUID: string;
        function  GetExclusions: TArray<integer>;
        function  GetCommonHTMLTable: string;
        function  GetCommonHTMLRow: string;
        function  GetOpenItemsRefs: TFOpenItemsRefs;
        function  GetCtrlStatusRefs: TFCtrlStatusRefs;
        function  GetControlStatus: TStringGrid;
        procedure SaveOutput(FileName: string);
        function  BuildHTML: integer;
        function  StatusCodeToText(TextCode: string; Source: TStringGrid): string;
        procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
    public

        /// <summary>
        /// HTML table holder.
        /// </summary>
        property HTMLTable: string read GetHTMLTable;

        /// <summary>
        /// HTML template holder.
        /// </summary>
        property HTMLTemp: string read GetHTMLTemp;

        /// <summary>
        /// HTML table row holder.
        /// </summary>
        property HTMLRow: string read GetHTMLRow;

        /// <summary>
        /// Open items holder.
        /// </summary>
        property OpenItems: TStringGrid read GetOpenItems write SetOpenItems;

        /// <summary>
        /// Open items column references holder.
        /// </summary>
        property OpenItemsRefs: TFOpenItemsRefs read GetOpenItemsRefs write SetOpenItemsRefs;

        /// <summary>
        /// Control status holder.
        /// </summary>
        property ControlStatus: TStringGrid read GetControlStatus write SetControlStatus;

        /// <summary>
        /// Control status column references holder.
        /// </summary>
        property CtrlStatusRefs: TFCtrlStatusRefs read GetCtrlStatusRefs write SetCtrlStatusRefs;

        /// <summary>
        /// HTML layout holder loaded from file.
        /// </summary>
        property HTMLLayout: string read GetHTMLLayout write SetHTMLLayout;

        /// <summary>
        /// Customer name.
        /// </summary>
        property CustName: string read GetCustName write SetCustName;

        /// <summary>
        /// Customer address.
        /// </summary>
        property CustAddr: string read GetCustAddr write SetCustAddr;

        /// <summary>
        /// LBU (Local Business Unit) name.
        /// </summary>
        property LBUName: string read GetLBUName write SetLBUName;

        /// <summary>
        /// LBU (Local Business Unit) address.
        /// </summary>
        property LBUAddress: string read GetLBUAddress write SetLBUAddress;

        /// <summary>
        /// Company telephone holder.
        /// </summary>
        property Telephone: string read GetTelephone write SetTelephone;

        /// <summary>
        /// Company bank details that includes SWIFT/BIC/SORT CODE.
        /// </summary>
        property BankDetails: string read GetBankDetails write SetBankDetails;

        /// <summary>
        /// Custom message to a customer.
        /// </summary>
        property CustMess: string read GetCustMess write SetCustMess;

        /// <summary>
        /// Document type based on invoice selection (ex. all invoices implies account statement, overdue invoices makes reminder).
        /// </summary>
        property InvFilter: TInvoiceFilter read GetInvFilter write SetInvFilter;

        /// <summary>
        /// Starting date for invoice due date filter.
        /// </summary>
        property BeginWith: string read GetBeginWith write SetBeginWith;

        /// <summary>
        /// Ending date for invoice due date filter.
        /// </summary>
        property EndWith: string read GetEndWith write SetEndWith;

        /// <summary>
        /// Customer unique identifier.
        /// </summary>
        property CUID: string read GetCUID write SetCUID;

        /// <summary>
        /// List of excluded invoices based on control status value.
        /// </summary>
        property Exclusions: TArray<integer> read GetExclusions write SetExclusions;

        /// <summary>
        /// Allow to load HTML template from the file. It can return HTML layout with or without control status column.
        /// </summary>
        function  LoadTemplate(FileName: string; IsCtrlStatus: boolean): string;

        /// <summary>
        /// Method that allows to send document via email.
        /// </summary>
        function  SendDocument(IsUserInCopy: boolean): boolean;

        /// <summary>
        /// Inherited from base class.
        /// </summary>
        constructor Create();

        /// <summary>
        /// Inherited from base class.
        /// </summary>
        destructor Destroy(); override;

    end;


implementation


uses
    Data.Win.ADODB,
    DbModel,
    Unity.Chars,
    Unity.Settings,
    Unity.SessionService;


constructor TDocument.Create();
begin
    inherited;
end;


destructor TDocument.Destroy();
begin
    inherited;
end;


function TDocument.LoadTemplate(FileName: string; IsCtrlStatus: boolean): string;
begin

    // ------------------------------------------------------------------------------
    // Template is made out of two files that defines HTML table and single row.
    // Note: we have two different tables, with Control Status column and without it.
    // ------------------------------------------------------------------------------

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
        if not IsCtrlStatus then KeyName:=KeyName.Replace('%', '1')
            else KeyName:=KeyName.Replace('%', '2');

        var HtmlTablePath: string:=Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        SL.LoadFromFile(HtmlTablePath);
        FCommonHTMLTable:=SL.Text;

        // ----------------------
        // Upload row definition.
        // ----------------------

        KeyName:='SERVICE%ROW';
        if not IsCtrlStatus then KeyName:=KeyName.Replace('%', '1')
            else KeyName:=KeyName.Replace('%', '2');

        var HtmlRowPath: string:=Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        SL.LoadFromFile(HtmlRowPath);
        FCommonHTMLRow:=SL.Text;

    finally
        SL.Free;
    end;

end;


function TDocument.SendDocument(IsUserInCopy: boolean): boolean;
var RAND: integer; (* DEBUG *)
begin

    // -----------------------------------------------------------------------------
    // Do not send if we have no items (due to selected due date range by the user).
    // -----------------------------------------------------------------------------

    Result:=False;
    if BuildHTML = 0 then Exit();

    // ---------------------------
    // Put data into placeholders.
    // ---------------------------

    FHTMLTable:=StringReplace(FHTMLTable,  '{ROWS}',         FHTMLStat,    [rfReplaceAll]);
    MailBody  :=StringReplace(FHTMLLayout, '{INVOICE_LIST}', FHTMLTable,   [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{ADDR_DATA}',    CustAddr,     [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{BANKS}',        BankDetails,  [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{NAME_LBU}',     LBUName,      [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{ADDR_LBU}',     LBUAddress,   [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{EMAIL}',        MailFrom,     [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{TEL}',          Telephone,    [rfReplaceAll]);

    // ----------------------------------------------
    // Custom template title (statement or reminder).
    // ----------------------------------------------

    case InvFilter of
        TInvoiceFilter.ReminderOvd:    MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
        TInvoiceFilter.ReminderNonOvd: MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
        TInvoiceFilter.ShowAllItems:   MailBody:=StringReplace(MailBody, '{TITLE}', 'STATEMENT', [rfReplaceAll]);
    end;

    // ----------------------------------
    // Custom salutation and the message.
    // ----------------------------------

    if CustMess <> '' then MailBody:=StringReplace(MailBody, '{TEXT}', CustMess, [rfReplaceAll]);

    // ------------------------
    // Put user in carbon copy.
    // ------------------------

    var Settings: ISettings:=TSettings.Create();
    case IsUserInCopy of
        True:  MailBcc:=SessionService.SessionData.EmailAddress;
        False: MailBcc:='';
    end;

    XMailer:=MailFrom;
    MailCc :=MailFrom;
    MailRt :='';
    //Result :=SendNow;

    (* DEBUG *)
    RAND:=Random(100000);
    SaveOutput('I:\Temp\TestEmails\' + IntToStr(RAND) + '.html');
    Result:=True;

end;


procedure TDocument.SaveOutput(FileName: string);
begin

    // ----------------------------
    // Save the email as HTML file.
    // ----------------------------
    var SL: TStringList:=TStringList.Create;
    try
        SL.Text:=MailBody;
        SL.SaveToFile(FileName);
    finally
        SL.Free;
    end;

end;


function TDocument.StatusCodeToText(TextCode: string; Source: TStringGrid): string;
begin

    // ------------------------------------------
    // Replace status code with text description.
    // ------------------------------------------
    for var iCNT: integer:=1 to Source.RowCount do
    begin

        if Source.Cells[FCtrlStatusRefs.Code, iCNT] = TextCode then
        begin
            Result:=Source.Cells[FCtrlStatusRefs.Text, iCNT];
            Break;
        end;

    end;

end;


procedure TDocument.OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
begin

    // ------------------------
    // Get outstanding amounts.
    // ------------------------

    var CurAmount: string:=SG.Cells[FOpenItemsRefs.CurAmCol, ActualRow];
    var Amount:    string:=SG.Cells[FOpenItemsRefs.OpenCurAmCol, ActualRow];

    // ----------------------------------------------
    // Change format number from 1000,00 to 1 000,00.
    // ----------------------------------------------

    CurAmount:=FormatFloat('#,##0.00', StrToFloat(CurAmount));
    Amount   :=FormatFloat('#,##0.00', StrToFloat(Amount));

    // --------------
    // Generate HTML.
    // --------------

    FHTMLTemp:=HTMLRow;
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_NUM}', SG.Cells[FOpenItemsRefs.InvoNoCol, ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_DAT}', SG.Cells[FOpenItemsRefs.ValDtCol,  ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{DUE_DAT}', SG.Cells[FOpenItemsRefs.DueDtCol,  ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_CUR}', SG.Cells[FOpenItemsRefs.ISOCol,    ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_AMT}', CurAmount, [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_OSA}', Amount,    [rfReplaceAll]);

    // -------------------------------------------------------------------
    // Text on the invoice may be very long (but not more than 200 chars),
    // the decision was to put hard limit of 32 chars.
    // -------------------------------------------------------------------

    var Text: string:=SG.Cells[FOpenItemsRefs.Text, ActualRow];
    var Code: string:=SG.Cells[FOpenItemsRefs.CtrlCol, ActualRow];
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_TXT}', LeftStr(Text, 32), [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_CRL}', StatusCodeToText(Code, FControlStatus), [rfReplaceAll]);

    HtmlStatement:=HtmlStatement + FHTMLTemp;
    Inc(FItems);

end;


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

        var CtrlStatus: integer:=(OpenItems.Cells[FOpenItemsRefs.CtrlCol, iCNT]).ToInteger();

        if (OpenItems.Cells[FOpenItemsRefs.CuidCol, iCNT] = CUID) and (not TArrayUtils<integer>.Contains(CtrlStatus, FExclusions)) then
        begin

            if FPos = 0 then FPos:=iCNT;

            case InvFilter of

                // --------------------------------------
                // Include all items (account statement).
                // --------------------------------------

                TInvoiceFilter.ShowAllItems:
                begin

                    if (StrToFloatDef(OpenItems.Cells[FOpenItemsRefs.OpenAmCol, iCNT], 0) <> 0) then
                    begin

                        if not(String.IsNullOrEmpty(BeginWith)) and not(String.IsNullOrEmpty(EndWith)) then
                        begin

                            if
                                (
                                    StrToDate(OpenItems.Cells[FOpenItemsRefs.DueDtCol, iCNT], LocalFrmt) >= StrToDate(BeginWith, UnityFrmt)
                                )
                            and
                                (
                                    StrToDate(OpenItems.Cells[FOpenItemsRefs.DueDtCol, iCNT], LocalFrmt) <= StrToDate(EndWith, UnityFrmt)
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
                            StrToFloatDef(OpenItems.Cells[FOpenItemsRefs.OpenAmCol, iCNT], 0) <> 0
                        )
                    and
                        (
                            StrToFloatDef(OpenItems.Cells[FOpenItemsRefs.PmtStatCol, iCNT], 0) < 0
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
                            StrToFloatDef(OpenItems.Cells[FOpenItemsRefs.OpenAmCol, iCNT], 0) <> 0
                        )
                    and
                        (
                            StrToDate(OpenItems.Cells[FOpenItemsRefs.DueDtCol, iCNT], LocalFrmt) >= StrToDate(BeginWith, UnityFrmt)
                        )
                    and
                        (
                            StrToDate(OpenItems.Cells[FOpenItemsRefs.DueDtCol, iCNT], LocalFrmt) <= StrToDate(EndWith, UnityFrmt)
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

    var AddrFld1: string:=OpenItems.Cells[FOpenItemsRefs.Ad1Col,   FPos];
    var AddrFld2: string:=OpenItems.Cells[FOpenItemsRefs.Ad2Col,   FPos];
    var AddrFld3: string:=OpenItems.Cells[FOpenItemsRefs.Ad3Col,   FPos];
    var PoCode:   string:=OpenItems.Cells[FOpenItemsRefs.PnoCol,   FPos];
    var PoArea:   string:=OpenItems.Cells[FOpenItemsRefs.PAreaCol, FPos];

    if not String.IsNullOrWhiteSpace(AddrFld1) then CustAddr:=CustAddr + AddrFld1 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(AddrFld2) then CustAddr:=CustAddr + AddrFld2 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(AddrFld3) then CustAddr:=CustAddr + AddrFld3 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(PoCode)   then CustAddr:=CustAddr + PoCode   + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(PoArea)   then CustAddr:=CustAddr + PoArea   + '<br />' + TChars.CRLF;

    CustAddr:=CustAddr + '</p>' + TChars.CRLF;

    Result:=FItems;

end;


function  TDocument.GetOpenItemsRefs: TFOpenItemsRefs;
begin
    Result:=FOpenItemsRefs;
end;


function  TDocument.GetCtrlStatusRefs: TFCtrlStatusRefs;
begin
    Result:=FCtrlStatusRefs;
end;


function TDocument.GetControlStatus: TStringGrid;
begin
    Result:=FControlStatus;
end;


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


procedure TDocument.SetOpenItemsRefs(NewValue: TFOpenItemsRefs);
begin
    FOpenItemsRefs:=NewValue;
end;


procedure TDocument.SetCtrlStatusRefs(NewValue: TFCtrlStatusRefs);
begin
    FCtrlStatusRefs:=NewValue;
end;


procedure TDocument.SetControlStatus(NewValue: TStringGrid);
begin
    FControlStatus:=NewValue;
end;


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

