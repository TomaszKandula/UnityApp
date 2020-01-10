unit Sync.Document;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Winapi.Windows,
    System.Classes,
    System.Generics.Collections,
    Unity.Enums,
    Unity.Grid,
    Unity.Records,
    Unity.References,
    Sync.Mailer;


type


    /// <summary>
    /// Provides methods and properties for setting up and sending new document (account statement or reminder)
    /// using HTML layout with defined tables for invoice list.
    /// </summary>
    IDocument = Interface(IMailer)
    ['{C3D66D48-891B-438B-9EB6-F53B62E2FCAD}']
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
        procedure SetCustMessage(NewValue: string);
        procedure SetInvFilter(NewValue: TInvoiceFilter);
        procedure SetBeginWith(NewValue: string);
        procedure SetEndWith(NewValue: string);
        procedure SetOpenItems(NewValue: TStringGrid);
        procedure SetSourceDBName(NewValue: string);
        procedure SetCustNumber(NewValue: integer);
        procedure SetExclusions(NewValue: TArray<integer>);
        procedure SetCommonHTMLTable(NewValue: string);
        procedure SetCommonHTMLRow(NewValue: string);
        procedure SetOpenItemsRefs(NewValue: TFOpenItemsRefs);
        procedure SetCtrlStatusRefs(NewValue: TFCtrlStatusRefs);
        procedure SetControlStatus(NewValue: TStringGrid);
        function  GetDocumentType(): string;
        function GetTotalAmountAggr(): double;
        function GetHTMLTable(): string;
        function GetHTMLTemp(): string;
        function GetHTMLRow(): string;
        function GetHTMLLayout(): string;
        function GetCustName(): string;
        function GetCustAddr(): string;
        function GetLBUName(): string;
        function GetLBUAddress(): string;
        function GetTelephone(): string;
        function GetBankDetails(): string;
        function GetCustMessage(): string;
        function GetInvFilter(): TInvoiceFilter;
        function GetBeginWith(): string;
        function GetEndWith(): string;
        function GetOpenItems(): TStringGrid;
        function GetSourceDBName: string;
        function GetCustNumber: integer;
        function GetExclusions(): TArray<integer>;
        function GetCommonHTMLTable(): string;
        function GetCommonHTMLRow(): string;
        function GetOpenItemsRefs(): TFOpenItemsRefs;
        function GetCtrlStatusRefs(): TFCtrlStatusRefs;
        function GetControlStatus: TStringGrid;
        property HTMLTable: string read GetHTMLTable;
        property HTMLTemp: string read GetHTMLTemp;
        property HTMLRow: string read GetHTMLRow;
        property DocumentType: string read GetDocumentType;
        property TotalAmountAggr: double read GetTotalAmountAggr;
        property OpenItems: TStringGrid read GetOpenItems write SetOpenItems;
        property OpenItemsRefs: TFOpenItemsRefs read GetOpenItemsRefs write SetOpenItemsRefs;
        property ControlStatus: TStringGrid read GetControlStatus write SetControlStatus;
        property CtrlStatusRefs: TFCtrlStatusRefs read GetCtrlStatusRefs write SetCtrlStatusRefs;
        property HTMLLayout: string read GetHTMLLayout write SetHTMLLayout;
        property CustName: string read GetCustName write SetCustName;
        property CustAddr: string read GetCustAddr write SetCustAddr;
        property LBUName: string read GetLBUName write SetLBUName;
        property LBUAddress: string read GetLBUAddress write SetLBUAddress;
        property Telephone: string read GetTelephone write SetTelephone;
        property BankDetails: string read GetBankDetails write SetBankDetails;
        property CustMessage: string read GetCustMessage write SetCustMessage;
        property InvFilter: TInvoiceFilter read GetInvFilter write SetInvFilter;
        property BeginWith: string read GetBeginWith write SetBeginWith;
        property EndWith: string read GetEndWith write SetEndWith;
        property SourceDBName: string read GetSourceDBName write SetSourceDBName;
        property CustNumber: integer read GetCustNumber write SetCustNumber;
        property Exclusions: TArray<integer> read GetExclusions write SetExclusions;
        function LoadTemplate(FileName: string; IsCtrlStatus: boolean): string;
        function SendDocument(IsUserInCopy: boolean): boolean;
    end;


    /// <summary>
    /// Provides methods and properties for setting up and sending new document (account statement or reminder)
    /// using HTML layout with defined tables for invoice list.
    /// Do not use direct implementation.
    /// </summary>
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
        var FCustMessage: string;
        var FInvFilter: TInvoiceFilter;
        var FBeginWith: string;
        var FEndWith: string;
        var FSourceDBName: string;
        var FCustNumber: integer;
        var FExclusions: TArray<integer>;
        var FCommonHTMLTable: string;
        var FCommonHTMLRow: string;
        var FOpenItems: TStringGrid;
        var FOpenItemsRefs: TFOpenItemsRefs;
        var FControlStatus: TStringGrid;
        var FCtrlStatusRefs: TFCtrlStatusRefs;
        var FTotalAmountAggr: double;
        var FDocumentType: string;
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
        procedure SetCustMessage(NewValue: string);
        procedure SetInvFilter(NewValue: TInvoiceFilter);
        procedure SetBeginWith(NewValue: string);
        procedure SetEndWith(NewValue: string);
        procedure SetOpenItems(NewValue: TStringGrid);
        procedure SetSourceDBName(NewValue: string);
        procedure SetCustNumber(NewValue: integer);
        procedure SetExclusions(NewValue: TArray<integer>);
        procedure SetCommonHTMLTable(NewValue: string);
        procedure SetCommonHTMLRow(NewValue: string);
        procedure SetOpenItemsRefs(NewValue: TFOpenItemsRefs);
        procedure SetCtrlStatusRefs(NewValue: TFCtrlStatusRefs);
        procedure SetControlStatus(NewValue: TStringGrid);
        function  GetDocumentType(): string;
        function  GetTotalAmountAggr(): double;
        function  GetHTMLTable(): string;
        function  GetHTMLTemp(): string;
        function  GetHTMLRow(): string;
        function  GetHTMLLayout(): string;
        function  GetCustName(): string;
        function  GetCustAddr(): string;
        function  GetLBUName(): string;
        function  GetLBUAddress(): string;
        function  GetTelephone(): string;
        function  GetBankDetails(): string;
        function  GetCustMessage(): string;
        function  GetInvFilter(): TInvoiceFilter;
        function  GetBeginWith(): string;
        function  GetEndWith(): string;
        function  GetOpenItems(): TStringGrid;
        function  GetSourceDBName(): string;
        function  GetCustNumber(): integer;
        function  GetExclusions(): TArray<integer>;
        function  GetCommonHTMLTable(): string;
        function  GetCommonHTMLRow(): string;
        function  GetOpenItemsRefs(): TFOpenItemsRefs;
        function  GetCtrlStatusRefs(): TFCtrlStatusRefs;
        function  GetControlStatus(): TStringGrid;
        procedure SaveOutput(FileName: string);
        function  BuildHTML(): integer;
        function  StatusCodeToDesc(TextCode: string; Source: TStringGrid): string;
        procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: Integer);
    public
        property DocumentType: string read GetDocumentType;
        property HTMLTable: string read GetHTMLTable;
        property HTMLTemp: string read GetHTMLTemp;
        property HTMLRow: string read GetHTMLRow;
        property TotalAmountAggr: double read GetTotalAmountAggr;
        property OpenItems: TStringGrid read GetOpenItems write SetOpenItems;
        property OpenItemsRefs: TFOpenItemsRefs read GetOpenItemsRefs write SetOpenItemsRefs;
        property ControlStatus: TStringGrid read GetControlStatus write SetControlStatus;
        property CtrlStatusRefs: TFCtrlStatusRefs read GetCtrlStatusRefs write SetCtrlStatusRefs;
        property HTMLLayout: string read GetHTMLLayout write SetHTMLLayout;
        property CustName: string read GetCustName write SetCustName;
        property CustAddr: string read GetCustAddr write SetCustAddr;
        property LBUName: string read GetLBUName write SetLBUName;
        property LBUAddress: string read GetLBUAddress write SetLBUAddress;
        property Telephone: string read GetTelephone write SetTelephone;
        property BankDetails: string read GetBankDetails write SetBankDetails;
        property CustMessage: string read GetCustMessage write SetCustMessage;
        property InvFilter: TInvoiceFilter read GetInvFilter write SetInvFilter;
        property BeginWith: string read GetBeginWith write SetBeginWith;
        property EndWith: string read GetEndWith write SetEndWith;
        property SourceDBName: string read GetSourceDBName write SetSourceDBName;
        property CustNumber: integer read GetCustNumber write SetCustNumber;
        property Exclusions: TArray<integer> read GetExclusions write SetExclusions;
        function LoadTemplate(FileName: string; IsCtrlStatus: boolean): string;
        function SendDocument(IsUserInCopy: boolean): boolean;
    end;


implementation


uses
    System.SysUtils,
    System.StrUtils,
    Unity.Helpers,
    Unity.Constants,
    Unity.Settings,
    Unity.SessionService;


function TDocument.LoadTemplate(FileName: string; IsCtrlStatus: boolean): string;
begin

    // ------------------------------------------------------------------------------
    // Template is made out of two files that defines HTML table and single row.
    // Note: we have two different tables, with Control Status column and without it.
    // ------------------------------------------------------------------------------
    var KeyName: string;
    var Settings: ISettings:=TSettings.Create();
    var StringList: TStringList:=TStringList.Create();
    try

        // ------------------------
        // Upload table definition.
        // ------------------------
        StringList.LoadFromFile(FileName);
        Result:=StringList.Text;

        KeyName:='SERVICE%TBL';
        if not IsCtrlStatus then
            KeyName:=KeyName.Replace('%', '1')
        else
            KeyName:=KeyName.Replace('%', '2');

        var HtmlTablePath:=Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        StringList.LoadFromFile(HtmlTablePath);
        FCommonHTMLTable:=StringList.Text;

        // ----------------------
        // Upload row definition.
        // ----------------------
        KeyName:='SERVICE%ROW';
        if not IsCtrlStatus then
            KeyName:=KeyName.Replace('%', '1')
        else
            KeyName:=KeyName.Replace('%', '2');

        var HtmlRowPath:=Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, KeyName, '');
        StringList.LoadFromFile(HtmlRowPath);
        FCommonHTMLRow:=StringList.Text;

    finally
        StringList.Free();
    end;

end;


function TDocument.SendDocument(IsUserInCopy: boolean): boolean;
begin

    Result:=False;
    if BuildHTML = 0 then Exit();

    FHTMLTable:=StringReplace(FHTMLTable,  '{ROWS}',         FHTMLStat,    [rfReplaceAll]);
    MailBody  :=StringReplace(FHTMLLayout, '{INVOICE_LIST}', FHTMLTable,   [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{ADDR_DATA}',    CustAddr,     [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{BANKS}',        BankDetails,  [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{NAME_LBU}',     LBUName,      [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{ADDR_LBU}',     LBUAddress,   [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{EMAIL}',        MailFrom,     [rfReplaceAll]);
    MailBody  :=StringReplace(MailBody,    '{TEL}',          Telephone,    [rfReplaceAll]);

    case InvFilter of

        TInvoiceFilter.ReminderOvd:
        begin
            MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
            FDocumentType:='Reminder';
        end;

        TInvoiceFilter.ReminderNonOvd:
        begin
            MailBody:=StringReplace(MailBody, '{TITLE}', 'REMINDER',  [rfReplaceAll]);
            FDocumentType:='Reminder';
        end;

        TInvoiceFilter.ShowAllItems:
        begin
            MailBody:=StringReplace(MailBody, '{TITLE}', 'STATEMENT', [rfReplaceAll]);
            FDocumentType:='Statement';
        end;

    end;

    if CustMessage <> '' then MailBody:=StringReplace(MailBody, '{TEXT}', CustMessage, [rfReplaceAll]);

    var Settings: ISettings:=TSettings.Create();

    case IsUserInCopy of
        True:  MailBcc:=TArray<string>.Create(SessionService.SessionData.EmailAddress);
        False: MailBcc:=TArray<string>.Create();
    end;

    var CallResponse: TCallResponse;
    CallResponse:=SendNowSync();
    Result:=CallResponse.IsSucceeded;

end;


procedure TDocument.SaveOutput(FileName: string);
begin

    var StringList: TStringList:=TStringList.Create();
    try
        StringList.Text:=MailBody;
        StringList.SaveToFile(FileName);
    finally
        StringList.Free();
    end;

end;


function TDocument.StatusCodeToDesc(TextCode: string; Source: TStringGrid): string;
begin

    for var iCNT:=1 to Source.RowCount do
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

    var CurAmount:=SG.Cells[FOpenItemsRefs.CurAmCol, ActualRow];
    var Amount   :=SG.Cells[FOpenItemsRefs.OpenCurAmCol, ActualRow];

    FTotalAmountAggr:=FTotalAmountAggr + Amount.ToDouble();

    CurAmount:=FormatFloat('#,##0.00', StrToFloat(CurAmount));
    Amount   :=FormatFloat('#,##0.00', StrToFloat(Amount));

    FHTMLTemp:=HTMLRow;
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_NUM}', SG.Cells[FOpenItemsRefs.InvoNoCol, ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_DAT}', SG.Cells[FOpenItemsRefs.ValDtCol,  ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{DUE_DAT}', SG.Cells[FOpenItemsRefs.DueDtCol,  ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_CUR}', SG.Cells[FOpenItemsRefs.ISOCol,    ActualRow], [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_AMT}', CurAmount, [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_OSA}', Amount,    [rfReplaceAll]);

    // -------------------------------------------------------------------
    // Text on the invoice may be very long (but not more than 200 chars),
    // the manager's decision was to put hard limit of 32 chars.
    // -------------------------------------------------------------------
    var Text:=SG.Cells[FOpenItemsRefs.Text, ActualRow];
    var Code:=SG.Cells[FOpenItemsRefs.CtrlCol, ActualRow];
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_TXT}', LeftStr(Text, 32), [rfReplaceAll]);
    FHTMLTemp:=StringReplace(FHTMLTemp, '{INV_CRL}', StatusCodeToDesc(Code, FControlStatus), [rfReplaceAll]);

    HtmlStatement:=HtmlStatement + FHTMLTemp;
    Inc(FItems);

end;


function TDocument.BuildHTML(): integer;
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

    FPos:=0;
    FItems:=0;
    FTotalAmountAggr:=0;

    FHTMLTable:=FCommonHTMLTable;
    FHTMLRow  :=FCommonHTMLRow;

    for var iCNT:=1 to OpenItems.RowCount - 1 do
    begin

        var CtrlStatus:=(OpenItems.Cells[FOpenItemsRefs.CtrlCol, iCNT]).ToInteger();

        if (OpenItems.Cells[FOpenItemsRefs.CoCodeCol, iCNT] = SourceDBName) and
        (OpenItems.Cells[FOpenItemsRefs.CustNumCol, iCNT] = CustNumber.ToString()) and
        (not TArrayUtils<integer>.Contains(CtrlStatus, FExclusions)) then
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

    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' + TChars.CRLF;

    var AddrFld1:=OpenItems.Cells[FOpenItemsRefs.Ad1Col,   FPos];
    var AddrFld2:=OpenItems.Cells[FOpenItemsRefs.Ad2Col,   FPos];
    var AddrFld3:=OpenItems.Cells[FOpenItemsRefs.Ad3Col,   FPos];
    var PoCode  :=OpenItems.Cells[FOpenItemsRefs.PnoCol,   FPos];
    var PoArea  :=OpenItems.Cells[FOpenItemsRefs.PAreaCol, FPos];

    if not String.IsNullOrWhiteSpace(AddrFld1) then CustAddr:=CustAddr + AddrFld1 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(AddrFld2) then CustAddr:=CustAddr + AddrFld2 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(AddrFld3) then CustAddr:=CustAddr + AddrFld3 + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(PoCode)   then CustAddr:=CustAddr + PoCode   + '<br />' + TChars.CRLF;
    if not String.IsNullOrWhiteSpace(PoArea)   then CustAddr:=CustAddr + PoArea   + '<br />' + TChars.CRLF;

    CustAddr:=CustAddr + '</p>' + TChars.CRLF;

    Result:=FItems;

end;


function TDocument.GetOpenItemsRefs(): TFOpenItemsRefs;
begin
    Result:=FOpenItemsRefs;
end;


function TDocument.GetCtrlStatusRefs(): TFCtrlStatusRefs;
begin
    Result:=FCtrlStatusRefs;
end;


function TDocument.GetControlStatus(): TStringGrid;
begin
    Result:=FControlStatus;
end;


function TDocument.GetDocumentType(): string;
begin
    Result:=FDocumentType;
end;


function TDocument.GetTotalAmountAggr(): double;
begin
    Result:=FTotalAmountAggr;
end;


function TDocument.GetHTMLTable(): string;
begin
    Result:=FHTMLTable;
end;


function TDocument.GetHTMLTemp(): string;
begin
    Result:=FHTMLTemp;
end;


function TDocument.GetHTMLRow(): string;
begin
    Result:=FHTMLRow;
end;


function TDocument.GetHTMLLayout(): string;
begin
    Result:=FHTMLLayout;
end;


function TDocument.GetCustName(): string;
begin
    Result:=FCustName;
end;


function TDocument.GetCustAddr(): string;
begin
    Result:=FCustAddr;
end;


function TDocument.GetLBUName(): string;
begin
    Result:=FLBUName;
end;


function TDocument.GetLBUAddress(): string;
begin
    Result:=FLBUAddress;
end;


function TDocument.GetTelephone(): string;
begin
    Result:=FTelephone;
end;


function TDocument.GetBankDetails(): string;
begin
    Result:=FBankDetails;
end;


function TDocument.GetCustMessage(): string;
begin
    Result:=FCustMessage;
end;


function TDocument.GetInvFilter(): TInvoiceFilter;
begin
    Result:=FInvFilter;
end;


function TDocument.GetBeginWith(): string;
begin
    Result:=FBeginWith;
end;


function TDocument.GetEndWith(): string;
begin
    Result:=FEndWith;
end;


function TDocument.GetOpenItems(): TStringGrid;
begin
    Result:=FOpenItems;
end;


function TDocument.GetSourceDBName(): string;
begin
    Result:=FSourceDBName;
end;


function TDocument.GetCustNumber(): integer;
begin
    Result:=FCustNumber;
end;


function TDocument.GetExclusions(): TArray<integer>;
begin
    Result:=FExclusions;
end;


function TDocument.GetCommonHTMLTable(): string;
begin
    Result:=FCommonHTMLTable;
end;


function TDocument.GetCommonHTMLRow(): string;
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


procedure TDocument.SetCustMessage(NewValue: string);
begin
    FCustMessage:=NewValue;
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


procedure TDocument.SetSourceDBName(NewValue: string);
begin
    FSourceDBName:=NewValue;
end;


procedure TDocument.SetCustNumber(NewValue: integer);
begin
    FCustNumber:=NewValue;
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

