unit Api.OpenItemsFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


	TOpenItemsFields = class
	strict private
        var FSourceDbName:      string;
        var FCustNumber:        integer;
        var FVoucherType:       integer;
        var FOpenCurAmount:     double;
        var FOpenAmount:        double;
        var FCustName:          string;
        var FIso:               string;
        var FCurAmount:         double;
        var FAmount:            double;
        var FInvoiceNumber:     string;
        var FDueDate:           string;
        var FInf4:              string;
        var FInf7:              string;
        var FCreditLimit:       double;
        var FCountry:           integer;
        var FPmtTerms:          integer;
        var FPmtStatus:         integer;
        var FAgent:             string;
        var FControlStatus:     integer;
        var FAddress1:          string;
        var FAddress2:          string;
        var FAddress3:          string;
        var FPostalNumber:      string;
        var FPostalArea:        string;
        var FGenAccNumber:      integer;
        var FValueDate:         string;
        var FDivision:          integer;
        var FText:              string;
        var FDirectDebit:       string;
        var FAdditionalText:    string;
        var FSalesResponsible:  string;
        var FCustomerGroup:     string;
        var FPersonResponsible: string;
        var FAccountType:       string;
        var FVoucherNumber:     integer;
        var FVoucherDate:       string;
	public
        const _SourceDbName      = 'SourceDbName';
        const _CustNumber        = 'Customer Number';
        const _VoucherType       = 'Voucher Type';
        const _OpenCurAmount     = 'Open CurAmount';
        const _OpenAmount        = 'Open Amount';
        const _CustName          = 'Customer Name';
        const _Iso               = 'Iso';
        const _CurAmount         = 'Currency Amount';
        const _Amount            = 'Amount';
        const _InvoiceNumber     = 'Invoice Number';
        const _DueDate           = 'Due Date';
        const _Inf4              = 'Inf4';
        const _Inf7              = 'Inf7';
        const _CreditLimit       = 'Credit Limit';
        const _Country           = 'Country Code';
        const _PmtTerms          = 'Payment Terms';
        const _PmtStatus         = 'Payment Status';
        const _Agent             = 'Agent';
        const _ControlStatus     = 'Control Status';
        const _Address1          = 'Address 1';
        const _Address2          = 'Address 2';
        const _Address3          = 'Address 3';
        const _PostalNumber      = 'Postal Number';
        const _PostalArea        = 'Postal Area';
        const _GenAccNumber      = 'Gen AccNumber';
        const _ValueDate         = 'Value Date';
        const _Division          = 'Division';
        const _Text              = 'Text';
        const _DirectDebit       = 'Direct Debit';
        const _AdditionalText    = 'Additional Text';
        const _SalesResponsible  = 'Sales Responsible';
        const _CustomerGroup     = 'Customer Group';
        const _PersonResponsible = 'Person Responsible';
        const _AccountType       = 'Account Type';
        const _VoucherNumber     = 'Voucher Number';
        const _VoucherDate       = 'Voucher Date';
        property SourceDbName:      string  read FSourceDbName      write FSourceDbName;
        property CustNumber:        integer read FCustNumber        write FCustNumber;
        property VoucherType:       integer read FVoucherType       write FVoucherType;
        property OpenCurAmount:     double  read FOpenCurAmount     write FOpenCurAmount;
        property OpenAmount:        double  read FOpenAmount        write FOpenAmount;
        property CustName:          string  read FCustName          write FCustName;
        property Iso:               string  read FIso               write FIso;
        property CurAmount:         double  read FCurAmount         write FCurAmount;
        property Amount:            double  read FAmount            write FAmount;
        property InvoiceNumber:     string  read FInvoiceNumber     write FInvoiceNumber;
        property DueDate:           string  read FDueDate           write FDueDate;
        property Inf4:              string  read FInf4              write FInf4;
        property Inf7:              string  read FInf7              write FInf7;
        property CreditLimit:       double  read FCreditLimit       write FCreditLimit;
        property Country:           integer read FCountry           write FCountry;
        property PmtTerms:          integer read FPmtTerms          write FPmtTerms;
        property PmtStatus:         integer read FPmtStatus         write FPmtStatus;
        property Agent:             string  read FAgent             write FAgent;
        property ControlStatus:     integer read FControlStatus     write FControlStatus;
        property Address1:          string  read FAddress1          write FAddress1;
        property Address2:          string  read FAddress2          write FAddress2;
        property Address3:          string  read FAddress3          write FAddress3;
        property PostalNumber:      string  read FPostalNumber      write FPostalNumber;
        property PostalArea:        string  read FPostalArea        write FPostalArea;
        property GenAccNumber:      integer read FGenAccNumber      write FGenAccNumber;
        property ValueDate:         string  read FValueDate         write FValueDate;
        property Division:          integer read FDivision          write FDivision;
        property Text:              string  read FText              write FText;
        property DirectDebit:       string  read FDirectDebit       write FDirectDebit;
        property AdditionalText:    string  read FAdditionalText    write FAdditionalText;
        property SalesResponsible:  string  read FSalesResponsible  write FSalesResponsible;
        property CustomerGroup:     string  read FCustomerGroup     write FCustomerGroup;
        property PersonResponsible: string  read FPersonResponsible write FPersonResponsible;
        property AccountType:       string  read FAccountType       write FAccountType;
        property VoucherNumber:     integer read FVoucherNumber     write FVoucherNumber;
        property VoucherDate:       string  read FVoucherDate       write FVoucherDate;
	end;


implementation


end.
