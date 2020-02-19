unit Api.ReturnOpenItems;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.ErrorHandler,
    Api.MetaData;


type


	TReturnOpenItems = class
	strict private
        var FSourceDbName:      TArray<string>;
        var FCustNumber:        TArray<integer>;
        var FVoucherType:       TArray<integer>;
        var FOpenCurAmount:     TArray<double>;
        var FOpenAmount:        TArray<double>;
        var FCustName:          TArray<string>;
        var FIso:               TArray<string>;
        var FCurAmount:         TArray<double>;
        var FAmount:            TArray<double>;
        var FInvoiceNumber:     TArray<string>;
        var FDueDate:           TArray<string>;
        var FInf4:              TArray<string>;
        var FInf7:              TArray<string>;
        var FCreditLimit:       TArray<double>;
        var FCountry:           TArray<integer>;
        var FPmtTerms:          TArray<integer>;
        var FPmtStatus:         TArray<integer>;
        var FAgent:             TArray<string>;
        var FControlStatus:     TArray<integer>;
        var FAddress1:          TArray<string>;
        var FAddress2:          TArray<string>;
        var FAddress3:          TArray<string>;
        var FPostalNumber:      TArray<string>;
        var FPostalArea:        TArray<string>;
        var FGenAccNumber:      TArray<integer>;
        var FValueDate:         TArray<string>;
        var FDivision:          TArray<integer>;
        var FText:              TArray<string>;
        var FDirectDebit:       TArray<string>;
        var FAdditionalText:    TArray<string>;
        var FSalesResponsible:  TArray<string>;
        var FCustomerGroup:     TArray<string>;
        var FPersonResponsible: TArray<string>;
        var FAccountType:       TArray<string>;
        var FVoucherNumber:     TArray<integer>;
        var FVoucherDate:       TArray<string>;
        var FIsSucceeded:       boolean;
        var FError:             TErrorHandler;
        var FMeta:              TMetaData;
	public
        destructor Destroy(); override;
        const _SourceDbName      = 'SourceDbName';
        const _CustNumber        = 'CustomerNumber';
        const _VoucherType       = 'VoucherType';
        const _OpenCurAmount     = 'OpenCurAmount';
        const _OpenAmount        = 'OpenAmount';
        const _CustName          = 'CustomerName';
        const _Iso               = 'Iso';
        const _CurAmount         = 'CurrencyAmount';
        const _Amount            = 'Amount';
        const _InvoiceNumber     = 'InvoiceNumber';
        const _DueDate           = 'DueDate';
        const _Inf4              = 'Inf4';
        const _Inf7              = 'Inf7';
        const _CreditLimit       = 'CreditLimit';
        const _Country           = 'CountryCode';
        const _PmtTerms          = 'PaymentTerms';
        const _PmtStatus         = 'PaymentStatus';
        const _Agent             = 'Agent';
        const _ControlStatus     = 'ControlStatus';
        const _Address1          = 'Address1';
        const _Address2          = 'Address2';
        const _Address3          = 'Address3';
        const _PostalNumber      = 'PostalNumber';
        const _PostalArea        = 'PostalArea';
        const _GenAccNumber      = 'GenAccNumber';
        const _ValueDate         = 'ValueDate';
        const _Division          = 'Division';
        const _Text              = 'Text';
        const _DirectDebit       = 'DirectDebit';
        const _AdditionalText    = 'AdditionalText';
        const _SalesResponsible  = 'SalesResponsible';
        const _CustomerGroup     = 'CustomerGroup';
        const _PersonResponsible = 'PersonResponsible';
        const _AccountType       = 'AccountType';
        const _VoucherNumber     = 'VoucherNumber';
        const _VoucherDate       = 'VoucherDate';
        const _IsSucceeded       = 'IsSucceeded';
        const _Error             = 'Error';
        const _Meta              = 'Meta';
        property SourceDbName:      TArray<string>  read FSourceDbName      write FSourceDbName;
        property CustNumber:        TArray<integer> read FCustNumber        write FCustNumber;
        property VoucherType:       TArray<integer> read FVoucherType       write FVoucherType;
        property OpenCurAmount:     TArray<double>  read FOpenCurAmount     write FOpenCurAmount;
        property OpenAmount:        TArray<double>  read FOpenAmount        write FOpenAmount;
        property CustName:          TArray<string>  read FCustName          write FCustName;
        property Iso:               TArray<string>  read FIso               write FIso;
        property CurAmount:         TArray<double>  read FCurAmount         write FCurAmount;
        property Amount:            TArray<double>  read FAmount            write FAmount;
        property InvoiceNumber:     TArray<string>  read FInvoiceNumber     write FInvoiceNumber;
        property DueDate:           TArray<string>  read FDueDate           write FDueDate;
        property Inf4:              TArray<string>  read FInf4              write FInf4;
        property Inf7:              TArray<string>  read FInf7              write FInf7;
        property CreditLimit:       TArray<double>  read FCreditLimit       write FCreditLimit;
        property Country:           TArray<integer> read FCountry           write FCountry;
        property PmtTerms:          TArray<integer> read FPmtTerms          write FPmtTerms;
        property PmtStatus:         TArray<integer> read FPmtStatus         write FPmtStatus;
        property Agent:             TArray<string>  read FAgent             write FAgent;
        property ControlStatus:     TArray<integer> read FControlStatus     write FControlStatus;
        property Address1:          TArray<string>  read FAddress1          write FAddress1;
        property Address2:          TArray<string>  read FAddress2          write FAddress2;
        property Address3:          TArray<string>  read FAddress3          write FAddress3;
        property PostalNumber:      TArray<string>  read FPostalNumber      write FPostalNumber;
        property PostalArea:        TArray<string>  read FPostalArea        write FPostalArea;
        property GenAccNumber:      TArray<integer> read FGenAccNumber      write FGenAccNumber;
        property ValueDate:         TArray<string>  read FValueDate         write FValueDate;
        property Division:          TArray<integer> read FDivision          write FDivision;
        property Text:              TArray<string>  read FText              write FText;
        property DirectDebit:       TArray<string>  read FDirectDebit       write FDirectDebit;
        property AdditionalText:    TArray<string>  read FAdditionalText    write FAdditionalText;
        property SalesResponsible:  TArray<string>  read FSalesResponsible  write FSalesResponsible;
        property CustomerGroup:     TArray<string>  read FCustomerGroup     write FCustomerGroup;
        property PersonResponsible: TArray<string>  read FPersonResponsible write FPersonResponsible;
        property AccountType:       TArray<string>  read FAccountType       write FAccountType;
        property VoucherNumber:     TArray<integer> read FVoucherNumber     write FVoucherNumber;
        property VoucherDate:       TArray<string>  read FVoucherDate       write FVoucherDate;
        property IsSucceeded:       boolean         read FIsSucceeded       write FIsSucceeded;
        property Error:             TErrorHandler   read FError             write FError;
        property Meta:              TMetaData       read FMeta              write FMeta;
	end;


implementation


destructor TReturnOpenItems.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.

