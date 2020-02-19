unit Api.ReturnCustSnapshots;

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


    TReturnCustSnapshots = class
    strict private
        var FCustomerName:      TArray<string>;
        var FCustomerNumber:    TArray<integer>;
        var FCountryCode:       TArray<integer>;
        var FNotDue:            TArray<double>;
        var FRange1:            TArray<double>;
        var FRange2:            TArray<double>;
        var FRange3:            TArray<double>;
        var FRange4:            TArray<double>;
        var FRange5:            TArray<double>;
        var FRange6:            TArray<double>;
        var FOverdue:           TArray<double>;
        var FTotal:             TArray<double>;
        var FCreditLimit:       TArray<double>;
        var FCreditBalance:     TArray<double>;
        var FPaymentTerms:      TArray<integer>;
        var FSourceDbName:      TArray<string>;
        var FLedgerIso:         TArray<string>;
        var FInf4:              TArray<string>;
        var FGroup3:            TArray<string>;
        var FSalesResponsible:  TArray<string>;
        var FCustomerGroup:     TArray<string>;
        var FPersonResponsible: TArray<string>;
        var FAccountType:       TArray<string>;
        var FFollowUp:          TArray<string>;
        var FFree1:             TArray<string>;
        var FFree2:             TArray<string>;
        var FFree3:             TArray<string>;
        var FAgeDate:           string;
        var FIsSucceeded:       boolean;
        var FError:             TErrorHandler;
        var FMeta:              TMetaData;
    public
        destructor Destroy(); override;
        const _CustomerName      = 'Customer Name';
        const _CustomerNumber    = 'Customer Number';
        const _CountryCode       = 'Country Code';
        const _NotDue            = 'Not Due';
        const _Range1            = '1 - 7';
        const _Range2            = '8 - 30';
        const _Range3            = '31 - 60';
        const _Range4            = '61 - 90';
        const _Range5            = '91 - 120';
        const _Range6            = '121 - oo';
        const _Overdue           = 'Overdue';
        const _Total             = 'Total';
        const _CreditLimit       = 'Credit Limit';
        const _CreditBalance     = 'Credit Balance';
        const _PaymentTerms      = 'Payment Terms';
        const _SourceDbName      = 'SourceDbName';
        const _LedgerIso         = 'Ledger Iso';
        const _Inf4              = 'Inf4';
        const _Group3            = 'Group3';
        const _SalesResponsible  = 'Sales Responsible';
        const _CustomerGroup     = 'Customer Group';
        const _PersonResponsible = 'Person Responsible';
        const _AccountType       = 'Account Type';
        const _FollowUp          = 'Follow Up';
        const _Free1             = 'Free 1';
        const _Free2             = 'Free 2';
        const _Free3             = 'Free 3';
        const _AgeDate           = 'Age Date';
        const _IsSucceeded       = 'IsSucceeded';
        const _Error             = 'Error';
        const _Meta              = 'Meta';
        property CustomerName:      TArray<string>  read FCustomerName      write FCustomerName;
        property CustomerNumber:    TArray<integer> read FCustomerNumber    write FCustomerNumber;
        property CountryCode:       TArray<integer> read FCountryCode       write FCountryCode;
        property NotDue:            TArray<double>  read FNotDue            write FNotDue;
        property Range1:            TArray<double>  read FRange1            write FRange1;
        property Range2:            TArray<double>  read FRange2            write FRange2;
        property Range3:            TArray<double>  read FRange3            write FRange3;
        property Range4:            TArray<double>  read FRange4            write FRange4;
        property Range5:            TArray<double>  read FRange5            write FRange5;
        property Range6:            TArray<double>  read FRange6            write FRange6;
        property Overdue:           TArray<double>  read FOverdue           write FOverdue;
        property Total:             TArray<double>  read FTotal             write FTotal;
        property CreditLimit:       TArray<double>  read FCreditLimit       write FCreditLimit;
        property CreditBalance:     TArray<double>  read FCreditBalance     write FCreditBalance;
        property PaymentTerms:      TArray<integer> read FPaymentTerms      write FPaymentTerms;
        property SourceDbName:      TArray<string>  read FSourceDbName      write FSourceDbName;
        property LedgerIso:         TArray<string>  read FLedgerIso         write FLedgerIso;
        property Inf4:              TArray<string>  read FInf4              write FInf4;
        property Group3:            TArray<string>  read FGroup3            write FGroup3;
        property SalesResponsible:  TArray<string>  read FSalesResponsible  write FSalesResponsible;
        property CustomerGroup:     TArray<string>  read FCustomerGroup     write FCustomerGroup;
        property PersonResponsible: TArray<string>  read FPersonResponsible write FPersonResponsible;
        property AccountType:       TArray<string>  read FAccountType       write FAccountType;
        property FollowUp:          TArray<string>  read FFollowUp          write FFollowUp;
        property Free1:             TArray<string>  read FFree1             write FFree1;
        property Free2:             TArray<string>  read FFree2             write FFree2;
        property Free3:             TArray<string>  read FFree3             write FFree3;
        property AgeDate:           string          read FAgeDate           write FAgeDate;
        property IsSucceeded:       boolean         read FIsSucceeded       write FIsSucceeded;
        property Error:             TErrorHandler   read FError             write FError;
        property Meta:              TMetaData       read FMeta              write FMeta;
    end;


implementation


destructor TReturnCustSnapshots.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;

end.

