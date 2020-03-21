unit Api.CustomerSnapshot;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TCustomerSnapshot = class
    strict private
        var FCustomerName:      string;
        var FCustomerNumber:    Int64;
        var FCountryCode:       integer;
        var FNotDue:            extended;
        var FRange1:            extended;
        var FRange2:            extended;
        var FRange3:            extended;
        var FRange4:            extended;
        var FRange5:            extended;
        var FRange6:            extended;
        var FOverdue:           extended;
        var FTotal:             extended;
        var FCreditLimit:       extended;
        var FCreditBalance:     extended;
        var FPaymentTerms:      integer;
        var FSourceDbName:      string;
        var FLedgerIso:         string;
        var FInf4:              string;
        var FGroup3:            integer;
        var FSalesResponsible:  string;
        var FCustomerGroup:     string;
        var FPersonResponsible: string;
        var FAccountType:       string;
        var FFollowUp:          string;
        var FFree1:             string;
        var FFree2:             string;
        var FFree3:             string;
        var FGeneralComment:    string;
        var FDailyComment:      string;
    public
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
        const _GeneralComment    = 'GeneralComment';
        const _DailyComment      = 'DailyComment';
        property CustomerName:      string   read FCustomerName      write FCustomerName;
        property CustomerNumber:    Int64    read FCustomerNumber    write FCustomerNumber;
        property CountryCode:       integer  read FCountryCode       write FCountryCode;
        property NotDue:            extended read FNotDue            write FNotDue;
        property Range1:            extended read FRange1            write FRange1;
        property Range2:            extended read FRange2            write FRange2;
        property Range3:            extended read FRange3            write FRange3;
        property Range4:            extended read FRange4            write FRange4;
        property Range5:            extended read FRange5            write FRange5;
        property Range6:            extended read FRange6            write FRange6;
        property Overdue:           extended read FOverdue           write FOverdue;
        property Total:             extended read FTotal             write FTotal;
        property CreditLimit:       extended read FCreditLimit       write FCreditLimit;
        property CreditBalance:     extended read FCreditBalance     write FCreditBalance;
        property PaymentTerms:      integer  read FPaymentTerms      write FPaymentTerms;
        property SourceDbName:      string   read FSourceDbName      write FSourceDbName;
        property LedgerIso:         string   read FLedgerIso         write FLedgerIso;
        property Inf4:              string   read FInf4              write FInf4;
        property Group3:            integer  read FGroup3            write FGroup3;
        property SalesResponsible:  string   read FSalesResponsible  write FSalesResponsible;
        property CustomerGroup:     string   read FCustomerGroup     write FCustomerGroup;
        property PersonResponsible: string   read FPersonResponsible write FPersonResponsible;
        property AccountType:       string   read FAccountType       write FAccountType;
        property FollowUp:          string   read FFollowUp          write FFollowUp;
        property Free1:             string   read FFree1             write FFree1;
        property Free2:             string   read FFree2             write FFree2;
        property Free3:             string   read FFree3             write FFree3;
        property GeneralComment:    string   read FGeneralComment    write FGeneralComment;
        property DailyComment:      string   read FDailyComment      write FDailyComment;
    end;


implementation


end.
