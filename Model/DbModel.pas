unit DbModel;

// ----------------------------------------
// Database model for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Handler.Sql;

    // legacy code - to be removed after REST is implemented

type


    TSnapshots = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Snapshots = 'Customer.Snapshots';
    public
        const Id                = 'Id';
        const GroupId           = 'GroupId';
        const AgeDate           = 'AgeDate';
        const SnapshotDt        = 'SnapshotDt';
        const CustomerName      = 'CustomerName';
        const CustomerNumber    = 'CustomerNumber';
        const CountryCode       = 'CountryCode';
        const NotDue            = 'NotDue';
        const Range1            = 'Range1';
        const Range2            = 'Range2';
        const Range3            = 'Range3';
        const Range4            = 'Range4';
        const Range5            = 'Range5';
        const Range6            = 'Range6';
        const Overdue           = 'Overdue';
        const Total             = 'Total';
        const CreditLimit       = 'CreditLimit';
        const ExceededAmount    = 'ExceededAmount';
        const PaymentTerms      = 'PaymentTerms';
        const Agent             = 'Agent';
        const Division          = 'Division';
        const CoCode            = 'CoCode';
        const LedgerIso         = 'LedgerIso';
        const Inf4              = 'Inf4';
        const Inf7              = 'Inf7';
        const Person            = 'Person';
        const Group3            = 'Group3';
        const RiskClass         = 'RiskClass';
        const Cuid              = 'Cuid';
        const SalesResponsible  = 'SalesResponsible';
        const CustomerGroup     = 'CustomerGroup';
        const PersonResponsible = 'PersonResponsible';
        const AccountType       = 'AccountType';
        const fCustomerName      = 'Customer Name';
        const fCustomerNumber    = 'Customer Number';
        const fNotDue            = 'Not Due';
        const fCountryCode       = 'Country Code';
        const fRange1            = '1 - 7';
        const fRange2            = '8 - 30';
        const fRange3            = '31 - 60';
        const fRange4            = '61 - 90';
        const fRange5            = '91 - 120';
        const fRange6            = '121 - oo';
        const fTotal             = 'Total';
        const fOverdue           = 'Overdue';
        const fCreditLimit       = 'Credit Limit';
        const fCreditBalance     = 'Credit Balance';
        const fAgent             = 'Agent';
        const fCoCode            = 'Co Code';
        const fPaymentTerms      = 'Payment Terms';
        const fDivision          = 'Division';
        const fLedgerIso         = 'Ledger Iso';
        const fInf4              = 'Inf4';
        const fInf7              = 'Inf7';
        const fPerson            = 'Person';
        const fGroup3            = 'Group3';
        const fRiskClass         = 'Risk Class';
        const fCuid              = 'Cuid';
        const fSalesResponsible  = 'Sales Responsible';
        const fCustomerGroup     = 'Customer Group';
        const fPersonResponsible = 'Person Responsible';
        const fAccountType       = 'Account Type';
        const fFollowUp          = 'Follow Up';
        const fFree1             = 'Free 1';
        const fFree2             = 'Free 2';
        const fFree3             = 'Free 3';
    end;


implementation


end.

