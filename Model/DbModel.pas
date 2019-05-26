unit DbModel;


interface


uses
    SqlHandler;

    // legacy code - to be removed after REST is implemented

type


    TCompanyData = class(TDataTables)
    {$TYPEINFO ON}
    published
        const CompanyData = 'Customer.CompanyData';
    public
        const Id                   = 'Id';
        const CoCode               = 'CoCode';
        const DbName               = 'DbName';
        const Branch               = 'Branch';
        const CoName               = 'CoName';
        const CoCurrency           = 'CoCurrency';
        const CoType               = 'CoType';
        const Country              = 'Country';
        const City                 = 'City';
        const FinManager           = 'FinManager';
        const InterestRate         = 'InterestRate';
        const VatNo                = 'VatNo';
        const CoAddress            = 'CoAddress';
        const Agents               = 'Agents';
        const KpiOverdueTarget     = 'KpiOverdueTarget';
        const KpiUnallocatedTarget = 'KpiUnallocatedTarget';
        const SendNoteFrom         = 'SendNoteFrom';
        const LegalTo              = 'LegalTo';
        const BankAccounts         = 'BankAccounts';
        const StatementExcept      = 'StatementExcept';
        const FirstStatement       = 'FirstStatement';
        const SecondStatement      = 'SecondStatement';
        const ReminderException1   = 'ReminderException1';
        const ReminderException2   = 'ReminderException2';
        const ReminderException3   = 'ReminderException3';
        const ReminderException4   = 'ReminderException4';
        const ReminderException5   = 'ReminderException5';
        const Duns                 = 'Duns';
        const TelephoneNumbers     = 'TelephoneNumbers';
        const IdManager            = 'IdManager';
        const IdTeamleader         = 'IdTeamleader';
        const Divisions            = 'Divisions';
    end;

    TAddressBook = class(TDataTables)
    {$TYPEINFO ON}
    published
        const AddressBook = 'Customer.AddressBook';
    public
        const Id             = 'Id';
        const UserAlias      = 'UserAlias';
        const Scuid          = 'Scuid';
        const CustomerNumber = 'CustomerNumber';
        const CustomerName   = 'CustomerName';
        const Emails         = 'Emails';
        const PhoneNumbers   = 'PhoneNumbers';
        const Contact        = 'Contact';
        const Estatements    = 'Estatements';
        const Agent          = 'Agent';
        const Division       = 'Division';
        const CoCode         = 'CoCode';
    end;

    TDailyComment = class(TDataTables)
    {$TYPEINFO ON}
    published
        const DailyComment = 'Customer.DailyComment';
    public
        const Id            = 'Id';
        const GroupId       = 'GroupId';
        const Cuid          = 'Cuid';
        const AgeDate       = 'AgeDate';
        const Stamp         = 'Stamp';
        const UserAlias     = 'UserAlias';
        const Email         = 'Email';
        const CallEvent     = 'CallEvent';
        const CallDuration  = 'CallDuration';
        const FixedComment  = 'FixedComment';
        const EmailReminder = 'EmailReminder';
        const EmailAutoStat = 'EmailAutoStat';
        const EmailManuStat = 'EmailManuStat';
        const DataCheckSum  = 'DataCheckSum';
    end;

    TGeneralComment = class(TDataTables)
    {$TYPEINFO ON}
    published
        const GeneralComment = 'Customer.GeneralComment';
    public
        const Id           = 'Id';
        const Cuid         = 'Cuid';
        const Stamp        = 'Stamp';
        const UserAlias    = 'UserAlias';
        const FixedComment = 'FixedComment';
        const FollowUp     = 'FollowUp';
        const Free1        = 'Free1';
        const Free2        = 'Free2';
        const Free3        = 'Free3';
        const fFollowUp    = 'Follow Up'; // User friendly name for given columns
    end;

    TOpenitems = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Openitems = 'Customer.Openitems';
    public
        const Id                = 'Id';
        const SourceDBName      = 'SourceDBName';
        const CustNo            = 'CustNo';
        const VoTp              = 'VoTp';
        const OpenCurAm         = 'OpenCurAm';
        const OpenAm            = 'OpenAm';
        const Nm                = 'Nm';
        const ISO               = 'ISO';
        const CurAm             = 'CurAm';
        const Am                = 'Am';
        const InvoNo            = 'InvoNo';
        const DueDt             = 'DueDt';
        const Inf4              = 'Inf4';
        const Inf7              = 'Inf7';
        const CrLmt             = 'CrLmt';
        const Ctry              = 'Ctry';
        const CPmtTrm           = 'CPmtTrm';
        const PdSts             = 'PdSts';
        const Agent             = 'Agent';
        const Ctrl              = 'Ctrl';
        const Ad1               = 'Ad1';
        const Ad2               = 'Ad2';
        const Ad3               = 'Ad3';
        const Pno               = 'Pno';
        const PArea             = 'PArea';
        const GenAcNo           = 'GenAcNo';
        const ValDt             = 'ValDt';
        const R1                = 'R1';
        const Gr3               = 'Gr3';
        const Txt               = 'Txt';
        const R8                = 'R8';
        const DirDeb            = 'DirDeb';
        const AddTxt            = 'AddTxt';
        const ExtractDateStamp  = 'ExtractDateStamp';
        const ProcessBatchKey   = 'ProcessBatchKey';
        const SalesResponsible  = 'SalesResponsible';
        const CustomerGroup     = 'CustomerGroup';
        const PersonResponsible = 'PersonResponsible';
        const AccountType       = 'AccountType';
        const PmtStat           = 'PmtStat';  // calculated "on the fly"
        const Cuid              = 'Cuid';     // calculated "on the fly"
    end;

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

        // Temporary - reflects friendly column names in string grid

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
        const fExceededAmount    = 'Exceeded Amount';
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

    end;

    TPaidinfo = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Paidinfo = 'Customer.Paidinfo';
    public
        const Id               = 'Id';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
    end;

    TTrackerData = class(TDataTables)
    {$TYPEINFO ON}
    published
        const TrackerData = 'Customer.TrackerData';
    public
        const Id             = 'Id';
        const UserAlias      = 'UserAlias';
        const Cuid           = 'Cuid';
        const CoCode         = 'CoCode';
        const Branch         = 'Branch';
        const CustomerName   = 'CustomerName';
        const Stamp          = 'Stamp';
        const SendReminder1  = 'SendReminder1';
        const SendReminder2  = 'SendReminder2';
        const SendReminder3  = 'SendReminder3';
        const SendReminder4  = 'SendReminder4';
        const Sciud          = 'Sciud';
        const ReminderLayout = 'ReminderLayout';
        const PreStatement   = 'PreStatement';
        const SendFrom       = 'SendFrom';
        const StatementTo    = 'StatementTo';
        const ReminderTo     = 'ReminderTo';
    end;

    TTrackerInvoices = class(TDataTables)
    {$TYPEINFO ON}
    published
        const TrackerInvoices = 'Customer.TrackerInvoices';
    public
        const Id           = 'Id';
        const Sk           = 'Sk';
        const Cuid         = 'Cuid';
        const InvoiceNo    = 'InvoiceNo';
        const InvoiceState = 'InvoiceState';
        const Stamp        = 'Stamp';
    end;

    TUAC = class(TDataTables)
    {$TYPEINFO ON}
    published
        const UAC = 'Customer.UAC';
    public
        const Id          = 'Id';
        const UserName    = 'UserName';
        const AccessLevel = 'AccessLevel';
        const AccessMode  = 'AccessMode';
    end;

    TGroups = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Groups = 'Customer.Groups';
    public
        const Id        = 'Id';
        const GroupId   = 'GroupId';
        const GroupName = 'GroupName';
        const Fid       = 'Fid';
    end;

    TControlStatus = class(TDataTables)
    {$TYPEINFO ON}
    published
        const ControlStatus = 'Customer.ControlStatus';
    public
        const Id          = 'Id';
        const Code        = 'Code';
        const Text        = 'Text';
        const Description = 'Description';
    end;

    TQmsReasons = class(TDataTables)
    {$TYPEINFO ON}
    published
        const QmsReasons = 'Customer.QmsReasons';
    public
        const Id          = 'Id';
        const QueryReason = 'QueryReason';
    end;

    TQmsLog = class(TDataTables)
    {$TYPEINFO ON}
    published
        const QmsLog = 'Customer.QmsLog';
    public
        const Id          = 'Id';
	    const InvoNo      = 'InvoNo';
	    const OpenAm      = 'OpenAm';
	    const Am          = 'Am';
	    const OpenCurAm   = 'OpenCurAm';
	    const CurAm       = 'CurAm';
	    const ISO         = 'ISO';
	    const DueDt       = 'DueDt';
	    const ValDt       = 'ValDt';
	    const LogType     = 'LogType';
	    const QueryReason = 'QueryReason';
        const QueryDesc   = 'QueryDesc';
        const QueryStatus = 'QueryStatus';
        const FscComment  = 'FscComment';
        const LbuComment  = 'LbuComment';
        const Receiver    = 'Receiver';
	    const UserAlias   = 'UserAlias';
	    const Stamp       = 'Stamp';
        const QueryUid    = 'QueryUid';
    end;


    TPerson = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Person = 'Erp.Person';
    public
        const Id               = 'Id';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
        const Entity           = 'Entity';
    end;

    TGroup3 = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Group3 = 'Erp.Group3';
    public
        const Id               = 'Id';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
        const Entity           = 'Entity';
    end;

    TPersonResponsible = class(TDataTables)
    {$TYPEINFO ON}
    published
        const PersonResponsible = 'Erp.PersonResponsible';
    public
        const Id               = 'Id';
        const SourceDBName     = 'SourceDBName';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
    end;

    TSalesResponsible = class(TDataTables)
    {$TYPEINFO ON}
    published
        const SalesResponsible = 'Erp.SalesResponsible';
    public
        const Id               = 'Id';
        const SourceDBName     = 'SourceDBName';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
    end;

    TAccountType = class(TDataTables)
    {$TYPEINFO ON}
    published
        const AccountType = 'Erp.AccountType';
    public
        const Id               = 'Id';
        const SourceDBName     = 'SourceDBName';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
    end;

    TCustomerGroup = class(TDataTables)
    {$TYPEINFO ON}
    published
        const CustomerGroup = 'Erp.CustomerGroup';
    public
        const Id               = 'Id';
        const SourceDBName     = 'SourceDBName';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
    end;

    TPaymentTerms = class(TDataTables)
    {$TYPEINFO ON}
    published
        const PaymentTerms = 'Erp.PaymentTerms';
    public
        const Id               = 'Id';
        const ErpCode          = 'ErpCode';
        const Description      = 'Description';
        const Month            = 'Month';
        const Days             = 'Days';
        const DaysNet          = 'DaysNet';
        const Using            = 'Using';
        const ExtractDateStamp = 'ExtractDateStamp';
        const ProcessBatchKey  = 'ProcessBatchKey';
        const Entity           = 'Entity';
    end;

    TFxRates = class(TDataTables)
    {$TYPEINFO ON}
    published
        const FxRates = 'Erp.FxRates';
    public
        const Id     = 'Id';
        const Iso    = 'Iso';
        const ExRate = 'ExRate';
        const ProcessBatchKey    = 'ProcessBatchKey';
    end;

    TSSISMaster = class(TDataTables)
    {$TYPEINFO ON}
    published
        const SSISMaster = 'Erp.SSISMaster';
    public
        const ProcessBatchKey = 'ProcessBatchKey';
        const StartDateTime   = 'StartDateTime';
        const EndDateTime     = 'EndDateTime';
        const StatusCode      = 'StatusCode';
        const SystemCode      = 'SystemCode';
    end;


    TCurrencies = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Currencies = 'Common.Currencies';
    public
        const Id           = 'Id';
        const Iso          = 'Iso';
        const CurrencyName = 'CurrencyName';
    end;

    TUnityEventLogs = class(TDataTables)
    {$TYPEINFO ON}
    published
        const UnityEventLogs = 'Common.UnityEventLogs';
    public
        const Id            = 'Id';
        const UserAlias     = 'UserAlias';
        const DateTimeStamp = 'DateTimeStamp';
        const AppEventLog   = 'AppEventLog';
        const AppName       = 'AppName';
    end;


implementation


end.

