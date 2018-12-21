
{$I .\Include\Header.inc}

unit Model;


interface


uses
    Main,
    SQL,
    Windows,
    Classes,
    SysUtils,
    StrUtils,
    ADODB;

    /// <remarks>
    ///     Because we use ADODB and no other ORM, we encapsulate columns name (fields) under table name (class), so we have one point of reference
    ///     and yet we can use it to build our own SQL statement(s), although templates are preferred over manual SQL.
    /// </remarks>

type


    // ------------------------------------------------------------------------------------------------------------------------------------ SCHEMA: CUSTOMER //


    TCompanyData = class(TDataTables)
    {$TYPEINFO ON}
    published
        const CompanyData = 'Customer.CompanyData';
    public
        const ID                     = 'Id';
        const CO_CODE                = 'CoCode';
        const DBNAME                 = 'DbName';
        const BRANCH                 = 'Branch';
        const CONAME                 = 'CoName';
        const COCURRENCY             = 'CoCurrency';
        const COTYPE                 = 'CoType';
        const COUNTRY                = 'Country';
        const CITY                   = 'City';
        const FMANAGER               = 'FinManager';
        const INTEREST_RATE          = 'InterestRate';
        const VATNO                  = 'VatNo';
        const COADDRESS              = 'CoAddress';
        const AGENTS                 = 'Agents';
        const KPI_OVERDUE_TARGET     = 'KpiOverdueTarget';
        const KPI_UNALLOCATED_TARGET = 'KpiUnallocatedTarget';
        const SEND_NOTE_FROM         = 'SendNoteFrom';
        const LEGALTO                = 'LegalTo';
        const BANKDETAILS            = 'BankAccounts';
        const STAT_EXCEPT            = 'StatementExcept';
        const FIRST_STATEMENT        = 'FirstStatement';
        const SECOND_STATEMENT       = 'SecondStatement';
        const REM_EX1                = 'ReminderException1';
        const REM_EX2                = 'ReminderException2';
        const REM_EX3                = 'ReminderException3';
        const REM_EX4                = 'ReminderException4';
        const REM_EX5                = 'ReminderException5';
        const DUNS                   = 'Duns';
        const Telephone              = 'TelephoneNumbers';
        const MAN_ID                 = 'IdManager';
        const TL_ID                  = 'IdTeamleader';
        const DIVISIONS              = 'Divisions';
    end;

    TAddressBook = class(TDataTables)
    {$TYPEINFO ON}
    published
        const AddressBook = 'Customer.AddressBook';
    public
        const ID              = 'Id';
        const USER_ALIAS      = 'UserAlias';
        const SCUID           = 'Scuid';
        const CUSTOMER_NUMBER = 'CustomerNumber';
        const CUSTOMER_NAME   = 'CustomerName';
        const EMAILS          = 'Emails';
        const PHONE_NUMBERS   = 'PhoneNumbers';
        const CONTACT         = 'Contact';
        const ESTATEMENTS     = 'Estatements';
        const AGENT           = 'Agent';
        const DIVISION        = 'Division';
        const COCODE          = 'CoCode';
    end;

    TDailyComment = class(TDataTables)
    {$TYPEINFO ON}
    published
        const DailyComment = 'Customer.DailyComment';
    public
        const ID             = 'Id';
        const GROUP_ID       = 'GroupId';
        const CUID           = 'Cuid';
        const AGEDATE        = 'AgeDate';
        const STAMP          = 'Stamp';
        const USER_ALIAS     = 'UserAlias';
        const EMAIL          = 'Email';
        const CALLEVENT      = 'CallEvent';
        const CALLDURATION   = 'CallDuration';
        const FIXCOMMENT     = 'FixedComment';
        const EMAIL_Reminder = 'EmailReminder';
        const EMAIL_AutoStat = 'EmailAutoStat';
        const EMAIL_ManuStat = 'EmailManuStat';
        const DATACHECKSUM   = 'DataCheckSum';
    end;

    TGeneralComment = class(TDataTables)
    {$TYPEINFO ON}
    published
        const GeneralComment = 'Customer.GeneralComment';
    public
        const ID         = 'Id';
        const CUID       = 'Cuid';
        const STAMP      = 'Stamp';
        const USER_ALIAS = 'UserAlias';
        const FIXCOMMENT = 'FixedComment';
        const FOLLOWUP   = 'FollowUp';
        const Free1      = 'Free1';
        const Free2      = 'Free2';
        const Free3      = 'Free3';
        // User friendly name for given columns.
        const fFOLLOWUP  = 'Follow Up';
    end;

    TOpenitems = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Openitems = 'Customer.Openitems';
    public
        const ID                = 'Id';
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
        const CUID              = 'Cuid';     // calculated "on the fly"
    end;

    TSnapshots = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Snapshots = 'Customer.Snapshots';
    public
        const ID                = 'Id';
        const GROUP_ID          = 'GroupId';
        const AGE_DATE          = 'AgeDate';
        const SNAPSHOT_DT       = 'SnapshotDt';
        const CUSTOMER_NAME     = 'CustomerName';
        const CUSTOMER_NUMBER   = 'CustomerNumber';
        const COUNTRY_CODE      = 'CountryCode';
        const NOT_DUE           = 'NotDue';
        const RANGE1            = 'Range1';
        const RANGE2            = 'Range2';
        const RANGE3            = 'Range3';
        const RANGE4            = 'Range4';
        const RANGE5            = 'Range5';
        const RANGE6            = 'Range6';
        const OVERDUE           = 'Overdue';
        const TOTAL             = 'Total';
        const CREDIT_LIMIT      = 'CreditLimit';
        const EXCEEDED_AMOUNT   = 'ExceededAmount';
        const PAYMENT_TERMS     = 'PaymentTerms';
        const AGENT             = 'Agent';
        const DIVISION          = 'Division';
        const CO_CODE           = 'CoCode';
        const LEDGER_ISO        = 'LedgerIso';
        const INF4              = 'Inf4';
        const INF7              = 'Inf7';
        const PERSON            = 'Person';
        const GROUP3            = 'Group3';
        const RISK_CLASS        = 'RiskClass';
        const CUID              = 'Cuid';
        const SalesResponsible  = 'SalesResponsible';
        const CustomerGroup     = 'CustomerGroup';
        const PersonResponsible = 'PersonResponsible';
        const AccountType       = 'AccountType';
        // Temporary - reflects friendly column names in string grid.
        const fCUSTOMER_NAME     = 'Customer Name';
        const fCUSTOMER_NUMBER   = 'Customer Number';
        const fNOT_DUE           = 'Not Due';
        const fCOUNTRY_CODE      = 'Country Code';
        const fRANGE1            = '1 - 7';
        const fRANGE2            = '8 - 30';
        const fRANGE3            = '31 - 60';
        const fRANGE4            = '61 - 90';
        const fRANGE5            = '91 - 120';
        const fRANGE6            = '121 - oo';
        const fTOTAL             = 'Total';
        const fOVERDUE           = 'Overdue';
        const fCREDIT_LIMIT      = 'Credit Limit';
        const fEXCEEDED_AMOUNT   = 'Exceeded Amount';
        const fAGENT             = 'Agent';
        const fCO_CODE           = 'Co Code';
        const fPAYMENT_TERMS     = 'Payment Terms';
        const fDIVISION          = 'Division';
        const fLEDGER_ISO        = 'Ledger Iso';
        const fINF4              = 'Inf4';
        const fINF7              = 'Inf7';
        const fPERSON            = 'Person';
        const fGROUP3            = 'Group3';
        const fRISK_CLASS        = 'Risk Class';
        const fCUID              = 'Cuid';
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
        const ID          = 'Id';
        const ERP_CODE    = 'ErpCode';
        const DESCRIPTION = 'Description';
        const STAMP       = 'ExtractDateStamp';
        const KEY         = 'ProcessBatchKey';
    end;

    TTrackerData = class(TDataTables)
    {$TYPEINFO ON}
    published
        const TrackerData = 'Customer.TrackerData';
    public
        const ID          = 'Id';
        const USER_ALIAS  = 'UserAlias';
        const CUID        = 'Cuid';
        const CO_CODE     = 'CoCode';
        const BRANCH      = 'Branch';
        const CUSTNAME    = 'CustomerName';
        const STAMP       = 'Stamp';
        const INDV_REM1   = 'SendReminder1';
        const INDV_REM2   = 'SendReminder2';
        const INDV_REM3   = 'SendReminder3';
        const INDV_REM4   = 'SendReminder4';
        const SCUID       = 'Sciud';
        const LAYOUT      = 'ReminderLayout';
        const STATEMENT   = 'PreStatement';
        const SENDFROM    = 'SendFrom';
        const STATEMENTTO = 'StatementTo';
        const REMINDERTO  = 'ReminderTo';
    end;

    TTrackerInvoices = class(TDataTables)
    {$TYPEINFO ON}
    published
        const TrackerInvoices = 'Customer.TrackerInvoices';
    public
        const ID           = 'Id';
        const SK           = 'Sk';
        const CUID         = 'Cuid';
        const INVOICENO    = 'InvoiceNo';
        const INVOICESTATE = 'InvoiceState';
        const STAMP        = 'Stamp';
    end;

    TUAC = class(TDataTables)
    {$TYPEINFO ON}
    published
        const UAC = 'Customer.UAC';
    public
        const ID           = 'Id';
        const USERNAME     = 'UserName';
        const ACCESS_LEVEL = 'AccessLevel';
        const ACCESS_MODE  = 'AccessMode';
    end;

    TGroups = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Groups = 'Customer.Groups';
    public
        const ID         = 'Id';
        const GROUP_ID   = 'GroupId';
        const GROUP_NAME = 'GroupName';
        const FID        = 'Fid';
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


    // ----------------------------------------------------------------------------------------------------------------------------------------- SCHEMA: ERP //


    TPerson = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Person = 'Erp.Person';
    public
        const ID          = 'Id';
        const ERP_CODE    = 'ErpCode';
        const DESCRIPTION = 'Description';
        const STAMP       = 'ExtractDateStamp';
        const KEY         = 'ProcessBatchKey';
        const COCODE      = 'Entity';
    end;

    TGroup3 = class(TDataTables)
    {$TYPEINFO ON}
    published
        const Group3 = 'Erp.Group3';
    public
        const ID          = 'Id';
        const ERP_CODE    = 'ErpCode';
        const DESCRIPTION = 'Description';
        const STAMP       = 'ExtractDateStamp';
        const KEY         = 'ProcessBatchKey';
        const COCODE      = 'Entity';
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
        const ID          = 'Id';
        const ERP_CODE    = 'ErpCode';
        const DESCRIPTION = 'Description';
        const I_MONTH     = 'Month';
        const I_DAYS      = 'Days';
        const I_DAYS_NET  = 'DaysNet';
        const I_USING     = 'Using';
        const STAMP       = 'ExtractDateStamp';
        const KEY         = 'ProcessBatchKey';
        const COCODE      = 'Entity';
    end;

    TFxRates = class(TDataTables)
    {$TYPEINFO ON}
    published
        const FxRates = 'Erp.FxRates';
    public
        const ID     = 'Id';
        const ISO    = 'Iso';
        const EXRATE = 'ExRate';
        const KEY    = 'ProcessBatchKey';
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


    // -------------------------------------------------------------------------------------------------------------------------------------- SCHEMA: COMMON //


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

