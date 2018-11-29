
{$I .\Include\Header.inc}

unit Model;

interface

uses
    Main, SQL, Windows, Classes, SysUtils, StrUtils, ADODB;

    /// <remarks>
    ///     Because we use ADODB and no other ORM, we encapsulate columns name under table name (class), so we have one point of reference
    ///     and yet we can use it to build own SQL statements much like LINQ allows.
    /// </remarks>

type

    /// <summary>
    ///     Company Data holds all the basic information about the certain entity.
    /// </summary>

    TCompany = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID                     : string = 'Id';   // Primary key
        const CO_CODE                : string = 'CoCode';
        const DBNAME                 : string = 'DbName';
        const BRANCH                 : string = 'Branch';
        const CONAME                 : string = 'CoName';
        const COCURRENCY             : string = 'CoCurrency';
        const COTYPE                 : string = 'CoType';
        const COUNTRY                : string = 'Country';
        const CITY                   : string = 'City';
        const FMANAGER               : string = 'FinManager';
        const INTEREST_RATE          : string = 'InterestRate';
        const VATNO                  : string = 'VatNo';
        const COADDRESS              : string = 'CoAddress';
        const AGENTS                 : string = 'Agents';
        const KPI_OVERDUE_TARGET     : string = 'KpiOverdueTarget';
        const KPI_UNALLOCATED_TARGET : string = 'KpiUnallocatedTarget';
        const SEND_NOTE_FROM         : string = 'SendNoteFrom';
        const LEGALTO                : string = 'LegalTo';
        const BANKDETAILS            : string = 'BankAccounts';
        const STAT_EXCEPT            : string = 'StatementExcept';
        const FIRST_STATEMENT        : string = 'FirstStatement';
        const SECOND_STATEMENT       : string = 'SecondStatement';
        const REM_EX1                : string = 'ReminderException1';
        const REM_EX2                : string = 'ReminderException2';
        const REM_EX3                : string = 'ReminderException3';
        const REM_EX4                : string = 'ReminderException4';
        const REM_EX5                : string = 'ReminderException5';
        const DUNS                   : string = 'Duns';
        const Telephone              : string = 'TelephoneNumbers';
        const MAN_ID                 : string = 'IdManager';    // Foreign Key -> Primary Key in "MANAGERS"
        const TL_ID                  : string = 'IdTeamleader'; // Foreign Key -> Primary Key in "TEAM LEADERS"
        const DIVISIONS              : string = 'Divisions';
    end;

    /// <summary>
    ///     Managers assigned to given entity.
    /// </summary>

    TManagers = class(TDataTables)  { MANY-TO-MANY }
    {$TYPEINFO ON}
    public
        const ID        : string = 'Id';            // Primary Key <- Foreign Key from "COMPANY DATA" }
        const ManagerAR : string = 'ManagerAR';
        const ManagerAP : string = 'ManagerAP';
        const ManagerGL : string = 'ManagerGL';
    end;

    /// <summary>
    ///     Team Leaders assigned to given entity.
    /// </summary>

    TTeamleaders = class(TDataTables)  { MANY-TO-MANY }
    {$TYPEINFO ON}
    public
        const ID  :  string = 'Id';                 // Primary Key <- Foreign Key "COMPANY DATA" }
        const AP1 :  string = 'TeamleaderAp1';
        const AP2 :  string = 'TeamleaderAp2';
        const AR1 :  string = 'TeamleaderAr1';
        const AR2 :  string = 'TeamleaderAr2';
        const GL1 :  string = 'TeamleaderGl1';
        const GL2 :  string = 'TeamleaderGl2';
    end;

    /// <summary>
    ///     Address Book.
    /// </summary>

    TAddressBook = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID               : string = 'Id';     // Primary key
        const USER_ALIAS       : string = 'UserAlias';
        const SCUID            : string = 'Scuid';  // Constraint unique
        const CUSTOMER_NUMBER  : string = 'CustomerNumber';
        const CUSTOMER_NAME    : string = 'CustomerName';
        const EMAILS           : string = 'Emails';
        const PHONE_NUMBERS    : string = 'PhoneNumbers';
        const CONTACT          : string = 'Contact';
        const ESTATEMENTS      : string = 'Estatements';
        const AGENT            : string = 'Agent';
        const DIVISION         : string = 'Division';
        const COCODE           : string = 'CoCode';
    end;

    /// <summary>
    ///     Daily comment table, holds all credit controller comment per customer.
    ///     Aditionally, it also holds number of emails sent and calls performed per day per customer.
    /// </summary>

    TDaily = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID            : string = 'Id';    // Primary key
        const GROUP_ID      : string = 'GroupId';
        const CUID          : string = 'Cuid';
        const AGEDATE       : string = 'AgeDate';
        const STAMP         : string = 'Stamp';
        const USER_ALIAS    : string = 'UserAlias';
        const EMAIL         : string = 'Email';
        const CALLEVENT     : string = 'CallEvent';
        const CALLDURATION  : string = 'CallDuration';
        const FIXCOMMENT    : string = 'FixedComment';
        const EMAIL_Reminder: string = 'EmailReminder';
        const EMAIL_AutoStat: string = 'EmailAutoStat';
        const EMAIL_ManuStat: string = 'EmailManuStat';
        const DATACHECKSUM  : string = 'DataCheckSum';
    end;

    /// <summary>
    ///     General purpose comment and folow-up table, it also contain FREE1 and FREE2 columns. Commentary is kept at the customer level.
    /// </summary>

    TGeneral = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID          : string = 'Id';      // Primary key
        const CUID        : string = 'Cuid';    // Constraint unique
        const STAMP       : string = 'Stamp';
        const USER_ALIAS  : string = 'UserAlias';
        const FIXCOMMENT  : string = 'FixedComment';
        const FOLLOWUP    : string = 'FollowUp';
        const Free1       : string = 'Free1';
        const Free2       : string = 'Free2';
        const Free3       : string = 'Free3';

        /// <remarks>
        ///     User friendly name for given columns.
        /// </remarks>

        const fFOLLOWUP   : string = 'Follow Up';
    end;

    /// <summary>
    ///     Open items from ERP system. Feeded on regular basis.
    /// </summary>

    TOpenitems = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID               : string = 'Id';     // Primary key
        const SourceDBName     : string = 'SourceDBName';
        const CustNo           : string = 'CustNo';
        const VoTp             : string = 'VoTp';
        const OpenCurAm        : string = 'OpenCurAm';
        const OpenAm           : string = 'OpenAm';
        const Nm               : string = 'Nm';
        const ISO              : string = 'ISO';
        const CurAm            : string = 'CurAm';
        const Am               : string = 'Am';
        const InvoNo           : string = 'InvoNo';
        const DueDt            : string = 'DueDt';
        const Inf4             : string = 'Inf4';
        const Inf7             : string = 'Inf7';
        const CrLmt            : string = 'CrLmt';
        const Ctry             : string = 'Ctry';
        const CPmtTrm          : string = 'CPmtTrm';
        const PdSts            : string = 'PdSts';
        const Agent            : string = 'Agent';
        const Ctrl             : string = 'Ctrl';
        const Ad1              : string = 'Ad1';
        const Ad2              : string = 'Ad2';
        const Ad3              : string = 'Ad3';
        const Pno              : string = 'Pno';
        const PArea            : string = 'PArea';
        const GenAcNo          : string = 'GenAcNo';
        const ValDt            : string = 'ValDt';
        const R1               : string = 'R1';
        const Gr3              : string = 'Gr3';
        const Txt              : string = 'Txt';
        const R8               : string = 'R8';
        const DirDeb           : string = 'DirDeb';
        const AddTxt           : string = 'AddTxt';
        const ExtractDateStamp : string = 'ExtractDateStamp';
        const ProcessBatchKey  : string = 'ProcessBatchKey';
        const SalesResponsible : string = 'SalesResponsible';
        const CustomerGroup    : string = 'CustomerGroup';
        const PersonResponsible: string = 'PersonResponsible';
        const AccountType      : string = 'AccountType';

        /// <remarks>
        ///     CUID and PMTSTAT are additional columns calculated "on the fly".
        /// </remarks>

        const PmtStat          : string = 'PmtStat';
        const CUID             : string = 'Cuid';
    end;

    /// <summary>
    ///     Snapshots from open items. Performed on daily basis.
    /// </summary>

    TSnapshots = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID               : string = 'Id';  // Primary key
        const GROUP_ID         : string = 'GroupId';
        const AGE_DATE         : string = 'AgeDate';
        const SNAPSHOT_DT      : string = 'SnapshotDt';
        const CUSTOMER_NAME    : string = 'CustomerName';
        const CUSTOMER_NUMBER  : string = 'CustomerNumber';
        const COUNTRY_CODE     : string = 'CountryCode';
        const NOT_DUE          : string = 'NotDue';
        const RANGE1           : string = 'Range1';
        const RANGE2           : string = 'Range2';
        const RANGE3           : string = 'Range3';
        const RANGE4           : string = 'Range4';
        const RANGE5           : string = 'Range5';
        const RANGE6           : string = 'Range6';
        const OVERDUE          : string = 'Overdue';
        const TOTAL            : string = 'Total';
        const CREDIT_LIMIT     : string = 'CreditLimit';
        const EXCEEDED_AMOUNT  : string = 'ExceededAmount';
        const PAYMENT_TERMS    : string = 'PaymentTerms';
        const AGENT            : string = 'Agent';
        const DIVISION         : string = 'Division';
        const CO_CODE          : string = 'CoCode';
        const LEDGER_ISO       : string = 'LedgerIso';
        const INF4             : string = 'Inf4';
        const INF7             : string = 'Inf7';
        const PERSON           : string = 'Person';
        const GROUP3           : string = 'Group3';
        const RISK_CLASS       : string = 'RiskClass';
        const CUID             : string = 'Cuid';
        const SalesResponsible : string = 'SalesResponsible';
        const CustomerGroup    : string = 'CustomerGroup';
        const PersonResponsible: string = 'PersonResponsible';
        const AccountType      : string = 'AccountType';

        /// <remarks>
        ///     Temporary - reflects friendly column names in string grid.
        /// </remarks>

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

    /// <summary>
    ///     Paid info, fixed for all enitities.
    /// </summary>

    TPaidinfo = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID          : string = 'Id';  // Primary key
        const ERP_CODE    : string = 'ErpCode';
        const DESCRIPTION : string = 'Description';
        const STAMP       : string = 'ExtractDateStamp';
        const KEY         : string = 'ProcessBatchKey';
    end;

    /// <summary>
    ///     Feed from ERP. Contain all sales persons for each entity.
    /// </summary>

    TPerson = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID            : string = 'Id';    // Primary key
        const ERP_CODE      : string = 'ErpCode';
        const DESCRIPTION   : string = 'Description';
        const STAMP         : string = 'ExtractDateStamp';
        const KEY           : string = 'ProcessBatchKey';
        const COCODE        : string = 'Entity';
    end;

    /// <summary>
    ///     Feed from ERP. Contain all sales managers for each entity.
    /// </summary>

    TGroup3 = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID            : string = 'Id';    // Primary key
        const ERP_CODE      : string = 'ErpCode';
        const DESCRIPTION   : string = 'Description';
        const STAMP         : string = 'ExtractDateStamp';
        const KEY           : string = 'ProcessBatchKey';
        const COCODE        : string = 'Entity';
    end;

    /// <summary>
    ///
    /// </summary>

    TPersonResponsible = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id              : string = 'Id';
        const SourceDBName    : string = 'SourceDBName';
        const ErpCode         : string = 'ErpCode';
        const Description     : string = 'Description';
        const ExtractDateStamp: string = 'ExtractDateStamp';
        const ProcessBatchKey : string = 'ProcessBatchKey';
    end;

    /// <summary>
    ///
    /// </summary>

    TSalesResponsible = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id              : string = 'Id';
        const SourceDBName    : string = 'SourceDBName';
        const ErpCode         : string = 'ErpCode';
        const Description     : string = 'Description';
        const ExtractDateStamp: string = 'ExtractDateStamp';
        const ProcessBatchKey : string = 'ProcessBatchKey';
    end;

    /// <summary>
    ///
    /// </summary>

    TAccountType = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id              : string = 'Id';
        const SourceDBName    : string = 'SourceDBName';
        const ErpCode         : string = 'ErpCode';
        const Description     : string = 'Description';
        const ExtractDateStamp: string = 'ExtractDateStamp';
        const ProcessBatchKey : string = 'ProcessBatchKey';
    end;

    /// <summary>
    ///
    /// </summary>

    TCustomerGroup = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id              : string = 'Id';
        const SourceDBName    : string = 'SourceDBName';
        const ErpCode         : string = 'ErpCode';
        const Description     : string = 'Description';
        const ExtractDateStamp: string = 'ExtractDateStamp';
        const ProcessBatchKey : string = 'ProcessBatchKey';
    end;

    /// <summary>
    ///     Feed from ERP. Contain all possible payment terms for each entity.
    /// </summary>

    TPmtterms = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID            : string = 'Id';    // Primary key
        const ERP_CODE      : string = 'ErpCode';
        const DESCRIPTION   : string = 'Description';
        const I_MONTH       : string = 'Month';
        const I_DAYS        : string = 'Days';
        const I_DAYS_NET    : string = 'DaysNet';
        const I_USING       : string = 'Using';
        const STAMP         : string = 'ExtractDateStamp';
        const KEY           : string = 'ProcessBatchKey';
        const COCODE        : string = 'Entity';
    end;

    /// <summary>
    ///     Invoice Tracker working table.
    /// </summary>

    TTracker = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID         : string = 'Id';   // Primary Key -> Foreign Key in "INVOICES"
        const USER_ALIAS : string = 'UserAlias';
        const CUID       : string = 'Cuid'; // Constraint unique
        const CO_CODE    : string = 'CoCode';
        const BRANCH     : string = 'Branch';
        const CUSTNAME   : string = 'CustomerName';
        const STAMP      : string = 'Stamp';
        const INDV_REM1  : string = 'SendReminder1';
        const INDV_REM2  : string = 'SendReminder2';
        const INDV_REM3  : string = 'SendReminder3';
        const INDV_REM4  : string = 'SendReminder4';
        const EXP_REM2   : string = 'ExceptionReminder2';
        const EXP_REM3   : string = 'ExceptionReminder3';
        const SCUID      : string = 'Sciud';
        const LAYOUT     : string = 'ReminderLayout';
        const STATEMENT  : string = 'PreStatement';
    end;

    /// <summary>
    ///     List of invoices that has been put in the reminder.
    /// </summary>

    TInvoices = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID           : string = 'Id'; // Primary Key
        const SK           : string = 'Sk'; // Foreign Key -> Primary Key in "TRACKER"
        const CUID         : string = 'Cuid';
        const INVOICENO    : string = 'InvoiceNo';
        const INVOICESTATE : string = 'InvoiceState';
        const STAMP        : string = 'Stamp';
    end;

    /// <summary>
    ///     List of layouts.
    /// </summary>

    TReminderLayouts = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID           : string = 'Id'; // Primary Key
        const LAYOUTNAME   : string = 'LayoutName';
    end;

    /// <summary>
    ///     List of users and assigned group name and id.
    /// </summary>

    TUAC = class(TDataTables)  // change model!!!
    {$TYPEINFO ON}
    public
        const ID           : string = 'Id'; // Primary Key -> Foreign Key in "GROUPS"
        const USERNAME     : string = 'UserName';   // Constraint unique
        const ACCESS_LEVEL : string = 'AccessLevel';
        const ACCESS_MODE  : string = 'AccessMode';
    end;

    /// <summary>
    ///     List of groups and assigned users.
    /// </summary>

    TGroups = class(TDataTables)  // change model!!!
    {$TYPEINFO ON}
    public
        const ID            : string = 'Id';    // Primary Key
        const GROUP_ID      : string = 'GroupId';
        const GROUP_NAME    : string = 'GroupName';
        const FID           : string = 'Fid';   // Foreign Key -> Primary Key in "UAC"
    end;

    /// <summary>
    ///     Feed from ERP. List of current FX rates.
    /// </summary>

    TFxRates = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ID            : string = 'Id';    // Primary Key
        const ISO           : string = 'Iso';
        const EXRATE        : string = 'ExRate';
        const KEY           : string = 'ProcessBatchKey';
    end;

    /// <summary>
    ///     List of all possible currencies. Static/stand-alone table.
    /// </summary>

    TCurrencies = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id           : string = 'Id';// Primary Key
        const Iso          : string = 'Iso';
        const CurrencyName : string = 'CurrencyName';
    end;

    /// <summary>
    ///     Log table for ERP transfer (via SSIS).
    /// </summary>

    TSSISMaster = class(TDataTables)
    {$TYPEINFO ON}
    public
        const ProcessBatchKey : string = 'ProcessBatchKey';
        const StartDateTime   : string = 'StartDateTime';
        const EndDateTime     : string = 'EndDateTime';
        const StatusCode      : string = 'StatusCode';
        const SystemCode      : string = 'SystemCode';
    end;

    /// <summary>
    ///     Contains all user logs from application.
    /// </summary>

    TUnityEventLogs = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id           : string = 'Id';
        const UserAlias    : string = 'UserAlias';
        const DateTimeStamp: string = 'DateTimeStamp';
        const AppEventLog  : string = 'AppEventLog';
        const AppName      : string = 'AppName';
    end;

    /// <summary>
    ///     Contains description of all control statuses use in ERP system.
    ///     TEXT field should be used to replace status code in open items list.
    /// </summary>

    TControlStatus = class(TDataTables)
    {$TYPEINFO ON}
    public
        const Id          : string = 'Id';
        const Code        : string = 'Code';
        const Text        : string = 'Text';
        const Description : string = 'Description';
    end;


implementation


end.
