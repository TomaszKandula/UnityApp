unit Unity.Records;

// ---------------------------------------------------------------------
// Extension unit for application.
// Can be referenced by anyone. Cannot hold references to View or Logic.
// We use records instead of classes because we only use them to group
// specific types of data ("variable of variables") to pass where it
// needs to be passed as a parameter. It acts usually as a pay load
// for both requests and responses.
// ---------------------------------------------------------------------

interface


uses
    Unity.Enums,
    Unity.Grid,
    Unity.References,
    Unity.ListView;


// -----------------------------------
// CAUTION! Do not use PACKED records.
// -----------------------------------


type

    /// <remarks>
    /// Group of variables that carries last response that have been returned after processing
    /// given requested. Returned Code field is present for failed REST calls (status code returned).
    /// </remarks>

    TCallResponse = record
        LastMessage:  string;
        ErrorNumber:  integer;
        IsSucceeded:  boolean;
        ReturnedCode: integer;
    end;

    /// <remarks>
    /// Carries a group of variables to be updated in AddressBook table.
    /// </remarks>

    TAddressBookUpdateFields = record
        Scuid:      string;
        Phones:     string;
        Contact:    string;
        Estatement: string;
        Email:      string;
    end;

    /// <remarks>
    /// Carries a group of variables for update in DailyComments table.
    /// </remarks>

    TDailyCommentFields = record
        GroupIdSel:     string;
        AgeDateSel:     string;
        CUID:           string;
        Email:          boolean;
        CallEvent:      boolean;
        CallDuration:   integer;
        Comment:        string;
        EmailReminder:  boolean;
        EmailAutoStat:  boolean;
        EmailManuStat:  boolean;
        EventLog:       boolean;
        UpdateGrid:     boolean;
        ExtendComment:  boolean;
    end;

    /// <remarks>
    /// Carries a group of variables for update in GeneralComment table.
    /// </remarks>

    TGeneralCommentFields = record
        CUID:         string;
        FixedComment: string;
        FollowUp:     string;
        Free1:        string;
        Free2:        string;
        Free3:        string;
        EventLog:     boolean;
    end;

    /// <remarks>
    /// Carries a group of variables with information necessary to process emails with current account statement(s).
    /// </remarks>

    TAccountStatementPayLoad = record
        Layout:         TDocMode;
        Subject:        string;
        Mess:           string;
        InvFilter:      TInvoiceFilter;
        BeginDate:      string;
        EndDate:        string;
        CUID:           string;
        SendFrom:       string;
        MailTo:         string;
        CustName:       string;
        CustNumber:     string;
        LBUName:        string;
        LBUAddress:     string;
        Telephone:      string;
        BankDetails:    string;
        Series:         boolean;
        ItemNo:         integer;
        MailerList:     TListView;
        OpenItems:      TStringGrid;
        OpenItemsRefs:  TFOpenItemsRefs;
        ControlStatus:  TStringGrid;
        CtrlStatusRefs: TFCtrlStatusRefs;
        IsCtrlStatus:   boolean;
        IsUserInCopy:   boolean;
    end;

    /// <remarks>
    /// Carries a group of variables for update open items summary.
    /// </remarks>

    TOpenItemsPayLoad = record
        TotalItems:     integer;
        OverdueItems:   integer;
        NumOfInvoices:  integer;
        OsAmount:       double;
        OvdAmount:      double;
        UnallocatedAmt: double;
    end;

    /// <remarks>
    /// Carries a group of variables for open items summary with ledger currency and other currency.
    /// </remarks>

    TOpenItemsTotal = record
        OpenAm:     double;
        Am:         double;
        OpenCurAm:  double;
        CurAm:      double;
    end;


implementation


end.

