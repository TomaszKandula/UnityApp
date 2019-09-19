unit Unity.Records;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Unity.Enums,
    Unity.Grid,
    Unity.ListView;


type

    /// <remarks>
    /// Definition of last error that occured during the processing, Returned Code field is present for failed REST calls (status code returned).
    /// </remarks>

    TLastError = record
        ErrorMessage: string;
        ErrorNumber:  integer;
        IsSucceeded:  boolean;
        ReturnedCode: integer;
    end;

    /// <remarks>
    ///
    /// </remarks>

    TAddressBookUpdateFields = record
        Scuid:      string;
        Phones:     string;
        Contact:    string;
        Estatement: string;
        Email:      string;
    end;

    /// <remarks>
    ///
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
    ///
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
    ///
    /// </remarks>

    TSendAccountStatementFields = record
        Layout:      TDocMode;
        Subject:     string;
        Mess:        string;
        InvFilter:   TInvoiceFilter;
        BeginDate:   string;
        EndDate:     string;
        OpenItems:   TStringGrid;
        CUID:        string;
        SendFrom:    string;
        MailTo:      string;
        CustName:    string;
        CustNumber:  string;
        LBUName:     string;
        LBUAddress:  string;
        Telephone:   string;
        BankDetails: string;
        Series:      boolean;
        ItemNo:      integer;
        MailerList:  TListView;
    end;

    /// <remarks>
    /// Holds open items total amounts for ledger currency and other currency.
    /// </remarks>

    TOpenItemsTotal = record
        OpenAm:     double;
        Am:         double;
        OpenCurAm:  double;
        CurAm:      double;
    end;

    /// <remarks>
    /// This record definition holds column numbers for given column name. This is necessary as column order may change.
    /// Normally we would use "ReturnColumn" extension method, but in case of multithreading, we must pre-set them before many threads
    /// use it at the same time (VCL components are not thread safe). Having record with fields simplify things.
    /// </remarks>

    TFOpenItemsRefs = record
        Ad1Col:       integer;
        Ad2Col:       integer;
        Ad3Col:       integer;
        PnoCol:       integer;
        PAreaCol:     integer;
        CuidCol:      integer;
        OpenAmCol:    integer;
        PmtStatCol:   integer;
        CtrlCol:      integer;
        InvoNoCol:    integer;
        ValDtCol:     integer;
        DueDtCol:     integer;
        ISOCol:       integer;
        CurAmCol:     integer;
        OpenCurAmCol: integer;
        Text:         integer;
    end;

    /// <remarks>
    /// This record defines column numbers for given field, so we do not have to call "ReturnColumn" method each time.
    /// </remarks>

    TFControlStatusRefs = record
        Id:          integer;
        Code:        integer;
        Text:        integer;
        Description: integer;
    end;


implementation


end.

