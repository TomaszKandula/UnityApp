unit Helpers;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    Vcl.Grids,
    Statics;


type

    // ---------------------------------------------------------------------------------------------------------------------------------------- ENUMERATIONS //

    /// <summary>
    /// Defines different statement options.
    /// </summary>

    TInvoiceFilter = (ReminderOvd, ReminderNonOvd, ShowAllItems);

    // -------------------------------------------------------------------------------------------------------------------------------------------- POINTERS //

    /// <remarks>
    /// Pointer reference for packed record.
    /// </remarks>

    PTokenUser = ^TTokenUser;

    // ---------------------------------------------------------------------------------------------------------------------------------------------- ARRAYS //

    /// <remarks>
    /// Reference to two dimensional string array.
    /// </remarks>

    TALists = array of array of string;

    /// <remarks>
    /// Reference to one dimensional string array.
    /// </remarks>

    TAStrings = array of string;

    /// <remarks>
    /// Reference to one dimensional integer array.
    /// </remarks>

    TAIntigers = array of integer;

    /// <remarks>
    /// Reference to one dimensional double array.
    /// </remarks>

    TADoubles = array of double;

    // --------------------------------------------------------------------------------------------------------------------------------------------- RECORDS //

    /// <remarks>
    /// Packed record for user SID in Windows environment.
    /// </remarks>

    TTokenUser = packed record
        User: TSidAndAttributes;
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
        Layout:      TEnums.TDocMode;
        Subject:     string;
        Mess:        string;
        InvFilter:   TInvoiceFilter;
        BeginDate:   string;
        EndDate:     string;
        //OpenItems:   InterposerClasses.TStringGrid;
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
    end;

    /// <remarks>
    ///
    /// </remarks>

    type TOpenItemsTotal = record
        OpenAm:     double;
        Am:         double;
        OpenCurAm:  double;
        CurAm:      double;
    end;

    /// <remarks>
    /// This record definition allows to hold column numbers for given column name. This is necessary as column order may change.
    /// Normally we would use "ReturnColumn" extension method, but in case of multithreading, we must pre-set them before many threads
    /// use it at the same time (VCL components are not thread safe). Having record with fields simplify things.
    /// </remarks>

    TOpenItemsRefs = record
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
    /// This record define column numbers for given field, so we do not have to use each time "ReturnColumn".
    /// </remarks>

    TControlStatusRefs = record
        Id:          integer;
        Code:        integer;
        Text:        integer;
        Description: integer;
    end;


implementation


end.

