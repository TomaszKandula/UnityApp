unit Unity.Enums;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


type


    TColumns = (
        Inf7,
        Inf4,
        CoCode,
        Agent,
        Division,
        Follow,
        Group3,
        Free1,
        Free2,
        Free3,
        SalesResponsible,
        PersonResponsible,
        CustomerGroup,
        AccountType
    );

    TDataType = (
        TString,
        TInteger,
        TFloat
    );

    TDocType = (
        Reminder,
        Statement,
        Auto,
        Manual
    );

    TDocMode = (
        Custom,
        Defined
    );

    TCalendar = (
        DateToDB,
        GetDate,
        TimeOnly,
        DateOnly,
        DateTime
    );

    TActions = (
        OpenAll,
        OpenForUser,
        Insert,
        Update,
        Export,
        Import,
        Copy,
        Paste,
        Cut,
        Escape,
        Delete
    );

    TWindowState = (
        Modal,
        Modeless
    );

    TInvoiceFilter = (
        ReminderOvd,
        ReminderNonOvd,
        ShowAllItems,
        SendNotDue
    );

    TAppFiles = (
        Configuration,
        Licence
    );

    TAppTimers = (
        TurnedOn,
        TurnedOff
    );

    TAppMessage = (
        Info,      // OK button only
        Warn,      // OK button only
        Error,     // OK button only
        Question1, // OK button only
        Question2  // Yes and No buttons
    );

    TListSelection = (
        First,
        Last
    );

    TPermissions = (
        Undefined,
        Read,
        ReadWrite,
        Deny
    );

    TModules = (
        AddressBook    = 1001,
        ActionWindow   = 1002,
        InvoiceTracker = 1003,
        Free1Field     = 1004,
        Free2Field     = 1005,
        Free3Field     = 1006,
        GeneralComment = 1007,
        DailyComment   = 1008,
        Statements     = 1009,
        Reminders      = 1010,
        Calling        = 1011,
        Reporting      = 1012,
        OpenItems      = 1013,
        Unidentified   = 1014,
        Documents      = 1015
    );


implementation


end.

