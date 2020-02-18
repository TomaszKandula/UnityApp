unit Unity.Enums;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


type


    /// <summary>
    /// Indicates age view sorting column.
    /// </summary>
    TColumns = (Inf7, Inf4, CoCode, Agent, Division, Follow, Group3, Free1, Free2, Free3, SalesResponsible, PersonResponsible, CustomerGroup, AccountType);
    /// <summary>
    /// Defines data type to be sorted.
    /// </summary>
    TDataType = (TString, TInteger, TFloat);
    /// <summary>
    /// Indicates document type to be sent to the customer.
    /// </summary>
    TDocType = (Reminder, Statement, Auto, Manual);
    /// <summary>
    /// Indicates whether the document uses default template or it is customized by the user.
    /// </summary>
    TDocMode = (Custom, Defined);
    /// <summary>
    /// Defines action type to be executed after user chooses the date from calendar.
    /// </summary>
    TCalendar = (DateToDB, GetDate, TimeOnly, DateOnly, DateTime);
    /// <summary>
    /// Defines action type.
    /// </summary>
    TActions = (OpenAll, OpenForUser, Insert, Update, Export, Import, Copy, Paste, Cut, Escape, Delete);
    /// <summary>
    /// Defines whether window is modal or modeless.
    /// </summary>
    TWindowState = (Modal, Modeless);
    /// <summary>
    /// Defines what types of invoices will be shown on the statement(s).
    /// </summary>
    TInvoiceFilter = (ReminderOvd, ReminderNonOvd, ShowAllItems);
    /// <summary>
    /// Application file type for encoding/decoding.
    /// </summary>
    TAppFiles = (Configuration, Licence);
    /// <summary>
    /// Indicates whether the application timers are enabled or disabled.
    /// </summary>
    TAppTimers = (TurnedOn, TurnedOff);
    /// <summary>
    /// States type of the icon on message box and button combinations.
    /// </summary>
    TAppMessage = (Info{OK}, Warn{OK}, Error{OK}, Question1{OK}, Question2{YES_NO});
    /// <summary>
    /// Indicates wheter the list must return first or last item.
    /// </summary>
    TListSelection = (First, Last);


implementation


end.

