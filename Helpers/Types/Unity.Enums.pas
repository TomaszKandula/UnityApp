unit Unity.Enums;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


type


    /// <summary>
    /// Used by sorting methods to indicate data type to be sorted.
    /// </summary>
    TDataType = (TString, TInteger, TFloat);

    /// <summary>
    /// Used to indicate document type to be sent to the customer.
    /// </summary>
    TDocType = (Reminder, Statement, Auto, Manual);

    /// <summary>
    /// Used to indicate whether the document uses default template or it is customized by the user.
    /// </summary>
    TDocMode = (Custom, Defined);

    /// <summary>
    /// Defines type of brackets in SQL generator methods.
    /// </summary>
    TBrackets = (Round, Square, Curly);

    /// <summary>
    /// Defines whether the quotes shall be used or not in SQL builder methods.
    /// </summary>
    TQuotes = (Enabled, Disabled);

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
    /// Indicates what application file type to be encoded/decoded.
    /// </summary>
    TAppFiles = (Configuration, Licence);

    /// <summary>
    /// Indicates whether the application timers are enabled or disabled.
    /// </summary>
    TAppTimers = (TurnedOn, TurnedOff);

    /// <summary>
    /// Indicates type of icon on message box and button combinations.
    /// </summary>
    TAppMessage = (Info{OK}, Warn{OK}, Error{OK}, Question1{OK}, Question2{YES_NO});

    /// <summary>
    /// Indicates wheter the list must return first or last item.
    /// </summary>
    TListSelection = (First, Last);

    /// <summary>
    /// CDOSYS authentication types.
    /// </summary>
    TAuthTypes = (cdoAnonymous, cdoBasic, cdoNTLM);{Legacy}

    /// <summary>
    /// Used to indicate authorisation scheme in CDOSYS.
    /// </summary>
    TAuthUsing = (cdoNone, cdoSendUsingPickup, cdoSendUsingPort, cdoSendUsingExchange);{Legacy}


implementation


end.

