unit Unity.Enums;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TDocType       = (Reminder, Statement, Auto, Manual);
    TDocMode       = (Custom, Defined);
    TBrackets      = (Round, Square, Curly);
    TQuotes        = (Enabled, Disabled);
    TLoading       = (NullParameter, CallOpenItems, CallMakeAge);
    TCalendar      = (DateToDB, GetDate, TimeOnly, DateOnly, DateTime);
    TActions       = (OpenAll, OpenForUser, Insert, Update, Export, Import, Copy, Paste, Cut, Escape, Delete);
    TWindowState   = (Modal, Modeless);
    TInvoiceFilter = (ReminderOvd, ReminderNonOvd, ShowAllItems);
    TAuthTypes     = (cdoAnonymous, cdoBasic, cdoNTLM);
    TAuthUsing     = (cdoNone, cdoSendUsingPickup, cdoSendUsingPort, cdoSendUsingExchange);
    TAppFiles      = (Configuration, Licence);
    TAppTimers     = (TurnedOn, TurnedOff);
    TAppMessage    = (Info, Warn, Error, Question1, Question2);


implementation


end.

