unit Mediator;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Async.Accounts,
    Async.AddressBook,
    Async.Comments,
    Async.Companies,
    Async.Debtors,
    Async.Documents,
    Async.GeneralTables,
    Async.InvoiceTracker,
    Async.Mailer,
    Async.OpenItems,
    //Async.Queries,
    Async.Utilities;


type


    TMediator = class(TObject)
    {$TYPEINFO ON}
    strict private
        var FAccounts:       IAccounts;
        var FAddressBook:    IAddressBook;
        var FComments:       IComments;
        var FCompanies:      ICompanies;
        var FDebtors:        IDebtors;
        var FDocuments:      IDocuments;
        var FGeneralTables:  IGeneralTables;
        var FInvoiceTracker: IInvoiceTracker;
        var FMailer:         IMailer;
        var FOpenItems:      IOpenItems;
        //var FQueries:        IQueries;
        var FUtilities:      IUtilities;
    public
        property Accounts:       IAccounts       read FAccounts;
        property AddressBook:    IAddressBook    read FAddressBook;
        property Comments:       IComments       read FComments;
        property Companies:      ICompanies      read FCompanies;
        property Debtors:        IDebtors        read FDebtors;
        property Documents:      IDocuments      read FDocuments;
        property GeneralTables:  IGeneralTables  read FGeneralTables;
        property InvoiceTracker: IInvoiceTracker read FInvoiceTracker;
        property Mailer:         IMailer         read FMailer;
        property OpenItems:      IOpenItems      read FOpenItems;
        //property Queries:        IQueries        read FQueries;
        property Utilities:      IUtilities      read FUtilities;
    end;


implementation


end.
