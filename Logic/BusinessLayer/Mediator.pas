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
    Async.Utilities,
    IMediator;


type


    TContext = class(TInterfacedObject, IContext)
    {$TYPEINFO ON}
    strict private
        function GetAccounts():       IAccounts;
        function GetAddressBook():    IAddressBook;
        function GetComments():       IComments;
        function GetCompanies():      ICompanies;
        function GetDebtors():        IDebtors;
        function GetDocuments():      IDocuments;
        function GetGeneralTables():  IGeneralTables;
        function GetInvoiceTracker(): IInvoiceTracker;
        function GetMailer():         IMailer;
        function GetOpenItems():      IOpenItems;
        function GetUtilities():      IUtilities;
        var FAccounts:                IAccounts;
        var FAddressBook:             IAddressBook;
        var FComments:                IComments;
        var FCompanies:               ICompanies;
        var FDebtors:                 IDebtors;
        var FDocuments:               IDocuments;
        var FGeneralTables:           IGeneralTables;
        var FInvoiceTracker:          IInvoiceTracker;
        var FMailer:                  IMailer;
        var FOpenItems:               IOpenItems;
        var FUtilities:               IUtilities;
    public
        property Accounts:       IAccounts       read GetAccounts;
        property AddressBook:    IAddressBook    read GetAddressBook;
        property Comments:       IComments       read GetComments;
        property Companies:      ICompanies      read GetCompanies;
        property Debtors:        IDebtors        read GetDebtors;
        property Documents:      IDocuments      read GetDocuments;
        property GeneralTables:  IGeneralTables  read GetGeneralTables;
        property InvoiceTracker: IInvoiceTracker read GetInvoiceTracker;
        property Mailer:         IMailer         read GetMailer;
        property OpenItems:      IOpenItems      read GetOpenItems;
        property Utilities:      IUtilities      read GetUtilities;
    end;


implementation


function TMediator.GetAccounts(): IAccounts;
begin
    Result:=FAccounts;
end;


function TMediator.GetAddressBook(): IAddressBook;
begin
    Result:=FAddressBook;
end;


function TMediator.GetComments(): IComments;
begin
    Result:=FComments;
end;


function TMediator.GetCompanies(): ICompanies;
begin
    Result:=FCompanies;
end;


function TMediator.GetDebtors(): IDebtors;
begin
    Result:=FDebtors;
end;


function TMediator.GetDocuments(): IDocuments;
begin
    Result:=FDocuments;
end;


function TMediator.GetGeneralTables(): IGeneralTables;
begin
    Result:=FGeneralTables;
end;


function TMediator.GetInvoiceTracker(): IInvoiceTracker;
begin
    Result:=FInvoiceTracker;
end;


function TMediator.GetMailer(): IMailer;
begin
    Result:=FMailer;
end;


function TMediator.GetOpenItems(): IOpenItems;
begin
    Result:=FOpenItems;
end;


function TMediator.GetUtilities(): IUtilities;
begin
    Result:=FUtilities;
end;


end.
