unit Unity.Mediator;

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
    Async.Mailer,
    Async.OpenItems,
    Async.Utilities;


type


    IMediator = interface;
    TMediator = class;


    IMediator = interface(IInterface)
    ['{6AE0D0FF-BA65-450C-9972-C226F01CBC99}']
        function GetAccounts():       IAccounts;
        function GetAddressBook():    IAddressBook;
        function GetComments():       IComments;
        function GetCompanies():      ICompanies;
        function GetDebtors():        IDebtors;
        function GetDocuments():      IDocuments;
        function GetGeneralTables():  IGeneralTables;
        function GetMailer():         IMailer;
        function GetOpenItems():      IOpenItems;
        function GetUtilities():      IUtilities;
        property Accounts:       IAccounts       read GetAccounts;
        property AddressBook:    IAddressBook    read GetAddressBook;
        property Comments:       IComments       read GetComments;
        property Companies:      ICompanies      read GetCompanies;
        property Debtors:        IDebtors        read GetDebtors;
        property Documents:      IDocuments      read GetDocuments;
        property GeneralTables:  IGeneralTables  read GetGeneralTables;
        property Mailer:         IMailer         read GetMailer;
        property OpenItems:      IOpenItems      read GetOpenItems;
        property Utilities:      IUtilities      read GetUtilities;
    end;


    TMediator = class(TInterfacedObject, IMediator)
    {$TYPEINFO ON}
    strict private
        function GetAccounts():       IAccounts;
        function GetAddressBook():    IAddressBook;
        function GetComments():       IComments;
        function GetCompanies():      ICompanies;
        function GetDebtors():        IDebtors;
        function GetDocuments():      IDocuments;
        function GetGeneralTables():  IGeneralTables;
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
        var FMailer:                  IMailer;
        var FOpenItems:               IOpenItems;
        var FUtilities:               IUtilities;
    public
        constructor Create();
        destructor Destroy(); override;
        property Accounts:       IAccounts       read GetAccounts;
        property AddressBook:    IAddressBook    read GetAddressBook;
        property Comments:       IComments       read GetComments;
        property Companies:      ICompanies      read GetCompanies;
        property Debtors:        IDebtors        read GetDebtors;
        property Documents:      IDocuments      read GetDocuments;
        property GeneralTables:  IGeneralTables  read GetGeneralTables;
        property Mailer:         IMailer         read GetMailer;
        property OpenItems:      IOpenItems      read GetOpenItems;
        property Utilities:      IUtilities      read GetUtilities;
    end;


implementation


constructor TMediator.Create();
begin
    FAccounts      :=TAccounts.Create();
    FAddressBook   :=TAddressBook.Create();
    FComments      :=TComments.Create();
    FCompanies     :=TCompanies.Create();
    FDebtors       :=TDebtors.Create();
    FDocuments     :=TDocuments.Create();
    FGeneralTables :=TGeneralTables.Create();
    FMailer        :=TMailer.Create();
    FOpenItems     :=TOpenItems.Create();
    FUtilities     :=TUtilities.Create();
end;


destructor TMediator.Destroy();
begin
    {Empty}
    inherited;
end;


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
