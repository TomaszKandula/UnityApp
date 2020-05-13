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
    Async.Utilities,
    Sync.Utility;


type


    IMediator = interface(IInterface)
    ['{6AE0D0FF-BA65-450C-9972-C226F01CBC99}']
        function GetAccounts():      IAccounts;
        function GetAddressBook():   IAddressBook;
        function GetComments():      IComments;
        function GetCompanies():     ICompanies;
        function GetDebtors():       IDebtors;
        function GetDocuments():     IDocuments;
        function GetGeneralTables(): IGeneralTables;
        function GetMailer():        IMailer;
        function GetOpenItems():     IOpenItems;
        function GetUtilities():     IUtilities;
        function GetUtility():       IUtility;
        property Accounts:      IAccounts      read GetAccounts;
        property AddressBook:   IAddressBook   read GetAddressBook;
        property Comments:      IComments      read GetComments;
        property Companies:     ICompanies     read GetCompanies;
        property Debtors:       IDebtors       read GetDebtors;
        property Documents:     IDocuments     read GetDocuments;
        property GeneralTables: IGeneralTables read GetGeneralTables;
        property Mailer:        IMailer        read GetMailer;
        property OpenItems:     IOpenItems     read GetOpenItems;
        property Utilities:     IUtilities     read GetUtilities;
        property Utility:       IUtility       read GetUtility;
        procedure ForceInitialize();
    end;


    TMediator = class(TInterfacedObject, IMediator)
    strict private
        function GetAccounts():      IAccounts;
        function GetAddressBook():   IAddressBook;
        function GetComments():      IComments;
        function GetCompanies():     ICompanies;
        function GetDebtors():       IDebtors;
        function GetDocuments():     IDocuments;
        function GetGeneralTables(): IGeneralTables;
        function GetMailer():        IMailer;
        function GetOpenItems():     IOpenItems;
        function GetUtilities():     IUtilities;
        function GetUtility():       IUtility;
        var FAccounts:               IAccounts;
        var FAddressBook:            IAddressBook;
        var FComments:               IComments;
        var FCompanies:              ICompanies;
        var FDebtors:                IDebtors;
        var FDocuments:              IDocuments;
        var FGeneralTables:          IGeneralTables;
        var FMailer:                 IMailer;
        var FOpenItems:              IOpenItems;
        var FUtilities:              IUtilities;
        var FUtility:                IUtility;
    public
        // A direct implementation of dependency injection at a class level
        // allow to inject mock interfaces to the class for test purposes
        constructor Create(
            const Accounts:      IAccounts      = nil;
            const AddressBook:   IAddressBook   = nil;
            const Comments:      IComments      = nil;
            const Companies:     ICompanies     = nil;
            const Debtors:       IDebtors       = nil;
            const Documents:     IDocuments     = nil;
            const GeneralTables: IGeneralTables = nil;
            const Mailer:        IMailer        = nil;
            const OpenItems:     IOpenItems     = nil;
            const Utilities:     IUtilities     = nil;
            const Utility:       IUtility       = nil
        );
        destructor Destroy(); override;
        // Use this method to ensure all interfaces are created regardless,
        // if mock interfaces are used via consturctor or not
        procedure ForceInitialize(); virtual;
        property Accounts:       IAccounts      read GetAccounts;
        property AddressBook:    IAddressBook   read GetAddressBook;
        property Comments:       IComments      read GetComments;
        property Companies:      ICompanies     read GetCompanies;
        property Debtors:        IDebtors       read GetDebtors;
        property Documents:      IDocuments     read GetDocuments;
        property GeneralTables:  IGeneralTables read GetGeneralTables;
        property Mailer:         IMailer        read GetMailer;
        property OpenItems:      IOpenItems     read GetOpenItems;
        property Utilities:      IUtilities     read GetUtilities;
        property Utility:        IUtility       read GetUtility;
    end;


implementation


constructor TMediator.Create(
    const Accounts:      IAccounts      = nil;
    const AddressBook:   IAddressBook   = nil;
    const Comments:      IComments      = nil;
    const Companies:     ICompanies     = nil;
    const Debtors:       IDebtors       = nil;
    const Documents:     IDocuments     = nil;
    const GeneralTables: IGeneralTables = nil;
    const Mailer:        IMailer        = nil;
    const OpenItems:     IOpenItems     = nil;
    const Utilities:     IUtilities     = nil;
    const Utility:       IUtility       = nil
);
begin
    FAccounts     :=Accounts;
    FAddressBook  :=AddressBook;
    FComments     :=Comments;
    FCompanies    :=Companies;
    FDebtors      :=Debtors;
    FDocuments    :=Documents;
    FGeneralTables:=GeneralTables;
    FMailer       :=Mailer;
    FOpenItems    :=OpenItems;
    FUtilities    :=Utilities;
    FUtility      :=Utility;
end;


destructor TMediator.Destroy();
begin
    inherited;
end;


procedure TMediator.ForceInitialize();
begin
    if not Assigned(FAccounts)      then FAccounts     :=TAccounts.Create();
    if not Assigned(FAddressBook)   then FAddressBook  :=TAddressBook.Create();
    if not Assigned(FComments)      then FComments     :=TComments.Create();
    if not Assigned(FCompanies)     then FCompanies    :=TCompanies.Create();
    if not Assigned(FDebtors)       then FDebtors      :=TDebtors.Create();
    if not Assigned(FDocuments)     then FDocuments    :=TDocuments.Create();
    if not Assigned(FGeneralTables) then FGeneralTables:=TGeneralTables.Create();
    if not Assigned(FMailer)        then FMailer       :=TMailer.Create();
    if not Assigned(FOpenItems)     then FOpenItems    :=TOpenItems.Create();
    if not Assigned(FUtilities)     then FUtilities    :=TUtilities.Create();
    if not Assigned(FUtility)       then FUtility      :=TUtility.Create();
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


function TMediator.GetUtility(): IUtility;
begin
    Result:=FUtility;
end;


end.
