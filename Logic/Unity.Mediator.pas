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
        function GetSessions():      ISessions;
        function GetUsers():         IUsers;
        function GetRatings():       IRatings;
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
        property Sessions:      ISessions      read GetSessions;
        property Users:         IUsers         read GetUsers;
        property Ratings:       IRatings       read GetRatings;
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


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and extend them.
    /// </remarks>
    TMediator = class(TInterfacedObject, IMediator)
    strict private
        function GetSessions():      ISessions;
        function GetUsers():         IUsers;
        function GetRatings():       IRatings;
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
        var FSessions:               ISessions;
        var FUsers:                  IUsers;
        var FRatings:                IRatings;
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
        // A direct implementation of dependency injection at a interface level
        // allow to inject mock interfaces to the interface for test purposes
        constructor Create(
            const ASessions:      ISessions      = nil;
            const AUsers:         IUsers         = nil;
            const ARatings:       IRatings       = nil;
            const AAddressBook:   IAddressBook   = nil;
            const AComments:      IComments      = nil;
            const ACompanies:     ICompanies     = nil;
            const ADebtors:       IDebtors       = nil;
            const ADocuments:     IDocuments     = nil;
            const AGeneralTables: IGeneralTables = nil;
            const AMailer:        IMailer        = nil;
            const AOpenItems:     IOpenItems     = nil;
            const AUtilities:     IUtilities     = nil;
            const AUtility:       IUtility       = nil
        );
        destructor Destroy(); override;
        // Use this method to ensure all interfaces are created whether
        // the mock interfaces are used via consturctor or not
        procedure ForceInitialize(); virtual;
        property Sessions:       ISessions      read GetSessions;
        property Users:          IUsers         read GetUsers;
        property Ratings:        IRatings       read GetRatings;
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
    const ASessions:      ISessions      = nil;
    const AUsers:         IUsers         = nil;
    const ARatings:       IRatings       = nil;
    const AAddressBook:   IAddressBook   = nil;
    const AComments:      IComments      = nil;
    const ACompanies:     ICompanies     = nil;
    const ADebtors:       IDebtors       = nil;
    const ADocuments:     IDocuments     = nil;
    const AGeneralTables: IGeneralTables = nil;
    const AMailer:        IMailer        = nil;
    const AOpenItems:     IOpenItems     = nil;
    const AUtilities:     IUtilities     = nil;
    const AUtility:       IUtility       = nil
);
begin
    FSessions     :=ASessions;
    FUsers        :=AUsers;
    FRatings      :=ARatings;
    FAddressBook  :=AAddressBook;
    FComments     :=AComments;
    FCompanies    :=ACompanies;
    FDebtors      :=ADebtors;
    FDocuments    :=ADocuments;
    FGeneralTables:=AGeneralTables;
    FMailer       :=AMailer;
    FOpenItems    :=AOpenItems;
    FUtilities    :=AUtilities;
    FUtility      :=AUtility;
end;


destructor TMediator.Destroy();
begin
    inherited;
end;


procedure TMediator.ForceInitialize();
begin
    if not Assigned(FSessions)      then FSessions     :=TSessions.Create();
    if not Assigned(FUsers)         then FUsers        :=TUsers.Create();
    if not Assigned(FRatings)       then FRatings      :=TRatings.Create();
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


function TMediator.GetSessions(): ISessions;
begin
    Result:=FSessions;
end;


function TMediator.GetUsers(): IUsers;
begin
    Result:=FUsers;
end;


function TMediator.GetRatings(): IRatings;
begin
    Result:=FRatings;
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
