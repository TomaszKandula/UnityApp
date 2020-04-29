unit Api.AddressBookFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TAddressBookFields = class
    strict private
        var FId:              integer;
        var FSourceDbName:    string;
        var FCustomerNumber:  Int64;
        var FCustomerName:    string;
        var FContactPerson:   string;
        var FRegularEmails:   string;
        var FStatementEmails: string;
        var FPhoneNumbers:    string;
    public
        const _Id              = 'Id';
        const _SourceDbName    = 'SourceDbName';
        const _CustomerNumber  = 'Customer Number';
        const _CustomerName    = 'Customer Name';
        const _ContactPerson   = 'Contact Person';
        const _RegularEmails   = 'Regular Emails';
        const _StatementEmails = 'Statement Emails';
        const _PhoneNumbers    = 'Phone Numbers';
        property Id:              integer read FId              write FId;
        property SourceDbName:    string  read FSourceDbName    write FSourceDbName;
        property CustomerNumber:  Int64   read FCustomerNumber  write FCustomerNumber;
        property CustomerName:    string  read FCustomerName    write FCustomerName;
        property ContactPerson:   string  read FContactPerson   write FContactPerson;
        property RegularEmails:   string  read FRegularEmails   write FRegularEmails;
        property StatementEmails: string  read FStatementEmails write FStatementEmails;
        property PhoneNumbers:    string  read FPhoneNumbers    write FPhoneNumbers;
    end;


implementation


end.
