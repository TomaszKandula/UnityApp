unit Api.AddressBookEntry;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    REST.Json.Types;


type


    TAddressBookEntry = class
    {$TYPEINFO ON}
    strict private
        var FId:              integer;
        var FContactPerson:   string;
        var FRegularEmails:   string;
        var FStatementEmails: string;
        var FPhoneNumbers:    string;
    public
        const _Id              = 'Id';
        const _ContactPerson   = 'ContactPerson';
        const _RegularEmails   = 'RegularEmails';
        const _StatementEmails = 'StatementEmails';
        const _PhoneNumbers    = 'PhoneNumbers';
    published

        [JSONName('Id')]
        property Id: integer read FId write FId;

        [JSONName('ContactPerson')]
        property ContactPerson: string read FContactPerson write FContactPerson;

        [JSONName('RegularEmails')]
        property RegularEmails: string read FRegularEmails write FRegularEmails;

        [JSONName('StatementEmails')]
        property StatementEmails: string read FStatementEmails write FStatementEmails;

        [JSONName('PhoneNumbers')]
        property PhoneNumbers: string read FPhoneNumbers write FPhoneNumbers;

    end;


implementation


end.

