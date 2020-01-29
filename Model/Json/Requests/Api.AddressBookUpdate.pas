unit Api.AddressBookUpdate;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type


    TAddressBookUpdate = class
    {$TYPEINFO ON}
    strict private
        var FId:              TArray<integer>;
        var FContactPerson:   TArray<string>;
        var FRegularEmails:   TArray<string>;
        var FStatementEmails: TArray<string>;
        var FPhoneNumbers:    TArray<string>;
    public
        const _Id              = 'Id';
        const _ContactPerson   = 'ContactPerson';
        const _RegularEmails   = 'RegularEmails';
        const _StatementEmails = 'StatementEmails';
        const _PhoneNumbers    = 'PhoneNumbers';
//        procedure InitializeFields(Count: integer);
    published
        property Id:              TArray<integer> read FId              write FId;
        property ContactPerson:   TArray<string>  read FContactPerson   write FContactPerson;
        property RegularEmails:   TArray<string>  read FRegularEmails   write FRegularEmails;
        property StatementEmails: TArray<string>  read FStatementEmails write FStatementEmails;
        property PhoneNumbers:    TArray<string>  read FPhoneNumbers    write FPhoneNumbers;
    end;


implementation


//procedure TAddressBookUpdate.InitializeFields(Count: integer);
//begin
//    SetLength(FId, Count);
//    SetLength(FContactPerson, Count);
//    SetLength(FRegularEmails, Count);
//    SetLength(FStatementEmails, Count);
//    SetLength(FPhoneNumbers, Count);
//end;


end.

