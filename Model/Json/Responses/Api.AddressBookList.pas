unit Api.AddressBookList;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.ErrorHandler,
    Api.MetaData;


type


    TAddressBookList = class
    {$TYPEINFO ON}
    strict private
        var FId:              TArray<integer>;
        var FSourceDbName:    TArray<string>;
        var FCustomerNumber:  TArray<Int64>;
        var FCustomerName:    TArray<string>;
        var FContactPerson:   TArray<string>;
        var FRegularEmails:   TArray<string>;
        var FStatementEmails: TArray<string>;
        var FPhoneNumbers:    TArray<string>;
        var FIsSucceeded:     boolean;
        var FError:           TErrorHandler;
        var FMeta:            TMetaData;
    public
        const _Id              = 'Id';
        const _SourceDbName    = 'SourceDbName';
        const _CustomerNumber  = 'CustomerNumber';
        const _CustomerName    = 'CustomerName';
        const _ContactPerson   = 'ContactPerson';
        const _RegularEmails   = 'RegularEmails';
        const _StatementEmails = 'StatementEmails';
        const _PhoneNumbers    = 'PhoneNumbers';
        const _IsSucceeded     = 'IsSucceeded';
        const _Error           = 'Error';
        const _Meta            = 'Meta';
        destructor Destroy(); override;
    published
        property Id:              TArray<integer> read FId              write FId;
        property SourceDbName:    TArray<string>  read FSourceDbName    write FSourceDbName;
        property CustomerNumber:  TArray<Int64>   read FCustomerNumber  write FCustomerNumber;
        property CustomerName:    TArray<string>  read FCustomerName    write FCustomerName;
        property ContactPerson:   TArray<string>  read FContactPerson   write FContactPerson;
        property RegularEmails:   TArray<string>  read FRegularEmails   write FRegularEmails;
        property StatementEmails: TArray<string>  read FStatementEmails write FStatementEmails;
        property PhoneNumbers:    TArray<string>  read FPhoneNumbers    write FPhoneNumbers;
        property IsSucceeded:     boolean         read FIsSucceeded     write FIsSucceeded;
        property Error:           TErrorHandler   read FError           write FError;
        property Meta:            TMetaData       read FMeta            write FMeta;
    end;


implementation


destructor TAddressBookList.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.
