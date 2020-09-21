unit Api.AddressBookItem;

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


    TAddressBookItem = class
    strict private
        var FId:              integer;
        var FSourceDbName:    string;
        var FCustomerNumber:  Int64;
        var FCustomerName:    string;
        var FContactPerson:   string;
        var FRegularEmails:   string;
        var FStatementEmails: string;
        var FPhoneNumbers:    string;
        var FIsSucceeded:     boolean;
        var FError:           TErrorHandler;
        var FMeta:            TMetaData;
    public
        constructor Create();
        destructor Destroy(); override;
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
        const _FMeta           = 'Meta';
        property Id:              integer       read FId              write FId;
        property SourceDbName:    string        read FSourceDbName    write FSourceDbName;
        property CustomerNumber:  Int64         read FCustomerNumber  write FCustomerNumber;
        property CustomerName:    string        read FCustomerName    write FCustomerName;
        property ContactPerson:   string        read FContactPerson   write FContactPerson;
        property RegularEmails:   string        read FRegularEmails   write FRegularEmails;
        property StatementEmails: string        read FStatementEmails write FStatementEmails;
        property PhoneNumbers:    string        read FPhoneNumbers    write FPhoneNumbers;
        property IsSucceeded:     boolean       read FIsSucceeded     write FIsSucceeded;
        property Error:           TErrorHandler read FError           write FError;
        property Meta:            TMetaData     read FMeta            write FMeta;
    end;



implementation


constructor TAddressBookItem.Create();
begin
    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();
end;


destructor TAddressBookItem.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.
