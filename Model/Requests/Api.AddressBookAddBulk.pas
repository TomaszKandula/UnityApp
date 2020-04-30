unit Api.AddressBookAddBulk;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.AddressBookFields;


type


    TAddressBookAddBulk = class
	strict private
        var FUserAlias:   string;
        var FAddressBookData: TArray<TAddressBookFields>;
    public
        constructor Create(Count: cardinal);
        destructor Destroy(); override;
        const _UserAlias   = 'UserAlias';
        const _AddressBook = 'AddressBook';
        property UserAlias:  string  read FUserAlias  write FUserAlias;
        property AddressBookData: TArray<TAddressBookFields> read FAddressBookData write FAddressBookData;
    end;


implementation


constructor TAddressBookAddBulk.Create(Count: cardinal);
begin
    SetLength(FAddressBookData, Count);
    for var Index:=0 to Count - 1 do FAddressBookData[Index]:=TAddressBookFields.Create();
end;


destructor TAddressBookAddBulk.Destroy();
begin

    for var AddressBookData: TAddressBookFields in FAddressBookData do
        if Assigned(AddressBookData) then AddressBookData.Free();

    inherited;

end;


end.
