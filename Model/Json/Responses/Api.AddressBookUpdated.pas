unit Api.AddressBookUpdated;

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


	TAddressBookUpdated = class
    {$TYPEINFO ON}
	strict private
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
        var FMeta:        TMetaData;
	public
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        const _Meta        = 'Meta';
        destructor Destroy(); override;
    published
        property IsSucceeded: boolean read FIsSucceeded write FIsSucceeded;
        property Error: TErrorHandler read FError       write FError;
        property Meta:      TMetaData read FMeta        write FMeta;
	end;


implementation


destructor TAddressBookUpdated.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.

