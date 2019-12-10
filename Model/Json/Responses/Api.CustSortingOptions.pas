unit Api.CustSortingOptions;

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
    Rest.Json,
    Api.ErrorHandler;


type


	TCustSortingOptions = class
    {$TYPEINFO ON}
	strict private
        var FSortingOptions: TArray<string>;
        var FError: TErrorHandler;
	public
        destructor Destroy(); override;
        property SortingOptions: TArray<string> read FSortingOptions write FSortingOptions;
        property Error: TErrorHandler read FError write FError;
	end;


implementation


destructor TCustSortingOptions.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.
