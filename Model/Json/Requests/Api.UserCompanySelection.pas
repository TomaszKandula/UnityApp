unit Api.UserCompanySelection;

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


	TUserCompanySelection = class
    {$TYPEINFO ON}
	strict private
        var FSelectedCoCodes: TArray<string>;
	public
        property SelectedCoCodes: TArray<string> read FSelectedCoCodes write FSelectedCoCodes;
	end;


implementation


end.

