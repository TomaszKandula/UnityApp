unit Api.UserSessionAdd;

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


	TUserSessionAdd = class
    {$TYPEINFO ON}
	strict private
        var FAliasName: string;
	public
        property AliasName: string read FAliasName write FAliasName;
	end;

	
implementation


end.
