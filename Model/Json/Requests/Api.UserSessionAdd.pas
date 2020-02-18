unit Api.UserSessionAdd;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface

	
type


	TUserSessionAdd = class
	strict private
        var FAliasName: string;
	public
        const _AliasName = 'AliasName';
        property AliasName: string read FAliasName write FAliasName;
	end;

	
implementation


end.
