unit Api.UserSessionAdd;

// -------------------------------------------------
// JSON model for REST. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// Cannot have any implementation other than fields.
// -------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;

	
type


	TUserSessionAdd = class
	strict private
        var FAliasName: string;
	public
        property AliasName: string read FAliasName write FAliasName;
	end;

	
implementation


end.
