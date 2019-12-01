unit Api.PostCheckSession;

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


	TPostCheckSession = class
	strict private
        var FSessionToken: string;
	public
        property SessionToken: string read FSessionToken write FSessionToken;
	end;


implementation


end.

