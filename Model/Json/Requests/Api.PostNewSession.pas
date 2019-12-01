unit Api.PostNewSession;

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


	TPostNewSession = class
	strict private
        var FSessionToken: string;
        var FAliasName:    string;
	public
        property SessionToken: string read FSessionToken write FSessionToken;
        property AliasName:    string read FAliasName    write FAliasName;
	end;

	
implementation


end.
