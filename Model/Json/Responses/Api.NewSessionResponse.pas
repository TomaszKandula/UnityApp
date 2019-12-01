unit Api.NewSessionResponse;

// -------------------------------------------------
// JSON model for REST. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// Cannot have any implementation other than fields.
// -------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json,
    Api.ErrorHandler;
	
type


	TNewSessionResponse = class
	strict private
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
	public
        property IsSucceeded: boolean read FIsSucceeded write FIsSucceeded;
        property Error: TErrorHandler read FError       write FError;
	end;

	
implementation


end.

