unit Api.UserCompaniesUpdated;

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


	TUserCompaniesUpdated = class
    {$TYPEINFO ON}
	strict private
        var FIsSucceeded: boolean;
        var FError: TErrorHandler;
	public
        destructor Destroy(); override;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
	end;


implementation


destructor TUserCompaniesUpdated.Destroy();
begin
    if Assigned(FError) then FError.Free();
end;


end.
