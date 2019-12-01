unit Api.CheckSessionResponse;

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


	TCheckSessionResponse = class
	strict private
        var FIsValidated:  boolean;
        var FUserId:       integer;
        var FDepartment:   string;
        var FAliasName:    string;
        var FDisplayName:  string;
        var FEmailAddress: string;
        var FError:        TErrorHandler;
	public
        property IsValidated:  boolean       read FIsValidated  write FIsValidated;
        property UserId:       integer       read FUserId       write FUserId;
        property Department:   string        read FDepartment   write FDepartment;
        property AliasName:    string        read FAliasName    write FAliasName;
        property DisplayName:  string        read FDisplayName  write FDisplayName;
        property EmailAddress: string        read FEmailAddress write FEmailAddress;
        property Error:        TErrorHandler read FError        write FError;
	end;

	
implementation


end.

