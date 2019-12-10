unit Api.UserSessionChecked;

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


	TUserSessionChecked = class
    {$TYPEINFO ON}
	strict private
        var FIsValidated:  boolean;
        var FUserId:       integer;
        var FDepartment:   string;
        var FAliasName:    string;
        var FDisplayName:  string;
        var FEmailAddress: string;
        var FError:        TErrorHandler;
	public
        destructor Destroy(); override;
        property IsValidated:  boolean       read FIsValidated  write FIsValidated;
        property UserId:       integer       read FUserId       write FUserId;
        property Department:   string        read FDepartment   write FDepartment;
        property AliasName:    string        read FAliasName    write FAliasName;
        property DisplayName:  string        read FDisplayName  write FDisplayName;
        property EmailAddress: string        read FEmailAddress write FEmailAddress;
        property Error:        TErrorHandler read FError        write FError;
	end;


implementation


destructor TUserSessionChecked.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.

