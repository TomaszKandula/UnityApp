unit Api.UserCompanyList;

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


	TUserCompanyList = class
    {$TYPEINFO ON}
	strict private
        var FCompanies:  TArray<string>;
        var FIsSelected: TArray<boolean>;
        var FError:      TErrorHandler;
	public
        destructor Destroy(); override;
        property Companies:  TArray<string>  read FCompanies  write FCompanies;
        property IsSelected: TArray<boolean> read FIsSelected write FIsSelected;
        property Error:      TErrorHandler   read FError      write FError;
	end;


implementation


destructor TUserCompanyList.Destroy();
begin
    if Assigned(FError) then FError.Free();
end;


end.
