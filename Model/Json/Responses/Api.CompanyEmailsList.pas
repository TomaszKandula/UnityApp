unit Api.CompanyEmailsList;


interface


uses
    Generics.Collections,
    Rest.Json,
    Api.Errorhandler,
    Api.RegisteredEmails;


type


    TCompanyEmailsList = class
    {$TYPEINFO ON}
    strict private
        var FEmailList:   TArray<TRegisteredEmails>;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
    public
        destructor Destroy(); override;
        property EmailList:   TArray<TRegisteredEmails> read FEmailList   write FEmailList;
        property IsSucceeded: boolean                   read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler             read FError       write FError;
    end;


implementation


destructor TCompanyEmailsList.Destroy();
begin
    if Assigned(FError) then FError.Free();
    for var RegisteredEmails: TRegisteredEmails in FEmailList do RegisteredEmails.Free();
    inherited;
end;


end.
