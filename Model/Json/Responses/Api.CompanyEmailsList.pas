unit Api.CompanyEmailsList;

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
        const _EmailList   = 'EmailList';
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        destructor Destroy(); override;
    published
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
