unit Api.RegisteredEmails;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TRegisteredEmails = class
    strict private
        var FCompanyCode:  integer;
        var FCompanyEmail: string;
    public
        const _CompanyCode  = 'CompanyCode';
        const _CompanyEmail = 'CompanyEmail';
        property CompanyCode:  integer read FCompanyCode  write FCompanyCode;
        property CompanyEmail: string  read FCompanyEmail write FCompanyEmail;
    end;


implementation


end.

