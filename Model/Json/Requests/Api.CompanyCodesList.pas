unit Api.CompanyCodesList;

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
    Rest.Json;


type


    TCompanyCodesList = class
    {$TYPEINFO ON}
    strict private
        var FCompanyCodes: TArray<integer>;
    public
        const _CompanyCodes = 'CompanyCodes';
    published
        property CompanyCodes: TArray<integer> read FCompanyCodes write FCompanyCodes;
    end;


implementation


end.

