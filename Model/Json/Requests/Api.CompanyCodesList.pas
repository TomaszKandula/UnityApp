unit Api.CompanyCodesList;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TCompanyCodesList = class
    {$TYPEINFO ON}
    strict private
        var FSourceDBName: TArray<string>;
    public
        const _SourceDBName = 'SourceDBName';
    published
        property SourceDBName: TArray<string> read FSourceDBName write FSourceDBName;
    end;


implementation


end.

