unit Api.ReturnCompanies;

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


    TReturnCompanies = class
    {$TYPEINFO ON}
    strict private
        var FSourceDbName:   TArray<string>;
        var FCompanyCode:    TArray<integer>;
        var FCompanyName:    TArray<string>;
        var FCompanyAddress: TArray<string>;
        var FCompanyVat:     TArray<string>;
        var FCompanyDuns:    TArray<integer>;
        var FCompanyRate:    TArray<double>;
        var FKpiOvdTarget:   TArray<double>;
        var FKpiUnallTarget: TArray<double>;
        var FStatBeforeRem:  TArray<boolean>;
        var FLbuType:        TArray<string>;
        var FCurrency:       TArray<string>;
        var FCity:           TArray<string>;
        var FCountry:        TArray<string>;
        var FIsSucceeded:    boolean;
        var FError:          TErrorHandler;
    public
        const _SourceDbName    = 'SourceDbName';
        const _CompanyCode     = 'CompanyCode';
        const _CompanyName     = 'CompanyName';
        const _CompanyAddress  = 'CompanyAddress';
        const _CompanyVat      = 'CompanyVat';
        const _CompanyDuns     = 'CompanyDuns';
        const _CompanyRate     = 'CompanyRate';
        const _KpiOvdTarget    = 'KpiOvdTarget';
        const _KpiUnallTarget  = 'KpiUnallTarget';
        const _StatBeforeRem   = 'StatBeforeRem';
        const _LbuType         = 'LbuType';
        const _Currency        = 'Currency';
        const _City            = 'City';
        const _Country         = 'Country';
        const _IsSucceeded     = 'IsSucceeded';
        const _Error           = 'Error';
        destructor Destroy(); override;
    published
        property SourceDbName:   TArray<string>  read FSourceDbName   write FSourceDbName;
        property CompanyCode:    TArray<integer> read FCompanyCode    write FCompanyCode;
        property CompanyName:    TArray<string>  read FCompanyName    write FCompanyName;
        property CompanyAddress: TArray<string>  read FCompanyAddress write FCompanyAddress;
        property CompanyVat:     TArray<string>  read FCompanyVat     write FCompanyVat;
        property CompanyDuns:    TArray<integer> read FCompanyDuns    write FCompanyDuns;
        property CompanyRate:    TArray<double>  read FCompanyRate    write FCompanyRate;
        property KpiOvdTarget:   TArray<double>  read FKpiOvdTarget   write FKpiOvdTarget;
        property KpiUnallTarget: TArray<double>  read FKpiUnallTarget write FKpiUnallTarget;
        property StatBeforeRem:  TArray<boolean> read FStatBeforeRem  write FStatBeforeRem;
        property LbuType:        TArray<string>  read FLbuType        write FLbuType;
        property Currency:       TArray<string>  read FCurrency       write FCurrency;
        property City:           TArray<string>  read FCity           write FCity;
        property Country:        TArray<string>  read FCountry        write FCountry;
        property IsSucceeded:    boolean         read FIsSucceeded    write FIsSucceeded;
        property Error:          TErrorHandler   read FError          write FError;
    end;


implementation


destructor TReturnCompanies.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.

