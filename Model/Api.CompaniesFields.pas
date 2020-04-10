unit Api.CompaniesFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TCompaniesFields = class
    strict private
        var FSourceDbName:   string;
        var FCompanyCode:    integer;
        var FCompanyName:    string;
        var FCompanyAddress: string;
        var FCompanyVat:     string;
        var FCompanyDuns:    integer;
        var FCompanyRate:    double;
        var FKpiOvdTarget:   double;
        var FKpiUnallTarget: double;
        var FStatBeforeRem:  boolean;
        var FLbuType:        string;
        var FCurrency:       string;
        var FCity:           string;
        var FCountry:        string;
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
        property SourceDbName:   string  read FSourceDbName   write FSourceDbName;
        property CompanyCode:    integer read FCompanyCode    write FCompanyCode;
        property CompanyName:    string  read FCompanyName    write FCompanyName;
        property CompanyAddress: string  read FCompanyAddress write FCompanyAddress;
        property CompanyVat:     string  read FCompanyVat     write FCompanyVat;
        property CompanyDuns:    integer read FCompanyDuns    write FCompanyDuns;
        property CompanyRate:    double  read FCompanyRate    write FCompanyRate;
        property KpiOvdTarget:   double  read FKpiOvdTarget   write FKpiOvdTarget;
        property KpiUnallTarget: double  read FKpiUnallTarget write FKpiUnallTarget;
        property StatBeforeRem:  boolean read FStatBeforeRem  write FStatBeforeRem;
        property LbuType:        string  read FLbuType        write FLbuType;
        property Currency:       string  read FCurrency       write FCurrency;
        property City:           string  read FCity           write FCity;
        property Country:        string  read FCountry        write FCountry;
    end;


implementation


end.
