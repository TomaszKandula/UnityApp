unit Api.CompanyDetails;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.BankDetails;


type


    TCompanyDetails = class
    strict private
        var FSourceDbName:   string;
        var FCompanyName:    string;
        var FCompanyAddress: string;
        var FCompanyEmails:  TArray<string>;
        var FCompanyPhones:  TArray<string>;
        var FExclusions:     TArray<integer>;
        var FCompanyBanks:   TArray<TBankDetails>;
    public
        destructor Destroy(); override;
        const _SourceDbName    = 'SourceDbName';
        const _CompanyName     = 'CompanyName';
        const _CompanyAddress  = 'CompanyAddress';
        const _CompanyEmails   = 'CompanyEmails';
        const _CompanyPhones   = 'CompanyPhones';
        const _Exclusions      = 'Exclusions';
        const _CompanyBanks    = 'CompanyBanks';
        property SourceDbName:   string               read FSourceDbName   write FSourceDbName;
        property CompanyName:    string               read FCompanyName    write FCompanyName;
        property CompanyAddress: string               read FCompanyAddress write FCompanyAddress;
        property CompanyEmails:  TArray<string>       read FCompanyEmails  write FCompanyEmails;
        property CompanyPhones:  TArray<string>       read FCompanyPhones  write FCompanyPhones;
        property Exclusions:     TArray<integer>      read FExclusions     write FExclusions;
        property CompanyBanks:   TArray<TBankDetails> read FCompanyBanks   write FCompanyBanks;
    end;


implementation


destructor TCompanyDetails.Destroy();
begin

    for var CompanyBanks: TBankDetails in FCompanyBanks do
        if Assigned(CompanyBanks) then CompanyBanks.Free();

    inherited;

end;


end.
