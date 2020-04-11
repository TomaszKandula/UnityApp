unit Api.ReturnCompanyData;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.BankDetails,
    Api.ErrorHandler,
    Api.MetaData;


type


    TReturnCompanyData = class
    strict private
        var FCompanyName:    string;
        var FCompanyAddress: string;
        var FCompanyEmails:  TArray<string>;
        var FCompanyPhones:  TArray<string>;
        var FExclusions:     TArray<integer>;
        var FCompanyBanks:   TArray<TBankDetails>;
        var FIsSucceeded:    boolean;
        var FError:          TErrorHandler;
        var FMeta:           TMetaData;
    public
        destructor Destroy(); override;
        const _CompanyName    = 'CompanyName';
        const _CompanyAddress = 'CompanyAddress';
        const _CompanyEmails  = 'CompanyEmails';
        const _CompanyPhones  = 'CompanyPhones';
        const _Exclusions     = 'Exclusions';
        const _CompanyBanks   = 'CompanyBanks';
        const _IsSucceeded    = 'IsSucceeded';
        const _Error          = 'Error';
        const _Meta           = 'Meta';
        property CompanyName:    string               read FCompanyName    write FCompanyName;
        property CompanyAddress: string               read FCompanyAddress write FCompanyAddress;
        property CompanyEmails:  TArray<string>       read FCompanyEmails  write FCompanyEmails;
        property CompanyPhones:  TArray<string>       read FCompanyPhones  write FCompanyPhones;
        property Exclusions:     TArray<integer>      read FExclusions     write FExclusions;
        property CompanyBanks:   TArray<TBankDetails> read FCompanyBanks   write FCompanyBanks;
        property IsSucceeded:    boolean              read FIsSucceeded    write FIsSucceeded;
        property Error:          TErrorHandler        read FError          write FError;
        property Meta:           TMetaData            read FMeta           write FMeta;
    end;


implementation


destructor TReturnCompanyData.Destroy();
begin

    for var BankDetails: TBankDetails in FCompanyBanks do
        if Assigned(BankDetails) then BankDetails.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.
