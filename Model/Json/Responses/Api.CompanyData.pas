unit Api.CompanyData;

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
    Api.BankDetails,
    Api.ErrorHandler;


type


    TCompanyData = class
    {$TYPEINFO ON}
    strict private
        var FCompanyName:    string;
        var FCompanyAddress: string;
        var FCompanyEmails:  TArray<string>;
        var FCompanyPhones:  TArray<string>;
        var FCompanyBanks:   TArray<TBankDetails>;
        var FError:          TErrorHandler;
    public
        destructor Destroy(); override;
        property CompanyName:    string               read FCompanyName    write FCompanyName;
        property CompanyAddress: string               read FCompanyAddress write FCompanyAddress;
        property CompanyEmails:  TArray<string>       read FCompanyEmails  write FCompanyEmails;
        property CompanyPhones:  TArray<string>       read FCompanyPhones  write FCompanyPhones;
        property CompanyBanks:   TArray<TBankDetails> read FCompanyBanks   write FCompanyBanks;
        property Error:          TErrorHandler        read FError          write FError;
    end;


implementation


destructor TCompanyData.Destroy();
begin
    if Assigned(FError) then FError.Free();
    for var BankDetails: TBankDetails in FCompanyBanks do BankDetails.Free();
    inherited;
end;


end.
