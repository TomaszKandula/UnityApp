unit Api.DetailsFields;

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


    TDetailsFields = class
    strict private
        var FSourceDbName:  string;
        var FCompanyEmails: TArray<string>;
    public
        destructor Destroy(); override;
        const _SourceDbName  = 'SourceDbName';
        const _CompanyEmails = 'Company Emails';
        property SourceDbName:   string         read FSourceDbName  write FSourceDbName;
        property CompanyEmails:  TArray<string> read FCompanyEmails write FCompanyEmails;
    end;


implementation


destructor TDetailsFields.Destroy();
begin
    inherited;
end;


end.
