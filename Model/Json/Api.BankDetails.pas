unit Api.BankDetails;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TBankDetails = class
    {$TYPEINFO ON}
    strict private
        var FBankName: string;
        var FBankAcc:  string;
        var FBankCode: string;
        var FBankIso:  string;
    public
        const _BankName = 'BankName';
        const _BankAcc  = 'BankAcc';
        const _BankCode = 'BankCode';
        const _BankIso  = 'BankIso';
    published
        property BankName: string read FBankName write FBankName;
        property BankAcc:  string read FBankAcc  write FBankAcc;
        property BankCode: string read FBankCode write FBankCode;
        property BankIso:  string read FBankIso  write FBankIso;
    end;


implementation


end.

