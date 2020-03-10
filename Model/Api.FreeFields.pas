unit Api.FreeFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TFreeFields = class
    strict private
        var FSourceDbNames:   string;
        var FCustomerNumbers: Int64;
        var FFree1:           string;
        var FFree2:           string;
        var FFree3:           string;
    public
        const _SourceDbNames   = 'SourceDbNames';
        const _CustomerNumbers = 'Customer Numbers';
        const _Free1           = 'Free 1';
        const _Free2           = 'Free 2';
        const _Free3           = 'Free 3';
        property SourceDbNames:   string read FSourceDbNames   write FSourceDbNames;
        property CustomerNumbers: Int64  read FCustomerNumbers write FCustomerNumbers;
        property Free1:           string read FFree1           write FFree1;
        property Free2:           string read FFree2           write FFree2;
        property Free3:           string read FFree3           write FFree3;
    end;


implementation


end.
