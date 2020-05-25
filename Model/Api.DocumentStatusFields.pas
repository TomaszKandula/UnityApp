unit Api.DocumentStatusFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TDocumentStatusFields = class
    strict private
        var FCustomerNumber: Int64;
        var FSourceDbName:   string;
        var FIsSucceeded:    boolean;
    public
        const _CustomerNumber = 'Customer Number';
        const _SourceDbName   = 'SourceDbName';
        const _IsSucceeded    = 'Is Succeeded';
        property CustomerNumber: Int64   read FCustomerNumber write FCustomerNumber;
        property SourceDbName:   string  read FSourceDbName   write FSourceDbName;
        property IsSucceeded:    boolean read FIsSucceeded    write FIsSucceeded;
    end;


implementation


end.
