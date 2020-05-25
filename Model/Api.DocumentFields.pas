unit Api.DocumentFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TDocumentFields = class
    strict private
        var FSendFrom:       string;
        var FEmailTo:        string;
        var FSourceDbName:   string;
        var FCustomerNumber: Int64;
    public
        const _SendFrom        = 'Send From';
        const _EmailTo         = 'Email To';
        const _SourceDbName    = 'SourceDbName';
        const _CustomerNumber  = 'Customer Number';
        property SendFrom:       string  read FSendFrom       write FSendFrom;
        property EmailTo:        string  read FEmailTo        write FEmailTo;
        property SourceDbName:   string  read FSourceDbName   write FSourceDbName;
        property CustomerNumber: Int64   read FCustomerNumber write FCustomerNumber;
    end;


implementation


end.
