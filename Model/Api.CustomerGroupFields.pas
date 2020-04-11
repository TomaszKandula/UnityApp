unit Api.CustomerGroupFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TCustomerGroupFields = class
    strict private
        var FId:               integer;
        var FSourceDbName:     string;
        var FErpCode:          string;
        var FDescription:      string;
        var FExtractDateStamp: TDateTime;
        var FProcessBatchKey:  integer;
    public
        const _Id               = 'Id';
        const _SourceDbName     = 'SourceDbName';
        const _ErpCode          = 'ErpCode';
        const _Description      = 'Description';
        const _ExtractDateStamp = 'ExtractDateStamp';
        const _ProcessBatchKey  = 'ProcessBatchKey';
        property Id:               integer   read FId               write FId;
        property SourceDbName:     string    read FSourceDbName     write FSourceDbName;
        property ErpCode:          string    read FErpCode          write FErpCode;
        property Description:      string    read FDescription      write FDescription;
        property ExtractDateStamp: TDateTime read FExtractDateStamp write FExtractDateStamp;
        property ProcessBatchKey:  integer   read FProcessBatchKey  write FProcessBatchKey;
    end;


implementation


end.
