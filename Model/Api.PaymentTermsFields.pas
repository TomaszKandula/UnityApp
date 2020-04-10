unit Api.PaymentTermsFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TPaymentTermsFields = class
    strict private
        var FId:               integer;
        var FErpCode:          integer;
        var FDescription:      string;
        var FMonth:            integer;
        var FDays:             integer;
        var FDaysNet:          integer;
        var FUsing:            integer;
        var FExtractDateStamp: TDateTime;
        var FProcessBatchKey:  integer;
        var FEntity:           integer;
    public
        const _Id               = 'Id';
        const _ErpCode          = 'ErpCode';
        const _Description      = 'Description';
        const _Month            = 'Month';
        const _Days             = 'Days';
        const _DaysNet          = 'DaysNet';
        const _Using            = 'Using';
        const _ExtractDateStamp = 'ExtractDateStamp';
        const _ProcessBatchKey  = 'ProcessBatchKey';
        const _Entity           = 'Entity';
        property Id:               integer   read FId               write FId;
        property ErpCode:          integer   read FErpCode          write FErpCode;
        property Description:      string    read FDescription      write FDescription;
        property Month:            integer   read FMonth            write FMonth;
        property Days:             integer   read FDays             write FDays;
        property DaysNet:          integer   read FDaysNet          write FDaysNet;
        property Using:            integer   read FUsing            write FUsing;
        property ExtractDateStamp: TDateTime read FExtractDateStamp write FExtractDateStamp;
        property ProcessBatchKey:  integer   read FProcessBatchKey  write FProcessBatchKey;
        property Entity:           integer   read FEntity           write FEntity;
    end;


implementation


end.
