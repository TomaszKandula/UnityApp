unit Api.PaidInfoFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TPaidInfoFields = class
    strict private
        var FId:          integer;
        var FErpCode:     string;
        var FDescription: string;
    public
        const _Id          = 'Id';
        const _ErpCode     = 'ErpCode';
        const _Description = 'Description';
        property Id:          integer read FId          write FId;
        property ErpCode:     string  read FErpCode     write FErpCode;
        property Description: string  read FDescription write FDescription;
    end;


implementation


end.
