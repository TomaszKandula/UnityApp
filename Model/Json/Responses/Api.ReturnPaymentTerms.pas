unit Api.ReturnPaymentTerms;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.ErrorHandler,
    Api.MetaData;


type


    TReturnPaymentTerms = class
    {$TYPEINFO ON}
    strict private
        var FId:               TArray<integer>;
        var FErpCode:          TArray<integer>;
        var FDescription:      TArray<string>;
        var FMonth:            TArray<integer>;
        var FDays:             TArray<integer>;
        var FDaysNet:          TArray<integer>;
        var FUsing:            TArray<integer>;
        var FExtractDateStamp: TArray<TDateTime>;
        var FProcessBatchKey:  TArray<integer>;
        var FEntity:           TArray<integer>;
        var FIsSucceeded:      boolean;
        var FError:            TErrorHandler;
        var FMeta:             TMetaData;
    public
        const _Id               = 'Id';
        const _ErpCode          = 'ErpCode';
        const _Description      = 'Description';
        const _Month            = 'Month';
        const _Days             = 'Days';
        const _DaysNet          = 'DaysNet';
        const _Using            = 'Using';
        const _IsSucceeded      = 'IsSucceeded';
        const _ExtractDateStamp = 'ExtractDateStamp';
        const _ProcessBatchKey  = 'ProcessBatchKey';
        const _Entity           = 'Entity';
        const _Error            = 'Error';
        const _Meta             = 'Meta';
        destructor Destroy(); override;
    published
        property Id:               TArray<integer>   read FId               write FId;
        property ErpCode:          TArray<integer>   read FErpCode          write FErpCode;
        property Description:      TArray<string>    read FDescription      write FDescription;
        property Month:            TArray<integer>   read FMonth            write FMonth;
        property Days:             TArray<integer>   read FDays             write FDays;
        property DaysNet:          TArray<integer>   read FDaysNet          write FDaysNet;
        property Using:            TArray<integer>   read FUsing            write FUsing;
        property ExtractDateStamp: TArray<TDateTime> read FExtractDateStamp write FExtractDateStamp;
        property ProcessBatchKey:  TArray<integer>   read FProcessBatchKey  write FProcessBatchKey;
        property Entity:           TArray<integer>   read FEntity           write FEntity;
        property IsSucceeded:      boolean           read FIsSucceeded      write FIsSucceeded;
        property Error:            TErrorHandler     read FError            write FError;
        property Meta:             TMetaData         read FMeta             write FMeta;
    end;


implementation


destructor TReturnPaymentTerms.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.

