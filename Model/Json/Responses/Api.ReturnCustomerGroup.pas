unit Api.ReturnCustomerGroup;

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
    Api.ErrorHandler;


type


    TReturnCustomerGroup = class
    {$TYPEINFO ON}
    strict private
        var FId:               TArray<integer>;
        var FSourceDbName:     TArray<string>;
        var FErpCode:          TArray<string>;
        var FDescription:      TArray<string>;
        var FExtractDateStamp: TArray<TDateTime>;
        var FProcessBatchKey:  TArray<integer>;
        var FIsSucceeded:      boolean;
        var FError:            TErrorHandler;
    public
        const _Id               = 'Id';
        const _SourceDbName     = 'SourceDbName';
        const _ErpCode          = 'ErpCode';
        const _Description      = 'Description';
        const _ExtractDateStamp = 'ExtractDateStamp';
        const _ProcessBatchKey  = 'ProcessBatchKey';
        const _IsSucceeded      = 'IsSucceeded';
        const _Error            = 'Error';
        destructor Destroy(); override;
    published
        property Id:               TArray<integer>   read FId               write FId;
        property SourceDbName:     TArray<string>    read FSourceDbName     write FSourceDbName;
        property ErpCode:          TArray<string>    read FErpCode          write FErpCode;
        property Description:      TArray<string>    read FDescription      write FDescription;
        property ExtractDateStamp: TArray<TDateTime> read FExtractDateStamp write FExtractDateStamp;
        property ProcessBatchKey:  TArray<integer>   read FProcessBatchKey  write FProcessBatchKey;
        property IsSucceeded:      boolean           read FIsSucceeded      write FIsSucceeded;
        property Error:            TErrorHandler     read FError            write FError;
    end;


implementation


destructor TReturnCustomerGroup.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.

