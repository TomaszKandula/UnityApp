unit Api.SentDocument;

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
    Api.MetaData,
    Api.DocumentStatusFields;


type


    TSentDocument = class
    strict private
        var FSentDocuments: TArray<TDocumentStatusFields>;
        var FIsSucceeded:   boolean;
        var FError:         TErrorHandler;
        var FMeta:          TMetaData;
    public
        constructor Create(Count: cardinal = 1);
        destructor Destroy(); override;
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        const _Meta        = 'Meta';
        property SentDocuments: TArray<TDocumentStatusFields>   read FSentDocuments write FSentDocuments;
        property IsSucceeded:   boolean       read FIsSucceeded write FIsSucceeded;
        property Error:         TErrorHandler read FError       write FError;
        property Meta:          TMetaData     read FMeta        write FMeta;
    end;


implementation


constructor TSentDocument.Create(Count: cardinal = 1);
begin

    if not Assigned(FSentDocuments) then
    begin
        SetLength(FSentDocuments, Count);
        for var Index:=0 to Count - 1 do FSentDocuments[Index]:=TDocumentStatusFields.Create();
    end;

    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();

end;


destructor TSentDocument.Destroy();
begin

    for var SentDocuments: TDocumentStatusFields in FSentDocuments do
        if Assigned(SentDocuments) then SentDocuments.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.
