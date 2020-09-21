unit Api.ReturnOpenItems;

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
    Api.OpenItemsFields;


type


	TReturnOpenItems = class
	strict private
        var FOpenItems:   TArray<TOpenItemsFields>;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
        var FMeta:        TMetaData;
	public
        constructor Create(Count: cardinal);
        destructor Destroy(); override;
        const _IsSucceeded       = 'IsSucceeded';
        const _Error             = 'Error';
        const _Meta              = 'Meta';
        property OpenItems:   TArray<TOpenItemsFields> read FOpenItems write FOpenItems;
        property IsSucceeded: boolean         read FIsSucceeded       write FIsSucceeded;
        property Error:       TErrorHandler   read FError             write FError;
        property Meta:        TMetaData       read FMeta              write FMeta;
	end;


implementation


constructor TReturnOpenItems.Create(Count: cardinal);
begin

    if not Assigned(FOpenItems) then
    begin
        SetLength(FOpenItems, Count);
        for var Index:=0 to Count - 1 do FOpenItems[Index]:=TOpenItemsFields.Create();
    end;

    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();

end;


destructor TReturnOpenItems.Destroy();
begin

    for var OpenItems: TOpenItemsFields in FOpenItems do
        if Assigned(OpenItems) then OpenItems.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.

