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
    Api.ErrorHandler,
    Api.MetaData,
    Api.CustomerGroupFields;


type


    TReturnCustomerGroup = class
    strict private
        var FCustomerGroup: TArray<TCustomerGroupFields>;
        var FIsSucceeded:   boolean;
        var FError:         TErrorHandler;
        var FMeta:          TMetaData;
    public
        constructor Create(Count: cardinal = 0);
        destructor Destroy(); override;
        const _IsSucceeded      = 'IsSucceeded';
        const _Error            = 'Error';
        const _Meta             = 'Meta';
        property CustomerGroup: TArray<TCustomerGroupFields> read FCustomerGroup write FCustomerGroup;
        property IsSucceeded:   boolean       read FIsSucceeded write FIsSucceeded;
        property Error:         TErrorHandler read FError       write FError;
        property Meta:          TMetaData     read FMeta        write FMeta;
    end;


implementation


constructor TReturnCustomerGroup.Create(Count: cardinal = 0);
begin

    if Count > 0 then
    begin
        SetLength(FCustomerGroup, Count);
        for var Index:=0 to Count - 1 do FCustomerGroup[Index]:=TCustomerGroupFields.Create();
    end;

    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();

end;


destructor TReturnCustomerGroup.Destroy();
begin

    for var CustomerGroup: TCustomerGroupFields in FCustomerGroup do
        if Assigned(CustomerGroup) then CustomerGroup.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta)  then FMeta.Free();

    inherited;

end;


end.

