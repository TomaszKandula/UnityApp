unit Api.ReturnCustomerSnapshots;

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
    Api.CustomerSnapshot;


type


    TReturnCustomerSnapshots = class
    strict private
        var FCustomerSnapshot: TArray<TCustomerSnapshot>;
        var FAgeDate:          string;
        var FIsSucceeded:      boolean;
        var FError:            TErrorHandler;
        var FMeta:             TMetaData;
    public
        constructor Create(Count: cardinal = 1);
        destructor Destroy(); override;
        const _AgeDate           = 'Age Date';
        const _IsSucceeded       = 'IsSucceeded';
        const _Error             = 'Error';
        const _Meta              = 'Meta';
        property CustomerSnapshot: TArray<TCustomerSnapshot> read FCustomerSnapshot write FCustomerSnapshot;
        property AgeDate:          string                    read FAgeDate          write FAgeDate;
        property IsSucceeded:      boolean                   read FIsSucceeded      write FIsSucceeded;
        property Error:            TErrorHandler             read FError            write FError;
        property Meta:             TMetaData                 read FMeta             write FMeta;
    end;


implementation


constructor TReturnCustomerSnapshots.Create(Count: cardinal = 1);
begin

    if not Assigned(FCustomerSnapshot) then
    begin
        SetLength(FCustomerSnapshot, Count);
        for var Index:=0 to Count - 1 do FCustomerSnapshot[Index]:=TCustomerSnapshot.Create();
    end;

    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();

end;


destructor TReturnCustomerSnapshots.Destroy();
begin

    for var CustomerSnapshot: TCustomerSnapshot in FCustomerSnapshot do
        if Assigned(CustomerSnapshot) then CustomerSnapshot.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.

