unit Api.ReturnCustomerReport;

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


    TReturnCustomerReport = class
    strict private
        var FAgeDate:           string;
        var FSnapshotDt:        string;
        var FCustomerSnapshot:  TArray<TCustomerSnapshot>;
        var FIsSucceeded:       boolean;
        var FError:             TErrorHandler;
        var FMeta:              TMetaData;
    public
        destructor Destroy(); override;
        const _AgeDate          = 'Age Date';
        const _SnapshotDt       = 'Snapshot Date';
        const _IsSucceeded      = 'IsSucceeded';
        const _Error            = 'Error';
        const _Meta             = 'Meta';
        property AgeDate:          string                    read FAgeDate          write FAgeDate;
        property SnapshotDt:       string                    read FSnapshotDt       write FSnapshotDt;
        property CustomerSnapshot: TArray<TCustomerSnapshot> read FCustomerSnapshot write FCustomerSnapshot;
        property IsSucceeded:      boolean                   read FIsSucceeded      write FIsSucceeded;
        property Error:            TErrorHandler             read FError            write FError;
        property Meta:             TMetaData                 read FMeta             write FMeta;
    end;


implementation


destructor TReturnCustomerReport.Destroy();
begin
    for var CustomerSnapshot: TCustomerSnapshot in FCustomerSnapshot do
        if Assigned(CustomerSnapshot) then CustomerSnapshot.Free();
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.
