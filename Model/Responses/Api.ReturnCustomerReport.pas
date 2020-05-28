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
    Api.CustomerSnapshotEx;


type


    TReturnCustomerReport = class
    strict private
        var FCustomerSnapshotEx: TArray<TCustomerSnapshotEx>;
        var FAgeDate:            string;
        var FSnapshotDt:         string;
        var FIsSucceeded:        boolean;
        var FError:              TErrorHandler;
        var FMeta:               TMetaData;
    public
        destructor Destroy(); override;
        const _AgeDate     = 'Age Date';
        const _SnapshotDt  = 'Snapshot Date';
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        const _Meta        = 'Meta';
        property CustomerSnapshotEx: TArray<TCustomerSnapshotEx> read FCustomerSnapshotEx write FCustomerSnapshotEx;
        property AgeDate:            string                      read FAgeDate            write FAgeDate;
        property SnapshotDt:         string                      read FSnapshotDt         write FSnapshotDt;
        property IsSucceeded:        boolean                     read FIsSucceeded        write FIsSucceeded;
        property Error:              TErrorHandler               read FError              write FError;
        property Meta:               TMetaData                   read FMeta               write FMeta;
    end;


implementation


destructor TReturnCustomerReport.Destroy();
begin

    for var CustomerSnapshotEx: TCustomerSnapshotEx in FCustomerSnapshotEx do
        if Assigned(CustomerSnapshotEx) then CustomerSnapshotEx.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.
