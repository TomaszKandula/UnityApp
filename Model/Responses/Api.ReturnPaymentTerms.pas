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
    Api.MetaData,
    Api.PaymentTermsFields;


type


    TReturnPaymentTerms = class
    strict private
        var FPaymentTerms: TArray<TPaymentTermsFields>;
        var FIsSucceeded:  boolean;
        var FError:        TErrorHandler;
        var FMeta:         TMetaData;
    public
        destructor Destroy(); override;
        const _IsSucceeded      = 'IsSucceeded';
        const _Error            = 'Error';
        const _Meta             = 'Meta';
        property PaymentTerms: TArray<TPaymentTermsFields> read FPaymentTerms write FPaymentTerms;
        property IsSucceeded:      boolean           read FIsSucceeded      write FIsSucceeded;
        property Error:            TErrorHandler     read FError            write FError;
        property Meta:             TMetaData         read FMeta             write FMeta;
    end;


implementation


destructor TReturnPaymentTerms.Destroy();
begin

    for var PaymentTerms: TPaymentTermsFields in FPaymentTerms do
        if Assigned(PaymentTerms) then PaymentTerms.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.

