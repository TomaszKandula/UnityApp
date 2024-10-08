unit Api.ReturnCompanies;

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
    Api.CompaniesFields;


type


    TReturnCompanies = class
    strict private
        var FCompanies:   TArray<TCompaniesFields>;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
        var FMeta:        TMetaData;
    public
        constructor Create();
        destructor Destroy(); override;
        const _IsSucceeded     = 'IsSucceeded';
        const _Error           = 'Error';
        const _Meta            = 'Meta';
        property Companies:   TArray<TCompaniesFields> read FCompanies   write FCompanies;
        property IsSucceeded: boolean                  read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler            read FError       write FError;
        property Meta:        TMetaData                read FMeta        write FMeta;
    end;


implementation


constructor TReturnCompanies.Create();
begin
    if not Assigned(Error) then Error:=TErrorHandler.Create();
    if not Assigned(Meta)  then Meta :=TMetaData.Create();
end;


destructor TReturnCompanies.Destroy();
begin

    for var Companies: TCompaniesFields in FCompanies do
        if Assigned(Companies) then Companies.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta)  then FMeta.Free();

    inherited;

end;


end.

