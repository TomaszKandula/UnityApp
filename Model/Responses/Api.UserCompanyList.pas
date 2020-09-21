unit Api.UserCompanyList;

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
    Api.UserCompanyFields;


type


	TUserCompanyList = class
	strict private
        var FUserCompanies: TArray<TUserCompanyFields>;
        var FIsSucceeded:   boolean;
        var FError:         TErrorHandler;
        var FMeta:          TMetaData;
	public
        constructor Create(Count: cardinal = 1);
        destructor Destroy(); override;
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        const _Meta        = 'Meta';
        property UserCompanies: TArray<TUserCompanyFields> read FUserCompanies write FUserCompanies;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
        property Meta:        TMetaData     read FMeta        write FMeta;
	end;


implementation


constructor TUserCompanyList.Create(Count: cardinal = 1);
begin

    if not Assigned(FUserCompanies) then
    begin
        SetLength(FUserCompanies, Count);
        for var Index:=0 to Count - 1 do FUserCompanies[Index]:=TUserCompanyFields.Create();
    end;

    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();

end;


destructor TUserCompanyList.Destroy();
begin

    for var UserCompanies: TUserCompanyFields in FUserCompanies do
        if Assigned(UserCompanies) then UserCompanies.Free();

    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();

    inherited;

end;


end.
