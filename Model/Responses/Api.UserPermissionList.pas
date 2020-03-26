unit Api.UserPermissionList;

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
    Api.UserPermissions;


type


    TUserPermissionList = class
    strict private
        var FUserPermissions: TArray<TUserPermissions>;
        var FIsSucceeded:     boolean;
        var FError:           TErrorHandler;
        var FMeta:            TMetaData;
    public
        destructor Destroy(); override;
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        const _Meta        = 'Meta';
        property UserPermissions: TArray<TUserPermissions> read FUserPermissions write FUserPermissions;
        property IsSucceeded:     boolean                  read FIsSucceeded     write FIsSucceeded;
        property Error:           TErrorHandler            read FError           write FError;
        property Meta:            TMetaData                read FMeta            write FMeta;
    end;


implementation


destructor TUserPermissionList.Destroy();
begin
    for var UserPermissions: TUserPermissions in FUserPermissions do
        if Assigned(UserPermissions) then UserPermissions.Free();
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.
