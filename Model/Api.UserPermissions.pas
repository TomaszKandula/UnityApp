unit Api.UserPermissions;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TUserPermissions = class
    strict private
        var FModuleId:      integer;
        var FPermissionId:  integer;
    public
        const _ModuleId     = 'ModuleId';
        const _PermissionId = 'PermissionId';
        property ModuleId:     integer read FModuleId     write FModuleId;
        property PermissionId: integer read FPermissionId write FPermissionId;
    end;


implementation


end.
