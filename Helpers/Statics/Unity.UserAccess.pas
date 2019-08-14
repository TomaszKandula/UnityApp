unit Unity.UserAccess;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TUserAccess = class abstract
        type TTypes = (AccessLevel, AccessMode, UserKeyId);
        const AccessFull  = 'FULL';
        const AccessBasic = 'BASIC';
        const Admin       = 'AD';
        const ReadWrite   = 'RW';
        const ReadOnly    = 'RO';
    end;


implementation


end.

