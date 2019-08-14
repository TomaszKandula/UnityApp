unit Unity.LyncLib;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TLyncLib = class abstract
        const LyncControls  = 'Microsoft.Lync.Controls.dll';
        const LyncFramework = 'Microsoft.Lync.Controls.Framework.dll';
        const LyncModel     = 'Microsoft.Lync.Model.dll';
        const LyncUtils     = 'Microsoft.Lync.Utilities.dll';
        const OfficeUc      = 'Microsoft.Office.Uc.dll';
        const LyncCall      = 'LyncCall.exe';
    end;


implementation


end.

