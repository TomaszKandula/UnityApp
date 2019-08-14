unit Unity.Unknown;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TUnknown = class abstract
        const Null:       string = 'NULL';
        const Unassigned: string = 'Unassigned item.';
        const NA:         string = 'N/A';
        const NotFound:   string = 'Not found!';
    end;


implementation


end.

