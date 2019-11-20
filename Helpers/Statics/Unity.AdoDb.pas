unit Unity.AdoDb;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface

// legacy - to be removed!


type


    TAdoDb = class abstract
        type  TFilters    = (adFilterNone, adFilterPendingRecords, adFilterAffectedRecords, adFilterFetchedRecords, adFilterConflictingRecords);
        const dbOLEDB     = 'OLEDB';
        const dbODBC      = 'ODBC';
        const ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
        const ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';
    end;


implementation


end.

