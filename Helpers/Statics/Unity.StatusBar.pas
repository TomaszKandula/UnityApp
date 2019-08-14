unit Unity.StatusBar;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TStatusBar = class abstract
        const Ready       = 'Ready';
        const Processing  = 'Processing...';
        const ExportXLS   = 'Exporting to Excel...';
        const ExportCSV   = 'Exporting to CSV file...';
        const ImportCSV   = 'Importing from CSV file...';
        const Generating  = 'Generating age view...';
        const Downloading = 'Downloading Open Items...';
        const Loading     = 'Loading Aging Report...';
        const SQLupdate   = 'Sending to SQL Server...';
    end;


implementation


end.

