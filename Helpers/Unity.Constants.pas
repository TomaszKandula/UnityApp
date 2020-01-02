unit Unity.Constants;


interface


type


    TAdoDb = class abstract {Legacy}
        type  TFilters    = (adFilterNone, adFilterPendingRecords, adFilterAffectedRecords, adFilterFetchedRecords, adFilterConflictingRecords);
        const dbOLEDB     = 'OLEDB';
        const dbODBC      = 'ODBC';
        const ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
        const ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';
    end;


    TUnknown = class abstract {Legacy}
        const Null:       string = 'NULL';
        const Unassigned: string = 'Unassigned item.';
        const NA:         string = 'N/A';
        const NotFound:   string = 'Not found!';
    end;


    TSql = class abstract {Legacy}
        const INSERT       = ' INSERT INTO ';
        const VAL          = ' VALUES ';
        const SELECT       = ' SELECT ';
        const SELECT_DIS   = ' SELECT DISTINCT ';
        const DISTINCT     = ' DISTINCT ';
        const _UPDATE      = ' UPDATE ';
        const DELETE_FROM  = ' DELETE FROM ';
        const _SET         = ' SET ';
        const _BEGIN       = ' BEGIN ';
        const _END         = ' END ';
        const UNION        = ' UNION ALL ';
        const EQUAL        = ' = ';
        const FROM         = ' FROM ';
        const ALL          = ' * ';
        const WHERE        = ' WHERE ';
        const LEFT_JOIN    = ' LEFT JOIN ';
        const RIGHT_JOIN   = ' RIGHT JOIN ';
        const INNER_JOIN   = ' INNER JOIN ';
        const OUTER_JOIN   = ' OUTER JOIN ';
        const _ON          = ' ON ';
        const _OR          = ' OR ';
        const ORDER        = ' ORDER BY ';
        const ASC          = ' ASC ';
        const DESC         = ' DESC ';
        const _AND         = ' AND ';
        const _AS          = ' AS ';
        const MAX          = ' MAX ';
        const SUM          = ' SUM ';
        const LIKE         = ' LIKE ';
        const EXECUTE      = ' EXEC ';
        const MATCHCASE    = ' COLLATE SQL_Latin1_General_CP1_CS_AS ';
    end;


    TChars = class abstract
        const CrLf:        string = #13#10;
        const Cr:          char   = #13;
        const Lf:          char   = #10;
        const Backspace:   char   = #8;
        const Tab:         char   = #9;
        const Esc:         char   = #27;
        const Space:       char   = #32;
        const Quote:       char   = #39;
        const Comma:       char   = #44;
        const Point:       char   = #46;
        const SingleQuote: string = '''';
        const DoubleQuote: string = '''''';
    end;


    TCommon = class abstract
    public
        const SelectionColor: integer   = $00E3B268; // RGB: 68B2E3 BGR: E3B268
        const FontColor:      integer   = $006433C9; // RGB: C93364 BGR: 6433C9
        const AltColor:       integer   = $00FFDBB7; // RGB: B7DBFF BGR: FFDBB7
        const DecryptKey:     integer   = 429496;
        const AppCaption:     string    = 'Unity';
        const UnityReader:    string    = 'UnityReader.exe';
        const LicenceFile:    string    = 'Unity.lic';
        const GridImgFile:    string    = 'Unity.img';
        const ReleaseFile:    string    = 'Release.zip';
        const LayoutPak:      string    = 'Layouts.zip';
        const ManifestFile:   string    = 'Unity.manifest';
        const ConfigFile:     string    = 'Config.cfg';
    end;


    TDtFormat = class abstract
        const TimeFormat     = 'hh:mm:ss';
        const DateFormat     = 'YYYY-MM-DD';
        const DateTimeFormat = 'YYYY-MM-DD hh:mm:ss';
        const NullDate: TDateTime = 0;
    end;


    TDelimiters = class abstract
        const Semicolon = ';';
        const Comma     = ',';
        const Pipe      = '|';
    end;


    TQms = class abstract
        const Open     = 'OPEN';
        const Pending  = 'PENDING';
        const Rejected = 'REJECTED';
        const Solved   = 'SOLVED';
    end;


    TStatusBar = class abstract
        const Ready       = 'Ready';
        const Processing  = 'Processing...';
        const ExportXLS   = 'Exporting to Excel...';
        const ExportCSV   = 'Exporting to CSV file...';
        const ImportCSV   = 'Importing from CSV file...';
        const Downloading = 'Downloading Open Items...';
        const Loading     = 'Loading Aging Report...';
        const Mapping     = 'Mapping table...';
    end;


implementation


end.
