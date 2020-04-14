unit Unity.Constants;


interface


type


    TConfigSections = class abstract
        // Use Section.Key schema
        const PasswordSection    = 'PASSWORD';
        const ApplicationDetails = 'APPLICATION';
        const RiskClassDetails   = 'RISK_CLASS_DETAILS';
        const InvoiceTypes       = 'INVOICE_TYPES';
        const Unallocated        = 'UNALLOCATED_DEFINITION';
        const Layouts            = 'EMAIL_LAYOUTS';
        const TimersSettings     = 'TIMERS_INTERVALS';
        const FollowUpColors     = 'FOLLOWUPS_COLORS';
        const AgingRanges        = 'AGEVIEW_BUCKETS';
        const UserFeedback       = 'USER_FEEDBACK';
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
        const AppCaption:     string    = 'Unity Platform';
        const UnityReader:    string    = 'UnityReader.exe';
        const LicenceFile:    string    = 'Unity.lic';
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
        const MakeReport  = 'Generating aging report...';
        const WritingXLS  = 'Writing to Excel...';
        const ExportXLS   = 'Exporting to Excel...';
        const ExportCSV   = 'Exporting to CSV file...';
        const ImportCSV   = 'Importing from CSV file...';
        const Downloading = 'Downloading Open Items...';
        const Loading     = 'Loading Aging Report...';
        const Mapping     = 'Mapping table...';
    end;


implementation


end.

