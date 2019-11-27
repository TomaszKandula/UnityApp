unit Unity.Common;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


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


implementation


end.

