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
        const SelectionColor: integer   = $00E3B268; // rgb D7E4F2 => bgr F2E4D7
        const FontColor:      integer   = $006433C9; // rgb C93364 => bgr 6433C9
        const AltColor:       integer   = $00FFDBB7; // rgb B7DBFF => bgr FFDBB7
        const DecryptKey:     integer   = 429496;
        const AppCaption:     string    = 'Unity';
        const UnityReader:    string    = 'UnityReader.exe';
        const LicenceFile:    string    = 'Unity.lic';
        const GridImgFile:    string    = 'Unity.img';
        const ReleaseFile:    string    = 'Release.zip';
        const LayoutPak:      string    = 'Layouts.zip';
        const ManifestFile:   string    = 'Unity.manifest';
        const CurrentMutex:   PWideChar = 'UNITY_10255';
        const ConfigFile:     string    = 'Config.cfg';
    end;


implementation


end.

