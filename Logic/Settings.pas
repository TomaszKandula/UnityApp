
{$I .\Include\Header.inc}

unit Settings;


interface


uses
    Main,
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellAPI,
    System.StrUtils,
    System.SysUtils,
    System.Classes,
    System.INIFiles,
    Vcl.Forms,
    Vcl.Graphics,
    CRC32u;


type


    ISettings = Interface(IInterface)
    ['{FF5CBEC3-2576-4E1C-954E-C892AB4A7CC1}']
        // Exposed methods
        function  Encode(ConfigType: integer): boolean;
        function  Decode(ConfigType: integer; ToMemory: boolean): boolean;
        function  ConfigToMemory: boolean;
        function  GetLicenceValue(Section: string; Key: string): string;
        function  GetStringValue(Section: string; Key: string; Default: string): string;
        procedure SetStringValue(Section: string; Key: string; Value: string);
        function  GetIntegerValue(Section: string; Key: string; Default: integer): integer;
        procedure SetIntegerValue(Section: string; Key: string; Value: integer);
        procedure GetSectionValues(Section: string; var Values: TStringList);
        procedure GetSection(Section: string; var Keys: TStringList);
        procedure GetSections(List: TStringList);
        procedure DeleteSection(SectionName: string);
        procedure DeleteKey(Section: string; Ident: string);
        function  FindSettingsKey(Section: string; KeyPosition: integer): string;
        function  GetLastError     : integer;
        function  GetAppDir        : string;
        function  GetAppLog        : string;
        function  GetLayoutDir     : string;
        function  GetPackageDir    : string;
        function  GetWinUserName   : string;
        function  GetWinTempFolder : string;
        function  GetPathGridImage : string;
        function  GetPathEventLog  : string;
        function  GetPathAppCfg    : string;
        function  GetPathLicenceLic: string;
        function  GetReleasePakURL : string;
        function  GetReleaseManURL : string;
        function  GetLayoutsURL    : string;
        // Property's methods (invisible under interface)
        function  GetReleaseDateTime: TDateTime;
        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        function  GetReleaseNumber: cardinal;
        procedure SetReleaseNumber(NewRelease: cardinal);
        function  GetLayoutLists: TStringList;
        function  GetTodayFColor  : TColor;
        function  GetTodayBColor  : TColor;
        function  GetPastFColor   : TColor;
        function  GetPastBColor   : TColor;
        function  GetFutureFColor : TColor;
        function  GetFutureBColor : TColor;
        procedure SetTodayFColor (NewColor: TColor);
        procedure SetTodayBColor (NewColor: TColor);
        procedure SetPastFColor  (NewColor: TColor);
        procedure SetPastBColor  (NewColor: TColor);
        procedure SetFutureFColor(NewColor: TColor);
        procedure SetFutureBColor(NewColor: TColor);
        // Properties
        property  LayoutLists    : TStringList read GetLayoutLists;
        property  ReleaseNumber  : cardinal    read GetReleaseNumber   write SetReleaseNumber;
        property  ReleaseDateTime: TDateTime   read GetReleaseDateTime write SetReleaseDateTime;
        property  TodayFColor    : TColor      read GetTodayFColor     write SetTodayFColor;
        property  TodayBColor    : TColor      read GetTodayBColor     write SetTodayBColor;
        property  PastFColor     : TColor      read GetPastFColor      write SetPastFColor;
        property  PastBColor     : TColor      read GetPastBColor      write SetPastBColor;
        property  FutureFColor   : TColor      read GetFutureFColor    write SetFutureFColor;
        property  FutureBColor   : TColor      read GetFutureBColor    write SetFutureBColor;
    end;

    /// <summary>
    /// Class constructor is responsibe for setting up variables with file paths and for the initialization of two separate
    /// classes that keeps settings during program runtime.
    /// </summary>

    TSettings = class(TInterfacedObject, ISettings)
    {$TYPEINFO ON}
    private
        // TMemoryIni holding encoded settings
        var TMIG: TMemIniFile;
        var TMIL: TMemIniFile;
        var List: TStringList;
        var pGetLastError:  integer;
        var pWinUserName:   string;
        var pPathGridImage: string;
        // Directories
        var pAppLog:         string;
        var pPathEventLog:   string;
        var pPathAppCfg:     string;
        var pPathLicenceLic: string;
        // Files
        var pAppDir:        string;
        var pDirLayouts:    string;
        var pDirPackage:    string;
        var pWinTempFolder: string;
        // URL
        var pReleasePakURL: string;
        var pReleaseManURL: string;
        var pGetLayoutsURL: string;
        // Others
        function  GetReleaseDateTime: TDateTime;
        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        function  GetReleaseNumber: cardinal;
        procedure SetReleaseNumber(NewRelease: cardinal);
        function  GetLayoutLists: TStringList;
        // Getters and setters for "follow-up" colors saved in settings file
        function  GetTodayFColor  : TColor;
        function  GetTodayBColor  : TColor;
        function  GetPastFColor   : TColor;
        function  GetPastBColor   : TColor;
        function  GetFutureFColor : TColor;
        function  GetFutureBColor : TColor;
        procedure SetTodayFColor (NewColor: TColor);
        procedure SetTodayBColor (NewColor: TColor);
        procedure SetPastFColor  (NewColor: TColor);
        procedure SetPastBColor  (NewColor: TColor);
        procedure SetFutureFColor(NewColor: TColor);
        procedure SetFutureBColor(NewColor: TColor);
    public
        property FWinUserName:     string    read pWinUserName;
        property FAppLog:          string    read pAppLog;
        property FPathGridImage:   string    read pPathGridImage;
        property FPathEventLog:    string    read pPathEventLog;
        property FPathAppCfg:      string    read pPathAppCfg;
        property FPathLicenceLic:  string    read pPathLicenceLic;
        property FReleasePakURL:   string    read pReleasePakURL;
        property FReleaseManURL:   string    read pReleaseManURL;
        property FGetLayoutsURL:   string    read pGetLayoutsURL;
        property FAppDir:          string    read pAppDir;
        property FDirLayouts:      string    read pDirLayouts;
        property FDirPackage:      string    read pDirPackage;
        property FWinTempFolder:   string    read pWinTempFolder;
        property FReleaseDateTime: TDateTime read GetReleaseDateTime write SetReleaseDateTime;
        property TodayFColor:      TColor    read GetTodayFColor     write SetTodayFColor;
        property TodayBColor:      TColor    read GetTodayBColor     write SetTodayBColor;
        property PastFColor:       TColor    read GetPastFColor      write SetPastFColor;
        property PastBColor:       TColor    read GetPastBColor      write SetPastBColor;
        property FutureFColor:     TColor    read GetFutureFColor    write SetFutureFColor;
        property FutureBColor:     TColor    read GetFutureBColor    write SetFutureBColor;
        constructor Create;
        destructor  Destroy; override;
        // Methods
        function  Encode(ConfigType: integer): boolean;
        function  Decode(ConfigType: integer; ToMemory: boolean): boolean;
        function  ConfigToMemory: boolean;
        function  GetLicenceValue(Section: string; Key: string): string;
        function  GetStringValue(Section: string; Key: string; Default: string): string;
        procedure SetStringValue(Section: string; Key: string; Value: string);
        function  GetIntegerValue(Section: string; Key: string; Default: integer): integer;
        procedure SetIntegerValue(Section: string; Key: string; Value: integer);
        procedure GetSectionValues(Section: string; var Values: TStringList);
        procedure GetSection(Section: string; var Keys: TStringList);
        procedure GetSections(List: TStringList);
        procedure DeleteSection(SectionName: string);
        procedure DeleteKey(Section: string; Ident: string);
        function  FindSettingsKey(Section: string; KeyPosition: integer): string;
        function  GetLastError: integer;
        function  GetAppDir:         string;
        function  GetLayoutDir:      string;
        function  GetPackageDir:     string;
        function  GetAppLog:         string;
        function  GetWinUserName:    string;
        function  GetWinTempFolder:  string;
        function  GetPathGridImage:  string;
        function  GetPathEventLog:   string;
        function  GetPathAppCfg:     string;
        function  GetPathLicenceLic: string;
        function  GetReleasePakURL:  string;
        function  GetReleaseManURL:  string;
        function  GetLayoutsURL:     string;
    end;


implementation


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TSettings.Create;
begin

    /// <remarks>
    /// General settings (config.cfg), containing all the necessary data for the application.
    /// </remarks>

    TMIG:=TMemIniFile.Create('');

    /// <remarks>
    /// Licence details (unity.licx).
    /// </remarks>

    TMIL:=TMemIniFile.Create('');

    /// <summary>
    /// Holds list of all registered layouts.
    /// </summary>

    List:=TStringList.Create;

    // User Alias
    pWinUserName   :=Trim(LowerCase(GetEnvironmentVariable('username')));

    // Directories
    pAppDir        :=ExtractFileDir(Application.ExeName) + '\';
    pDirLayouts    :=pAppDir + 'layouts\';
    pDirPackage    :=pAppDir + 'package\';
    pWinTempFolder :=GetEnvironmentVariable('TEMP');

    // Files
    pAppLog        :=pWinUserName + '.log';
    pPathAppCfg    :=pAppDir + ConfigFile;
    pPathLicenceLic:=pAppDir + LicenceFile;
    pPathEventLog  :=pAppDir + pAppLog;
    pPathGridImage :=pAppDir + GridImgFile;

    /// <remarks>
    /// Return 404 error code if configuration file cannot be found.
    /// </remarks>

    if FileExists(pPathAppCfg) then
        ConfigToMemory
            else
                pGetLastError:=404;

end;


destructor TSettings.Destroy;
begin
    TMIG.Free;
    TMIL.Free;
    List.Free;
    inherited;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- CONFIGURATION //


/// <summary>
/// Push config content to the memory.
/// </summary>

function TSettings.ConfigToMemory: boolean;
var
    iCNT: cardinal;
begin
    Result:=False;

    if Assigned(TMIG) then
    begin
        Decode(AppConfig, True);

        pReleasePakURL:=TMIG.ReadString(ApplicationDetails, 'UPDATE_PATH', '') + ReleaseFile;
        pReleaseManURL:=TMIG.ReadString(ApplicationDetails, 'UPDATE_PATH', '') + ManifestFile;

        pGetLayoutsURL:=TMIG.ReadString(ApplicationDetails, 'LAYOUT_PATH', '');

        GetSectionValues(Layouts, List);
        for iCNT:=0 to List.Count - 1 do
        begin
            List.Strings[iCNT]:=MidStr(List.Strings[iCNT], AnsiPos('=', List.Strings[iCNT]) + 1, 255);
        end;

        pGetLastError:=0;
        Result:=True;
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------- GET RELEASE STATUS CODE //


/// <summary>
/// Get release number from release manifest hosted at:
/// https://unityinfo.azurewebsites.net/release/package/unity.manifest
/// </summary>

function TSettings.GetReleaseNumber: cardinal;
begin
    Result:=0;

    if Assigned(TMIG) then
    begin
        Result:=StrToIntDef(TMIG.ReadString(ApplicationDetails, 'RELEASE_NUMBER', ''), 0);
    end;
end;


/// <summary>
/// Set release number in settings file.
/// </summary>

procedure TSettings.SetReleaseNumber(NewRelease: cardinal);
begin
    if Assigned(TMIG) then
    begin
        TMIG.WriteInteger(ApplicationDetails, 'RELEASE_NUMBER', NewRelease);
        Encode(AppConfig);
    end;
end;


/// <summary>
/// Pass layout list from settings file to a usable variable.
/// </summary>

function TSettings.GetLayoutLists: TStringList;
begin
    Result:=List;
end;


/// <summary>
/// Get update time and date registered in setting file.
/// </summary>

function TSettings.GetReleaseDateTime: TDateTime;
begin
    Result:=NULLDATE;

    if Assigned(TMIG) then
    begin
        Result:=StrToDateTimeDef(TMIG.ReadString(ApplicationDetails, 'UPDATE_DATETIME', ''), NULLDATE);
    end;

end;


/// <summary>
/// Set new update time and date.
/// </summary>

procedure TSettings.SetReleaseDateTime(NewDateTime: TDateTime);
begin
    if Assigned(TMIG) then
    begin
        TMIG.WriteString(ApplicationDetails, 'UPDATE_DATETIME', DateTimeToStr(NewDateTime));
        Encode(AppConfig);
    end;
end;


// --------------------------------------------------------------------------------------------------------------------------------------- ENCRYPT & DECRYPT //


/// <summary>
/// Encoding method based on XOR and SHR with secret KEY.
/// </summary>

function TSettings.Encode(ConfigType: integer): boolean;
var
    iCNT:        integer;
    wStream:     TMemoryStream;
    rStream:     TMemoryStream;
    hStream:     TStringList;
    buffer:      int64;
    vCRC:        dWord;
    sCRC:        string;
begin

    // Do not allow to encode licence file
    if ConfigType = LicData then
    begin
        Result:=False;
        Exit;
    end;

    rStream:=TMemoryStream.Create;
    wStream:=TMemoryStream.Create;
    hStream:=TStringList.Create;

    try
        try

            // Move file to memory
            if ConfigType = AppConfig then
                TMIG.GetStrings(hStream);

            hStream.SaveToStream(rStream);
            rStream.Position:=0;

            // Compute CRC32 checksum (between $00000000 and $FFFFFFF)
            ComputeCRC32(rStream.Memory, rStream.Size, vCRC);

            /// <remarks>
            /// To convert into HEX or Decimal, use:
            /// <code>
            ///    IntToHex(vCRC, 8);
            ///    IntToStr(vCRC);
            /// </code>
            /// </remarks>

            // Save last 8 bytes to stream
            sCRC:=IntToHex(vCRC, 8);
            rStream.Position:=rStream.Size;
            rStream.WriteBuffer(UTF8String(sCRC)[1], Length(UTF8String(sCRC)));
            rStream.Position:=0;

            // Encoding byte by byte
            for iCNT:=0 to rStream.Size - 1 do
            begin
                rStream.Read(buffer, 1);
                buffer:=(buffer xor not (ord(DecryptKey shr iCNT)));
                wStream.Write(buffer, 1);
            end;

            // Save to file
            wStream.Position:=0;

            if ConfigType = AppConfig then
                wStream.SaveToFile(FPathAppCfg);

            Result:=True;

            /// <remarks>
            /// To check stream size, use:
            /// <code>
            ///     IntToStr(wStream.Size);
            /// </code>
            /// </remarks>

        except
            Result:=False;
            pGetLastError:=IOResult;
        end;

    finally;
        rStream.Free;
        wStream.Free;
        hStream.Free;
    end;

end;


/// <summary>
/// Decoding method based on XOR and SHR with secret KEY. Routine remain the same, if the key is unchanged, then appling the same stream
/// will shift the characters numbers back to theirs original values.
/// </summary>

function TSettings.Decode(ConfigType: integer; ToMemory: boolean): boolean;
var
    iCNT:       integer;
    rStream:    TMemoryStream;
    wStream:    TMemoryStream;
    hString:    TStringList;
    bytes:      TBytes;
    buffer:     int64;
    vCRC:       dWord;
    sCRC:       string;
begin

    Result:=False;

    rStream:=TMemoryStream.Create;
    wStream:=TMemoryStream.Create;
    hString:=TStringList.Create;

    try
        try

            // Load to memory
            if ConfigType = AppConfig  then
                rStream.LoadFromFile(FPathAppCfg);

            if ConfigType = LicData then
                rStream.LoadFromFile(FPathLicenceLic);

            // Decode byte by byte
            for iCNT:=0 to rStream.Size - 1 do
            begin
                rStream.Read(buffer, 1);
                buffer:=(buffer xor not (ord(DecryptKey shr iCNT)));
                wStream.Write(buffer, 1);
            end;

            wStream.Position:=wStream.Size - 8;

            // Read last 8 bytes of embedded CRC32 checksum
            SetLength(bytes, wStream.Size - (wStream.Size - 8));
            wStream.Read(bytes[0], wStream.Size - (wStream.Size - 8));
            sCRC:=TEncoding.UTF8.GetString(bytes);

            /// <remarks>
            /// If using test file from drive, please use sCRC method instead of vCRC.
            /// </remarks>

            // Compute CRC32 checksum (between $00000000 and $FFFFFFF)
            wStream.Position:=0;
            wStream.SetSize(wStream.Size - 8);
            ComputeCRC32(wStream.Memory, wStream.Size, vCRC);

            hString.LoadFromStream(wStream);

            if ToMemory then
            begin
                if ConfigType = AppConfig then
                    TMIG.SetStrings(hString);

                if ConfigType = LicData then
                        TMIL.SetStrings(hString);

                Result:=True;
            end
            else
            begin

                if sCRC =  IntToHex(vCRC, 8) then
                    Result:=True;

                if sCRC <> IntToHex(vCRC, 8) then
                    Result:=False;

            end;

        except
            Result:=False;
            pGetLastError:=IOResult;
        end;
    finally
        rStream.Free;
        wStream.Free;
        hString.Free;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------ HELPER METHODS //


function TSettings.GetLicenceValue(Section: string; Key: string): string;
begin
    Result:='n/a';
    if Assigned(TMIL) then
        Result:=TMIL.ReadString(Section, Key, 'n/a');
end;


function TSettings.GetStringValue(Section: string; Key: string; Default: string): string;
begin
    Result:=Default;
    if Assigned(TMIG) then
        Result:=TMIG.ReadString(Section, Key, Default);
end;


procedure TSettings.SetStringValue(Section: string; Key: string; Value: string);
begin
    if Assigned(TMIG) then
        TMIG.WriteString(Section, Key, Value);
end;


function TSettings.GetIntegerValue(Section: string; Key: string; Default: integer): integer;
begin
    Result:=Default;
    if Assigned(TMIG) then
        Result:=TMIG.ReadInteger(Section, Key, Default);
end;


procedure TSettings.SetIntegerValue(Section: string; Key: string; Value: integer);
begin
    if Assigned(TMIG) then
        TMIG.WriteInteger(Section, Key, Value);
end;


procedure TSettings.GetSectionValues(Section: string; var Values: TStringList);
begin
    if Assigned(TMIG) then
        TMIG.ReadSectionValues(Section, Values);
end;


procedure TSettings.GetSection(Section: string; var Keys: TStringList);
begin
    if Assigned(TMIG) then
        TMIG.ReadSection(Section, Keys);
end;


procedure TSettings.GetSections(List: TStringList);
begin
    if Assigned(TMIG) then
        TMIG.ReadSections(List);
end;


procedure TSettings.DeleteSection(SectionName: string);
begin
    if Assigned(TMIG) then
        TMIG.EraseSection(SectionName);
end;


procedure TSettings.DeleteKey(Section: string; Ident: string);
begin
    if Assigned(TMIG) then
        TMIG.DeleteKey(Section, Ident);
end;


/// <summary>
/// Return key value for given list position.
/// </summary>

function TSettings.FindSettingsKey(Section: string; KeyPosition: integer): string;
var
    SL: TStringList;
begin
    Result:=unNA;
    SL:=TStringList.Create;
    GetSection(Section, SL);
    if KeyPosition > SL.Count then
        Exit
            else
                Result:=LeftStr(SL.Strings[KeyPosition], AnsiPos('=', SL.Strings[KeyPosition]) - 1);
end;


// --------------------------------------------------------------------------------------------------------------------------------------- GET SETTINGS DATA //


function TSettings.GetLastError: integer;
begin
    Result:=pGetLastError;
end;


function TSettings.GetAppDir: string;
begin
    Result:=FAppDir;
end;


function TSettings.GetLayoutDir: string;
begin
    Result:=FDirLayouts;
end;


function TSettings.GetPackageDir: string;
begin
    Result:=FDirPackage;
end;


function TSettings.GetAppLog: string;
begin
    Result:=FAppLog;
end;


function TSettings.GetWinUserName: string;
begin
    Result:=FWinUserName;
end;


function TSettings.GetWinTempFolder: string;
begin
    Result:=FWinTempFolder;
end;


function TSettings.GetPathGridImage: string;
begin
    Result:=FPathGridImage;
end;


function TSettings.GetPathEventLog: string;
begin
    Result:=FPathEventLog;
end;


function TSettings.GetPathAppCfg: string;
begin
    Result:=FPathAppCfg;
end;


function TSettings.GetPathLicenceLic: string;
begin
    Result:=FPathLicenceLic;
end;


function TSettings.GetReleasePakURL: string;
begin
    Result:=FReleasePakURL;
end;


function TSettings.GetReleaseManURL: string;
begin
    Result:=FReleaseManURL;
end;


function TSettings.GetLayoutsURL: string;
begin
    Result:=FGetLayoutsURL;
end;


// ---------------------------------------------------------------------------------------------------------------- GETTERS AND SETTERS FOR FOLLOW-UP COLORS //


// GETTERS ------------------------------------------------------------------------------------------------------------------------------------------------- //


// Font color
function TSettings.GetTodayFColor: TColor;
begin
    Result:=0;
    if not(Assigned(TMIG)) then
        Exit
            else
                Result:=TMIG.ReadInteger(FollowUpColors, 'TODAY_FCOLOR', 0);
end;


// Background color
function TSettings.GetTodayBColor: TColor;
begin
    Result:=0;
    if not(Assigned(TMIG)) then
        Exit
            else
                Result:=TMIG.ReadInteger(FollowUpColors, 'TODAY_BCOLOR', 0);
end;


// Font color
function TSettings.GetPastFColor: TColor;
begin
    Result:=0;
    if not(Assigned(TMIG)) then
        Exit
            else
                Result:=TMIG.ReadInteger(FollowUpColors, 'PAST_FCOLOR', 0);
end;


// Background color
function TSettings.GetPastBColor: TColor;
begin
    Result:=0;
    if not(Assigned(TMIG)) then
        Exit
            else
                Result:=TMIG.ReadInteger(FollowUpColors, 'PAST_BCOLOR', 0);
end;


// Font color
function TSettings.GetFutureFColor: TColor;
begin
    Result:=0;
    if not(Assigned(TMIG)) then
        Exit
            else
                Result:=TMIG.ReadInteger(FollowUpColors, 'FUTURE_FCOLOR', 0);
end;


// Background color
function TSettings.GetFutureBColor: TColor;
begin
    Result:=0;
    if not(Assigned(TMIG)) then
        Exit
            else
                Result:=TMIG.ReadInteger(FollowUpColors, 'FUTURE_BCOLOR', 0);
end;


// SETTERS ------------------------------------------------------------------------------------------------------------------------------------------------- //


// Font color
procedure TSettings.SetTodayFColor(NewColor: TColor);
begin
    if not(Assigned(TMIG)) then Exit;
    TMIG.WriteInteger(FollowUpColors, 'TODAY_FCOLOR', NewColor);
    Encode(AppConfig);
end;


// Background color
procedure TSettings.SetTodayBColor(NewColor: TColor);
begin
    if not(Assigned(TMIG)) then Exit;
    TMIG.WriteInteger(FollowUpColors, 'TODAY_BCOLOR', NewColor);
    Encode(AppConfig);
end;


// Font color
procedure TSettings.SetPastFColor(NewColor: TColor);
begin
    if not(Assigned(TMIG)) then Exit;
    TMIG.WriteInteger(FollowUpColors, 'PAST_FCOLOR', NewColor);
    Encode(AppConfig);
end;


// Background color
procedure TSettings.SetPastBColor(NewColor: TColor);
begin
    if not(Assigned(TMIG)) then Exit;
    TMIG.WriteInteger(FollowUpColors, 'PAST_BCOLOR', NewColor);
    Encode(AppConfig);
end;


// Font color
procedure TSettings.SetFutureFColor(NewColor: TColor);
begin
    if not(Assigned(TMIG)) then Exit;
    TMIG.WriteInteger(FollowUpColors, 'FUTURE_FCOLOR', NewColor);
    Encode(AppConfig);
end;


// Background color
procedure TSettings.SetFutureBColor(NewColor: TColor);
begin
    if not(Assigned(TMIG)) then Exit;
    TMIG.WriteInteger(FollowUpColors, 'FUTURE_BCOLOR', NewColor);
    Encode(AppConfig);
end;


end.

