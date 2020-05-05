unit Unity.Settings;

// --------------------------------------------------------------
// Application configuration module. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// --------------------------------------------------------------

interface


uses
    System.Classes,
    System.INIFiles,
    Vcl.Graphics,
    Unity.Enums;


type


    /// <summary>
    /// Exposes methods and properties for handling application settings encoded in configuration file.
    /// </summary>
    ISettings = Interface(IInterface)
    ['{FF5CBEC3-2576-4E1C-954E-C892AB4A7CC1}']
        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        procedure SetReleaseNumber(NewRelease: cardinal);
        procedure SetTodayFColor(NewColor: TColor);
        procedure SetTodayBColor(NewColor: TColor);
        procedure SetPastFColor(NewColor: TColor);
        procedure SetPastBColor(NewColor: TColor);
        procedure SetFutureFColor(NewColor: TColor);
        procedure SetFutureBColor(NewColor: TColor);
        function GetReleaseDateTime(): TDateTime;
        function GetReleaseNumber(): cardinal;
        function GetTodayFColor(): TColor;
        function GetTodayBColor(): TColor;
        function GetPastFColor(): TColor;
        function GetPastBColor(): TColor;
        function GetFutureFColor(): TColor;
        function GetFutureBColor(): TColor;
        function GetWinUserName(): string;
        function GetPathEventLog(): string;
        function GetPathConfig(): string;
        function GetPathLicence(): string;
        function GetDirApplication(): string;
        function GetDirRoaming(): string;
        function GetDirLayouts(): string;
        function GetDirAssets(): string;
        function GetDirWinTemp(): string;
        function GetNewSessionId(): string;
        function GetUrlReleasePak(): string;
        function GetUrlReleaseMan(): string;
        function GetUrlLayoutsLst(): string;
        function ConfigFileOK(): boolean;
        function CustomConfig(): boolean;
        function TestEnvSetup(): boolean;
        function GetBinFileName(): string;
        property WinUserName: string read GetWinUserName;
        property PathEventLog: string read GetPathEventLog;
        property PathConfig: string read GetPathConfig;
        property PathLicence: string read GetPathLicence;
        property DirApplication: string read GetDirApplication;
        property DirRoaming: string read GetDirRoaming;
        property DirLayouts: string read GetDirLayouts;
        property DirAssets: string read GetDirAssets;
        property DirWinTemp: string read GetDirWinTemp;
        property UrlReleasePak: string read GetUrlReleasePak;
        property UrlReleaseMan: string read GetUrlReleaseMan;
        property UrlLayoutsLst: string read GetUrlLayoutsLst;
        property ReleaseNumber: cardinal read GetReleaseNumber write SetReleaseNumber;
        property ReleaseDateTime: TDateTime read GetReleaseDateTime write SetReleaseDateTime;
        property TodayFColor: TColor read GetTodayFColor write SetTodayFColor;
        property TodayBColor: TColor read GetTodayBColor write SetTodayBColor;
        property PastFColor: TColor read GetPastFColor write SetPastFColor;
        property PastBColor: TColor read GetPastBColor write SetPastBColor;
        property FutureFColor: TColor read GetFutureFColor write SetFutureFColor;
        property FutureBColor: TColor read GetFutureBColor write SetFutureBColor;
        property NewSessionId: string read GetNewSessionId;
        property CheckConfigFile: boolean read ConfigFileOK;
        property IsUsedCustomConfig: boolean read CustomConfig;
        property IsTestEnvSetup: boolean read TestEnvSetup;
        property BinFileName: string read GetBinFileName;
        procedure Initialize(AConfigFileName: string);
        function Encode(ConfigType: TAppFiles): boolean;
        function Decode(ConfigType: TAppFiles; ToMemory: boolean): boolean;
        function ConfigToMemory(): boolean;
        function GetLicenceValue(Section: string; Key: string): string;
        function GetStringValue(Section: string; Key: string; Default: string = ''): string;
        procedure SetStringValue(Section: string; Key: string; Value: string);
        function  GetIntegerValue(Section: string; Key: string; Default: integer = 0): integer;
        procedure SetIntegerValue(Section: string; Key: string; Value: integer);
        procedure GetSectionValues(Section: string; var Values: TStringList);
        procedure GetSection(Section: string; var Keys: TStringList);
        procedure GetSections(List: TStringList);
        procedure DeleteSection(SectionName: string);
        procedure DeleteKey(Section: string; Ident: string);
        function  FindSettingsKey(Section: string; KeyPosition: integer): string;
        procedure MakeNewSessionId();
        function  MakeNewSessionFile(SessionId: string): string;
    end;


    /// <summary>
    /// Implementation for handling the configuration file.
    /// Do not use it directly.
    /// </summary>
    TSettings = class(TInterfacedObject, ISettings)
    strict private
        var FAppSettings: TMemIniFile;
        var FAppLicence: TMemIniFile;
        var FAppEnviron: TMemIniFile;
        var FLastSessionId: string;
        var FWinUserName: string;
        var FPathEventLog: string;
        var FPathConfig: string;
        var FPathLicence: string;
        var FDirApplication: string;
        var FDirRoaming: string;
        var FDirLayouts: string;
        var FDirSessions: string;
        var FDirAssets: string;
        var FDirWinTemp: string;
        var FUrlReleasePak: string;
        var FUrlReleaseMan: string;
        var FUrlLayoutsLst: string;
        var FConfigFileOK: boolean;
        var FIsUsedCustomConfig: boolean;
        var FIsTestEnvSetup: boolean;
        var FBinFileName: string;
        function GetReleaseDateTime(): TDateTime;
        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        function GetReleaseNumber(): cardinal;
        procedure SetReleaseNumber(NewRelease: cardinal);
        function GetTodayFColor(): TColor;
        function GetTodayBColor(): TColor;
        function GetPastFColor(): TColor;
        function GetPastBColor(): TColor;
        function GetFutureFColor(): TColor;
        function GetFutureBColor(): TColor;
        procedure SetTodayFColor(NewColor: TColor);
        procedure SetTodayBColor(NewColor: TColor);
        procedure SetPastFColor(NewColor: TColor);
        procedure SetPastBColor(NewColor: TColor);
        procedure SetFutureFColor(NewColor: TColor);
        procedure SetFutureBColor(NewColor: TColor);
        function GetWinUserName(): string;
        function GetPathEventLog(): string;
        function GetPathConfig(): string;
        function GetPathLicence(): string;
        function GetDirApplication(): string;
        function GetDirRoaming(): string;
        function GetDirLayouts(): string;
        function GetDirAssets(): string;
        function GetDirWinTemp(): string;
        function GetNewSessionId(): string;
        function GetUrlReleasePak(): string;
        function GetUrlReleaseMan(): string;
        function GetUrlLayoutsLst(): string;
        function ConfigFileOK(): boolean;
        function CustomConfig(): boolean;
        function TestEnvSetup(): boolean;
        function GetBinFileName(): string;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Initialize(AConfigFileName: string); virtual;
        function Encode(ConfigType: TAppFiles): boolean; virtual;
        function Decode(ConfigType: TAppFiles; ToMemory: boolean): boolean; virtual;
        function ConfigToMemory: boolean; virtual;
        function GetLicenceValue(Section: string; Key: string): string; virtual;
        function GetStringValue(Section: string; Key: string; Default: string = ''): string; virtual;
        procedure SetStringValue(Section: string; Key: string; Value: string); virtual;
        function GetIntegerValue(Section: string; Key: string; Default: integer = 0): integer; virtual;
        procedure SetIntegerValue(Section: string; Key: string; Value: integer); virtual;
        procedure GetSectionValues(Section: string; var Values: TStringList); virtual;
        procedure GetSection(Section: string; var Keys: TStringList); virtual;
        procedure GetSections(List: TStringList); virtual;
        procedure DeleteSection(SectionName: string); virtual;
        procedure DeleteKey(Section: string; Ident: string); virtual;
        function FindSettingsKey(Section: string; KeyPosition: integer): string; virtual;
        procedure MakeNewSessionId(); virtual;
        function MakeNewSessionFile(SessionId: string): string; virtual;
        property WinUserName: string read GetWinUserName;
        property PathEventLog: string read GetPathEventLog;
        property PathConfig: string read GetPathConfig;
        property PathLicence: string read GetPathLicence;
        property DirApplication: string read GetDirApplication;
        property DirRoaming: string read GetDirRoaming;
        property DirLayouts: string read GetDirLayouts;
        property DirAssets: string read GetDirAssets;
        property DirWinTemp: string read GetDirWinTemp;
        property NewSessioId: string read GetNewSessionId;
        property UrlReleasePak: string read GetUrlReleasePak;
        property UrlReleaseMan: string read GetUrlReleaseMan;
        property UrlLayoutsLst: string read GetUrlLayoutsLst;
        property CheckConfigFile: boolean read ConfigFileOK;
        property IsUsedCustomConfig: boolean read CustomConfig;
        property IsTestEnvSetup: boolean read TestEnvSetup;
        property BinFileName: string read GetBinFileName;
        property ReleaseDateTime: TDateTime read GetReleaseDateTime write SetReleaseDateTime;
        property TodayFColor: TColor read GetTodayFColor write SetTodayFColor;
        property TodayBColor: TColor read GetTodayBColor write SetTodayBColor;
        property PastFColor: TColor read GetPastFColor write SetPastFColor;
        property PastBColor: TColor read GetPastBColor write SetPastBColor;
        property FutureFColor: TColor read GetFutureFColor write SetFutureFColor;
        property FutureBColor: TColor read GetFutureBColor write SetFutureBColor;
    end;


implementation


uses
    System.SysUtils,
    System.IOUtils,
    Vcl.Forms,
    Unity.Crc32,
    Unity.Constants;


constructor TSettings.Create();
begin

    if not Assigned(FAppSettings) then FAppSettings:=TMemIniFile.Create('');
    if not Assigned(FAppLicence) then FAppLicence:=TMemIniFile.Create('');

    var InfFileName:=ExtractFileDir(Application.ExeName) + TPath.DirectorySeparatorChar + 'Unity.inf';
    if not Assigned(FAppEnviron) then FAppEnviron:=TMemIniFile.Create(InfFileName);

end;


destructor TSettings.Destroy();
begin
    if Assigned(FAppSettings) then FAppSettings.Free();
    if Assigned(FAppLicence) then FAppLicence.Free();
    if Assigned(FAppEnviron) then FAppEnviron.Free();
    inherited;
end;


function TSettings.ConfigToMemory: boolean;
begin

    Result:=False;

    if Assigned(FAppSettings) then
    begin
        FConfigFileOK:=Decode(Configuration, True);
        FUrlLayoutsLst:=FAppSettings.ReadString(TConfigSections.ApplicationDetails, 'LAYOUT_PATH', '');
        Result:=True;
    end;

end;


procedure TSettings.Initialize(AConfigFileName: string);
begin

    FWinUserName   :=Trim(LowerCase(GetEnvironmentVariable('username')));
    FDirWinTemp    :=GetEnvironmentVariable('TEMP');
    FDirApplication:=ExtractFileDir(Application.ExeName) + TPath.DirectorySeparatorChar;

    var LDataFolder:=FAppEnviron.ReadString('Environment', 'DataFolder', '');
    var LBinSource :=FAppEnviron.ReadString('Environment', 'BinSource', '');
    var LSetup     :=FAppEnviron.ReadString('Environment', 'Setup', '').ToLower();

    FBinFileName:=LBinSource;
    FDirRoaming:=TPath.GetPublicPath()
        + TPath.DirectorySeparatorChar
        + LDataFolder
        + TPath.DirectorySeparatorChar;

    FDirLayouts :=FDirRoaming + 'layouts' + TPath.DirectorySeparatorChar;
    FDirSessions:=FDirRoaming + 'sessions' + TPath.DirectorySeparatorChar;
    FDirAssets  :=FDirApplication + 'assets' + TPath.DirectorySeparatorChar;
    FPathLicence:=FDirApplication + TCommon.LicenceFile;

    if (LSetup = 'test') or (LSetup = 'env') or (LSetup = 'dev') then
        FIsTestEnvSetup:=True;

    if not String.IsNullOrEmpty(AConfigFileName) then
    begin
        FPathConfig:=FDirRoaming + AConfigFileName;
        FIsUsedCustomConfig:=True;
    end
    else
    begin
        FPathConfig:=FDirRoaming + TCommon.ConfigFile;
        FIsUsedCustomConfig:=False;
    end;

    if FileExists(FPathConfig) then ConfigToMemory() else FConfigFileOK:=False;

end;


function TSettings.Encode(ConfigType: TAppFiles): boolean;
begin

    var buffer: int64;
    var vCRC:   DWord;
    var sCRC:   string;

    // ------------------------------------
    // Do not allow to encode licence file.
    // ------------------------------------
    if ConfigType = TAppFiles.Licence then
    begin
        Result:=False;
        Exit();
    end;

    var rStream: TMemoryStream:=TMemoryStream.Create();
    var wStream: TMemoryStream:=TMemoryStream.Create();
    var hStream: TStringList:=TStringList.Create();

    try
        try

            if ConfigType = TAppFiles.Configuration then
                FAppSettings.GetStrings(hStream);

            hStream.SaveToStream(rStream);
            rStream.Position:=0;
            ComputeCRC32(rStream.Memory, rStream.Size, vCRC);

            // -------------------------------------------------------
            // To convert into HEX or Decimal, use:
            //    IntToHex(vCRC, 8); (* Save last 8 bytes to stream *)
            //    IntToStr(vCRC);
            // -------------------------------------------------------
            sCRC:=IntToHex(vCRC, 8);

            rStream.Position:=rStream.Size;
            rStream.WriteBuffer(UTF8String(sCRC)[1], Length(UTF8String(sCRC)));
            rStream.Position:=0;

            for var iCNT: integer:=0 to rStream.Size - 1 do
            begin
                rStream.Read(buffer, 1);
                buffer:=(buffer xor not (ord(TCommon.DecryptKey shr iCNT)));
                wStream.Write(buffer, 1);
            end;

            // --------------------------------------------------
            // Save encoded data to the given configuration file.
            // Note: to check stream size, use:
            //    IntToStr(wStream.Size).
            // --------------------------------------------------
            wStream.Position:=0;

            if ConfigType = Configuration then
                wStream.SaveToFile(FPathConfig);

            Result:=True;

        except
            Result:=False;
        end;

    finally;
        rStream.Free();
        wStream.Free();
        hStream.Free();
    end;

end;


function TSettings.Decode(ConfigType: TAppFiles; ToMemory: boolean): boolean;
begin

    Result:=False;

    var bytes:  TBytes;
    var buffer: int64;
    var vCRC:   DWord;
    var sCRC:   string;

    var rStream: TMemoryStream:=TMemoryStream.Create;
    var wStream: TMemoryStream:=TMemoryStream.Create;
    var hString: TStringList:=TStringList.Create;
    try
        try

            if ConfigType = TAppFiles.Configuration  then
                rStream.LoadFromFile(FPathConfig);

            if ConfigType = TAppFiles.Licence then
                rStream.LoadFromFile(FPathLicence);

            for var iCNT: integer:=0 to rStream.Size - 1 do
            begin
                rStream.Read(buffer, 1);
                buffer:=(buffer xor not (ord(TCommon.DecryptKey shr iCNT)));
                wStream.Write(buffer, 1);
            end;

            wStream.Position:=wStream.Size - 8;

            // ----------------------------------------------------------------------------
            // Read last 8 bytes of embedded CRC32 checksum.
            // Note: if using test file from drive, please use sCRC method instead of vCRC.
            // ----------------------------------------------------------------------------
            SetLength(bytes, wStream.Size - (wStream.Size - 8));
            wStream.Read(bytes[0], wStream.Size - (wStream.Size - 8));
            sCRC:=TEncoding.UTF8.GetString(bytes);

            wStream.Position:=0;
            wStream.SetSize(wStream.Size - 8);
            ComputeCRC32(wStream.Memory, wStream.Size, vCRC);

            hString.LoadFromStream(wStream);

            if ToMemory then
            begin

                if ConfigType = TAppFiles.Configuration then
                    FAppSettings.SetStrings(hString);

                if ConfigType = TAppFiles.Licence then
                        FAppLicence.SetStrings(hString);

                Result:=True;

            end
            else
            begin

                if sCRC = IntToHex(vCRC, 8) then
                    Result:=True;

                if sCRC <> IntToHex(vCRC, 8) then
                    Result:=False;

            end;

        except
            Result:=False;
        end;

    finally
        rStream.Free();
        wStream.Free();
        hString.Free();
    end;

end;


function TSettings.GetLicenceValue(Section: string; Key: string): string;
begin
    Result:='n/a';
    if Assigned(FAppLicence) then
        Result:=FAppLicence.ReadString(Section, Key, 'n/a');
end;


function TSettings.GetStringValue(Section: string; Key: string; Default: string): string;
begin
    Result:=Default;
    if Assigned(FAppSettings) then
        Result:=FAppSettings.ReadString(Section, Key, Default);
end;


procedure TSettings.SetStringValue(Section: string; Key: string; Value: string);
begin
    if Assigned(FAppSettings) then
        FAppSettings.WriteString(Section, Key, Value);
end;


function TSettings.GetIntegerValue(Section: string; Key: string; Default: integer): integer;
begin
    Result:=Default;
    if Assigned(FAppSettings) then
        Result:=FAppSettings.ReadInteger(Section, Key, Default);
end;


procedure TSettings.SetIntegerValue(Section: string; Key: string; Value: integer);
begin
    if Assigned(FAppSettings) then
        FAppSettings.WriteInteger(Section, Key, Value);
end;


procedure TSettings.GetSectionValues(Section: string; var Values: TStringList);
begin
    if Assigned(FAppSettings) then
        FAppSettings.ReadSectionValues(Section, Values);
end;


procedure TSettings.GetSection(Section: string; var Keys: TStringList);
begin
    if Assigned(FAppSettings) then
        FAppSettings.ReadSection(Section, Keys);
end;


procedure TSettings.GetSections(List: TStringList);
begin
    if Assigned(FAppSettings) then
        FAppSettings.ReadSections(List);
end;


procedure TSettings.DeleteSection(SectionName: string);
begin
    if Assigned(FAppSettings) then
        FAppSettings.EraseSection(SectionName);
end;


procedure TSettings.DeleteKey(Section: string; Ident: string);
begin
    if Assigned(FAppSettings) then
        FAppSettings.DeleteKey(Section, Ident);
end;


function TSettings.FindSettingsKey(Section: string; KeyPosition: integer): string;
begin

    Result:='N/A';

    var SL: TStringList:=TStringList.Create;
    try

        // ----------------------------------------
        // Return key name for given list position.
        // ----------------------------------------
        GetSection(Section, SL);
        if KeyPosition > SL.Count then
            Exit
        else
            Result:=SL.Strings[KeyPosition];

    finally
        SL.Free();
    end;

end;


procedure TSettings.MakeNewSessionId();
begin
    FLastSessionId:=TGUID.NewGuid.ToString().Replace('{','').Replace('}','').ToLower();
end;


function TSettings.MakeNewSessionFile(SessionId: string): string;
begin

    // ------------------------------------
    // Create file: <UserAlias>.<GUID>.log.
    // ------------------------------------
    var EventLog:=FDirSessions + SessionId.Replace('-','') + '.log';

    var FL: TFileStream:=TFileStream.Create(EventLog, fmCreate);
    try

        var StrWrite: string:=
            'Started at ' + DateToStr(Now) + ' (' + TimeToStr(Now) + '). Session signature: ' +
            SessionId +
            TChars.CRLF +
            TChars.CRLF;

        FL.Position:=FL.Size;
        for var iCNT: integer:=1 to length(StrWrite) do
            FL.Write(StrWrite[iCNT], 1);

    finally
        FL.Free;
        Result:=EventLog;
    end;

end;


function TSettings.GetReleaseNumber: cardinal;
begin

    Result:=0;

    // -------------------------------------------------------------------
    // Get release number from release manifest hosted at:
    // https://unityinfo.azurewebsites.net/release/package/unity.manifest.
    // -------------------------------------------------------------------
    if Assigned(FAppSettings) then
    begin
        Result:=StrToIntDef(FAppSettings.ReadString(TConfigSections.ApplicationDetails, 'RELEASE_NUMBER', ''), 0);
    end;

end;


function TSettings.GetReleaseDateTime: TDateTime;
begin

    Result:=TDtFormat.NullDate;

    // ----------------------------------------------------
    // Get update time and date registered in setting file.
    // ----------------------------------------------------
    if Assigned(FAppSettings) then
        Result:=StrToDateTimeDef(FAppSettings.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_DATETIME', ''), TDtFormat.NullDate);

end;


function TSettings.GetTodayFColor: TColor;
begin
    Result:=0;
    if not(Assigned(FAppSettings)) then
        Exit()
    else
        Result:=FAppSettings.ReadInteger(TConfigSections.FollowUpColors, 'TODAY_FCOLOR', 0);
end;


function TSettings.GetTodayBColor: TColor;
begin
    Result:=0;
    if not(Assigned(FAppSettings)) then
        Exit()
    else
        Result:=FAppSettings.ReadInteger(TConfigSections.FollowUpColors, 'TODAY_BCOLOR', 0);
end;


function TSettings.GetPastFColor: TColor;
begin
    Result:=0;
    if not(Assigned(FAppSettings)) then
        Exit()
    else
        Result:=FAppSettings.ReadInteger(TConfigSections.FollowUpColors, 'PAST_FCOLOR', 0);
end;


function TSettings.GetPastBColor: TColor;
begin
    Result:=0;
    if not(Assigned(FAppSettings)) then
        Exit()
    else
        Result:=FAppSettings.ReadInteger(TConfigSections.FollowUpColors, 'PAST_BCOLOR', 0);
end;


function TSettings.GetFutureFColor: TColor;
begin
    Result:=0;
    if not(Assigned(FAppSettings)) then
        Exit()
    else
        Result:=FAppSettings.ReadInteger(TConfigSections.FollowUpColors, 'FUTURE_FCOLOR', 0);
end;


function TSettings.GetFutureBColor: TColor;
begin
    Result:=0;
    if not(Assigned(FAppSettings)) then
        Exit()
    else
        Result:=FAppSettings.ReadInteger(TConfigSections.FollowUpColors, 'FUTURE_BCOLOR', 0);
end;


function TSettings.GetWinUserName(): string;
begin
    Result:=FWinUserName;
end;


function TSettings.GetPathEventLog(): string;
begin
    Result:=FPathEventLog;
end;


function TSettings.GetPathConfig(): string;
begin
    Result:=FPathConfig;
end;


function TSettings.GetPathLicence(): string;
begin
    Result:=FPathLicence;
end;


function TSettings.GetDirApplication(): string;
begin
    Result:=FDirApplication;
end;


function TSettings.GetDirRoaming(): string;
begin
    Result:=FDirRoaming;
end;


function TSettings.GetDirLayouts(): string;
begin
    Result:=FDirLayouts;
end;


function TSettings.GetDirAssets(): string;
begin
    Result:=FDirAssets;
end;


function TSettings.GetDirWinTemp(): string;
begin
    Result:=FDirWinTemp;
end;


function TSettings.GetNewSessionId(): string;
begin
    Result:=FLastSessionId;
end;


function TSettings.GetUrlReleasePak(): string;
begin
    Result:=FUrlReleasePak;
end;


function TSettings.GetUrlReleaseMan(): string;
begin
    Result:=FUrlReleaseMan;
end;


function TSettings.GetUrlLayoutsLst(): string;
begin
    Result:=FUrlLayoutsLst;
end;


function TSettings.ConfigFileOK(): boolean;
begin
    Result:=FConfigFileOK;
end;


function TSettings.CustomConfig(): boolean;
begin
    Result:=FIsUsedCustomConfig;
end;


function TSettings.TestEnvSetup(): boolean;
begin
    Result:=FIsTestEnvSetup;
end;


function TSettings.GetBinFileName(): string;
begin
    Result:=FBinFileName;
end;


procedure TSettings.SetReleaseNumber(NewRelease: cardinal);
begin

    if Assigned(FAppSettings) then
    begin
        FAppSettings.WriteInteger(TConfigSections.ApplicationDetails, 'RELEASE_NUMBER', NewRelease);
        Encode(TAppFiles.Configuration);
    end;

end;


procedure TSettings.SetReleaseDateTime(NewDateTime: TDateTime);
begin

    if Assigned(FAppSettings) then
    begin
        FAppSettings.WriteString(TConfigSections.ApplicationDetails, 'UPDATE_DATETIME', DateTimeToStr(NewDateTime));
        Encode(TAppFiles.Configuration);
    end;

end;


procedure TSettings.SetTodayFColor(NewColor: TColor);
begin
    if not(Assigned(FAppSettings)) then Exit();
    FAppSettings.WriteInteger(TConfigSections.FollowUpColors, 'TODAY_FCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetTodayBColor(NewColor: TColor);
begin
    if not(Assigned(FAppSettings)) then Exit();
    FAppSettings.WriteInteger(TConfigSections.FollowUpColors, 'TODAY_BCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetPastFColor(NewColor: TColor);
begin
    if not(Assigned(FAppSettings)) then Exit();
    FAppSettings.WriteInteger(TConfigSections.FollowUpColors, 'PAST_FCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetPastBColor(NewColor: TColor);
begin
    if not(Assigned(FAppSettings)) then Exit();
    FAppSettings.WriteInteger(TConfigSections.FollowUpColors, 'PAST_BCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetFutureFColor(NewColor: TColor);
begin
    if not(Assigned(FAppSettings)) then Exit();
    FAppSettings.WriteInteger(TConfigSections.FollowUpColors, 'FUTURE_FCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetFutureBColor(NewColor: TColor);
begin
    if not(Assigned(FAppSettings)) then Exit();
    FAppSettings.WriteInteger(TConfigSections.FollowUpColors, 'FUTURE_BCOLOR', NewColor);
    Encode(Configuration);
end;


end.

