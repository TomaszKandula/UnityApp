unit Unity.Settings;

// ----------------------------------------
// Application configuration module.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    Winapi.ShellAPI,
    System.StrUtils,
    System.SysUtils,
    System.Classes,
    System.INIFiles,
    Data.Win.ADODB,
    Vcl.Forms,
    Vcl.Graphics,
    Unity.EventLogger,
    Unity.Enums;


type


    /// <summary>
    /// This interface exposes methods and properties for handling application settings
    /// encoded in configuration file.
    /// </summary>

    ISettings = Interface(IInterface)
    ['{FF5CBEC3-2576-4E1C-954E-C892AB4A7CC1}']

        // --------------------------------
        // Undisclosed getters and setters.
        // --------------------------------

        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        procedure SetReleaseNumber(NewRelease:    cardinal);
        procedure SetTodayFColor (NewColor:       TColor);
        procedure SetTodayBColor (NewColor:       TColor);
        procedure SetPastFColor  (NewColor:       TColor);
        procedure SetPastBColor  (NewColor:       TColor);
        procedure SetFutureFColor(NewColor:       TColor);
        procedure SetFutureBColor(NewColor:       TColor);
        function  GetReleaseDateTime: TDateTime;
        function  GetReleaseNumber:   cardinal;
        function  GetTodayFColor:     TColor;
        function  GetTodayBColor:     TColor;
        function  GetPastFColor:      TColor;
        function  GetPastBColor:      TColor;
        function  GetFutureFColor:    TColor;
        function  GetFutureBColor:    TColor;
        function  GetWinUserName:     string;
        function  GetPathGridImage:   string;
        function  GetPathEventLog:    string;
        function  GetPathConfig:      string;
        function  GetPathLicence:     string;
        function  GetDirApplication:  string;
        function  GetDirLayouts:      string;
        function  GetDirPackage:      string;
        function  GetDirWinTemp:      string;
        function  GetNewSessionId:    string;
        function  GetUrlReleasePak:   string;
        function  GetUrlReleaseMan:   string;
        function  GetUrlLayoutsLst:   string;
        function  ConfigFileOK:       boolean;

        // -------------------
        // Exposed properties.
        // -------------------

        property  WinUserName:     string    read GetWinUserName;
        property  PathGridImage:   string    read GetPathGridImage;
        property  PathEventLog:    string    read GetPathEventLog;
        property  PathConfig:      string    read GetPathConfig;
        property  PathLicence:     string    read GetPathLicence;
        property  DirApplication:  string    read GetDirApplication;
        property  DirLayouts:      string    read GetDirLayouts;
        property  DirPackage:      string    read GetDirPackage;
        property  DirWinTemp:      string    read GetDirWinTemp;
        property  UrlReleasePak:   string    read GetUrlReleasePak;
        property  UrlReleaseMan:   string    read GetUrlReleaseMan;
        property  UrlLayoutsLst:   string    read GetUrlLayoutsLst;
        property  ReleaseNumber:   cardinal  read GetReleaseNumber   write SetReleaseNumber;
        property  ReleaseDateTime: TDateTime read GetReleaseDateTime write SetReleaseDateTime;
        property  TodayFColor:     TColor    read GetTodayFColor     write SetTodayFColor;
        property  TodayBColor:     TColor    read GetTodayBColor     write SetTodayBColor;
        property  PastFColor:      TColor    read GetPastFColor      write SetPastFColor;
        property  PastBColor:      TColor    read GetPastBColor      write SetPastBColor;
        property  FutureFColor:    TColor    read GetFutureFColor    write SetFutureFColor;
        property  FutureBColor:    TColor    read GetFutureBColor    write SetFutureBColor;
        property  NewSessionId:    string    read GetNewSessionId;
        property  CheckConfigFile: boolean   read ConfigFileOK;

        // ----------------
        // Exposed methods.
        // ----------------

        function  Encode(ConfigType: TAppFiles): boolean;
        function  Decode(ConfigType: TAppFiles; ToMemory: boolean): boolean;
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
        procedure MakeNewSessionId();
        function  MakeNewSessionFile(SessionId: string): string;
    end;


    /// <summary>
    /// This class exposes methods and properties for configuration handling.
    /// </summary>

    TSettings = class(TInterfacedObject, ISettings)
    {$TYPEINFO ON}
    private
        var FTMIG:             TMemIniFile;
        var FTMIL:             TMemIniFile;
        var FLastSessionId:    string;
        var FWinUserName:      string;
        var FPathGridImage:    string;
        var FPathEventLog:     string;
        var FPathConfig:       string;
        var FPathLicence:      string;
        var FDirApplication:   string;
        var FDirLayouts:       string;
        var FDirPackage:       string;
        var FDirSessions:      string;
        var FDirWinTemp:       string;
        var FUrlReleasePak:    string;
        var FUrlReleaseMan:    string;
        var FUrlLayoutsLst:    string;
        var FConfigFileOK:     boolean;
        function  GetReleaseDateTime: TDateTime;
        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        function  GetReleaseNumber: cardinal;
        procedure SetReleaseNumber(NewRelease: cardinal);
        function  GetTodayFColor:  TColor;
        function  GetTodayBColor:  TColor;
        function  GetPastFColor:   TColor;
        function  GetPastBColor:   TColor;
        function  GetFutureFColor: TColor;
        function  GetFutureBColor: TColor;
        procedure SetTodayFColor (NewColor: TColor);
        procedure SetTodayBColor (NewColor: TColor);
        procedure SetPastFColor  (NewColor: TColor);
        procedure SetPastBColor  (NewColor: TColor);
        procedure SetFutureFColor(NewColor: TColor);
        procedure SetFutureBColor(NewColor: TColor);
        function  GetWinUserName:     string;
        function  GetPathGridImage:   string;
        function  GetPathEventLog:    string;
        function  GetPathConfig:      string;
        function  GetPathLicence:     string;
        function  GetDirApplication:  string;
        function  GetDirLayouts:      string;
        function  GetDirPackage:      string;
        function  GetDirWinTemp:      string;
        function  GetNewSessionId:    string;
        function  GetUrlReleasePak:   string;
        function  GetUrlReleaseMan:   string;
        function  GetUrlLayoutsLst:   string;
        function  ConfigFileOK:       boolean;
    public
        constructor Create;
        destructor Destroy; override;
        function  Encode(ConfigType: TAppFiles): boolean;
        function  Decode(ConfigType: TAppFiles; ToMemory: boolean): boolean;
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
        procedure MakeNewSessionId();
        function  MakeNewSessionFile(SessionId: string): string;
        property WinUserName:     string    read GetWinUserName;
        property PathGridImage:   string    read GetPathGridImage;
        property PathEventLog:    string    read GetPathEventLog;
        property PathConfig:      string    read GetPathConfig;
        property PathLicence:     string    read GetPathLicence;
        property DirApplication:  string    read GetDirApplication;
        property DirLayouts:      string    read GetDirLayouts;
        property DirPackage:      string    read GetDirPackage;
        property DirWinTemp:      string    read GetDirWinTemp;
        property NewSessioId:     string    read GetNewSessionId;
        property UrlReleasePak:   string    read GetUrlReleasePak;
        property UrlReleaseMan:   string    read GetUrlReleaseMan;
        property UrlLayoutsLst:   string    read GetUrlLayoutsLst;
        property CheckConfigFile: boolean   read ConfigFileOK;
        property ReleaseDateTime: TDateTime read GetReleaseDateTime write SetReleaseDateTime;
        property TodayFColor:     TColor    read GetTodayFColor     write SetTodayFColor;
        property TodayBColor:     TColor    read GetTodayBColor     write SetTodayBColor;
        property PastFColor:      TColor    read GetPastFColor      write SetPastFColor;
        property PastBColor:      TColor    read GetPastBColor      write SetPastBColor;
        property FutureFColor:    TColor    read GetFutureFColor    write SetFutureFColor;
        property FutureBColor:    TColor    read GetFutureBColor    write SetFutureBColor;
    end;


    /// <summary>
    /// This constants class defines configuration fields.
    /// </summary>

    TConfigSections = class abstract
        const ApplicationDetails = 'APPLICATION';
        const PasswordSection    = 'PASSWORD';
        const TabSheetsNames     = 'TABSHEETS_NAMES';
        const RiskClassDetails   = 'RISK_CLASS_DETAILS';
        const MailerNTLM         = 'MAILER_NTLM';
        const MailerBASIC        = 'MAILER_BASIC';
        const MailerSetup        = 'MAILER_SETUP';
        const DatabaseSetup      = 'DATABASE_SETTINGS';
        const InvoiceTypes       = 'INVOICE_TYPES';
        const TabSheetsCaps      = 'TABSHEETS_CAPTIONS';
        const Unallocated        = 'UNALLOCATED_DEFINITION';
        const Layouts            = 'EMAIL_LAYOUTS';
        const TimersSettings     = 'TIMERS_INTERVALS';
        const FollowUpColors     = 'FOLLOWUPS_COLORS';
        const AgingColumns       = 'AGEVIEW_COLUMNS';
        const AgingRanges        = 'AGEVIEW_BUCKETS';
        const ColumnPrefix       = 'COLUMN';
        const ColumnWidthName    = 'COLUMNWIDTH';
        const ColumnOrderName    = 'COLUMNORDER';
        const ColumnNames        = 'COLUMNNAMES';
        const RestApi            = 'REST_API';
    end;


implementation


uses
    Unity.Crc32,
    Unity.Common,
    Unity.Unknown,
    Unity.Chars,
    Unity.DateTimeFormats;


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TSettings.Create;
begin

    FTMIG:=TMemIniFile.Create('');
    FTMIL:=TMemIniFile.Create('');

    FWinUserName   :=Trim(LowerCase(GetEnvironmentVariable('username')));
    FDirWinTemp    :=GetEnvironmentVariable('TEMP');
    FDirApplication:=ExtractFileDir(Application.ExeName) + '\';
    FDirLayouts    :=FDirApplication + 'layouts\';
    FDirPackage    :=FDirApplication + 'package\';
    FDirSessions   :=FDirApplication + 'sessions\';
    FPathConfig    :=FDirApplication + TCommon.ConfigFile;
    FPathLicence   :=FDirApplication + TCommon.LicenceFile;
    FPathGridImage :=FDirApplication + TCommon.GridImgFile;

    if FileExists(FPathConfig) then ConfigToMemory else FConfigFileOK:=False;

end;


destructor TSettings.Destroy;
begin
    FTMIG.Free;
    FTMIL.Free;
    inherited;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- CONFIGURATION //


/// <summary>
/// Push config content to the memory.
/// </summary>

function TSettings.ConfigToMemory: boolean;
begin

    Result:=False;

    if Assigned(FTMIG) then
    begin
        FConfigFileOK:=Decode(Configuration, True);
        FUrlReleasePak:=FTMIG.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_PATH', '') + TCommon.ReleaseFile;
        FUrlReleaseMan:=FTMIG.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_PATH', '') + TCommon.ManifestFile;
        FUrlLayoutsLst:=FTMIG.ReadString(TConfigSections.ApplicationDetails, 'LAYOUT_PATH', '');
        Result:=True;
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------- ENCRYPT & DECRYPT //


/// <summary>
/// Encoding method based on XOR and SHR with secret KEY.
/// </summary>

function TSettings.Encode(ConfigType: TAppFiles): boolean;
begin

    var buffer: int64;
    var vCRC:   DWord;
    var sCRC:   string;

    // Do not allow to encode licence file
    if ConfigType = TAppFiles.Licence then
    begin
        Result:=False;
        Exit;
    end;

    var rStream: TMemoryStream:=TMemoryStream.Create;
    var wStream: TMemoryStream:=TMemoryStream.Create;
    var hStream: TStringList:=TStringList.Create;

    try
        try

            // Move file to memory
            if ConfigType = TAppFiles.Configuration then
                FTMIG.GetStrings(hStream);

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
            for var iCNT: integer:=0 to rStream.Size - 1 do
            begin
                rStream.Read(buffer, 1);
                buffer:=(buffer xor not (ord(TCommon.DecryptKey shr iCNT)));
                wStream.Write(buffer, 1);
            end;

            // Save to file
            wStream.Position:=0;

            if ConfigType = Configuration then
                wStream.SaveToFile(FPathConfig);

            Result:=True;

            /// <remarks>
            /// To check stream size, use:
            /// <code>
            ///     IntToStr(wStream.Size);
            /// </code>
            /// </remarks>

        except
            Result:=False;
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

            // Load to memory
            if ConfigType = TAppFiles.Configuration  then
                rStream.LoadFromFile(FPathConfig);

            if ConfigType = TAppFiles.Licence then
                rStream.LoadFromFile(FPathLicence);

            // Decode byte by byte
            for var iCNT: integer:=0 to rStream.Size - 1 do
            begin
                rStream.Read(buffer, 1);
                buffer:=(buffer xor not (ord(TCommon.DecryptKey shr iCNT)));
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

                if ConfigType = TAppFiles.Configuration then
                    FTMIG.SetStrings(hString);

                if ConfigType = TAppFiles.Licence then
                        FTMIL.SetStrings(hString);

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
        rStream.Free;
        wStream.Free;
        hString.Free;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------ HELPER METHODS //


function TSettings.GetLicenceValue(Section: string; Key: string): string;
begin
    Result:='n/a';
    if Assigned(FTMIL) then
        Result:=FTMIL.ReadString(Section, Key, 'n/a');
end;


function TSettings.GetStringValue(Section: string; Key: string; Default: string): string;
begin
    Result:=Default;
    if Assigned(FTMIG) then
        Result:=FTMIG.ReadString(Section, Key, Default);
end;


procedure TSettings.SetStringValue(Section: string; Key: string; Value: string);
begin
    if Assigned(FTMIG) then
        FTMIG.WriteString(Section, Key, Value);
end;


function TSettings.GetIntegerValue(Section: string; Key: string; Default: integer): integer;
begin
    Result:=Default;
    if Assigned(FTMIG) then
        Result:=FTMIG.ReadInteger(Section, Key, Default);
end;


procedure TSettings.SetIntegerValue(Section: string; Key: string; Value: integer);
begin
    if Assigned(FTMIG) then
        FTMIG.WriteInteger(Section, Key, Value);
end;


procedure TSettings.GetSectionValues(Section: string; var Values: TStringList);
begin
    if Assigned(FTMIG) then
        FTMIG.ReadSectionValues(Section, Values);
end;


procedure TSettings.GetSection(Section: string; var Keys: TStringList);
begin
    if Assigned(FTMIG) then
        FTMIG.ReadSection(Section, Keys);
end;


procedure TSettings.GetSections(List: TStringList);
begin
    if Assigned(FTMIG) then
        FTMIG.ReadSections(List);
end;


procedure TSettings.DeleteSection(SectionName: string);
begin
    if Assigned(FTMIG) then
        FTMIG.EraseSection(SectionName);
end;


procedure TSettings.DeleteKey(Section: string; Ident: string);
begin
    if Assigned(FTMIG) then
        FTMIG.DeleteKey(Section, Ident);
end;


function TSettings.FindSettingsKey(Section: string; KeyPosition: integer): string;
begin

    Result:=TUnknown.NA;

    var SL: TStringList:=TStringList.Create;
    try

        // Return key name for given list position.
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
    FLastSessionId:=TGUID.NewGuid.ToString().Replace('{','').Replace('}','').Replace('-','').ToLower();
end;


function TSettings.MakeNewSessionFile(SessionId: string): string;
begin

    // Create file: <UserAlias>.<GUID>.log
    var EventLog:=FDirSessions + FWinUserName + '.' + SessionId + '.log';

    var FL: TFileStream:=TFileStream.Create(EventLog, fmCreate);
    try

        var StrWrite: string:=
            'User `' + FWinUserName + '` @ ' +
            DateToStr(Now) + ' (' + TimeToStr(Now) + '). Session signature: ' +
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


// ------------------------------------------------------------------------------------------------------------------------------------------------- GETTERS //



function TSettings.GetReleaseNumber: cardinal;
begin

    Result:=0;

    // Get release number from release manifest hosted at:
    // https://unityinfo.azurewebsites.net/release/package/unity.manifest
    if Assigned(FTMIG) then
    begin
        Result:=StrToIntDef(FTMIG.ReadString(TConfigSections.ApplicationDetails, 'RELEASE_NUMBER', ''), 0);
    end;

end;


function TSettings.GetReleaseDateTime: TDateTime;
begin

    Result:=TDateTimeFormats.NullDate;

    // Get update time and date registered in setting file.
    if Assigned(FTMIG) then
        Result:=StrToDateTimeDef(FTMIG.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_DATETIME', ''), TDateTimeFormats.NullDate);

end;


function TSettings.GetTodayFColor: TColor;
begin
    // Font color
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'TODAY_FCOLOR', 0);
end;


function TSettings.GetTodayBColor: TColor;
begin
    // Background color
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'TODAY_BCOLOR', 0);
end;


function TSettings.GetPastFColor: TColor;
begin
    // Font color
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'PAST_FCOLOR', 0);
end;


function TSettings.GetPastBColor: TColor;
begin
    // Background color
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'PAST_BCOLOR', 0);
end;


function TSettings.GetFutureFColor: TColor;
begin
    // Font color
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'FUTURE_FCOLOR', 0);
end;


function TSettings.GetFutureBColor: TColor;
begin
    // Background color
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'FUTURE_BCOLOR', 0);
end;


function TSettings.GetWinUserName: string;
begin
    Result:=FWinUserName;
end;


function TSettings.GetPathGridImage: string;
begin
    Result:=FPathGridImage;
end;


function TSettings.GetPathEventLog: string;
begin
    Result:=FPathEventLog;
end;


function TSettings.GetPathConfig: string;
begin
    Result:=FPathConfig;
end;


function TSettings.GetPathLicence: string;
begin
    Result:=FPathLicence;
end;


function TSettings.GetDirApplication: string;
begin
    Result:=FDirApplication;
end;


function TSettings.GetDirLayouts: string;
begin
    Result:=FDirLayouts;
end;


function TSettings.GetDirPackage: string;
begin
    Result:=FDirPackage;
end;


function TSettings.GetDirWinTemp: string;
begin
    Result:=FDirWinTemp;
end;


function TSettings.GetNewSessionId: string;
begin
    Result:=FLastSessionId;
end;


function TSettings.GetUrlReleasePak: string;
begin
    Result:=FUrlReleasePak;
end;


function TSettings.GetUrlReleaseMan: string;
begin
    Result:=FUrlReleaseMan;
end;


function TSettings.GetUrlLayoutsLst: string;
begin
    Result:=FUrlLayoutsLst;
end;


function TSettings.ConfigFileOK: boolean;
begin
    Result:=FConfigFileOK;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- SETTERS //


procedure TSettings.SetReleaseNumber(NewRelease: cardinal);
begin

    // Set release number in settings file.
    if Assigned(FTMIG) then
    begin
        FTMIG.WriteInteger(TConfigSections.ApplicationDetails, 'RELEASE_NUMBER', NewRelease);
        Encode(TAppFiles.Configuration);
    end;

end;


procedure TSettings.SetReleaseDateTime(NewDateTime: TDateTime);
begin

    // Set new update time and date.
    if Assigned(FTMIG) then
    begin
        FTMIG.WriteString(TConfigSections.ApplicationDetails, 'UPDATE_DATETIME', DateTimeToStr(NewDateTime));
        Encode(TAppFiles.Configuration);
    end;

end;


procedure TSettings.SetTodayFColor(NewColor: TColor);
begin
    // Font color
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'TODAY_FCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetTodayBColor(NewColor: TColor);
begin
    // Background color
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'TODAY_BCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetPastFColor(NewColor: TColor);
begin
    // Font color
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'PAST_FCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetPastBColor(NewColor: TColor);
begin
    // Background color
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'PAST_BCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetFutureFColor(NewColor: TColor);
begin
    // Font color
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'FUTURE_FCOLOR', NewColor);
    Encode(Configuration);
end;


procedure TSettings.SetFutureBColor(NewColor: TColor);
begin
    // Background color
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'FUTURE_BCOLOR', NewColor);
    Encode(Configuration);
end;


end.

