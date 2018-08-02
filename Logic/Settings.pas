
{$I .\Include\Header.inc}

unit Settings;

interface

uses
    Main, Forms, Windows, Messages, SysUtils, Classes, ShellAPI, CRC32u, INIFiles, Graphics;

type

    /// <summary>
    ///     Interface exposing methods for encoding, decoding settings file and uploading it into memory. It also exposes
    ///     getters and setters for follow-up colors.
    /// </summary>

    ISettings = Interface(IInterface)
    ['{FF5CBEC3-2576-4E1C-954E-C892AB4A7CC1}']
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
        function  Encode(ConfigType: integer): boolean;
        function  Decode(ConfigType: integer; ToMemory: boolean): boolean;
        function  ConfigToMemory: boolean;
        property TodayFColor : TColor read GetTodayFColor  write SetTodayFColor;
        property TodayBColor : TColor read GetTodayBColor  write SetTodayBColor;
        property PastFColor  : TColor read GetPastFColor   write SetPastFColor;
        property PastBColor  : TColor read GetPastBColor   write SetPastBColor;
        property FutureFColor: TColor read GetFutureFColor write SetFutureFColor;
        property FutureBColor: TColor read GetFutureBColor write SetFutureBColor;
    end;

    /// <summary>
    ///    Class constructor is responsibe for setting up variables with file paths and for the initialization of two separate
    ///    classes that keeps settings during program runtime.
    /// </summary>

    TSettings = class(TInterfacedObject, ISettings)
    {$TYPEINFO ON}
    private
        // Files
        var pAppDir        : string;
        var pLayoutDir     : string;
        var pAppLog        : string;
        var pWinUserName   : string;
        var pWinTempFolder : string;
        // Paths
        var pPathEventLog  : string;
        var pPathAppCfg    : string;
        var pPathLicence   : string;
        var pPathGridImage : string;
        var pPathRelease   : string;
        // Time and date
        function  pGetReleaseDateTime: TDateTime;
        procedure pSetReleaseDateTime(NewDateTime: TDateTime);
        function  pGetRelFileDateTime: TDateTime;
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
        var GetLastError           : integer;
        var TMIG                   : TMemIniFile;
        var TMIL                   : TMemIniFile;
        property FAppDir           : string    read pAppDir;
        property FLayoutDir        : string    read pLayoutDir;
        property FAppLog           : string    read pAppLog;
        property FWinUserName      : string    read pWinUserName;
        property FWinTempFolder    : string    read pWinTempFolder;
        property FPathGridImage    : string    read pPathGridImage;
        property FPathEventLog     : string    read pPathEventLog;
        property FPathAppCfg       : string    read pPathAppCfg;
        property FPathLicence      : string    read pPathLicence;
        property FPathRelease      : string    read pPathRelease;
        property FRelFileDateTime  : TDateTime read pGetRelFileDateTime;
        property FReleaseDateTime  : TDateTime read pGetReleaseDateTime write pSetReleaseDateTime;
        property TodayFColor       : TColor    read GetTodayFColor      write SetTodayFColor;
        property TodayBColor       : TColor    read GetTodayBColor      write SetTodayBColor;
        property PastFColor        : TColor    read GetPastFColor       write SetPastFColor;
        property PastBColor        : TColor    read GetPastBColor       write SetPastBColor;
        property FutureFColor      : TColor    read GetFutureFColor     write SetFutureFColor;
        property FutureBColor      : TColor    read GetFutureBColor     write SetFutureBColor;
        constructor Create;
        destructor  Destroy; override;
        function    Encode(ConfigType: integer): boolean;
        function    Decode(ConfigType: integer; ToMemory: boolean): boolean;
        function    ConfigToMemory: boolean;
    end;

implementation


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TSettings.Create;
begin

    /// <remarks>
    ///    General settings (config.cfg), containing all the necessary data for the application.
    /// </remarks>

    TMIG:=TMemIniFile.Create('');

    /// <remarks>
    ///    Licence details (unity.licx).
    /// </remarks>

    TMIL:=TMemIniFile.Create('');


    pAppDir        :=ExtractFileDir(Application.ExeName) + '\';
    pWinUserName   :=Trim(LowerCase(GetEnvironmentVariable('username')));
    pWinTempFolder :=GetEnvironmentVariable('TEMP');
    pAppLog        :=pWinUserName + '.log';
    pPathAppCfg    :=pAppDir + ConfigFile;
    pPathLicence   :=pAppDir + LicenceFile;
    pPathEventLog  :=pAppDir + pAppLog;
    pPathGridImage :=pAppDir + GridImgFile;


    /// <remarks>
    ///    Return 404 error code if configuration file cannot be found.
    /// </remarks>

    if FileExists(pPathAppCfg) then
        ConfigToMemory
            else
                GetLastError:=404;

end;

/// <summary>
///    Release all from memory. Call it before main form is destroyed.
/// </summary>

destructor TSettings.Destroy;
begin
    TMIG.Free;
    TMIL.Free;
    inherited;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- CONFIGURATION //


/// <summary>
///    Push config content to memory.
/// </summary>

function TSettings.ConfigToMemory: boolean;
begin
    Result:=False;

    if Assigned(TMIG) then
    begin
        Decode(AppConfig, True);
        pLayoutDir  :=TMIG.ReadString(VariousLayouts,     'PATH',        '');
        pPathRelease:=TMIG.ReadString(ApplicationDetails, 'UPDATE_PATH', '');
        pPathRelease:=pPathRelease + ReleaseFile;
        GetLastError:=0;
        Result:=True;
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------- RELEASE PACKAGE DATE & TIME //


/// <summary>
///   Get file date and time for "Release.pak".
/// </summary>

function TSettings.pGetRelFileDateTime: TDateTime;
var
    PakDateTime: TDateTimeInfoRec;
begin

    Result:=NULLDATE;

    if pPathRelease <> '' then
    begin
        FileGetDateTimeInfo(pPathRelease, PakDateTime, True);

        /// <remarks>
        ///    Timestamp method is precise to a miliseconds.
        ///    This has to be considered during any comparision.
        /// </remarks>

        Result:=StrToDateTime(FormatDateTime('yyyy-mm-dd hh:mm:ss', PakDateTime.TimeStamp));
    end;

end;

/// <summary>
///    Get update time and date registered in setting file.
/// </summary>

function TSettings.pGetReleaseDateTime: TDateTime;
begin
    Result:=NULLDATE;

    if Assigned(TMIG) then
    begin
        Result:=StrToDateTimeDef(TMIG.ReadString(ApplicationDetails, 'UPDATE_DATETIME', ''), NULLDATE);
    end;

end;

/// <summary>
///    Set new update time and date.
/// </summary>

procedure TSettings.pSetReleaseDateTime(NewDateTime: TDateTime);
begin
    if Assigned(TMIG) then
    begin
        TMIG.WriteString(ApplicationDetails, 'UPDATE_DATETIME', DateTimeToStr(NewDateTime));
        Encode(AppConfig);
    end;
end;


// --------------------------------------------------------------------------------------------------------------------------------------- ENCRYPT & DECRYPT //


/// <summary>
///    Encoding method based on XOR and SHR with secret KEY.
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
            ///    To convert into HEX or Decimal, use:
            ///    <code>
            ///        IntToHex(vCRC, 8);
            ///        IntToStr(vCRC);
            ///    </code>
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
            ///    To check stream size, use:
            ///    <code>
            ///        IntToStr(wStream.Size);
            ///    </code>
            /// </remarks>

        except
            Result:=False;
            GetLastError:=IOResult;
        end;

    finally;
        rStream.Free;
        wStream.Free;
        hStream.Free;
    end;

end;

/// <summary>
///    Decoding method based on XOR and SHR with secret KEY. Routine remain the same, if the key is unchanged, then appling the same stream
///    will shift the characters numbers back to theirs original values.
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
                rStream.LoadFromFile(FPathLicence);

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
            ///    If using test file from drive, please use sCRC method instead of vCRC.
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
            GetLastError:=IOResult;
        end;
    finally
        rStream.Free;
        wStream.Free;
        hString.Free;
    end;

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
