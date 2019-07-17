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
    Vcl.Forms,
    Vcl.Graphics,
    Unity.Crc32,
    Unity.Statics;


type


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
        function  GetLayoutLists:     TStringList;
        function  GetTodayFColor:     TColor;
        function  GetTodayBColor:     TColor;
        function  GetPastFColor:      TColor;
        function  GetPastBColor:      TColor;
        function  GetFutureFColor:    TColor;
        function  GetFutureBColor:    TColor;

        // -------------------
        // Exposed properties.
        // -------------------

        property  LayoutLists:     TStringList read GetLayoutLists;
        property  ReleaseNumber:   cardinal    read GetReleaseNumber   write SetReleaseNumber;
        property  ReleaseDateTime: TDateTime   read GetReleaseDateTime write SetReleaseDateTime;
        property  TodayFColor:     TColor      read GetTodayFColor     write SetTodayFColor;
        property  TodayBColor:     TColor      read GetTodayBColor     write SetTodayBColor;
        property  PastFColor:      TColor      read GetPastFColor      write SetPastFColor;
        property  PastBColor:      TColor      read GetPastBColor      write SetPastBColor;
        property  FutureFColor:    TColor      read GetFutureFColor    write SetFutureFColor;
        property  FutureBColor:    TColor      read GetFutureBColor    write SetFutureBColor;

        // ----------------
        // Exposed methods.
        // ----------------

        function  Encode(ConfigType: TCommon.TFiles): boolean;
        function  Decode(ConfigType: TCommon.TFiles; ToMemory: boolean): boolean;
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
        function  GetLastError:      integer;
        function  GetAppDir:         string;
        function  GetAppLog:         string;
        function  GetLayoutDir:      string;
        function  GetPackageDir:     string;
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


    TSettings = class(TInterfacedObject, ISettings)
    {$TYPEINFO ON}
    private

        // ------------------------------------
        // TMemoryIni holding encoded settings.
        // ------------------------------------

        var FTMIG: TMemIniFile;
        var FTMIL: TMemIniFile;
        var FList: TStringList;
        var FGetLastError:  integer;
        var FWinUserName:   string;
        var FPathGridImage: string;

        // --------------------
        // Directories holders.
        // --------------------

        var FAppLog:         string;
        var FPathEventLog:   string;
        var FPathAppCfg:     string;
        var FPathLicenceLic: string;

        // -------------
        // Files holder.
        // -------------

        var FAppDir:        string;
        var FDirLayouts:    string;
        var FDirPackage:    string;
        var FWinTempFolder: string;

        // -------------
        // URLs holders.
        // -------------

        var FReleasePakURL: string;
        var FReleaseManURL: string;
        var FGetLayoutsURL: string;

        // --------
        // Methods.
        // --------

        function  GetReleaseDateTime: TDateTime;
        procedure SetReleaseDateTime(NewDateTime: TDateTime);
        function  GetReleaseNumber: cardinal;
        procedure SetReleaseNumber(NewRelease: cardinal);
        function  GetLayoutLists: TStringList;

        // ------------------------------------------------------------------
        // Getters and setters for "follow-up" colors saved in settings file.
        // ------------------------------------------------------------------

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
    public
        constructor Create;
        destructor Destroy; override;
        function  Encode(ConfigType: TCommon.TFiles): boolean;
        function  Decode(ConfigType: TCommon.TFiles; ToMemory: boolean): boolean;
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
        function  GetLastError:      integer;
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
        property WinUserName:     string    read FWinUserName;
        property AppLog:          string    read FAppLog;
        property PathGridImage:   string    read FPathGridImage;
        property PathEventLog:    string    read FPathEventLog;
        property PathAppCfg:      string    read FPathAppCfg;
        property PathLicenceLic:  string    read FPathLicenceLic;
        property AppDir:          string    read FAppDir;
        property DirLayouts:      string    read FDirLayouts;
        property DirPackage:      string    read FDirPackage;
        property WinTempFolder:   string    read FWinTempFolder;
        property ReleaseDateTime: TDateTime read GetReleaseDateTime write SetReleaseDateTime;
        property TodayFColor:     TColor    read GetTodayFColor     write SetTodayFColor;
        property TodayBColor:     TColor    read GetTodayBColor     write SetTodayBColor;
        property PastFColor:      TColor    read GetPastFColor      write SetPastFColor;
        property PastBColor:      TColor    read GetPastBColor      write SetPastBColor;
        property FutureFColor:    TColor    read GetFutureFColor    write SetFutureFColor;
        property FutureBColor:    TColor    read GetFutureBColor    write SetFutureBColor;
    end;


    TConfigSections = class abstract
        const ApplicationDetails = 'APPLICATION';
        const PasswordSection    = 'PASSWORD';
        const TabSheetsNames     = 'TABSHEETS_NAMES';
        const RiskClassDetails   = 'RISK_CLASS_DETAILS';
        const MailerNTLM         = 'MAILER_NTLM';
        const MailerBASIC        = 'MAILER_BASIC';
        const MailerSetup        = 'MAILER_SETUP';
        const DatabaseSetup      = 'DATABASE_SETTINGS';
        const OpenItemsData      = 'OPEN_ITEMS';
        const AddressBookData    = 'ADDRESS_BOOK';
        const GeneralTables      = 'GENERAL_TABLES';
        const InvoiceTypes       = 'INVOICE_TYPES';
        const TabSheetsCaps      = 'TABSHEETS_CAPTIONS';
        const AgingRanges        = 'AGEVIEW_RANGES';
        const AgingBasic         = 'AGEVIEW_BASIC';
        const AgingFull          = 'AGEVIEW_FULL';
        const Unallocated        = 'UNALLOCATED_DEFINITION';
        const Layouts            = 'LAYOUTS';
        const TimersSettings     = 'TIMERS_INTERVALS';
        const FollowUpColors     = 'FOLLOWUPS_COLORS';
        const ColumnPrefix       = 'COLUMN';
        const ColumnWidthName    = 'COLUMNWIDTH';
        const ColumnOrderName    = 'COLUMNORDER';
        const ColumnNames        = 'COLUMNNAMES';
    end;


implementation


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TSettings.Create;
begin

    // -------------------------------------------------------------------------------------
    // General settings (config.cfg), containing all the necessary data for the application.
    // -------------------------------------------------------------------------------------

    FTMIG:=TMemIniFile.Create('');

    // -----------------------------
    // Licence details (unity.licx).
    // -----------------------------

    FTMIL:=TMemIniFile.Create('');

    // -------------------------------------
    // Holds list of all registered layouts.
    // -------------------------------------

    FList:=TStringList.Create;

    // -----------
    // User Alias.
    // -----------

    FWinUserName   :=Trim(LowerCase(GetEnvironmentVariable('username')));

    // ------------
    // Directories.
    // ------------

    FAppDir        :=ExtractFileDir(Application.ExeName) + '\';
    FDirLayouts    :=FAppDir + 'layouts\';
    FDirPackage    :=FAppDir + 'package\';
    FWinTempFolder :=GetEnvironmentVariable('TEMP');

    // ------
    // Files.
    // ------

    FAppLog        :=FWinUserName + '.log';
    FPathAppCfg    :=FAppDir + TCommon.ConfigFile;
    FPathLicenceLic:=FAppDir + TCommon.LicenceFile;
    FPathEventLog  :=FAppDir + FAppLog;
    FPathGridImage :=FAppDir + TCommon.GridImgFile;

    // ------------------------------------------------------------
    // Return 404 error code if configuration file cannot be found.
    // ------------------------------------------------------------

    if FileExists(FPathAppCfg) then ConfigToMemory else FGetLastError:=404;

end;


destructor TSettings.Destroy;
begin
    FTMIG.Free;
    FTMIL.Free;
    FList.Free;
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

        Decode(AppConfig, True);

        FReleasePakURL:=FTMIG.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_PATH', '') + TCommon.ReleaseFile;
        FReleaseManURL:=FTMIG.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_PATH', '') + TCommon.ManifestFile;

        FGetLayoutsURL:=FTMIG.ReadString(TConfigSections.ApplicationDetails, 'LAYOUT_PATH', '');

        GetSectionValues(TConfigSections.Layouts, FList);
        for var iCNT: integer:=0 to FList.Count - 1 do
        begin
            FList.Strings[iCNT]:=MidStr(FList.Strings[iCNT], AnsiPos('=', FList.Strings[iCNT]) + 1, 255);
        end;

        FGetLastError:=0;
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

    if Assigned(FTMIG) then
    begin
        Result:=StrToIntDef(FTMIG.ReadString(TConfigSections.ApplicationDetails, 'RELEASE_NUMBER', ''), 0);
    end;

end;


/// <summary>
/// Set release number in settings file.
/// </summary>

procedure TSettings.SetReleaseNumber(NewRelease: cardinal);
begin
    if Assigned(FTMIG) then
    begin
        FTMIG.WriteInteger(TConfigSections.ApplicationDetails, 'RELEASE_NUMBER', NewRelease);
        Encode(TCommon.TFiles.AppConfig);
    end;
end;


/// <summary>
/// Pass layout list from settings file to a usable variable.
/// </summary>

function TSettings.GetLayoutLists: TStringList;
begin
    Result:=FList;
end;


/// <summary>
/// Get update time and date registered in setting file.
/// </summary>

function TSettings.GetReleaseDateTime: TDateTime;
begin

    Result:=TDateTimeFormats.NullDate;

    if Assigned(FTMIG) then
        Result:=StrToDateTimeDef(FTMIG.ReadString(TConfigSections.ApplicationDetails, 'UPDATE_DATETIME', ''), TDateTimeFormats.NullDate);

end;


/// <summary>
/// Set new update time and date.
/// </summary>

procedure TSettings.SetReleaseDateTime(NewDateTime: TDateTime);
begin

    if Assigned(FTMIG) then
    begin
        FTMIG.WriteString(TConfigSections.ApplicationDetails, 'UPDATE_DATETIME', DateTimeToStr(NewDateTime));
        Encode(TCommon.TFiles.AppConfig);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------- ENCRYPT & DECRYPT //


/// <summary>
/// Encoding method based on XOR and SHR with secret KEY.
/// </summary>

function TSettings.Encode(ConfigType: TCommon.TFiles): boolean;
begin

    var buffer:  int64;
    var vCRC:    DWord;
    var sCRC:    string;

    // Do not allow to encode licence file
    if ConfigType = TCommon.TFiles.LicData then
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
            if ConfigType = TCommon.TFiles.AppConfig then
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
            FGetLastError:=IOResult;
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

function TSettings.Decode(ConfigType: TCommon.TFiles; ToMemory: boolean): boolean;
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
            if ConfigType = TCommon.TFiles.AppConfig  then
                rStream.LoadFromFile(FPathAppCfg);

            if ConfigType = TCommon.TFiles.LicData then
                rStream.LoadFromFile(FPathLicenceLic);

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
                if ConfigType = TCommon.TFiles.AppConfig then
                    FTMIG.SetStrings(hString);

                if ConfigType = TCommon.TFiles.LicData then
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
            FGetLastError:=IOResult;
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


/// <summary>
/// Return key value for given list position.
/// </summary>

function TSettings.FindSettingsKey(Section: string; KeyPosition: integer): string;
begin

    Result:=TUnknown.NA;

    var SL: TStringList:=TStringList.Create;

    GetSection(Section, SL);
    if KeyPosition > SL.Count then
        Exit
            else
                Result:=LeftStr(SL.Strings[KeyPosition], AnsiPos('=', SL.Strings[KeyPosition]) - 1);
end;


// --------------------------------------------------------------------------------------------------------------------------------------- GET SETTINGS DATA //


function TSettings.GetLastError: integer;
begin
    Result:=FGetLastError;
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
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'TODAY_FCOLOR', 0);
end;


// Background color
function TSettings.GetTodayBColor: TColor;
begin
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'TODAY_BCOLOR', 0);
end;


// Font color
function TSettings.GetPastFColor: TColor;
begin
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'PAST_FCOLOR', 0);
end;


// Background color
function TSettings.GetPastBColor: TColor;
begin
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'PAST_BCOLOR', 0);
end;


// Font color
function TSettings.GetFutureFColor: TColor;
begin
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'FUTURE_FCOLOR', 0);
end;


// Background color
function TSettings.GetFutureBColor: TColor;
begin
    Result:=0;
    if not(Assigned(FTMIG)) then
        Exit
            else
                Result:=FTMIG.ReadInteger(TConfigSections.FollowUpColors, 'FUTURE_BCOLOR', 0);
end;


// SETTERS ------------------------------------------------------------------------------------------------------------------------------------------------- //


// Font color
procedure TSettings.SetTodayFColor(NewColor: TColor);
begin
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'TODAY_FCOLOR', NewColor);
    Encode(AppConfig);
end;


// Background color
procedure TSettings.SetTodayBColor(NewColor: TColor);
begin
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'TODAY_BCOLOR', NewColor);
    Encode(AppConfig);
end;


// Font color
procedure TSettings.SetPastFColor(NewColor: TColor);
begin
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'PAST_FCOLOR', NewColor);
    Encode(AppConfig);
end;


// Background color
procedure TSettings.SetPastBColor(NewColor: TColor);
begin
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'PAST_BCOLOR', NewColor);
    Encode(AppConfig);
end;


// Font color
procedure TSettings.SetFutureFColor(NewColor: TColor);
begin
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'FUTURE_FCOLOR', NewColor);
    Encode(AppConfig);
end;


// Background color
procedure TSettings.SetFutureBColor(NewColor: TColor);
begin
    if not(Assigned(FTMIG)) then Exit;
    FTMIG.WriteInteger(TConfigSections.FollowUpColors, 'FUTURE_BCOLOR', NewColor);
    Encode(AppConfig);
end;


end.

