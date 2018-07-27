{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Settings;

interface

uses
  Main, Forms, Windows, Messages, SysUtils, Classes, ShellAPI, CRC32u, INIFiles;

{ --------------------------------------------------------- ! APPLICATION SETTING CLASS ! ------------------------------------------------------------------- }
type
  TSettings = class                                    (* BASE CLASS FOR SETTINGS HANDLING *)
  {$TYPEINFO ON}
  private
    { GENERAL }
    var pAppDir                : string;
    var pLayoutDir             : string;
    var pAppLog                : string;
    var pWinUserName           : string;
    var pWinTempFolder         : string;
    { PATHS }
    var pPathEventLog          : string;
    var pPathAppCfg            : string;
    var pPathLicence           : string;
    var pPathGridImage         : string;
    var pPathRelease           : string;
    { RELEASE DATE AND TIME }
    function  pGetReleaseDateTime: TDateTime;
    procedure pSetReleaseDateTime(NewDateTime: TDateTime);
    function  pGetRelFileDateTime: TDateTime;
  public
    { GENERAL }
    var GetLastError           : integer;
    var TMIG                   : TMemIniFile;
    var TMIL                   : TMemIniFile;
    { READ ONLY PROPERTIES }
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
    { READ/WRITE PROPERTIES }
    property FReleaseDateTime  : TDateTime read pGetReleaseDateTime write pSetReleaseDateTime;
  published
    constructor Create;
    destructor  Destroy; override;
    function    Encode(ConfigType: integer): boolean;
    function    Decode(ConfigType: integer; ToMemory: boolean): boolean;
    function    ConfigToMemory: boolean;
  end;

implementation

{ ############################################################# ! SETTINGS CLASS ! ########################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALZIE }
constructor TSettings.Create;
begin

  (* INITIALIZATION *)
  TMIG:=TMemIniFile.Create('');
  TMIL:=TMemIniFile.Create('');

  (* POPULATE *)
  pAppDir        :=ExtractFileDir(Application.ExeName) + '\';
  pWinUserName   :=Trim(LowerCase(GetEnvironmentVariable('username')));
  pWinTempFolder :=GetEnvironmentVariable('TEMP');
  pAppLog        :=pWinUserName + '.log';
  pPathAppCfg    :=pAppDir + ConfigFile;
  pPathLicence   :=pAppDir + LicenceFile;
  pPathEventLog  :=pAppDir + pAppLog;
  pPathGridImage :=pAppDir + GridImgFile;

  (* READ CONFIG FILES *)
  if FileExists(pPathAppCfg) then
    ConfigToMemory
      else
        GetLastError:=404;

end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TSettings.Destroy;
begin
  TMIG.Free;
  TMIL.Free;
  inherited;
end;

{ ----------------------------------------------------------------------------------------------------------------------------- PUSH CONFIG CONTENT TO MEMORY }
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

{ -------------------------------------------------------------------------------------------------------------------------------- GET RELEASE FILE DATE&TIME }
function TSettings.pGetRelFileDateTime: TDateTime;
var
  PakDateTime: TDateTimeInfoRec;
begin
  Result:=NULLDATE;
  if pPathRelease <> '' then
  begin
    FileGetDateTimeInfo(pPathRelease, PakDateTime, True);
    Result:=StrToDateTime(FormatDateTime('yyyy-mm-dd hh:mm:ss', PakDateTime.TimeStamp)); { NOTE! "TIMESTAMP" IS PRECISE TO MILLISECONDS }
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------- GET NEW RELEASE FILE DATE&TIME }
function TSettings.pGetReleaseDateTime: TDateTime;
begin
  Result:=StrToDateTimeDef(TMIG.ReadString(ApplicationDetails, 'UPDATE_DATETIME', ''), NULLDATE);
end;

{ ---------------------------------------------------------------------------------------------------------------------------- SET NEW RELEASE FILE DATE&TIME }
procedure TSettings.pSetReleaseDateTime(NewDateTime: TDateTime);
begin
  TMIG.WriteString(ApplicationDetails, 'UPDATE_DATETIME', DateTimeToStr(NewDateTime));
  Encode(AppConfig);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- ENCRYPT & COMPUTE CRC32 }
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
  { DO NOT ALLOW TO ENCODE LICENCE }
  if ConfigType = LicData then
  begin
    Result:=False;
    Exit;
  end;
  { INITIALIZE }
  rStream:=TMemoryStream.Create;
  wStream:=TMemoryStream.Create;
  hStream:=TStringList.Create;
  try
    try
      { LOAD INI FROM MEMORY }
      if ConfigType = AppConfig  then TMIG.GetStrings(hStream);
      hStream.SaveToStream(rStream);
      rStream.Position:=0;
      { COMPUTE CRC32 CHECKSUM (BETWEEN $00000000 and $FFFFFFF) }
      ComputeCRC32(rStream.Memory, rStream.Size, vCRC);
      (* FOR DEBUG PURPOSES CRC32 (HEX): IntToHex(vCRC, 8) *)
      (* FOR DEBUG PURPOSES CRC32 (DEC): IntToStr(vCRC)    *)
      { SAVE LAST 8 BYTES TO STREAM }
      sCRC:=IntToHex(vCRC, 8);
      rStream.Position:=rStream.Size;
      rStream.WriteBuffer(UTF8String(sCRC)[1], Length(UTF8String(sCRC)));
      rStream.Position:=0;
      (*  FOR DEBUG PURPOSES: rStream.SaveToFile(AppDir + 'check.txt'); *)
      { ENCODING BYTE BY BYTE }
      for iCNT:=0 to rStream.Size - 1 do begin
        rStream.Read(buffer, 1);
        buffer:=(buffer xor not (ord(DecryptKey shr iCNT)));
        wStream.Write(buffer, 1);
      end;
      { SAVE TO FILE }
      wStream.Position:=0;
      if ConfigType = AppConfig  then wStream.SaveToFile(FPathAppCfg);
      Result:=True;
      (* FOR DEBUG: IntToStr(wStream.Size); *)
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

{ ------------------------------------------------------------------------------------------------------------------------------------- DECRYPT & CHECK CRC32 }
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
  { INITIALIZE }
  Result:=False;
  rStream:=TMemoryStream.Create;
  wStream:=TMemoryStream.Create;
  hString:=TStringList.Create;
  { PROCEED }
  try
    try
      { LOAD FROM FILE }
      if ConfigType = AppConfig  then rStream.LoadFromFile(FPathAppCfg);
      if ConfigType = LicData    then rStream.LoadFromFile(FPathLicence);
      { DECODE ALL }
      for iCNT:=0 to rStream.Size - 1 do
      begin
        rStream.Read(buffer, 1);
        buffer:=(buffer xor not (ord(DecryptKey shr iCNT)));
        wStream.Write(buffer, 1);
      end;
      wStream.Position:=wStream.Size - 8;
      { READ LAST 8 BYTES OF EMBEDDED CRC32 CHECKSUM }
      SetLength(bytes, wStream.Size - (wStream.Size - 8));
      wStream.Read(bytes[0], wStream.Size - (wStream.Size - 8));
      sCRC:=TEncoding.UTF8.GetString(bytes);
      (* FOR DEBUG: CRC32 (FROM FILE): sCRC *)
      { COMPUTE CRC32 CHECKSUM (BETWEEN $00000000 and $FFFFFFF) }
      wStream.Position:=0;
      wStream.SetSize(wStream.Size - 8);
      ComputeCRC32(wStream.Memory, wStream.Size, vCRC);
      (* FOR DEBUG: CRC32 (COMPUTED): IntToHex(vCRC, 8) (HEX)  *)
      (* FOR DEBUG: CRC32 (COMPUTED): IntToStr(vCRC) (DEC)     *)
      (* FOR DEBUG: wStream.SaveToFile(AppDir + 'decoded.txt') *)
      hString.LoadFromStream(wStream);
      if ToMemory then
      begin
        if ConfigType = AppConfig  then TMIG.SetStrings(hString);
        if ConfigType = LicData    then TMIL.SetStrings(hString);
        Result:=True;
      end
      else
      begin
        if sCRC =  IntToHex(vCRC, 8) then Result:=True;
        if sCRC <> IntToHex(vCRC, 8) then Result:=False;
      end;
    { ON ERROR }
    except
      Result:=False;
      GetLastError:=IOResult;
    end;
  finally
    (* FOR DEBUG: IntToStr(wStream.Size) *)
    { RELEASE }
    rStream.Free;
    wStream.Free;
    hString.Free;
  end;
end;

end.
