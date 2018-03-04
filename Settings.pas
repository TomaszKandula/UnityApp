{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Settings;

interface

uses
  Main, Forms, Windows, Messages, SysUtils, Classes, ShellAPI, CRC32u, INIFiles;

{ --------------------------------------------------------- ! APPLICATION SETTING CLASS ! ------------------------------------------------------------------- }
type                                                              (* ANY THREAD *)
  TSettings = class
  {$TYPEINFO ON}
  private
    var pAppDir                : string;
    var pLayoutDir             : string;
    var pAppCfg                : string;
    var pUserCfg               : string;
    var pWinUserName           : string;
    var pLogTextTo             : string;
    var pConfigPath            : string;
    var pUserPath              : string;
    var pLicencePath           : string;
    var pGetLastError          : integer;
    var pTMIP                  : TMemIniFile;
    var pTMIG                  : TMemIniFile;
    var pTMIL                  : TMemIniFile;
  public
    property AppDir            : string      read pAppDir;
    property LayoutDir         : string      read pLayoutDir;
    property AppCfg            : string      read pAppCfg;
    property UserCfg           : string      read pUserCfg;
    property WinUserName       : string      read pWinUserName;
    property FPathEventLog     : string      read pLogTextTo;
    property FPathAppCfg       : string      read pConfigPath;
    property FPathUserCfg      : string      read pUserPath;
    property FPathLicence      : string      read pLicencePath;
    property GetLastError      : integer     read pGetLastError write pGetLastError default 0;
    property TMIP              : TMemIniFile read pTMIP         write pTMIP;
    property TMIG              : TMemIniFile read pTMIG         write pTMIG;
    property TMIL              : TMemIniFile read pTMIL         write pTMIL;
  published
    constructor Create;
    destructor  Destroy; override;
    function    Encode(ConfigType: integer): boolean;
    function    Decode(ConfigType: integer; ToMemory: boolean): boolean;
  end;

implementation

{ ############################################################# ! SETTINGS CLASS ! ########################################################################## }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TSettings.Create;
begin
  (* INITIALIZATION *)
  pTMIP:=TMemIniFile.Create('');
  pTMIG:=TMemIniFile.Create('');
  pTMIL:=TMemIniFile.Create('');
  pAppDir     :=ExtractFileDir(Application.ExeName) + '\';
  pWinUserName:=Trim(LowerCase(GetEnvironmentVariable('username')));
  pAppCfg     :=pWinUserName + '.log';                  { EVENT LOG FILE NAME   }
  pUserCfg    :=pWinUserName + '.cfg';                  { USER CONFIG FILE NAME }
  pConfigPath :=pAppDir + ConfigFile;                   { CONFIG FULL PATH      }
  pLicencePath:=pAppDir + LicenceFile;                  { LICENCE FULL PATH     }
  pUserPath   :=pAppDir + UserFolder + '\' + pUserCfg;  { USER CONFIG FULL PATH }
  pLogTextTo  :=pAppDir + UserFolder + '\' + pAppCfg;   { EVENT LOG FULL PATH   }
  (* READ CONFIG FILES *)
  if FileExists(pConfigPath) then
  begin
    Decode(AppConfig, True);
    pLayoutDir:=pTMIG.ReadString(VariousLayouts, 'PATH', 'C:\');
  end else pGetLastError:=104;
  if FileExists(pUserPath) then Decode(UserConfig, True) else pGetLastError:=104;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- DISPOSE }
destructor TSettings.Destroy;
begin
  FreeAndNil(pTMIP);
  FreeAndNil(pTMIG);
  FreeAndNil(pTMIL);
  inherited;
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
      if ConfigType = UserConfig then TMIP.GetStrings(hStream);
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
        buffer:=(buffer xor not (ord(FILEKEY shr iCNT)));
        wStream.Write(buffer, 1);
      end;
      { SAVE TO FILE }
      wStream.Position:=0;
      if ConfigType = UserConfig then wStream.SaveToFile(FPathUserCfg);
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
      if ConfigType = UserConfig then rStream.LoadFromFile(FPathUserCfg);
      if ConfigType = AppConfig  then rStream.LoadFromFile(FPathAppCfg);
      if ConfigType = LicData    then rStream.LoadFromFile(FPathLicence);
      { DECODE ALL }
      for iCNT:=0 to rStream.Size - 1 do
      begin
        rStream.Read(buffer, 1);
        buffer:=(buffer xor not (ord(FILEKEY shr iCNT)));
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
        if ConfigType = UserConfig then pTMIP.SetStrings(hString);
        if ConfigType = AppConfig  then pTMIG.SetStrings(hString);
        if ConfigType = LicData    then pTMIL.SetStrings(hString);
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
