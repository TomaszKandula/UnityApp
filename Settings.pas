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
    var pLogFile               : string;
    var pLogonFile             : string;
    var pWinUserName           : string;
    var pLogTextTo             : string;
    var pConfigPath            : string;
    var pLogonPath             : string;
    var pLicencePath           : string;
    var pTMIP                  : TMemIniFile;
    var pTMIG                  : TMemIniFile;
  public
    property AppDir            : string      read pAppDir;
    property LayoutDir         : string      read pLayoutDir;
    property LogFile           : string      read pLogFile;
    property LogonFile         : string      read pLogonFile;
    property WinUserName       : string      read pWinUserName;
    property FPathLog          : string      read pLogTextTo;
    property FPathConfig       : string      read pConfigPath;
    property FPathLogon        : string      read pLogonPath;
    property FPathLicence      : string      read pLicencePath;
    property TMIP              : TMemIniFile read pTMIP;
    property TMIG              : TMemIniFile read pTMIG;
  published
    constructor Create(Caption: string);
    destructor  Destroy; override;
    function    Encode(ConfigType: integer): boolean;
    function    Decode(ConfigType: integer; ToMemory: boolean): boolean;
  end;

implementation

{ ############################################################# ! SETTINGS CLASS ! ########################################################################## }

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TSettings.Create(Caption: string);
begin

  (* INITIALIZATION *)
  pTMIP:=TMemIniFile.Create('');
  pTMIG:=TMemIniFile.Create('');

  pAppDir     :=ExtractFileDir(Application.ExeName) + '\';
  pWinUserName:=Trim(LowerCase(GetEnvironmentVariable('username')));

  pLogFile    :=UserFolder + '\' + pWinUserName + '.log'; { EVENT LOG   }
  pLogonFile  :=UserFolder + '\' + pWinUserName + '.cfg'; { USER CONFIG }

  pConfigPath :=pAppDir + ConfigFile;                     { CONFIG FULL PATH      }
  pLicencePath:=pAppDir + LicenceFile;                    { LICENCE FULL PATH     }
  pLogonPath  :=pAppDir + UserFolder + '\' + pLogonFile;  { USER CONFIG FULL PATH }
  pLogTextTo  :=pAppDir + UserFolder + '\' + pLogFile;    { EVENT LOG FULL PATH   }



  (* CONFIG.CFG *)

  if not (Decode(AppConfig, True)) then
  begin
    LogText(pLogTextTo, 'Cannot read master configuration file. Error code: ' + IntToStr(Error) + '. Application terminated.');
    Application.MessageBox(PChar('Cannot read master configuration file. Error code: ' + IntToStr(Error) + '. Please contact support.'),
                           PChar(Caption), MB_OK + MB_ICONSTOP);
  end;

  (* [LOGON].CFG *)

  if not (Decode(UserConfig, True)) then
  begin
    LogText(pLogTextTo, 'Cannot read user configuration file. Error code: ' + IntToStr(Error) + '. Application terminated.');
    Application.MessageBox(PChar('Cannot read user configuration file. Error code: ' + IntToStr(Error) + '. Please contact support.'),
                           PChar(Caption), MB_OK + MB_ICONSTOP);
  end;

  (* PASSWORD *)
  if TMIG.ReadString(Password, 'VALUE', '') = '' then
  begin
    LogText(pLogTextTo, 'WARNING! Master password is not established.');
    Application.MessageBox(PChar('Master password is not established. Please contact IT support to reset it.'), PChar(Caption), MB_OK + MB_ICONWARNING);
  end;

  (* LAYOUTS *)
  pLayoutDir:=TMIG.ReadString(VariousLayouts, 'PATH', 'C:\');


end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- DISPOSE }
destructor TSettings.Destroy;
begin
  FreeAndNil(pTMIP);
  FreeAndNil(pTMIG);
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
      if ConfigType = UserConfig then wStream.SaveToFile(FPathLogon);
      if ConfigType = AppConfig  then wStream.SaveToFile(FPathConfig);

      Result:=True;

      (* FOR DEBUG: IntToStr(wStream.Size); *)

    except
       Result:=False;
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
    { LOAD FROM FILE }
    if ConfigType = UserConfig then rStream.LoadFromFile(FPathLogon);
    if ConfigType = AppConfig  then rStream.LoadFromFile(FPathConfig);

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
      if ConfigType = UserConfig then TMIP.SetStrings(hString);
      if ConfigType = AppConfig  then TMIG.SetStrings(hString);
      Result:=True;
    end
    else
    begin
      if sCRC =  IntToHex(vCRC, 8) then Result:=True;
      if sCRC <> IntToHex(vCRC, 8) then Result:=False;
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
