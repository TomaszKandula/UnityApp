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
program UnityCmd;

{$APPTYPE CONSOLE}

{ ---------------------------------------------------------- ! LIBRARYS ! ----------------------------------------------------------------------------------- }
uses
  SysUtils, Windows, Messages, ShellAPI, INIFIles, Coder;

{ ---------------------------------------------- ! COMMON VARIABLES, CONSTANTS AND TYPES ! ------------------------------------------------------------------ }
type
  COORD = record
    X:  smallint;
    Y:  smallint;
  end;

  TCONSOLE_FONT_INFOEX = record
    cbSize:      cardinal;
    nFont:       longword;
    dwFontSize:  COORD;
    FontFamily:  cardinal;
    FontWeight:  cardinal;
    FaceName:    array [0 .. LF_FACESIZE - 1] of WideChar;
  end;

  PCONSOLE_FONT_INFOEX = ^TCONSOLE_FONT_INFOEX;

const
  ExeName:   string  = 'Unity.exe';
  CfgName:   string  = 'Config.cfg';
  LW_KEY:    integer =  429496;

var
  AppDir:    string;
  UpdDir:    string;
  UpMode:    string;
  ExpMsg:    string;
  Error:     integer;

{ ----------------------------------------------------- ! DLL STATIC IMPORTS ! ------------------------------------------------------------------------------ }
function SetCurrentConsoleFontEx(ConsoleOutput: THandle; MaximumWindow: BOOL; ConsoleInfo: PCONSOLE_FONT_INFOEX): BOOL; stdcall;
         external kernel32 name 'SetCurrentConsoleFontEx';
function GetCurrentConsoleFontEx(ConsoleOutput: THandle; MaximumWindow: BOOL; ConsoleInfo: PCONSOLE_FONT_INFOEX): BOOL; stdcall;
         external kernel32 name 'GetCurrentConsoleFontEx';
function GetOSVer(mode: integer): string; stdcall; external 'Unitylib.dll';

{ ------------------------------------------------------- ! COMMON METHODS ! -------------------------------------------------------------------------------- }
{ ------------------------------------------------------------------------------------------------------------------------------------------ SET CONSOLE FONT }
function SetConsoleFont(const AFontSize: word): boolean;
var
  ci: TCONSOLE_FONT_INFOEX;
  ch: THandle;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZATION }
  Result:=True;
  { -------------------------------------------------------------------------- SET CONSOLE FONT AVAILABLE SINCE WINDOWS VISTA, WE REQUIRE WINDOWS 7 AND NEWER }
  if not StrToInt(GetOSVer(0)) >= 61 then Result:=False;
  { ------------------------------------------------------------------------------------------------------------------------ SET CONSOLE HANDLE AND FONT SIZE }
  FillChar(ci, SizeOf(TCONSOLE_FONT_INFOEX), 0);
  ci.cbSize:=  SizeOf(TCONSOLE_FONT_INFOEX);
  ch:=GetStdHandle(STD_OUTPUT_HANDLE);
  { --------------------------------------------------------------------------------------------------------------------------------------------- GET CURRENT }
  GetCurrentConsoleFontEx(ch, FALSE, @ci);
  { ------------------------------------------------------------------------------------------------------------------------------------------------- SET NEW }
  ci.FontFamily   := FF_DONTCARE;
  ci.FaceName     := 'Consolas';
  ci.dwFontSize.X := 0;
  ci.dwFontSize.Y := AFontSize;
  ci.FontWeight   := FW_BOLD;
  SetCurrentConsoleFontEx(ch, FALSE, @ci);
end;
{ ----------------------------------------------------------------------------------------------------------------------------------- WRITE SPECIAL CHARACTER }
function WriteSp(Text: string): string;
(* WRAPPER OF WINAPI COMMAND SO THE CONSOLE CAN DISPLAY SPECIAL CHARACTERS | NECESSARY FOR DRAWINGS *)
var
  NumWritten: DWORD;
begin
  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(Text), Length(Text), NumWritten, nil);
  Writeln;  (* #13#10 *)
end;
{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
function Initialize: boolean;
var
  TMIG:  TMemIniFile;
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------------- INITIAIZE }
  Result:=True;
  SetConsoleOutputCP(CP_UTF8);
  { ---------------------------------------------------------------------------------------------------------------------------------------- SET CONSOLE FONT }
  if SetConsoleFont(15) = False then begin
    Result:=False;
    Exit;
  end;
  { ----------------------------------------------------------------------------------------------------------------------------------------- READ PARAMETERS }
  AppDir:=ExtractFileDir(ParamStr(0)) + '\';
  Error:=0;
  TMIG:=TMemIniFile.Create('');
  try
    Decode(AppDir + CfgName, LW_KEY, True, Error, TMIG);
    if Error = 0 then begin
      UpdDir:=TMIG.ReadString('APPLICATION', 'UPDATE_PATH', 'N/A');
      UpMode:=TMIG.ReadString('APPLICATION', 'UPDATE_MODE', 'N/A');
    end;
  finally
    TMIG.Free;
    if Error <> 0 then Result:=False;
  end;
end;
{ ----------------------------------------------------------------------------------------------------------------------------------------------- UPDATE FILE }
function CopyTo(OpenAfter: boolean): integer;
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------------- INITIAIZE }
  Result:=0;
  { ------------------------------------------------------------------------------------------------------- COPY (OVERWRITE) FROM NETWORK DRIVE OR LOCAL DISK }
  if UpperCase(UpMode) = 'FILE' then begin
    if not (FileExists(UpdDir + ExeName)) then begin
      Result:=101;
      ExpMsg:='source file does not exists.';
      Exit;
    end;
    if CopyFile(PChar(UpdDir + ExeName), PChar(AppDir + ExeName), True) = False then begin
      Result:=102;
      ExpMsg:='unknown error.';
      Exit;
    end;
    if OpenAfter then ShellExecute(0, 'Open', PChar(AppDir + ExeName), nil, nil, SW_SHOWNORMAL);
  end;
  { -------------------------------------------------------------------------------------------------------------------------- DOWNLOAD FROM THE WEB LOCATION }
  if UpperCase(UpMode) = 'HTTP' then begin
    // CODE HERE...
  end;
end;
{ --------------------------------------------------------- ! MAIN BLOCK ! ---------------------------------------------------------------------------------- }
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  if not (Initialize) then begin
    Writeln('Could not initialize. Program has ended.');
    Readln;
    Exit;
  end;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- MAIN VIEW }
  Writeln;
  WriteSp('┌──────────────────────────────────────────────┐ '                              );
  WriteSp('│ Welcome to the Unity Command Line.           │█'                              );
  WriteSp('├──────────────────────────────────────────────┤█'                              );
  WriteSp('│ Made for "Unity for Debt Management".        │█'                              );
  WriteSp('│ Author: Tomasz Kandula.                      │█'                              );
  WriteSp('├──────────────────────────────────────────────┤█'                              );
  WriteSp('│ Created: 2017-12-05                          │█'                              );
  WriteSp('└──────────────────────────────────────────────┘█'                              );
  WriteSp('  ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀'                              );
  Writeln;
  { ---------------------------------------------------------------------------------------------------------------------------------- NO PARAMETERS PROVIDED }
  if ParamCount < 1 then begin
    Writeln('Available parameters:                                                        ');
    Writeln('=====================                                                        ');
    Writeln;
    Writeln('  -update    : update Unity to the latest version and open when finished.    ');
    Writeln('               Use option -silent so Unity won''t be opened after updating.  ');
    Writeln;
    Writeln('Press <ENTER> key to exit.                                                   ');
    Readln;
    Exit;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------ INCORRECT PARAMETERS }
  if (ParamCount = 1) and (UpperCase(ParamStr(1)) <> '-UPDATE') then begin
    Writeln('Incorrect parameter(s). Program has ended.                                    ');
    Writeln;
    Exit;
  end;
  if (ParamCount = 2) and ((UpperCase(ParamStr(1)) <> '-UPDATE') or (UpperCase(ParamStr(2)) <> '-SILENT')) then begin
    Writeln('Incorrect parameter(s). Program has ended.                                   ');
    Writeln;
    Exit;
  end;
  { ----------------------------------------------------------------------------------------------------------------------------------------- UPDATE AND OPEN }
  if (ParamCount = 1) and (UpperCase(ParamStr(1)) = '-UPDATE') then Error:=CopyTo(True);
  { ----------------------------------------------------------------------------------------------------------------------------------------- UPDATE SILENTLY }
  if (ParamCount = 2) and
     (UpperCase(ParamStr(1)) = '-UPDATE') and
     (UpperCase(ParamStr(2)) = '-SILENT') then Error:=CopyTo(False);
  { -------------------------------------------------------------------------------------------------------------------------------------------- FINALIZATION }
  if Error = 0 then begin
    Writeln('Update executed successfully.                                                ');
    Writeln;
    Readln;
  end else begin
    Writeln('Could not update the file. Please contact IT support.                        ');
    Writeln('Error message: ', ExpMsg                                                      );
    Writeln;
    Readln;
  end;
end.
