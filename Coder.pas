{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Third-party libraries & own libraries                                                                                                     }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Coder;

interface

uses
  SysUtils, CRC32u, INIFiles, Grids, Classes;

procedure Encode(filename: string; key: integer; mode: boolean; var error: integer; var MINI: TMemIniFile);
procedure Decode(filename: string; key: integer; mode: boolean; var error: integer; var MINI: TMemIniFile);

implementation

{ --------------------------------------------------------------- ! ENCODE AND DECODE DATA ! ---------------------------------------------------------------- }
{ ### ERROR CODES ------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{  Error codes                  |  Description                                                                                                                }
{  -----------------------------|---------------------------------------------------------------------------------------------------------------------------- }
{  100: Disk read error         | Reported by Read on a typed file if you attempt to read past the end of the file.                                           }
{  101: Disk write error        | Reported by CloseFile, Write, WriteIn, or Flush if the disk becomes full.                                                   }
{  102: File not assigned       | Reported by Reset, Rewrite, Append, Rename, or Erase if the file variable                                                   }
{                               | has not been assigned a name through a call to Assign or AssignFile.                                                        }
{  103: File not open  	        | Reported by CloseFile, Read Write, Seek, Eof, FilePos, FileSize, Flush, BlockRead,                                          }
{                               | or BlockWrite if the file is not open.                                                                                      }
{  104: File not open for input | Reported by Read, Readln, Eof, Eoln, SeekEof, or SeekEoln on a text file                                                    }
{                               | if the file is not open for input.                                                                                          }
{  105: File not open for output| Reported by Write or Writeln on a text file if you do not generate a Console                                                }
{                               | application.                                                                                                                }
{  106: Invalid numeric format  |	Reported by Read or Readln if a numeric value read from a text file does not                                                }
{                               | conform to the proper numeric format.                                                                                       }
{                                                                                                                                                             }
{  ### EN(DE)CODING ----------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{  Encoding & decoding:                                                                                                                                       }
{                                                                                                                                                             }
{  1. TMI must be initialized before attept to encode or decode file. However, this is not needed for CRC32 check.                                            }
{  2. Enocde scheme:                                                                                                                                          }
{     Plain text INI data from Memory (TMemIniFile) --> Strting Encoded with CRC32 checksum (last 8 bytes) --> save to file.                                  }
{  3. Decode scheme:                                                                                                                                          }
{     a. Read from file to memory --> decode all --> read last 8 bytes & compute CRC32 for rest --> save to TMemIniFile (TMI)                                 }
{     b. Read from file to memory --> decode all --> read last 8 bytes & compute CRC32 for rest --> error code after CRC32 comparision                        }
{                                                                                                                                                             }
{ ### EOL --------------------------------------------------------------------------------------------------------------------------------------------------- }

(* MOVE BOTH ENCRYPTION AND DECRYPTION PROCEDURES TO THE SEPARATE DLL *)

{ ----------------------------------------------------------------------------------------------------------------------------------- ENCRYPT & COMPUTE CRC32 }
procedure Encode(filename: string; key: integer; mode: boolean; var error: integer; var MINI: TMemIniFile);
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
  Error:=0;
  rStream:=TMemoryStream.Create;
  wStream:=TMemoryStream.Create;
  hStream:=TStringList.Create;
  try
    { LOAD INI FROM MEMORY }
    if mode then MINI.GetStrings(hStream);
    { LOAD INI FROM FILE }
    (* if not mode then { NOT USED } *)

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
      buffer:=(buffer xor not (ord(key shr iCNT)));
      wStream.Write(buffer, 1);
    end;
    { SAVE TO FILE }
    wStream.Position:=0;
    wStream.SaveToFile(filename);

    (* FOR DEBUG: IntToStr(wStream.Size); *)

  finally;
    rStream.Free;
    wStream.Free;
    hStream.Free;
  end;
  if IOResult <> 0 then Error:=IOResult;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- DECRYPT & CHECK CRC32 }
procedure Decode(filename: string; key: integer; mode: boolean; var error: integer; var MINI: TMemIniFile);
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
  Error:=0;
  rStream:=TMemoryStream.Create;
  wStream:=TMemoryStream.Create;
  hString:=TStringList.Create;
  { PROCEED }
  try
    { LOAD FROM FILE }
    rStream.LoadFromFile(FileName);
    { DECODE ALL }
    for iCNT:=0 to rStream.Size - 1 do begin
      rStream.Read(buffer, 1);
      buffer:=(buffer xor not (ord(key shr iCNT)));
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

    if mode then begin
      hString.LoadFromStream(wStream);
      MINI.SetStrings(hString);
    end;
  finally

    (* FOR DEBUG: IntToStr(wStream.Size) *)

    { RELEASE }
    rStream.Free;
    wStream.Free;
    hString.Free;
  end;
  if IOResult <> 0 then Error:=IOResult
    else begin
      { CRC32 test result here: 0 - OK; 404 - NO MATCH }
      if sCRC =  IntToHex(vCRC, 8) then Error:=0;
      if sCRC <> IntToHex(vCRC, 8) then Error:=404;
    end;
end;

end.
