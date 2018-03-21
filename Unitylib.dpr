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
{ ---------------------------------------------------------------- ! IMPORTANT NOTE ! ----------------------------------------------------------------------- }
{ IMPORTANT NOTE ABOUT 'DLL' MEMORY MANAGAMENT: SHAREMEM MUST BE THE FIRST UNIT IN YOUR LIBRARY'S 'USES' CLAUSE & YOUR PROJECT'S (SELECT                      }
{ PROJECT-VIEW SOURCE) 'USES' CLAUSE IF YOUR 'DLL' EXPORTS ANY PROCEDURES OR FUNCTIONS THAT PASS STRINGS AS PARAMETERS OR FUNCTION RESULTS. THIS              }
{ APPLIES TO ALL STRINGS PASSED TO AND FROM YOUR 'DLL--EVEN' THOSE THAT ARE NESTED IN RECORDS AND CLASSES. SHAREMEM IS THE INTERFACE UNIT TO                  }
{ THE 'BORLNDMM.DLL' SHARED MEMORY MANAGER, WHICH MUST BE DEPLOYED ALONG WITH YOUR 'DLL'. TO AVOID USING 'BORLNDMM.DLL', PASS STRING INFORMATION              }
{ USING 'PCHAR' OR 'SHORTSTRING' PARAMETERS.                                                                                                                  }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
library Unitylib;

uses
  Windows, SysUtils, Classes, Grids;

{$R *.res}

{ ---------------------------------------------------------------------------------------------------------------------------------------------- GET USER SID }

(*

  SID IS A DATA STRUCTURE OF VARIABLE LENGTH THAT IDENTIFIES USER, GROUP, AND COMPUTER ACCOUNTS.
  EVERY ACCOUNT ON A NETWORK IS ISSUED A UNIQUE SID WHEN THE ACCOUNT IS FIRST CREATED.
  INTERNAL PROCESSESES IN WINDOWS REFER TO AN ACCOUNT'S SID RATHER THAN THE ACCOUNT'S USER OR GROUP NAME.

*)

function GetCurrentUserSid: string; stdcall;

{ ------------------------------------------------------ ! COMMON CONSTANTS, VARIABLES AND TYPES ! ---------------------------------------------------------- }
const
  HEAP_ZERO_MEMORY = $00000008;
  SID_REVISION     = 1; { CURRENT REVISION LEVEL }
type
  PTokenUser = ^TTokenUser;
  TTokenUser = packed record
    User: TSidAndAttributes;
  end;

{ ----------------------------------------------------------------- ! NESTED METHODS ! ---------------------------------------------------------------------- }

{ ------------------------------------------------------------------ ! CONVERT SID ! ------------------------------------------------------------------------ }
function ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL;
var
  psia:              PSIDIdentifierAuthority;
  dwSubAuthorities:  DWORD;
  dwSidRev:          DWORD;
  dwCounter:         DWORD;
  dwSidSize:         DWORD;
begin
  { INITIALIZE }
  Result := False;
  dwSidRev := SID_REVISION;
  { EXIT IF NOT VALID }
  if not IsValidSid(Sid) then Exit;
  { GET DATA }
  psia:=GetSidIdentifierAuthority(Sid);
  dwSubAuthorities := GetSidSubAuthorityCount(Sid)^;
  dwSidSize:=(15 + 12 + (12 * dwSubAuthorities) + 1) * SizeOf(Char);
  { SET BUFFER AND EXIT IF NOT POSSIBLE }
  if (dwBufferLen < dwSidSize) then begin
    dwBufferLen:=dwSidSize;
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    Exit;
  end;
  { STRING FORMAT }
  StrFmt(pszSidText, 'S-%u-', [dwSidRev]);
  { SET DATA }
  if (psia.Value[0] <> 0) or (psia.Value[1] <> 0) then
    StrFmt(pszSidText + StrLen(pszSidText), '0x%.2x%.2x%.2x%.2x%.2x%.2x',
	  [psia.Value[0], psia.Value[1], psia.Value[2],
       psia.Value[3], psia.Value[4], psia.Value[5]])
  else
    StrFmt(pszSidText + StrLen(pszSidText), '%u',
      [DWORD(psia.Value[5]) +
       DWORD(psia.Value[4] shl 8) +
       DWORD(psia.Value[3] shl 16) +
       DWORD(psia.Value[2] shl 24)]);
  { ALLOCATE MEMORY FOR SID }
  dwSidSize:=StrLen(pszSidText);
  for dwCounter:=0 to dwSubAuthorities - 1 do begin
    StrFmt(pszSidText + dwSidSize, '-%u', [GetSidSubAuthority(Sid, dwCounter)^]);
    dwSidSize := StrLen(pszSidText);
  end;
  { SET TRUE IF NO ERROR OCCURED }
  Result := True;
end;

{ ------------------------------------------------------------------ ! GET TEXT-SID ! ----------------------------------------------------------------------- }
function ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL;
var
  dwReturnLength:    DWORD;
  dwTokenUserLength: DWORD;
  tic:               TTokenInformationClass;
  ptu:               Pointer;
begin
  { INITIALIZE }
  Result           :=False;
  dwReturnLength   :=0;
  dwTokenUserLength:=0;
  tic              :=TokenUser;
  ptu              :=nil;
  { GET TEXT-SID }
  if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
      ptu := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwReturnLength);
      if ptu = nil then Exit;
      dwTokenUserLength := dwReturnLength;
      dwReturnLength    := 0;
      if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then Exit;
    end
      else
        Exit;
  end;
  { TRY TO CONVERT SID, EXIT IF FAILED }
  if not ConvertSid((PTokenUser(ptu).User).Sid, pszSid, dwBufferLen) then Exit;
  { RELEASE FROM MEMORY }
  if not HeapFree(GetProcessHeap, 0, ptu) then Exit;
  { SET TRUE IF NO ERROR }
  Result:=True;
end;

{ ----------------------------------------------------------------- ! MAIN BLOCK ! -------------------------------------------------------------------------- }
var
  hAccessToken: THandle;
  bSuccess:     BOOL;
  dwBufferLen:  DWORD;
  szSid:        array[0..260] of Char;
begin
  { INITIALIZE }
  Result:='';
  { GET TOKEN }
  bSuccess:=OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  { RETURN ERROR IF NOT SUCCESSFULL }
  if not bSuccess then
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess:=OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  { IF SUCCEEDED, SET PROPER SID }
  if bSuccess then begin
    ZeroMemory(@szSid, SizeOf(szSid));
    dwBufferLen := SizeOf(szSid);
    if ObtainTextSid(hAccessToken, szSid, dwBufferLen) theN Result:=szSid;
    CloseHandle(hAccessToken);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ RETURN WINDOWS VERSION }
function GetOSVer(mode: integer): string; stdcall;
begin
  result:='0';
  if mode = 0 then begin
    case Win32MajorVersion of
      4:
        case Win32MinorVersion of
          0:  result:='40';     { Windows 95    }
          10: result:='41';     { Windows 98    }
          90: result:='49';     { Windows ME    }
        end;
      5:
        case Win32MinorVersion of
          0: result:='50';      { Windows 2000  }
          1: result:='51';      { Windows XP    }
        end;
      6:
        case Win32MinorVersion of
          0: result:='60';      { Windows Vista }
          1: result:='61';      { Windows 7     }
          2: result:='62';      { Windows 8     }
          3: result:='63';      { Windows 8.1   }
        end;
      10:
        case Win32MinorVersion of
          0: result:='100';     { Windows 10    }
        end;
    end;
  end;
  if mode = 1 then begin
    case Win32MajorVersion of
      4:
        case Win32MinorVersion of
          0:  result:='Windows 95';
          10: result:='Windows 98';
          90: result:='Windows ME';
        end;
      5:
        case Win32MinorVersion of
          0: result:='Windows 2000';
          1: result:='Windows XP';
        end;
      6:
        case Win32MinorVersion of
          0: result:='Windows Vista';
          1: result:='Windows 7';
          2: result:='Windows 8';
          3: result:='Windows 8.1';
         end;
      10:
        case Win32MinorVersion of
          0: result:='Windows 10';
        end;
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- TEXT INTO LOGFILE }
procedure LogText(FileName: string; Text: string); stdcall;
var
  FL:         TFileStream;
  cDateTime:  TDateTime;
begin
  cDateTime:=Now;
  text:='#' + DateToStr(cDateTime) + ' (' + TimeToStr(cDateTime) + '): ' + Text + #13#10;
  FL:=TFileStream.Create(FileName, fmOpenWrite);
  try
    try
      FL.Position:=FL.Size;
      FL.WriteBuffer(UTF8String(Text)[1], Length(UTF8String(Text)));
    except
      { DO NOTHING }
    end;
  finally
    FL.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------- MERGE SORT MAIN PROCEDURE }
procedure MergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall;

{ -------------------------------------------------------------- ! NESTED METHODS ! ------------------------------------------------------------------------- }
var
  { TEMPORARY ARRAY FOR INTEGERS }
  AVals:  array of integer;

{ ---------------------------------------------------------------- ! COMPARISION ! -------------------------------------------------------------------------- }
function compare(val1, val2:string): integer;
var
  int1:     int64;
  int2:     int64;
  errcode:  integer;
  float1:   extended;
  float2:   extended;
begin
  case datatype of
    { STRINGS }
    0: result:=ANSIComparetext(val1, val2);
    { INTEGERS }
    1: begin
         int1:=strtointdef(val1, 0);
         int2:=strtointdef(val2, 0);
         if (int1 > int2) then result:= 1
           else
             if int1 < int2 then result:= - 1
               else
                 result:=0;
       end;
    { REAL NUMBERS }
    2: begin
         val(val1, float1, errcode);
         if errcode <> 0 then float1:=0;
         val(val2, float2, errcode);
         if errcode <> 0 then float2:=0;
         if float1 > float2 then result:= 1
           else
             if float1<float2 then result:= - 1
               else result:=0;
       end;
    else result:=0;
  end;
end;

{ ------------------------------------------------------------- ! MERGE PROCEDURE ! ------------------------------------------------------------------------- }
procedure Merge(ALo, AMid, AHi: integer);
var
  i:    integer;
  j:    integer;
  k:    integer;
  m:    integer;
  n:    integer;
begin
  { INITIALIZE }
  i:=0;
  { PERFORM }
  SetLength(Avals, AMid - ALo + 1);
  for j:=ALo to AMid do begin
    { COPY LOWER HALF OF 'VALS' INTO TEMPORARY ARRAY 'AVALS' }
    AVals[i]:=Vals[j];
    inc(i);
  end;
  { INITIALIZE }
  i:=0;
  j:=AMid + 1;
  k:=ALo;
  { PERFORM }
  {NOTE: COMPARE UPPER HALF TO COPIED VERSION OF THE LOWER HALF AND MOVE THE APPROPRIATE VALUE (SMALLEST FOR ASCENDING, LARGEST FOR DESCENDING) INTO }
  {      THE LOWER HALF POSITIONS, FOR EQUALS USE 'AVALS' TO PRESERVE ORIGINAL ORDER                                                                 }
  while ((k < j) and (j <= AHi)) do begin
    with grid do n:=compare(Cells[sortcol, Vals[j]], Cells[sortcol, AVals[i]]);
    if ascending and (n >= 0) or ((not ascending) and (n <= 0)) then begin
      Vals[k]:=AVals[i];
      inc(i);inc(k);
    end
      else begin
        Vals[k]:=Vals[j];
        inc(k);inc(j);
      end;
    end;
    { COPY ANY REMANING, UNSORTED ELEMENTS }
    for m:=k to j - 1 do begin
      Vals[m]:=AVals[i];
      inc(i);
    end;
  end;

{ ------------------------------------------------------------ ! PERFORM MERGE SORT ! ----------------------------------------------------------------------- }
procedure PerformMergeSort(ALo, AHi:Integer);
(* RECURSIVELY SPLIT THE VALUE INTO TWO PIECES AND MERGE THEM BACK TOGETHER AS WE UNWIND THE RECURSION *)
var
  AMid:Integer;
begin
  if (ALo < AHi) then begin
    AMid:=(ALo + AHi) shr 1;
    PerformMergeSort(ALo, AMid);
    PerformMergeSort(AMid + 1, AHi);
    Merge(ALo, AMid, AHi);
  end;
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  PerformMergeSort(0, high(vals));
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ QUICK SORT }
procedure QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
{ 'A' VARIABLE HOLDS NUMERICAL DATA TO BE SORTED. 'L' VARIABLE IS ASSOCIATED COLUMN WITH ORIGINAL LIST POSITION. THE SECOND ASSOCIATED COLUMN FOLLOWS   }
{ 'A' COLUMN, BUT IT IS NOT SORTED. IT ALLOWS TO ASSIGN SORTED VALUES BACK TO ORIGINAL LIST POSITION AFTER COMPUTATION IS DONE. THIS IS TO BE USED WHEN }
{ SORTING IS NECESSARY BEFORE APPLAYING COMPUTATION AND AFTER WHICH WE MUST PUT VALUES BACK TO ITS ORIGINAL POSITIONS.                                  }
var
  Lo:     integer;
  Hi:     integer;
  Pivot:  double;
  T1:     double;   { FOR SORTING COLUMN    }
  T2:     integer;  { FOR ASSOCIATED COLUMN }
begin
   Lo:=iLo;
   Hi:=iHi;
   Pivot:=A[(Lo + Hi) div 2];  { ACCESS VIOLATION AFTER USING IN THE PROGRAM, IT WORKS IF USED AS LOCAL METHOD }
   repeat
     { ASCENDING }
     if ASC then begin
       while A[Lo] < Pivot do Inc(Lo);
       while A[Hi] > Pivot do Dec(Hi);
     end;
     { DESCENDING }
     if not ASC then begin
       while A[Lo] > Pivot do Inc(Lo);
       while A[Hi] < Pivot do Dec(Hi);
     end;
     { MOVING POSITIONS }
     if Lo <= Hi then begin
       T1:=A[Lo];
       T2:=L[Lo];
       { SORTING COLUMN }
       A[Lo]:= A[Hi];
       A[Hi]:= T1;
       { ASSOCIATED COLUMN }
       L[Lo]:= L[Hi];
       L[Hi]:= T2;
       { MOVE NEXT }
       Inc(Lo);
       Dec(Hi);
     end;
   until Lo > Hi;
   if Hi > iLo then QuickSort(A, L, iLo, Hi, ASC);
   if Lo < iHi then QuickSort(A, L, Lo, iHi, ASC);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- GET BUILD INFO }
procedure GetBuildInfo(var V1, V2, V3, V4: word);
var
  VerInfoSize:   DWORD;
  VerValueSize:  DWORD;
  Dummy:         DWORD;
  VerInfo:       Pointer;
  VerValue:      PVSFixedFileInfo;
begin
  VerInfoSize:=GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize > 0 then begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then begin
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        with VerValue^ do begin
          V1:=dwFileVersionMS shr 16;
          V2:=dwFileVersionMS and $FFFF;
          V3:=dwFileVersionLS shr 16;
          V4:=dwFileVersionLS and $FFFF;
        end;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- RETURN APP VERSION }
function GetBuildInfoAsString: string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- EXPORTS }

exports
  LogText,
  GetOSVer,
  MergeSort,
  QuickSort,  (* DO NOT USE, ACCESS VIOLATION ISSUE, TO BE FIXED *)
  GetCurrentUserSid,
  GetBuildInfoAsString;

begin
  { DO NOTHING }
end.

