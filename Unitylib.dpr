library Unitylib;


/// <remarks>
/// Important note about 'dll' memory managament: "sharemem" must be the first unit in your library's 'uses' clause & your project's (select
/// project-view source) 'uses' clause if your 'dll' exports any procedures or functions that pass strings as parameters or function results. This
/// applies to all strings passed to and from your 'dll--even' those that are nested in records and classes. Sharemem is the interface unit to
/// the 'borlndmm.dll' shared memory manager, which must be deployed along with your 'dll'. To avoid using 'borlndmm.dll', pass string information
/// using 'pchar' or 'shortstring' parameters.
/// </remarks>


uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    Vcl.Grids;


{$R *.res}


const
    HEAP_ZERO_MEMORY = $00000008;
    SID_REVISION     = 1;

type
    PTokenUser = ^TTokenUser;

    TTokenUser = packed record
        User: TSidAndAttributes;
    end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Helper method for "ObtainTextSid" method.
/// </summary>
/// <returns>Boolean</returns>

function ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL;
var
    psia:              PSIDIdentifierAuthority;
    dwSubAuthorities:  DWORD;
    dwSidRev:          DWORD;
    dwCounter:         DWORD;
    dwSidSize:         DWORD;
begin

    // Initialize
    Result  :=False;
    dwSidRev:=SID_REVISION;

    // Exit if invalid
    if not IsValidSid(Sid) then Exit;

    // Get data
    psia:=GetSidIdentifierAuthority(Sid);
    dwSubAuthorities:=GetSidSubAuthorityCount(Sid)^;
    dwSidSize:=(15 + 12 + (12 * dwSubAuthorities) + 1) * SizeOf(Char);

    // Set buffer and exit if insufficient
    if (dwBufferLen < dwSidSize) then
    begin
        dwBufferLen:=dwSidSize;
        SetLastError(ERROR_INSUFFICIENT_BUFFER);
        Exit;
    end;

    // String format
    StrFmt(pszSidText, 'S-%u-', [dwSidRev]);

    // Set data
    if (psia.Value[0] <> 0) or (psia.Value[1] <> 0) then
        StrFmt(pszSidText + StrLen(pszSidText), '0x%.2x%.2x%.2x%.2x%.2x%.2x',
	        [
                psia.Value[0],
                psia.Value[1],
                psia.Value[2],
                psia.Value[3],
                psia.Value[4],
                psia.Value[5]
            ])
    else
        StrFmt(pszSidText + StrLen(pszSidText), '%u',
            [
                DWORD(psia.Value[5])        +
                DWORD(psia.Value[4] shl 8)  +
                DWORD(psia.Value[3] shl 16) +
                DWORD(psia.Value[2] shl 24)
            ]);

    // Allocate memory for SID
    dwSidSize:=StrLen(pszSidText);
    for dwCounter:=0 to dwSubAuthorities - 1 do
    begin
        StrFmt(pszSidText + dwSidSize, '-%u', [GetSidSubAuthority(Sid, dwCounter)^]);
        dwSidSize := StrLen(pszSidText);
    end;

    // Set true if no error occured
    Result := True;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Helper method for "GetCurrentUserSid" method.
/// </summary>

function ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL;
var
    dwReturnLength:    DWORD;
    dwTokenUserLength: DWORD;
    tic:               TTokenInformationClass;
    ptu:               Pointer;
begin

    // Initialize
    Result           :=False;
    dwReturnLength   :=0;
    dwTokenUserLength:=0;
    tic              :=TokenUser;
    ptu              :=nil;

    // Get text-sid
    if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then
    begin
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin
            ptu := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwReturnLength);

            if ptu = nil
                then
                    Exit;

            dwTokenUserLength := dwReturnLength;
            dwReturnLength    := 0;

            if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength)
                then
                    Exit;

        end
            else
                Exit;
    end;

    // try to convert SID, exit if failed
    if not ConvertSid((PTokenUser(ptu).User).Sid, pszSid, dwBufferLen) then Exit;

    // Release from memory
    if not HeapFree(GetProcessHeap, 0, ptu) then Exit;

    // Set true if no error occured
    Result:=True;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Sid is a data structure of variable length that identifies user, group, and computer accounts.
/// every account on a network is issued a unique sid when the account is first created.
/// internal processes in windows refer to an account's sid rather than the account's user or group name.
/// </summary>
/// <returns>String with SID</returns>

function GetCurrentUserSid: string; stdcall;
var
    hAccessToken: THandle;
    bSuccess:     BOOL;
    dwBufferLen:  DWORD;
    szSid:        array[0..260] of Char;
begin

    Result:='';

    // Get token
    bSuccess:=OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);

    // Return error otherwise
    if not bSuccess then
        if GetLastError = ERROR_NO_TOKEN then
            bSuccess:=OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);

    // If succeed, then set proper SID
    if bSuccess then
    begin
        ZeroMemory(@szSid, SizeOf(szSid));
        dwBufferLen:=SizeOf(szSid);
        if ObtainTextSid(hAccessToken, szSid, dwBufferLen) theN Result:=szSid;
        CloseHandle(hAccessToken);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Get current version of running Microsoft Windows.
/// </summary>
/// <returns>String</returns>

function GetOSVer(CheckForOsName: boolean): string; stdcall;
begin

    result:='0';

    if not CheckForOsName then
    begin
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

    if CheckForOsName then
    begin
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


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Simple wrapper of string replace. Function in style of C printf. It requires "%s" variable in the given text string.
/// It automatically replaces break lines marked as "\n", "\r\n" and "\r".
/// </summary>
/// <param name="str">Text with "%s" variable</param>
/// <returns>String with given "%s" replaced by "str"</returns>

function Printf(str: string): string;
var
    Text: string;
begin
    Text:=StringReplace(Text, '%s',   str,    [rfReplaceAll]);
    Text:=StringReplace(Text, '\n',   #10,    [rfReplaceAll]);
    Text:=StringReplace(Text, '\r\n', #13#10, [rfReplaceAll]);
    Text:=StringReplace(Text, '\r',   #13,    [rfReplaceAll]);
    Result:=Text;
end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Put text into given logfile.
/// </summary>
/// <param name="FileName"></param>
/// <param name="Text"></param>

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
            // Do noting
        end;
    finally
        FL.Free;
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Merge Sort algorithm to be used with TStringGrid component directly (not arrays).
/// </summary>
/// <param name="Grid">TStringGrid component.</param>
/// <param name="Vals">Array of integer.</param>
/// <param name="sortcol">Integer, number of colunm to be sorted for given TStringGrid.</param>
/// <param name="datatype">Integer, data type in sorted column (string, integer, floar).</param>
/// <param name="ascending">Boolean. Set to "True" to get ascending sorting</param>

procedure MergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean); stdcall;

    /// <remarks>
    /// Temporary shared local array for integers.
    /// </remarks>

    var Avals:  array of integer;


    // ---------------------------------------------------------------------------------------------------------------------------------------------- NESTED //


    /// <summary>
    /// Helper nested method for comparision.
    /// </summary>

    function compare(val1, val2: string): integer;
    var
        int1:     int64;
        int2:     int64;
        errcode:  integer;
        float1:   extended;
        float2:   extended;
    begin
        case datatype of

            // String
            0:
                result:=ANSIComparetext(val1, val2);

            // Integers
            1:
            begin
                int1:=strtointdef(val1, 0);
                int2:=strtointdef(val2, 0);

                if (int1 > int2) then
                    result:= 1
                        else
                            if int1 < int2
                                then result:= -1
                                    else
                                        result:=0;
            end;

            // Floats (real numbers)
            2:
            begin
                val(val1, float1, errcode);

                if errcode <> 0 then
                    float1:=0;

                val(val2, float2, errcode);

                if errcode <> 0 then
                    float2:=0;

                if float1 > float2 then
                    result:= 1
                        else
                            if float1 < float2 then
                                result:= -1
                                    else
                                        result:=0;
            end;

            else result:=0;

        end;
    end;


    // ---------------------------------------------------------------------------------------------------------------------------------------------- NESTED //


    /// <summary>
    /// Heper nested merge method.
    /// </summary>

    procedure Merge(ALo, AMid, AHi: integer);
    var
        i:    integer;
        j:    integer;
        k:    integer;
        m:    integer;
        n:    integer;
    begin
        // Initialize
        i:=0;

        // Copy lower half of "vals" into temporary array "avals"
        SetLength(Avals, AMid - ALo + 1);
        for j:=ALo to AMid do begin
            AVals[i]:=Vals[j];
            inc(i);
        end;

        // Initialize
        i:=0;
        j:=AMid + 1;
        k:=ALo;

        /// <remarks>
        /// Compare upper half to copied version of the lower half and move
        /// the appropriate value (smallest for ascending, largest for descending) into
        /// the lower half positions, for equals use 'avals' to preserve original order.
        /// </remarks>

        // Execute moving
        while ((k < j) and (j <= AHi)) do
        begin
            with grid do n:=compare(Cells[sortcol, Vals[j]], Cells[sortcol, AVals[i]]);
            if ascending and (n >= 0) or ((not ascending) and (n <= 0)) then
            begin
                Vals[k]:=AVals[i];
                inc(i);
                inc(k);
            end
            else
            begin
                Vals[k]:=Vals[j];
                inc(k);
                inc(j);
            end;
        end;

        // Copy any remaning, unsorted elements
        for m:=k to j - 1 do begin
            Vals[m]:=AVals[i];
            inc(i);
        end;

    end;


    // ---------------------------------------------------------------------------------------------------------------------------------------------- NESTED //


    /// <summary>
    /// Recursively split the value into two pieces and merge them back together as we unwind the recursion.
    /// </summary>

    procedure PerformMergeSort(ALo, AHi:Integer);
    var
        AMid:Integer;
    begin
        if (ALo < AHi) then
        begin
            AMid:=(ALo + AHi) shr 1;
            PerformMergeSort(ALo, AMid);
            PerformMergeSort(AMid + 1, AHi);
            Merge(ALo, AMid, AHi);
        end;
    end;


// ---------------------------------------------------------------------------------------------------------------------------------------------- MAIN BLOCK //


begin
    PerformMergeSort(0, high(Vals));
end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Sorting algorithm (Quick Sort with pivot established between min and max value) with associated column,
/// that allows to put sorted values into its original positions. It is used to sort data for some other
/// computation that requires sorted input and unsorted table. In other words: sort, compute, put computed values
/// back into unsorted table.
/// </summary>
/// <param name="A">Array of double (sorting).</param>
/// <param name="L">Array of integer (original position)</param>
/// <param name="iLo">Integer, pivot low (min. value)</param>
/// <param name="iHi">Integer, pivot high (max. value)</param>
/// <param name="ASC">Boolean, set true to sort ascending</param>

procedure QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);

/// <remarks>
/// 'A' variable holds numerical data to be sorted. 'L' variable is associated column with original list position. The second associated column follows
/// 'a' column, but it is not sorted. It allows to assign sorted values back to original list position after computation is done. This is to be used when
/// sorting is necessary before applaying computation and after which we must put values back to its original positions.
/// </remarks>

var
  Lo:     integer;
  Hi:     integer;
  Pivot:  double;
  T1:     double;   // For sorting column
  T2:     integer;  // For associated column
begin

    Lo:=iLo;
    Hi:=iHi;
    Pivot:=A[(Lo + Hi) div 2];

    repeat

        // Ascending
        if ASC then begin
            while A[Lo] < Pivot do Inc(Lo);
            while A[Hi] > Pivot do Dec(Hi);
        end;

        // Descending
        if not ASC then begin
            while A[Lo] > Pivot do Inc(Lo);
            while A[Hi] < Pivot do Dec(Hi);
        end;

        // Moving positions
        if Lo <= Hi then begin
            T1:=A[Lo];
            T2:=L[Lo];

            // Sorting column
            A[Lo]:= A[Hi];
            A[Hi]:= T1;

            // Associated column
            L[Lo]:= L[Hi];
            L[Hi]:= T2;

            // Move next
            Inc(Lo);
            Dec(Hi);

        end;

    until Lo > Hi;

    if Hi > iLo then QuickSort(A, L, iLo, Hi, ASC);
    if Lo < iHi then QuickSort(A, L, Lo, iHi, ASC);

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Returns major, minor, release and build numbers.
/// </summary>
/// <param name="V1">Word</param>
/// <param name="V2">Word</param>
/// <param name="V3">Word</param>
/// <param name="V4">Word</param>

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
            if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
            begin
                VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
                with VerValue^ do
                begin
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


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


/// <summary>
/// Returns current application version.
/// </summary>
/// <returns>String with current number (format: major.minor.release.build)</returns>

function GetBuildInfoAsString: string;
var
    V1, V2, V3, V4: word;
begin
    GetBuildInfo(V1, V2, V3, V4);
    Result:=IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);
end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


exports
    Printf,
    LogText,
    GetOSVer,
    MergeSort,
    QuickSort,
    GetCurrentUserSid,
    GetBuildInfoAsString;

begin
end.

