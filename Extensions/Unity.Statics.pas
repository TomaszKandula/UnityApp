unit Unity.Statics;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    Vcl.Grids;


type


    TRestAuth = class abstract
    public
        const apiUserName       = 'your_login';
        const apiPassword       = 'your_password';
        const restApiBaseUrl    = 'your_endpoint';
        const restAccept        = 'application/json, text/plain; q=0.9, text/html;q=0.8,';
        const restAcceptCharset = 'UTF-8, *;q=0.8';
        const restContentType   = 'application/json';
        const restEncoding      = 'UTF-8';
        const restUserAgent     = 'Cheers RESTClient/1.0';
    end;


    TMessaging = class abstract

        type TWParams = class abstract
        {WParams}
            const AwaitForm        = 1;
            const StatusBar        = 2;
            const MessageInfo      = 3;
            const MessageWarn      = 4;
            const MessageError     = 5;
            const MessageQuestion1 = 6;
            const MessageQuestion2 = 7;
            const ConnectionOk     = 8;
            const ConnectionError  = 9;
            const MailerReportItem = 10;
        end;

        type TAwaitForm = class abstract
        {LParams}
            const Show = 1;
            const Hide = 2;
        end;

    end;


    TSorting = class abstract

        type TDataType = class abstract
            const TString  = 0;
            const TInteger = 1;
            const TFloat   = 2;
        end;

        type TMode = class abstract
            const Ranges   = 0;
            const FollowUp = 1;
            const Total    = 2;
            const Overdue  = 3;
        end;

        class procedure QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean); static;
        class procedure MergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean); static;

    end;


    TCommon = class abstract
    private
        class procedure GetBuildInfo(var V1, V2, V3, V4: word); static;
    public
        type  TFiles                    = (AppConfig, LicData);
        type  TTimers                   = (TurnedOn, TurnedOff);
        type  TMessage                  = (Info, Warn, Error, Question1, Question2);
        const SelectionColor: integer   = $00F2E4D7; // rgb D7E4F2 => bgr F2E4D7
        const FontColor:      integer   = $006433C9; // rgb C93364 => bgr 6433C9
        const AltColor:       integer   = $00FFDBB7; // rgb B7DBFF => bgr FFDBB7
        const DecryptKey:     integer   = 429496;
        const AppCaption:     string    = 'Unity';
        const UnityReader:    string    = 'UnityReader.exe';
        const LicenceFile:    string    = 'Unity.lic';
        const GridImgFile:    string    = 'Unity.img';
        const ReleaseFile:    string    = 'Release.zip';
        const LayoutPak:      string    = 'Layouts.zip';
        const ManifestFile:   string    = 'Unity.manifest';
        const CurrentMutex:   PWideChar = 'UNITY_10255';
        const ConfigFile:     string    = 'Config.cfg';
        class function  GetBuildInfoAsString: string; static;
        class function  GetOSVer(CheckForOsName: boolean): string; static;
        class procedure LogText(FileName: string; Text: string); static;
    end;


    TSplashScreen = class abstract
        const DelayStd       = 10;
        const DelayErr       = 750;
        const AllTasks       = 21;
        const SettingUp      = 'getting settings..., please wait.';
        const Connecting     = 'connecting to MS SQL Server..., please wait.';
        const GettingUsers   = 'getting user access details..., please wait.';
        const GettingAgeing  = 'loading default age view..., please wait.';
        const GettingGeneral = 'loading General Tables..., please wait.';
        const MappingTables  = 'loading Map Tables..., please wait.';
        const Finishing      = 'finishing..., please wait.';
    end;


    TLyncLib = class abstract
        const LyncControls  = 'Microsoft.Lync.Controls.dll';
        const LyncFramework = 'Microsoft.Lync.Controls.Framework.dll';
        const LyncModel     = 'Microsoft.Lync.Model.dll';
        const LyncUtils     = 'Microsoft.Lync.Utilities.dll';
        const OfficeUc      = 'Microsoft.Office.Uc.dll';
        const LyncCall      = 'LyncCall.exe';
    end;


    TStatusBar = class abstract
        const Ready       = 'Ready';
        const Processing  = 'Processing...';
        const ExportXLS   = 'Exporting to Excel...';
        const ExportCSV   = 'Exporting to CSV file...';
        const ImportCSV   = 'Importing from CSV file...';
        const Generating  = 'Generating age view...';
        const Downloading = 'Downloading Open Items...';
        const Loading     = 'Loading Aging Report...';
        const SQLupdate   = 'Sending to SQL Server...';
    end;


    TChars = class abstract
        const CrLf:        string = #13#10;
        const Cr:          char   = #13;
        const Lf:          char   = #10;
        const Backspace:   char   = #8;
        const Tab:         char   = #9;
        const Esc:         char   = #27;
        const Space:       char   = #32;
        const Quote:       char   = #39;
        const Comma:       char   = #44;
        const Point:       char   = #46;
        const SingleQuote: string = '''';
        const DoubleQuote: string = '''''';
    end;


    TDelimiters = class abstract
        const Semicolon = ';';
        const Comma     = ',';
        const Pipe      = '|';
    end;


    TRiskClass = class abstract
        const A = '0,80';
        const B = '0,15';
        const C = '0,05';
    end;


    TDateTimeFormats = class abstract
        const TimeFormat     = 'hh:mm:ss';
        const DateFormat     = 'YYYY-MM-DD';
        const DateTimeFormat = 'YYYY-MM-DD hh:mm:ss';
        const NullDate: TDateTime = 0;
    end;


    TUnknown = class abstract
        const Null:       string = 'NULL';
        const Unassigned: string = 'Unassigned item.';
        const NA:         string = 'N/A';
        const NotFound:   string = 'Not found!';
    end;


    TUserSid = class abstract
        const HEAP_ZERO_MEMORY = $00000008;
        const SID_REVISION     = 1;
        class function ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL; static;
        class function ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL; static;
        class function GetCurrentUserSid: string; static;
    end;


    TNCSI = class abstract
        const HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
        const HTTPREQUEST_SETCREDENTIALS_FOR_PROXY = 1;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW = 0;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM = 1;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH = 2;
        const MAX_CHECK_ATTEMPTS = 6;
        const ncsiWww:  string = 'http://www.msftncsi.com/';
        const ncsiFile: string = 'ncsi.txt';
        const ncsiGet:  string = 'GET';
        const ncsiHead: string = 'HEAD';
    end;


    // -----------------------------------
    // LEGACY SECTION - TO BE REMOVED
    // -----------------------------------


    TFiltering = class abstract
    {To be removed after new filtering is made}
        type TColumns = (
            Inf7,
            Inf4,
            CoCode,
            Agent,
            Division,
            Follow,
            Group3,
            Free1,
            Free2,
            Free3,
            SalesResponsible,
            PersonResponsible,
            CustomerGroup,
            AccountType
        );
    end;

    TAdoDb = class abstract
        type  TFilters    = (adFilterNone, adFilterPendingRecords, adFilterAffectedRecords, adFilterFetchedRecords, adFilterConflictingRecords);
        const dbOLEDB     = 'OLEDB';
        const dbODBC      = 'ODBC';
        const ERR_MESSAGE = 'Cannot connect with Microsoft SQL Server. Please re-check your server settings or contact IT support.';
        const ERR_LOGTEXT = 'ADO connection error. Exception thrown: ';
    end;

    TUserAccess = class abstract
        type TTypes = (AccessLevel, AccessMode, UserKeyId);
        const AccessFull  = 'FULL';
        const AccessBasic = 'BASIC';
        const Admin       = 'AD';
        const ReadWrite   = 'RW';
        const ReadOnly    = 'RO';
    end;

    TQms = class abstract
        const Open     = 'OPEN';
        const Pending  = 'PENDING';
        const Rejected = 'REJECTED';
        const Solved   = 'SOLVED';
    end;

    TSql = class abstract
        const INSERT       = ' INSERT INTO ';
        const VAL          = ' VALUES ';
        const SELECT       = ' SELECT ';
        const SELECT_DIS   = ' SELECT DISTINCT ';
        const DISTINCT     = ' DISTINCT ';
        const _UPDATE      = ' UPDATE ';
        const DELETE_FROM  = ' DELETE FROM ';
        const _SET         = ' SET ';
        const _BEGIN       = ' BEGIN ';
        const _END         = ' END ';
        const UNION        = ' UNION ALL ';
        const EQUAL        = ' = ';
        const FROM         = ' FROM ';
        const ALL          = ' * ';
        const WHERE        = ' WHERE ';
        const LEFT_JOIN    = ' LEFT JOIN ';
        const RIGHT_JOIN   = ' RIGHT JOIN ';
        const INNER_JOIN   = ' INNER JOIN ';
        const OUTER_JOIN   = ' OUTER JOIN ';
        const _ON          = ' ON ';
        const _OR          = ' OR ';
        const ORDER        = ' ORDER BY ';
        const ASC          = ' ASC ';
        const DESC         = ' DESC ';
        const _AND         = ' AND ';
        const _AS          = ' AS ';
        const MAX          = ' MAX ';
        const SUM          = ' SUM ';
        const LIKE         = ' LIKE ';
        const EXECUTE      = ' EXEC ';
        const MATCHCASE    = ' COLLATE SQL_Latin1_General_CP1_CS_AS ';
    end;


implementation


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


class function TUserSid.ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL;
begin

    Result:=False;

    var dwSidRev: DWORD:=SID_REVISION;
    if not IsValidSid(Sid) then Exit;

    var psia: PSIDIdentifierAuthority:=GetSidIdentifierAuthority(Sid);
    var dwSubAuthorities: DWORD:=GetSidSubAuthorityCount(Sid)^;
    var dwSidSize: DWORD:=(15 + 12 + (12 * dwSubAuthorities) + 1) * SizeOf(Char);

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
    for var dwCounter: DWORD:=0 to dwSubAuthorities - 1 do
    begin
        StrFmt(pszSidText + dwSidSize, '-%u', [GetSidSubAuthority(Sid, dwCounter)^]);
        dwSidSize := StrLen(pszSidText);
    end;

    // Set true if no error occured
    Result:=True;

end;


class function TUserSid.ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL;
begin

    Result:=False;
    var dwReturnLength: DWORD:=0;
    var dwTokenUserLength: DWORD:=0;
    var tic: TTokenInformationClass:=TokenUser;
    var ptu: Pointer:=nil;

    // Get text-sid
    if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then
    begin
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin
            ptu:=HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwReturnLength);

            if ptu = nil then Exit;

            dwTokenUserLength:=dwReturnLength;
            dwReturnLength:=0;

            if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then Exit;

        end else Exit;
    end;

    // Try to convert SID, exit if failed
    if not ConvertSid((PTokenUser(ptu).User).Sid, pszSid, dwBufferLen) then Exit;

    // Release from memory
    if not HeapFree(GetProcessHeap, 0, ptu) then Exit;

    // Set true if no error occured
    Result:=True;

end;


class function TUserSid.GetCurrentUserSid: string;
type TSidArray = array[0..260] of Char;
begin

    var hAccessToken: THandle;
    var bSuccess:     BOOL;
    var dwBufferLen:  DWORD;
    var szSid:        TSidArray;

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


class procedure TSorting.QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
begin

    /// <remarks>
    /// 'A' variable holds numerical data to be sorted. 'L' variable is associated column with original list position. The second associated column follows
    /// 'A' column, but it is not sorted. It allows to assign sorted values back to original list position after computation is done. This is to be used when
    /// sorting is necessary before applaying computation and after which we must put values back to its original positions.
    /// </remarks>

    var Lo:    integer;
    var Hi:    integer;
    var Pivot: double;
    var T1:    double;  {For sorting column}
    var T2:    integer; {For associated column}

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


class procedure TSorting.MergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol, datatype: integer; ascending: boolean);

    /// <remarks>
    /// Temporary shared local array for integers.
    /// </remarks>

    var Avals:  array of integer;

    /// <summary>
    /// Helper nested method for comparision.
    /// </summary>

    function compare(val1, val2: string): integer;
    begin

        case datatype of

            // String
            0: result:=ANSIComparetext(val1, val2);

            // Integers
            1:
            begin

                var int1: int64:=strtointdef(val1, 0);
                var int2: int64:=strtointdef(val2, 0);

                if (int1 > int2) then result:= 1
                else if int1 < int2 then result:= -1 else result:=0;

            end;

            // Floats (real numbers)
            2:
            begin

                var errcode: integer;
                var float1:  extended;
                var float2:  extended;

                val(val1, float1, errcode);

                if errcode <> 0 then float1:=0;

                val(val2, float2, errcode);

                if errcode <> 0 then float2:=0;

                if float1 > float2 then result:= 1
                else if float1 < float2 then result:= -1 else result:=0;

            end;

            else result:=0;

        end;
    end;

    /// <summary>
    /// Heper nested merge method.
    /// </summary>

    procedure Merge(ALo, AMid, AHi: integer);
    begin

        var j: integer;
        var k: integer;
        var m: integer;
        var n: integer;

        var i: integer:=0;

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

    /// <summary>
    /// Recursively split the value into two pieces and merge them back together as we unwind the recursion.
    /// </summary>

    procedure PerformMergeSort(ALo, AHi:Integer);
    begin

        var AMid:Integer;

        if (ALo < AHi) then
        begin
            AMid:=(ALo + AHi) shr 1;
            PerformMergeSort(ALo, AMid);
            PerformMergeSort(AMid + 1, AHi);
            Merge(ALo, AMid, AHi);
        end;

    end;

begin
    PerformMergeSort(0, high(Vals));
end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


class procedure TCommon.GetBuildInfo(var V1, V2, V3, V4: word);
begin

    var VerInfoSize:   DWORD;
    var VerValueSize:  DWORD;
    var Dummy:         DWORD;
    var VerInfo:       Pointer;
    var VerValue:      PVSFixedFileInfo;

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


class function TCommon.GetBuildInfoAsString: string;
begin

    var V1: word;
    var V2: word;
    var V3: word;
    var V4: word;

    GetBuildInfo(V1, V2, V3, V4);
    Result:=IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);

end;


class function TCommon.GetOSVer(CheckForOsName: boolean): string;
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


class procedure TCommon.LogText(FileName: string; Text: string);
begin

    var cDateTime: TDateTime:=Now;
    text:='#' + DateToStr(cDateTime) + ' (' + TimeToStr(cDateTime) + '): ' + Text + #13#10;

    var FL: TFileStream:=TFileStream.Create(FileName, fmOpenWrite);
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


end.

