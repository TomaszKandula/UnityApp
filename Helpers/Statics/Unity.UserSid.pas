unit Unity.UserSid;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Winapi.Windows;


type

    TUserSid = class abstract
        const HEAP_ZERO_MEMORY = $00000008;
        const SID_REVISION     = 1;
        class function ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL; static;
        class function ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL; static;
        class function GetCurrentUserSid: string; static;
    end;


implementation


uses
    System.SysUtils,
    System.Variants;


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


end.

