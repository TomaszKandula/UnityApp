unit Unity.Helpers;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Classes,
    System.SysUtils,
    System.IOUtils,
    Winapi.Messages,
    Winapi.Windows,
    Winapi.ShellAPI,
    Vcl.ExtCtrls,
    Vcl.Forms,
    Vcl.Menus,
    Vcl.Grids,
    Unity.Enums,
    Unity.Grid;


type


    /// <summary>
    /// Define custom type for anonymous method to be passed as parameter.
    /// </summary>
    TInputMethod = reference to procedure;


    /// <summary>
    /// Generic class exposing helper methods for basic operations on arrays.
    /// </summary>
    TArrayUtils<T> = class abstract
    public
        class function Contains(const x: T; const anArray: array of T): boolean; static;
        class procedure Copy(const FromArray: TArray<T>; var ToArray: TArray<T>); static;
        class procedure Move(var FromArray: TArray<T>; var ToArray: TArray<T>); static;
        class procedure MoveToList(const FromArray: TArray<T>; var TargetList: TList<T>); static;
    end;


    /// <summary>
    ///
    /// </summary>
    TSidArray = array[0..260] of Char;


    /// <summary>
    /// Holds various helper methods, shorthands and wrappers.
    /// </summary>
    THelpers = class abstract
    strict private
        const HEAP_ZERO_MEMORY = $00000008;
        const SID_REVISION = 1;
        class function ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL; static;
        class function ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL; static;
        class procedure GetBuildInfo(var V1, V2, V3, V4: word); static;
    public
        const WM_GETINFO = WM_USER + 120;
        const WM_EXTINFO = WM_APP  + 150;
        class procedure ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod); static;
        class procedure LoadImageFromStream(var Image: TImage; const FileName: string); static;
        class procedure TurnRowHighlight(var Grid: TStringGrid; var MenuItem: TMenuItem); static;
        class function WndCall(WinForm: TForm; Mode: TWindowState): integer; static;
        class function MsgCall(WndType: TAppMessage; WndText: string): integer; static;
        class function OleGetStr(RecordsetField: variant): string; static;
        class function CDate(StrDate: string): TDate; static;
        class function Explode(Text: string; SourceDelim: char): string; static;
        class function Implode(Text: TStrings; TargetDelim: char; ItemsHaveQuotes: boolean = False): string; static;
        class function ListToString(Input: TList<string>; TargetDelim: string): string; static;
        class function ArrayToString(Input: TArray<string>; TargetDelim: string): string; static;
        class function ExportToCSV(SourceArray: TArray<TArray<string>>; FileName: string = ''): TStringList; static;
        class function IsVoType(VoType: string): boolean; static;
        class function ShowReport(ReportNumber: cardinal; CurrentForm: TForm): cardinal; static;
        class procedure ReturnCoCodesList(var SourceGrid: TStringGrid; const SourceCol: integer;
            var TargetList: TStringList; HasHeader: boolean = False; Prefix: string = ''); static;
        class function CoConvert(CoNumber: string): string; static;
        class function GetSourceDBName(CoCode: string; Prefix: string): string; static;
        class function GetBuildInfoAsString: string; static;
        class function GetOSVer(CheckForOsName: boolean): string; static;
        class function Unpack(ItemID: integer; FileName: string; ShouldFileStay: boolean; var LastErrorMsg: string): boolean; static;
        class function UnzippLayouts(FileName: string; DestDir: string): boolean; static;
        class function LoadFileToStr(const FileName: TFileName): string; static;
        class function GetCurrentUserSid: string; static;
        class procedure RemoveAllInFolder(const Path: string; const Pattern: string); static;
    end;


implementation


uses
    System.StrUtils,
    System.Variants,
    System.Zip,
    Vcl.Graphics,
    DbModel{Legacy},
    Unity.DateTimeFormats,
    Unity.Unknown,
    Unity.Chars,
    Unity.Common,
    Unity.Settings;


class function TArrayUtils<T>.Contains(const x: T; const anArray: array of T): boolean;
begin

    // Usage: TArrayUtils<integer>.Contains(3, [1,2,3]).
    var y: T;
    var lComparer: IEqualityComparer<T>:=TEqualityComparer<T>.Default;

    for y in anArray do
        if lComparer.Equals(x, y) then
            Exit(True);

    Exit(False);

end;


class procedure TArrayUtils<T>.Copy(const FromArray: TArray<T>; var ToArray: TArray<T>);
begin
    ToArray:=FromArray;
    SetLength(ToArray, Length(FromArray));
end;


class procedure TArrayUtils<T>.Move(var FromArray: TArray<T>; var ToArray: TArray<T>);
begin
    ToArray:=FromArray;
    SetLength(ToArray, Length(FromArray));
    FromArray:=nil;
end;


class procedure TArrayUtils<T>.MoveToList(const FromArray: TArray<T>; var TargetList: TList<T>);
begin
    for var iCNT:=0 to Length(FromArray) - 1 do
        TargetList.Add(FromArray[iCNT]);
end;


class function THelpers.ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL;
begin

    Result:=False;

    var dwSidRev: DWORD:=SID_REVISION;
    if not IsValidSid(Sid) then Exit;

    var psia: PSIDIdentifierAuthority:=GetSidIdentifierAuthority(Sid);
    var dwSubAuthorities: DWORD:=GetSidSubAuthorityCount(Sid)^;
    var dwSidSize: DWORD:=(15 + 12 + (12 * dwSubAuthorities) + 1) * SizeOf(Char);

    if (dwBufferLen < dwSidSize) then
    begin
        dwBufferLen:=dwSidSize;
        SetLastError(ERROR_INSUFFICIENT_BUFFER);
        Exit();
    end;

    StrFmt(pszSidText, 'S-%u-', [dwSidRev]);

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

    dwSidSize:=StrLen(pszSidText);
    for var dwCounter: DWORD:=0 to dwSubAuthorities - 1 do
    begin
        StrFmt(pszSidText + dwSidSize, '-%u', [GetSidSubAuthority(Sid, dwCounter)^]);
        dwSidSize := StrLen(pszSidText);
    end;

    Result:=True;

end;


class function THelpers.ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL;
begin

    Result:=False;
    var dwReturnLength: DWORD:=0;
    var dwTokenUserLength: DWORD:=0;
    var tic: TTokenInformationClass:=TokenUser;
    var ptu: Pointer:=nil;

    if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then
    begin

        if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin

            ptu:=HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwReturnLength);

            if ptu = nil then Exit();
            dwTokenUserLength:=dwReturnLength;
            dwReturnLength:=0;

            if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength, dwReturnLength) then Exit();

        end
        else
        begin
            Exit();
        end;

    end;

    // Try to convert SID, exit if failed
    if not ConvertSid((PTokenUser(ptu).User).Sid, pszSid, dwBufferLen) then Exit();
    if not HeapFree(GetProcessHeap, 0, ptu) then Exit();

    Result:=True;

end;


class procedure THelpers.GetBuildInfo(var V1, V2, V3, V4: word);
begin

    var VerInfoSize:   DWORD;
    var VerValueSize:  DWORD;
    var Dummy:         DWORD;
    var VerInfo:       Pointer;
    var VerValue:      PVSFixedFileInfo;

    VerInfoSize:=GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);

    if VerInfoSize > 0 then
    begin

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


class procedure THelpers.ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod);
begin

    TThread.CreateAnonymousThread(procedure
    begin

        Sleep(Delay);

        TThread.Synchronize(nil, procedure
        begin

            AnonymousMethod;

        end);

    end).Start();

end;


class procedure THelpers.LoadImageFromStream(var Image: TImage; const FileName: string);
begin

    var FS:=TFileStream.Create(FileName, fmOpenRead);
    FS.Position:=0;

    var WIC:=TWICImage.Create();

    try
        WIC.LoadFromStream(FS);
        Image.Picture.Assign(WIC);
    finally
        WIC.Free();
        FS.Free();
    end;

end;


class procedure THelpers.TurnRowHighlight(var Grid: TStringGrid; var MenuItem: TMenuItem);
begin

    if MenuItem.Checked then
    begin
        Grid.Options:=Grid.Options - [goRowSelect];
        Grid.Options:=Grid.Options + [goRangeSelect];
        MenuItem.Checked:=False;
    end
    else
    begin
        Grid.Options:=Grid.Options + [goRowSelect];
        Grid.Options:=Grid.Options - [goRangeSelect];
        MenuItem.Checked:=True;
    end;

end;


class function THelpers.WndCall(WinForm: TForm; Mode: TWindowState): integer;
begin

    Result:=0;

    WinForm.PopupMode  :=pmAuto;
    WinForm.PopupParent:=WinForm;

    case Mode of
        TWindowState.Modal: Result:=WinForm.ShowModal;
        TWindowState.Modeless: WinForm.Show;
    end;

end;


class function THelpers.MsgCall(WndType: TAppMessage; WndText: string): integer;
begin

    Result:=0;
    if WndText = '' then Exit();

    case WndType of
        TAppMessage.Info:      Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OK       + MB_ICONINFORMATION);
        TAppMessage.Warn:      Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OK       + MB_ICONWARNING);
        TAppMessage.Error:     Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OK       + MB_ICONERROR);
        TAppMessage.Question1: Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OKCANCEL + MB_ICONQUESTION);
        TAppMessage.Question2: Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_YESNO    + MB_ICONQUESTION);
    end;

end;


class function THelpers.OleGetStr(RecordsetField: variant): string; {Legacy}
begin
    {$D-}
    try
        OleGetStr:=RecordsetField;
    except
        {case of null field}
        OleGetStr:=VarToStr(RecordsetField);
    end;
    {$D+}
end;


class function THelpers.CDate(StrDate: string): TDate;
begin
    Result:=StrToDateDef(StrDate, TDateTimeFormats.NullDate);
end;


class function THelpers.Explode(Text: string; SourceDelim: char): string;
begin
    Result:=StringReplace(Text, SourceDelim, TChars.CRLF, [rfReplaceAll]);
end;


class function THelpers.Implode(Text: TStrings; TargetDelim: char; ItemsHaveQuotes: boolean = False): string;
begin

    var Quote: string;
    if ItemsHaveQuotes then Quote:='''';

    for var iCNT:=0 to Text.Count - 1 do
    begin

        if iCNT < Text.Count - 1 then
            Result:=Result + Quote + Text.Strings[iCNT] + Quote + TargetDelim
        else
            Result:=Result + Quote + Text.Strings[iCNT] + Quote;

    end;

end;


class function THelpers.ListToString(Input: TList<string>; TargetDelim: string): string;
begin

    for var iCNT:=0 to Input.Count - 1 do
    begin

        if iCNT < Input.Count - 1 then
            Result:=Result + Input.Items[iCNT] + TargetDelim
        else
            Result:=Result + Input.Items[iCNT];

    end;

end;


class function THelpers.ArrayToString(Input: TArray<string>; TargetDelim: string): string;
begin

    for var iCNT:=0 to Length(Input) - 1 do
    begin

        if iCNT < Length(Input) - 1 then
            Result:=Result + Input[iCNT] + TargetDelim
        else
            Result:=Result + Input[iCNT];

    end;

end;


class function THelpers.ExportToCSV(SourceArray: TArray<TArray<string>>; FileName: string = ''): TStringList;
begin

    var TempStr: string;
    var SL:=TStringList.Create();
    try

        SL.Clear;

        for var iCNT: integer:=0 to High(SourceArray) - 1 do
        begin

            for var jCNT: integer:=0 to High(SourceArray[1]) do
                TempStr:=TempStr + SourceArray[iCNT, jCNT] + ';';

            SL.Add(TempStr);
            TempStr:='';

        end;

        if FileName <> '' then
            SL.SaveToFile(FileName);

    finally
        Result:=SL;
        SL.Free();
    end;

end;


class function THelpers.IsVoType(VoType: string): boolean;
begin

    Result:=False;

    var Settings: ISettings:=TSettings.Create();
    var tsVAL:=TStringList.Create();
    try

        Settings.GetSectionValues(TConfigSections.InvoiceTypes, tsVAL);

        for var iCNT: integer:=0 to tsVAL.Count - 1 do
        if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
        begin
            Result:=True;
            break;
        end;

    finally
        tsVAL.Free();
    end;

end;


class function THelpers.ShowReport(ReportNumber: cardinal; CurrentForm: TForm): cardinal;
begin

    var Settings: ISettings:=TSettings.Create;
    var AppParam: string:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'REPORT_Report' + IntToStr(ReportNumber), 'about:blank');

    Result:=ShellExecute(
        CurrentForm.Handle,
        'open',
        PChar(Settings.DirApplication + TCommon.UnityReader),
        PChar(AppParam),
        nil,
        SW_SHOWNORMAL
    );

end;


class procedure THelpers.ReturnCoCodesList(var SourceGrid: TStringGrid; const SourceCol: integer;
    var TargetList: TStringList; HasHeader: boolean = False; Prefix: string = '');
begin

    if (not Assigned(SourceGrid)) or (not Assigned(TargetList)) or (not SourceCol > 0 ) then Exit();

    var StartRowValue:=0;
    if HasHeader then StartRowValue:=1;

    TargetList.Clear();
    TargetList.Sorted:=True;
    TargetList.Duplicates:=TDuplicates.dupIgnore;

    for var iCNT:=StartRowValue to SourceGrid.RowCount - 1 do
    begin

        var CoCode: string:=SourceGrid.Cells[SourceCol, iCNT];

        if not String.IsNullOrWhiteSpace(Prefix) then
            CoCode:=CoConvert(CoCode);

        if not String.IsNullOrWhitespace(CoCode) then
                TargetList.Add(Prefix + CoCode);

    end;

end;


class function THelpers.CoConvert(CoNumber: string): string;
begin
    if Length(CoNumber) = 4 then Result:=CoNumber;
    if Length(CoNumber) = 3 then Result:='0'   + CoNumber;
    if Length(CoNumber) = 2 then Result:='00'  + CoNumber;
    if Length(CoNumber) = 1 then Result:='000' + CoNumber;
end;


class function THelpers.GetSourceDBName(CoCode: string; Prefix: string): string;
begin
    if Length(CoCode) = 4 then Result:=Prefix + CoCode;
    if Length(CoCode) = 3 then Result:=Prefix + '0'  + CoCode;
    if Length(CoCode) = 2 then Result:=Prefix + '00' + CoCode;
end;


class function THelpers.GetBuildInfoAsString: string;
begin

    var V1: word;
    var V2: word;
    var V3: word;
    var V4: word;

    GetBuildInfo(V1, V2, V3, V4);
    Result:=IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);

end;


class function THelpers.GetOSVer(CheckForOsName: boolean): string;
begin

    result:='0';

    if not CheckForOsName then
    begin

        case Win32MajorVersion of

            4:
            case Win32MinorVersion of
                // Windows 95
                0:  result:='40';
                // Windows 98
                10: result:='41';
                // Windows ME
                90: result:='49';
            end;

            5:
            case Win32MinorVersion of
                // Windows 2000
                0: result:='50';
                // Windows XP
                1: result:='51';
            end;

            6:
            case Win32MinorVersion of
                // Windows Vista
                0: result:='60';
                // Windows 7
                1: result:='61';
                // Windows 8
                2: result:='62';
                // Windows 8.1
                3: result:='63';
            end;

            10:
            case Win32MinorVersion of
                // Windows 10
                0: result:='100';
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


class function THelpers.Unpack(ItemID: integer; FileName: string; ShouldFileStay: boolean; var LastErrorMsg: string): boolean;
begin

    Result:=False;

    var RS: TResourceStream:=TResourceStream.CreateFromID(hInstance, ItemID, RT_RCDATA);
    try

        RS.Position:=0;
        if not ShouldFileStay then
            DeleteFile(PChar(FileName));

        try
            RS.SaveToFile(FileName);

        except
            on E: Exception do
            begin
                LastErrorMsg:='Cannot extract file from resource container. Exception has been thrown: ' + E.Message;
                Exit();
            end;

        end;

        Result:=True;

    finally
        RS.Free();
    end;

end;

//lekage TMBCSEncoding in System.SysUtils
class function THelpers.UnzippLayouts(FileName: string; DestDir: string): boolean;
begin

    var ZipFile:=TZipFile.Create();
    try

        try

            ZipFile.Open(FileName, zmRead);

            for var iCNT:=0 to ZipFile.FileCount - 1 do
            begin
                var FullPath:=DestDir + ZipFile.FileName[iCNT];
                ZipFile.Extract(iCNT, DestDir, True);
            end;

            Result:=True;

        except
            Result:=False;

        end;

    finally
        ZipFile.Free();
        DeleteFile(PChar(FileName));
    end;

end;


class function THelpers.LoadFileToStr(const FileName: TFileName): string;
begin

    var FileStream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try

        if FileStream.Size > 0 then
        begin
            SetLength(Result, FileStream.Size);
            FileStream.Read(Pointer(Result)^, FileStream.Size);
        end;

    finally
        FileStream.Free();
    end;

end;


class function THelpers.GetCurrentUserSid: string;
begin

    var hAccessToken: THandle;
    var bSuccess:     BOOL;
    var dwBufferLen:  DWORD;
    var szSid:        TSidArray;

    Result:='';
    bSuccess:=OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);

    if not bSuccess then
        if GetLastError = ERROR_NO_TOKEN then
            bSuccess:=OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);

    if bSuccess then
    begin
        ZeroMemory(@szSid, SizeOf(szSid));
        dwBufferLen:=SizeOf(szSid);
        if ObtainTextSid(hAccessToken, szSid, dwBufferLen) theN Result:=szSid;
        CloseHandle(hAccessToken);
    end;

end;


class procedure THelpers.RemoveAllInFolder(const Path: string; const Pattern: string);
begin

    var Rec: TSearchRec;
    var BaseDir:=IncludeTrailingPathDelimiter(Path);

    if System.SysUtils.FindFirst(BaseDir + '\' + Pattern, faAnyFile, Rec) = 0 then
    begin

        try

            repeat

                if (Rec.Attr and faDirectory) = faDirectory then
                begin
                    if (Rec.Name <> '.') and (Rec.Name <> '..') then
                        Winapi.Windows.RemoveDirectory(PChar(BaseDir + '\' + Rec.Name));
                end
                else
                begin
                    Winapi.Windows.DeleteFile(PChar(BaseDir + '\' + Rec.Name));
                end;

            until System.SysUtils.FindNext(Rec) <> 0;

        finally
            System.SysUtils.FindClose(Rec);
        end;

    end;

end;


end.

