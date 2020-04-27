unit Unity.Helpers;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    System.Rtti,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Classes,
    System.SysUtils,
    System.IOUtils,
    Winapi.Messages,
    Winapi.Windows,
    Vcl.ExtCtrls,
    Vcl.Forms,
    Vcl.Menus,
    Vcl.Grids,
    Unity.Types,
    Unity.Enums,
    Unity.Grid,
    Unity.Records,
    Unity.References,
    Api.BankDetails;


type


    TArrayUtils<T> = class abstract
    public
        class function Contains(const x: T; const anArray: array of T): boolean; static;
        class procedure Copy(FromArray: TArray<T>; var ToArray: TArray<T>); static;
        class procedure CopyToList(FromArray: TArray<T>; var TargetList: TList<T>); static;
        class procedure Move(var FromArray: TArray<T>; var ToArray: TArray<T>); static;
    end;


    TRecordUtils<T: record> = class abstract
    public
        class function GetFields(const Struct: T): TList<string>; static;
    end;


    THelpers = class abstract
    strict private
        const HtmlBanks = '<p class="p">{ROWS}</p>';
        const HtmlRow   = '<b>Bank Details</b>: <br> {BANK_NAME} (<b>{ISO}</b> payments) <br> IBAN/Account No: {BANK_ACC} {BIC} <br><br>';
        const HtmlBic   = '(BIC: {BIC_NUMBER})';
        const HtmlEmpty = '<!-- NO BANK ACCOUNT ATTACHED -->';
        const HEAP_ZERO_MEMORY = $00000008;
        const SID_REVISION = 1;
        const ToSkip = [#0..#32, '.', ',', ';', '[', ']', '(', ')', '{', '}'];
        class function ConvertSid(Sid: PSID; pszSidText: PChar; var dwBufferLen: DWORD): BOOL; static;
        class function ObtainTextSid(hToken: THandle; pszSid: PChar; var dwBufferLen: DWORD): BOOL; static;
        class procedure GetBuildInfo(var V1, V2, V3, V4: word); static;
    public
        /// <summary>For Windows internal messaging between objects.</summary>
        const WM_GETINFO = WM_USER + 120;
        /// <summary>For Windows external messaging between different applications.</summary>
        const WM_EXTINFO = WM_APP + 150;
        class procedure ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod); static;
        class procedure LoadImageFromStream(var Image: TImage; const FileName: string); static;
        class procedure TurnRowHighlight(var Grid: TStringGrid; var MenuItem: TMenuItem); static;
        class function WndCall(WinForm: TForm; Mode: TWindowState; ParentForm: TForm = nil): integer; static;
        class function MsgCall(Handler: Hwnd; WndType: TAppMessage; WndText: string): integer; static;
        class function CDate(StrDate: string): TDate; static;
        class function Explode(Text: string; SourceDelim: char): string; static;
        class function Implode(Text: TStrings; TargetDelim: char; ItemsHaveQuotes: boolean = False): string; static;
        class function ListToString(Input: TList<string>; TargetDelim: string): string; static;
        class function StringToArray(DelimitedText: string; Delim: char): TArray<string>; static;
        class function ArrayStrToString(Input: TArray<string>; TargetDelim: string): string; static;
        class function ArrayIntToString(Input: TArray<integer>; TargetDelim: string): string; static;
        class function ExportToCSV(SourceArray: TArray<TArray<string>>; FileName: string = ''): TStringList; static;
        class function IsVoType(VoType: string): boolean; static;
        class procedure ReturnCoCodesList(var SourceGrid: TStringGrid; const SourceCol: integer; var TargetList: TStringList; HasHeader: boolean = False; Prefix: string = ''); static;
        class function DbNameToCoCode(SourceDBName: string): string; static;
        class function CoConvert(CoNumber: string): string; static;
        class function GetSourceDBName(CoCode: string; Prefix: string): string; static;
        class function GetBuildInfoAsString: string; static;
        class function GetOSVer(CheckForOsName: boolean): string; static;
        class function Unpack(ItemID: integer; FileName: string; ShouldFileStay: boolean; var LastErrorMsg: string): boolean; static;
        class function UnzippLayouts(FileName: string; DestDir: string): boolean; static;
        class function LoadFileToStr(const FileName: TFileName): string; static;
        class function GetCurrentUserSid: string; static;
        class procedure RemoveAllInFolder(const Path: string; const Pattern: string); static;
        class function FormatDateTime(InputDateTime: string; Returns: TCalendar): string; static;
        class function BankListToHtml(BankDetails: TArray<TBankDetails>): string; static;
        class procedure StrArrayToStrings(Input: TArray<string>; var Output: TStringList); static;
        class function StringToArrayInt(Input: string; SourceDelim: char): TArray<integer>; static;
        class function WordCount(const InputStr: string): cardinal; static;
        class procedure ComputeAgeSummary(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad); static;
        class procedure ComputeRiskClass(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad; RiskClassGroup: TRiskClassGroup); static;
    end;


implementation


uses
    System.StrUtils,
    System.Variants,
    System.Zip,
    Vcl.Graphics,
    Unity.Constants,
    Unity.Settings,
    Unity.Sorting,
    Unity.Service,
    Api.CustomerSnapshotEx;


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


class procedure TArrayUtils<T>.Copy(FromArray: TArray<T>; var ToArray: TArray<T>);
begin
    ToArray:=FromArray;
    SetLength(ToArray, Length(FromArray));
end;


class procedure TArrayUtils<T>.CopyToList(FromArray: TArray<T>; var TargetList: TList<T>);
begin
    for var iCNT:=0 to Length(FromArray) - 1 do
        TargetList.Add(FromArray[iCNT]);
end;


class procedure TArrayUtils<T>.Move(var FromArray: TArray<T>; var ToArray: TArray<T>);
begin
    ToArray:=FromArray;
    SetLength(ToArray, Length(FromArray));
    FromArray:=nil;
end;


class function TRecordUtils<T>.GetFields(const Struct: T): TList<string>;
begin

    var RttiContext: TRttiContext;
    var RttiType:    TRttiType;
    var RttiField:   TRttiField;

    try

        RttiContext:=TRttiContext.Create();
        Result:=TList<string>.Create();

        for RttiField in RttiContext.GetType(TypeInfo(T)).GetFields do
        begin
            RttiType:=RttiField.FieldType;
            Result.Add(RttiType.Name);
        end;

    except
        on E: Exception do

    end;

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


class function THelpers.WndCall(WinForm: TForm; Mode: TWindowState; ParentForm: TForm = nil): integer;
begin

    Result:=0;

    WinForm.PopupMode  :=pmExplicit;
    WinForm.PopupParent:=ParentForm;

    case Mode of
        TWindowState.Modal:    Result:=WinForm.ShowModal;
        TWindowState.Modeless: WinForm.Show;
    end;

end;


class function THelpers.MsgCall(Handler: Hwnd; WndType: TAppMessage; WndText: string): integer;
begin

    Result:=0;
    if WndText = '' then Exit();

    case WndType of

        TAppMessage.Info: Result:=
            Winapi.Windows.MessageBox(
                Handler,
                PChar(WndText),
                PChar(TCommon.APPCAPTION),
                MB_OK + MB_ICONINFORMATION
            );

        TAppMessage.Warn: Result:=
            Winapi.Windows.MessageBox(
                Handler,
                PChar(WndText),
                PChar(TCommon.APPCAPTION),
                MB_OK + MB_ICONWARNING
            );

        TAppMessage.Error: Result:=
            Winapi.Windows.MessageBox(
                Handler,
                PChar(WndText),
                PChar(TCommon.APPCAPTION),
                MB_OK + MB_ICONERROR
            );

        TAppMessage.Question1: Result:=
            Winapi.Windows.MessageBox(
                Handler,
                PChar(WndText),
                PChar(TCommon.APPCAPTION),
                MB_OKCANCEL + MB_ICONQUESTION
            );

        TAppMessage.Question2: Result:=
            Winapi.Windows.MessageBox(
                Handler,
                PChar(WndText),
                PChar(TCommon.APPCAPTION),
                MB_YESNO + MB_ICONQUESTION
            );

    end;

end;


class function THelpers.CDate(StrDate: string): TDate;
begin
    Result:=StrToDateDef(StrDate, TDtFormat.NullDate);
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


class function THelpers.StringToArray(DelimitedText: string; Delim: char): TArray<string>;
begin

    Result:=TArray<string>.Create(DelimitedText);

    var StringList:=TStringList.Create();
    try
        StringList.Delimiter:=Delim;
        StringList.DelimitedText:=DelimitedText;
        Result:=StringList.ToStringArray;
    finally
        StringList.Free();
    end;

end;


class function THelpers.ArrayStrToString(Input: TArray<string>; TargetDelim: string): string;
begin

    for var iCNT:=0 to Length(Input) - 1 do
    begin

        if iCNT < Length(Input) - 1 then
            Result:=Result + Input[iCNT] + TargetDelim
        else
            Result:=Result + Input[iCNT];

    end;

end;


class function THelpers.ArrayIntToString(Input: TArray<integer>; TargetDelim: string): string;
begin

    for var iCNT:=0 to Length(Input) - 1 do
    begin

        if iCNT < Length(Input) - 1 then
            Result:=Result + Input[iCNT].ToString() + TargetDelim
        else
            Result:=Result + Input[iCNT].ToString();

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

    var tsVAL:=TStringList.Create();
    try

        Service.Settings.GetSectionValues(TConfigSections.InvoiceTypes, tsVAL);

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


class function THelpers.DbNameToCoCode(SourceDBName: string): string;
begin
    if String.IsNullOrWhitespace(SourceDBName) then Exit();
    if SourceDBName.Contains('F')   then Result:=SourceDBName.Replace('F', '');
    if SourceDBName.Contains('F0')  then Result:=SourceDBName.Replace('F0', '');
    if SourceDBName.Contains('F00') then Result:=SourceDBName.Replace('F00', '');
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


class function THelpers.FormatDateTime(InputDateTime: string; Returns: TCalendar): string;
begin

    if String.IsNullOrEmpty(InputDateTime) then Exit();

    // Expected format: 2019-12-18T23:32:04.137
    var TempStr:=InputDateTime.Split(['T']);
    var TempTime:=TempStr[1];
    var NewTime:=TempTime.Split(['.']);

    case Returns of
        TCalendar.TimeOnly: Result:=NewTime[0];
        TCalendar.DateOnly: Result:=TempStr[0];
        TCalendar.DateTime: Result:=TempStr[0] + ' ' + NewTime[0];
    end;

end;


class function THelpers.BankListToHtml(BankDetails: TArray<TBankDetails>): string;
begin

    Result:=HtmlEmpty;
    if Length(BankDetails) = 0 then Exit();

    var HtmlLines: string;
    for var iCNT:=0 to Length(BankDetails) - 1 do
    begin

        var HtmlLine:=HtmlRow;
        HtmlLines:=HtmlLines + #13#10 + HtmlLine
            .Replace('{BANK_NAME}', BankDetails[iCNT].BankName)
            .Replace('{ISO}',       BankDetails[iCNT].BankIso)
            .Replace('{BANK_ACC}',  BankDetails[iCNT].BankAcc);

        if not String.IsNullOrEmpty(BankDetails[iCNT].BankCode) then
        begin
            var BicLine:=HtmlBic;
            BicLine:=BicLine.Replace('{BIC_NUMBER}', BankDetails[iCNT].BankCode);
            HtmlLines:=HtmlLines.Replace('{BIC}', BicLine);
        end
        else
        begin
            HtmlLines:=HtmlLines.Replace('{BIC}', '');
        end;

    end;

    var HtmlOutput:=HtmlBanks;
    Result:=HtmlOutput.Replace('{ROWS}', HtmlLines);

end;


class procedure THelpers.StrArrayToStrings(Input: TArray<string>; var Output: TStringList);
begin
    for var iCNT:=0 to Length(Input) - 1 do Output.Add(Input[iCNT]);
end;


class function THelpers.StringToArrayInt(Input: string; SourceDelim: char): TArray<integer>;
begin

    var SL:=TStringList.Create();
    try

        Result:=TArray<integer>.Create();
        try

            SL.Delimiter:=SourceDelim;
            SL.DelimitedText:=Input;

            SetLength(Result, SL.Count);

            for var iCNT:=0 to SL.Count - 1 do
                Result[iCNT]:=SL.Strings[iCNT].ToInteger();

        except
            SetLength(Result, 0);
        end;

    finally
        SL.Free();
    end;

end;


class function THelpers.WordCount(const InputStr: string): cardinal;
begin

    Result:=0;

    var TextLength: integer:=Length(InputStr);
    var FindWord:   boolean:=False;

    // Count words in TMemo component while user is typing
    for var Index:=1 to TextLength do
    begin

        if not (CharInSet(InputStr[Index], ToSkip)) then
        begin

            if not FindWord then
            begin
                FindWord:=True;
                Inc(Result);
            end;

        end
        else
        begin
            FindWord:=False;
        end;

    end;

end;


class procedure THelpers.ComputeAgeSummary(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad);
begin

    for var Index:=1 to Grid.RowCount - 1 do if Grid.RowHeights[Index] <> Grid.sgRowHidden then
    begin

        AgingPayLoad.ANotDue:=AgingPayLoad.ANotDue + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._NotDue), Index], 0);
        AgingPayLoad.ARange1:=AgingPayLoad.ARange1 + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Range1), Index], 0);
        AgingPayLoad.ARange2:=AgingPayLoad.ARange2 + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Range2), Index], 0);
        AgingPayLoad.ARange3:=AgingPayLoad.ARange3 + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Range3), Index], 0);
        AgingPayLoad.ARange4:=AgingPayLoad.ARange4 + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Range4), Index], 0);
        AgingPayLoad.ARange5:=AgingPayLoad.ARange5 + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Range5), Index], 0);
        AgingPayLoad.ARange6:=AgingPayLoad.ARange6 + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Range6), Index], 0);

        AgingPayLoad.Balance:=AgingPayLoad.Balance + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Total), Index], 0);
        AgingPayLoad.Limits:=AgingPayLoad.Limits + StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._CreditLimit), Index], 0);

        if StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._CreditBalance), Index], 0) < 0 then
        begin

            Inc(AgingPayLoad.Exceeders);
            AgingPayLoad.TotalExceed:=
                AgingPayLoad.TotalExceed + Abs(StrToFloatDef(Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._CreditBalance), Index], 0));

        end;

        Inc(AgingPayLoad.CustAll);

    end;

end;


class procedure THelpers.ComputeRiskClass(var Grid: TStringGrid; var AgingPayLoad: TAgingPayLoad; RiskClassGroup: TRiskClassGroup);
begin

    if AgingPayLoad.Balance = 0 then Exit();

    var TotalPerItem: TArray<double>;
    var ListPosition: TArray<integer>;
    var Count: double:=0;
    var Rows: integer:=0;

    AgingPayLoad.RCA:=AgingPayLoad.Balance * RiskClassGroup.Class_A;
    AgingPayLoad.RCB:=AgingPayLoad.Balance * RiskClassGroup.Class_B;
    AgingPayLoad.RCC:=AgingPayLoad.Balance * RiskClassGroup.Class_C;

    // Move totals and its positions into array
    for var Index:=1 to Grid.RowCount do if Grid.RowHeights[Index] <> Grid.sgRowHidden then
    begin
        SetLength(ListPosition, Rows + 1);
        SetLength(TotalPerItem, Rows + 1);
        ListPosition[Rows]:=Index;
        TotalPerItem[Rows]:=StrToFloatDef((Grid.Cells[Grid.GetCol(TCustomerSnapshotEx._Total), Index]), 0);
        Inc(Rows);
    end;

    // Sort via total value
    TSorting.QuickSort(TotalPerItem, ListPosition, Low(TotalPerItem), High(TotalPerItem), False);

    // Compute and display RCA
    for var Index:=Low(ListPosition) to High(ListPosition) do
    begin

        Count:=Count + TotalPerItem[Index];

        // Risk Class 'A'
        if Count <= AgingPayLoad.RCA then
        begin
            Grid.Cells[Grid.GetCol('Risk Class'), ListPosition[Index]]:='A';
            inc(AgingPayLoad.RCAcount);
        end;

        // Risk Class 'B'
        if (Count > AgingPayLoad.RCA) and (Count <= AgingPayLoad.RCA + AgingPayLoad.RCB) then
        begin
            Grid.Cells[Grid.GetCol('Risk Class'), ListPosition[Index]]:='B';
            inc(AgingPayLoad.RCBcount);
        end;

        // Risk Class 'C'
        if Count > AgingPayLoad.RCA + AgingPayLoad.RCB then
        begin
            Grid.Cells[Grid.GetCol('Risk Class'), ListPosition[Index]]:='C';
            inc(AgingPayLoad.RCCcount);
        end;

    end;

end;


end.

