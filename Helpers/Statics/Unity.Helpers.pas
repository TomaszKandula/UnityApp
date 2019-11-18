unit Unity.Helpers;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    System.Classes,
    Winapi.Messages,
    Winapi.Windows,
    Winapi.ShellAPI,
    Vcl.ExtCtrls,
    Vcl.Forms,
    Vcl.Menus,
    Vcl.Grids,
    Unity.Enums,
    Unity.EventLogger,
    Unity.Grid,
    Unity.Arrays;


type


    /// <remarks>
    /// Define custom type for anonymous method to be passed as parameter.
    /// </remarks>
    TInputMethod = reference to procedure;


    THelpers = class abstract
    public
        const WM_GETINFO = WM_USER + 120;
        const WM_EXTINFO = WM_APP  + 150;
        class procedure ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod); static;
        class procedure ExecMessage(IsPostType: boolean; IntValue: cardinal; TextValue: string; Form: TForm); static;
        class procedure LoadImageFromStream(var Image: TImage; const FileName: string); static;
        class procedure TurnRowHighlight(var Grid: TStringGrid; var MenuItem: TMenuItem); static;
        class function  WndCall(WinForm: TForm; Mode: TWindowState): integer; static;
        class function  MsgCall(WndType: TAppMessage; WndText: string): integer; static;
        class function  OleGetStr(RecordsetField: variant): string; static;
        class function  CDate(StrDate: string): TDate; static;
        class function  Explode(Text: string; SourceDelim: char): string; static;
        class function  Implode(Text: TStrings; TargetDelim: char; ItemsHaveQuotes: boolean = False): string; static;
        class procedure QuickSortExt(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean); static;
        class function  ExportToCSV(SourceArray: TALists; FileName: string = ''): TStringList; static;
        class function  IsVoType(VoType: string): boolean; static;
        class function  ShowReport(ReportNumber: cardinal; CurrentForm: TForm): cardinal; static;
        class procedure ReturnCoCodesList(var SourceGrid: TStringGrid; const SourceCol: integer; var TargetList: TStringList; HasHeader: boolean = False; Prefix: string = ''); static;
        class function  CoConvert(CoNumber: string): string; static;
        class function  GetSourceDBName(CoCode: string; Prefix: string): string; static;
    end;


implementation


uses
    System.SysUtils,
    System.StrUtils,
    System.Variants,
    Vcl.Graphics,
    DbModel,
    Unity.DateTimeFormats,
    Unity.Unknown,
    Unity.Chars,
    Unity.Common,
    Unity.Settings;


class procedure THelpers.ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod);
begin

    TThread.CreateAnonymousThread(procedure
    begin

        Sleep(Delay);

        TThread.Synchronize(nil, procedure
        begin

            AnonymousMethod;

        end);

    end).Start;

end;


class procedure THelpers.ExecMessage(IsPostType: boolean; IntValue: cardinal; TextValue: string; Form: TForm);
begin

    var IntValueAlt: integer:=0;

    if TryStrToInt(TextValue, IntValueAlt) then
    begin

        case IsPostType of

            True:  PostMessage(Form.Handle, WM_GETINFO, IntValue, LPARAM(IntValueAlt));
            False: SendMessage(Form.Handle, WM_GETINFO, IntValue, LPARAM(IntValueAlt));

        end;

    end
    else
    begin

        case IsPostType of

            True:  PostMessage(Form.Handle, WM_GETINFO, IntValue, LPARAM(PCHAR(TextValue)));
            False: SendMessage(Form.Handle, WM_GETINFO, IntValue, LPARAM(PCHAR(TextValue)));

        end;

    end;

end;


class procedure THelpers.LoadImageFromStream(var Image: TImage; const FileName: string);
begin

    var FS: TFileStream:=TFileStream.Create(FileName, fmOpenRead);
    FS.Position:=0;

    var WIC: TWICImage:=TWICImage.Create;

    try
        WIC.LoadFromStream(FS);
        Image.Picture.Assign(WIC);
    finally
        WIC.Free;
        FS.Free;
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
    if WndText = '' then Exit;

    case WndType of
        TAppMessage.Info:      Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OK       + MB_ICONINFORMATION);
        TAppMessage.Warn:      Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OK       + MB_ICONWARNING);
        TAppMessage.Error:     Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OK       + MB_ICONERROR);
        TAppMessage.Question1: Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_OKCANCEL + MB_ICONQUESTION);
        TAppMessage.Question2: Result:=Application.MessageBox(PChar(WndText), PChar(TCommon.APPCAPTION), MB_YESNO    + MB_ICONQUESTION);
    end;

end;


class function THelpers.OleGetStr(RecordsetField: variant): string;
begin

    // ----------------------------------------------------------------------
    // Use this when dealing with database and/or datasets/recordset results,
    // field may be null and thus must be converted into string type.
    // ----------------------------------------------------------------------

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

    var Str: string;
    var Quote: string;

    if ItemsHaveQuotes then Quote:='''';

    for var iCNT:=0 to Text.Count - 1 do
    begin

        if iCNT < Text.Count - 1 then
            Str:=Str + Quote + Text.Strings[iCNT] + Quote + TargetDelim
                else
                    Str:=Str + Quote + Text.Strings[iCNT] + Quote;

    end;

    Result:=Str;

end;


class procedure THelpers.QuickSortExt(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
{ "A" VARIABLE HOLDS NUMERICAL DATA TO BE SORTED. "L" VARIABLE IS "ASSOCIATED" COLUMN WITH ORIGINAL LIST POSITION. THE SECOND ASSOCIATED COLUMN FOLLOWS }
{ "A" COLUMN, BUT IT IS NOT SORTED. IT ALLOWS TO ASSIGN SORTED VALUES BACK TO ORIGINAL LIST POSITION AFTER COMPUTATION IS DONE. THIS IS TO BE USED WHEN }
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
    Pivot:=A[(Lo + Hi) div 2];

    repeat

        { ASCENDING }
        if ASC then
        begin
            while A[Lo] < Pivot do Inc(Lo);
            while A[Hi] > Pivot do Dec(Hi);
        end;

        { DESCENDING }
        if not ASC then
        begin
            while A[Lo] > Pivot do Inc(Lo);
            while A[Hi] < Pivot do Dec(Hi);
        end;

        { MOVING POSITIONS }
        if Lo <= Hi then
        begin

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

    if Hi > iLo then QuickSortExt(A, L, iLo, Hi, ASC);
    if Lo < iHi then QuickSortExt(A, L, Lo, iHi, ASC);

end;


class function THelpers.ExportToCSV(SourceArray: TALists; FileName: string = ''): TStringList;
begin

    var TempStr: string;
    var SL: TStringList:=TStringList.Create;
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
        SL.Free;
    end;

end;


class function THelpers.IsVoType(VoType: string): boolean;
begin

    Result:=False;

    var Settings: ISettings:=TSettings.Create;
    var tsVAL: TStringList:=TStringList.Create;
    try

        Settings.GetSectionValues(TConfigSections.InvoiceTypes, tsVAL);

        for var iCNT: integer:=0 to tsVAL.Count - 1 do
        if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
        begin
            Result:=True;
            break;
        end;

    finally
        tsVAL.Free;
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


class procedure THelpers.ReturnCoCodesList(var SourceGrid: TStringGrid; const SourceCol: integer; var TargetList: TStringList; HasHeader: boolean = False; Prefix: string = '');
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


end.

