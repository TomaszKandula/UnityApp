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
    Vcl.ExtCtrls,
    Vcl.Forms,
    Vcl.Menus,
    Vcl.Grids,
    Unity.Enums,
    Unity.EventLogger,
    Unity.Grid,
    Unity.Arrays;


type


    THelpers = class abstract
        const WM_GETINFO = WM_USER + 120;
        const WM_EXTINFO = WM_APP  + 150;
        class procedure ExecMessage(IsPostType: boolean; IntValue: cardinal; TextValue: string; Form: TForm); static;
        class procedure LoadImageFromStream(var Image: TImage; const FileName: string); static;
        class procedure TurnRowHighlight(var Grid: TStringGrid; var MenuItem: TMenuItem); static;
        class function  WndCall(WinForm: TForm; Mode: TWindowState): integer; static;
        class function  MsgCall(WndType: TAppMessage; WndText: string): integer; static;
        class function  OleGetStr(RecordsetField: variant): string; static;
        class function  CDate(StrDate: string): TDate; static;
        class function  Explode(Text: string; SourceDelim: char): string; static;
        class function  Implode(Text: TStrings; TargetDelim: char): string; static;
        class procedure FindCoData(TargetColumn: integer; TargetGrid: TStringGrid; SourceGrid: TStringGrid); static;
        class function  ConvertCoCode(CoNumber: string; Prefix: string; mode: integer): string; static;
        class function  GetCoCode(CoPos: integer; GroupId: string): string; static;
        class procedure QuickSortExt(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean); static;
        class procedure ExportToCSV(FileName: string; SourceArray: TALists); static;
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
    Unity.Common;


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


/// <summary>
/// Use this when dealing with database and/or datasets/recordset results, field may be null and thus must be converted into string type.
/// </summary>

class function THelpers.OleGetStr(RecordsetField: variant): string;
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


class function THelpers.Implode(Text: TStrings; TargetDelim: char): string;
begin

    var Str: string;

    for var iCNT: integer:=0 to Text.Count do
    begin
        if iCNT < Text.Count then
            Str:=Str + Text.Strings[iCNT] + TargetDelim
                else
                    Str:=Str + Text.Strings[iCNT];
    end;

    Result:=Str;

end;


{Legacy}
class procedure THelpers.FindCoData(TargetColumn: integer; TargetGrid: TStringGrid; SourceGrid: TStringGrid);
begin

    if SourceGrid.RowCount = 0 then Exit;

    for var iCNT: integer:=1 to SourceGrid.RowCount - 1 do
    begin
        if TargetGrid.Cells[TargetColumn, 0] = SourceGrid.Cells[SourceGrid.ReturnColumn(TCompanyData.CoCode, 1, 1), iCNT] then
        begin
            TargetGrid.Cells[TargetColumn, 1]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TCompanyData.CoCurrency, 1, 1), iCNT];
            TargetGrid.Cells[TargetColumn, 2]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TCompanyData.Divisions,  1, 1), iCNT];
            TargetGrid.Cells[TargetColumn, 3]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TCompanyData.Agents,     1, 1), iCNT];
            Break;
        end
        else
        begin
            TargetGrid.Cells[TargetColumn, 1]:=TUnknown.NA;
            TargetGrid.Cells[TargetColumn, 2]:=TUnknown.NA;
            TargetGrid.Cells[TargetColumn, 3]:=TUnknown.NA;
        end;
    end;

end;


{Legacy}
class function THelpers.ConvertCoCode(CoNumber: string; Prefix: string; mode: integer): string;
begin

    Result:= '';

    /// <remarks>
    /// Used only for open items and aging view.
    /// </remarks>

    // Allow to convert '2020' to 'F2020', etc.
    if mode = 0 then
    begin
        if Length(CoNumber) = 4 then Result:=Prefix + CoNumber;
        if Length(CoNumber) = 3 then Result:=Prefix + '0'  + CoNumber;
        if Length(CoNumber) = 2 then Result:=Prefix + '00' + CoNumber;
    end;

    /// <remarks>
    /// Used only to build GroupID.
    /// </remarks>

    // Converts from 2020 to 02020, 340 to 00340 and so on.
    if mode = 1 then
    begin
        if Length(CoNumber) = 4 then Result:='0'   + CoNumber;
        if Length(CoNumber) = 3 then Result:='00'  + CoNumber;
        if Length(CoNumber) = 2 then Result:='000' + CoNumber;
        if Length(CoNumber) = 1 then Result:='00000';
    end;

    // Converts from 02020 to 2020.
    if mode = 2 then
    begin
        for var iCNT: integer:= 1 to Length(CoNumber) do
        begin
            if CoNumber[iCNT] <> '0' then
            begin
                Result:=System.Copy(CoNumber, iCNT, MaxInt);
                Exit;
            end;
        end;
    end;

    // Converts from 2020 to 2020, 340 to 0340... .
    if mode = 3 then
    begin
        if Length(CoNumber) = 4 then Result:=CoNumber;
        if Length(CoNumber) = 3 then Result:='0'   + CoNumber;
        if Length(CoNumber) = 2 then Result:='00'  + CoNumber;
        if Length(CoNumber) = 1 then Result:='000' + CoNumber;
    end;

end;


{Legacy}
class function THelpers.GetCoCode(CoPos: integer; GroupId: string): string;
begin

    /// <remarks>
    /// Return specific CoCode from the given group.
    /// Group id format: series of 4 groups of 5 digits, i.e.: '020470034000043' must be read as follows:
    /// 1. 1ST CO CODE: 02047 (2047)
    /// 2. 2ND CO CODE: 00340 (340)
    /// 3. 3RD CO CODE: 00043 (43)
    /// 4. 4TH CO CODE: 00000 (0)
    /// </remarks>
    if CoPos = 1 then Result:=(MidStr(GroupId, 1,  5).ToInteger).toString;
    if CoPos = 2 then Result:=(MidStr(GroupId, 6,  5).ToInteger).toString;
    if CoPos = 3 then Result:=(MidStr(GroupId, 11, 5).ToInteger).toString;
    if CoPos = 4 then Result:=(MidStr(GroupId, 16, 5).ToInteger).toString;

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


class procedure THelpers.ExportToCSV(FileName: string; SourceArray: TALists);
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
        SL.SaveToFile(FileName);
    finally
        SL.Free;
    end;

end;


end.

