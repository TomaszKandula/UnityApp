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
    Unity.Grid;


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
        class function  Explode(Text: string; SourceDelim: char): string; static;
        class function  Implode(Text: TStrings; TargetDelim: char): string; static;
    end;


implementation


uses
    System.SysUtils,
    System.Variants,
    Vcl.Graphics,
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


end.

