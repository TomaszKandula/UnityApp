unit Unity.Shape;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    Winapi.Messages,
    Vcl.ExtCtrls,
    Vcl.Controls,
    Vcl.Graphics;


type


    TShape = class(Vcl.ExtCtrls.TShape)
    protected
        procedure Paint; override;
        procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
        procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    published
        property  Caption;
        property  Font;
        procedure ShapeText(Left: integer; Top: integer; StrText: string; Format: TFontStyles; FontName: string; FontSize: integer; FontColor: TColor);
    public
        var CaptionLeft : integer;
        var CaptionTop  : integer;
    end;


implementation


uses
    Winapi.Windows;


procedure TShape.CMFontChanged(var Msg: TMessage);
begin
    inherited;
    Invalidate;
end;


procedure TShape.CMTextChanged(var Msg: TMessage);
begin
    inherited;
    Invalidate;
end;


/// <summary>
/// Paint method with text function.
/// </summary>

procedure TShape.Paint;
begin

    inherited;
    var R: TRect:=ClientRect;

    Canvas.Font.Assign(Font);

    /// <remarks>
    /// Alternative code:
    /// </remarks>
    /// <code>
    /// DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_VCENTER or DT_LEFT { DT_CENTER } or DT_SINGLELINE);
    /// </code>

    TextOut(Canvas.Handle, CaptionLeft, CaptionTop, PChar(Caption), Length(Caption));

end;


/// <summary>
/// Drwa text inside TShape component. Please note that font is fixed.
/// </summary>

procedure TShape.ShapeText(Left: integer; Top: integer; StrText: string; Format: TFontStyles; FontName: string; FontSize: integer; FontColor: TColor);
begin
    Font.Name  :=FontName;
    Font.Size  :=FontSize;
    Font.Color :=FontColor;
    Font.Style :=Format;
    Caption    :=StrText;
    CaptionLeft:=Left;
    CaptionTop :=Top;
end;


end.

