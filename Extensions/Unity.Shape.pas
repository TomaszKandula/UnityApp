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


    /// <summary>
    /// Extended version of Vcl.StdCtrls.TShape visual component.
    /// </summary>
    TShape = class(Vcl.ExtCtrls.TShape)
    protected
        procedure Paint; override;
        procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
        procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    public

        /// <summary>
        /// Left position of given text.
        /// </summary>
        var CaptionLeft: integer;

        /// <summary>
        /// Top position of given text.
        /// </summary>
        var CaptionTop: integer;

        /// <summary>
        /// Defines caption text assigned.
        /// </summary>
        property Caption;

        /// <summary>
        /// Defines font to be used for drawing text in caption.
        /// </summary>
        property Font;

        /// <summary>
        /// Draw text inside TShape component.
        /// </summary>
        procedure ShapeText(Left: integer; Top: integer; StrText: string; Format: TFontStyles; FontName: string; FontSize: integer; FontColor: TColor);

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


procedure TShape.Paint;
begin
    inherited;
    var R: TRect:=ClientRect;
    Canvas.Font.Assign(Font);
    // Alternative code:
    // DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_VCENTER or DT_LEFT { DT_CENTER } or DT_SINGLELINE);
    TextOut(Canvas.Handle, CaptionLeft, CaptionTop, PChar(Caption), Length(Caption));
end;


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

