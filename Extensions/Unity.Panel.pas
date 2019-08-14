unit Unity.Panel;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Vcl.Graphics,
    Vcl.ExtCtrls;


type


    TPanel = class(Vcl.ExtCtrls.TPanel)
    protected
        procedure Paint; override;
    published
        procedure PanelBorders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);
    public
        var PenWidthTop   :  integer;
        var PenWidthBottom:  integer;
        var PenWidthLeft  :  integer;
        var PenWidthRight :  integer;
        var PenColorTop   :  TColor;
        var PenColorBottom:  TColor;
        var PenColorLeft  :  TColor;
        var PenColorRight :  TColor;
        var mcBrushColor  :  TColor;
    end;


implementation


uses
    Winapi.Messages,
    Winapi.Windows,
    Vcl.Forms;


procedure TPanel.Paint;
begin

    inherited;

    /// <remarks>
    /// None of the given variables can be coloured black.
    /// </remarks>

    if (mcBrushColor   = $00000000) and
       (PenColorTop    = $00000000) and
       (PenColorBottom = $00000000) and
       (PenColorLeft   = $00000000) and
       (PenColorRight  = $00000000) then Exit;

    // Get dimensions
    var R: TRect:=ClientRect;

    // Fill background
    Canvas.Brush.Color:=mcBrushColor;

    // Top border
    Canvas.Pen.Width:=PenWidthTop;
    Canvas.Pen.Color:=PenColorTop;
    Canvas.MoveTo(1,           1);
    Canvas.LineTo(R.Right - 1, 1);

    // Bottom border
    Canvas.Pen.Width:=PenWidthBottom;
    Canvas.Pen.Color:=PenColorBottom;
    Canvas.MoveTo(1,           R.Bottom - 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);

    // Left border
    Canvas.Pen.Width:=PenWidthLeft;
    Canvas.Pen.Color:=PenColorLeft;
    Canvas.MoveTo(1,            1);
    Canvas.LineTo(1, R.Bottom - 1);

    // Right border
    Canvas.Pen.Width:=PenWidthRight;
    Canvas.Pen.Color:=PenColorLeft;
    Canvas.MoveTo(R.Right - 1,            1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);

end;


procedure TPanel.PanelBorders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);
begin
    // Turn-off styles
    BorderStyle   :=bsNone;
    // Assign colors and draw
    mcBrushColor  :=FillColor;
    PenColorTop   :=TopColor;
    PenColorBottom:=BottomColor;
    PenColorLeft  :=LeftColor;
    PenColorRight :=RightColor;
end;


end.

