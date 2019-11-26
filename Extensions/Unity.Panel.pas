unit Unity.Panel;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    Vcl.Graphics,
    Vcl.ExtCtrls;


type


    /// <summary>
    /// Extended version of Vcl.StdCtrls.TPanel visual component.
    /// </summary>
    TPanel = class(Vcl.ExtCtrls.TPanel)
    protected
        procedure Paint; override;
    strict private
        var FPenWidthTop:     integer;
        var FPenWidthBottom:  integer;
        var FPenWidthLeft:    integer;
        var FPenWidthRight:   integer;
        var FPenColorTop:     TColor;
        var FPenColorBottom:  TColor;
        var FPenColorLeft:    TColor;
        var FPenColorRight:   TColor;
        var FBrushColor:      TColor;
    public

        /// <summary>
        /// Allow to draw border line around panel component.
        /// </summary>
        procedure Borders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);

    end;


implementation


uses
    Winapi.Messages,
    Winapi.Windows,
    Vcl.Forms;


procedure TPanel.Paint;
begin

    inherited;

    // None of the given variables can be coloured black.
    if (FBrushColor = $00000000) and (FPenColorTop = $00000000) and (FPenColorBottom = $00000000)
        and (FPenColorLeft = $00000000) and (FPenColorRight  = $00000000) then Exit();

    // Get dimensions
    var R: TRect:=ClientRect;

    // Fill background
    Canvas.Brush.Color:=FBrushColor;

    // Top border
    Canvas.Pen.Width:=FPenWidthTop;
    Canvas.Pen.Color:=FPenColorTop;
    Canvas.MoveTo(1, 1);
    Canvas.LineTo(R.Right - 1, 1);

    // Bottom border
    Canvas.Pen.Width:=FPenWidthBottom;
    Canvas.Pen.Color:=FPenColorBottom;
    Canvas.MoveTo(1, R.Bottom - 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);

    // Left border
    Canvas.Pen.Width:=FPenWidthLeft;
    Canvas.Pen.Color:=FPenColorLeft;
    Canvas.MoveTo(1, 1);
    Canvas.LineTo(1, R.Bottom - 1);

    // Right border
    Canvas.Pen.Width:=FPenWidthRight;
    Canvas.Pen.Color:=FPenColorLeft;
    Canvas.MoveTo(R.Right - 1, 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);

end;


procedure TPanel.Borders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);
begin
    BorderStyle    :=bsNone;
    FBrushColor    :=FillColor;
    FPenColorTop   :=TopColor;
    FPenColorBottom:=BottomColor;
    FPenColorLeft  :=LeftColor;
    FPenColorRight :=RightColor;
end;


end.

