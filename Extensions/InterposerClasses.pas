
{$I .\Include\Header.inc}

unit InterposerClasses;


interface


uses
    Arrays,
    Grids,
    ExtCtrls,
    Messages,
    Controls,
    Graphics,
    Types,
    Dialogs,
    Forms,
    Winapi.Windows,
    Clipbrd,
    SysUtils,
    Math,
    Classes,
    ComObj,
    ComCtrls,
    Variants,
    StdCtrls,
    CheckLst;


    /// <summary>
    ///     This unit contains all extensions of standard components introduced via interposer class.
    ///     There is no need to register component as we do not make new derived class.
    /// </summary>

type

    /// <remarks>
    ///     Reference to TSTringGrid object, necessary for implementing "delete" function.
    /// </remarks>

    TAbstractGrid = class(Grids.TStringGrid);

    /// <summary>
    ///
    /// </summary>

    TCheckListBox = class(CheckLst.TCheckListBox)
    published
        procedure Freeze(PaintWnd: boolean);
    end;

    /// <summary>
    ///     Interposer class of TEdit. Extension.
    /// </summary>

    TEdit = Class(StdCtrls.TEdit)
    public
        FAlignment: TAlignment;
        procedure SetAlignment(value: TAlignment);
        procedure CreateParams(var params: TCreateParams); override;
        property  Alignment: TAlignment read FAlignment write SetAlignment;
    end;

    /// <summary>
    ///     Interposer class of TShape component. Extension.
    /// </summary>

    TShape = class(ExtCtrls.TShape)
    protected
        procedure Paint; override;
        procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
        procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    published
        property  Caption;
        property  Font;
        procedure ShapeText(Left, Top: integer; StrText: string; Format: TFontStyles);
    public
        var CaptionLeft : integer;
        var CaptionTop  : integer;
    end;

    /// <summary>
    ///     Interposer class of TPanel component. Extension.
    /// </summary>

    TPanel = class(ExtCtrls.TPanel)
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

    /// <summary>
    ///     Interposer class of TStringGrid component. Extension.
    /// </summary>

    TStringGrid = class(Grids.TStringGrid)
    private
        { EMPTY }
    protected
        procedure Paint; override;
    public
        var FHideFocusRect: Boolean;
        var OpenThdId:  integer;
        var SqlColumns: TLists;
        var UpdatedRowsHolder: TIntigers;
        procedure SetUpdatedRow(Row: integer);
        procedure RecordRowsAffected;
    published
        property  HideFocusRect:Boolean read FHideFocusRect write FHideFocusRect;
        procedure CopyCutPaste(mode: integer);
        procedure DelEsc(mode: integer; pCol, pRow: integer);
        procedure ClearAll(dfRows: integer; FixedRows: integer; FixedCols: integer; ZeroCol: boolean);
        procedure DeleteRowFrom(FixedRow: integer; FixedCol: integer);
        procedure DrawSelected(ARow: integer; ACol: integer; State: TGridDrawState; Rect: TRect; FontColorSel: TColor; BrushColorSel: TColor; FontColor: TColor; BrushColor: TColor; Headers: boolean);
        procedure ColorValues(ARow: integer; ACol: integer; Rect: TRect; NegativeColor: TColor; PositiveColor: TColor);
        procedure SetColWidth(FirstDefault: integer; AddSpace: integer; Limit: integer);
        procedure SetRowHeight(RowHeight, Header: integer);
	    procedure MSort(const SortCol, datatype: integer; const ascending: boolean);
        procedure AutoThumbSize;
        procedure SaveLayout(ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string);
        function  LoadLayout(var StrCol: string; ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string): boolean;
        function  ReturnColumn(ColumnName: string; FixedCol: integer; FixedRow: integer): integer;
        function  ToExcel(ASheetName, AFileName: string): Boolean;
        procedure Freeze(PaintWnd: boolean);
        function  ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
        function  ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
        procedure SelectAll;
        procedure HideGrids;
        procedure ShowGrids;
    end;

    /// <summary>
    ///     Interposer class of TListView component. Extension, add ability to freeze itself.
    /// </summary>

    TListView = class(ComCtrls.TListView)
    published
        procedure Freeze(PaintWnd: boolean);
    end;


implementation


uses
    Main,
    Settings,
    SQL;


// --------------------------------------------------------------------------------------------------------------------------- EXTENSION OF 'TLISTBOX' CLASS //

/// <summary>
///     Allow to freeze component.
/// </summary>

procedure TCheckListBox.Freeze(PaintWnd: Boolean);
begin

    if (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;

    if not (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Self.Repaint;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------ EXTENSION OF 'TEDIT' CLASS //


procedure TEdit.CreateParams(var Params: TCreateParams);
begin
    inherited CreateParams(Params);

    case Alignment of
        taLeftJustify:  Params.Style:=Params.Style or ES_LEFT   and not ES_MULTILINE;
        taRightJustify: Params.Style:=Params.Style or ES_RIGHT  and not ES_MULTILINE;
        taCenter:       Params.Style:=Params.Style or ES_CENTER and not ES_MULTILINE;
    end;

end;


procedure TEdit.SetAlignment(value: TAlignment);
begin
    if FAlignment <> value then
    begin
        FAlignment:=value;
        RecreateWnd;
    end;
end;


// ----------------------------------------------------------------------------------------------------------------------------- EXTENSION OF 'TSHAPE' CLASS //


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
///     Paint method with text function.
/// </summary>

procedure TShape.Paint;
var
    R: TRect;
begin
    inherited;

    Canvas.Font.Assign(Font);
    R:=ClientRect;

    /// <remarks>
    ///     Alternative code:
    /// </remarks>
    /// <code>
    ///     DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_VCENTER or DT_LEFT { DT_CENTER } or DT_SINGLELINE);
    /// </code>

    TextOut(Canvas.Handle, CaptionLeft, CaptionTop, PChar(Caption), Length(Caption));

end;


/// <summary>
///     Drwa text inside TShape component. Please note that font is fixed.
/// </summary>

procedure TShape.ShapeText(Left, Top: integer; StrText: string; Format: TFontStyles);
begin

    // Fixed
    Font.Name  :='Tahoma';
    Font.Size  :=10;
    Font.Color :=clBlack;

    // Non-fixed
    Font.Style :=Format;
    Caption    :=StrText;
    CaptionLeft:=Left;
    CaptionTop :=Top;

end;


// ----------------------------------------------------------------------------------------------------------------------------- EXTENSION OF 'TPANEL' CLASS //


/// <summary>
///     Painy method of TPanel class.
/// </summary>

procedure TPanel.Paint;
var
    R: TRect;
begin
    inherited;

    /// <remarks>
    ///     None of the given variables can be black.
    /// </remarks>

    if (mcBrushColor   <> $00000000) and
       (PenColorTop    <> $00000000) and
       (PenColorBottom <> $00000000) and
       (PenColorLeft   <> $00000000) and
       (PenColorRight  <> $00000000) then
    begin

        // Get dimensions
        R:=ClientRect;

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

end;


/// <summary>
///     Draw borders of the component.
/// </summary>

procedure TPanel.PanelBorders(FillColor, TopColor, BottomColor, LeftColor, RightColor: TColor);
begin
    // Turn-off styles
    BorderStyle   :=bsNone;
    // assign colors and draw
    mcBrushColor  :=FillColor;
    PenColorTop   :=TopColor;
    PenColorBottom:=BottomColor;
    PenColorLeft  :=LeftColor;
    PenColorRight :=RightColor;
end;


// ------------------------------------------------------------------------------------------------------------------------ EXTENSION OF 'TSTRINGGRID' CLASS //


/// <summary>
///     Register rows for update. Keeps row number to be updated, eg. if data is held in string grid popuated from database table, then to build SQL batch
///     with update statement, we must take all changed rowsby the user - once cell is changed we add row to the register. Later such information can be used
///     to build SQL update expression.
/// </summary>

procedure TStringGrid.SetUpdatedRow(Row: integer);
var
    Rows: integer;
begin

    if Row = 0 then
    begin
        UpdatedRowsHolder:=nil;
        Exit;
    end;

    if UpdatedRowsHolder = nil then
    begin
        SetLength(UpdatedRowsHolder, 1);
        UpdatedRowsHolder[0]:=Row;
    end
    else
    begin
        Rows:=high(UpdatedRowsHolder);
        Rows:=Rows + 2;
        SetLength(UpdatedRowsHolder, Rows);
        UpdatedRowsHolder[Rows - 1]:=Row;
    end;

end;


/// <summary>
///     Register rows affected.
/// </summary>

procedure TStringGrid.RecordRowsAffected;
var
    iCNT: integer;
begin

    if Selection.Top - Selection.Bottom = 0 then
    begin
        SetUpdatedRow(Row);
    end
    else
    begin
        for iCNT:=Selection.Top to Selection.Bottom do
            SetUpdatedRow(iCNT);
    end;

end;


/// <summary>
///     Remove focus rectangle from selection.
/// </summary>
/// <remarks>
///     It requires default drawing to be off.
/// </remarks>

procedure TStringGrid.Paint;
var
    FocusRect: TRect;
    //R: TRect;
begin
    inherited;

    if HideFocusRect then
    begin
        FocusRect:=CellRect(Col, Row);
        if DrawingStyle = gdsThemed then InflateRect(FocusRect, -1, -1);
        DrawFocusRect(Canvas.Handle, FocusRect);
    end;

//    /// <remarks>
//    ///     None of the given variables can be black.
//    /// </remarks>
//
//    if (mcBrushColor   <> $00000000) and
//       (PenColorTop    <> $00000000) and
//       (PenColorBottom <> $00000000) and
//       (PenColorLeft   <> $00000000) and
//       (PenColorRight  <> $00000000) then
//    begin
//
//        // Get dimensions
//        R:=ClientRect;
//
//        // Fill background
//        Canvas.Brush.Color:=mcBrushColor;
//
//        // Top border
//        Canvas.Pen.Width:=PenWidthTop;
//        Canvas.Pen.Color:=PenColorTop;
//        Canvas.MoveTo(0,           0);
//        Canvas.LineTo(R.Right - 0, 0);
//
//        // Bottom border
//        Canvas.Pen.Width:=PenWidthBottom;
//        Canvas.Pen.Color:=PenColorBottom;
//        Canvas.MoveTo(1,           R.Bottom - 1 + 10);
//        Canvas.LineTo(R.Right - 1, R.Bottom - 1 + 10);
//
//        // Left border
//        Canvas.Pen.Width:=PenWidthLeft;
//        Canvas.Pen.Color:=PenColorLeft;
//        Canvas.MoveTo(0,            0);
//        Canvas.LineTo(0, R.Bottom - 0);
//
//        // Right border
//        Canvas.Pen.Width:=PenWidthRight;
//        Canvas.Pen.Color:=PenColorLeft;
//        Canvas.MoveTo(R.Right - 1,            1 + 10);
//        Canvas.LineTo(R.Right - 1, R.Bottom - 1 + 10);
//
//    end;

end;


/// <summary>
///     Implementation of "Copy, Cut, Past" functionality.
/// </summary>
/// <param name="mode">Integer, use flag defined in common.inc.</param>

procedure TStringGrid.CopyCutPaste(mode: integer);
var
    Grect:       TGridRect;
    S:           string;
    CS:          string;
    F:           string;
    L:           integer;
    R:           integer;
    C:           integer;
    RTop:        integer;
    CLeft:       integer;
    Sel:         TGridRect;
    Row, Col:    integer;
    TxtFromSel:  string;
    RowNum:      integer;
begin

    // Paste data into string grid
    if mode = adPaste then
    begin
        GRect:=Selection;
        L    :=GRect.Left;
        R    :=GRect.Top;
        RTop :=R;
        CLeft:=L;
        C    :=0;
        S    :=ClipBoard.AsText;
        R    :=R - 1;

        // Go line by line
        while Pos(CR, S) > 0 do
        begin
            R :=R + 1;
            C :=L - 1;
            CS:=Copy(S, 1, Pos(CR, S));
            while Pos(TAB, CS) > 0 do
            begin
                C:=C + 1;
                if (C <= ColCount - 1) and (R <= RowCount - 1) then Cells[C, R]:=Copy(CS, 1, Pos(TAB, CS) - 1);
                F:=Copy(CS, 1, Pos(TAB, CS) - 1);
                Delete(CS,  1, Pos(TAB, CS));
            end;

            if (C <= ColCount - 1) and (R <= RowCount - 1) then
                Cells[C + 1, R]:=Copy(CS, 1, Pos(CR, CS) - 1);

            Delete(S, 1,Pos(CR, S));

            if Copy(S, 1, 1) = LF then
                Delete(S, 1, 1);
        end;

        Selection:=TGridRect(Rect(CLeft, RTop, C + 1, R));

    end;

    // Copy/Cut data from string grid
    if (mode = adCopy) or (mode = adCut) then
    begin
        Sel:=Selection;
        TxtFromSel:='';
        RowNum:=Sel.Top;

        for Row:=Sel.Top to Sel.Bottom do
        begin
            if RowHeights[RowNum] <> -1 then
            begin
                for Col:=Sel.Left to Sel.Right do
                begin
                    TxtFromSel:=TxtFromSel + Cells[Col, Row];

                    if mode = adCut then
                        // Cut
                        Cells[Col, Row]:='';

                    if Col < Sel.Right then
                        TxtFromSel:=TxtFromSel + TAB;
                end;

                if Row < Sel.Bottom then
                    TxtFromSel:=TxtFromSel + CRLF;

            end;

            inc(RowNum);

        end;

        ClipBoard.AsText:=TxtFromSel + CRLF;
        MainForm.DebugMsg(ClipBoard.AsText);

    end;

end;


/// <summary>
///     Delete and escape behaviour for string grid.
/// </summary>

procedure TStringGrid.DelEsc(mode: integer; pCol, pRow: integer);
begin
    if mode = adESC then EditorMode:=False;
    if mode = adDEL then Cells[pCol, pRow]:='';
end;


/// <summary>
///     Clear all content of string grid.
/// </summary>

procedure TStringGrid.ClearAll(dfRows: integer; FixedRows: integer; FixedCols: integer; ZeroCol: boolean);
var
    iCNT:    integer;
    jCNT:    integer;
    OffSet:  integer;
begin

    if not Enabled then Exit;

    if ZeroCol then
        OffSet:=1 else OffSet:=0;

    for iCNT:=FixedRows to Self.RowCount do
        for jCNT:=(FixedCols - OffSet) to Self.ColCount do
            Cells[jCNT, iCNT]:='';

    RowCount:=dfRows;

end;


/// <summary>
///     Delete selected row from given string grid.
/// </summary>

procedure TStringGrid.DeleteRowFrom(FixedRow: integer; FixedCol: integer);
var
    mySG:    TAbstractGrid;
    myRect:  TGridRect;
    sRow:    integer;
    iCNT:    integer;
    jCNT:    integer;
begin

    // Disable drawing
    with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);

    // Check for last row.
    if RowCount > FixedRow + 1 then
    begin
        // Remember selected row
        sRow         :=Row;

        // Selection
        myRect.Left  :=1;
        myRect.Right :=Col;
        myRect.Top   :=Row;
        myRect.Bottom:=Row;

        // Reference string grid to abstract string grid
        Selection :=myRect;

        /// <remarks>
        ///     Do not nil or free it.
        /// </remarks>

        mySG:=TAbstractGrid(Self);
        mySG.DeleteRow(sRow);

        // Keep selection in place
        if (sRow < (RowCount - 1)) then Row:=sRow;

    end
    else
    begin
        if RowCount = FixedRow + 1 then
            for iCNT:=FixedRow to RowCount do for jCNT:=FixedCol to ColCount do Cells[jCNT, iCNT]:='';
    end;

    // Enable drawing
    with Self do
    begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Repaint;
    end;

end;


/// <summary>
///     Selection colour.
/// </summary>

procedure TStringGrid.DrawSelected(ARow: integer; ACol: integer; State: TGridDrawState; Rect: TRect; FontColorSel: TColor; BrushColorSel: TColor; FontColor: TColor; BrushColor: TColor; Headers: boolean);
var
    FixedColumn:  integer;
    FixedRow:     integer;
begin

    // Extend drawing on headers if false
    FixedColumn:=0;
    FixedRow   :=0;

    if not (Headers) then
    begin
        FixedColumn:= -1;
        FixedRow   := -1;
    end;

    // Draw selected row
    if (ARow > FixedRow) and (ACol > FixedColumn) then
    begin
        // Selected line or cell(s)
        if gdSelected in State then
        begin
            // Selected
            Canvas.Font.Color :=FontColorSel;
            Canvas.Brush.Color:=BrushColorSel;
        end
        else
        begin
            // Normal
            Canvas.Font.Color :=FontColor;
            Canvas.Brush.Color:=BrushColor;
        end;

        Canvas.FillRect(Rect);
        Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, Cells[ACol, ARow]);

    end;

end;


/// <summary>
///     Font colors and numbers.
/// </summary>

procedure TStringGrid.ColorValues(ARow: integer; ACol: integer; Rect: TRect; NegativeColor: TColor; PositiveColor: TColor);
var
    MyCell:   string;
    TestCell: string;
begin

    // Get data
    MyCell:=Cells[ACol, ARow];
    TestCell:=MyCell;

    // Remove decimal separator so it can be converted correctly
    if FormatSettings.ThousandSeparator = ',' then TestCell:=StringReplace(TestCell, ',', '', [rfReplaceAll]);
    if FormatSettings.ThousandSeparator = '.' then TestCell:=StringReplace(TestCell, '.', '', [rfReplaceAll]);

    // Setup new colors
    if StrToFloatDef(TestCell, 0) < 0 then Canvas.Font.Color:=NegativeColor;
    if StrToFloatDef(TestCell, 0) > 0 then Canvas.Font.Color:=PositiveColor;
    if StrToFloatDef(TestCell, 0) = 0 then Canvas.Font.Color:=clSilver;

    // Format cell
    MyCell:=FormatFloat('#,##0.00', StrToFloatDef(TestCell, 0));

    // Make rectangle smaller enough so it will not overlap grid lines
    InflateRect(Rect, -2, -2);
    Canvas.TextRect(Rect, MyCell);

end;


{ ----------------------------------------------------------------------------------------------------------------------------------------- AUTO COLUMN WIDTH }

/// <summary>
///     Auto column width.
/// </summary>

procedure TStringGrid.SetColWidth(FirstDefault: integer; AddSpace: integer; Limit: integer);
var
    tblArray:  TIntigers;
    iCNT:      integer;
    jCNT:      integer;
    NewWidth:  integer;
begin

    if Row > 0 then
        SetLength(tblArray, RowCount)
            else
                Exit;

    ColWidths[0]:=FirstDefault;

    // Iterate throught all the columns
    for jCNT:=1 to ColCount - 1 do
    begin

        // Iterate throught all rows including actual header
        for iCNT:=0 to RowCount - 1 do tblArray[iCNT]:=Canvas.TextWidth(Cells[jCNT, iCNT]);

        // Return highest value
        if not (ColWidths[jCNT] = -1) then { Skip hidden columns }
        begin

            NewWidth:=MaxIntValue(tblArray) + AddSpace;

            if NewWidth < Limit then
                ColWidths[jCNT]:=NewWidth
                    else
                        ColWidths[jCNT]:=Limit;

        end;

    end;

    SetLength(tblArray, 1);

end;


/// <summary>
///     Set row height and header height (always first row).
/// </summary>

procedure TStringGrid.SetRowHeight(RowHeight: Integer; Header: Integer);
begin
    DefaultRowHeight:=RowHeight;
    RowHeights[0]:=Header;
end;


/// <summary>
///     Merge sort implementation.
/// </summary>

procedure TStringGrid.MSort(const SortCol: integer; const DataType: integer; const Ascending: boolean);
var
    iCNT:      integer;
    TempGrid:  TStringGrid;
    List:      array of integer;
begin
    TempGrid:=TStringGrid.create(nil);

    try
        TempGrid.RowCount :=RowCount;
        TempGrid.ColCount :=ColCount;
        TempGrid.FixedRows:=FixedRows;
        SetLength(List, RowCount - FixedRows);

        for iCNT:= FixedRows to RowCount - 1 do
        begin
            List[iCNT - FixedRows]:=iCNT;
            TempGrid.Rows[iCNT].Assign(Rows[iCNT]);
        end;

        MergeSort(Self, List, SortCol, DataType, Ascending);

        for iCNT:=0 to RowCount - FixedRows - 1 do
        begin
            Rows[iCNT + FixedRows].Assign(TempGrid.Rows[List[iCNT]]);
        end;

        Row:=FixedRows;

    finally
        TempGrid.Free;
    end;

    SetLength(List, 0);

end;


/// <summary>
///     Auto thumb size implementation.
/// </summary>
/// <remarks>
///     Do not use it, string grid scrolls are bugged. Instead, use separate scroll component, or default string grid scroll behaviour with no
///     auto thumb size.
/// </remarks>

procedure TStringGrid.AutoThumbSize;
var
    info:  TScrollInfo;
begin

    // Read data
    Fillchar(info, SizeOf(info), 0);

    // Veritical
    with info do
    begin
        cbSize:=SizeOf(info);
        fMask:=SIF_ALL;
        GetScrollInfo(Self.Handle, SB_VERT, info);
        fMask:=fMask or SIF_PAGE or SIF_RANGE;
        nPage:=(nMax - nMin) div Self.RowCount;
        //nPage:=Self.RowCount div Self.VisibleRowCount;
    end;

    SetScrollInfo(Self.Handle, SB_VERT, info, True);

    // Horizontal
    with info do
    begin
        cbSize:=SizeOf(info);
        fMask:=SIF_ALL;
        GetScrollInfo(Self.Handle, SB_HORZ, info);
        fMask:=fMask or SIF_PAGE or SIF_RANGE;
        nPage:=(nMax - nMin) div Self.ColCount;
        //nPage:=Self.ColCount div Self.VisibleColCount;
    end;

  SetScrollInfo(Self.Handle, SB_HORZ, info, True);

end;


/// <summary>
///     String grid column layout. It contains with two headers, one is displayed to the user (column title), and second
///     is used to hold original SQL column name, that other can refer and perform SQL queries.
/// </summary>

procedure TStringGrid.SaveLayout(ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string);
var
    Settings:  ISettings;
    iCNT:      integer;
begin
    Settings:=TSettings.Create;

    // Column width
    for iCNT:=0 to Self.ColCount - 1 do
    begin
        if iCNT = 0 then Settings.SetIntegerValue(ColWidthName, ColPrefix + IntToStr(iCNT), 10);
        if iCNT > 0 then Settings.SetIntegerValue(ColWidthName, ColPrefix + IntToStr(iCNT), Self.ColWidths[iCNT]);
    end;

    // SQL column name
    for iCNT:=0 to Self.ColCount - 1 do
        Settings.SetStringValue(ColOrderName, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 0]);

    // Column title
    for iCNT:=0 to Self.ColCount - 1 do
    begin
        if iCNT = 0 then Settings.SetStringValue(ColNames, ColPrefix + IntToStr(iCNT), '');
        if iCNT > 0 then Settings.SetStringValue(ColNames, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 1]);
    end;

    // Encode
    Settings.Encode(AppConfig);

end;


/// <summary>
///     Load layout from application settings.
/// </summary>
/// <remarks>
///     'Colordername' and 'colwidthname' provide the section names for column order and column width.
///     both sections must contains equal number of value keys. Each key contain column name used by
///     string grid component (age view) that displays data from sql server database, thus column names
///     are used to build sql query, this is because we use sql expressions to obtain initial output
///     with filtering and/or sorting etc. Separate filtering to some extend is allowed in string grid
///     however, separate sorting is not implemented to restrict user form "playing around"
///     therefore, there is one place (server) where there is decided how to display data to user,
///     this is part of automation and standard approach across all users, so the user is forced
///     in certain direction by automation, and thus can obtain better results, etc.
/// </remarks>

function TStringGrid.LoadLayout(var StrCol: string; ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string): boolean;
var
    Settings:       ISettings;
    ColOrderSec:    TStringList;
    ColWidthSec:    TStringList;
    ColNamesSec:    TStringList;
    iCNT:           integer;
begin

    // Check number of keys in given section
    Result:=False;

    ColWidthSec:=TStringList.Create;
    ColOrderSec:=TStringList.Create;
    ColNamesSec:=TStringList.Create;

    Settings:=TSettings.Create;
    try
        Settings.GetSection(ColWidthName, ColWidthSec);
        Settings.GetSection(ColOrderName, ColOrderSec);
        Settings.GetSection(ColNames, ColNamesSec);

        if (ColWidthSec.Count = ColOrderSec.Count) and (ColWidthSec.Count = ColNamesSec.Count) then
        begin
            Self.ColCount:=ColWidthSec.Count;
            Result:=True;
            SetLength(Self.SqlColumns, Self.ColCount, 2);

            for iCNT:=0 to Self.ColCount - 1 do
            begin
                // Skip first coumn as it holds empty column (by design, we do not display ID in first column, etc.)
                if iCNT > 0 then
                begin
                    if iCNT < (Self.ColCount - 1) then
                        StrCol:=StrCol + Settings.GetStringValue(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ','
                            else
                                StrCol:=StrCol + Settings.GetStringValue(ColOrderName, ColPrefix + IntToStr(iCNT), '') + ' ';

                    // Store SQL column name and user friendly column name (column title) into helper array
                    Self.SqlColumns[iCNT, 0]:=Settings.GetStringValue(ColOrderName, ColPrefix + IntToStr(iCNT), '');
                    Self.SqlColumns[iCNT, 1]:=Settings.GetStringValue(ColNames, ColPrefix + IntToStr(iCNT), '');

                    // Display only column title
                    Self.Cells[iCNT, 0]:=Settings.GetStringValue(ColNames, ColPrefix + IntToStr(iCNT), '');
                end;

                // Assign saved width
                Self.ColWidths[iCNT]:=Settings.GetIntegerValue(ColWidthName, ColPrefix + IntToStr(iCNT), 100);

            end;
        end;
    finally
        ColWidthSec.Free;
        ColOrderSec.Free;
        ColNamesSec.Free;
    end;

end;


/// <summary>
///     Return column number for given header (column title).
/// </summary>

function TStringGrid.ReturnColumn(ColumnName: string; FixedCol: integer; FixedRow: integer): integer;
var
    iCNT: integer;
begin
    // Out of bound by default
    Result:=-100;

    for iCNT:=FixedCol to ColCount - 1 do
    begin
        if Cells[iCNT, FixedRow - 1] = ColumnName then
        begin
            Result:=iCNT;
            Exit;
        end;
    end;

end;


/// <summary>
///     Export string grid content to Microsoft Excel file.
/// </summary>
/// <remarks>
///     User must have installed microsoft excel with vba installed before exporting string grid to excel file.
///     otherwise error message "invalid string class" will occur.
///     This method should be run in worker thread.
/// </remarks>

function TStringGrid.ToExcel(ASheetName: string; AFileName: string): boolean;
var
    Col:        integer;
    Row:        integer;

    /// <remarks>
    ///     Offsets cannot be less than one.
    /// </remarks>

    RowOffset:  integer;
    ColOffset:  integer;

    XLApp:      OLEVariant;
    Sheet:      OLEVariant;
    DataTables: TDataTables;

begin
    Result:=False;

    DataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        {TODO -oTomek -cTightCoupling : Refactor}

        // Assign command with stored procedure
        DataTables.StrSQL:=EXECUTE + AgeViewExport + SPACE +
                       QuotedStr(MainForm.GroupList[MainForm.GroupListBox.ItemIndex, 0]) + COMMA +
                       QuotedStr(MainForm.GroupListDates.Text);
        // Execute
        DataTables.SqlToGrid(Self, DataTables.ExecSQL, False, True);
    finally
        DataTables.Free;
    end;

    // Initiate Excel application
    try
        XLApp:=CreateOleObject('Excel.Application');
        try
            XLApp.Visible:=False;
            XLApp.Caption:='Unity For Debt Management - Data Export';
            XLApp.DisplayAlerts:=False;
            XLApp.Workbooks.Add(xlWBatWorkSheet);

            /// <remarks>
            ///     Code insight may show false error for below lines of code:
            /// </remarks>
            /// <code>
            ///     Sheet:=XLApp.Workbooks[1].WorkSheets[1];
            ///     XLApp.Workbooks[1].SaveAs(AFileName);
            /// </code>
            /// <remarks>
            ///     In such case, ignore it (test it on RAD Studio XE2 and Tokyo edition).
            /// </remarks>

            Sheet:=XLApp.Workbooks[1].WorkSheets[1];
            Sheet.Name:=ASheetName;

            ColOffset:=1;
            RowOffset:=1;

            // To Excel sheet
            for Col:=0 to Self.ColCount - 1 do
                for Row:=0 to Self.RowCount - 1 do
                    // We mitt first string grid column
                    Sheet.Cells[Row + RowOffset, Col + ColOffset]:=Self.Cells[Col + 1, Row];

            // Simple formatting (this can be extended
            for Col:=0 to Self.ColCount - 1 do Sheet.Columns[Col + ColOffset].ColumnWidth:=15;
            for Row:=0 to Self.RowCount - 1 do Sheet.Rows[Row + RowOffset].RowHeight:=15;

            // Save to file
            XLApp.Workbooks[1].SaveAs(AFileName);
            Result:=True;

        finally

            if not VarIsEmpty(XLApp) then
            begin
                XLApp.DisplayAlerts:=False;
                XLApp.Quit;
                XLAPP:=Unassigned;
                Sheet:=Unassigned;

                if Result then
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data has been successfully transferred to Excel.');
                    SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PCHAR('The data has been successfully transferred to Excel.')));
                end;

            end;
        end;

    except
        on E: Exception do
        begin
            if E.Message = xlWARN_MESSAGE then
            // Excel not found
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data cannot be exported because Microsoft Excel cannot be found.');
                SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported because Microsoft Excel cannot be found.')));
            end
            else
            // General message
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread: [' + IntToStr(OpenThdId) + ']: The data cannot be exported, error message has been thrown: ' + E.Message + '.');
                SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('The data cannot be exported. Description received: ' + E.Message)));
            end;
        end;
    end;

end;


/// <summary>
///     Allow to disable or enable component drawing.
/// </summary>
/// <param name="PaintWnd">
///     Boolean, set True to disable painting.
/// </param>

procedure TStringGrid.Freeze(PaintWnd: Boolean);
begin

    if (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;

    if not (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Self.Repaint;
    end;

end;


/// <summary>
///     Import from CSV file to string grid.
/// </summary>

function TStringGrid.ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
var
    iCNT:       integer;
    jCNT:       integer;
    Count:      integer;
    Data:       TStringList;
    Transit:    TStringList;
    fPath:      string;
    IsError:    boolean;
begin
    Result :=False;
    IsError:=False;
    Count  :=0;

    // GET THE FILE PATH AND PARSE
    if DialogBox.Execute = True then
    begin
        MainForm.ExecMessage(True, mcStatusBar, stImportCSV);
        fPath  :=DialogBox.FileName;

        Data   :=TStringList.Create;
        Transit:=TStringList.Create;

        try
            // Get data from file
            Data.LoadFromFile(fPath);

            // Get columns number
            for iCNT:=0 to Length(Data[0]) do
                if copy(Data[0], iCNT, 1) = Delimiter then inc(Count);

            // Get rows number and setup offset
            Self.RowCount:=Data.Count + 1;

            // Setup transit that will hold split line
            Transit.StrictDelimiter:=True;
            Transit.Delimiter:=Delimiter[1];

            // Iterate through all rows
            try
                for iCNT:= 0 to Data.Count - 1 do
                begin
                    // Split string using given delimiter
                    Transit.DelimitedText:=Data[iCNT];

                    for jCNT:=1 to Count do
                        Self.Cells[jCNT, iCNT + 1]:=Transit[jCNT - 1];

                    Self.Cells[0, iCNT + 1]:=IntToStr((iCNT + 1));
                    Transit.Clear;
                end;

                Result:=True;

            except
                on E: Exception do
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: CSV Import has failed: ' + ExtractFileName(fPath));
                    MainForm.ExecMessage(False, mcError, 'CSV Import has failed. Please check the file and try again.');
                    IsError:=True;
                end;
            end;

        finally
            if not IsError then
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Data has been imported successfully!');
                MainForm.ExecMessage(False, mcInfo, 'Data has been imported successfully!');
            end;
            Data.Free;
            Transit.Free;
            MainForm.ExecMessage(True, mcStatusBar, stReady);
        end;

    end;

end;


/// <summary>
///     Export to CSV (with given delimiter) all string grid content.
/// </summary>

function TStringGrid.ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
var
    iCNT:       integer;
    jCNT:       integer;
    fPath:      string;
    CSVData:    TStringList;
    MyStr:      string;
    CleanStr:   string;
    IsError:    boolean;
begin

    Result :=False;
    IsError:=False;
    CSVData:=TStringList.Create;

    // Write to CSV file
    try
        MainForm.ExecMessage(False, mcStatusBar, stExportCSV);

        // Add rows and columns with delimiter
        for iCNT:=1 to Self.RowCount - 1 do
        begin
            for jCNT:= 1 to Self.ColCount - 1 do
            begin
                CleanStr :=Self.Cells[jCNT, iCNT];
                CleanStr :=StringReplace(CleanStr, CRLF, ' ', [rfReplaceAll]);
                MyStr    :=MyStr + CleanStr + Delimiter;
            end;

            CSVData.Add(MyStr);
            MyStr:='';
        end;

        // Save to file as plain text
        try
            if DialogBox.Execute then
            begin
                CSVData.SaveToFile(DialogBox.FileName);
                Result:=True;
            end
                else
                    Exit;
        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Cannot saved file: ' + ExtractFileName(fPath));
                MainForm.ExecMessage(False, mcError, 'Cannot save the file in the given location.');
                IsError:=True;
            end;
        end;

    finally
        if not IsError then
        begin
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(OpenThdId) + ']: Data has been exported successfully!');
            MainForm.ExecMessage(False, mcInfo, 'Data have been exported successfully!');
        end;
        CSVData.Free;
        MainForm.ExecMessage(False, mcStatusBar, stReady);
    end;

end;


/// <summary>
///     Select all rows and columns (except first row which is presumbly a column title).
/// </summary>

procedure TStringGrid.SelectAll;
var
    GridRect: TGridRect;
begin
    GridRect.Left  :=1;
    GridRect.Top   :=1;
    GridRect.Right :=Self.ColCount;
    GridRect.Bottom:=Self.RowCount;
    Self.Selection :=GridRect;
end;


/// <summary>
///     Show all grids (along with header grids).
/// </summary>

procedure TStringGrid.ShowGrids;
begin
    Self.Options:=Self.Options
        + [goFixedVertLine]
        + [goFixedHorzLine]
        + [goVertLine]
        + [goHorzLine];
end;


/// <summary>
///     Hide all grids (along with header grids).
/// </summary>

procedure TStringGrid.HideGrids;
begin
    Self.Options:=Self.Options
        - [goFixedVertLine]
        - [goFixedHorzLine]
        - [goVertLine]
        - [goHorzLine];
end;


/// <summary>
///     Allow to disable or enable component drawing.
/// </summary>
/// <param name="PaintWnd">
///     Boolean, set True to disable painting.
/// </param>

procedure TListView.Freeze(PaintWnd: Boolean);
begin

    if (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;

    if not (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Self.Repaint;
    end;

end;


end.

