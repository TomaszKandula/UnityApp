unit Unity.Grid;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Vcl.Grids,
    Vcl.Graphics,
    Vcl.Dialogs,
    Unity.Enums;


type


    /// <summary>
    /// Shorthand (pointer) to TStringGrid necessary for row delete action.
    /// </summary>
    TAbstractGrid = class(Vcl.Grids.TStringGrid);


    /// <summary>
    /// Extended version of Vcl.StdCtrls.TEdit visual component.
    /// </summary>
    TStringGrid = class(Vcl.Grids.TStringGrid)
    strict private
        var FToExcelResult:   string;
        var FCsvImportResult: string;
        var FCsvExportResult: string;
    public
        const sgRowHeight = 19;
        const sgRowHidden = -1;
        const xlWBATWorksheet = -4167;
        const xlWARN_MESSAGE  = 'Invalid class string';
        const FFontWhite = $00FFFFFF;
        const FFontBlack = $00000000;
        const FBackRed = $008080FF;
        const FBackYellow = $00CCFFFF;
        const FBackGreen = $00ACAC59;
        /// <summary>
        /// Result message of a task executed by ToExcel method.
        /// </summary>
        property ToExcelResult: string read FToExcelResult;
        /// <summary>
        /// Result message of a task executed by ImportCSV method.
        /// </summary>
        property CsvImportResult: string read FCsvImportResult;
        /// <summary>
        /// Result message of a task executed by ExportCSV method.
        /// </summary>
        property CsvExportResult: string read FCsvExportResult;
        /// <summary>
        /// Allow to either copy from, paste to, or cut data.
        /// </summary>
        procedure CopyCutPaste(Mode: TActions; FirstColOnly: boolean = False);
        /// <summary>
        /// Unknown.
        /// </summary>
        procedure DelEsc(Mode: TActions; pCol, pRow: integer);
        /// <summary>
        /// Remove al the data from the rows and cols. Reset number of cols and rows to 1.
        /// </summary>
        procedure ClearAll(dfRows: integer; FixedRows: integer; FixedCols: integer; ZeroCol: boolean);
        /// <summary>
        /// Alow to remove entire row from the grid.
        /// </summary>
        procedure DeleteRowFrom(FixedRow: integer; FixedCol: integer);
        /// <summary>
        /// Defines how to draw grid.
        /// </summary>
        procedure DrawSelected(ARow: integer; ACol: integer; State: TGridDrawState; Rect: TRect; FontColorSel: TColor; BrushColorSel: TColor; FontColor: TColor; BrushColor: TColor; Headers: boolean);
        /// <summary>
        /// Defines how to color values (numbers) in grid.
        /// </summary>
        procedure ColorValues(ARow: integer; ACol: integer; Rect: TRect; NegativeColor: TColor; PositiveColor: TColor);
        /// <summary>
        /// Sets width of the columns in grid. It takes into account padding and longest text length in row. It requires max col length.
        /// </summary>
        procedure SetColWidth(FirstDefault: integer; AddSpace: integer; Limit: integer);
        /// <summary>
        /// Setup appropiate row height.
        /// </summary>
        procedure SetRowHeight(RowHeight, Header: integer);
        /// <summary>
        /// Hide currently selected column.
        /// </summary>
        procedure HideThisColumns();
        /// <summary>
        /// Setup default column width, this ultimately unhides all hidden columns (width = -1).
        /// </summary>
        procedure ShowAllColumns();
        /// <summary>
        /// Merge sort method.
        /// </summary>
	    procedure MSort(const SortCol: integer; const datatype: TDataType; const ascending: boolean);
        /// <summary>
        /// Auto thumb size custom implementation.
        /// </summary>
        /// <remarks>
        /// Do not use it, string grid scrolls are bugged. Instead, use separate scroll component,
        /// or default string grid scroll behaviour with no auto thumb size.
        /// </remarks>
        procedure AutoThumbSize();
        /// <summary>
        /// Return column namber for given column name. By default it skips first column (Lp) and first row (header).
        /// Fixed column and row must be explicitly provided if defaults cannot be used.
        /// </summary>
        function GetCol(ColumnName: string; FixedCol: integer = 1; FixedRow: integer = 1): integer;
        /// <summary>
        /// Allow to disable component painting. It will not process
        /// the events and will not repaint it, so it can be operated
        /// from worker thread safely.
        /// </summary>
        procedure Freeze(PaintWnd: boolean);
        /// <summary>
        /// Parse CSV data into grids.
        /// </summary>
        function ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
        /// <summary>
        /// Convert all data in TStringGrid to CSV file with choosen delimiter. Note, this method is
        /// executed within main thread, with large dataset it may be more feasible to run it in worker thread.
        /// </summary>
        function ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
        /// <summary>
        /// Export to Excel file current grid content.
        /// </summary>
        procedure ExportXLS(AFileName: string; ASheetName: string);
        /// <summary>
        /// Select all rows and columns (except first row which is presumbly a column title).
        /// </summary>
        procedure SelectAll();
        /// <summary>
        /// Hide all grids (along with header grids).
        /// </summary>
        procedure HideGrids();
        /// <summary>
        /// Show all grids (along with header grids).
        /// </summary>
        procedure ShowGrids();
        /// <summary>
        /// Enable edit option of the cells.
        /// </summary>
        procedure AllowEditing();
        /// <summary>
        /// Disable cells editing.
        /// </summary>
        procedure QuitEditing();
    end;


implementation


uses
    System.Math,
    System.SysUtils,
    System.Classes,
    System.Win.ComObj,
    System.Variants,
    System.Threading,
    Winapi.Messages,
    Vcl.Clipbrd,
    Unity.Constants,
    Unity.Helpers,
    Unity.Sorting,
    Unity.Settings;


procedure TStringGrid.CopyCutPaste(Mode: TActions; FirstColOnly: boolean = False);
begin

    // Paste data into string grid
    if Mode = TActions.Paste then
    begin

        // Get clipboard text
        var RowCounter: integer:=0;
        var NewRows:    integer:=0;
        var NewCols:    integer:=0;
        var Clipbrd:    string:=ClipBoard.AsText;

        // Get dimension from clipboard text
        for var iCNT: integer:=0 to Length(Clipbrd) do
        begin
            // Get number of columns in given row
            if (Clipbrd[iCNT] = TChars.TAB) and (NewRows = 0) then Inc(NewCols);
            // Get number of total rows
            if Clipbrd[iCNT] = TChars.LF then Inc(NewRows);
        end;

        // Split into rows and cols
        var TempRows: TStringList:=TStringList.Create;
        var TempCols: TStringList:=TStringList.Create;
        try

            TempRows.StrictDelimiter:=True;
            TempRows.Delimiter:=TChars.LF;
            TempRows.DelimitedText:=Clipbrd;

            // Set start anchor of new selection
            var Grect: TGridRect:=Selection;
            var RTop:  integer:=GRect.Top;
            var CLeft: integer:=GRect.Left;

            // Look for end anchor of new selection
            for var iCNT: integer:=RTop to RowCount - 1 do
            begin

                // Paste into visible row
                if RowHeights[iCNT] <> -1 then
                begin

                    // Only one column
                    if NewCols = 0 then
                        Cells[CLeft, iCNT]:=TempRows.Strings[RowCounter];

                    // Many columns
                    if NewCols > 0 then
                    begin

                        for var jCNT: integer:=0 to NewCols do
                        begin

                            TempCols.StrictDelimiter:=True;
                            TempCols.Delimiter:=TChars.TAB;
                            TempCols.DelimitedText:=TempRows.Strings[RowCounter];

                            // Paste into columns
                            for var zCNT: integer:=0 to TempCols.Count - 1 do
                            begin
                                Cells[CLeft + zCNT, iCNT]:=TempCols.Strings[zCNT];
                                // Allow only first column to be pasted
                                if FirstColOnly then Break;
                            end;

                        end;

                        TempCols.Clear;

                    end;

                    // Count visible cells starting from
                    // given top position
                    Inc(RowCounter);

                    // Pass row index for last visible cell
                    if RowCounter = NewRows then
                    begin
                        NewRows:=iCNT;
                        Break;
                    end;

                end;

            end;

            // Allow only first column to be pasted
            if FirstColOnly then NewCols:=0;

            // Display new selection for pasted values
            Selection:=TGridRect(Rect(CLeft, RTop, CLeft + NewCols, NewRows));

        finally
            TempRows.Free();
            TempCols.Free();
        end;

    end;

    // Copy/Cut data from string grid
    if (Mode = TActions.Copy) or (Mode = TActions.Cut) then
    begin

        var Sel: TGridRect:=Selection;
        var TxtFromSel: string;
        var Row: integer;
        var Col: integer;

        // Go row by row
        for Row:=Sel.Top to Sel.Bottom do
        begin

            // Skip hidden rows
            if RowHeights[Row] <> -1 then
            begin

                // Go column by column
                for Col:=Sel.Left to Sel.Right do
                begin

                    // Skip hidden columns
                    if ColWidths[Col] <> -1 then
                    begin

                        TxtFromSel:=TxtFromSel + Cells[Col, Row];

                        // Cut
                        if Mode = TActions.Cut then
                            Cells[Col, Row]:='';

                        if Col < Sel.Right then
                            TxtFromSel:=TxtFromSel + TChars.TAB;

                    end;

                end;

                if Row < Sel.Bottom then
                    TxtFromSel:=TxtFromSel + TChars.CRLF;
            end;

        end;

        ClipBoard.AsText:=TxtFromSel + TChars.CRLF;

    end;

end;


procedure TStringGrid.DelEsc(Mode: TActions; pCol, pRow: integer);
begin

    case Mode of
        TActions.Escape: EditorMode:=False;
        TActions.Delete: Cells[pCol, pRow]:='';
    end;

end;


procedure TStringGrid.ClearAll(dfRows: integer; FixedRows: integer; FixedCols: integer; ZeroCol: boolean);
begin

    if not Enabled then Exit;

    var OffSet: integer;
    if ZeroCol then
        OffSet:=1 else OffSet:=0;

    for var iCNT: integer:=FixedRows to Self.RowCount do
        for var jCNT: integer:=(FixedCols - OffSet) to Self.ColCount do
            Cells[jCNT, iCNT]:='';

    RowCount:=dfRows;

end;


procedure TStringGrid.DeleteRowFrom(FixedRow: integer; FixedCol: integer);
begin

    with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);

    // Check for last row.
    if RowCount > FixedRow + 1 then
    begin

        // Remember selected row
        var sRow: integer:=Row;

        // Selection
        var myRect: TGridRect;
        myRect.Left  :=1;
        myRect.Right :=Col;
        myRect.Top   :=Row;
        myRect.Bottom:=Row;

        // Reference string grid to abstract string grid
        Selection:=myRect;

        // Do not assign nil vaue or free it.
        var mySG: TAbstractGrid:=TAbstractGrid(Self);
        mySG.DeleteRow(sRow);

        // Keep selection in place
        if (sRow < (RowCount - 1)) then Row:=sRow;

    end
    else
    begin

        if RowCount = FixedRow + 1 then
            for var iCNT: integer:=FixedRow to RowCount do
                for var jCNT: integer:=FixedCol to ColCount do
                    Cells[jCNT, iCNT]:='';

    end;

    with Self do
    begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Repaint;
    end;

end;


procedure TStringGrid.DrawSelected(ARow: integer; ACol: integer; State: TGridDrawState; Rect: TRect; FontColorSel: TColor; BrushColorSel: TColor; FontColor: TColor; BrushColor: TColor; Headers: boolean);
begin

    // Extend drawing on headers if false
    var FixedColumn: integer:=0;
    var FixedRow:    integer:=0;

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


procedure TStringGrid.ColorValues(ARow: integer; ACol: integer; Rect: TRect; NegativeColor: TColor; PositiveColor: TColor);
begin

    // Get data
    var MyCell: string:=Cells[ACol, ARow];
    var TestCell: string:=MyCell;

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


procedure TStringGrid.SetColWidth(FirstDefault: integer; AddSpace: integer; Limit: integer);
begin

    var tblArray: TArray<integer>;
    var NewWidth: integer;

    if Row > 0 then
        SetLength(tblArray, RowCount)
    else
        Exit();

    ColWidths[0]:=FirstDefault;

    // Iterate throught all the columns
    for var jCNT: integer:=1 to ColCount - 1 do
    begin

        // Iterate throught all rows including actual header
        for var iCNT: integer:=0 to RowCount - 1 do tblArray[iCNT]:=Canvas.TextWidth(Cells[jCNT, iCNT]);

        // Return highest value
        if not (ColWidths[jCNT] = -1) then // Skip hidden columns
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


procedure TStringGrid.SetRowHeight(RowHeight: Integer; Header: Integer);
begin
    DefaultRowHeight:=RowHeight;
    RowHeights[0]:=Header;
end;


procedure TStringGrid.HideThisColumns();
begin
    ColWidths[Col]:=-1;
end;


procedure TStringGrid.ShowAllColumns();
begin

    if goRowSelect in Options then Exit();

    // Skip header
    for var iCNT:=1 to ColCount - 1 do
        if ColWidths[iCNT] = -1 then ColWidths[iCNT]:=100;

end;


procedure TStringGrid.MSort(const SortCol: integer; const DataType: TDataType; const Ascending: boolean);
begin

    var List:  TArray<integer>;
    var TempGrid: TStringGrid:=TStringGrid.create(nil);
    try
        TempGrid.RowCount :=RowCount;
        TempGrid.ColCount :=ColCount;
        TempGrid.FixedRows:=FixedRows;
        SetLength(List, RowCount - FixedRows);

        for var iCNT: integer:=FixedRows to RowCount - 1 do
        begin
            List[iCNT - FixedRows]:=iCNT;
            TempGrid.Rows[iCNT].Assign(Rows[iCNT]);
        end;

        TSorting.MergeSort(Self, List, SortCol, DataType, Ascending);

        for var iCNT: integer:=0 to RowCount - FixedRows - 1 do
        begin
            Rows[iCNT + FixedRows].Assign(TempGrid.Rows[List[iCNT]]);
        end;

        Row:=FixedRows;

    finally
        TempGrid.Free;
    end;

    SetLength(List, 0);

end;


procedure TStringGrid.AutoThumbSize;
begin

    var info: TScrollInfo;

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


function TStringGrid.GetCol(ColumnName: string; FixedCol: integer = 1; FixedRow: integer = 1): integer;
begin

    // This will cause out of bound warning
    Result:=-100;

    for var iCNT: integer:=FixedCol to ColCount - 1 do
    begin
        if Cells[iCNT, FixedRow - 1] = ColumnName then
        begin
            Result:=iCNT;
            Exit();
        end;
    end;

end;


procedure TStringGrid.Freeze(PaintWnd: Boolean);
begin

    if PaintWnd then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;

    if not PaintWnd then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Self.Repaint;
    end;

end;


function TStringGrid.ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
begin

    Result:=False;

    var fPath:   string:=DialogBox.FileName;
    var IsError: boolean:=False;
    var Count:   integer:=0;

    if DialogBox.Execute = True then
    begin

        var Data:    TStringList:=TStringList.Create;
        var Transit: TStringList:=TStringList.Create;
        try
            // Get data from file
            Data.LoadFromFile(fPath);

            // Get columns number
            for var iCNT: integer:=0 to Length(Data[0]) do
                if System.Copy(Data[0], iCNT, 1) = Delimiter then inc(Count);

            // Get rows number and setup offset
            Self.RowCount:=Data.Count + 1;

            // Setup transit that will hold split line
            Transit.StrictDelimiter:=True;
            Transit.Delimiter:=Delimiter[1];

            try
                for var iCNT: integer:= 0 to Data.Count - 1 do
                begin

                    // Split string using given delimiter
                    Transit.DelimitedText:=Data[iCNT];

                    for var jCNT: integer:=1 to Count do
                        Self.Cells[jCNT, iCNT + 1]:=Transit[jCNT - 1];

                    Self.Cells[0, iCNT + 1]:=IntToStr((iCNT + 1));
                    Transit.Clear;

                end;

                Result:=True;

            except
                on E: Exception do
                begin
                    FCsvImportResult:='CSV Import has failed: ' + ExtractFileName(fPath);
                    IsError:=True;
                end;
            end;

        finally

            if not IsError then
                FCsvImportResult:='Data has been imported successfully!';

            Data.Free();
            Transit.Free();

        end;

    end;

end;


function TStringGrid.ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
begin

    Result :=False;
    var IsError: boolean:=False;

    // Write to CSV file
    var CSVData: TStringList:=TStringList.Create;
    try

        var fPath:    string;
        var MyStr:    string;
        var CleanStr: string;

        // Add rows and columns with delimiter
        for var iCNT: integer:=1 to Self.RowCount - 1 do
        begin

            for var jCNT: integer:= 1 to Self.ColCount - 1 do
            begin
                CleanStr :=Self.Cells[jCNT, iCNT];
                CleanStr :=StringReplace(CleanStr, TChars.CRLF, ' ', [rfReplaceAll]);
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
                FCsvExportResult:='Cannot saved file: ' + ExtractFileName(fPath);
                IsError:=True;
            end;
        end;

    finally

        if not IsError then
            FCsvExportResult:='Data has been exported successfully!';

        CSVData.Free;

    end;

end;


procedure TStringGrid.ExportXLS(AFileName: string; ASheetName: string);
begin

    var XLApp: OLEVariant:=CreateOleObject('Excel.Application');
    var Sheet: OLEVariant;
    try

        try

            XLApp.Visible:=False;
            XLApp.Caption:='Unity Platform - Data Export';
            XLApp.DisplayAlerts:=False;
            XLApp.Workbooks.Add(xlWBatWorkSheet);

            Sheet:=XLApp.Workbooks[1].WorkSheets[1];
            Sheet.Name:=ASheetName;

            // Offsets cannot be less than one
            var RowOffset: integer:=1;
            var ColOffset: integer:=1;

            // To Excel sheet
            for var Col:=0 to Self.ColCount - 1 do
                for var Row:=0 to Self.RowCount - 1 do
                    // We skip first string grid column
                    Sheet.Cells[Row + RowOffset, Col + ColOffset]:=Self.Cells[Col + 1, Row];

            // Simple formatting
            for var Col:=0 to Self.ColCount - 1 do Sheet.Columns[Col + ColOffset].ColumnWidth:=15;
            for var Row:=0 to Self.RowCount - 1 do Sheet.Rows[Row + RowOffset].RowHeight:=15;

            // Save to file
            XLApp.Workbooks[1].SaveAs(AFileName);

        finally

            if not VarIsEmpty(XLApp) then
            begin
                XLApp.DisplayAlerts:=False;
                XLApp.Quit;
                XLAPP:=Unassigned;
                Sheet:=Unassigned;
            end;

        end;

    except
        on E: Exception do
        begin
            if E.Message = xlWARN_MESSAGE then
                FToExcelResult:='The data cannot be exported because Microsoft Excel cannot be found.'
            else
                FToExcelResult:='The data cannot be exported, error message has been thrown: ' + E.Message;
        end;

    end;

end;


procedure TStringGrid.SelectAll;
begin
    var GridRect: TGridRect;
    GridRect.Left  :=1;
    GridRect.Top   :=1;
    GridRect.Right :=Self.ColCount;
    GridRect.Bottom:=Self.RowCount;
    Self.Selection :=GridRect;
end;


procedure TStringGrid.ShowGrids;
begin
    Self.Options:=Self.Options
        + [goFixedVertLine]
        + [goFixedHorzLine]
        + [goVertLine]
        + [goHorzLine];
end;


procedure TStringGrid.HideGrids;
begin
    Self.Options:=Self.Options
        - [goFixedVertLine]
        - [goFixedHorzLine]
        - [goVertLine]
        - [goHorzLine];
end;


procedure TStringGrid.AllowEditing();
begin
    Self.Options:=Self.Options + [goEditing];
    Self.EditorMode:=True;
end;


procedure TStringGrid.QuitEditing();
begin
    Self.Options:=Self.Options - [goEditing];
    Self.EditorMode:=False;
end;


end.

