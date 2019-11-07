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
    Data.Win.ADODB,
    Unity.Arrays,
    Unity.Enums;


type


    /// <summary>
    ///
    /// </summary>
    TAbstractGrid = class(Vcl.Grids.TStringGrid);


    /// <summary>
    ///
    /// </summary>
    TStringGrid = class(Vcl.Grids.TStringGrid)
    protected
        procedure Paint; override;
    strict private
        var FHideFocusRect: boolean;
        var FToExcelResult: string;
        var FCsvImportResult: string;
        var FCsvExportResult: string;
    public
        const sgRowHeight = 19;
        const sgRowHidden = -1;
        const xlWBATWorksheet = -4167;
        const xlWARN_MESSAGE  = 'Invalid class string';
        const FFontWhite  = $00FFFFFF;
        const FBackRed    = $008080FF; //rgb FF8080 => bgr 8080FF
        const FFontBlack  = $00000000;
        const FBackYellow = $00CCFFFF; //rgb FFFFCC => bgr CCFFFF
        const FBackGreen  = $00ACAC59; //rgb 59ACAC => bgr ACAC59
        var SqlColumns: TALists;
        var UpdatedRowsHolder: TAIntigers;
        property  HideFocusRect: boolean read FHideFocusRect write FHideFocusRect;
        property  ToExcelResult: string read FToExcelResult;
        property  CsvImportResult: string read FCsvImportResult;
        property  CsvExportResult: string read FCsvExportResult;
        procedure SetUpdatedRow(Row: integer);
        procedure RecordRowsAffected;
        procedure CopyCutPaste(Mode: TActions; FirstColOnly: boolean = False);
        procedure DelEsc(Mode: TActions; pCol, pRow: integer);
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
        procedure Freeze(PaintWnd: boolean);
        function  ToExcel(ASheetName, AFileName: string; GroupId: string; AgeDate: string; ActiveConn: TADOConnection): boolean;
        function  ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
        function  ExportCSV(DialogBox: TSaveDialog; Delimiter: string): boolean;
        procedure SelectAll;
        procedure HideGrids;
        procedure ShowGrids;
    end;


implementation


uses
    System.Math,
    System.SysUtils,
    System.Classes,
    System.Win.ComObj,
    System.Variants,
    Winapi.Messages,
    Vcl.Clipbrd,
    Unity.Chars,
    Unity.StatusBar,
    Unity.Helpers,
    Unity.Messaging,
    Unity.Sql,
    Unity.Sorting,
    Unity.Settings,
    Handler.Sql;


/// <summary>
/// Register rows for update. Keeps row number to be updated, eg. if data is held in string grid popuated from database table, then to build SQL batch
/// with update statement, we must take all rows changed by the user - once cell(s) is(are) changed we add row to the register. Later such information
/// can be used to build SQL update expression.
/// </summary>

procedure TStringGrid.SetUpdatedRow(Row: integer);
begin

    var Rows: integer;

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


procedure TStringGrid.RecordRowsAffected;
begin

    if Selection.Top - Selection.Bottom = 0 then
    begin
        SetUpdatedRow(Row);
    end
    else
    begin
        for var iCNT: integer:=Selection.Top to Selection.Bottom do
            if RowHeights[iCNT] = sgRowHeight then
                SetUpdatedRow(iCNT);
    end;

end;


procedure TStringGrid.Paint;
begin

    inherited;

    if HideFocusRect then
    begin
        var FocusRect: TRect:=CellRect(Col, Row);
        if DrawingStyle = gdsThemed then InflateRect(FocusRect, -1, -1);
        DrawFocusRect(Canvas.Handle, FocusRect);
    end;

end;


procedure TStringGrid.CopyCutPaste(Mode: TActions; FirstColOnly: boolean = False{Option});
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
            TempRows.Free;
            TempCols.Free;
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

    // Disable drawing
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

        /// <remarks>
        /// Do not nil or free it.
        /// </remarks>
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

    // Enable drawing
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

    var tblArray: TAIntigers;
    var NewWidth: integer;

    if Row > 0 then
        SetLength(tblArray, RowCount)
            else
                Exit;

    ColWidths[0]:=FirstDefault;

    // Iterate throught all the columns
    for var jCNT: integer:=1 to ColCount - 1 do
    begin

        // Iterate throught all rows including actual header
        for var iCNT: integer:=0 to RowCount - 1 do tblArray[iCNT]:=Canvas.TextWidth(Cells[jCNT, iCNT]);

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


procedure TStringGrid.SetRowHeight(RowHeight: Integer; Header: Integer);
begin
    DefaultRowHeight:=RowHeight;
    RowHeights[0]:=Header;
end;


procedure TStringGrid.MSort(const SortCol: integer; const DataType: integer; const Ascending: boolean);
begin

    var List:  TAIntigers;
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


/// <summary>
/// Auto thumb size custom implementation.
/// </summary>
/// <remarks>
/// Do not use it, string grid scrolls are bugged. Instead, use separate scroll component,
/// or default string grid scroll behaviour with no auto thumb size.
/// </remarks>

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


/// <summary>
/// String grid column layout. It contains with two headers, one is displayed to the user (column title), and second
/// is used to hold original SQL column name, that other can refer and perform SQL queries.
/// </summary>

procedure TStringGrid.SaveLayout(ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string);
begin

    var Settings: ISettings:=TSettings.Create;

    // Column width
    for var iCNT: integer:=0 to Self.ColCount - 1 do
    begin
        if iCNT = 0 then Settings.SetIntegerValue(ColWidthName, ColPrefix + IntToStr(iCNT), 10);
        if iCNT > 0 then Settings.SetIntegerValue(ColWidthName, ColPrefix + IntToStr(iCNT), Self.ColWidths[iCNT]);
    end;

    // SQL column name
    for var iCNT: integer:=0 to Self.ColCount - 1 do
        Settings.SetStringValue(ColOrderName, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 0]);

    // Column title
    for var iCNT: integer:=0 to Self.ColCount - 1 do
    begin
        if iCNT = 0 then Settings.SetStringValue(ColNames, ColPrefix + IntToStr(iCNT), '');
        if iCNT > 0 then Settings.SetStringValue(ColNames, ColPrefix + IntToStr(iCNT), Self.SqlColumns[iCNT, 1]);
    end;

    // Encode
    Settings.Encode(TAppFiles.Configuration);

end;


/// <summary>
/// Load layout from application settings.
/// </summary>
/// <remarks>
/// 'Colordername' and 'colwidthname' provide the section names for column order and column width.
/// both sections must contains equal number of value keys. Each key contain column name used by
/// string grid component (age view) that displays data from sql server database, thus column names
/// are used to build sql query, this is because we use SQL expressions to obtain initial output
/// with filtering and/or sorting etc. Separate filtering to some extend is allowed in string grid
/// however, separate sorting is not implemented to restrict user form "playing around"
/// therefore, there is one place (server) where there is decided how to display data to the user,
/// this is part of automation and standard approach across all users, so the user is forced
/// to work certain way, and thus in management view user shall obtain better results, etc.
/// </remarks>

function TStringGrid.LoadLayout(var StrCol: string; ColWidthName: string; ColOrderName: string; ColNames: string; ColPrefix: string): boolean;
begin

    // Check number of keys in given section
    var ColOrderSec: TStringList:=TStringList.Create;
    var ColWidthSec: TStringList:=TStringList.Create;
    var ColNamesSec: TStringList:=TStringList.Create;
    var Settings: ISettings:=TSettings.Create;
    try

        try

            Settings.GetSection(ColWidthName, ColWidthSec);
            Settings.GetSection(ColOrderName, ColOrderSec);
            Settings.GetSection(ColNames, ColNamesSec);

            if (ColWidthSec.Count = ColOrderSec.Count) and (ColWidthSec.Count = ColNamesSec.Count) then
            begin

                Self.ColCount:=ColWidthSec.Count;
                SetLength(Self.SqlColumns, Self.ColCount, 2);

                for var iCNT: integer:=0 to Self.ColCount - 1 do
                begin

                    // Skip first column as it holds empty column (by design, we do not display ID in first column, etc.)
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

            Result:=True;

        except
            Result:=False;
        end;

    finally
        ColWidthSec.Free;
        ColOrderSec.Free;
        ColNamesSec.Free;
    end;

end;


function TStringGrid.ReturnColumn(ColumnName: string; FixedCol: integer; FixedRow: integer): integer;
begin

    Result:=-100; // Warning! Out of bound by default

    for var iCNT: integer:=FixedCol to ColCount - 1 do
    begin
        if Cells[iCNT, FixedRow - 1] = ColumnName then
        begin
            Result:=iCNT;
            Exit;
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


/// <summary>
/// Export string grid content to Microsoft Excel file.
/// </summary>
/// <remarks>
/// User must have installed Microsoft Excel with VBA installed before exporting string grid to excel file.
/// otherwise error message "invalid string class" will occur.
/// This method should be run in worker thread.
/// </remarks>

function TStringGrid.ToExcel(ASheetName: string; AFileName: string; GroupId: string; AgeDate: string; ActiveConn: TADOConnection): boolean;
begin

    Result:=False;

    var DataTables: TDataTables:=TDataTables.Create(ActiveConn);
    try
        DataTables.StrSQL:=TSql.EXECUTE + DataTables.AgeViewExport + TChars.SPACE + QuotedStr(GroupId) + TChars.COMMA + QuotedStr(AgeDate);
        DataTables.SqlToGrid(Self, DataTables.ExecSQL, False, True);
    finally
        DataTables.Free;
    end;

    // Initiate Excel application
    try

        var XLApp: OLEVariant:=CreateOleObject('Excel.Application');
        var Sheet: OLEVariant;

        try

            XLApp.Visible:=False;
            XLApp.Caption:='Unity For Debt Management - Data Export';
            XLApp.DisplayAlerts:=False;
            XLApp.Workbooks.Add(xlWBatWorkSheet);

            Sheet:=XLApp.Workbooks[1].WorkSheets[1];
            Sheet.Name:=ASheetName;

            /// <remarks>Offsets cannot be less than one.</remarks>
            var RowOffset: integer:=1;
            var ColOffset: integer:=1;

            // To Excel sheet
            for var Col: integer:=0 to Self.ColCount - 1 do
                for var Row: integer:=0 to Self.RowCount - 1 do
                    // We mitt first string grid column
                    Sheet.Cells[Row + RowOffset, Col + ColOffset]:=Self.Cells[Col + 1, Row];

            // Simple formatting (this can be extended
            for var Col: integer:=0 to Self.ColCount - 1 do Sheet.Columns[Col + ColOffset].ColumnWidth:=15;
            for var Row: integer:=0 to Self.RowCount - 1 do Sheet.Rows[Row + RowOffset].RowHeight:=15;

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
                    FToExcelResult:='The data has been successfully transferred to Excel.';

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


function TStringGrid.ImportCSV(DialogBox: TOpenDialog; Delimiter: string): boolean;
begin

    Result:=False;

    var fPath:   string:=DialogBox.FileName;
    var IsError: boolean:=False;
    var Count:   integer:=0;

    // GET THE FILE PATH AND PARSE
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

            // Iterate through all rows
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

            Data.Free;
            Transit.Free;

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


/// <summary>
/// Select all rows and columns (except first row which is presumbly a column title).
/// </summary>

procedure TStringGrid.SelectAll;
begin
    var GridRect: TGridRect;
    GridRect.Left  :=1;
    GridRect.Top   :=1;
    GridRect.Right :=Self.ColCount;
    GridRect.Bottom:=Self.RowCount;
    Self.Selection :=GridRect;
end;


/// <summary>
/// Show all grids (along with header grids).
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
/// Hide all grids (along with header grids).
/// </summary>

procedure TStringGrid.HideGrids;
begin
    Self.Options:=Self.Options
        - [goFixedVertLine]
        - [goFixedHorzLine]
        - [goVertLine]
        - [goHorzLine];
end;


end.

