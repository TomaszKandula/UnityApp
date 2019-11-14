unit View.GridSearch;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    Unity.Enums,
    Unity.Grid,
    Unity.Panel,
    Unity.Arrays;


type


    TGridSearchForm = class(TForm)
        btnSearch: TSpeedButton;
        CaseSensitive: TCheckBox;
        ShowAll: TCheckBox;
        btnUnhide: TSpeedButton;
        GroupSearch: TGroupBox;
        GroupOptions: TGroupBox;
        CheckUp: TRadioButton;
        CheckDown: TRadioButton;
        PanelEditBox: TPanel;
        PanelEditSearch: TPanel;
        EditSearch: TEdit;
        TextSearch: TLabel;
        PanelMain: TPanel;
        TextWarn: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure btnSearchClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnUnhideClick(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    strict private
        var FFoundRow:     integer;
        var FIsNext:       boolean;
        var FGroupping:    TAIntigers;
        var FSearchEnd:    integer;
        var FIsNumber:     boolean;
        var FSearchString: string;
        var FCompareValue: string;
        var FSearchColumn: integer;
        var FActualRow:    integer;
        procedure InitSearch;
        function  TryToInt(StrInput: string): boolean;
        function  SearchPartialPrepare(ActualRow: integer): string;
        procedure SearchPartialShowAll(ActualRow: integer);
        procedure SearchPartialNextBreak();
    public
        var FGrid:      TStringGrid;
        var FColName:   string;
        var FColNumber: string;
        procedure PerformSearch();
    end;


    function GridSearchForm(): TGridSearchForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.GridFilter,
    Unity.Chars,
    Unity.Helpers,
    Unity.Settings;


var vGridSearchForm: TGridSearchForm;


function GridSearchForm(): TGridSearchForm;
begin
    if not(Assigned(vGridSearchForm)) then Application.CreateForm(TGridSearchForm, vGridSearchForm);
    Result:=vGridSearchForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


function TGridSearchForm.TryToInt(StrInput: string): boolean;
begin
    Result:=True;
    try
        StrToInt64(StrInput);
    except
        Result:=False;
    end;
end;


function TGridSearchForm.SearchPartialPrepare(ActualRow: integer): string;
begin

    SetLength(FGroupping, FGrid.RowCount);

    if not CaseSensitive.Checked then
        Result:=UpperCase(FGrid.Cells[FSearchColumn, ActualRow])
    else
        Result:=FGrid.Cells[FSearchColumn, ActualRow];

end;


procedure TGridSearchForm.SearchPartialShowAll(ActualRow: integer);
begin

    // Put corresponding rows into an array
    if (ShowAll.Checked) then
    begin
        FGroupping[ActualRow]:=FFoundRow;
        FIsNext:=False;
    end;

end;


procedure TGridSearchForm.SearchPartialNextBreak();
begin

    FIsNext:=True;

    if FGrid.RowHeights[FFoundRow] = -1 then
    THelpers.MsgCall(
        Info,
        'The item has been found (' + FGrid.Cells[FSearchColumn, FFoundRow] + ') for search pattern "' +
        FSearchString + '". ' + TChars.CRLF +
        'However, it is hidden by the filter. Remove filtering to unhide this item.'
    );

end;


procedure TGridSearchForm.InitSearch();
begin

    FCompareValue:='';
    FSearchString:=EditSearch.Text;
    FSearchColumn:=0;
    FActualRow   :=0;

    if (FSearchString = '') or (FSearchString = TChars.SPACE) then
    begin
        THelpers.MsgCall(Warn, 'Cannot search empty string. Please provide with customer name or customer number and try again.');
        Exit();
    end;

    FIsNumber:=TryToInt(FSearchString);
    if not CaseSensitive.Checked then FSearchString:=UpperCase(FSearchString) else FSearchString:=FSearchString;
    if FIsNumber then FSearchColumn:=FGrid.ReturnColumn(FColNumber, 1, 1) else FSearchColumn:=FGrid.ReturnColumn(FColName, 1, 1);

    SetLength(FGroupping, 0);

end;


procedure TGridSearchForm.PerformSearch();
begin

    InitSearch();
    FGrid.Freeze(True);

    // Search up
    if CheckUp.Checked then
    begin

        if (FIsNext) and (FFoundRow > FSearchEnd) then FFoundRow:=FFoundRow - 1;
        FSearchEnd:=1;

        for var iCNT: integer:=FFoundRow downto FSearchEnd do
        begin

            FCompareValue:=SearchPartialPrepare(iCNT);

            if Pos(FSearchString, FCompareValue) > 0 then
            begin

                FFoundRow:=iCNT;

                // Exit on found given item
                if not (ShowAll.Checked) then
                begin
                    SearchPartialNextBreak();
                    Break;
                end;

                SearchPartialShowAll(iCNT);

            end
            else
            begin
                FIsNext  :=False;
                FFoundRow:=0;
            end;
        end;

    end;

    // Search down
    if CheckDown.Checked then
    begin

        if (FIsNext) and (FFoundRow < FSearchEnd) then FFoundRow:=FFoundRow + 1;
        FSearchEnd:=FGrid.RowCount - 1;

        for var iCNT: integer:=FFoundRow to FSearchEnd do
        begin

            FCompareValue:=SearchPartialPrepare(iCNT);

            if Pos(FSearchString, FCompareValue) > 0 then
            begin

                FFoundRow:=iCNT;

                // Exit when found given item
                if not (ShowAll.Checked) then
                begin
                    SearchPartialNextBreak;
                    Break;
                end;

                SearchPartialShowAll(iCNT);

            end
            else
            begin
                FIsNext  :=False;
                FFoundRow:=0;
            end;

        end;

    end;

    if not ShowAll.Checked then
    begin

        // Highlight row of found items
        if not (FFoundRow = 0) then
        begin
            FGrid.Row:=FFoundRow;
            FGrid.Col:=FGrid.ReturnColumn(FColName, 1, 1);
        end
        else
        THelpers.MsgCall(Info, 'Cannot find specified customer.');

    end
    else
    begin

        // Make sure that all rows are visible
        FGrid.DefaultRowHeight:=FGrid.sgRowHeight;

        // Show all itemns when found
        if High(FGroupping) > 0 then
        begin

            for var iCNT: integer:=0 to FGrid.RowCount - 1 do
            if FGroupping[iCNT] <> iCNT then FGrid.RowHeights[iCNT]:=FGrid.sgRowHidden;

            btnUnhide.Enabled:=True;

        end
        else
        begin
            THelpers.MsgCall(Info, 'Cannot find specified customer.');
            btnUnhide.Enabled:=False;
        end;

    end;

    FGrid.Freeze(False);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TGridSearchForm.FormCreate(Sender: TObject);
begin
    PanelEditSearch.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    FColName  :='';
    FColNumber:='';
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TGridSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then btnSearchClick(Self);
end;


procedure TGridSearchForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


procedure TGridSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FFoundRow:=0;
    FIsNext:=False;
    EditSearch.Text:='';
    CheckUp.Checked:=False;
    CheckDown.Checked:=True;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- CLICK EVENTS //


procedure TGridSearchForm.btnSearchClick(Sender: TObject);
begin
    if (FColName <> '') and (FColNumber <> '') and (FGrid <> nil) then PerformSearch();
end;


procedure TGridSearchForm.btnUnhideClick(Sender: TObject);
begin
    if (FColName <> '') and (FColNumber <> '') and (FGrid <> nil) then
    begin
        MainForm.Action_RemoveFiltersClick(Self);
        btnUnhide.Enabled:=False;
    end;
end;


end.

