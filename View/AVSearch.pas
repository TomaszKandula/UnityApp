unit AVSearch;


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
    InterposerClasses;


type


    TSearchForm = class(TForm)
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
    private
        var FFoundRow:  integer;
        var FIsNext:    boolean;
        var FGroupping: array of integer;
        var FSearchEnd: integer;
    public
        var FGrid:      TStringGrid;
        var FColName:   string;
        var FColNumber: string;
        procedure Search;
    end;


var
    SearchForm: TSearchForm;


implementation


uses
    Main,
    Filter,
    Settings,
    Helpers;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Perform search routine for given string (can be a number or varchar).
/// </summary>

procedure TSearchForm.Search;

    var IsNumber:      boolean;
    var SearchString:  string;
    var CompareValue:  string;
    var iCNT:          integer;
    var SearchColumn:  integer;

    procedure SearchPartial_Prepare;
    begin
        SetLength(FGroupping, FGrid.RowCount);
        if not (CaseSensitive.Checked) then CompareValue:=UpperCase(FGrid.Cells[SearchColumn, iCNT]);
        if (CaseSensitive.Checked)     then CompareValue:=FGrid.Cells[SearchColumn, iCNT];
    end;

    procedure SearchPartial_NextBreak;
    begin
        FIsNext:=True;

        if FGrid.RowHeights[FFoundRow] = -1 then
        MainForm.MsgCall(
            Info,
            'The item has been found (' + FGrid.Cells[SearchColumn, FFoundRow] + ') for search pattern "' +
            SearchString + '". ' + TChars.CRLF +
            'However, it is hidden by the filter. Remove filtering to unhide this item.'
        );

    end;

    procedure SearchPartial_ShowAll;
    begin

        // Put corresponding rows into an array
        if (ShowAll.Checked) then
        begin
            FGroupping[iCNT]:=FFoundRow;
            FIsNext:=False;
        end;

    end;

begin

    FGrid.Freeze(True);
    SearchColumn:=0;
    if not (CaseSensitive.Checked) then SearchString:=UpperCase(EditSearch.Text);
    if (CaseSensitive.Checked)     then SearchString:=EditSearch.Text;

    // Cannot be empty
    if (SearchString = '') or (SearchString = TChars.SPACE) then
    begin
        MainForm.MsgCall(Warn, 'Cannot search empty string. Please provide with customer name or customer number and try again.');
        Exit;
    end;

    // Check if user provided number
    IsNumber:=True;
    try
        StrToInt64(EditSearch.Text);
    except
        IsNumber:=False;
    end;

    // Assign proper column number from age view
    if (IsNumber)     then SearchColumn:=FGrid.ReturnColumn(FColNumber, 1, 1);
    if not (IsNumber) then SearchColumn:=FGrid.ReturnColumn(FColName,   1, 1);

    // Search direction (up)
    if CheckUp.Checked then
    begin
        if (FIsNext) and (FFoundRow > FSearchEnd) then FFoundRow:=FFoundRow - 1;
        FSearchEnd:=1;
    end;

    // Search direction (down)
    if CheckDown.Checked then
    begin
        if (FIsNext) and (FFoundRow < FSearchEnd) then FFoundRow:=FFoundRow + 1;
        FSearchEnd:=FGrid.RowCount - 1;
    end;

    SetLength(FGroupping, 0);

    // Search up
    if CheckUp.Checked then
    begin
        for iCNT:=FFoundRow downto FSearchEnd do
        begin
            SearchPartial_Prepare;

            if Pos(SearchString, CompareValue) > 0 then
            begin
                FFoundRow:=iCNT;
                // Exit on found given item
                if not (ShowAll.Checked) then
                begin
                    SearchPartial_NextBreak;
                    Break;
                end;
                SearchPartial_ShowAll;
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
        for iCNT:=FFoundRow to FSearchEnd do
        begin
            SearchPartial_Prepare;
            if Pos(SearchString, CompareValue) > 0 then
            begin
                FFoundRow:=iCNT;
                // Exit when found given item
                if not (ShowAll.Checked) then
                begin
                    SearchPartial_NextBreak;
                    Break;
                end;
                SearchPartial_ShowAll;
            end
            else
            begin
                FIsNext  :=False;
                FFoundRow:=0;
            end;
        end;
    end;

    // Highlight found row
    if not (ShowAll.Checked) then
    begin
        if not (FFoundRow = 0) then
        begin
            FGrid.Row:=FFoundRow;
            FGrid.Col:=FGrid.ReturnColumn(FColName, 1, 1);
        end
            else
                MainForm.MsgCall(Info, 'Cannot find specified customer.');
    end;

    // Show all found
    if (ShowAll.Checked) then
    begin
        // Make sure that all wors are visible
        FGrid.DefaultRowHeight:=FGrid.sgRowHeight;
        if High(FGroupping) > 0 then
        begin
            for iCNT:=0 to FGrid.RowCount - 1 do

            if FGroupping[iCNT] <> iCNT then
                FGrid.RowHeights[iCNT]:=FGrid.sgRowHidden;

            btnUnhide.Enabled:=True;
        end
        else
        begin
            MainForm.MsgCall(Info, 'Cannot find specified customer.');
            btnUnhide.Enabled:=False;
        end;
    end;

    FGrid.Freeze(False);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TSearchForm.FormCreate(Sender: TObject);
begin
    PanelEditSearch.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    FColName  :='';
    FColNumber:='';
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then btnSearchClick(Self);
end;


procedure TSearchForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


procedure TSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FFoundRow:=0;
    FIsNext:=False;
    EditSearch.Text:='';
    CheckUp.Checked:=False;
    CheckDown.Checked:=True;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


procedure TSearchForm.btnSearchClick(Sender: TObject);
begin
    if (FColName <> '') and (FColNumber <> '') and (FGrid <> nil) then Search;
end;


procedure TSearchForm.btnUnhideClick(Sender: TObject);
begin
    if (FColName <> '') and (FColNumber <> '') and (FGrid <> nil) then
    begin
        MainForm.Action_RemoveFiltersClick(Self);
        btnUnhide.Enabled:=False;
    end;
end;


end.

