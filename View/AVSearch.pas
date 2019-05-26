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
        var FoundRow   :  integer;
        var IsNext     :  boolean;
        var Groupping  :  array of integer;
    public
        var SGrid     :  TStringGrid;
        var SColName  :  string;
        var SColNumber:  string;
        var SearchEnd :  integer;
    published
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

    var
        IsNumber:      boolean;
        SearchString:  string;
        CompareValue:  string;
        iCNT:          integer;
        SearchColumn:  integer;

    procedure SearchPartial_Prepare;
    begin
        SetLength(Groupping, SGrid.RowCount);
        if not (CaseSensitive.Checked) then CompareValue:=UpperCase(SGrid.Cells[SearchColumn, iCNT]);
        if (CaseSensitive.Checked)     then CompareValue:=SGrid.Cells[SearchColumn, iCNT];
    end;

    procedure SearchPartial_NextBreak;
    begin
        IsNext:=True;

        if SGrid.RowHeights[FoundRow] = -1 then
        MainForm.MsgCall(
            Info,
            'The item has been found (' + SGrid.Cells[SearchColumn, FoundRow] + ') for search pattern "' +
            SearchString + '". ' + TUChars.CRLF +
            'However, it is hidden by the filter. Remove filtering to unhide this item.'
        );

    end;

    procedure SearchPartial_ShowAll;
    begin

        // Put corresponding rows into an array
        if (ShowAll.Checked) then
        begin
            Groupping[iCNT]:=FoundRow;
            IsNext:=False;
        end;

    end;

begin

    SGrid.Freeze(True);
    SearchColumn:=0;
    if not (CaseSensitive.Checked) then SearchString:=UpperCase(EditSearch.Text);
    if (CaseSensitive.Checked)     then SearchString:=EditSearch.Text;

    // Cannot be empty
    if (SearchString = '') or (SearchString = TUChars.SPACE) then
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
    if (IsNumber)     then SearchColumn:=SGrid.ReturnColumn(SColNumber, 1, 1);
    if not (IsNumber) then SearchColumn:=SGrid.ReturnColumn(SColName,   1, 1);

    // Search direction (up)
    if CheckUp.Checked then
    begin
        if (IsNext) and (FoundRow > SearchEnd) then FoundRow:=FoundRow - 1;
        SearchEnd:=1;
    end;

    // Search direction (down)
    if CheckDown.Checked then
    begin
        if (IsNext) and (FoundRow < SearchEnd) then FoundRow:=FoundRow + 1;
        SearchEnd:=SGrid.RowCount - 1;
    end;

    SetLength(Groupping, 0);

    // Search up
    if CheckUp.Checked then
    begin
        for iCNT:=FoundRow downto SearchEnd do
        begin
            SearchPartial_Prepare;

            if Pos(SearchString, CompareValue) > 0 then
            begin
                FoundRow:=iCNT;
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
                IsNext  :=False;
                FoundRow:=0;
            end;
        end;
    end;

    // Search down
    if CheckDown.Checked then
    begin
        for iCNT:=FoundRow to SearchEnd do
        begin
            SearchPartial_Prepare;
            if Pos(SearchString, CompareValue) > 0 then
            begin
                FoundRow:=iCNT;
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
                IsNext  :=False;
                FoundRow:=0;
            end;
        end;
    end;

    // Highlight found row
    if not (ShowAll.Checked) then
    begin
        if not (FoundRow = 0) then
        begin
            SGrid.Row:=FoundRow;
            SGrid.Col:=SGrid.ReturnColumn(SColName, 1, 1);
        end
            else
                MainForm.MsgCall(Info, 'Cannot find specified customer.');
    end;

    // Show all found
    if (ShowAll.Checked) then
    begin
        // Make sure that all wors are visible
        SGrid.DefaultRowHeight:=SGrid.sgRowHeight;
        if High(Groupping) > 0 then
        begin
            for iCNT:=0 to SGrid.RowCount - 1 do

            if Groupping[iCNT] <> iCNT then
                SGrid.RowHeights[iCNT]:=SGrid.sgRowHidden;

            btnUnhide.Enabled:=True;
        end
        else
        begin
            MainForm.MsgCall(Info, 'Cannot find specified customer.');
            btnUnhide.Enabled:=False;
        end;
    end;

    SGrid.Freeze(False);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TSearchForm.FormCreate(Sender: TObject);
begin
    PanelEditSearch.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    SColName  :='';
    SColNumber:='';
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
    FoundRow:=0;
    IsNext:=False;
    EditSearch.Text:='';
    CheckUp.Checked:=False;
    CheckDown.Checked:=True;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


procedure TSearchForm.btnSearchClick(Sender: TObject);
begin
    if (SColName <> '') and (SColNumber <> '') and (SGrid <> nil) then Search;
end;


procedure TSearchForm.btnUnhideClick(Sender: TObject);
begin
    if (SColName <> '') and (SColNumber <> '') and (SGrid <> nil) then
    begin
        MainForm.Action_RemoveFiltersClick(Self);
        btnUnhide.Enabled:=False;
    end;
end;


end.

