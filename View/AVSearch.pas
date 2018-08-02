{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit AVSearch;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, InterposerClasses;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
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
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Main, Filter, Settings;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------ SEARCH IN GIVEN STRING }
procedure TSearchForm.Search;

  (* COMMON VARIABLES *)

  var
    IsNumber:      boolean;
    SearchString:  string;
    CompareValue:  string;
    iCNT:          integer;
    SearchColumn:  integer;

  (* NESTED METHODS *)

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
                        mcInfo,
                        'The item has been found (' + SGrid.Cells[SearchColumn, FoundRow] + ') for search pattern "' +
                        SearchString + '". ' + CRLF +
                        'However, it is hidden by the filter. Remove filtering to unhide search item.'
                      );
  end;

  procedure SearchPartial_ShowAll;
  begin
    { PUT CORRESPONDING ROWS INTO AN ARRAY }
    if (ShowAll.Checked) then
    begin
      Groupping[iCNT]:=FoundRow;
      IsNext:=False;
    end;
  end;

begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  SGrid.Freeze(True);
  SearchColumn:=0;
  if not (CaseSensitive.Checked) then SearchString:=UpperCase(EditSearch.Text);
  if (CaseSensitive.Checked)     then SearchString:=EditSearch.Text;

  { ----------------------------------------------------------------------------------------------------------------------------------------- CANNOT BE EMPTY }
  if (SearchString = '') or (SearchString = SPACE) then
  begin
    MainForm.MsgCall(mcWarn, 'Cannot search empty string. Please provide with customer name or customer number and try again.');
    Exit;
  end;

  { --------------------------------------------------------------------------------------------------------------------------- CHECK IF USER PROVIDED NUMBER }
  IsNumber:=True;
  try
    StrToInt64(EditSearch.Text);
  except
    IsNumber:=False;
  end;

  { --------------------------------------------------------------------------------------------------------------- ASSIGN PROPER COLUMN NUMBER FROM AGE VIEW }
  if (IsNumber)     then SearchColumn:=SGrid.ReturnColumn(SColNumber, 1, 1);
  if not (IsNumber) then SearchColumn:=SGrid.ReturnColumn(SColName,   1, 1);

  { ----------------------------------------------------------------------------------------------------------------------------------- SEARCH DIRECTION | UP }
  if CheckUp.Checked then
  begin
    if (IsNext) and (FoundRow > SearchEnd) then FoundRow:=FoundRow - 1;
    SearchEnd:=1;
  end;

  { --------------------------------------------------------------------------------------------------------------------------------- SEARCH DIRECTION | DOWN }
  if CheckDown.Checked then
  begin
    if (IsNext) and (FoundRow < SearchEnd) then FoundRow:=FoundRow + 1;
    SearchEnd:=SGrid.RowCount - 1;
  end;

  SetLength(Groupping, 0);

  { ----------------------------------------------------------------------------------------------------------------------------------------------- SEARCH UP }
  if CheckUp.Checked then
  begin
    for iCNT:=FoundRow downto SearchEnd do
    begin
      SearchPartial_Prepare;
      if Pos(SearchString, CompareValue) > 0 then
      begin
        FoundRow:=iCNT;
        { EXIT ON FOUND GIVEN ITEM }
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

  { --------------------------------------------------------------------------------------------------------------------------------------------- SEARCH DOWN }
  if CheckDown.Checked then
  begin
    for iCNT:=FoundRow to SearchEnd do
    begin
      SearchPartial_Prepare;
      if Pos(SearchString, CompareValue) > 0 then
      begin
        FoundRow:=iCNT;
        { EXIT ON FOUND GIVEN ITEM }
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

  { ------------------------------------------------------------------------------------------------------------------------------------- HIGHLIGHT FOUND ROW }
  if not (ShowAll.Checked) then
  begin
    if not (FoundRow = 0) then
    begin
      SGrid.Row:=FoundRow;
      SGrid.Col:=SGrid.ReturnColumn(SColName, 1, 1);
    end
      else
        MainForm.MsgCall(mcInfo, 'Cannot find specified customer.');
  end;

  { ------------------------------------------------------------------------------------------------------------------------------------------ SHOW ALL FOUND }
  if (ShowAll.Checked) then
  begin
    { MAKE SURE THAT ALL ROWS ARE VISIBLE }
    SGrid.DefaultRowHeight:=sgRowHeight;
    if High(Groupping) > 0 then
    begin
      for iCNT:=0 to SGrid.RowCount - 1 do
        if Groupping[iCNT] <> iCNT then SGrid.RowHeights[iCNT]:=sgRowHidden;
      btnUnhide.Enabled:=True;
    end
    else
    begin
      MainForm.MsgCall(mcInfo, 'Cannot find specified customer.');
      btnUnhide.Enabled:=False;
    end;
  end;

  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  SGrid.Freeze(False);

end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TSearchForm.FormCreate(Sender: TObject);
var
  AppSettings:  TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    SearchForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_SEARCH', APPCAPTION);
  finally
    AppSettings.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  PanelEditSearch.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  SColName  :='';
  SColNumber:='';
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- SEARCH ON ENTER }
procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then btnSearchClick(Self);
end;

{ ----------------------------------------------------------------------------------------------------------------- PERFORM SEARCH BY CUSTOMER NAME OR NUMBER }
procedure TSearchForm.btnSearchClick(Sender: TObject);
begin
  if (SColName <> '') and (SColNumber <> '') and (SGrid <> nil) then Search;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- MAKE ALL ROWS VISIBLE }
procedure TSearchForm.btnUnhideClick(Sender: TObject);
begin
  if (SColName <> '') and (SColNumber <> '') and (SGrid <> nil) then
  begin
    MainForm.Action_RemoveFiltersClick(Self);
    btnUnhide.Enabled:=False;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- RESET SETTINIGS }
procedure TSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FoundRow:=0;
  IsNext:=False;
  EditSearch.Text:='';
  CheckUp.Checked:=False;
  CheckDown.Checked:=True;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TSearchForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.
