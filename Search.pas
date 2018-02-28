{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Search;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, Main;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TSearchForm = class(TForm)
    AppMain: TShape;
    btnSearch: TSpeedButton;
    EditSearch: TLabeledEdit;
    CaseSensitive: TCheckBox;
    ShowAll: TCheckBox;
    btnUnhide: TSpeedButton;
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnUnhideClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    const WHITESPACE =  #32;
    var   FoundRow   :  integer;
    var   IsNext     :  boolean;
    var   pSGrid     :  TStringGrid;
    var   Groupping  :  array of integer;
    var   pSName     :  string;
    var   pSNumber   :  string;
  public
    property SGrid     :  TStringGrid read pSGrid   write pSGrid;
    property SColName  :  string      read pSName   write pSName;
    property SColNumber:  string      read pSNumber write pSNumber;
  published
    procedure Search;
  end;

var
  SearchForm: TSearchForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Filter;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------ SEARCH IN GIVEN STRING }
procedure TSearchForm.Search;
var
  IsNumber:      boolean;
  SearchString:  string;
  CompareValue:  string;
  iCNT:          integer;
  SearchColumn:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  SearchColumn:=0;
  if not (CaseSensitive.Checked) then SearchString:=UpperCase(EditSearch.Text);
  if (CaseSensitive.Checked)     then SearchString:=EditSearch.Text;

  { CANNOT BE EMPTY }
  if (SearchString = '') or (SearchString = WHITESPACE) then
  begin
    MainForm.MsgCall(2, 'Cannot search empty string. Please provide with customer name or customer number and try again.');
    Exit;
  end;

  { CHECK IF USER PROVIDED NUMBER }
  IsNumber:=True;
  try
    StrToInt64(EditSearch.Text);
  except
    IsNumber:=False;
  end;

  { ASSIGN PROPER COLUMN NUMBER FROM AGE VIEW }
  if (IsNumber)     then SearchColumn:=SGrid.ReturnColumn(SColNumber, 1, 1);
  if not (IsNumber) then SearchColumn:=SGrid.ReturnColumn(SColName,   1, 1);

  { SEARCH DATA }
  if (IsNext) and (FoundRow <> (SGrid.RowCount - 1)) then FoundRow:=FoundRow + 1;
  SetLength(Groupping, 0);
  for iCNT:=FoundRow to SGrid.RowCount - 1 do
  begin
    SetLength(Groupping, SGrid.RowCount);
    if not (CaseSensitive.Checked) then CompareValue:=UpperCase(SGrid.Cells[SearchColumn, iCNT]);
    if (CaseSensitive.Checked)     then CompareValue:=SGrid.Cells[SearchColumn, iCNT];
    if Pos(SearchString, CompareValue) > 0 then
    begin
      FoundRow:=iCNT;
      { EXIT ON FOUND GIVEN ITEM }
      if not (ShowAll.Checked) then
      begin
        IsNext:=True;
        if SGrid.RowHeights[FoundRow] = -1 then
          MainForm.MsgCall(1, 'The item has been found (' + SGrid.Cells[SearchColumn, FoundRow] + ') for search pattern "' + SearchString + '". ' +#13#10 +
                              'However it is hidden by the filter you have used. Remove the filtering to unhide searched item.');
        Break;
      end;
      { PUT CORRESPONDING ROWS INTO AN ARRAY }
      if (ShowAll.Checked) then
      begin
        Groupping[iCNT]:=FoundRow;
        IsNext:=False;
      end;
    end
      else
      begin
        IsNext  :=False;
        FoundRow:=0;
      end;
  end;

  { HIGHLIGHT FOUND ROW }
  if not (ShowAll.Checked) then
  begin
    if not (FoundRow = 0) then
    begin
      SGrid.Row:=FoundRow;
      SGrid.Col:=SGrid.ReturnColumn(SColName, 1, 1);
    end
      else
        MainForm.MsgCall(1, 'Cannot find specified customer.');
  end;

  { SHOW ALL FOUND }
  if (ShowAll.Checked) then
  begin
    { MAKE SURE ALL ROWS ARE VISIBLE }
    SGrid.DefaultRowHeight:=17;
    if High(Groupping) > 0 then
    begin
      for iCNT:=0 to SGrid.RowCount - 1 do
        if Groupping[iCNT] <> iCNT then SGrid.RowHeights[iCNT]:=-1;
      btnUnhide.Enabled:=True;
    end
    else
    begin
      MainForm.MsgCall(1, 'Cannot find specified customer.');
      btnUnhide.Enabled:=False;
    end;
  end;

end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TSearchForm.FormCreate(Sender: TObject);
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  SearchForm.Caption:=Settings.TMIG.ReadString(Settings.ApplicationDetails, 'WND_SEARCH', Settings.APPNAME);
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  SGrid     :=nil;
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
    { RETURN FILTER STATE }
    FilterForm.btnFilterClick(self);
    btnUnhide.Enabled:=False;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- RESET SETTINIGS }
procedure TSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IsNext  :=False;
  FoundRow:=0;
  EditSearch.Text:='';
end;

end.
