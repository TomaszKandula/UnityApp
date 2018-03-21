{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Filter;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, CheckLst, Buttons, pngimage, DBGrids, StrUtils, Main;

{ -------------------------------------------------------------- ! INTERPOSES CLASS ! ----------------------------------------------------------------------- }
                                                     (* EXTEND CURRENT COMPONENTS | MAIN THREAD *)

{ ------------------------------------------------------------- ! TCHECKLISTBOX CLASS ! --------------------------------------------------------------------- }
type
  TCheckListBox = class(CheckLst.TCheckListBox)
  published
    procedure Freeze(PaintWnd: boolean);
  end;

{ ----------------------------------------------------------------- ! MAIN CLASS ! -------------------------------------------------------------------------- }
type
  TFilterForm = class(TForm)
    btnFilter: TSpeedButton;
    Text: TLabel;
    FilterList: TCheckListBox;
    AppMain: TShape;
    ButtonPanel: TPanel;
    btnClose: TSpeedButton;
    procedure btnFilterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FilterListClickCheck(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    var   ListState   :  TLists;      { KEEP VALUES AND THEIR STATE }
    var   pFGrid      :  TStringGrid;
    var   pFColName   :  string;
    var   pFColNumber :  integer;
    var   pFOverdue   :  string;
  public
    property  FColName   :  string      read pFColName   write pFColName;
    property  FColNumber :  integer     read pFColNumber write pFColNumber;
    property  FGrid      :  TStringGrid read pFGrid      write pFGrid;
    property  FOverdue   :  string      read pFOverdue   write pFOverdue;
    procedure FilterInit;
    procedure FilterPrep;
    procedure FilterNow;
  end;

var
  FilterForm:  TFilterForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Settings, Model;

{$R *.dfm}

{ ##################################################### ! EXTENSION OF 'TCHECKLISTBOX' CLASS ! ############################################################## }

{ ---------------------------------------------------------------------------------------------------------------------------------- COMPONENT DRAWING ON/OFF }
procedure TCheckListBox.Freeze(PaintWnd: Boolean);
begin
  { TRUE | HIDE STRING GRID CONTENT }
  if (PaintWnd) then
  begin
    with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  end;
  { FALSE | SHOW STRING GRID CONTENT }
  if not (PaintWnd) then
  begin
    with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
    Self.Repaint;
  end;
end;

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------ INITIAIZE FILTER }
procedure TFilterForm.FilterInit;
var
  iCNT:  integer;
  jCNT:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  FilterList.Items.Clear;
  FilterList.Items.Add('1A@(Select All)');  { MUST BE AN ITEM = 0 }
  FilterList.Items.Add('1B@(Blanks)');      { MUST BE AN ITEM = 1 }
  FilterList.Sorted:=False;
  FilterList.Freeze(True);
  { ------------------------------------------------------------------------------------------------------------------------------- MAKE UNIQUE LIST OF ITEMS }
  try
    if FGrid.RowCount > 2 then
    begin
      for iCNT:=1 to FGrid.RowCount - 1 do
      begin
        { POPULATE AND REMOVE DUPLICATES }
        if (FGrid.Cells[FColNumber, iCNT] <> SPACE) and
           (FilterList.Items.IndexOf(FGrid.Cells[FColNumber, iCNT]) = -1)
           then
             FilterList.Items.Add(FGrid.Cells[FColNumber, iCNT]);
      end;
      { SORT ALL ADDED ITEMS ASCENDING }
      FilterList.Sorted:=True;
      { REMOVE PREFIXES FROM TWO FIXED ITEMS }
      FilterList.Items.Strings[0]:=MidStr(FilterList.Items.Strings[0], 4, Length(FilterList.Items.Strings[0]) - 3);
      FilterList.Items.Strings[1]:=MidStr(FilterList.Items.Strings[1], 4, Length(FilterList.Items.Strings[1]) - 3);
    end;
    { ----------------------------------------------------------------------------------------------------------- UNTICK IF VALUE WAS FILTERED OUT PREVIOUSLY }
    FilterList.CheckAll(cbChecked, False, True);
    if not (FilterForm.ListState[0, 0] = '') then
    begin
      for iCNT:=0 to High(FilterForm.ListState) - 1 do
        for jCNT:=0 to FilterList.Count - 1 do
          if (UpperCase(FilterForm.ListState[iCNT, 0]) = UpperCase(FilterList.Items.Strings[jCNT])) then
          begin
            if (FilterForm.ListState[iCNT, 1] = 'False') then FilterList.Checked[jCNT]:=False;
            if (FilterForm.ListState[iCNT, 1] = 'True' ) then FilterList.Checked[jCNT]:=True;
          end;
    end;
  finally
    { ------------------------------------------------------------------------------------------------------------------------------------------ UNINITIALIZE }
    FilterList.Freeze(False);
    Screen.Cursor:=crDefault;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- PREPARE FOR FILTERING }
procedure TFilterForm.FilterPrep;
begin
  if (FGrid <> nil) and (FColName <> '') then
  begin
    FColNumber:=FGrid.ReturnColumn(FColName, 1, 1);
    FilterInit;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ FILTER NOW }
procedure TFilterForm.FilterNow;
var
  iCNT:  integer;
  jCNT:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  FilterList.Items.Strings[1]:=SPACE; { MAKE '(BLANKS)' A WHITESPACE }
  Screen.Cursor:=crHourGlass;
  FGrid.Freeze(True);
  { ----------------------------------------------------------------------------------------------------------------------------------- ADD TO THE LIST STATE }
  try
    for iCNT:=0 to FilterList.Count - 1 do
    begin
      FilterForm.ListState[iCNT, 0]:=FilterList.Items.Strings[iCNT];
      FilterForm.ListState[iCNT, 1]:=BoolToStr(FilterList.Checked[iCNT], True);
      SetLength(FilterForm.ListState, iCNT + 2, 2);
    end;
    { --------------------------------------------------------------------------------------------------------------------------------- FILTER SELECTED ITEMS }
    for iCNT:=0 to High(FilterForm.ListState) - 1 do
      for jCNT:=0 to MainForm.sgAgeView.RowCount - 1 do
      begin
        if (UpperCase(FilterForm.ListState[iCNT, 0]) = UpperCase(FGrid.Cells[FColNumber, jCNT])) then
        begin
          if (FilterForm.ListState[iCNT, 1] = 'True' ) then
          begin
            FGrid.RowHeights[jCNT]:= 17;
          end;
          if (FilterForm.ListState[iCNT, 1] = 'False') or
             (  (MainForm.Action_Overdue.Checked) and
                (FGrid.Cells[FGrid.ReturnColumn(FOverdue, 1, 1), jCNT] = '0')
             ) then
                 FGrid.RowHeights[jCNT]:= -1;
        end;
      end;
  finally
    { ------------------------------------------------------------------------------------------------------------------------------------------ UNINITIALIZE }
    FGrid.Freeze(False);
    FilterForm.ListState[1, 0]:='(Blanks)';  { REVERT TO '(BLANKS)' }
    Screen.Cursor:=crDefault;
    Close;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TFilterForm.FormCreate(Sender: TObject);
var
  AppSettings:  TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    FilterForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_FILTER', APPNAME);
  finally
    AppSettings.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  SetLength(FilterForm.ListState, 1, 2);
end;

{ --------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE | INITIALIZE FILTER }
procedure TFilterForm.FormActivate(Sender: TObject);
begin
  FilterPrep;
end;

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- FILTER }
procedure TFilterForm.btnFilterClick(Sender: TObject);
begin
  if FColNumber > 0 then FilterNow;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TFilterForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ SELECT ALL ITEMS }
procedure TFilterForm.FilterListClickCheck(Sender: TObject);
var
  iCNT:  integer;
begin
  { WE USE FIRST POSITION 'SELECT ALL' THE SAME WAY AS IN THE MANY POPULAR APPLICATION }
  { IF USER TICK ITEM 'SELECT ALL' WE SELECT OR DESELECT REST OF THE LISTED ITEMS      }
  if (FilterList.Selected[0] = True)  then 
    if (FilterList.Checked[0] = True) then 
      for iCNT:=1 to FilterList.Count - 1 do FilterList.Checked[iCNT]:=True
        else
          for iCNT:=1 to FilterList.Count - 1 do FilterList.Checked[iCNT]:=False;
end;

end.
