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
    FilterList: TCheckListBox;
    cbSelectAll: TCheckBox;
    PanelListItems: TPanel;
    PanelBackground: TPanel;
    btnRemove: TSpeedButton;
    procedure btnFilterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure cbSelectAllClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { KEEP VALUES AND THEIR STATE }
    var INF7       :  TLists;
    var INF4       :  TLists;
    var CoCode     :  TLists;
    var Agent      :  TLists;
    var Division   :  TLists;
    var FollowUp   :  TLists;
    var Gr3        :  TLists;
    var Free1      :  TLists;
    { FILTER USAGE COUNTERS }
    var countINF7      : integer;
    var countINF4      : integer;
    var countCoCode    : integer;
    var countAgent     : integer;
    var countDivision  : integer;
    var countFollowUp  : integer;
    var countGr3       : integer;
    var countFree1     : integer;
    { GLOBAL FITER COUNT }
    var HowManyFlts    : integer;
  public
    var FColName   :  string;
    var FColNumber :  integer;
    var FGrid      :  TStringGrid;
    var FOverdue   :  string;
    var FFilterNum :  integer;
    var InUse      :  boolean;
    procedure FilterClearAll;
    procedure FilterSelectCheck;
    procedure FilterPrep;
    procedure FilterCount(Change: integer);
    procedure FilterInit(var FFilter: TLists);
    procedure FilterNow(var FFilter: TLists);
    procedure FilterRemove(var FFilter: TLists);
  end;

var
  FilterForm:  TFilterForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Settings, Model, AgeView;

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

{ ----------------------------------------------------------------------------------------------------------------------------------------- CLEAR ALL FILTERS }
procedure TFilterForm.FilterClearAll;
begin
  SetLength(FilterForm.INF7,     1, 2);
  SetLength(FilterForm.INF4,     1, 2);
  SetLength(FilterForm.CoCode,   1, 2);
  SetLength(FilterForm.Agent,    1, 2);
  SetLength(FilterForm.Division, 1, 2);
  SetLength(FilterForm.FollowUp, 1, 2);
  SetLength(FilterForm.Gr3,      1, 2);
  SetLength(FilterForm.Free1,    1, 2);
  countINF7    :=0;
  countINF4    :=0;
  countCoCode  :=0;
  countAgent   :=0;
  countDivision:=0;
  countFollowUp:=0;
  countGr3     :=0;
  countFree1   :=0;
  HowManyFlts  :=0;
  InUse:=False;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- CHECK IF ALL SELECTED }
procedure TFilterForm.FilterSelectCheck;
var
  iCNT:  integer;
  Check: integer;
begin
  Check:=0;

  { CHECK IF ITEM IS SELECTED }
  for iCNT:=0 to FilterList.Count - 1 do
    if FilterList.Checked[iCNT] = True then inc(Check);

  { IF ALL SELECTED, THEN TICK }
  if Check = FilterList.Count then
    cbSelectAll.Checked:=True
      else
        cbSelectAll.Checked:=False;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- PREPARE FOR FILTERING }
procedure TFilterForm.FilterPrep;
begin
  if (FGrid <> nil) and (FColName <> '') then
  begin
    FColNumber:=FGrid.ReturnColumn(FColName, 1, 1);
    if FFilterNum = fltINF7     then FilterInit(INF7);
    if FFilterNum = fltINF4     then FilterInit(INF4);
    if FFilterNum = fltCoCode   then FilterInit(CoCode);
    if FFilterNum = fltAgent    then FilterInit(Agent);
    if FFilterNum = fltDIVISION then FilterInit(Division);
    if FFilterNum = fltFOLLOWUP then FilterInit(FollowUp);
    if FFilterNum = fltGR3      then FilterInit(Gr3);
    if FFilterNum = fltFREE1    then FilterInit(Free1);
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- UPDATE FILTER COUNT }
procedure TFilterForm.FilterCount(Change: integer);
begin

  if Change = fltIncrement then
  begin
    if (FFilterNum = fltINF7)     and (countINF7     = 0) then Inc(countINF7);
    if (FFilterNum = fltINF4)     and (countINF4     = 0) then Inc(countINF4);
    if (FFilterNum = fltCoCode)   and (countCoCode   = 0) then Inc(countCoCode);
    if (FFilterNum = fltAgent)    and (countAgent    = 0) then Inc(countAgent);
    if (FFilterNum = fltDIVISION) and (countDivision = 0) then Inc(countDivision);
    if (FFilterNum = fltFOLLOWUP) and (countFollowUp = 0) then Inc(countFollowUp);
    if (FFilterNum = fltGR3)      and (countGr3      = 0) then Inc(countGr3);
    if (FFilterNum = fltFREE1)    and (countFree1    = 0) then Inc(countFree1);
  end;

  if Change = fltDecrement then
  begin
    if (FFilterNum = fltINF7)     and (countINF7     > 0) then Dec(countINF7);
    if (FFilterNum = fltINF4)     and (countINF4     > 0) then Dec(countINF4);
    if (FFilterNum = fltCoCode)   and (countCoCode   > 0) then Dec(countCoCode);
    if (FFilterNum = fltAgent)    and (countAgent    > 0) then Dec(countAgent);
    if (FFilterNum = fltDIVISION) and (countDivision > 0) then Dec(countDivision);
    if (FFilterNum = fltFOLLOWUP) and (countFollowUp > 0) then Dec(countFollowUp);
    if (FFilterNum = fltGR3)      and (countGr3      > 0) then Dec(countGr3);
    if (FFilterNum = fltFREE1)    and (countFree1    > 0) then Dec(countFree1);
  end;

  HowManyFlts:=countINF7 + countINF4 + countCoCode + countAgent + countDivision + countFollowUp + countGr3 + countFree1;

end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ INITIAIZE FILTER }
procedure TFilterForm.FilterInit(var FFilter: TLists);
var
  iCNT:  integer;
  SL:    TStringList;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  FilterList.Items.Clear;
  FilterList.Freeze(True);
  { ------------------------------------------------------------------------------------------------------------------------------- MAKE UNIQUE LIST OF ITEMS }
  try
    if FGrid.RowCount > 2 then
    begin
      SL:=TStringList.Create;
      try
        SL.Sorted:=True;
        SL.Duplicates:=dupIgnore;

        { IF NOT PREVIOUSLY FILTERED, THEN SHOW ALL ITEMS }
        if (High(FFilter) = 0) and (InUse = False) then
          for iCNT:=1 to FGrid.RowCount - 1 do SL.Add(FGrid.Cells[FColNumber, iCNT]);

        { IF NOT PREVIOUSLY FILTERED BY ANY OTHER COLUMN, THEN SHOW ONLY VISIBLE ITEMS }
        if (High(FFilter) = 0) and (InUse = True) then
          for iCNT:=1 to FGrid.RowCount - 1 do
            if FGrid.RowHeights[iCNT] = sgRowHeight then
              SL.Add(FGrid.Cells[FColNumber, iCNT]);

        { IF PREVIOUSLY FILTERED, THEN UPLOAD ITEMS WITH FILTER STATE }
        if High(FFilter) > 0 then
          for iCNT:=0 to High(FFilter) - 1 do
            SL.Add(FFilter[iCNT, 0]);

        { MOVE TO CHECKBOX LIST }
        for iCNT:=0 to SL.Count - 1 do FilterList.Items.Add(SL.Strings[iCNT]);

      finally
        SL.Free;
      end;
    end;
    { -------------------------------------------------------------------------------------------------------------------- TICK OR UNTICK PREVIOUSLY FILTERED }
    FilterList.CheckAll(cbChecked, False, True);

    for iCNT:=0 to High(FFilter) - 1 do
    begin

      if FFilter[iCNT, 1] = 'False' then
      begin
        FilterList.ItemEnabled[iCNT]:=True;
        FilterList.Checked[iCNT]:=False;
        if HowManyFlts > 1 then FilterList.ItemEnabled[iCNT]:=False;
      end;

      if FFilter[iCNT, 1] = 'True'  then FilterList.Checked[iCNT]:=True;

    end;
  finally
    { ------------------------------------------------------------------------------------------------------------------------------------------ UNINITIALIZE }
    FilterList.Freeze(False);
    FilterList.Repaint;
    Screen.Cursor:=crDefault;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ FILTER NOW }
procedure TFilterForm.FilterNow(var FFilter: TLists);
var
  iCNT:  integer;
  jCNT:  integer;
begin
  { ----------------------------------------------------------------------------------------------------------------------------------- ADD TO THE LIST STATE }
  for iCNT:=0 to FilterList.Count - 1 do
  begin
    FFilter[iCNT, 0]:=FilterList.Items.Strings[iCNT];
    FFilter[iCNT, 1]:=BoolToStr(FilterList.Checked[iCNT], True);
    SetLength(FFilter, iCNT + 2, 2);
  end;
  InUse:=True;
  FilterCount(fltIncrement);
  { --------------------------------------------------------------------------------------------------------------------------------- FILTER SELECTED ITEMS }
  for iCNT:=0 to High(FFilter) - 1 do
    for jCNT:=1 { SKIP HEADER } to FGrid.RowCount - 1 do
    begin
      if (UpperCase(FFilter[iCNT, 0]) = UpperCase(FGrid.Cells[FColNumber, jCNT])) then
      begin

        { UNHIDE ROW }
        if
          (
            HowManyFlts = 1
          )
          and
          (
            FFilter[iCNT, 1] = 'True'
          )
        then
          FGrid.RowHeights[jCNT]:=sgRowHeight;

        { UNHIDE ROW }
        if
          (
            HowManyFlts > 1
          )
          and
          (
            FFilter[iCNT, 1] = 'True'
          )
          and
          (
            FGrid.RowHeights[jCNT] <> sgRowHidden
          )
        then
          FGrid.RowHeights[jCNT]:=sgRowHeight;

        { HIDE ROW }
        if
          (
            FFilter[iCNT, 1] = 'False'
          )
          or
          (
            (
              MainForm.Action_Overdue.Checked
            )
            and
            (
              FGrid.Cells[FGrid.ReturnColumn(FOverdue, 1, 1), jCNT] = '0'
            )
          )
        then
          FGrid.RowHeights[jCNT]:=sgRowHidden;
      end;
    end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- CLEAR GIVEN FILTER }
procedure TFilterForm.FilterRemove(var FFilter: TLists);
var
  iCNT:  integer;
  jCNT:  integer;
begin
  for iCNT:=0 to high(FFilter) - 1 do
  begin
    FilterList.ItemEnabled[iCNT]:=True;
    if FFilter[iCNT, 1] = 'False' then
    begin
      FilterList.Checked[iCNT]:=True;
      for jCNT:=1 { SKIP HEADER } to FGrid.RowCount - 1 do
      begin
        if (UpperCase(FFilter[iCNT, 0]) = UpperCase(FGrid.Cells[FColNumber, jCNT])) then
        begin
          { UNHIDE ROW }
          if
            (
              FFilter[iCNT, 1] = 'False'
            )
          then
          begin
            FFilter[iCNT, 1]:='True';
            FGrid.RowHeights[jCNT]:=sgRowHeight;
          end;
        end;
      end;
    end;
  end;
  cbSelectAll.Checked:=True;
  FilterCount(fltDecrement);
end;

{ ################################################################## ! EVENTS ! ############################################################################# }

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
  PanelListItems.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  FilterClearAll;
end;

{ --------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE | INITIALIZE FILTER }
procedure TFilterForm.FormActivate(Sender: TObject);

  (* NESTED METHOD *)

  procedure SetAndQuit;
  begin
    btnRemove.Enabled:=True;
    Exit;
  end;

begin
  FilterPrep;
  FilterSelectCheck;
  btnRemove.Enabled:=False;
  if (FFilterNum = fltINF7)     and (countINF7     > 0) then SetAndQuit;
  if (FFilterNum = fltINF4)     and (countINF4     > 0) then SetAndQuit;
  if (FFilterNum = fltCOCODE)   and (countCoCode   > 0) then SetAndQuit;
  if (FFilterNum = fltAGENT)    and (countAgent    > 0) then SetAndQuit;
  if (FFilterNum = fltDIVISION) and (countDivision > 0) then SetAndQuit;
  if (FFilterNum = fltFOLLOWUP) and (countFollowUp > 0) then SetAndQuit;
  if (FFilterNum = fltGR3)      and (countGr3      > 0) then SetAndQuit;
  if (FFilterNum = fltFree1)    and (countFree1    > 0) then SetAndQuit;
end;

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- FILTER }
procedure TFilterForm.btnFilterClick(Sender: TObject);
var
  AgeView: TAgeView;
begin
  if FColNumber > 0 then
  begin
    Screen.Cursor:=crHourGlass;
    AgeView:=TAgeView.Create(MainForm.DbConnect);
    try
      FGrid.Freeze(True);
      { FILTER }
      if FFilterNum = fltINF7     then FilterNow(INF7);
      if FFilterNum = fltINF4     then FilterNow(INF4);
      if FFilterNum = fltCoCode   then FilterNow(CoCode);
      if FFilterNum = fltAgent    then FilterNow(Agent);
      if FFilterNum = fltDIVISION then FilterNow(Division);
      if FFilterNum = fltFOLLOWUP then FilterNow(FollowUp);
      if FFilterNum = fltGR3      then FilterNow(Gr3);
      if FFilterNum = fltFREE1    then FilterNow(Free1);
      { RE-COMPUTE AGING SUMMARY }
      AgeView.ComputeAgeSummary(FGrid);
      AgeView.ComputeAndShowRCA(FGrid);
      AgeView.UpdateSummary;
    finally
      AgeView.Free;
      FGrid.Repaint;
      FGrid.Freeze(False);
      Screen.Cursor:=crDefault;
    end;
    Close;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- REMOVE FILTER }
procedure TFilterForm.btnRemoveClick(Sender: TObject);
var
  AgeView: TAgeView;
begin

  if HowManyFlts < 2 then
  begin
    MainForm.Action_RemoveFiltersClick(Self);
    Close;
  end;

  if FColNumber > 0 then
  begin
    Screen.Cursor:=crHourGlass;
    AgeView:=TAgeView.Create(MainForm.DbConnect);
    try
      FGrid.Freeze(True);
      { FILTER }
      if FFilterNum = fltINF7     then FilterRemove(INF7);
      if FFilterNum = fltINF4     then FilterRemove(INF4);
      if FFilterNum = fltCoCode   then FilterRemove(CoCode);
      if FFilterNum = fltAgent    then FilterRemove(Agent);
      if FFilterNum = fltDIVISION then FilterRemove(Division);
      if FFilterNum = fltFOLLOWUP then FilterRemove(FollowUp);
      if FFilterNum = fltGR3      then FilterRemove(Gr3);
      if FFilterNum = fltFREE1    then FilterRemove(Free1);
      { RE-COMPUTE AGING SUMMARY }
      AgeView.ComputeAgeSummary(FGrid);
      AgeView.ComputeAndShowRCA(FGrid);
      AgeView.UpdateSummary;
    finally
      AgeView.Free;
      FGrid.Repaint;
      FGrid.Freeze(False);
      Screen.Cursor:=crDefault;
    end;
    Close;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ SELECT ALL ITEMS }
procedure TFilterForm.cbSelectAllClick(Sender: TObject);
var
  iCNT:  integer;
begin
  if cbSelectAll.Checked then
  begin
    for iCNT:=0 to FilterList.Count - 1 do
      if FilterList.ItemEnabled[iCNT] = True then FilterList.Checked[iCNT]:=True
  end
  else
    for iCNT:=0 to FilterList.Count - 1 do
      if FilterList.ItemEnabled[iCNT] = True then FilterList.Checked[iCNT]:=False;
end;

{ -------------------------------------------------------------- ! KEYBOARD EVENTS ! ------------------------------------------------------------------------ }

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TFilterForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

end.
