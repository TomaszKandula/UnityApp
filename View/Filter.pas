
{$I .\Include\Header.inc}

unit Filter;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    CheckLst,
    Buttons,
    pngimage,
    DBGrids,
    StrUtils,
    InterposerClasses,
    Arrays;


type

    /// <summary>
    ///     View form class with helpers for filter window. It is used to filter given string grid.
    /// </summary>

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
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
    private
        var CheckEvent :  boolean;
        // Holds values and theirs state
        var INF7       :  TLists;
        var INF4       :  TLists;
        var Gr3        :  TLists;
        var SalesResp  :  TLists;
        var PersonResp :  TLists;
        var CustomerGrp:  TLists;
        var AccountType:  TLists;
        var FollowUp   :  TLists;
        var CoCode     :  TLists;
        var Agent      :  TLists;
        var Division   :  TLists;
        var Free1      :  TLists;
        var Free2      :  TLists;
        var Free3      :  TLists;
        // Usage counter (how many times column was filtered)
        var countINF7       : integer;
        var countINF4       : integer;
        var countGr3        : integer;
        var countSalesResp  : integer;
        var countPersonResp : integer;
        var countCustomerGrp: integer;
        var countAccountType: integer;
        var countFollowUp   : integer;
        var countCoCode     : integer;
        var countAgent      : integer;
        var countDivision   : integer;
        var countFree1      : integer;
        var countFree2      : integer;
        var countFree3      : integer;
        // Global filter count
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


implementation


uses
    Main,
    Settings,
    Model,
    AgeView;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Remove all fiters (reset arrays and assign 0).
/// </summary>

procedure TFilterForm.FilterClearAll;
begin
    SetLength(FilterForm.INF7,        1, 2);
    SetLength(FilterForm.INF4,        1, 2);
    SetLength(FilterForm.Gr3,         1, 2);
    SetLength(FilterForm.SalesResp,   1, 2);
    SetLength(FilterForm.PersonResp,  1, 2);
    SetLength(FilterForm.CustomerGrp, 1, 2);
    SetLength(FilterForm.AccountType, 1, 2);
    SetLength(FilterForm.FollowUp,    1, 2);
    SetLength(FilterForm.CoCode,      1, 2);
    SetLength(FilterForm.Agent,       1, 2);
    SetLength(FilterForm.Division,    1, 2);
    SetLength(FilterForm.Free1,       1, 2);
    SetLength(FilterForm.Free2,       1, 2);
    SetLength(FilterForm.Free3,       1, 2);
    countINF7       :=0;
    countINF4       :=0;
    countGr3        :=0;
    countSalesResp  :=0;
    countPersonResp :=0;
    countCustomerGrp:=0;
    countAccountType:=0;
    countFollowUp   :=0;
    countCoCode     :=0;
    countAgent      :=0;
    countDivision   :=0;
    countFree1      :=0;
    countFree2      :=0;
    countFree3      :=0;
    HowManyFlts     :=0;
    InUse:=False;
end;


/// <summary>
///     Check if all items are selected.
/// </summary>

procedure TFilterForm.FilterSelectCheck;
var
    iCNT:  integer;
    Check: integer;
begin

    Check:=0;

    // CHECK IF ITEM IS SELECTED
    for iCNT:=0 to FilterList.Count - 1 do
        if FilterList.Checked[iCNT] = True then
            inc(Check);

    // IF ALL SELECTED, THEN TICK
    if Check = FilterList.Count then
        cbSelectAll.Checked:=True
            else
                cbSelectAll.Checked:=False;

end;


/// <summary>
///     Prepare for filtering.
/// </summary>

procedure TFilterForm.FilterPrep;
begin
    if (FGrid <> nil) and (not(string.IsNullOrEmpty(FColName))) then
    begin
        FColNumber:=FGrid.ReturnColumn(FColName, 1, 1);
        if FFilterNum = fltINF7      then FilterInit(INF7);
        if FFilterNum = fltINF4      then FilterInit(INF4);
        if FFilterNum = fltGR3       then FilterInit(Gr3);
        if FFilterNum = fltSalesRep  then FilterInit(SalesResp);
        if FFilterNum = fltPersonRep then FilterInit(PersonResp);
        if FFilterNum = fltCustGroup then FilterInit(CustomerGrp);
        if FFilterNum = fltAccType   then FilterInit(AccountType);
        if FFilterNum = fltFOLLOWUP  then FilterInit(FollowUp);
        if FFilterNum = fltCoCode    then FilterInit(CoCode);
        if FFilterNum = fltAgent     then FilterInit(Agent);
        if FFilterNum = fltDIVISION  then FilterInit(Division);
        if FFilterNum = fltFREE1     then FilterInit(Free1);
        if FFilterNum = fltFREE2     then FilterInit(Free2);
        if FFilterNum = fltFREE3     then FilterInit(Free3);
    end;
end;


/// <summary>
///     Update filter count.
/// </summary>

procedure TFilterForm.FilterCount(Change: integer);
begin

    if Change = fltIncrement then
    begin
        if (FFilterNum = fltINF7)      and (countINF7        = 0) then Inc(countINF7);
        if (FFilterNum = fltINF4)      and (countINF4        = 0) then Inc(countINF4);
        if (FFilterNum = fltGR3)       and (countGr3         = 0) then Inc(countGr3);
        if (FFilterNum = fltSalesRep)  and (countSalesResp   = 0) then Inc(countSalesResp);
        if (FFilterNum = fltPersonRep) and (countPersonResp  = 0) then Inc(countPersonResp);
        if (FFilterNum = fltCustGroup) and (countCustomerGrp = 0) then Inc(countCustomerGrp);
        if (FFilterNum = fltAccType)   and (countAccountType = 0) then Inc(countAccountType);
        if (FFilterNum = fltFOLLOWUP)  and (countFollowUp    = 0) then Inc(countFollowUp);
        if (FFilterNum = fltCoCode)    and (countCoCode      = 0) then Inc(countCoCode);
        if (FFilterNum = fltAgent)     and (countAgent       = 0) then Inc(countAgent);
        if (FFilterNum = fltDIVISION)  and (countDivision    = 0) then Inc(countDivision);
        if (FFilterNum = fltFREE1)     and (countFree1       = 0) then Inc(countFree1);
        if (FFilterNum = fltFREE2)     and (countFree2       = 0) then Inc(countFree2);
        if (FFilterNum = fltFREE3)     and (countFree3       = 0) then Inc(countFree3);
    end;

    if Change = fltDecrement then
    begin
        if (FFilterNum = fltINF7)      and (countINF7        > 0) then Dec(countINF7);
        if (FFilterNum = fltINF4)      and (countINF4        > 0) then Dec(countINF4);
        if (FFilterNum = fltGR3)       and (countGr3         > 0) then Dec(countGr3);
        if (FFilterNum = fltSalesRep)  and (countSalesResp   > 0) then Dec(countSalesResp);
        if (FFilterNum = fltPersonRep) and (countPersonResp  > 0) then Dec(countPersonResp);
        if (FFilterNum = fltCustGroup) and (countCustomerGrp > 0) then Dec(countCustomerGrp);
        if (FFilterNum = fltAccType)   and (countAccountType > 0) then Dec(countAccountType);
        if (FFilterNum = fltFOLLOWUP)  and (countFollowUp    > 0) then Dec(countFollowUp);
        if (FFilterNum = fltCoCode)    and (countCoCode      > 0) then Dec(countCoCode);
        if (FFilterNum = fltAgent)     and (countAgent       > 0) then Dec(countAgent);
        if (FFilterNum = fltDIVISION)  and (countDivision    > 0) then Dec(countDivision);
        if (FFilterNum = fltFREE1)     and (countFree1       > 0) then Dec(countFree1);
        if (FFilterNum = fltFREE2)     and (countFree2       > 0) then Dec(countFree2);
        if (FFilterNum = fltFREE3)     and (countFree3       > 0) then Dec(countFree3);
    end;

    HowManyFlts:=countINF7 + countINF4 + countGr3 +
                 countSalesResp + countPersonResp + countCustomerGrp + countAccountType +
                 countFollowUp + countCoCode + countAgent + countDivision +
                 countFree1 + countFree2 + countFree3;

end;


/// <summary>
///     Initialize filtering.
/// </summary>

procedure TFilterForm.FilterInit(var FFilter: TLists);
var
    iCNT:  integer;
    SL:    TStringList;
begin

    Screen.Cursor:=crHourGlass;
    FilterList.Items.Clear;
    FilterList.Freeze(True);

    // Make unique list of items
    try
        if FGrid.RowCount > 2 then
        begin

            SL:=TStringList.Create;

            try
                SL.Sorted:=True;
                SL.Duplicates:=dupIgnore;

                // If not previously filtered, then show all items
                if (High(FFilter) = 0) and (InUse = False) then
                    for iCNT:=1 to FGrid.RowCount - 1 do
                        SL.Add(FGrid.Cells[FColNumber, iCNT]);

                // If not previously filtered by any other column, then show only visible items
                if (High(FFilter) = 0) and (InUse = True) then
                    for iCNT:=1 to FGrid.RowCount - 1 do
                        if FGrid.RowHeights[iCNT] = sgRowHeight then
                            SL.Add(FGrid.Cells[FColNumber, iCNT]);

                // If previosuly filtered, then upload items with filter state
                if High(FFilter) > 0 then
                    for iCNT:=0 to High(FFilter) - 1 do
                        SL.Add(FFilter[iCNT, 0]);

                // Move to checkbox list
                for iCNT:=0 to SL.Count - 1 do
                    FilterList.Items.Add(SL.Strings[iCNT]);

            finally
                SL.Free;
            end;
        end;

        // Tick or untick previously filtered
        FilterList.CheckAll(cbChecked, False, True);

        for iCNT:=0 to High(FFilter) - 1 do
        begin

            if FFilter[iCNT, 1] = 'False' then
            begin
                FilterList.ItemEnabled[iCNT]:=True;
                FilterList.Checked[iCNT]:=False;

                if HowManyFlts > 1 then
                    FilterList.ItemEnabled[iCNT]:=False;
            end;

            if FFilter[iCNT, 1] = 'True'  then FilterList.Checked[iCNT]:=True;

        end;

    finally
        FilterList.Freeze(False);
        FilterList.Repaint;
        Screen.Cursor:=crDefault;
    end;

end;


/// <summary>
///     Filter now.
/// </summary>

procedure TFilterForm.FilterNow(var FFilter: TLists);
var
    iCNT:  integer;
    jCNT:  integer;
begin

    // Add to the list state (false or true)
    for iCNT:=0 to FilterList.Count - 1 do
    begin
        FFilter[iCNT, 0]:=FilterList.Items.Strings[iCNT];
        FFilter[iCNT, 1]:=BoolToStr(FilterList.Checked[iCNT], True);
        SetLength(FFilter, iCNT + 2, 2);
    end;

    InUse:=True;
    FilterCount(fltIncrement);

    // Filter selected items
    for iCNT:=0 to High(FFilter) - 1 do
        for jCNT:=1 { Skip header } to FGrid.RowCount - 1 do
        begin

            if (UpperCase(FFilter[iCNT, 0]) = UpperCase(FGrid.Cells[FColNumber, jCNT])) then
            begin

                // Unhide row
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

                // Unhide row
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

                // Hide row (sikp overdue items if checkbox is ticked)
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


/// <summary>
///     Clear given filter.
/// </summary>

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


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TFilterForm.FormCreate(Sender: TObject);
var
    Settings:  ISettings;
begin
    Settings:=TSettings.Create;
    FilterForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_FILTER', APPCAPTION);
    PanelListItems.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    FilterClearAll;
end;


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
    if (FFilterNum = fltINF7)      and (countINF7        > 0) then SetAndQuit;
    if (FFilterNum = fltINF4)      and (countINF4        > 0) then SetAndQuit;
    if (FFilterNum = fltGR3)       and (countGr3         > 0) then SetAndQuit;
    if (FFilterNum = fltSalesRep)  and (countSalesResp   > 0) then SetAndQuit;
    if (FFilterNum = fltPersonRep) and (countPersonResp  > 0) then SetAndQuit;
    if (FFilterNum = fltCustGroup) and (countCustomerGrp > 0) then SetAndQuit;
    if (FFilterNum = fltAccType)   and (countAccountType > 0) then SetAndQuit;
    if (FFilterNum = fltFOLLOWUP)  and (countFollowUp    > 0) then SetAndQuit;
    if (FFilterNum = fltCOCODE)    and (countCoCode      > 0) then SetAndQuit;
    if (FFilterNum = fltAGENT)     and (countAgent       > 0) then SetAndQuit;
    if (FFilterNum = fltDIVISION)  and (countDivision    > 0) then SetAndQuit;
    if (FFilterNum = fltFree1)     and (countFree1       > 0) then SetAndQuit;
    if (FFilterNum = fltFree2)     and (countFree2       > 0) then SetAndQuit;
    if (FFilterNum = fltFree3)     and (countFree3       > 0) then SetAndQuit;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //


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

            // Filter
            if FFilterNum = fltINF7      then FilterNow(INF7);
            if FFilterNum = fltINF4      then FilterNow(INF4);
            if FFilterNum = fltGR3       then FilterNow(Gr3);
            if FFilterNum = fltSalesRep  then FilterNow(SalesResp);
            if FFilterNum = fltPersonRep then FilterNow(PersonResp);
            if FFilterNum = fltCustGroup then FilterNow(CustomerGrp);
            if FFilterNum = fltAccType   then FilterNow(AccountType);
            if FFilterNum = fltFOLLOWUP  then FilterNow(FollowUp);
            if FFilterNum = fltCoCode    then FilterNow(CoCode);
            if FFilterNum = fltAgent     then FilterNow(Agent);
            if FFilterNum = fltDIVISION  then FilterNow(Division);
            if FFilterNum = fltFREE1     then FilterNow(Free1);
            if FFilterNum = fltFREE2     then FilterNow(Free2);
            if FFilterNum = fltFREE3     then FilterNow(Free3);

            // Re-compute aging summary
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

            // Unfilter
            if FFilterNum = fltINF7      then FilterRemove(INF7);
            if FFilterNum = fltINF4      then FilterRemove(INF4);
            if FFilterNum = fltGR3       then FilterRemove(Gr3);
            if FFilterNum = fltSalesRep  then FilterRemove(SalesResp);
            if FFilterNum = fltPersonRep then FilterRemove(PersonResp);
            if FFilterNum = fltCustGroup then FilterRemove(CustomerGrp);
            if FFilterNum = fltAccType   then FilterRemove(AccountType);
            if FFilterNum = fltFOLLOWUP  then FilterRemove(FollowUp);
            if FFilterNum = fltCoCode    then FilterRemove(CoCode);
            if FFilterNum = fltAgent     then FilterRemove(Agent);
            if FFilterNum = fltDIVISION  then FilterRemove(Division);
            if FFilterNum = fltFREE1     then FilterRemove(Free1);
            if FFilterNum = fltFREE2     then FilterRemove(Free2);
            if FFilterNum = fltFREE3     then FilterRemove(Free3);

            // Re-compute aging summary
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


procedure TFilterForm.cbSelectAllClick(Sender: TObject);
var
    iCNT:  integer;
begin

    if cbSelectAll.Checked then
    begin
        for iCNT:=0 to FilterList.Count - 1 do
            if FilterList.ItemEnabled[iCNT] = True then
                    FilterList.Checked[iCNT]:=True
    end
    else
    begin
        for iCNT:=0 to FilterList.Count - 1 do
            if FilterList.ItemEnabled[iCNT] = True then
                FilterList.Checked[iCNT]:=False;
    end;

end;


/// <remarks>
///
/// </remarks>

procedure TFilterForm.FilterListClick(Sender: TObject);
begin

    if not(CheckEvent) then
    begin

        if FilterList.Checked[FilterList.ItemIndex] then
            FilterList.Checked[FilterList.ItemIndex]:=False
                else
                    FilterList.Checked[FilterList.ItemIndex]:=True;

    end;

    CheckEvent:=False;

end;


/// <summary>
///
/// </summary>

procedure TFilterForm.FilterListClickCheck(Sender: TObject);
begin
    CheckEvent:=True;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TFilterForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = ESC then Close;
end;


end.

