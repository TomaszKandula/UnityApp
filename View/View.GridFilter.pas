unit View.GridFilter;

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
    System.StrUtils,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.CheckLst,
    Vcl.Buttons,
    Vcl.Imaging.pngimage,
    Vcl.DBGrids,
    Unity.Grid,
    Unity.ListView,
    Unity.ChkListBox,
    Unity.Panel,
    Unity.Enums,
    Unity.Records;

    {TODO -oTomek -cGeneral : Redesign this completly}

type


    TFilterForm = class(TForm)
        btnFilter: TSpeedButton;
        FilterList: TCheckListBox;
        cbSelectAll: TCheckBox;
        PanelListItems: TPanel;
        PanelBackground: TPanel;
        btnRemove: TSpeedButton;
        PanelButtons: TPanel;
        ImageGrip: TImage;
        PanelHeader: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure btnFilterClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure cbSelectAllClick(Sender: TObject);
        procedure btnRemoveClick(Sender: TObject);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
    strict private
        var CheckEvent:       boolean;
        // Holds values and theirs state
        var INF7:             TArray<TArray<string>>;
        var INF4:             TArray<TArray<string>>;
        var Gr3:              TArray<TArray<string>>;
        var SalesResp:        TArray<TArray<string>>;
        var PersonResp:       TArray<TArray<string>>;
        var CustomerGrp:      TArray<TArray<string>>;
        var AccountType:      TArray<TArray<string>>;
        var FollowUp:         TArray<TArray<string>>;
        var CoCode:           TArray<TArray<string>>;
        var Agent:            TArray<TArray<string>>;
        var Division:         TArray<TArray<string>>;
        var Free1:            TArray<TArray<string>>;
        var Free2:            TArray<TArray<string>>;
        var Free3:            TArray<TArray<string>>;
        // Usage counter (how many times column was filtered)
        var countINF7:        integer;
        var countINF4:        integer;
        var countGr3:         integer;
        var countSalesResp:   integer;
        var countPersonResp:  integer;
        var countCustomerGrp: integer;
        var countAccountType: integer;
        var countFollowUp:    integer;
        var countCoCode:      integer;
        var countAgent:       integer;
        var countDivision:    integer;
        var countFree1:       integer;
        var countFree2:       integer;
        var countFree3:       integer;
        // Global filter count
        var HowManyFlts:      integer;
        procedure RecalcAgeViewSummary_Callback(PayLoad: TAgingPayLoad; CallResponse: TCallResponse);
    public
        var FColName:    string;
        var FColNumber:  integer;
        var FGrid:       TStringGrid;
        var FOverdue:    string;
        var FFilterNum:  TColumns;
        var InUse:  boolean;
        procedure FilterClearAll();
        procedure FilterSelectCheck();
        procedure FilterPrep();
        procedure FilterCount(IsIncrementing: boolean);
        procedure FilterInit(var FFilter: TArray<TArray<string>>);
        procedure FilterNow(var FFilter: TArray<TArray<string>>);
        procedure FilterRemove(var FFilter: TArray<TArray<string>>);
    end;


    function FilterForm(): TFilterForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Helpers,
    Unity.Constants,
    Unity.Settings,
    Unity.Service;


var vFilterForm: TFilterForm;


function FilterForm(): TFilterForm;
begin
    if not(Assigned(vFilterForm)) then Application.CreateForm(TFilterForm, vFilterForm);
    Result:=vFilterForm;
end;


{$REGION 'LOCAL HELPERS'}


procedure TFilterForm.FilterClearAll();
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

    Service.Mediator.Utilities.RecalcAgeViewSummaryAsync(
        MainForm.sgAgeView,
        MainForm.FRiskClassGroup,
        RecalcAgeViewSummary_Callback
    );

end;


procedure TFilterForm.FilterSelectCheck();
begin

    var Check: integer:=0;

    // Check if item is selected
    for var iCNT: integer:=0 to FilterList.Count - 1 do
        if FilterList.Checked[iCNT] = True then
            inc(Check);

    // If all selected, then tick
    if Check = FilterList.Count then
        cbSelectAll.Checked:=True
            else
                cbSelectAll.Checked:=False;

end;


procedure TFilterForm.FilterPrep();
begin
    if (FGrid <> nil) and (not(string.IsNullOrEmpty(FColName))) then
    begin
        FColNumber:=FGrid.GetCol(FColName);
        if FFilterNum = TColumns.Inf7              then FilterInit(INF7);
        if FFilterNum = TColumns.Inf4              then FilterInit(INF4);
        if FFilterNum = TColumns.Group3            then FilterInit(Gr3);
        if FFilterNum = TColumns.SalesResponsible  then FilterInit(SalesResp);
        if FFilterNum = TColumns.PersonResponsible then FilterInit(PersonResp);
        if FFilterNum = TColumns.CustomerGroup     then FilterInit(CustomerGrp);
        if FFilterNum = TColumns.AccountType       then FilterInit(AccountType);
        if FFilterNum = TColumns.Follow            then FilterInit(FollowUp);
        if FFilterNum = TColumns.CoCode            then FilterInit(CoCode);
        if FFilterNum = TColumns.Agent             then FilterInit(Agent);
        if FFilterNum = TColumns.Division          then FilterInit(Division);
        if FFilterNum = TColumns.Free1             then FilterInit(Free1);
        if FFilterNum = TColumns.Free2             then FilterInit(Free2);
        if FFilterNum = TColumns.Free3             then FilterInit(Free3);
    end;
end;


procedure TFilterForm.FilterCount(IsIncrementing: boolean);
begin

    if IsIncrementing then
    begin
        if (FFilterNum = TColumns.Inf7)              and (countINF7        = 0) then Inc(countINF7);
        if (FFilterNum = TColumns.Inf4)              and (countINF4        = 0) then Inc(countINF4);
        if (FFilterNum = TColumns.Group3)            and (countGr3         = 0) then Inc(countGr3);
        if (FFilterNum = TColumns.SalesResponsible)  and (countSalesResp   = 0) then Inc(countSalesResp);
        if (FFilterNum = TColumns.PersonResponsible) and (countPersonResp  = 0) then Inc(countPersonResp);
        if (FFilterNum = TColumns.CustomerGroup)     and (countCustomerGrp = 0) then Inc(countCustomerGrp);
        if (FFilterNum = TColumns.AccountType)       and (countAccountType = 0) then Inc(countAccountType);
        if (FFilterNum = TColumns.Follow)            and (countFollowUp    = 0) then Inc(countFollowUp);
        if (FFilterNum = TColumns.CoCode)            and (countCoCode      = 0) then Inc(countCoCode);
        if (FFilterNum = TColumns.Agent)             and (countAgent       = 0) then Inc(countAgent);
        if (FFilterNum = TColumns.Division)          and (countDivision    = 0) then Inc(countDivision);
        if (FFilterNum = TColumns.Free1)             and (countFree1       = 0) then Inc(countFree1);
        if (FFilterNum = TColumns.Free2)             and (countFree2       = 0) then Inc(countFree2);
        if (FFilterNum = TColumns.Free3)             and (countFree3       = 0) then Inc(countFree3);
    end
    else
    begin
        if (FFilterNum = TColumns.Inf7)              and (countINF7        > 0) then Dec(countINF7);
        if (FFilterNum = TColumns.Inf4)              and (countINF4        > 0) then Dec(countINF4);
        if (FFilterNum = TColumns.Group3)            and (countGr3         > 0) then Dec(countGr3);
        if (FFilterNum = TColumns.SalesResponsible)  and (countSalesResp   > 0) then Dec(countSalesResp);
        if (FFilterNum = TColumns.PersonResponsible) and (countPersonResp  > 0) then Dec(countPersonResp);
        if (FFilterNum = TColumns.CustomerGroup)     and (countCustomerGrp > 0) then Dec(countCustomerGrp);
        if (FFilterNum = TColumns.AccountType)       and (countAccountType > 0) then Dec(countAccountType);
        if (FFilterNum = TColumns.Follow)            and (countFollowUp    > 0) then Dec(countFollowUp);
        if (FFilterNum = TColumns.CoCode)            and (countCoCode      > 0) then Dec(countCoCode);
        if (FFilterNum = TColumns.Agent)             and (countAgent       > 0) then Dec(countAgent);
        if (FFilterNum = TColumns.Division)          and (countDivision    > 0) then Dec(countDivision);
        if (FFilterNum = TColumns.Free1)             and (countFree1       > 0) then Dec(countFree1);
        if (FFilterNum = TColumns.Free2)             and (countFree2       > 0) then Dec(countFree2);
        if (FFilterNum = TColumns.Free3)             and (countFree3       > 0) then Dec(countFree3);
    end;

    HowManyFlts:=countINF7 + countINF4 + countGr3 +
                 countSalesResp + countPersonResp + countCustomerGrp + countAccountType +
                 countFollowUp + countCoCode + countAgent + countDivision +
                 countFree1 + countFree2 + countFree3;

end;


procedure TFilterForm.FilterInit(var FFilter: TArray<TArray<string>>);
begin

    Screen.Cursor:=crHourGlass;
    FilterList.Items.Clear();
    FilterList.Freeze(True);

    // Make unique list of items
    try
        if FGrid.RowCount > 2 then
        begin

            var SL: TStringList:=TStringList.Create;

            try
                SL.Sorted:=True;
                SL.Duplicates:=dupIgnore;

                // If not previously filtered, then show all items
                if (High(FFilter) = 0) and (InUse = False) then
                    for var iCNT: integer:=1 to FGrid.RowCount - 1 do
                        SL.Add(FGrid.Cells[FColNumber, iCNT]);

                // If not previously filtered by any other column, then show only visible items
                if (High(FFilter) = 0) and (InUse = True) then
                    for var iCNT: integer:=1 to FGrid.RowCount - 1 do
                        if FGrid.RowHeights[iCNT] = FGrid.sgRowHeight then
                            SL.Add(FGrid.Cells[FColNumber, iCNT]);

                // If previosuly filtered, then upload items with filter state
                if High(FFilter) > 0 then
                    for var iCNT: integer:=0 to High(FFilter) - 1 do
                        SL.Add(FFilter[iCNT, 0]);

                // Move to checkbox list
                for var iCNT: integer:=0 to SL.Count - 1 do
                    FilterList.Items.Add(SL.Strings[iCNT]);

            finally
                SL.Free();
            end;
        end;

        // Tick or untick previously filtered
        FilterList.CheckAll(cbChecked, False, True);

        for var iCNT: integer:=0 to High(FFilter) - 1 do
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
        FilterList.Repaint();
        Screen.Cursor:=crDefault;
    end;

end;


procedure TFilterForm.FilterNow(var FFilter: TArray<TArray<string>>);
begin

    // Add to the list state (false or true)
    for var iCNT: integer:=0 to FilterList.Count - 1 do
    begin
        FFilter[iCNT, 0]:=FilterList.Items.Strings[iCNT];
        FFilter[iCNT, 1]:=BoolToStr(FilterList.Checked[iCNT], True);
        SetLength(FFilter, iCNT + 2, 2);
    end;

    InUse:=True;
    FilterCount(True);

    // Filter selected items
    for var iCNT: integer:=0 to High(FFilter) - 1 do
        for var jCNT: integer:=1 to FGrid.RowCount - 1 do
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
                    FGrid.RowHeights[jCNT]:=FGrid.sgRowHeight;

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
                    FGrid.RowHeights[jCNT] <> FGrid.sgRowHidden
                )
                then
                    FGrid.RowHeights[jCNT]:=FGrid.sgRowHeight;

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
                        FGrid.Cells[FGrid.GetCol(FOverdue), jCNT] = '0'
                    )
                )
                then
                    FGrid.RowHeights[jCNT]:=FGrid.sgRowHidden;
            end;

        end;

end;


procedure TFilterForm.FilterRemove(var FFilter: TArray<TArray<string>>);
begin

    for var iCNT: integer:=0 to high(FFilter) - 1 do
    begin
        FilterList.ItemEnabled[iCNT]:=True;

        if FFilter[iCNT, 1] = 'False' then
        begin
            FilterList.Checked[iCNT]:=True;

            for var jCNT: integer:=1 to FGrid.RowCount - 1 do
            begin

                if (UpperCase(FFilter[iCNT, 0]) = UpperCase(FGrid.Cells[FColNumber, jCNT])) then
                begin
                    // Unhide row
                    if
                    (
                        FFilter[iCNT, 1] = 'False'
                    )
                    then
                    begin
                        FFilter[iCNT, 1]:='True';
                        FGrid.RowHeights[jCNT]:=FGrid.sgRowHeight;
                    end;
                end;

            end;

        end;

    end;

    cbSelectAll.Checked:=True;
    FilterCount(False);

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TFilterForm.FormCreate(Sender: TObject);
begin
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    FilterClearAll();
end;


procedure TFilterForm.FormActivate(Sender: TObject);

    procedure SetAndQuit();
    begin
        btnRemove.Enabled:=True;
        Exit();
    end;

begin

    FilterPrep();
    FilterSelectCheck();
    btnRemove.Enabled:=False;

    if (FFilterNum = TColumns.Inf7)              and (countINF7        > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Inf4)              and (countINF4        > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Group3)            and (countGr3         > 0) then SetAndQuit;
    if (FFilterNum = TColumns.SalesResponsible)  and (countSalesResp   > 0) then SetAndQuit;
    if (FFilterNum = TColumns.PersonResponsible) and (countPersonResp  > 0) then SetAndQuit;
    if (FFilterNum = TColumns.CustomerGroup)     and (countCustomerGrp > 0) then SetAndQuit;
    if (FFilterNum = TColumns.AccountType)       and (countAccountType > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Follow)            and (countFollowUp    > 0) then SetAndQuit;
    if (FFilterNum = TColumns.CoCode)            and (countCoCode      > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Agent)             and (countAgent       > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Division)          and (countDivision    > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Free1)             and (countFree1       > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Free2)             and (countFree2       > 0) then SetAndQuit;
    if (FFilterNum = TColumns.Free3)             and (countFree3       > 0) then SetAndQuit;

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TFilterForm.RecalcAgeViewSummary_Callback(PayLoad: TAgingPayLoad; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[RecalcAgeViewSummary_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
    end;

    MainForm.sgAgeView.Freeze(False);
    MainForm.sgAgeView.Repaint();
    MainForm.UpdateAgeSummary(PayLoad);
    MainForm.UpdateFollowUps(MainForm.sgAgeView);
    Screen.Cursor:=crDefault;

end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TFilterForm.btnFilterClick(Sender: TObject);
begin

    if FColNumber > 0 then
    begin

        Screen.Cursor:=crHourGlass;
        FGrid.Freeze(True);

        if (FFilterNum = TColumns.Inf7)              then FilterNow(INF7);
        if (FFilterNum = TColumns.Inf4)              then FilterNow(INF4);
        if (FFilterNum = TColumns.Group3)            then FilterNow(Gr3);
        if (FFilterNum = TColumns.SalesResponsible)  then FilterNow(SalesResp);
        if (FFilterNum = TColumns.PersonResponsible) then FilterNow(PersonResp);
        if (FFilterNum = TColumns.CustomerGroup)     then FilterNow(CustomerGrp);
        if (FFilterNum = TColumns.AccountType)       then FilterNow(AccountType);
        if (FFilterNum = TColumns.Follow)            then FilterNow(FollowUp);
        if (FFilterNum = TColumns.CoCode)            then FilterNow(CoCode);
        if (FFilterNum = TColumns.Agent)             then FilterNow(Agent);
        if (FFilterNum = TColumns.Division)          then FilterNow(Division);
        if (FFilterNum = TColumns.Free1)             then FilterNow(Free1);
        if (FFilterNum = TColumns.Free2)             then FilterNow(Free2);
        if (FFilterNum = TColumns.Free3)             then FilterNow(Free3);

        Service.Mediator.Utilities.RecalcAgeViewSummaryAsync(
            FGrid,
            MainForm.FRiskClassGroup,
            RecalcAgeViewSummary_Callback
        );

        Close();

    end;

end;


procedure TFilterForm.btnRemoveClick(Sender: TObject);
begin

    if HowManyFlts < 2 then
    begin
        MainForm.Action_RemoveFiltersClick(Self);
        Close();
    end;

    if FColNumber > 0 then
    begin

        Screen.Cursor:=crHourGlass;
        FGrid.Freeze(True);

        if (FFilterNum = TColumns.Inf7)              then FilterRemove(INF7);
        if (FFilterNum = TColumns.Inf4)              then FilterRemove(INF4);
        if (FFilterNum = TColumns.Group3)            then FilterRemove(Gr3);
        if (FFilterNum = TColumns.SalesResponsible)  then FilterRemove(SalesResp);
        if (FFilterNum = TColumns.PersonResponsible) then FilterRemove(PersonResp);
        if (FFilterNum = TColumns.CustomerGroup)     then FilterRemove(CustomerGrp);
        if (FFilterNum = TColumns.AccountType)       then FilterRemove(AccountType);
        if (FFilterNum = TColumns.Follow)            then FilterRemove(FollowUp);
        if (FFilterNum = TColumns.CoCode)            then FilterRemove(CoCode);
        if (FFilterNum = TColumns.Agent)             then FilterRemove(Agent);
        if (FFilterNum = TColumns.Division)          then FilterRemove(Division);
        if (FFilterNum = TColumns.Free1)             then FilterRemove(Free1);
        if (FFilterNum = TColumns.Free2)             then FilterRemove(Free2);
        if (FFilterNum = TColumns.Free3)             then FilterRemove(Free3);

        Service.Mediator.Utilities.RecalcAgeViewSummaryAsync(
            FGrid,
            MainForm.FRiskClassGroup,
            RecalcAgeViewSummary_Callback
        );

        Close;

    end;

end;


procedure TFilterForm.cbSelectAllClick(Sender: TObject);
begin

    if cbSelectAll.Checked then
    begin
        for var iCNT: integer:=0 to FilterList.Count - 1 do
            if FilterList.ItemEnabled[iCNT] = True then
                    FilterList.Checked[iCNT]:=True
    end
    else
    begin
        for var iCNT: integer:=0 to FilterList.Count - 1 do
            if FilterList.ItemEnabled[iCNT] = True then
                FilterList.Checked[iCNT]:=False;
    end;

end;


procedure TFilterForm.FilterListClick(Sender: TObject);
begin

    try

        if not(CheckEvent) then
        begin

            if FilterList.Checked[FilterList.ItemIndex] then
                FilterList.Checked[FilterList.ItemIndex]:=False
                    else
                        FilterList.Checked[FilterList.ItemIndex]:=True;

        end;

    except on
        E: Exception do
        // Nothing
    end;

    CheckEvent:=False;

end;


procedure TFilterForm.FilterListClickCheck(Sender: TObject);
begin
    CheckEvent:=True;
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TFilterForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.ESC then Close();
end;


{$ENDREGION}


end.

