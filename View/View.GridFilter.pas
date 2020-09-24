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
    System.Generics.Collections,
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
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure btnFilterClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
        procedure cbSelectAllMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure btnRemoveClick(Sender: TObject);
    strict private
        var FFilteredColumns: cardinal;
        var FFilterList:   TList<TColumnData>;
        var FInUse:        boolean;
        var CheckEvent:    boolean;
        var FColumnNumber: integer;
        var FColumnName:   string;
        var FSourceGrid:   TStringGrid;
        procedure LoadUniqueValues();
        procedure FilterSourceGrid();
        procedure UnfilterSourceGrid();
        procedure FilterSelectCheck();
        function IsIndexInAnotherFilter(ExcludedCol: integer; LookupIndex: integer): boolean;
        procedure RecalcAgeViewSummary_Callback(CallResponse: TCallResponse; AgingSummary: TAgingSummary);
        function GetState(ASearchValue: string; AColumnDataPos: integer; RowIndex: integer): integer;
        function GetColumnDataPos(AColumnNumber: integer): integer;
    public
        procedure RemoveAllFilters();
        property InUse:        boolean     read FInUse;
        property SourceGrid:   TStringGrid read FSourceGrid   write FSourceGrid;
        property ColumnNumber: integer     read FColumnNumber write FColumnNumber;
        property ColumnName:   string      read FColumnName   write FColumnName;
    end;


    function FilterForm(): TFilterForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.BusyScreen,
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


procedure TFilterForm.FilterSelectCheck();
begin

    var Check:=0;

    // Check if item is selected
    for var Index:=0 to FilterList.Count - 1 do
        if FilterList.Checked[Index] = True then
            Inc(Check);

    // If all selected, then tick
    if Check = FilterList.Count then
        cbSelectAll.Checked:=True
            else cbSelectAll.Checked:=False;

end;


function TFilterForm.IsIndexInAnotherFilter(ExcludedCol: integer; LookupIndex: integer): boolean;
begin

    Result:=False;

    for var ColIndex:=0 to FFilterList.Count - 1 do
    begin

        if ColIndex <> ExcludedCol then
        begin

            for var ItemIndex:=0 to Length(FFilterList[ColIndex].UniqueItems) - 1 do
            begin

                var SplitArray:=FFilterList[ColIndex].UniqueItems[ItemIndex, 2].Split([' ']);

                if TArrayUtils<string>.Contains(LookupIndex.ToString(), SplitArray) then
                begin
                    Result:=True;
                    Exit();
                end;

            end;

        end;

    end;

end;


function TFilterForm.GetState(ASearchValue: string; AColumnDataPos: integer; RowIndex: integer): integer;
begin

    Result:=-1;

    for var Index:=0 to Length(FFilterList[AColumnDataPos].UniqueItems) - 1 do
    begin

        if ASearchValue = FFilterList[AColumnDataPos].UniqueItems[Index, 0] then
        begin

            var LReturn:=FFilterList[AColumnDataPos].UniqueItems[Index, 1];

            if LReturn = 'True'  then Result:=1;
            if LReturn = 'False' then Result:=0;

            if (Result = 0) and (FSourceGrid.RowHeights[RowIndex] = FSourceGrid.sgRowHeight) then
            begin

                var Values:=FFilterList[AColumnDataPos].UniqueItems[Index, 2];
                Values:=Values + ' ' + RowIndex.ToString();

                FFilterList[AColumnDataPos].UniqueItems[Index, 2]:=Values.TrimLeft();

            end;

            Exit();

        end;

    end;

end;


function TFilterForm.GetColumnDataPos(AColumnNumber: integer): integer;
begin

    Result:=-1;

    for var Index:=0 to FFilterList.Count - 1 do
    begin

        if FFilterList[Index].ColumnNumber = AColumnNumber then
        begin
            Result:=Index;
            Exit();
        end;

    end;

end;


procedure TFilterForm.LoadUniqueValues();
begin

    if not Assigned(FSourceGrid) then
    begin
        THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, 'Source grid object is not assigned.');
        Exit();
    end;

    if FColumnNumber < 1 then Exit();

    var ColumnDataPos:=GetColumnDataPos(FColumnNumber);
    FilterList.Clear();

    // Check if column has been already queried
    // and if so, read the filter state
    if ColumnDataPos > -1 then
    begin

        if FFilterList[ColumnDataPos].UniqueItems = nil then
        begin
            THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, 'Array has not been created.');
            Exit();
        end;

        for var Items:=0 to Length(FFilterList[ColumnDataPos].UniqueItems) - 1 do
        begin
            FilterList.Items.Add(FFilterList[ColumnDataPos].UniqueItems[Items, 0]);
            FilterList.Checked[FilterList.Items.Count - 1]:=FFilterList[ColumnDataPos].UniqueItems[Items, 1].ToBoolean();
        end;

        if FFilterList[ColumnDataPos].IsFiltered then btnRemove.Enabled:=True else btnRemove.Enabled:=False;

    end
    else
    // Make new query and display unique values
    begin

        btnRemove.Enabled:=False;
        var StringList:=TStringList.Create();
        try

            StringList.Sorted:=True;
            StringList.Duplicates:=dupIgnore;

            // Put unique values to string list, skip header
            for var Index:=1 to FSourceGrid.RowCount - 1 do
            begin

                var Value:=FSourceGrid.Cells[FColumnNumber, Index].Trim();

                if FInUse then
                begin

                    if FSourceGrid.RowHeights[Index] = FSourceGrid.sgRowHeight then
                    begin
                        Value:=Value + '&&True';
                        StringList.Add(Value);
                    end;

                end
                else
                begin

                    if FSourceGrid.RowHeights[Index] = FSourceGrid.sgRowHidden then Value:=Value + '&&False';
                    if FSourceGrid.RowHeights[Index] = FSourceGrid.sgRowHeight then Value:=Value + '&&True';
                    StringList.Add(Value);

                end;

            end;

            var NewColumnData: TColumnData;
            NewColumnData.ColumnName  :=FColumnName;
            NewColumnData.ColumnNumber:=FColumnNumber;
            NewColumnData.IsFiltered  :=False;
            SetLength(NewColumnData.UniqueItems, StringList.Count, 3);

            for var Index:=0 to StringList.Count - 1 do
            begin

                var Value:=StringList.Strings[Index];

                if Value.Contains('&&') then
                begin

                    var Parsed:=Value.Split(['&&']);

                    FilterList.Items.Add(Parsed[0]);
                    NewColumnData.UniqueItems[Index, 0]:=Parsed[0];
                    NewColumnData.UniqueItems[Index, 1]:=Parsed[1];
                    NewColumnData.UniqueItems[Index, 2]:='';

                    if Parsed[1] = 'False' then
                        FilterList.Checked[Index]:=False
                            else FilterList.Checked[Index]:=True;

                end;

            end;

            FFilterList.Add(NewColumnData);

        finally
            StringList.Free();
        end;

    end;

end;


procedure TFilterForm.FilterSourceGrid();
begin

    var LColumnDataPos:=GetColumnDataPos(FColumnNumber);

    var UpdatedColumnData: TColumnData;
    UpdatedColumnData.ColumnName  :=FColumnName;
    UpdatedColumnData.ColumnNumber:=FColumnNumber;
    UpdatedColumnData.IsFiltered  :=True;
    UpdatedColumnData.UniqueItems :=FFilterList[LColumnDataPos].UniqueItems;

    FInUse:=True;
    FFilterList[LColumnDataPos]:=UpdatedColumnData;

    for var Index:=0 to Length(FFilterList[LColumnDataPos].UniqueItems) - 1 do
    begin
        var IsChecked:=FilterList.Checked[Index];
        FFilterList[LColumnDataPos].UniqueItems[Index, 1]:=IfThen(IsChecked, 'True', 'False');
    end;

    Inc(FFilteredColumns);

    for var Index:=1 to FSourceGrid.RowCount - 1 do
    begin

        var Value:=FSourceGrid.Cells[FColumnNumber, Index].Trim();

        if not IsIndexInAnotherFilter(LColumnDataPos, Index) then
        begin

            if GetState(Value, LColumnDataPos, Index) = 1 then
                FSourceGrid.RowHeights[Index]:=FSourceGrid.sgRowHeight
            else
                FSourceGrid.RowHeights[Index]:=FSourceGrid.sgRowHidden;

        end;

    end;

    Service.Logger.Log(Length(FFilterList[0].UniqueItems).ToString());

end;


procedure TFilterForm.UnfilterSourceGrid();
begin

    var LColumnDataPos:=GetColumnDataPos(FColumnNumber);
    var ToUnfilter: string;

    for var Index:=0 to Length(FFilterList[LColumnDataPos].UniqueItems) - 1 do
        ToUnfilter:=ToUnfilter + ' ' + FFilterList[LColumnDataPos].UniqueItems[Index, 2];

    ToUnfilter:=ToUnfilter.TrimLeft();
    var Parsed:=ToUnfilter.Split([' ']);

    for var Index:=1 to FSourceGrid.RowCount - 1 do
    begin

        if not IsIndexInAnotherFilter(LColumnDataPos, Index) then
        begin

            if TArrayUtils<string>.Contains(Index.ToString(), Parsed) then
                FSourceGrid.RowHeights[Index]:=FSourceGrid.sgRowHeight;

        end;

    end;

    FFilterList.Delete(LColumnDataPos);
    Dec(FFilteredColumns);

    if FFilteredColumns = 0 then
    begin
        FFilterList.Clear();
        FInUse:=False;
    end;

end;


procedure TFilterForm.RemoveAllFilters();
begin

    Screen.Cursor:=crHourGlass;
    BusyForm.Show();

    FSourceGrid.Freeze(True);

    THelpers.ExecWithDelay(150, procedure
    begin

        for var Index:=1 to FSourceGrid.RowCount - 1 do
            FSourceGrid.RowHeights[Index]:=FSourceGrid.sgRowHeight;

        Service.Mediator.Utilities.RecalcAgeViewSummaryAsync(
            FSourceGrid,
            MainForm.FRiskClassGroup,
            RecalcAgeViewSummary_Callback
        );

    end);

    FFilterList.Clear();
    FFilteredColumns:=0;
    FInUse:=False;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TFilterForm.FormCreate(Sender: TObject);
begin
    FilterList.Clear();
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    if not Assigned(FFilterList) then FFilterList:=TList<TColumnData>.Create();
end;


procedure TFilterForm.FormShow(Sender: TObject);
begin
    LoadUniqueValues();
end;


procedure TFilterForm.FormActivate(Sender: TObject);
begin
    FilterSelectCheck();
end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TFilterForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FFilterList) then FFilterList.Free();
end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TFilterForm.RecalcAgeViewSummary_Callback(CallResponse: TCallResponse; AgingSummary: TAgingSummary);
begin

    FSourceGrid.Freeze(False);
    Screen.Cursor:=crDefault;
    BusyForm.Close();

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[RecalcAgeViewSummary_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    MainForm.UpdateAgeSummary(AgingSummary);
    MainForm.UpdateFollowUps(FSourceGrid);
    FSourceGrid.Repaint();

end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TFilterForm.btnFilterClick(Sender: TObject);
begin

    Screen.Cursor:=crHourGlass;
    BusyForm.Show();

    FSourceGrid.Freeze(True);
    Close();

    THelpers.ExecWithDelay(150, procedure
    begin

        FilterSourceGrid();

        Service.Mediator.Utilities.RecalcAgeViewSummaryAsync(
            FSourceGrid,
            MainForm.FRiskClassGroup,
            RecalcAgeViewSummary_Callback
        );

    end);

end;


procedure TFilterForm.btnRemoveClick(Sender: TObject);
begin

    Screen.Cursor:=crHourGlass;
    BusyForm.Show();

    FSourceGrid.Freeze(True);
    Close();

    THelpers.ExecWithDelay(150, procedure
    begin

        UnfilterSourceGrid();

        Service.Mediator.Utilities.RecalcAgeViewSummaryAsync(
            FSourceGrid,
            MainForm.FRiskClassGroup,
            RecalcAgeViewSummary_Callback
        );

    end);

end;


procedure TFilterForm.cbSelectAllMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

    FilterList.Freeze(True);
    try

        if cbSelectAll.Checked then
        begin
            for var iCNT:=0 to FilterList.Count - 1 do
                if FilterList.ItemEnabled[iCNT] = True then
                        FilterList.Checked[iCNT]:=True
        end
        else
        begin
            for var iCNT:=0 to FilterList.Count - 1 do
                if FilterList.ItemEnabled[iCNT] = True then
                    FilterList.Checked[iCNT]:=False;
        end;

    finally
        FilterList.Freeze(False);
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

