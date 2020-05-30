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
        procedure RecalcAgeViewSummary_Callback(CallResponse: TCallResponse; AgingSummary: TAgingSummary);
        function GetState(ASearchValue: string; AColumnDataPos: integer): boolean;
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


function TFilterForm.GetState(ASearchValue: string; AColumnDataPos: integer): boolean;
begin

    Result:=False;

    for var Index:=0 to Length(FFilterList[AColumnDataPos].UniqueItems) - 1 do
    begin

        if ASearchValue = FFilterList[AColumnDataPos].UniqueItems[Index, 0] then
        begin
            Result:=FFilterList[AColumnDataPos].UniqueItems[Index, 1].ToBoolean();
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

    if not FColumnNumber > 0 then
    begin
        THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, 'Invalid column selection.');
        Exit();
    end;

    var ColumnDataPos:=GetColumnDataPos(FColumnNumber);
    FilterList.Clear();

    // Check if column has been already queried
    // and if so, ready the filter state
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

        if FFilterList[ColumnDataPos].IsFiltered then
            btnRemove.Enabled:=True
                else btnRemove.Enabled:=False;

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

                var stop:=0;

            end;

            var NewColumnData: TColumnData;
            NewColumnData.ColumnName  :=FColumnName;
            NewColumnData.ColumnNumber:=FColumnNumber;
            NewColumnData.IsFiltered  :=False;
            SetLength(NewColumnData.UniqueItems, StringList.Count, 2);

            for var Index:=0 to StringList.Count - 1 do
            begin

                var Value:=StringList.Strings[Index];

                if Value.Contains('&&') then
                begin

                    var Parsed:=Value.Split(['&&']);

                    FilterList.Items.Add(Parsed[0]);
                    NewColumnData.UniqueItems[Index, 0]:=Parsed[0];
                    NewColumnData.UniqueItems[Index, 1]:=Parsed[1];

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

    var ColumnDataPos:=GetColumnDataPos(FColumnNumber);

    var UpdatedColumnData: TColumnData;
    UpdatedColumnData.ColumnName  :=FColumnName;
    UpdatedColumnData.ColumnNumber:=FColumnNumber;
    UpdatedColumnData.IsFiltered  :=True;
    UpdatedColumnData.UniqueItems :=FFilterList[ColumnDataPos].UniqueItems;

    FInUse:=True;
    FFilterList[ColumnDataPos]:=UpdatedColumnData;

    for var Index:=0 to Length(FFilterList[ColumnDataPos].UniqueItems) - 1 do
    begin
        var IsChecked:=FilterList.Checked[Index];
        FFilterList[ColumnDataPos].UniqueItems[Index, 1]:=IfThen(IsChecked, 'True', 'False');
    end;

    Inc(FFilteredColumns);

    for var Index:=1 to FSourceGrid.RowCount - 1 do
    begin

        var Value:=FSourceGrid.Cells[ColumnNumber, Index].Trim();

        if GetState(Value, ColumnDataPos) then
            FSourceGrid.RowHeights[Index]:=FSourceGrid.sgRowHeight
                else FSourceGrid.RowHeights[Index]:=FSourceGrid.sgRowHidden;

    end;

end;


procedure TFilterForm.UnfilterSourceGrid();
begin


end;


procedure TFilterForm.RemoveAllFilters();
begin



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

    THelpers.ExecWithDelay(500, procedure
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
    UnfilterSourceGrid();
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

