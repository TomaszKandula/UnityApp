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
        procedure cbSelectAllClick(Sender: TObject);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
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
        procedure FilterSelectCheck();
        procedure RecalcAgeViewSummary_Callback(CallResponse: TCallResponse; AgingSummary: TAgingSummary);
    public
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
    for var iCNT:=0 to FilterList.Count - 1 do
        if FilterList.Checked[iCNT] = True then
            Inc(Check);

    // If all selected, then tick
    if Check = FilterList.Count then
        cbSelectAll.Checked:=True
            else cbSelectAll.Checked:=False;

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

    // Check if column has been already queried
    if FFilterList.Count > 0 then
    begin

        for var Index:=0 to FFilterList.Count - 1 do
        begin

            // If so, ready the filter state
            if FFilterList[Index].ColumnNumber = FColumnNumber then
            begin

                FilterList.Clear();

                if FFilterList[Index].UniqueItems = nil then
                begin
                    THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, 'Array has not been created.');
                    Exit();
                end;

                for var Items:=0 to Length(FFilterList[Index].UniqueItems) - 1 do
                begin
                    FilterList.Items.Add(FFilterList[Index].UniqueItems[Items, 0]);
                    FilterList.Checked[FilterList.Items.Count - 1]:=FFilterList[Index].UniqueItems[Items, 1].ToBoolean();
                end;

            end;

        end;

    end
    else
    // Make new query and display unique values
    begin

        var StringList:=TStringList.Create();
        try

            StringList.Sorted:=True;
            StringList.Duplicates:=dupIgnore;

            // Get unique values to string list, skip header
            for var Index:=1 to FSourceGrid.RowCount - 1 do
            begin

                var Value:=FSourceGrid.Cells[FColumnNumber, Index].Trim();

                if FSourceGrid.RowHeights[Index] = FSourceGrid.sgRowHidden then
                    Value:=Value + '&&False' else Value:=Value + '&&True';

                StringList.Add(Value);

            end;

            var NewColumnData: TColumnData;
            NewColumnData.ColumnName:=FColumnName;
            NewColumnData.ColumnNumber:=FColumnNumber;
            NewColumnData.IsFiltered:=False;
            SetLength(NewColumnData.UniqueItems, StringList.Count, 2);

            for var Index:=0 to StringList.Count - 1 do
            begin

                var Value:=StringList.Strings[Index];
                var Parsed:=Value.Split(['&&']);

                FilterList.Items.Add(Parsed[0]);
                NewColumnData.UniqueItems[Index, 0]:=Parsed[0];
                NewColumnData.UniqueItems[Index, 1]:=Parsed[1];

                if Parsed[1] = 'False' then
                    FilterList.Checked[Index]:=False
                        else FilterList.Checked[Index]:=True;

            end;

            FFilterList.Add(NewColumnData);

        finally
            StringList.Free();
        end;

    end;

end;


procedure TFilterForm.FilterSourceGrid();

    function GetState(ASearchValue: string; AColumnDataPos: integer): boolean;
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

    function GetColumnDataPos(AColumnNumber: integer): integer;
    begin

        Result:=0;

        for var Index:=0 to FFilterList.Count - 1 do
        begin

            if FFilterList[Index].ColumnNumber = AColumnNumber then
            begin
                Result:=Index;
                Exit();
            end;

        end;

    end;

begin

    var ColumnDataPos:=GetColumnDataPos(FColumnNumber);

    var UpdatedColumnData: TColumnData;
    UpdatedColumnData.ColumnName  :=FColumnName;
    UpdatedColumnData.ColumnNumber:=FColumnNumber;
    UpdatedColumnData.IsFiltered  :=True;
    UpdatedColumnData.UniqueItems :=FFilterList[ColumnDataPos].UniqueItems;

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


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TFilterForm.FormCreate(Sender: TObject);
begin
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    if not Assigned(FFilterList) then FFilterList:=TList<TColumnData>.Create();
end;


procedure TFilterForm.FormShow(Sender: TObject);
begin
    FilterList.Clear();
    LoadUniqueValues();
end;


procedure TFilterForm.FormActivate(Sender: TObject);
begin
    FilterSelectCheck();
    btnRemove.Enabled:=False;
    //FInUse:=
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

    Screen.Cursor:=crDefault;
    BusyForm.Close();

    FSourceGrid.Freeze(False);
    FSourceGrid.Repaint();

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(FilterForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[RecalcAgeViewSummary_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
    end;

    MainForm.UpdateAgeSummary(AgingSummary);
    MainForm.UpdateFollowUps(MainForm.sgAgeView);

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


procedure TFilterForm.cbSelectAllClick(Sender: TObject);
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

