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
        procedure btnFilterClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure cbSelectAllClick(Sender: TObject);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
    strict private
        var FInUse:        boolean;
        var CheckEvent:    boolean;
        var FColumnNumber: integer;
        var FSourceGrid:   TStringGrid;
        procedure LoadUniqueValues();
        procedure FilterSourceGrid();
        procedure FilterSelectCheck();
        procedure RecalcAgeViewSummary_Callback(CallResponse: TCallResponse; PayLoad: TAgingPayLoad);
    public
        property InUse:        boolean     read FInUse;
        property SourceGrid:   TStringGrid read FSourceGrid   write FSourceGrid;
        property ColumnNumber: integer     read FColumnNumber write FColumnNumber;
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

    var StringList:=TStringList.Create;
    try

        StringList.Sorted:=True;
        StringList.Duplicates:=dupIgnore;

        // Get unique values to string list, skip header
        for var IndexRow:=1 to FSourceGrid.RowCount - 1 do
        begin

            var Value:=FSourceGrid.Cells[FColumnNumber, IndexRow].Trim();

            if FSourceGrid.RowHeights[IndexRow] = FSourceGrid.sgRowHidden then
                Value:=Value + '&&False' else Value:=Value + '&&True';

            StringList.Add(Value);

        end;

        // Move to filter list and untick hidden rows
        for var IndexRow:=0 to StringList.Count - 1 do
        begin

            var Value:=StringList.Strings[IndexRow];
            var Parsed:=Value.Split(['&&']);

            FilterList.Items.Add(Parsed[0]);

            if Parsed[1] = 'False' then
                FilterList.Checked[IndexRow]:=False
                    else FilterList.Checked[IndexRow]:=True;

        end;

    finally
        StringList.Free();
    end;

end;


procedure TFilterForm.FilterSourceGrid();
begin

    for var IndexLst:=0 to FilterList.Count - 1 do
    begin

        for var IndexSrc:=1 to FSourceGrid.RowCount - 1 do
        begin

            var Value:=FSourceGrid.Cells[ColumnNumber, IndexSrc].Trim();

            if FilterList.Items[IndexLst] = Value then
            begin

                if FilterList.Checked[IndexLst] = True then
                    FSourceGrid.RowHeights[IndexSrc]:=FSourceGrid.sgRowHeight
                        else FSourceGrid.RowHeights[IndexSrc]:=FSourceGrid.sgRowHidden;

            end;

        end;

    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TFilterForm.FormCreate(Sender: TObject);
begin
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
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


{$REGION 'CALLBACKS'}


procedure TFilterForm.RecalcAgeViewSummary_Callback(CallResponse: TCallResponse; PayLoad: TAgingPayLoad);
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

    MainForm.UpdateAgeSummary(PayLoad);
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

