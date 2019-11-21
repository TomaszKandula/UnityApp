unit View.CompanyList;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.CheckLst,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Unity.Grid;


type


    TCompanyListForm = class(TForm)
        PanelBackground: TPanel;
        btnSelect: TSpeedButton;
        PanelListItems: TPanel;
        FilterList: TCheckListBox;
        cbSelectAll: TCheckBox;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure btnSelectClick(Sender: TObject);
        procedure cbSelectAllClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    strict private
        var FCheckEvent: boolean;
        var FIsDataLoaded: boolean;
    end;


    function CompanyListForm(): TCompanyListForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Async.Debtors,
    Async.Utilities,
    Unity.Helpers,
    Unity.Chars;


var VCompanyListForm: TCompanyListForm;


function CompanyListForm(): TCompanyListForm;
begin
    if not(Assigned(VCompanyListForm)) then Application.CreateForm(TCompanyListForm, VCompanyListForm);
    Result:=VCompanyListForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TCompanyListForm.FormCreate(Sender: TObject);
begin
    FilterList.Clear();
end;


procedure TCompanyListForm.FormShow(Sender: TObject);
begin
    {Do nothing}
end;


procedure TCompanyListForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crHourGlass;

        THelpers.ExecWithDelay(500, procedure
        begin

            FilterList.Clear();
            var Utilities: IUtilities:=TUtilities.Create();
            var GetCoCodeList: TStringGrid;
            try

                GetCoCodeList:=Utilities.GetCompanyCodesAwaited();
                for var iCNT:=1 {Skip header} to GetCoCodeList.RowCount - 1 do
                    FilterList.Items.Add(GetCoCodeList.Cells[1{Skip Lp}, iCNT]);

            finally
                GetCoCodeList.Free();
            end;

            Screen.Cursor:=crDefault;
            FIsDataLoaded:=True;

        end);

    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- CLOSING //


procedure TCompanyListForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsDataLoaded:=False;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- CLICK EVENTS //


procedure TCompanyListForm.btnSelectClick(Sender: TObject);
begin

    var SelectedCoCodes: string;
    var ListEnd:=FilterList.Count - 1;

    for var iCNT:=0 to ListEnd do
    begin

        if FilterList.Checked[iCNT] = True then
            SelectedCoCodes:=SelectedCoCodes + FilterList.Items[iCNT] + ','

    end;

    SelectedCoCodes:=SelectedCoCodes.Substring(0, Length(SelectedCoCodes) - 1);

    var Debtors: IDebtors:=TDebtors.Create();
    Debtors.ReadAgeViewAsync(SelectedCoCodes, 0, MainForm.ReadAgeView_Callback);

end;


procedure TCompanyListForm.cbSelectAllClick(Sender: TObject);
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


procedure TCompanyListForm.FilterListClick(Sender: TObject);
begin

    try

        if not(FCheckEvent) then
        begin

            if FilterList.Checked[FilterList.ItemIndex] then
                FilterList.Checked[FilterList.ItemIndex]:=False
            else
                FilterList.Checked[FilterList.ItemIndex]:=True;

        end;

    except
        {Do nothing}
    end;

    FCheckEvent:=False;

end;


procedure TCompanyListForm.FilterListClickCheck(Sender: TObject);
begin
    FCheckEvent:=True;
end;


// ---------------------------------------------------------------------------------------------------------------------------------------- KEYBOARDS EVENTS //


procedure TCompanyListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.ESC then Close();
end;


end.
