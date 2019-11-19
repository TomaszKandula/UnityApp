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
    Vcl.ExtCtrls;


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
    strict private
        var FCheckEvent: boolean;
    end;


    function CompanyListForm(): TCompanyListForm;


implementation


{$R *.dfm}


uses
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
    {Do nothing}
end;


procedure TCompanyListForm.FormShow(Sender: TObject);
begin
    {Do nothing}
end;


procedure TCompanyListForm.FormActivate(Sender: TObject);
begin

    // Wait 500ms
    // Get from UnityApi list of company codes with latest age date
    // Show on the list

    // Get user saved selections
    // Update items

end;


// -------------------------------------------------------------------------------------------------------------------------------------------- CLICK EVENTS //


procedure TCompanyListForm.btnSelectClick(Sender: TObject);
begin

    // call to api to get aging for selected co codes

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
