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
    Vcl.Imaging.pngimage,
    Unity.Grid,
    Unity.Panel;


type


    TCompanyListForm = class(TForm)
        PanelBackground: TPanel;
        btnSelect: TSpeedButton;
        PanelListItems: TPanel;
        FilterList: TCheckListBox;
        PanelButtons: TPanel;
        ImageGrip: TImage;
        PanelHeader: TPanel;
        btnRemove: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnSelectClick(Sender: TObject);
        procedure btnRemoveClick(Sender: TObject);
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
    Unity.Chars,
    Unity.Enums;


var VCompanyListForm: TCompanyListForm;


function CompanyListForm(): TCompanyListForm;
begin
    if not(Assigned(VCompanyListForm)) then Application.CreateForm(TCompanyListForm, VCompanyListForm);
    Result:=VCompanyListForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TCompanyListForm.FormCreate(Sender: TObject);
begin
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
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
            var GetCoCodeList:=TStringList.Create();

            try
                Utilities.GetCompanyCodesAwaited(GetCoCodeList);
                FilterList.Items.AddStrings(GetCoCodeList);
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
    {FIsDataLoaded:=False;}
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- CLICK EVENTS //


procedure TCompanyListForm.btnSelectClick(Sender: TObject);

    function GetCoCodeOnly(InputStr: string): string;
    begin
        var SeparatorPos:=AnsiPos('-', InputStr);
        Result:=InputStr.Substring(0, SeparatorPos - 1);
    end;

begin

    var SelectedCoCodes: string;
    var ListEnd:=FilterList.Count - 1;

    for var iCNT:=0 to ListEnd do
    begin

        if FilterList.Checked[iCNT] = True then
            SelectedCoCodes:=SelectedCoCodes + GetCoCodeOnly(FilterList.Items[iCNT]) + ','

    end;

    SelectedCoCodes:=SelectedCoCodes.Substring(0, Length(SelectedCoCodes) - 1).Replace(' ','');

    if String.IsNullOrWhiteSpace(SelectedCoCodes) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please select company/companies you want to open.');
        Exit();
    end;

    MainForm.LoadAgeReport(SelectedCoCodes);
    Close();

end;


procedure TCompanyListForm.btnRemoveClick(Sender: TObject);
begin

    for var iCNT:=0 to FilterList.Count - 1 do
        if FilterList.ItemEnabled[iCNT] = True then
            FilterList.Checked[iCNT]:=False;

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
