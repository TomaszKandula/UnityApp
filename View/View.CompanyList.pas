unit View.CompanyList;

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
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.CheckLst,
    Vcl.Buttons,
    Vcl.ExtCtrls,
    Vcl.Imaging.pngimage,
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
        procedure FormDestroy(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FilterListClick(Sender: TObject);
        procedure FilterListClickCheck(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnSelectClick(Sender: TObject);
        procedure btnRemoveClick(Sender: TObject);
    strict private
        var FCheckEvent: boolean;
        var FIsDataLoaded: boolean;
        var FCompaniesHolder: TArray<TArray<string>>;
        function GetCoCodeOnly(CompanyName: string): string;
        procedure RequestAgeReport();
        procedure ClearCompanyList();
        procedure CheckItem(var CheckEvent: boolean);
    end;


    function CompanyListForm(): TCompanyListForm;


implementation


{$R *.dfm}


uses
    System.Generics.Collections,
    View.Main,
    Async.Debtors,
    Async.Accounts,
    Unity.Helpers,
    Unity.Grid,
    Unity.Constants,
    Unity.Enums,
    Unity.Records;


var VCompanyListForm: TCompanyListForm;


function CompanyListForm(): TCompanyListForm;
begin
    if not(Assigned(VCompanyListForm)) then Application.CreateForm(TCompanyListForm, VCompanyListForm);
    Result:=VCompanyListForm;
end;


{$REGION 'LOCAL HELPERS'}


function TCompanyListForm.GetCoCodeOnly(CompanyName: string): string;
begin
    var SeparatorPos:=AnsiPos('-', CompanyName);
    Result:=CompanyName.Substring(0, SeparatorPos - 1).Replace(' ','');
end;


procedure TCompanyListForm.RequestAgeReport();
begin

    var SelectedCoCodes: string;{Legacy}
    var ListEnd:=FilterList.Count - 1;

    for var iCNT:=0 to ListEnd do
    begin

        if FilterList.Checked[iCNT] = True then
        begin
            MainForm.LoadedCompanies.Add(GetCoCodeOnly(FilterList.Items[iCNT]));
            SelectedCoCodes:=SelectedCoCodes + GetCoCodeOnly(FilterList.Items[iCNT]) + ','{Legacy}
        end;

    end;

    SelectedCoCodes:=SelectedCoCodes.Substring(0, Length(SelectedCoCodes) - 1).Replace(' ','');{Legacy}

    {Legacy}
    if String.IsNullOrWhiteSpace(SelectedCoCodes) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please select company/companies you want to open.');
        Exit();
    end;

    var Accounts: IAccounts:=TAccounts.Create();
    var CallResponse: TCallResponse;
    CallResponse:=Accounts.SaveUserCompanyListAwaited(MainForm.LoadedCompanies);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    MainForm.LoadAgeReport(SelectedCoCodes{Legacy});
    Close();

 end;


procedure TCompanyListForm.ClearCompanyList();
begin
    for var iCNT:=0 to FilterList.Count - 1 do
        if FilterList.ItemEnabled[iCNT] = True then
            FilterList.Checked[iCNT]:=False;
end;


procedure TCompanyListForm.CheckItem(var CheckEvent: boolean);
begin

    try

        if not(CheckEvent) then
        begin

            if FilterList.Checked[FilterList.ItemIndex] then
                FilterList.Checked[FilterList.ItemIndex]:=False
            else
                FilterList.Checked[FilterList.ItemIndex]:=True;

        end;

    except
        {Do nothing}
    end;

    CheckEvent:=False;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TCompanyListForm.FormCreate(Sender: TObject);
begin
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    FilterList.Clear();
    SetLength(FCompaniesHolder, 1, 2);
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
            var Accounts: IAccounts:=TAccounts.Create();
            var CallResponse: TCallResponse;

            CallResponse:=Accounts.GetUserCompanyListAwaited(FCompaniesHolder);

            if CallResponse.IsSucceeded then for var iCNT:=0 to Length(FCompaniesHolder) - 1 do
            begin
                FilterList.Items.Add(FCompaniesHolder[iCNT, 0]);
                if FCompaniesHolder[iCNT, 1] = '-1' then FilterList.Checked[iCNT]:=True;
            end;

            Screen.Cursor:=crDefault;
            FIsDataLoaded:=True;

        end);

    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TCompanyListForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsDataLoaded:=False;
end;


procedure TCompanyListForm.FormDestroy(Sender: TObject);
begin
    FCompaniesHolder:=nil;
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TCompanyListForm.btnSelectClick(Sender: TObject);
begin
    RequestAgeReport();
end;


procedure TCompanyListForm.btnRemoveClick(Sender: TObject);
begin
    ClearCompanyList();
end;


procedure TCompanyListForm.FilterListClick(Sender: TObject);
begin
    CheckItem(FCheckEvent);
end;


procedure TCompanyListForm.FilterListClickCheck(Sender: TObject);
begin
    FCheckEvent:=True;
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TCompanyListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.ESC then Close();
end;


{$ENDREGION}


end.
