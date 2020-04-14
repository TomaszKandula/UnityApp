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
    Unity.Panel,
    Unity.Records,
    Api.UserCompanyList;


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
        procedure GetUserCompanyList_Callback(PayLoad: TUserCompanyList; CallResponse: TCallResponse);
    end;


    function CompanyListForm(): TCompanyListForm;


implementation


{$R *.dfm}


uses
    System.Generics.Collections,
    View.Main,
    Unity.Helpers,
    Unity.Grid,
    Unity.Constants,
    Unity.Enums,
    Unity.Service;


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

    var ListEnd:=FilterList.Count - 1;
    MainForm.LoadedCompanies.Clear();

    for var iCNT:=0 to ListEnd do
    begin

        if FilterList.Checked[iCNT] = True then
        begin
            var CoCode:=GetCoCodeOnly(FilterList.Items[iCNT]);
            MainForm.LoadedCompanies.Add(THelpers.GetSourceDBName(CoCode, 'F'));
        end;

    end;

    var CallResponse: TCallResponse;
    CallResponse:=Service.Mediator.Accounts.SaveUserCompanyListAwaited(MainForm.LoadedCompanies);

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(CompanyListForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    MainForm.LoadAgeReport();
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
        // Do nothing
    end;

    CheckEvent:=False;

end;


{$ENDREGION}


procedure TCompanyListForm.GetUserCompanyList_Callback(PayLoad: TUserCompanyList; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(CompanyListForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[GetUserCompanyList_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    var Count:=Length(PayLoad.UserCompanies);
    SetLength(FCompaniesHolder, Count, 2);

    for var Index:=0 to Count - 1 do
    begin
        FilterList.Items.Add(PayLoad.UserCompanies[Index].Companies);
        if PayLoad.UserCompanies[Index].IsSelected then FilterList.Checked[Index]:=True;
        FCompaniesHolder[Index, 0]:=PayLoad.UserCompanies[Index].Companies;
        FCompaniesHolder[Index, 1]:=PayLoad.UserCompanies[Index].IsSelected.ToString();
    end;

    Screen.Cursor:=crDefault;
    FIsDataLoaded:=True;

end;


{$REGION 'STARTUP'}


procedure TCompanyListForm.FormCreate(Sender: TObject);
begin
    PanelListItems.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    FilterList.Clear();
    SetLength(FCompaniesHolder, 1, 2);
end;


procedure TCompanyListForm.FormShow(Sender: TObject);
begin
    // Empty
end;


procedure TCompanyListForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crHourGlass;
        FilterList.Clear();

        THelpers.ExecWithDelay(500, procedure
        begin
            Service.Mediator.Accounts.GetUserCompanyListAsync(GetUserCompanyList_Callback);
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
