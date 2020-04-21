unit View.MassMailer;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.Generics.Collections,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ComCtrls,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    Vcl.Imaging.pngimage,
    Unity.Grid,
    Unity.Panel,
    Unity.ListView,
    Unity.Records,
    Unity.References,
    Unity.Enums,
    Api.ReturnCompanyDetails;


type


    TMassMailerForm = class(TForm)
        PanelBottom: TPanel;
        Text_Warn: TLabel;
        btnCancel: TSpeedButton;
        btnSendEmail: TSpeedButton;
        PanelClient: TPanel;
        TextMessage: TLabel;
        PanelMessage: TPanel;
        Text_Message: TMemo;
        PanelList: TPanel;
        CustomerList: TListView;
        PanelSubject: TPanel;
        TextSubject: TLabel;
        PanelEmailContainer: TPanel;
        Text_Subject: TEdit;
        PanelOption: TPanel;
        Shape_Dates: TShape;
        Shape_Options: TShape;
        Text_Begin: TLabel;
        Text_End: TLabel;
        Text_Options: TLabel;
        ValBeginDate: TLabel;
        ValEndDate: TLabel;
        btnBeginDate: TSpeedButton;
        btnEndDate: TSpeedButton;
        cbNonOverdue: TCheckBox;
        cbOverdueOnly: TCheckBox;
        cbShowAll: TCheckBox;
        DueDateLabel: TLabel;
        ImgCover: TImage;
        btnDelBegin: TSpeedButton;
        btnDelEnd: TSpeedButton;
        GroupEmails: TGroupBox;
        txtCompany: TLabel;
        cbUserInCopy: TCheckBox;
        cbCtrlStatusOff: TCheckBox;
        selCompany: TComboBox;
        btnApply: TSpeedButton;
        lstLbuEmails: TListBox;
        grSettings: TGroupBox;
        shapeLbuEmails: TShape;
        cbMergeList: TCheckBox;
        cbNotDueOnly: TCheckBox;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnCancelClick(Sender: TObject);
        procedure btnSendEmailClick(Sender: TObject);
        procedure btnBeginDateClick(Sender: TObject);
        procedure btnEndDateClick(Sender: TObject);
        procedure btnDelBeginClick(Sender: TObject);
        procedure btnDelEndClick(Sender: TObject);
        procedure btnApplyClick(Sender: TObject);
        procedure Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbShowAllClick(Sender: TObject);
        procedure cbOverdueOnlyClick(Sender: TObject);
        procedure cbNonOverdueClick(Sender: TObject);
        procedure CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure selCompanySelect(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure cbNotDueOnlyClick(Sender: TObject);
        procedure cbNotDueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    strict private
        var OpenItemsRefs: TFOpenItemsRefs;
        var CtrlStatusRefs: TFCtrlStatusRefs;
        var FPayLoad: TAccDocumentPayLoad;
        var FItemCount: integer;
        var FIsDataLoaded:  boolean;
        var FLbuEmails: TArray<TArray<string>>;
        var FCompanyDetails: TArray<TArray<string>>;
        const FLargestSubitemTxt = -1;
        const FSizeOfTxtInHeader = -2;
        procedure ListViewAutoFit(List: TListView; const AutoFit: integer);
        function GetEmailAddress(SourceDbName: string; CustNumber: string; Source: TStringGrid): string;
        procedure SetCompanyDetails(var TargetList: TListView; SourceArray: TArray<TArray<string>>);
        procedure SetEmailAddresses(List: TListView);
        procedure SetLbuEmails(var LbuEmailsList: TListBox; SelectedCompany: string; Source: TArray<TArray<string>>);
        procedure SetLbuCompanies(var LbuCompanyList: TComboBox; Source: TArray<TArray<string>>);
        procedure FinishStartUp();
        procedure LoadFromGrid();
        procedure ExecuteMailer();
    public
        property ItemCount: integer read FItemCount write FItemCount;
        procedure GetCompanyDetails_Callback(CompanyDetails: TReturnCompanyDetails; CallResponse: TCallResponse);
        procedure SendAccDocumentsAsync_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);
    end;


    function MassMailerForm(): TMassMailerForm;


implementation


{$R *.dfm}


uses
    View.BusyScreen,
    View.Main,
    View.Calendar,
    View.Actions,
    Unity.Helpers,
    Unity.Settings,
    Unity.Constants,
    Unity.Service,
    Api.ReturnOpenItems,
    Api.OpenItemsFields,
    Api.CustomerSnapshotEx,
    Api.AddressBookFields;


var vMassMailerForm: TMassMailerForm;


function MassMailerForm(): TMassMailerForm;
begin
    if not(Assigned(vMassMailerForm)) then Application.CreateForm(TMassMailerForm, vMassMailerForm);
    Result:=vMassMailerForm;
end;


{$REGION 'LOCAL HELPERS'}


procedure TMassMailerForm.ListViewAutoFit(List: TListView; const AutoFit: integer);
begin

    for var iCNT:=0 to List.Columns.Count - 1 do if iCNT < 7 then
        List.Column[iCNT].Width:=AutoFit else List.Column[iCNT].Width:=0;

end;


function TMassMailerForm.GetEmailAddress(SourceDbName: string; CustNumber: string; Source: TStringGrid): string;
begin

    Result:='';

    var Col1:=Source.GetCol(TAddressBookFields._SourceDbName);
    var Col2:=Source.GetCol(TAddressBookFields._CustomerNumber);

    for var iCNT:=1 to Source.RowCount - 1 do
    begin

        var srcSourceDbName  :=Source.Cells[Col1, iCNT];
        var srcCustomerNumber:=Source.Cells[Col2, iCNT];

        if (SourceDbName = srcSourceDbName) and (CustNumber = srcCustomerNumber) then
        begin
            Result:=Source.Cells[Source.GetCol(TAddressBookFields._StatementEmails), iCNT];
            Break;
        end;

    end;

end;


procedure TMassMailerForm.GetCompanyDetails_Callback(CompanyDetails: TReturnCompanyDetails; CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[GetCompanyDetails_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    var Items:=Length(CompanyDetails.CompanyDetails);
    if not Items > 0 then Exit();

    SetLength(FCompanyDetails, Items, 6);
    for var Index:=0 to Items - 1 do
    begin

        var LbuPhones :=THelpers.ArrayStrToString(CompanyDetails.CompanyDetails[Index].CompanyPhones, ';');
        var Exclusions:=THelpers.ArrayIntToString(CompanyDetails.CompanyDetails[Index].Exclusions, ';');
        var LbuBanks  :=THelpers.BankListToHtml(CompanyDetails.CompanyDetails[Index].CompanyBanks);

        FCompanyDetails[Index, 0]:=CompanyDetails.CompanyDetails[Index].SourceDbName;
        FCompanyDetails[Index, 1]:=CompanyDetails.CompanyDetails[Index].CompanyName;
        FCompanyDetails[Index, 2]:=CompanyDetails.CompanyDetails[Index].CompanyAddress;
        FCompanyDetails[Index, 3]:=LbuPhones;
        FCompanyDetails[Index, 4]:=LbuBanks;
        FCompanyDetails[Index, 5]:=Exclusions;

        var PreservedLen:=Length(FLbuEmails);
        SetLength(FLbuEmails, PreservedLen + Length(CompanyDetails.CompanyDetails[Index].CompanyPhones), 2);

        for var jCNT:=0 to Length(CompanyDetails.CompanyDetails[Index].CompanyEmails) - 1 do
        begin
            FLbuEmails[jCNT + PreservedLen, 0]:=CompanyDetails.CompanyDetails[Index].SourceDbName;
            FLbuEmails[jCNT + PreservedLen, 1]:=CompanyDetails.CompanyDetails[Index].CompanyEmails[jCNT];
        end;

    end;

    FinishStartUp();

end;


procedure TMassMailerForm.SetCompanyDetails(var TargetList: TListView; SourceArray: TArray<TArray<string>>);
begin

    for var iCNT:=0 to TargetList.Items.Count - 1 do
    begin

        for var jCNT:=0 to Length(SourceArray) - 1 do
        begin

            var ListSourceDbName:=SourceArray[jCNT, 0];
            var CompSourceDbName:=TargetList.Items[iCNT].SubItems[5];

            if ListSourceDbName = CompSourceDbName then
            begin
                TargetList.Items[iCNT].SubItems[7]{LbuName}    :=SourceArray[jCNT, 1{LbuName}];
                TargetList.Items[iCNT].SubItems[8]{LbuAddress} :=SourceArray[jCNT, 2{LbuAddress}];
                TargetList.Items[iCNT].SubItems[9]{LbuPhones}  :=SourceArray[jCNT, 3{LbuPhones}];
                TargetList.Items[iCNT].SubItems[10]{Exclusions}:=SourceArray[jCNT, 5{Exclusions}];
                TargetList.Items[iCNT].SubItems[6]{BanksHtml}  :=SourceArray[jCNT, 4{BanksHtml}];
            end;

        end;

    end;

end;


procedure TMassMailerForm.SetEmailAddresses(List: TListView);
begin

    if List.Items.Count > 0 then
    begin

        for var iCNT:=0 to List.Items.Count - 1 do
        begin

            var lstSourceDbName  :=List.Items[iCNT].SubItems[5]{SourceDbName};
            var lstCustomerNumber:=List.Items[iCNT].SubItems[0]{CustomerNumber};

            var EmailAddress:=GetEmailAddress(lstSourceDbName, lstCustomerNumber, MainForm.sgAddressBook);

            if not(string.IsNullOrEmpty(EmailAddress)) then List.Items[iCNT].SubItems[4]{StatementEmails}:=EmailAddress
                else EmailAddress:='Not found!';

        end;

    end;

end;


procedure TMassMailerForm.SetLbuEmails(var LbuEmailsList: TListBox; SelectedCompany: string; Source: TArray<TArray<string>>);
begin

    LbuEmailsList.Clear();

    for var iCNT:=0 to Length(Source) - 1 do
        if Source[iCNT, 0] = SelectedCompany then
            LbuEmailsList.Items.Add(Source[iCNT, 1]);

end;


procedure TMassMailerForm.SetLbuCompanies(var LbuCompanyList: TComboBox; Source: TArray<TArray<string>>);
begin

    var NoDuplicates:=TStringList.Create();
    try

        NoDuplicates.Sorted:=True;
        NoDuplicates.Duplicates:=dupIgnore;

        for var iCNT:=0 to Length(Source) - 1 do
            NoDuplicates.Add(Source[iCNT, 0]);

        LbuCompanyList.Clear();
        LbuCompanyList.Items.AddStrings(NoDuplicates);

        if LbuCompanyList.Items.Count > 0 then LbuCompanyList.ItemIndex:=0;

    finally
        NoDuplicates.Free();
    end;

end;


procedure TMassMailerForm.FinishStartUp();
begin

    SetCompanyDetails(CustomerList, FCompanyDetails);
    ListViewAutoFit(CustomerList, FSizeOfTxtInHeader);
    SetLbuCompanies(selCompany, FLbuEmails);

    MainForm.TimerCustSnapshots.Enabled:=False;
    Service.Logger.Log('[TMassMailerForm.FormActivate]: Mass mailer has been opened, open items loader is on hold.');

    Text_Subject.SetFocus();
    FIsDataLoaded:=True;
    Screen.Cursor:=crDefault;
    MainForm.UpdateStatusBar(TStatusBar.Ready);

end;


procedure TMassMailerForm.LoadFromGrid();
begin

    var Item: TListItem;
    CustomerList.Clear();

    for var iCNT:=MainForm.sgAgeView.Selection.Top to MainForm.sgAgeView.Selection.Bottom do
    begin

        if MainForm.sgAgeView.RowHeights[iCNT] <> MainForm.sgAgeView.sgRowHidden then
        begin

            var LCustomerNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TCustomerSnapshotEx._CustomerNumber), iCNT];
            var LCustomerName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TCustomerSnapshotEx._CustomerName), iCNT];
            var LSourceDbName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TCustomerSnapshotEx._SourceDbName), iCNT];

            // Visible fields
            Item:=MassMailerForm.CustomerList.Items.Add();
            Item.Caption:=IntToStr(iCNT);
            Item.SubItems.Add(LCustomerNumber);
            Item.SubItems.Add(LCustomerName);
            Item.SubItems.Add('No');
            Item.SubItems.Add('Not set!');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add(LSourceDbName);

            // Hidden fields
            Item.SubItems.Add('empty');
            Item.SubItems.Add('empty');
            Item.SubItems.Add('empty');
            Item.SubItems.Add('empty');
            Item.SubItems.Add('empty');

        end;

    end;

end;


procedure TMassMailerForm.ExecuteMailer();
begin

    if (String.IsNullOrEmpty(Text_Subject.Text)) or (String.IsNullOrEmpty(Text_Message.Text))
    then
    begin

        THelpers.MsgCall(
            MassMailerForm.Handle,
            TAppMessage.Warn,
            'Cannot send incomplete form. Please re-check it and try again.'
        );

        Exit();

    end;

    if THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Question2, 'Do you want to send it now?') = IDNO then Exit();

    var InvFilter: TInvoiceFilter:=TInvoiceFilter.ShowAllItems;

    if cbShowAll.Checked     then InvFilter:=TInvoiceFilter.ShowAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TInvoiceFilter.ReminderOvd;
    if cbNonOverdue.Checked  then InvFilter:=TInvoiceFilter.ReminderNonOvd;
    if cbNotDueOnly.Checked  then InvFilter:=TInvoiceFilter.SendNotDue;

    // -----------------------------------
    // Get item count for sendable emails.
    // -----------------------------------
    for var iCNT:=0 to CustomerList.Items.Count - 1 do
        if CustomerList.Items[iCNT].SubItems[4] <> 'Not found!' then
            ItemCount:=ItemCount + 1;

    var MessStr:=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);

    MainForm.sgOpenItems.MSort(MainForm.sgOpenItems.GetCol(TOpenItemsFields._PmtStatus), TDataType.TFloat, True);
    OpenItemsRefs.InitWith(MainForm.sgOpenItems);
    CtrlStatusRefs.InitWith(MainForm.sgControlStatus);

    FPayLoad.Layout        :=TDocMode.Custom;
    FPayLoad.Subject       :=Text_Subject.Text;
    FPayLoad.Message       :=MessStr;
    FPayLoad.InvFilter     :=InvFilter;
    FPayLoad.BeginDate     :=ValBeginDate.Caption;
    FPayLoad.EndDate       :=ValEndDate.Caption;
    FPayLoad.MailerList    :=MassMailerForm.CustomerList;
    FPayLoad.OpenItems     :=MainForm.sgOpenItems;
    FPayLoad.OpenItemsRefs :=OpenItemsRefs;
    FPayLoad.ControlStatus :=MainForm.sgControlStatus;
    FPayLoad.CtrlStatusRefs:=CtrlStatusRefs;
    FPayLoad.IsCtrlStatus  :=cbCtrlStatusOff.Checked;
    FPayLoad.IsUserInCopy  :=cbUserInCopy.Checked;

    Screen.Cursor:=crHourGlass;
    MainForm.UpdateStatusBar(TStatusBar.Processing);
    BusyForm.Show();

    Service.Mediator.Documents.SendAccDocumentsAsync(MainForm.LoadedAgeDate, FPayLoad, SendAccDocumentsAsync_Callback);

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TMassMailerForm.SendAccDocumentsAsync_Callback(ProcessingItemNo: integer; CallResponse: TCallResponse);

    procedure ActivityProgressOff();
    begin
        Screen.Cursor:=crDefault;
        MainForm.UpdateStatusBar(TStatusBar.Ready);
        BusyForm.Close();
    end;

begin

    if not CallResponse.IsSucceeded then
    begin
        ActivityProgressOff();
        THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    if CallResponse.LastMessage <> 'Processed.' then
    begin

        if ProcessingItemNo > -1 then
        begin
            CustomerList.Items[ProcessingItemNo].SubItems[2]:='Sent';
            ItemCount:=MassMailerForm.ItemCount - 1;
        end

    end
    else if CallResponse.LastMessage = 'Processed.' then
    begin
        ActivityProgressOff();
        THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Info, 'All listed items have been processed.');
    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TMassMailerForm.FormCreate(Sender: TObject);
begin

    var lsColumns: TListColumn;

    for var iCNT:=0 to 11 do
    begin

        lsColumns:=CustomerList.Columns.Add;

        case iCNT of
            // Visible fields
            0:  lsColumns.Caption:='Lp';
            1:  lsColumns.Caption:='Customer number';
            2:  lsColumns.Caption:='Customer name';
            3:  lsColumns.Caption:='Is sent?';
            4:  lsColumns.Caption:='Send from';
            5:  lsColumns.Caption:='Send to';
            6:  lsColumns.Caption:='Source';
            // Hidden (helper) fields
            7:  lsColumns.Caption:='BanksHtml';
            8:  lsColumns.Caption:='LbuName';
            9:  lsColumns.Caption:='LbuAddress';
            10: lsColumns.Caption:='LbuPhone';
            11: lsColumns.Caption:='Exclusions';
        end;

    end;

    CustomerList.Column[7].Width:=0;
    CustomerList.Column[8].Width:=0;
    CustomerList.Column[9].Width:=0;
    CustomerList.Column[10].Width:=0;
    CustomerList.Column[11].Width:=0;

    PanelEmailContainer.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSubject.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelMessage.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);

    lstLbuEmails.Clear();
    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';

end;


procedure TMassMailerForm.FormShow(Sender: TObject);
begin
    SetLength(FLbuEmails, 0, 2);
    SetLength(FCompanyDetails, 0, 6);
end;


procedure TMassMailerForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crHourGlass;
        MainForm.UpdateStatusBar(TStatusBar.Processing);

        THelpers.ExecWithDelay(500, procedure
        begin
            LoadFromGrid();
            SetEmailAddresses(CustomerList);
            Service.Mediator.Companies.GetCompanyDetailsAsync(MainForm.LoadedCompanies, GetCompanyDetails_Callback);
        end);

    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TMassMailerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    // ------------------------------------------------
    // Turn on disabled timer for open items scanner,
    // so open items will not change during processing.
    // ------------------------------------------------
    FIsDataLoaded:=False;
    CustomerList.Clear();
    MainForm.TimerCustSnapshots.Enabled:=True;
    Service.Logger.Log('[TMassMailerForm.FormClose]: Mass mailer has been closed, open items loader is resumed.');
end;


procedure TMassMailerForm.FormDestroy(Sender: TObject);
begin
    // Do nonthing
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TMassMailerForm.selCompanySelect(Sender: TObject);
begin
    SetLbuEmails(lstLbuEmails, (Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex], FLbuEmails);
end;


procedure TMassMailerForm.btnApplyClick(Sender: TObject);
begin

    if lstLbuEmails.ItemIndex < 0 then
    begin
        THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Warn, 'Please select e-mail address you want to apply.');
        Exit();
    end;

    if CustomerList.Items.Count = 0 then Exit();

    for var iCNT:=0 to CustomerList.Items.Count - 1 do
    begin

        if CustomerList.Items[iCNT].SubItems[5]{SourceDbName} = selCompany.Text then
            CustomerList.Items[iCNT].SubItems[3]{SentFrom}:=lstLbuEmails.Items[lstLbuEmails.ItemIndex];

    end;

end;


procedure TMassMailerForm.btnBeginDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    THelpers.WndCall(CalendarForm, Modal);
    ValBeginDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TMassMailerForm.btnEndDateClick(Sender: TObject);
begin
    CalendarForm.FCalendarMode:=GetDate;
    THelpers.WndCall(CalendarForm, Modal);
    ValEndDate.Caption:=DateToStr(CalendarForm.FSelectedDate);
end;


procedure TMassMailerForm.cbShowAllClick(Sender: TObject);
begin

    if cbShowAll.Checked then
    begin
        cbOverdueOnly.Checked:=False;
        cbNonOverdue.Checked:=False;
        cbNotDueOnly.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and
        not(cbNonOverdue.Checked) and not(cbNotDueOnly.Checked) then cbShowAll.Checked:=True;

end;


procedure TMassMailerForm.cbOverdueOnlyClick(Sender: TObject);
begin

    if cbOverdueOnly.Checked then
    begin
        cbShowAll.Checked:=False;
        cbNonOverdue.Checked:=False;
        cbNotDueOnly.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and
        not(cbNonOverdue.Checked) then cbOverdueOnly.Checked:=True;

end;


procedure TMassMailerForm.cbNonOverdueClick(Sender: TObject);
begin

    if cbNonOverdue.Checked then
    begin
        cbShowAll.Checked:=False;
        cbOverdueOnly.Checked:=False;
        cbNotDueOnly.Checked:=False;
        ImgCover.Visible:=False;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and
        not(cbNonOverdue.Checked) and not(cbNotDueOnly.Checked) then cbNonOverdue.Checked:=True;

end;


procedure TMassMailerForm.cbNotDueOnlyClick(Sender: TObject);
begin

    if cbNotDueOnly.Checked then
    begin
        cbShowAll.Checked:=False;
        cbOverdueOnly.Checked:=False;
        cbNonOverdue.Checked:=False;
        ImgCover.Visible:=True;
    end;

    if not(cbShowAll.Checked) and not(cbOverdueOnly.Checked) and
        not(cbNonOverdue.Checked) and not(cbNotDueOnly.Checked) then cbNotDueOnly.Checked:=True;

end;


procedure TMassMailerForm.btnDelBeginClick(Sender: TObject);
begin
    ValBeginDate.Caption:='';
end;


procedure TMassMailerForm.btnDelEndClick(Sender: TObject);
begin
    ValEndDate.Caption:='';
end;


procedure TMassMailerForm.btnSendEmailClick(Sender: TObject);
begin
    ExecuteMailer();
end;


procedure TMassMailerForm.btnCancelClick(Sender: TObject);
begin
    Close();
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TMassMailerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


procedure TMassMailerForm.Text_SubjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Message.SetFocus();
end;


procedure TMassMailerForm.Text_MessageKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbShowAll.SetFocus();
end;


procedure TMassMailerForm.cbShowAllKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbOverdueOnly.SetFocus();
end;


procedure TMassMailerForm.cbOverdueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbNonOverdue.SetFocus();
end;


procedure TMassMailerForm.cbNonOverdueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then cbNotDueOnly.SetFocus();
end;


procedure TMassMailerForm.cbNotDueOnlyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then CustomerList.SetFocus();
end;


procedure TMassMailerForm.CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_TAB then Text_Subject.SetFocus();
end;


{$ENDREGION}


end.

