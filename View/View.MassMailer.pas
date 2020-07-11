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
    Unity.Enums,
    Api.ReturnCompanyEmails,
    Api.SentDocument;


type


    TMassMailerForm = class(TForm)
        PanelBottom: TPanel;
        txtWarning: TLabel;
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
        Text_Begin: TLabel;
        Text_End: TLabel;
        ValBeginDate: TLabel;
        ValEndDate: TLabel;
        btnBeginDate: TSpeedButton;
        btnEndDate: TSpeedButton;
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
        cbIncludeSource: TCheckBox;
        GroupFiltering: TGroupBox;
        GroupOptions: TGroupBox;
        cbShowAll: TRadioButton;
        cbOverdueOnly: TRadioButton;
        cbNonOverdue: TRadioButton;
        cbNotDueOnly: TRadioButton;
        EditAgent: TEdit;
        txtAgent: TLabel;
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
        procedure CustomerListItemChecked(Sender: TObject; Item: TListItem);
    strict private
        var FIsDataLoaded:  boolean;
        var FLbuEmails: TArray<TArray<string>>;
        var FCompanyEmails: TArray<TArray<string>>;
        const FLargestSubitemTxt = -1;
        const FSizeOfTxtInHeader = -2;
        procedure ListViewAutoFit(List: TListView; const AutoFit: integer);
        function GetEmailAddress(SourceDbName: string; CustNumber: string; Source: TStringGrid): string;
        procedure SetEmailAddresses(List: TListView);
        procedure SetLbuEmails(var LbuEmailsList: TListBox; SelectedCompany: string; Source: TArray<TArray<string>>);
        procedure SetLbuCompanies(var LbuCompanyList: TComboBox; Source: TArray<TArray<string>>);
        procedure FinishStartUp();
        procedure LoadFromGrid();
        procedure ExecuteMailer();
        procedure GetCompanyEmails_Callback(PayLoad: TReturnCompanyEmails);
        procedure SendAccountDocument_Callback(PayLoad: TSentDocument);
    end;


    function MassMailerForm(): TMassMailerForm;


implementation


{$R *.dfm}


uses
    Vcl.Clipbrd,
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
    Api.AddressBookFields,
    Api.SendDocument,
    Api.DocumentFields,
    Api.DocumentStatusFields;


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


procedure TMassMailerForm.GetCompanyEmails_Callback(PayLoad: TReturnCompanyEmails);
begin

    if not PayLoad.IsSucceeded then
    begin
        THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Error, PayLoad.Error.ErrorDesc);
        Service.Logger.Log('[GetCompanyEmails_Callback]: Error has been thrown "' + PayLoad.Error.ErrorDesc + '".');
        Exit();
    end;

    var Items:=Length(PayLoad.Details);
    if not Items > 0 then Exit();

    SetLength(FCompanyEmails, Items, 6);
    for var Index:=0 to Items - 1 do
    begin

        FCompanyEmails[Index, 0]:=PayLoad.Details[Index].SourceDbName;

        var PreservedLen:=Length(FLbuEmails);
        SetLength(FLbuEmails, PreservedLen + Length(PayLoad.Details[Index].CompanyEmails), 2);

        for var jCNT:=0 to Length(PayLoad.Details[Index].CompanyEmails) - 1 do
        begin
            FLbuEmails[jCNT + PreservedLen, 0]:=PayLoad.Details[Index].SourceDbName;
            FLbuEmails[jCNT + PreservedLen, 1]:=PayLoad.Details[Index].CompanyEmails[jCNT];
        end;

    end;

    FinishStartUp();

end;


procedure TMassMailerForm.SetEmailAddresses(List: TListView);
begin

    if List.Items.Count > 0 then
    begin

        for var Index:=0 to List.Items.Count - 1 do
        begin

            var lstSourceDbName  :=List.Items[Index].SubItems[5]; // SourceDbName
            var lstCustomerNumber:=List.Items[Index].SubItems[0]; // CustomerNumber

            var EmailAddress:=GetEmailAddress(lstSourceDbName, lstCustomerNumber, MainForm.sgAddressBook);

            if not(string.IsNullOrEmpty(EmailAddress)) then List.Items[Index].SubItems[4]:=EmailAddress // StatementEmails
                else EmailAddress:='Not found!';

        end;

    end;

end;


procedure TMassMailerForm.SetLbuEmails(var LbuEmailsList: TListBox; SelectedCompany: string; Source: TArray<TArray<string>>);
begin

    LbuEmailsList.Clear();

    for var Index:=0 to Length(Source) - 1 do
        if Source[Index, 0] = SelectedCompany then
            LbuEmailsList.Items.Add(Source[Index, 1]);

end;


procedure TMassMailerForm.SetLbuCompanies(var LbuCompanyList: TComboBox; Source: TArray<TArray<string>>);
begin

    var NoDuplicates:=TStringList.Create();
    try

        NoDuplicates.Sorted:=True;
        NoDuplicates.Duplicates:=dupIgnore;

        for var Index:=0 to Length(Source) - 1 do
            NoDuplicates.Add(Source[Index, 0]);

        LbuCompanyList.Clear();
        LbuCompanyList.Items.AddStrings(NoDuplicates);

        if LbuCompanyList.Items.Count > 0 then LbuCompanyList.ItemIndex:=0;

    finally
        NoDuplicates.Free();
    end;

end;


procedure TMassMailerForm.FinishStartUp();
begin

    ListViewAutoFit(CustomerList, FSizeOfTxtInHeader);
    SetLbuCompanies(selCompany, FLbuEmails);

    FIsDataLoaded:=True;
    Screen.Cursor:=crDefault;

    Text_Subject.SetFocus();
    MainForm.UpdateStatusBar(TStatusBar.Ready);
    CustomerList.ClearSelection();

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

            Item:=MassMailerForm.CustomerList.Items.Add();
            Item.Caption:=IntToStr(iCNT);
            Item.SubItems.Add(LCustomerNumber);
            Item.SubItems.Add(LCustomerName);
            Item.SubItems.Add('No');
            Item.SubItems.Add('Not set!');
            Item.SubItems.Add('Not found!');
            Item.SubItems.Add(LSourceDbName);

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

    var InvFilter: string;

    if cbShowAll.Checked     then InvFilter:=TFilterType.StatementAllItems;
    if cbOverdueOnly.Checked then InvFilter:=TFilterType.ReminderOverdueAll;
    if cbNonOverdue.Checked  then InvFilter:=TFilterType.ReminderOverdueRange;
    if cbNotDueOnly.Checked  then InvFilter:=TFilterType.StatementAllFuture;

    var TotalItems:=0;
    for var Index:=0 to CustomerList.Items.Count - 1 do
        if CustomerList.Items[Index].SubItems[4] <> 'Not found!' then
            Inc(TotalItems);

    var BeginDate:='';
    var EndDate:='';

    if String.IsNullOrEmpty(ValBeginDate.Caption) then BeginDate:=DateToStr(Now()) else BeginDate:=ValBeginDate.Caption;
    if String.IsNullOrEmpty(ValEndDate.Caption) then EndDate:=DateToStr(Now()) else EndDate:=ValEndDate.Caption;

    var FPayLoad:=TSendDocument.Create(TotalItems);

    FPayLoad.LayoutType     :=TLayoutType.Custom;
    FPayLoad.ReportedAgeDate:=MainForm.LoadedAgeDate;
    FPayLoad.Subject        :=Text_Subject.Text;
    FPayLoad.Message        :=StringReplace(Text_Message.Text, TChars.CRLF, '<br>', [rfReplaceAll]);
    FPayLoad.UserEmail      :=Service.SessionData.EmailAddress;
    FPayLoad.InvoiceFilter  :=InvFilter;
    FPayLoad.Agent          :=EditAgent.Text;
    FPayLoad.BeginDate      :=BeginDate;
    FPayLoad.EndDate        :=EndDate;
    FPayLoad.IsCtrlStatus   :=not cbCtrlStatusOff.Checked;
    FPayLoad.IsUserInCopy   :=cbUserInCopy.Checked;
    FPayLoad.IsSourceInCopy :=cbIncludeSource.Checked;

    for var Index:=0 to CustomerList.Items.Count - 1 do
    begin

        if CustomerList.Items[Index].SubItems[4] <> 'Not found!' then
        begin

            var DocumentFields:=TDocumentFields.Create();
            DocumentFields.CustomerNumber:=CustomerList.Items[Index].SubItems[0].ToInt64(); // Customer Number
            DocumentFields.SourceDbName  :=CustomerList.Items[Index].SubItems[5]; // SourceDbName
            DocumentFields.SendFrom      :=CustomerList.Items[Index].SubItems[3]; // Send from
            DocumentFields.EmailTo       :=CustomerList.Items[Index].SubItems[4]; // Send to

            FPayLoad.Documents[Index]:=DocumentFields;

        end;

    end;

    Screen.Cursor:=crHourGlass;
    MainForm.UpdateStatusBar(TStatusBar.Processing);
    BusyForm.Show();

    Service.Mediator.Documents.SendAccountDocumentAsync(FPayLoad, SendAccountDocument_Callback);

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TMassMailerForm.SendAccountDocument_Callback(PayLoad: TSentDocument);
begin

    Screen.Cursor:=crDefault;
    MainForm.UpdateStatusBar(TStatusBar.Ready);
    BusyForm.Close();

    if not PayLoad.IsSucceeded then
    begin
        THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Error, PayLoad.Error.ErrorDesc);
        Exit();
    end;

    for var ListIndex:=0 to CustomerList.Items.Count - 1 do
    begin

        for var PayLoadIndex:=0 to Length(PayLoad.SentDocuments) - 1 do
        begin

            if (CustomerList.Items[ListIndex].SubItems[0] = PayLoad.SentDocuments[PayLoadIndex].CustomerNumber.ToString())
            and (CustomerList.Items[ListIndex].SubItems[5] = PayLoad.SentDocuments[PayLoadIndex].SourceDbName) then
                CustomerList.Items[ListIndex].SubItems[2]:='Yes';

        end;

    end;

    THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Info, 'All listed items have been processed within ' + PayLoad.Meta.ProcessingTimeSpan + '.');

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TMassMailerForm.FormCreate(Sender: TObject);
begin

    var lsColumns: TListColumn;

    for var Index:=0 to 11 do
    begin

        lsColumns:=CustomerList.Columns.Add;

        case Index of
            0:  lsColumns.Caption:='Lp';
            1:  lsColumns.Caption:='Customer number';
            2:  lsColumns.Caption:='Customer name';
            3:  lsColumns.Caption:='Is sent?';
            4:  lsColumns.Caption:='Send from';
            5:  lsColumns.Caption:='Send to';
            6:  lsColumns.Caption:='Source';
        end;

    end;

    PanelEmailContainer.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelSubject.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelMessage.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);

    lstLbuEmails.Clear();

    cbShowAll.Checked:=True;
    ValBeginDate.Caption:='2010-01-01';
    ValEndDate.Caption:='';

end;


procedure TMassMailerForm.FormShow(Sender: TObject);
begin
    lstLbuEmails.Clear();
    SetLength(FLbuEmails, 0, 2);
    SetLength(FCompanyEmails, 0, 6);
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
            Service.Mediator.Companies.GetCompanyEmailsAsync(MainForm.LoadedCompanies, GetCompanyEmails_Callback);
        end);

    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TMassMailerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsDataLoaded:=False;
    CustomerList.Clear();
end;


procedure TMassMailerForm.FormDestroy(Sender: TObject);
begin
    SetLength(FLbuEmails, 0);
    SetLength(FCompanyEmails, 0);
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TMassMailerForm.CustomerListItemChecked(Sender: TObject; Item: TListItem);
begin
    Item.Selected:=True;
end;


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

    for var Index:=0 to CustomerList.Items.Count - 1 do
    begin

        if CustomerList.Items[Index].SubItems[5] = selCompany.Text then // SourceDbName
            CustomerList.Items[Index].SubItems[3]:=lstLbuEmails.Items[lstLbuEmails.ItemIndex]; // SentFrom

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
    if cbShowAll.Checked then ImgCover.Visible:=True;
end;


procedure TMassMailerForm.cbOverdueOnlyClick(Sender: TObject);
begin
    if cbOverdueOnly.Checked then ImgCover.Visible:=True;
end;


procedure TMassMailerForm.cbNonOverdueClick(Sender: TObject);
begin
    if cbNonOverdue.Checked then ImgCover.Visible:=False;
end;


procedure TMassMailerForm.cbNotDueOnlyClick(Sender: TObject);
begin
    if cbNotDueOnly.Checked then ImgCover.Visible:=True;
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

    // <CTRL> + <C>
    if (Key = 67) and (Shift = [ssCtrl]) then
    begin

        var ClipboardContent:='';

        for var ItemsCount:=0 to CustomerList.Items.Count - 1 do
        begin

            if CustomerList.Items[ItemsCount].Checked then
            begin

                var ColumnsContent:='';

                for var ColCounts:=0 to CustomerList.Items[ItemsCount].SubItems.Count - 1 do
                        ColumnsContent:=ColumnsContent + CustomerList.Items[ItemsCount].SubItems[ColCounts] + TChars.Tab;

                ClipboardContent:=ClipboardContent + ColumnsContent + TChars.CrLf;

            end;

        end;

        if not String.IsNullOrEmpty(ClipboardContent) then
        begin
            ClipBoard.AsText:=ClipboardContent;
            THelpers.MsgCall(MassMailerForm.Handle, TAppMessage.Info, 'Clipboard has been populated successfully.');
        end;

    end;

end;


{$ENDREGION}


end.

