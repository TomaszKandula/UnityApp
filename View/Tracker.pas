unit Tracker;


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
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.ComCtrls,
    Data.Win.ADODB,
    InterposerClasses;


type


    TTrackerForm = class(TForm)
        Label6: TLabel;
        ListEmailFrom: TComboBox;
        TextReminder1: TLabeledEdit;
        TextReminder2: TLabeledEdit;
        TextReminder3: TLabeledEdit;
        ListLayout: TComboBox;
        TextReminder4: TLabeledEdit;
        Label5: TLabel;
        ErrorEmailFrom: TLabel;
        Exp_Rem3_Switch: TCheckBox;
        Exp_Rem2_Switch: TCheckBox;
        GroupBoxMiddle: TGroupBox;
        GroupBoxLeft: TGroupBox;
        GroupBoxClient: TGroupBox;
        CustomerList: TListView;
        PanelBottom: TPanel;
        btnOK: TSpeedButton;
        btnCancel: TSpeedButton;
        PanelClient: TPanel;
        Memo: TMemo;
        Help: TGroupBox;
        btnApply: TSpeedButton;
        PanelCustomerList: TPanel;
        TextReminder0: TLabeledEdit;
        btnSelection: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure btnOKClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure CustomerListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
        procedure btnApplyClick(Sender: TObject);
        procedure CustomerListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnSelectionClick(Sender: TObject);
    protected
        var Multiselect: TStringList;
        var CtrlClicked: boolean;
    private
        var pTrackerGrid  : TStringGrid;
        var pAgeGrid      : TStringGrid;
        var pReminderMail : string;
        var pStatementMail: string;
    public
        property  TrackerGrid  : TStringGrid read pTrackerGrid;
        property  AgeGrid      : TStringGrid read pAgeGrid;
        property  ReminderMail : string      read pReminderMail  write pReminderMail;
        property  StatementMail: string      read pStatementMail write pStatementMail;
        procedure GetSendFrom(List: TComboBox);
        procedure GetEmailAddress(Scuid: string);
        procedure SetEmailAddresses(List: TListView);
        procedure GetLayouts(LayoutContainer: TComboBox);
        procedure ApplyTimings(List: TListView);
        procedure SaveToDb(List: TListView);
    end;


var
    TrackerForm: TTrackerForm;


implementation


uses
    Main,
    SqlHandler,
    DbModel,
    Worker,
    Settings,
    DbHandler,
    Helpers;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


procedure TTrackerForm.GetSendFrom(List: TComboBox);
var
    Database: TDataTables;
    CoCode1:  string;
    CoCode2:  string;
    CoCode3:  string;
    CoCode4:  string;
begin

    // Get Co Codes that are opened (age view snapshot)
    if MainForm.tcCOCODE1.Caption <> 'n/a' then CoCode1:=MainForm.tcCOCODE1.Caption;
    if MainForm.tcCOCODE2.Caption <> 'n/a' then CoCode2:=MainForm.tcCOCODE2.Caption;
    if MainForm.tcCOCODE3.Caption <> 'n/a' then CoCode3:=MainForm.tcCOCODE3.Caption;
    if MainForm.tcCOCODE4.Caption <> 'n/a' then CoCode4:=MainForm.tcCOCODE4.Caption;

    Database:=TDataTables.Create(MainForm.DbConnect);
    try
        Database.Columns.Add(TSql.DISTINCT + TCompanyData.SendNoteFrom);
        Database.CustFilter:=TSql.WHERE +
                                TCompanyData.CoCode + TSql.EQUAL + QuotedStr(COCODE1) +
                             TSql._OR +
                                TCompanyData.CoCode + TSql.EQUAL + QuotedStr(COCODE2) +
                             TSql._OR +
                                TCompanyData.CoCode + TSql.EQUAL + QuotedStr(COCODE3) +
                             TSql._OR +
                                TCompanyData.CoCode + TSql.EQUAL + QuotedStr(COCODE4);
        Database.OpenTable(TCompanyData.CompanyData);
        if not(Database.DataSet.RecordCount = 0) then
            Database.SqlToSimpleList(List, Database.DataSet)
                else
                    MainForm.MsgCall(TCommon.TMsgTypes.Warn, 'Cannot find assigned email address to your organisation. Please contact IT support.');
    finally
        Database.Free;

        if List.Items.Count > 0 then
        begin
            ErrorEmailFrom.Visible:=False;
            List.ItemIndex:=0;
        end
        else
        begin
            ErrorEmailFrom.Visible:=True;
            List.Items.Clear;
        end;

    end;

end;


procedure TTrackerForm.GetEmailAddress(Scuid: string);
var
    Database: TDataTables;
begin

    Database:=TDataTables.Create(MainForm.DbConnect);

    try
        Database.Columns.Add(TAddressBook.Emails);
        Database.Columns.Add(TAddressBook.Estatements);
        Database.CustFilter:=TSql.WHERE + TAddressBook.Scuid + TSql.EQUAL + Scuid;
        Database.OpenTable(TAddressBook.AddressBook);

        if Database.DataSet.RecordCount > 0 then
        begin
            ReminderMail:=MainForm.OleGetStr(Database.DataSet.Fields[TAddressBook.Emails].Value);
            StatementMail:=MainForm.OleGetStr(Database.DataSet.Fields[TAddressBook.Estatements].Value);
        end;

    finally
        Database.Free;
    end;

end;


procedure TTrackerForm.SetEmailAddresses(List: TListView);
var
    iCNT: integer;
begin

    if List.Items.Count > 0 then
    begin
        for iCNT:=0 to List.Items.Count - 1 do
        begin

            GetEmailAddress(List.Items[iCNT].SubItems[1]);

            if not(string.IsNullOrEmpty(StatementMail)) then
                List.Items[iCNT].SubItems[4]:=StatementMail;

            if not(string.IsNullOrEmpty(ReminderMail)) then
                List.Items[iCNT].SubItems[5]:=ReminderMail;

        end;
    end;

end;


/// <remarks>
/// Layout file format: "reminder_<team>_<country>_<number>.htm".
/// Remove last six characters to display in list box.
/// Note: We use TStringList as a middleman to remove duplicates.
/// </remarks>

procedure TTrackerForm.GetLayouts(LayoutContainer: TComboBox);
var
    Settings:   ISettings;
    SearchRec:  TSearchRec;
    LayoutLst:  TStringList;
    LayoutFld:  string;
    LayoutNam:  string;
    iCNT:       integer;
begin

    Settings :=TSettings.Create;
    LayoutFld:=Settings.GetLayoutDir;
    LayoutLst:=TStringList.Create;

    try

        // Get files from local folder
        try
            {$WARN SYMBOL_PLATFORM OFF} { faArchives - Windows only }
            if FindFirst(LayoutFld + 'r*.htm', faArchive, SearchRec) = 0 then
            {$WARN SYMBOL_PLATFORM ON}
            begin

                LayoutLst.Sorted:=True;
                LayoutLst.Duplicates:=dupIgnore;
                repeat
                    LayoutNam:=SearchRec.Name;
                    LayoutNam:=LayoutNam.Substring(0, LayoutNam.Length - 6);
                    LayoutLst.Add(LayoutNam);
                until
                    FindNext(SearchRec) <> 0;

                FindClose(SearchRec);

            end;

            // Display
            LayoutContainer.Clear;
            for iCNT:=0 to LayoutLst.Count - 1 do
                LayoutContainer.Items.Add(LayoutLst.Strings[iCNT]);

            if LayoutContainer.Items.Count > 0 then
                LayoutContainer.ItemIndex:=0;

        except
            on E: Exception do
                MainForm.MsgCall(Error, 'Cannot get list of layouts. Please contact IT support.');

        end;

    finally

      LayoutLst.Free;

    end;

end;


procedure TTrackerForm.ApplyTimings(List: TListView);
var
    SelItem: integer;
    iCNT:    integer;
begin

    // First reminder and legal notice cannot be zero
    if (TextReminder1.Text = '0') or (TextReminder4.Text = '0') then
    begin
        MainForm.MsgCall(Warn, 'Please provide value for Reminder 1 and Legal Action different than zero.');
        Exit;
    end;

    if (Exp_Rem2_Switch.Checked) and (TextReminder2.Text = '0') then
    begin
        MainForm.MsgCall(Warn, 'Please provide value for Reminder 2 different than zero.');
        Exit;
    end;

    if (Exp_Rem3_Switch.Checked) and (TextReminder3.Text = '0') then
    begin
        MainForm.MsgCall(Warn, 'Please provide value for Reminder 3 different than zero.');
        Exit;
    end;

    // Applay only on selected customers
    if Multiselect.Count > 0 then
    begin
        for iCNT:=0 to Multiselect.Count - 1 do
        begin
            SelItem:=Multiselect.Strings[iCNT].ToInteger;
            List.Items[SelItem].SubItems[3] :=ListEmailFrom.Text;
            List.Items[SelItem].SubItems[6] :=TextReminder0.Text;
            List.Items[SelItem].SubItems[7] :=TextReminder1.Text;
            List.Items[SelItem].SubItems[8] :=TextReminder2.Text;
            List.Items[SelItem].SubItems[9] :=TextReminder3.Text;
            List.Items[SelItem].SubItems[10]:=TextReminder4.Text;
        end;
    end
    // Apply on all listed customers
    else
    begin
        if MainForm.MsgCall(Question2, 'You have not selected any customers, do you want Unity to apply proposed conditions for all listed items?') = mrYes then
        begin
            for iCNT:=0 to List.Items.Count - 1 do
            begin
                List.Items[iCNT].SubItems[3] :=ListEmailFrom.Text;
                List.Items[iCNT].SubItems[6] :=TextReminder0.Text;
                List.Items[iCNT].SubItems[7] :=TextReminder1.Text;
                List.Items[iCNT].SubItems[8] :=TextReminder2.Text;
                List.Items[iCNT].SubItems[9] :=TextReminder3.Text;
                List.Items[iCNT].SubItems[10]:=TextReminder4.Text;
            end;
        end;
    end;

end;


procedure TTrackerForm.SaveToDb(List: TListView);
var
    TrackerData: TDataTables;
    TempData:    TStringGrid;
    iCNT:        integer;
    Check:       integer;
begin

    Check:=0;

    for iCNT:=0 to List.Items.Count - 1 do
    begin
        // Not found
        if List.Items[iCNT].SubItems[4] = 'Not found' then Inc(Check);
        if List.Items[iCNT].SubItems[5] = 'Not found' then Inc(Check);
        // Not set
        if List.Items[iCNT].SubItems[3]  = 'Not set' then Inc(Check);
        if List.Items[iCNT].SubItems[6]  = 'Not set' then Inc(Check);
        if List.Items[iCNT].SubItems[7]  = 'Not set' then Inc(Check);
        if List.Items[iCNT].SubItems[8]  = 'Not set' then Inc(Check);
        if List.Items[iCNT].SubItems[9]  = 'Not set' then Inc(Check);
        if List.Items[iCNT].SubItems[10] = 'Not set' then Inc(Check);
    end;

    if Check > 0 then
    begin
        MainForm.MsgCall(Warn, 'Please make sure you that the list do not contain "not found" or "not set" items.');
        Exit;
    end;

    TrackerData:=TDataTables.Create(MainForm.DbConnect);
    TempData:=TStringGrid.Create(nil);
    try
        try
            // Select database columns
            TrackerData.CleanUp;
            TrackerData.Columns.Add(TTrackerData.UserAlias);
            TrackerData.Columns.Add(TTrackerData.Cuid);
            TrackerData.Columns.Add(TTrackerData.CoCode);
            TrackerData.Columns.Add(TTrackerData.Branch);
            TrackerData.Columns.Add(TTrackerData.CustomerName);
            TrackerData.Columns.Add(TTrackerData.Stamp);
            TrackerData.Columns.Add(TTrackerData.SendReminder1);
            TrackerData.Columns.Add(TTrackerData.SendReminder2);
            TrackerData.Columns.Add(TTrackerData.SendReminder3);
            TrackerData.Columns.Add(TTrackerData.SendReminder4);
            TrackerData.Columns.Add(TTrackerData.Sciud);
            TrackerData.Columns.Add(TTrackerData.ReminderLayout);
            TrackerData.Columns.Add(TTrackerData.PreStatement);
            TrackerData.Columns.Add(TTrackerData.SendFrom);
            TrackerData.Columns.Add(TTrackerData.StatementTo);
            TrackerData.Columns.Add(TTrackerData.ReminderTo);

            // Put ListView data to StringGrid
            TempData.RowCount:=List.Items.Count;
            TempData.ColCount:=TrackerData.Columns.Count;
            for iCNT:=0 to List.Items.Count - 1 do
            begin
                TempData.Cells[0,  iCNT]:=MainForm.WinUserName;             // user alias
                TempData.Cells[1,  iCNT]:=List.Items[iCNT].SubItems[0];     // cuid
                TempData.Cells[2,  iCNT]:=List.Items[iCNT].SubItems[11];    // co code
                TempData.Cells[3,  iCNT]:=List.Items[iCNT].SubItems[12];    // branch/agent
                TempData.Cells[4,  iCNT]:=List.Items[iCNT].SubItems[2];     // cust name
                TempData.Cells[5,  iCNT]:=DateTimeToStr(Now);               // stamp
                TempData.Cells[6,  iCNT]:=List.Items[iCNT].SubItems[7];     // reminder 1
                TempData.Cells[7,  iCNT]:=List.Items[iCNT].SubItems[8];     // reminder 2
                TempData.Cells[8,  iCNT]:=List.Items[iCNT].SubItems[9];     // reminder 3
                TempData.Cells[9,  iCNT]:=List.Items[iCNT].SubItems[10];    // reminder 4
                TempData.Cells[10, iCNT]:=List.Items[iCNT].SubItems[1];     // scuid
                TempData.Cells[11, iCNT]:=ListLayout.Text;                  // layout
                TempData.Cells[12, iCNT]:=List.Items[iCNT].SubItems[6];     // pre-statement
                TempData.Cells[13, iCNT]:=List.Items[iCNT].SubItems[3];     // send from
                TempData.Cells[14, iCNT]:=List.Items[iCNT].SubItems[4];     // statement to
                TempData.Cells[15, iCNT]:=List.Items[iCNT].SubItems[5];     // reminder to
            end;

            // Insert data to database
            if TrackerData.InsertInto(TTrackerData.TrackerData, True, TempData, nil, False) then
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Invoice tracker data updated.');
                MainForm.MsgCall(Info, 'Invoice Tracker has been updates successfully!');
            end
            else
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Invoice tracker error, cannot perform transaction.');
                MainForm.MsgCall(Error, 'Cannot perform transaction. Please contact IT support.');
            end;

        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Error has been thrown [Tracker_SaveToDb]: ' + E.Message);
                MainForm.MsgCall(Error, E.Message);
            end;
        end;
    finally
        TrackerData.Free;
        TempData.Free;
    end;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TTrackerForm.FormCreate(Sender: TObject);
var
    lsColumns: TListColumn;
begin

    Multiselect:=TStringList.Create;

    // List view initialization

    // From debtors view
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LP';
    lsColumns.Width  :=40;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Cuid'; //0
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Scuid';//1
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer Name';
    lsColumns.Width  :=150;    //2

    // From address book
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Send from';
    lsColumns.Width  :=100;    //3
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Statement to';
    lsColumns.Width  :=100;    //4
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Reminder to';
    lsColumns.Width  :=100;    //5

    // Timings
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Pre-statement';
    lsColumns.Width  :=100;    //6
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Reminder 1';
    lsColumns.Width  :=100;    //7
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Reminder 2';
    lsColumns.Width  :=100;    //8
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Reminder 3';
    lsColumns.Width  :=100;    //9
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Legal Action';
    lsColumns.Width  :=100;    //10

    // Company details
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Co Code';
    lsColumns.Width  :=100;    //11
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Agent';
    lsColumns.Width  :=100;    //12

    PanelCustomerList.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);

end;


procedure TTrackerForm.FormDestroy(Sender: TObject);
begin
    if Assigned(Multiselect) then
        Multiselect.Free;
end;


procedure TTrackerForm.FormShow(Sender: TObject);
begin
    ListLayout.Clear;
    ListEmailFrom.Clear;
    ErrorEmailFrom.Visible:=True;
    TextReminder0.Text:='0';
    TextReminder1.Text:='0';
    TextReminder2.Text:='0';
    TextReminder3.Text:='0';
    TextReminder4.Text:='0';
    Exp_Rem2_Switch.Checked:=True;
    Exp_Rem3_Switch.Checked:=True;
end;


procedure TTrackerForm.FormActivate(Sender: TObject);
begin
    Screen.Cursor:=crSQLWait;
    GetSendFrom(ListEmailFrom);
    SetEmailAddresses(CustomerList);
    GetLayouts(ListLayout);
    Screen.Cursor:=crDefault;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ BUTTONS EVENTS //


procedure TTrackerForm.btnApplyClick(Sender: TObject);
begin
    ApplyTimings(CustomerList);
end;


procedure TTrackerForm.btnOKClick(Sender: TObject);
begin
    SaveToDb(CustomerList);
end;


procedure TTrackerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


procedure TTrackerForm.CustomerListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    CtrlClicked:=True;
end;


procedure TTrackerForm.CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    CtrlClicked:=False;
end;


procedure TTrackerForm.btnSelectionClick(Sender: TObject);
begin
    CustomerList.ClearSelection;
end;


procedure TTrackerForm.CustomerListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin

    if not(CtrlClicked) and (Selected) then
    begin
        Multiselect.Clear;
        Multiselect.Add(Item.Index.ToString);
    end
    else if (CtrlClicked) and (Selected) then
        Multiselect.Add(Item.Index.ToString)
    else
        Multiselect.Clear;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TTrackerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


end.

