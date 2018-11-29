
{$I \Include\Header.inc}

unit Tracker;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, StrUtils, ADODB, ComCtrls, InterposerClasses;

type

    /// <summary>
    ///     View form class for registering customers to Invoice Tracker.
    /// </summary>

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
        procedure SaveToDb;
    end;

var
    TrackerForm: TTrackerForm;


implementation


uses
    Main, SQL, Model, Worker, Settings, Database;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///
/// </summary>

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
        Database.Columns.Add(DISTINCT + TCompany.SEND_NOTE_FROM);
        Database.CustFilter:=WHERE +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE1) +
                             _OR +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE2) +
                             _OR +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE3) +
                             _OR +
                                TCompany.CO_CODE + EQUAL + QuotedStr(COCODE4);
        Database.OpenTable(TblCompany);
        if not(Database.DataSet.RecordCount = 0) then
            Database.SqlToSimpleList(List, Database.DataSet)
                else
                    MainForm.MsgCall(mcWarn, 'Cannot find assigned email address to your organisation. Please contact IT support.');
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


/// <summary>
///     Set email that should be used for sending statements and reminders.
/// </summary>

procedure TTrackerForm.GetEmailAddress(Scuid: string);
var
    Database: TDataTables;
begin

    Database:=TDataTables.Create(MainForm.DbConnect);

    try
        Database.Columns.Add(TAddressBook.EMAILS);
        Database.Columns.Add(TAddressBook.ESTATEMENTS);
        Database.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + Scuid;
        Database.OpenTable(TblAddressbook);

        if Database.DataSet.RecordCount > 0 then
        begin
            ReminderMail:=MainForm.OleGetStr(Database.DataSet.Fields[TAddressBook.EMAILS].Value);
            StatementMail:=MainForm.OleGetStr(Database.DataSet.Fields[TAddressBook.ESTATEMENTS].Value);
        end;

    finally
        Database.Free;
    end;

end;


/// <summary>
///     Set email address used to send emails.
/// </summary>

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


/// <summary>
///
/// </summary>

/// <remarks>
///     Layout file format: "reminder_<team>_<country>_<number>.htm".
///     Remove last six characters to display in list box.
///     NOTE: We use TStringList as a middleman to remove duplicates.
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

        try

            // Get files from local folder
            if FindFirst(LayoutFld + 'r*.htm', faArchive, SearchRec) = 0 then
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
                MainForm.MsgCall(mcError, 'Cannot get list of layouts. Please contact IT support.');

        end;

    finally

      LayoutLst.Free;

    end;

end;


/// <summary>
///
/// </summary>

procedure TTrackerForm.ApplyTimings(List: TListView);
var
    SelItem: integer;
    iCNT:    integer;
begin

    // First reminder and legal notice cannot be zero
    if (TextReminder1.Text = '0') or (TextReminder4.Text = '0') then
    begin
        MainForm.MsgCall(mcWarn, 'Please provide value for Reminder 1 and Legal Action different than zero.');
        Exit;
    end;

    if (Exp_Rem2_Switch.Checked) and (TextReminder2.Text = '0') then
    begin
        MainForm.MsgCall(mcWarn, 'Please provide value for Reminder 2 different than zero.');
        Exit;
    end;

    if (Exp_Rem3_Switch.Checked) and (TextReminder3.Text = '0') then
    begin
        MainForm.MsgCall(mcWarn, 'Please provide value for Reminder 3 different than zero.');
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
        if MainForm.MsgCall(mcQuestion2, 'You have not selected any customers, do you want Unity to apply proposed conditions for all listed items?') = mrYes then
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


/// <summary>
///
/// </summary>

procedure TTrackerForm.SaveToDb;
//var
//    Database: TDataTables;
begin
//    Database:=TDataBase.Create(MainForm.DbConnect);
//    Database.CleanUp;
//
//    Database.Columns.Add(TTracker.USER_ALIAS);
//    Database.Columns.Add(TTracker.CUID);
//    Database.Columns.Add(TTracker.CO_CODE);
//    Database.Columns.Add(TTracker.BRANCH);
//    Database.Columns.Add(TTracker.CUSTNAME);
//    Database.Columns.Add(TTracker.STAMP);
//    Database.Columns.Add(TTracker.INDV_REM1);
//    Database.Columns.Add(TTracker.INDV_REM2);
//    Database.Columns.Add(TTracker.INDV_REM3);
//    Database.Columns.Add(TTracker.INDV_REM4);
//    Database.Columns.Add(TTracker.EXP_REM2);
//    Database.Columns.Add(TTracker.EXP_REM3);
//    Database.Columns.Add(TTracker.SCUID);
//    Database.Columns.Add(TTracker.LAYOUT);
//    Database.Columns.Add(TTracker.STATEMENT);
//
//    Database.Values.Add(MainForm.WinUserName);
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();
//    Database.Values.Add();



end;

// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TTrackerForm.FormCreate(Sender: TObject);
var
    Settings:  ISettings;
    lsColumns: TListColumn;
begin

    Multiselect:=TStringList.Create;
    Settings:=TSettings.Create;
    TrackerForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_TRACKER', APPCAPTION);

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

    PanelCustomerList.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);

end;


/// <summary>
///
/// </summary>

procedure TTrackerForm.FormDestroy(Sender: TObject);
begin
    if Assigned(Multiselect) then
        Multiselect.Free;
end;


/// <summary>
///
/// </summary>

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


/// <summary>
///
/// </summary>

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
    SaveToDb;
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
    if Key = ESC then Close;
end;


end.

