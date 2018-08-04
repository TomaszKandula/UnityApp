
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
        EmailFromList: TComboBox;
        TextStatTo: TLabeledEdit;
        ErrorStatTo: TLabel;
        AppMain: TShape;
        btnOK: TSpeedButton;
        btnCancel: TSpeedButton;
        TextReminder1: TLabeledEdit;
        TextReminder2: TLabeledEdit;
        TextReminder3: TLabeledEdit;
        TextMailTo: TLabeledEdit;
        TextLegalTo: TLabeledEdit;
        LayoutList: TComboBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        ErrorLegalTo: TLabel;
        ErrorMailTo: TLabel;
        TextReminder4: TLabeledEdit;
        Label4: TLabel;
        Label5: TLabel;
        Label7: TLabel;
        ErrorEmailFrom: TLabel;
        Exp_Rem2_Switch: TCheckBox;
        Exp_Rem3_Switch: TCheckBox;
        GroupBoxLeft: TGroupBox;
        GroupBoxBottom: TGroupBox;
        GroupBoxMid: TGroupBox;
        GroupBoxRight: TGroupBox;
        CustomerList: TListView;
        procedure FormCreate(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    private
        var pTrackerGrid  :  TStringGrid;
        var pAgeGrid      :  TStringGrid;
    public
        var UserAlias :  string;
        var CUID      :  string;
        var SCUID     :  string;
        var CoCode    :  string;
        var Branch    :  string;
        var CustNumber:  string;
        var CustName  :  string;
        var Layout    :  string;
        var Indv_Rem1 :  string;
        var Indv_Rem2 :  string;
        var Indv_Rem3 :  string;
        var Indv_Rem4 :  string;
        var Exp_Rem2  :  string;
        var Exp_Rem3  :  string;
        property  TrackerGrid :  TStringGrid read pTrackerGrid;
        property  AgeGrid     :  TStringGrid read pAgeGrid;
        procedure Display;
        procedure Add;
        procedure Delete;
        procedure GetData;
    end;

var
    TrackerForm: TTrackerForm;


implementation


uses
    Main, SQL, Model, Worker, Settings, Database;

{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Refresh invoice tracker list on application tab sheets (string grid component).
/// </summary>

procedure TTrackerForm.Display;
var
    TrackerItems: TDataTables;
    Source:       TStringGrid;
begin
    TrackerItems:=TDataTables.Create(MainForm.DbConnect);
    Source:=TrackerGrid;
    try
        TrackerGrid.Freeze(True);
        TrackerItems.StrSQL:=EXECUTE + TrackerList + SPACE + QuotedStr(UserAlias);
        TrackerItems.SqlToGrid(Source, TrackerItems.ExecSQL, False, True);
    finally
        TrackerItems.Free;
        TrackerGrid.Freeze(False);
    end;
end;

/// <summary>
///     Ad new customer to the invoice list.
/// </summary>

procedure TTrackerForm.Add;
var
    TrackerItems: TDataTables;
begin
    TrackerItems:=TDataTables.Create(MainForm.DbConnect);

    try

        // Clean up and fill with new data
        TrackerItems.CleanUp;
        TrackerItems.Columns.Add(TTracker.USER_ALIAS);  TrackerItems.Values.Add(UserAlias);
        TrackerItems.Columns.Add(TTracker.CUID);        TrackerItems.Values.Add(CUID);
        TrackerItems.Columns.Add(TTracker.CO_CODE);     TrackerItems.Values.Add(CoCode);
        TrackerItems.Columns.Add(TTracker.BRANCH);      TrackerItems.Values.Add(Branch);
        TrackerItems.Columns.Add(TTracker.CUSTNAME);    TrackerItems.Values.Add(CustName);
        TrackerItems.Columns.Add(TTracker.LAYOUT_ID);   TrackerItems.Values.Add(Layout);
        TrackerItems.Columns.Add(TTracker.STAMP);       TrackerItems.Values.Add(DateTimeToStr(Now));
        TrackerItems.Columns.Add(TTracker.INDV_REM1);   TrackerItems.Values.Add(Indv_Rem1);
        TrackerItems.Columns.Add(TTracker.INDV_REM2);   TrackerItems.Values.Add(Indv_Rem2);
        TrackerItems.Columns.Add(TTracker.INDV_REM3);   TrackerItems.Values.Add(Indv_Rem3);
        TrackerItems.Columns.Add(TTracker.INDV_REM4);   TrackerItems.Values.Add(Indv_Rem4);
        TrackerItems.Columns.Add(TTracker.EXP_REM2);    TrackerItems.Values.Add(Exp_Rem2);
        TrackerItems.Columns.Add(TTracker.EXP_REM3);    TrackerItems.Values.Add(Exp_Rem3);

        // Execute
        if TrackerItems.InsertInto(TblTracker, ttExplicit) then
            MainForm.MsgCall(mcInfo, 'Customer has been successfuly added to the Invoice Tracker.')
                else
                    MainForm.MsgCall(mcError, 'Cannot execute writing to database. Please contact with IT support.');

    finally
        TrackerItems.Free;
    end;

end;

/// <summary>
///     Delete given customer.
/// </summary>

procedure TTrackerForm.Delete;
var
    TrackerItems: TDataTables;
    PrimaryTable: string;
    ForeignTable: string;
begin

    TrackerItems:=TDataTables.Create(MainForm.DbConnect);

    try
        PrimaryTable:=DELETE_FROM + TblTracker  + WHERE + TTracker.CUID  + EQUAL + QuotedStr(CUID);  { HOLDS RECORDED CUSTOMERS }
        ForeignTable:=DELETE_FROM + TblInvoices + WHERE + TInvoices.CUID + EQUAL + QuotedStr(CUID);  { HOLDS CUSTOMERS INVOICES }
        TrackerItems.StrSQL:=ForeignTable + ';' + PrimaryTable;
        TrackerItems.ExecSQL;
        TrackerGrid.DeleteRowFrom(1, 1);
  finally
        TrackerItems.Free;
  end;

end;

/// <summary>
///     Retrieve and display data.
/// </summary>

procedure TTrackerForm.GetData;
var
    Tables: TDataTables;
begin


    Screen.Cursor:=crHourGlass;
    CUID      :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCUID,            1, 1), AgeGrid.Row];
    CoCode    :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), AgeGrid.Row];
    Branch    :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fAGENT,           1, 1), AgeGrid.Row];
    CustName  :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), AgeGrid.Row];
    CustNumber:=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), AgeGrid.Row];
    SCUID     :=CustNumber + MainForm.ConvertName(CoCode, 'F', 3);

    Tables:=TDataTables.Create(MainForm.DbConnect);

    try
        // Get list of layouts
        Tables.Columns.Add(TReminderLayouts.LAYOUTNAME);
        Tables.OpenTable(TblReminderLayouts);

        if Tables.DataSet.RecordCount > 0 then
        begin
            while not Tables.DataSet.EOF do
            begin
                LayoutList.Items.Add(Tables.DataSet.Fields[TReminderLayouts.LAYOUTNAME].Value);
                Tables.DataSet.MoveNext;
            end;

            LayoutList.ItemIndex:=0;

        end;

        Tables.CleanUp;

        // Get data from Address Book
        Tables.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
        Tables.OpenTable(TblAddressbook);

        if Tables.DataSet.RecordCount = 1 then { CUID/SCUDI IS UNIQUE }
        begin
            TextMailTo.Text:=Tables.DataSet.Fields[TAddressBook.EMAILS].Value;
            TextStatTo.Text:=Tables.DataSet.Fields[TAddressBook.ESTATEMENTS].Value;
        end;

        Tables.CleanUp;

        // Get data from company table
        Tables.CustFilter:=WHERE +
                                TCompany.CO_CODE +
                           EQUAL +
                                QuotedStr(CoCode) +
                           _AND +
                                TCompany.BRANCH +
                           EQUAL +
                                QuotedStr(Branch);

        Tables.OpenTable(TblCompany);

        if Tables.DataSet.RecordCount = 1 then
        begin
            TextLegalTo.Text:=Tables.DataSet.Fields[TCompany.LEGALTO].Value;
            EmailFromList.Items.Add(Tables.DataSet.Fields[TCompany.SEND_NOTE_FROM].Value);
            EmailFromList.ItemIndex:=0;
        end;

    finally
        Tables.Free;

        if Length(TextMailTo.Text)    > 5 then ErrorMailTo.Visible   :=False;
        if Length(TextStatTo.Text)    > 5 then ErrorStatTo.Visible   :=False;
        if Length(TextLegalTo.Text)   > 5 then ErrorLegalTo.Visible  :=False;
        if Length(EmailFromList.Text) > 5 then ErrorEmailFrom.Visible:=False;
        if not (ErrorLegalTo.Visible) and not (ErrorMailTo.Visible) and not (ErrorStatTo.Visible) then btnOK.Enabled:=True;

        Screen.Cursor:=crDefault;

    end;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TTrackerForm.FormCreate(Sender: TObject);
var
    Settings:  ISettings;
    lsColumns: TListColumn;
begin

    Settings:=TSettings.Create;
    TrackerForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_TRACKER', APPCAPTION);

    // List view initialization
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='LP';
    lsColumns.Width  :=40;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='CUID';
    lsColumns.Width  :=80;
    lsColumns:=CustomerList.Columns.Add;
    lsColumns.Caption:='Customer Name';
    lsColumns.Width  :=150;

    // Load all invoice tracker items
    pAgeGrid    :=MainForm.sgAgeView;
    pTrackerGrid:=MainForm.sgInvoiceTracker;
    UserAlias   :='*';
    Display;
end;

procedure TTrackerForm.FormShow(Sender: TObject);
begin
    LayoutList.Items.Clear;
    EmailFromList.Items.Clear;
    TextReminder1.Text    :='0';
    TextReminder2.Text    :='0';
    TextReminder3.Text    :='0';
    TextReminder4.Text    :='0';
    TextMailTo.Text       :='';
    TextStatTo.Text       :='';
    TextLegalTo.Text      :='';
    ErrorMailTo.Visible   :=True;
    ErrorStatTo.Visible   :=True;
    ErrorLegalTo.Visible  :=True;
    ErrorEmailFrom.Visible:=True;
    btnOK.Enabled         :=False;
end;

procedure TTrackerForm.FormActivate(Sender: TObject);
begin
    GetData;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


procedure TTrackerForm.btnOKClick(Sender: TObject);
var
    Tables: TDataTables;
begin

    UserAlias:=UpperCase(MainForm.WinUserName);
    Tables:=TDataTables.Create(MainForm.DbConnect);

    try
        Tables.Columns.Add(TReminderLayouts.ID);
        Tables.CustFilter:=WHERE + TReminderLayouts.LAYOUTNAME + EQUAL + QuotedStr(LayoutList.Text);
        Tables.OpenTable(TblReminderLayouts);

        if Tables.DataSet.RecordCount = 1 then
            Layout:=Tables.DataSet.Fields[TReminderLayouts.ID].Value;

    finally
        Tables.Free;
    end;

    Indv_Rem1:=TextReminder1.Text;
    Indv_Rem2:=TextReminder2.Text;
    Indv_Rem3:=TextReminder3.Text;
    Indv_Rem4:=TextReminder4.Text;

    if Exp_Rem2_Switch.Checked then
        Exp_Rem2:='1'
            else
                Exp_Rem2:='0';

    if Exp_Rem3_Switch.Checked then
        Exp_Rem3:='1'
            else
                Exp_Rem3:='0';

    // Disallow zero values
    if (Indv_Rem1 = '0') or (Indv_Rem2 = '0') or (Indv_Rem3 = '0') or (Indv_Rem4 = '0') then
    begin
        MainForm.MsgCall(mcWarn, 'Zero values are disallowed. Please correct and try again.');
        Exit;
    end;

    Add;
    Close;

end;

procedure TTrackerForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TTrackerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = ESC then Close;
end;


end.
