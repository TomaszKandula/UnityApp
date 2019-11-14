unit View.InvoiceTracker;

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
    Unity.Grid,
    Unity.Panel;


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
    strict private
        var FMultiselect:   TStringList;
        var FCtrlClicked:   boolean;
        var FTrackerGrid:   TStringGrid;
        var FAgeGrid:       TStringGrid;
        var FReminderMail : string;
        var FStatementMail: string;
        procedure GetSendFrom(List: TComboBox);
        procedure GetEmailAddress(Scuid: string);
        procedure SetEmailAddresses(List: TListView);
        procedure GetLayouts(LayoutContainer: TComboBox);
        procedure ApplyTimings(List: TListView);
        procedure SaveToDb(List: TListView);
    public
        property  TrackerGrid: TStringGrid read FTrackerGrid;
        property  AgeGrid:     TStringGrid read FAgeGrid;
    end;


    function TrackerForm(): TTrackerForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Handler.Sql{Legacy},
    Handler.Database{Legacy},
    DbModel{Legacy},
    Unity.Enums,
    Unity.Records,
    Unity.Helpers,
    Unity.Sql,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Async.AddressBook,
    Async.Utilities,
    Async.InvoiceTracker;


var vTrackerForm: TTrackerForm;


function TrackerForm: TTrackerForm;
begin
    if not(Assigned(vTrackerForm)) then Application.CreateForm(TTrackerForm, vTrackerForm);
    Result:=vTrackerForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


procedure TTrackerForm.GetSendFrom(List: TComboBox);
begin

    var Utilities: IUtilities:=TUtilities.Create();
    var EmailList:=TStringList.Create();
    try

        EmailList:=Utilities.GetCompanyEmailsAwaited(
            THelpers.ReturnCoCodesList(
                MainForm.sgAgeView,
                MainForm.sgAgeView.ReturnColumn(TSnapshots.fCoCode, 1, 1),
                True
            )
        );

    finally
        EmailList.Free();
    end;

    if List.Items.Count > 0 then
    begin
        ErrorEmailFrom.Visible:=False;
        List.ItemIndex:=0;
    end
    else
    begin
        ErrorEmailFrom.Visible:=True;
        List.Items.Clear();
    end;

end;


procedure TTrackerForm.GetEmailAddress(Scuid: string);
begin
    var AddressBook: IAddressBook:=TAddressBook.Create();
    var CustomerDetails: TCustomerDetails;
    CustomerDetails:=AddressBook.GetCustomerDetailsAwaited(Scuid);
    FReminderMail :=CustomerDetails.CustMailGen;
    FStatementMail:=CustomerDetails.CustMailStat;
end;


procedure TTrackerForm.SetEmailAddresses(List: TListView);
begin

    if List.Items.Count > 0 then
    begin

        for var iCNT: integer:=0 to List.Items.Count - 1 do
        begin

            GetEmailAddress(List.Items[iCNT].SubItems[1]);
            if not(string.IsNullOrEmpty(FStatementMail)) then List.Items[iCNT].SubItems[4]:=FStatementMail;
            if not(string.IsNullOrEmpty(FReminderMail)) then List.Items[iCNT].SubItems[5]:=FReminderMail;

        end;

    end;

end;


procedure TTrackerForm.GetLayouts(LayoutContainer: TComboBox);
begin

    // -------------------------------------------------------------
    // Layout file format: "reminder_<team>_<country>_<number>.htm".
    // Remove last six characters to display in list box.
    // Note: We use TStringList as a middleman to remove duplicates.
    // -------------------------------------------------------------

    var Settings: ISettings:=TSettings.Create();
    var LayoutLst: TStringList:=TStringList.Create();
    var LayoutFld: string:=Settings.DirLayouts;
    try

        // Get files from local folder
        try

            var SearchRec:  TSearchRec;
            {$WARN SYMBOL_PLATFORM OFF} { faArchives - Windows only }
            if FindFirst(LayoutFld + 'r*.htm', faArchive, SearchRec) = 0 then
            {$WARN SYMBOL_PLATFORM ON}
            begin

                var LayoutNam:  string;

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
            LayoutContainer.Clear();
            for var iCNT: integer:=0 to LayoutLst.Count - 1 do
                LayoutContainer.Items.Add(LayoutLst.Strings[iCNT]);

            if LayoutContainer.Items.Count > 0 then
                LayoutContainer.ItemIndex:=0;

        except
            on E: Exception do
                THelpers.MsgCall(Error, 'Cannot get list of layouts. Please contact IT support.');

        end;

    finally

      LayoutLst.Free();

    end;

end;


procedure TTrackerForm.ApplyTimings(List: TListView);
begin

    // First reminder and legal notice cannot be zero
    if (TextReminder1.Text = '0') or (TextReminder4.Text = '0') then
    begin
        THelpers.MsgCall(Warn, 'Please provide value for Reminder 1 and Legal Action different than zero.');
        Exit();
    end;

    if (Exp_Rem2_Switch.Checked) and (TextReminder2.Text = '0') then
    begin
        THelpers.MsgCall(Warn, 'Please provide value for Reminder 2 different than zero.');
        Exit();
    end;

    if (Exp_Rem3_Switch.Checked) and (TextReminder3.Text = '0') then
    begin
        THelpers.MsgCall(Warn, 'Please provide value for Reminder 3 different than zero.');
        Exit();
    end;

    // Applay only on selected customers
    if FMultiselect.Count > 0 then
    begin

        for var iCNT: integer:=0 to FMultiselect.Count - 1 do
        begin
            var SelItem: integer:=FMultiselect.Strings[iCNT].ToInteger();
            List.Items[SelItem].SubItems[3] :=ListEmailFrom.Text;
            List.Items[SelItem].SubItems[6] :=TextReminder0.Text;
            List.Items[SelItem].SubItems[7] :=TextReminder1.Text;
            List.Items[SelItem].SubItems[8] :=TextReminder2.Text;
            List.Items[SelItem].SubItems[9] :=TextReminder3.Text;
            List.Items[SelItem].SubItems[10]:=TextReminder4.Text;
        end;

    end
    else
    begin

        // Apply on all listed customers
        if THelpers.MsgCall(Question2, 'You have not selected any customers, do you want Unity to apply proposed conditions for all listed items?') = mrYes then
        begin

            for var iCNT: integer:=0 to List.Items.Count - 1 do
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
begin

    var Check: integer:=0;
    for var iCNT: integer:=0 to List.Items.Count - 1 do
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
        THelpers.MsgCall(Warn, 'Please make sure you that the list do not contain items marked as "not found" or "not set".');
        Exit();
    end;

    var CallResponse: TCallResponse;
    var PayLoad: TStringGrid:=TStringGrid.Create(nil);
    try

        // --------------------------------------------------------
        // Put ListView data to StringGrid to be saved to database.
        // --------------------------------------------------------

        PayLoad.RowCount:=List.Items.Count;
        PayLoad.ColCount:=16;

        for var iCNT: integer:=0 to List.Items.Count - 1 do
        begin
            PayLoad.Cells[0,  iCNT]:=SessionService.SessionUser;     // user alias
            PayLoad.Cells[1,  iCNT]:=List.Items[iCNT].SubItems[0];   // cuid
            PayLoad.Cells[2,  iCNT]:=List.Items[iCNT].SubItems[11];  // co code
            PayLoad.Cells[3,  iCNT]:=List.Items[iCNT].SubItems[12];  // branch/agent
            PayLoad.Cells[4,  iCNT]:=List.Items[iCNT].SubItems[2];   // cust name
            PayLoad.Cells[5,  iCNT]:=DateTimeToStr(Now);             // stamp
            PayLoad.Cells[6,  iCNT]:=List.Items[iCNT].SubItems[7];   // reminder 1
            PayLoad.Cells[7,  iCNT]:=List.Items[iCNT].SubItems[8];   // reminder 2
            PayLoad.Cells[8,  iCNT]:=List.Items[iCNT].SubItems[9];   // reminder 3
            PayLoad.Cells[9,  iCNT]:=List.Items[iCNT].SubItems[10];  // reminder 4
            PayLoad.Cells[10, iCNT]:=List.Items[iCNT].SubItems[1];   // scuid
            PayLoad.Cells[11, iCNT]:=ListLayout.Text;                // layout
            PayLoad.Cells[12, iCNT]:=List.Items[iCNT].SubItems[6];   // pre-statement
            PayLoad.Cells[13, iCNT]:=List.Items[iCNT].SubItems[3];   // send from
            PayLoad.Cells[14, iCNT]:=List.Items[iCNT].SubItems[4];   // statement to
            PayLoad.Cells[15, iCNT]:=List.Items[iCNT].SubItems[5];   // reminder to
        end;

        var InvoiceTracker: IInvoiceTracker:=TInvoiceTracker.Create();
        CallResponse:=InvoiceTracker.SaveTrackerDataAwaited(PayLoad);

    finally
        PayLoad.Free();
    end;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(TAppMessage.Info, 'The user list has been saved successfully.');
    ThreadFileLog.Log('[TTrackerForm.SaveToDb]: The user list has been saved successfully.');

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TTrackerForm.FormCreate(Sender: TObject);
begin

    var lsColumns: TListColumn;
    FMultiselect:=TStringList.Create();

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

    PanelCustomerList.PanelBorders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);

end;


procedure TTrackerForm.FormShow(Sender: TObject);
begin
    ListLayout.Clear();
    ListEmailFrom.Clear();
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


procedure TTrackerForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FMultiselect) then FMultiselect.Free();
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- CLICK EVENTS //


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
    Close();
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


procedure TTrackerForm.CustomerListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    FCtrlClicked:=True;
end;


procedure TTrackerForm.CustomerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    FCtrlClicked:=False;
end;


procedure TTrackerForm.btnSelectionClick(Sender: TObject);
begin
    CustomerList.ClearSelection();
end;


procedure TTrackerForm.CustomerListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin

    if not(FCtrlClicked) and (Selected) then
    begin
        FMultiselect.Clear();
        FMultiselect.Add(Item.Index.ToString);
    end
    else
    if (FCtrlClicked) and (Selected) then
        FMultiselect.Add(Item.Index.ToString)
            else
                FMultiselect.Clear();

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TTrackerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


end.

