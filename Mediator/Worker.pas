unit Worker;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    System.Diagnostics,
    System.Win.ComObj,
    System.SyncObjs,
    Vcl.Graphics,
    Vcl.ComCtrls,
    Vcl.Dialogs,
    Data.Win.ADODB,
    Data.DB,
    InterposerClasses,
    Helpers;


type

    {TODO -oTomek -cGeneral: replace TThread with ITask, remove TCriticalSections}

    IThreading = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']


    end;

    TThreading = class(TInterfacedObject, IThreading)
    {$TYPEINFO ON}


    end;


    // ----------------------------------------------------------------------------------------------------------------------------------------------------- //


    TTCheckServerConnection = class(TThread)
    protected
        procedure Execute; override;
    end;


    TTInvoiceTrackerRefresh = class(TThread)
    protected
        procedure Execute; override;
    private
        var pUserAlias: string;
        var FIDThd    : integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(UserAlias: string);
    end;


    TTMakeAgeView = class(TThread)
    protected
        procedure Execute; override;
        private
        var FLock      : TCriticalSection;
        var FIDThd     : integer;
        var FOpenAmount: double;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(OpenAmount: double);
        destructor  Destroy; override;
    end;


    TTReadAgeView = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:  TCriticalSection;
        var FIDThd: integer;
        var FMode:  TEnums.TLoading;
        var FSort:  integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(ActionMode: TEnums.TLoading; SortMode: integer);
        destructor  Destroy; override;
    end;


    TTOpenItemsScanner = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:  TCriticalSection;
        var FIDThd: integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create;
        destructor  Destroy; override;
    end;


    TTReadOpenItems = class(TThread)
    protected
        procedure Execute; override;
    private
        var FMode:  TEnums.TLoading;
        var FLock:  TCriticalSection;
        var FIDThd: integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(ActionMode: TEnums.TLoading);
        destructor  Destroy; override;
    end;


    TTAddressBook = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:       TCriticalSection;
        var FMode:       TEnums.TActions;
        var FGrid:       TStringGrid;
        var FIDThd:      integer;
        var FContact:    string;
        var FEstatement: string;
        var FEmail:      string;
        var FPhones:     string;
        var FSCUID:      string;
        var FConditions: string;
    public
        property    IDThd:  integer read FIDThd;
        function    Read     : boolean;
        function    Update   : boolean;
        function    Add      : boolean;
        destructor  Destroy; override;
        constructor Create(
            ActionMode:     TEnums.TActions;
            Grid:           TStringGrid;
            SCUID:          string;
            Contact:        string;
            Estatement:     string;
            Email:          string;
            Phones:         string;
            Conditions:     string
        );
    end;


    TTSendUserFeedback = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:   TCriticalSection;
        var FIDThd:  integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create;
        destructor  Destroy; override;
    end;


    TTExcelExport = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:   TCriticalSection;
        var FIDThd:  integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create;
        destructor  Destroy; override;
    end;


    TTDailyComment = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:          TCriticalSection;
        var FIDThd:         integer;
        var FCUID:          string;
        var FEmail:         boolean;
        var FCallEvent:     boolean;
        var FCallDuration:  string;
        var FFixedComment:  string;
        var FEmailReminder: boolean;
        var FEmailAutoStat: boolean;
        var FEmailManuStat: boolean;
        var FEventLog:      boolean;
        var FUpdateGrid:    boolean;
        var FExtendComment: boolean;
    public
        property    IDThd:  integer read FIDThd;
        destructor  Destroy; override;
        constructor Create(
            CUID:           string;
            Email:          boolean;
            CallEvent:      boolean;
            CallDuration:   integer;
            Comment:        string;
            EmailReminder:  boolean;
            EmailAutoStat:  boolean;
            EmailManuStat:  boolean;
            EventLog:       boolean;
            UpdateGrid:     boolean = true;
            ExtendComment:  boolean = false
        );
    end;


    TTGeneralComment = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:         TCriticalSection;
        var FIDThd:        integer;
        var FCUID:         string;
        var FFixedComment: string;
        var FFollowUp:     string;
        var FFree1:        string;
        var FFree2:        string;
        var FFree3:        string;
        var FEventLog:     boolean;
    public
        property    IDThd:  integer read FIDThd;
        destructor  Destroy; override;
        constructor Create(
            CUID:           string;
            FixedComment:   string;
            FollowUp:       string;
            Free1:          string;
            Free2:          string;
            Free3:          string;
            EventLog:       boolean
        );
    end;


    TTSendAccountStatement = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:        TCriticalSection;
        var FIDThd:       integer;
        var FLayout:      TEnums.TDocMode;
        var FSubject:     string;
        var FMess:        string;
        var FInvFilter:   TInvoiceFilter;
        var FBeginDate:   string;
        var FEndDate:     string;
        var FOpenItems:   TStringGrid;
        var FCUID:        string;
        var FSendFrom:    string;
        var FMailTo:      string;
        var FCustName:    string;
        var FCustNumber:  string;
        var FLBUName:     string;
        var FLBUAddress:  string;
        var FTelephone:   string;
        var FBankDetails: string;
        var FSeries:      boolean;
        var FItemNo:      integer;
    public
        property    IDThd:  integer read FIDThd;
        destructor  Destroy; override;
        constructor Create(
            Layout:      TEnums.TDocMode;
            Subject:     string;
            Mess:        string;
            InvFilter:   TInvoiceFilter;
            BeginDate:   string;
            EndDate:     string;
            OpenItems:   TStringGrid;
            CUID:        string;
            SendFrom:    string;
            MailTo:      string;
            CustName:    string;
            CustNumber:  string;
            LBUName:     string;
            LBUAddress:  string;
            Telephone:   string;
            BankDetails: string;
            Series:      boolean = False;
            ItemNo:      integer = 0
        );
    end;


    TTSendAccountStatements = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:       TCriticalSection;
        var FIDThd:      integer;
        var FSubject:    string;
        var FMess:       string;
        var FInvFilter:  TInvoiceFilter;
        var FBeginDate:  string;
        var FEndDate:    string;
        var FOpenItems:  TStringGrid;
        var FMailerList: TListView;
    public
        property    IDThd: integer read FIDThd;
        destructor  Destroy; override;
        constructor Create(
            Subject:    string;
            Mess:       string;
            InvFilter:  TInvoiceFilter;
            BeginDate:  string;
            EndDate:    string;
            OpenItems:  TStringGrid;
            MailerList: TListView
        );
    end;


    TTGeneralTables = class(TThread)
    protected
        procedure Execute; override;
    private
        var FTableName:   string;
        var FColumns:     string;
        var FDestGrid:    TStringGrid;
        var FConditions:  string;
        var FAutoRelease: boolean;
    public
        constructor Create(
            TableName:   string;
            DestGrid:    TStringGrid;
            Columns:     string = ''    {Option};
            Conditions:  string = ''    {Option};
            AutoRelease: boolean = True {Option}
        );
    end;


implementation


uses
    Main,
    SqlHandler,
    DbModel,
    DbHandler,
    Settings,
    UAC,
    Mailer,
    AgeView,
    Transactions,
    Tracker,
    Actions,
    SendFeedback;


// --------------------------------------------------------------------------------------------------------------------------------- CHECK SERVER CONNECTION //


procedure TTCheckServerConnection.Execute;
begin

    var DataBase: TDataBase:=TDataBase.Create(False);
    try

        var IDThd: integer:=TTCheckServerConnection.CurrentThread.ThreadID;
        if (not(MainForm.IsConnected)) and (DataBase.Check = 0) then
        begin
            Synchronize(procedure
            begin
                MainForm.TryInitConnection;
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Connection with SQL Server database has been re-established.');
            end);
        end;

        if DataBase.Check <> 0 then
        begin
            MainForm.IsConnected:=False;
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Connection with SQL Server database has been lost, waiting to reconnect...');
        end;

    finally
        DataBase.Free;
    end;

    FreeOnTerminate:=True;

end;


// --------------------------------------------------------------------------------------------------------------------------------- INVOICE TRACKER REFRESH //


constructor TTInvoiceTrackerRefresh.Create(UserAlias: string);
begin
    inherited Create(False);
    pUserAlias:=UserAlias;
end;


procedure TTInvoiceTrackerRefresh.Execute;
begin

    FIDThd:=CurrentThread.ThreadID;

    try
        MainForm.UpdateTrackerList(pUserAlias);
    except
        on E: Exception do
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Execution of this tread work has been stopped. Error has been thrown: ' + E.Message + ' (TInvoiceTracker).');
    end;

    // Release when finished
    FreeOnTerminate:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- MAKE AGE VIEW //


constructor TTMakeAgeView.Create(OpenAmount: double);
begin
    inherited Create(False);
    FLock      :=TCriticalSection.Create;
    FOpenAmount:=OpenAmount;
    FIDThd     :=0;
end;


destructor TTMakeAgeView.Destroy;
begin
    FLock.Free;
end;


procedure TTMakeAgeView.Execute;
var
    THDMili:   extended;
    THDSec:    extended;
    StopWatch: TStopWatch;
    AgeView:   TAgeView;
    UserCtrl:  TUserControl;
    CanReload: boolean;
begin

    CanReload:=False;
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    AgeView :=TAgeView.Create(MainForm.DbConnect);
    UserCtrl:=TUserControl.Create(MainForm.DbConnect);

    try
        StopWatch:=TStopWatch.StartNew;
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Generating);
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']:' + TStatusBar.Generating);

        try
            AgeView.idThd:=IDThd;

            // Async
            if MainForm.EditGroupID.Text = MainForm.GroupIdSel then AgeView.GroupID:=MainForm.GroupIdSel
                else
                    if MainForm.EditGroupID.Text <> '' then AgeView.GroupID:=MainForm.EditGroupID.Text
                        else
                            AgeView.GroupID:=MainForm.GroupIdSel;

            // Generate aging
            AgeView.Make(MainForm.OSAmount);

            // CSV or server?
            if MainForm.cbDump.Checked then
            begin
                if MainForm.CSVExport.Execute then
                AgeView.ExportToCSV(MainForm.CSVExport.FileName, AgeView.ArrAgeView);
            end
            else

            // Send to SQL Server, update age date list and reload age view on main tabsheet
            begin
                AgeView.Write(TSnapshots.Snapshots, AgeView.ArrAgeView);
                Synchronize(procedure
                begin
                    try
                        UserCtrl.GetAgeDates(MainForm.GroupListDates, MainForm.GroupList[0, 0]);
                        MainForm.AgeDateSel:=MainForm.GroupListDates.Text;
                    finally
                        UserCtrl.Free;
                    end;
                end);

                CanReload:=True;
            end;


        except
            on E: Exception do
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTMakeAgeView". Error has been thrown: ' + E.Message);
        end;

    finally
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);
        THDMili:=StopWatch.ElapsedMilliseconds;
        THDSec:=THDMili / 1000;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Age View thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
        AgeView.Free;
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;
    if CanReload then
        TTReadAgeView.Create(TEnums.TLoading.NullParameter, TSorting.TMode.Ranges);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- READ AGE VIEW //


constructor TTReadAgeView.Create(ActionMode: TEnums.TLoading; SortMode: integer);
begin
    inherited Create(False);
    FLock :=TCriticalSection.Create;
    FMode :=ActionMode;
    FSort :=SortMode;
    FIDThd:=0;
end;


destructor TTReadAgeView.Destroy;
begin
    FLock.Free;
end;


Procedure TTReadAgeView.Execute;
var
    THDMili:    extended;
    THDSec:     extended;
    StopWatch:  TStopWatch;
    AgeView:    TAgeView;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;
    AgeView:=TAgeView.Create(MainForm.DbConnect);
    try
        StopWatch:=TStopWatch.StartNew;
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading);
        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString);

        try
            // Sync
            Synchronize(AgeView.ClearSummary);

            // Async
            AgeView.idThd  :=IDThd;
            AgeView.GroupID:=MainForm.GroupIdSel;
            AgeView.AgeDate:=MainForm.AgeDateSel;
            AgeView.Read(MainForm.sgAgeView, FSort);

            // Sync
            Synchronize(procedure
            begin
                AgeView.ComputeAgeSummary(MainForm.sgAgeView);
                AgeView.ComputeAndShowRCA(MainForm.sgAgeView);
                AgeView.UpdateSummary;
                AgeView.GetDetails(MainForm.sgCompanyData);

                // Map data (source General Tables tabsheet)
                AgeView.MapGroup3(MainForm.sgAgeView, MainForm.sgGroup3);
                AgeView.MapTable1(MainForm.sgAgeView, MainForm.sgPersonResp);
                AgeView.MapTable2(MainForm.sgAgeView, MainForm.sgSalesResp);
                AgeView.MapTable3(MainForm.sgAgeView, MainForm.sgAccountType);
                AgeView.MapTable4(MainForm.sgAgeView, MainForm.sgCustomerGr);

                MainForm.sgAgeView.Repaint;
            end);

        except
            on E: Exception do
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTReadAgeView". Error has been thrown: ' + E.Message);
        end;

    finally
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);
        THDMili:=StopWatch.ElapsedMilliseconds;
        THDSec:=THDMili / 1000;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Thread for selected Group Id "' + AgeView.GroupID + '" has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
        AgeView.Free;
        FLock.Release;

        // Switch on all timers
        MainForm.SwitchTimers(TurnedOn);

        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);

    end;

    // Release when finished
    FreeOnTerminate:=True;

    // Call open items if user select another age view
    if FMode = CallOpenItems then
        TTReadOpenItems.Create(TEnums.TLoading.NullParameter);

end;


// -------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS SCANNER //


constructor TTOpenItemsScanner.Create;
begin
    inherited Create(False);
    FLock :=TCriticalSection.Create;
    FIDThd:=0;
end;


destructor TTOpenItemsScanner.Destroy;
begin
    FLock.Free;
end;


procedure TTOpenItemsScanner.Execute;
var
    Transactions: TTransactions;
    ReadDateTime: string;
    ReadStatus:   string;
    CanMakeAge:   boolean;
begin
    CanMakeAge:=False;
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;
    Transactions:=TTransactions.Create(MainForm.DbConnect);

    try
        try
            ReadDateTime:=Transactions.GetDateTime(DateTime);
            ReadStatus:=Transactions.GetStatus(ReadDateTime);
            if ( StrToDateTime(MainForm.OpenItemsUpdate) < StrToDateTime(ReadDateTime) ) and ( ReadStatus = 'Completed' ) then
            begin
                // Switch off all of the timers
                MainForm.SwitchTimers(TurnedOff);

                // Refresh open items and make new aging view
                MainForm.OpenItemsUpdate:=ReadDateTime;
                MainForm.OpenItemsStatus:='';
                CanMakeAge:=True;
            end;
        except
            on E: Exception do
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTOpenItemsScanner". Error has been thrown: ' + E.Message);
        end;
    finally
        Transactions.Free;
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;
    if CanMakeAge then
        TTReadOpenItems.Create(TEnums.TLoading.CallMakeAge);

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- READ OPEN ITEMS //


constructor TTReadOpenItems.Create(ActionMode: TEnums.TLoading);
begin
    inherited Create(False);
    FLock :=TCriticalSection.Create;
    FMode :=ActionMode;
    FIDThd:=0;
end;


destructor TTReadOpenItems.Destroy;
begin
    FLock.Free;
end;


procedure TTReadOpenItems.Execute;
var
    THDMili:    extended;
    THDSec:     extended;
    StopWatch:  TStopWatch;
    OpenItems:  TTransactions;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;
    OpenItems:=TTransactions.Create(MainForm.DbConnect);

    try
        StopWatch:=TStopWatch.StartNew;
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Downloading);

        try
            OpenItems.DestGrid   :=MainForm.sgOpenItems;
            OpenItems.SettingGrid:=MainForm.sgCompanyData;
            OpenItems.DestGrid.Freeze(True);

            // Sync with GUI
            Synchronize(OpenItems.ClearSummary);

            // Async
            OpenItems.LoadToGrid;
            OpenItems.UpdateSummary;
        except
            on E: Exception do
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(FIDThd) + ']: Cannot execute "TTReadOpenItems". Error has been thorwn: ' + E.Message);
        end;

    finally
        THDMili:=StopWatch.ElapsedMilliseconds;
        THDSec:=THDMili / 1000;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(FIDThd) + ']: Open Items loading thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);

        // Release VCL and set auto column width
        Synchronize(procedure
        begin
            OpenItems.DestGrid.SetColWidth(10, 20, 400);
        end);

        OpenItems.DestGrid.Freeze(False);
        OpenItems.Free;
        FLock.Release;

    end;

    // Release when finished
    FreeOnTerminate:=True;

    // Make age view from open items and send to SQL Server
    if FMode = CallMakeAge then
    begin
        MainForm.cbDump.Checked:=False;
        TTMakeAgeView.Create(MainForm.OSAmount);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------- ADRESS BOOK //


constructor TTAddressBook.Create(ActionMode: TEnums.TActions; Grid: TStringGrid; SCUID, Contact, Estatement, Email, Phones: string; Conditions: string);
begin
    inherited Create(False);
    FLock      :=TCriticalSection.Create;
    FMode      :=ActionMode;
    FGrid      :=Grid;
    FContact   :=Contact;
    FEstatement:=Estatement;
    FEmail     :=Email;
    FPhones    :=Phones;
    FSCUID     :=SCUID;
    FIDThd     :=0;
    FConditions:=Conditions;
end;


destructor TTAddressBook.Destroy;
begin
    FLock.Free;
end;


procedure TTAddressBook.Execute;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    try
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Processing);

        // Open
        if (FMode = TEnums.TActions.OpenAll) or (FMode = TEnums.TActions.OpenForUser) then
        begin
            if Read then
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IDThd.ToString + ']: Address Book has been opened successfully.')
                    else
                        begin
                            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot open Address Book.');
                            MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Read function of Address Book has failed. Please contact IT support.');
                        end;
        end;

        // Update
        if FMode = TEnums.TActions.Update then
        begin
            if Update then
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IDThd.ToString + ']: The Address Book has been updated successfully.')
                    else
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IDThd.ToString + ']: Cannot insert data to Address Book, either error occured or item already exist.');
        end;

        // Insert new recodrs
        if FMode = TEnums.TActions.Insert then
        begin
            if Add then
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IDThd.ToString + ']: The Address Book insertion has been executed successfully.')
                    else
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IDThd.ToString + ']: Cannot insert data to Address Book, either error occured or item already exist.');
        end;

        // Export
        if FMode = TEnums.TActions.Export then
        begin
            FGrid.OpenThdId:=IDThd;
            FGrid.ExportCSV(MainForm.CSVExport, '|');
        end;

    finally
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;

end;


function TTAddressBook.Read: boolean;
var
    DataTables: TDataTables;
begin
    Result:=True;
    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString);
    DataTables:=TDataTables.Create(MainForm.DbConnect);
    try

        try
            // Freeze StringGrid
            FGrid.Freeze(True);

            // Column selection
            DataTables.Columns.Add(TAddressBook.UserAlias);
            DataTables.Columns.Add(TAddressBook.Scuid);
            DataTables.Columns.Add(TAddressBook.CustomerNumber);
            DataTables.Columns.Add(TAddressBook.CustomerName);
            DataTables.Columns.Add(TAddressBook.Emails);
            DataTables.Columns.Add(TAddressBook.Estatements);
            DataTables.Columns.Add(TAddressBook.PhoneNumbers);
            DataTables.Columns.Add(TAddressBook.Contact);
            DataTables.Columns.Add(TAddressBook.CoCode);
            DataTables.Columns.Add(TAddressBook.Agent);
            DataTables.Columns.Add(TAddressBook.Division);

            // Filter by User Alias (if given)
            if FMode = TEnums.TActions.OpenForUser then
                DataTables.CustFilter:=FConditions;

            DataTables.OpenTable(TAddressBook.AddressBook);
            if not(DataTables.SqlToGrid(FGrid, DataTables.DataSet, True, True)) then
                MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'No results found in the database.');

        except
            on E: Exception do
            begin
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ' + E.Message);
                Result:=False;
            end;
        end;

    finally
        DataTables.Free;

        // Set auto column width
        Synchronize(procedure
        begin
            FGrid.SetColWidth(40, 10, 400);
        end);

        // Release StringGrid
        FGrid.Freeze(False);
    end;

    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);

end;


function TTAddressBook.Update: boolean;
var
    Book:       TDataTables;
    iCNT:       integer;
    Condition:  string;
begin
    Result:=False;
    Book:=TDataTables.Create(MainForm.DbConnect);

    try
        // Update from Address Book String Grid
        if FGrid <> nil then
        begin
            if FGrid.UpdatedRowsHolder <> nil then
            begin
                for iCNT:=low(FGrid.UpdatedRowsHolder) to high(FGrid.UpdatedRowsHolder) do
                begin
                    Condition:=TAddressBook.Scuid + TSql.EQUAL + FGrid.Cells[FGrid.ReturnColumn(TAddressBook.Scuid, 1, 1), FGrid.UpdatedRowsHolder[iCNT]];
                    // Columns
                    Book.Columns.Add(TAddressBook.Emails);
                    Book.Columns.Add(TAddressBook.PhoneNumbers);
                    Book.Columns.Add(TAddressBook.Contact);
                    Book.Columns.Add(TAddressBook.Estatements);
                    // Values
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.Emails,       1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.PhoneNumbers, 1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.Contact,      1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.Estatements,  1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                end;

                Result:=Book.UpdateRecord(TAddressBook.AddressBook, True, Condition);

                // Success
                if Result then
                begin
                    FGrid.SetUpdatedRow(0);
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Address Book has been updated succesfully!');
                end
                else
                // Error during post
                begin
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update Address Book. Please contact IT support.');
                end;
            end
            else

            // No changes within Address Book string grid
            begin
                MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Nothing to update. Please make changes first and try again.');
            end;
        end;

        // Update from Action Log View
        if FGrid = nil then
        begin
            Condition:=TAddressBook.Scuid + TSql.EQUAL + QuotedStr(FSCUID);
            // Columns
            Book.Columns.Add(TAddressBook.PhoneNumbers);
            Book.Columns.Add(TAddressBook.Contact);
            Book.Columns.Add(TAddressBook.Estatements);
            Book.Columns.Add(TAddressBook.Emails);
            // Values
            Book.Values.Add(FPhones);
            Book.Values.Add(FContact);
            Book.Values.Add(FEstatement);
            Book.Values.Add(FEmail);

            Result:=Book.UpdateRecord(TAddressBook.AddressBook, True, Condition);

            // Ending
            if Result then
                MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Address Book has been updated succesfully!')
                    else
                        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update Address Book. Please contact IT support.');
        end;
    finally
        Book.Free;
    end;

end;


function TTAddressBook.Add: boolean;
var
    iCNT:       integer;
    jCNT:       integer;
    SCUID:      string;
    AddrBook:   TALists;
    Book:       TDataTables;
    Check:      cardinal;
begin
    Result:=False;
    SetLength(AddrBook, 1, 11);
    jCNT:=0;
    Check:=0;

    // Get data from String Grid
    Book:=TDataTables.Create(MainForm.DbConnect);
    try
        for iCNT:=FGrid.Selection.Top to FGrid.Selection.Bottom do
        begin
            if FGrid.RowHeights[iCNT] <> FGrid.sgRowHidden then
            begin
                // Build SCUID
                SCUID:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), iCNT] +
                    MainForm.ConvertCoCode(
                        FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT],
                        'F',
                        3
                    );
                Book.CleanUp;
                Book.Columns.Add(TAddressBook.Scuid);
                Book.CustFilter:=TSql.WHERE + TAddressBook.Scuid + TSql.EQUAL + QuotedStr(SCUID);
                Book.OpenTable(TAddressBook.AddressBook);

                // Add to array if not exists
                if Book.DataSet.RecordCount = 0 then
                begin
                    Inc(Check);
                    AddrBook[jCNT,  0]:=UpperCase(MainForm.WinUserName);
                    AddrBook[jCNT,  1]:=SCUID;
                    AddrBook[jCNT,  2]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCustomerNumber,1, 1), iCNT];
                    AddrBook[jCNT,  3]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCustomerName,  1, 1), iCNT];
                    AddrBook[jCNT,  8]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fAgent,         1, 1), iCNT];
                    AddrBook[jCNT,  9]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fDivision,      1, 1), iCNT];
                    AddrBook[jCNT, 10]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCoCode,        1, 1), iCNT];
                    Inc(jCNT);
                    SetLength(AddrBook, jCNT + 1, 11);
                end;
            end;
        end;

    finally
        Book.Free;
    end;

    // Send to database
    if Check > 0 then
    begin
        Book:=TDataTables.Create(MainForm.DbConnect);
        try
            Book.Columns.Add(TAddressBook.UserAlias);
            Book.Columns.Add(TAddressBook.Scuid);
            Book.Columns.Add(TAddressBook.CustomerNumber);
            Book.Columns.Add(TAddressBook.CustomerName);
            Book.Columns.Add(TAddressBook.Emails);
            Book.Columns.Add(TAddressBook.PhoneNumbers);
            Book.Columns.Add(TAddressBook.Contact);
            Book.Columns.Add(TAddressBook.Estatements);
            Book.Columns.Add(TAddressBook.Agent);
            Book.Columns.Add(TAddressBook.Division);
            Book.Columns.Add(TAddressBook.CoCode);
            try

                Book.InsertInto(TAddressBook.AddressBook, True, nil, AddrBook);

                if Book.RowsAffected > 0 then
                begin
                    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Address Book has been successfully populated by selected item(s).');
                    Result:=True;
                end
                else
                begin
                    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Cannot update Address Book. Please contact IT support.');
                end;
            except
                on E: Exception do
                begin
                    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot save selected item(s). Exception has been thrown: ' + E.Message);
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Cannot write Address Book item(s) into database. Error: ' + E.Message);
                end;
            end;
        finally
            Book.Free;
            AddrBook:=nil;
        end;
    end
    else
    begin
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Selected customers are already in Address Book.');
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- USER FEEDBACK //


constructor TTSendUserFeedback.Create;
begin
    inherited Create(False);
    FLock :=TCriticalSection.Create;
    FIDThd:=0;
end;


destructor TTSendUserFeedback.Destroy;
begin
    FLock.Free;
end;


procedure TTSendUserFeedback.Execute;
begin
    FLock.Acquire;
    FIDThd:=GetCurrentThreadId;

    try
        if ReportForm.SendReport then
        begin
            MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Report has been sent successfully!');
            Synchronize(ReportForm.ReportMemo.Clear);
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Bug Report has been successfully sent by the user.');
        end
        else
        begin
            MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot send Bug Report. Please contact IT support.');
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot send Bug Report.');
        end;
    finally
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- EXPORT TO EXCEL //


constructor TTExcelExport.Create;
begin
    inherited Create(False);
    FLock :=TCriticalSection.Create;
    FIDThd:=0;
end;


destructor TTExcelExport.Destroy;
begin
    FLock.Free;
end;


procedure TTExcelExport.Execute;
var
    FileName:  string;
    Temp:      TStringGrid;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    try
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.ExportXLS);

        // Open save dialog box (sync with GUI)
        Synchronize(procedure
        begin
            if MainForm.XLExport.Execute then
                FileName:=MainForm.XLExport.FileName
                    else
                        FileName:='';
        end);

        // Generate and save
        Temp:=TStringGrid.Create(nil);

        try
            Temp.OpenThdId:=IDThd;
            Temp.ToExcel('Age Report', FileName);
        finally
            Temp.Free;
        end;

    finally
        FLock.Release;
        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);
    end;

    FreeOnTerminate:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------------- WRITE DAILY COMMENT //


constructor TTDailyComment.Create
(
    CUID:          string;
    Email:         boolean;
    CallEvent:     boolean;
    CallDuration:  integer;
    Comment:       string;
    EmailReminder,
    EmailAutoStat,
    EmailManuStat: boolean;
    EventLog:      boolean;
    UpdateGrid:    boolean = true;
    ExtendComment: boolean = false
);
begin
    inherited Create(False);
    FLock         :=TCriticalSection.Create;
    FIDThd        :=0;
    FCUID         :=CUID;
    FEmail        :=Email;
    FCallEvent    :=CallEvent;
    FCallDuration :=IntToStr(CallDuration);
    FFixedComment :=Comment;
    FEmailReminder:=EmailReminder;
    FEmailAutoStat:=EmailAutoStat;
    FEmailManuStat:=EmailManuStat;
    FEventLog     :=EventLog;
    FUpdateGrid   :=UpdateGrid;
    FExtendComment:=ExtendComment;
end;


destructor TTDailyComment.Destroy;
begin
    FLock.Free;
end;


procedure TTDailyComment.Execute;
var
    DailyText:     TDataTables;
    Condition:     string;
    Email:         string;
    CallEvent:     integer;
    EmailReminder: integer;
    EmailAutoStat: integer;
    EmailManuStat: integer;
    DataCheckSum:  string;
    CurrComment:   string;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    try
        DailyText:=TDataTables.Create(MainForm.DbConnect);
        try
            Condition:=TDailyComment.Cuid + TSql.EQUAL + QuotedStr(FCUID) + TSql._AND + TDailyComment.AgeDate + TSql.EQUAL + QuotedStr(MainForm.AgeDateSel);
            DataCheckSum:=FCUID + StringReplace(MainForm.AgeDateSel, '-', '', [rfReplaceAll]);
            DailyText.CustFilter:=TSql.WHERE + Condition;
            DailyText.OpenTable(TDailyComment.DailyComment);

            // Update exisiting comment
            if not (DailyText.DataSet.RecordCount = 0) then
            begin

                // Allow to extend comment by adding to existing wording a new comment line
                if FExtendComment then
                    CurrComment:=DailyText.DataSet.Fields[TDailyComment.FixedComment].Value + TChars.CRLF;

                DailyText.CleanUp;

                // Define columns, values and conditions
                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));
                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(MainForm.WinUserName));

                if FEmail then
                begin
                    Email:=IntToStr(StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.Email].Value), 0));
                    DailyText.Columns.Add(TDailyComment.Email);
                    DailyText.Values.Add(Email);
                end;

                // Call event and call duration always comes together
                if FCallEvent then
                begin
                    CallEvent:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.CallEvent].Value), 0);
                    Inc(CallEvent);
                    DailyText.Columns.Add(TDailyComment.CallEvent);
                    DailyText.Values.Add(IntToStr(CallEvent));
                    DailyText.Columns.Add(TDailyComment.CallDuration);
                    DailyText.Values.Add(FCallDuration);
                end;

                if FEmailReminder then
                begin
                    EmailReminder:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailReminder].Value), 0);
                    Inc(EmailReminder);
                    DailyText.Columns.Add(TDailyComment.EmailReminder);
                    DailyText.Values.Add(IntToStr(EmailReminder));
                end;

                if FEmailAutoStat then
                begin
                    EmailAutoStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailAutoStat].Value), 0);
                    Inc(EmailAutoStat);
                    DailyText.Columns.Add(TDailyComment.EmailAutoStat);
                    DailyText.Values.Add(IntToStr(EmailAutoStat));
                end;

                if FEmailManuStat then
                begin
                    EmailManuStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailManuStat].Value), 0);
                    Inc(EmailManuStat);
                    DailyText.Columns.Add(TDailyComment.EmailManuStat);
                    DailyText.Values.Add(IntToStr(EmailManuStat));
                end;

                if not(FFixedComment = '') then
                begin
                    DailyText.Columns.Add(TDailyComment.FixedComment);
                    DailyText.Values.Add(CurrComment + FFixedComment);
                end;

                // Execute
                if (DailyText.UpdateRecord(TDailyComment.DailyComment, True, Condition)) and (DailyText.RowsAffected > 0) then
                begin

                    // Refresh history grid
                    Synchronize(procedure
                    begin
                        if FUpdateGrid then ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                    end);

                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''DailyComment'' table has been updated (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update daily comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '. Error message received: ' + DailyText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
                end;
            end
            else

            // Insert new record
            begin
                DailyText.CleanUp;

                // Define columns and values
                DailyText.Columns.Add(TDailyComment.GroupId);      DailyText.Values.Add(MainForm.GroupIdSel);
                DailyText.Columns.Add(TDailyComment.Cuid);         DailyText.Values.Add(FCUID);
                DailyText.Columns.Add(TDailyComment.AgeDate);      DailyText.Values.Add(MainForm.AgeDateSel);
                DailyText.Columns.Add(TDailyComment.Stamp);        DailyText.Values.Add(DateTimeToStr(Now));
                DailyText.Columns.Add(TDailyComment.UserAlias);    DailyText.Values.Add(UpperCase(MainForm.WinUserName));
                DailyText.Columns.Add(TDailyComment.DataCheckSum); DailyText.Values.Add(DataCheckSum);

                if FEmail then
                begin
                    DailyText.Columns.Add(TDailyComment.Email);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDailyComment.Email);
                    DailyText.Values.Add('0');
                end;

                if FEmailReminder then
                begin
                    DailyText.Columns.Add(TDailyComment.EmailReminder);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDailyComment.EmailReminder);
                    DailyText.Values.Add('0');
                end;

                if FEmailAutoStat then
                begin
                    DailyText.Columns.Add(TDailyComment.EmailAutoStat);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDailyComment.EmailAutoStat);
                    DailyText.Values.Add('0');
                end;

                if FEmailManuStat then
                begin
                    DailyText.Columns.Add(TDailyComment.EmailManuStat);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDailyComment.EmailManuStat);
                    DailyText.Values.Add('0');
                end;

                if FCallEvent then
                begin
                    DailyText.Columns.Add(TDailyComment.CallEvent);
                    DailyText.Values.Add('1');
                    DailyText.Columns.Add(TDailyComment.CallDuration);
                    DailyText.Values.Add(FCallDuration);
                end
                else
                begin
                    DailyText.Columns.Add(TDailyComment.CallEvent);
                    DailyText.Values.Add('0');
                    DailyText.Columns.Add(TDailyComment.CallDuration);
                    DailyText.Values.Add('0');
                end;

                DailyText.Columns.Add(TDailyComment.FixedComment);
                DailyText.Values.Add(FFixedComment);

                // Execute
                if (DailyText.InsertInto(TDailyComment.DailyComment, True)) and (DailyText.RowsAffected > 0) then
                begin

                    // Refresh history grid
                    Synchronize(procedure
                    begin
                        if FUpdateGrid then ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                    end);

                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''DailyComment'' table has been posted (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update daily comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '. Error message received: ' + DailyText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot post daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
                end;
            end;

        finally
            DailyText.Free;
        end;

    finally
        FLock.Release;
    end;

    FreeOnTerminate:=True;

end;


// ----------------------------------------------------------------------------------------------------------------------------------- WRITE GENERAL COMMENT //


constructor TTGeneralComment.Create(CUID: string; FixedComment: string; FollowUp: string; Free1: string; Free2: string; Free3: string; EventLog: boolean);
begin
    inherited Create(False);
    FLock        :=TCriticalSection.Create;
    FIDThd       :=0;
    FCUID        :=CUID;
    FFixedComment:=FixedComment;
    FFollowUp    :=FollowUp;
    FFree1       :=Free1;
    FFree2       :=Free2;
    FFree3       :=Free3;
    FEventLog    :=EventLog;
end;


destructor TTGeneralComment.Destroy;
begin
    FLock.Free;
end;


procedure TTGeneralComment.Execute;
var
    GenText:   TDataTables;
    Condition: string;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    try
        GenText:=TDataTables.Create(MainForm.DbConnect);
        try
            Condition:=TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(FCUID);
            GenText.CustFilter:=TSql.WHERE + Condition;
            GenText.OpenTable(TGeneralComment.GeneralComment);

            // Update
            if not (GenText.DataSet.RecordCount = 0) then
            begin
                GenText.CleanUp;

                // Define columns, vaues and conditions
                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));
                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(MainForm.WinUserName));

                if not(FFixedComment = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.FixedComment);
                    GenText.Values.Add(FFixedComment);
                end;

                if not(FFollowUp = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.FollowUp);
                    GenText.Values.Add(FFollowUp);
                end;

                if not(FFree1 = TUnknown.Null) then
                begin
                    GenText.Columns.Add(TGeneralComment.Free1);
                    GenText.Values.Add(FFree1);
                end;

                if not(FFree2 = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.Free2);
                    GenText.Values.Add(FFree2);
                end;

                if not(FFree3 = TUnknown.Null) then
                begin
                    GenText.Columns.Add(TGeneralComment.Free3);
                    GenText.Values.Add(FFree3);
                end;

                // Execute
                if (GenText.UpdateRecord(TGeneralComment.GeneralComment, True, Condition)) and (GenText.RowsAffected > 0) then
                begin
                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''GeneralComment'' table has been updated (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update general comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '. Error message received: ' + GenText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update general comment into database.' +  TChars.CRLF + 'Error message received: ' + GenText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
                end;
            end
            else

            // Insert new
            begin
                GenText.CleanUp;
                // Define columns and values
                GenText.Columns.Add(TGeneralComment.Cuid);      GenText.Values.Add(FCUID);
                GenText.Columns.Add(TGeneralComment.Stamp);     GenText.Values.Add(DateTimeToStr(Now));
                GenText.Columns.Add(TGeneralComment.UserAlias); GenText.Values.Add(UpperCase(MainForm.WinUserName));

                if not(FFixedComment = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.FixedComment);
                    GenText.Values.Add(FFixedComment);
                end
                else
                begin
                    GenText.Columns.Add(TGeneralComment.FixedComment);
                    GenText.Values.Add('');
                end;

                if not(FFollowUp = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.FollowUp);
                    GenText.Values.Add(FFollowUp);
                end
                else
                begin
                    GenText.Columns.Add(TGeneralComment.FollowUp);
                    GenText.Values.Add('');
                end;

                if not(FFree1 = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.Free1);
                    GenText.Values.Add(FFree1);
                end
                else
                begin
                    GenText.Columns.Add(TGeneralComment.Free1);
                    GenText.Values.Add('');
                end;

                if not(FFree2 = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.Free2);
                    GenText.Values.Add(FFree2);
                end
                else
                begin
                    GenText.Columns.Add(TGeneralComment.Free2);
                    GenText.Values.Add('');
                end;

                if not(FFree3 = TUnknown.NULL) then
                begin
                    GenText.Columns.Add(TGeneralComment.Free3);
                    GenText.Values.Add(FFree3);
                end
                else
                begin
                    GenText.Columns.Add(TGeneralComment.Free3);
                    GenText.Values.Add('');
                end;

                // Execute
                if (GenText.InsertInto(TGeneralComment.GeneralComment, True)) and (GenText.RowsAffected > 0) then
                begin
                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''GeneralComment'' table has been posted (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update general comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '. Error message received: ' + GenText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update general comment into database.' +  TChars.CRLF + 'Error message received: ' + GenText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
                end;
            end;

        finally
            GenText.Free;
        end;

    finally
        FLock.Release;
    end;

    FreeOnTerminate:=True;

end;


// ---------------------------------------------------------------------------------------------------------------------------------- SEND ACCOUNT STATEMENT //


constructor TTSendAccountStatement.Create
(
    Layout:      TEnums.TDocMode;
    Subject:     string;
    Mess:        string;
    InvFilter:   TInvoiceFilter;
    BeginDate:   string;
    EndDate:     string;
    OpenItems:   TStringGrid;
    CUID:        string;
    SendFrom:    string;
    MailTo:      string;
    CustName:    string;
    CustNumber:  string;
    LBUName:     string;
    LBUAddress:  string;
    Telephone:   string;
    BankDetails: string;
    Series:      boolean = False;
    ItemNo:      integer = 0
);
begin
    inherited Create(False);
    FLock       :=TCriticalSection.Create;
    FIDThd      :=0;
    FLayout     :=Layout;
    FSubject    :=Subject;
    FMess       :=Mess;
    FInvFilter  :=InvFilter;
    FBeginDate  :=BeginDate;
    FEndDate    :=EndDate;
    FOpenItems  :=OpenItems;
    FCUID       :=CUID;
    FSendFrom   :=SendFrom;
    FMailTo     :=MailTo;
    FCustName   :=CustName;
    FCustNumber :=CustNumber;
    FSeries     :=Series;
    FItemNo     :=ItemNo;
    FLBUName    :=LBUName;
    FLBUAddress :=LBUAddress;
    FTelephone  :=Telephone;
    FBankDetails:=BankDetails;
end;


destructor TTSendAccountStatement.Destroy;
begin
    FLock.Free;
end;


procedure TTSendAccountStatement.Execute;
var
    Statement:  TDocument;
    Settings:   ISettings;
    CommThread: TTDailyComment;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;
    CurrentThread.Sleep(15);

    try
        Statement:=TDocument.Create;
        Settings :=TSettings.Create;
        try

            /// <remarks>
            /// Assign all the necessary parameters.
            /// </remarks>

            Statement.CUID       :=FCUID;
            Statement.MailFrom   :=FSendFrom;
            Statement.MailTo     :=FMailTo;
            Statement.CustName   :=FCustName;
            Statement.LBUName    :=FLBUName;
            Statement.LBUAddress :=FLBUAddress;
            Statement.Telephone  :=FTelephone;
            Statement.BankDetails:=FBankDetails;
            Statement.CustMess   :=FMess;
            Statement.OpenItems  :=FOpenItems;
            Statement.InvFilter  :=FInvFilter;
            Statement.BeginWith  :=FBeginDate;
            Statement.EndWith    :=FEndDate;

            // quick fix - to be refactored - data should be taken from the table
            Statement.REM_EX1:='1';
            Statement.REM_EX2:='102';
            Statement.REM_EX3:='103';
            Statement.REM_EX4:='104';
            Statement.REM_EX5:='514';

            Statement.MailSubject:=FSubject + ' - ' + FCustName + ' - ' + FCustNumber;

            /// <remarks>
            /// Load either fixed template or customizable template.
            /// </remarks>
            /// <param name="FLayout">
            /// Use maDefined for fully pre-defined template.
            /// Use maCustom for customised template. It requires FSalut, FMess and FSubject to be provided.
            /// </param>

            if FLayout = TEnums.TDocMode.Defined then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''));

            if FLayout = TEnums.TDocMode.Custom then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''));

            /// <remarks>
            /// Send email with account statement.
            /// </remarks>

            if Statement.SendDocument then
            begin

                /// <summary>
                /// Register sent email either as manual statement or automatic statement.
                /// </summary>

                if FLayout = TEnums.TDocMode.Defined then
                begin
                    CommThread:=TTDailyComment.Create(
                        FCUID,
                        False,
                        False,
                        0,
                        'New communication has been sent to the customer',
                        False,
                        True,
                        False,
                        True,
                        False,
                        True
                    );
                    CommThread.WaitFor;
                end;

                if FLayout = TEnums.TDocMode.Custom  then
                begin
                    CommThread:=TTDailyComment.Create(
                        FCUID,
                        False,
                        False,
                        0,
                        'New communication has been sent to the customer',
                        False,
                        False,
                        True,
                        True,
                        False,
                        True
                    );
                    CommThread.WaitFor;
                end;

                /// <remarks>
                /// Either single email (manual by user) or executed by mass mailer (multiple emails).
                /// </remarks>

                if not(FSeries) then
                begin
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Account Statement has been sent successfully!');
                end
                else
                begin
                    MainForm.ExecMessage(False, TMessaging.TWParams.MailerReportItem, FItemNo.ToString);
                end;

            end
            else
            begin
                if not(FSeries) then MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Account Statement cannot be sent. Please contact IT support.')
            end;

        finally
            Statement.Free;
        end;

    finally
        FLock.Release;
    end;

    FreeOnTerminate:=True;

end;


// --------------------------------------------------------------------------------------------------------------------------------- SEND ACCOUNT STATEMENTS //


constructor TTSendAccountStatements.Create
(
    Subject:    string;
    Mess:       string;
    InvFilter:  TInvoiceFilter;
    BeginDate:  string;
    EndDate:    string;
    OpenItems:  TStringGrid;
    MailerList: TListView
);
begin
    inherited Create(False);
    FLock      :=TCriticalSection.Create;
    FIDThd     :=0;
    FSubject   :=Subject;
    FMess      :=Mess;
    FInvFilter :=InvFilter;
    FBeginDate :=BeginDate;
    FEndDate   :=EndDate;
    FOpenItems :=OpenItems;
    FMailerList:=MailerList;
end;


destructor TTSendAccountStatements.Destroy;
begin
    FLock.Free;
end;


procedure TTSendAccountStatements.Execute;
var
    Statement:  TDocument;
    iCNT:       integer;
    SendStat:   TTSendAccountStatement;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    try

        /// <remarks>
        /// Lock VCL during processing by worker thread.
        /// </remarks>

        FOpenItems.Freeze(True);
        FMailerList.Freeze(True);

        Statement:=TDocument.Create;
        try

            /// <remarks>
            /// Update column references, as they depend on view from SQL which may be changed at runtime.
            /// </remarks>

            MainForm.UpdateOpenItemsRefs(MainForm.sgOpenItems);
            MainForm.UpdateControlStatusRefs(MainForm.sgControlStatus);

            for iCNT:=0 to FMailerList.Items.Count - 1 do
            begin

                if FMailerList.Items[iCNT].SubItems[4] <> 'Not found!' then
                begin

                    /// <remarks>
                    /// Execute worker thread and wait until it finishes the work. This may is due to limitness of multithreading capability,
                    /// user may want to send hundreds of emails and we do not want to create hundreds of threads. One worker thread is
                    /// enough to perform email sending wihtout blocking main thread (GUI).
                    /// </remarks>

                    SendStat:=TTSendAccountStatement.Create(
                        TEnums.TDocMode.Custom,
                        FSubject,
                        FMess,
                        FInvFilter,
                        FBeginDate,
                        FEndDate,
                        FOpenItems,
                        FMailerList.Items[iCNT].SubItems[10], // cuid
                        FMailerList.Items[iCNT].SubItems[3],  // send from
                        FMailerList.Items[iCNT].SubItems[4],  // send to
                        FMailerList.Items[iCNT].SubItems[0],  // cust name
                        FMailerList.Items[iCNT].SubItems[1],  // cust number
                        FMailerList.Items[iCNT].SubItems[5],  // lbu name
                        FMailerList.Items[iCNT].SubItems[6],  // lbu address
                        FMailerList.Items[iCNT].SubItems[7],  // lbu phone
                        FMailerList.Items[iCNT].SubItems[12], // bank html
                        True,
                        iCNT
                    );
                    SendStat.WaitFor;

                end;

            end;

        finally
            Statement.Free;
            MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
        end;

    finally
        FOpenItems.Freeze(False);
        FMailerList.Freeze(False);
        FLock.Release;
    end;

    FreeOnTerminate:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------- LOAD ASYNC. GENERAL TABLE //


constructor TTGeneralTables.Create(TableName: string; DestGrid: TStringGrid; Columns: string = '' {OPTION}; Conditions: string = '' {OPTION}; AutoRelease: boolean = True {OPTION});
begin
    inherited Create(False);
    FTableName  :=TableName;
    FDestGrid   :=DestGrid;
    FColumns    :=Columns;
    FConditions :=Conditions;
    FAutoRelease:=AutoRelease;
end;


procedure TTGeneralTables.Execute;
var
    IDThd:       cardinal;
    DataTables:  TDataTables;
begin

    IDThd:=TTGeneralTables.CurrentThread.ThreadID;
    DataTables:=TDataTables.Create(MainForm.DbConnect);

    try
        try
            DataTables.CleanUp;

            if not(string.IsNullOrEmpty(FColumns)) then
                DataTables.Columns.Text:=FColumns;

            if not(string.IsNullOrEmpty(FConditions)) then
                DataTables.CustFilter:=FConditions;

            if DataTables.OpenTable(FTableName) then
                DataTables.SqlToGrid(FDestGrid, DataTables.DataSet, False, True);

        except
            on E: Exception do
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot load general table, error has been thrown: ' + E.Message);
        end;
    finally
        DataTables.Free;
    end;

    /// <remarks>
    /// Do not use FreeOnTerminate, manually destroy the object after the job is done.
    /// This allow to execute this thread in series with "WaitFor" method.
    /// By default, FreeOnTerminate is always ON, use option "False" to disable it.
    /// </remarks>

    FreeOnTerminate:=FAutoRelease;

end;


end.

