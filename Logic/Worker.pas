
{$I .\Include\Header.inc}

unit Worker;

interface

uses
    Windows, Messages, SysUtils, Classes, Diagnostics, Graphics, ADODB, ComObj, SyncObjs, Dialogs, DB, InterposerClasses, Arrays;

    /// <remarks>
    ///     Asynchronous methods executed within single thread classes. Most of the thread classes aquire lock, so they can be called
    ///     only once at a time. If necessary, remove lock aquisition.
    /// </remarks>

type

    /// <summary>
    ///     Check connection to database (to be executed on regular basis).
    /// </summary>

    TTCheckServerConnection = class(TThread)
    protected
        procedure Execute; override;
    end;

    /// <summary>
    ///     Invoice tracker scanner.
    /// </summary>

    TTInvoiceTrackerScanner = class(TThread)
    protected
        procedure Execute; override;
    public
        //...
    end;

    /// <summary>
    ///     Invoice Tracker list refresh.
    /// </summary>

    TTInvoiceTrackerRefresh = class(TThread)
    protected
        procedure   Execute; override;
    private
        var pUserAlias: string;
        var FIDThd    : integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(UserAlias: string);
    end;

    /// <summary>
    ///     Generate agev view (aging report).
    /// </summary>

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

    /// <summary>
    ///     Read selected age view.
    /// </summary>

    TTReadAgeView = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:  TCriticalSection;
        var FIDThd: integer;
        var FMode:  integer;
        var FSort:  integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(ActionMode: integer; SortMode: integer);
        destructor  Destroy; override;
    end;

    /// <summary>
    ///     Check periodically if new open items are populated by SSIS from ERP system.
    /// </summary>

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

    /// <summary>
    ///     Read open items from database table and put it into String Grid component.
    /// </summary>

    TTReadOpenItems = class(TThread)
    protected
        procedure Execute; override;
    private
        var FMode:  integer;
        var FLock:  TCriticalSection;
        var FIDThd: integer;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(ActionMode: integer);
        destructor  Destroy; override;
    end;

    /// <summary>
    ///     Read and write from/to Address Book string grid.
    /// </summary>

    TTAddressBook = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:       TCriticalSection;
        var FMode:       integer;
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
        constructor Create(ActionMode: integer; Grid: TStringGrid; SCUID, Contact, Estatement, Email, Phones: string; Conditions: string);
        destructor  Destroy; override;
        function    Read     : boolean;
        function    Update   : boolean;
        function    Add      : boolean;
    end;

    /// <summary>
    ///     Send user feedback from ReportBug form.
    /// </summary>

    TTSendBugReport = class(TThread)
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

    /// <summary>
    ///     Generate Excel report.
    /// </summary>

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

    /// <summary>
    ///     Write daily comment into database table.
    /// </summary>

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
        var FShouldLock:    boolean;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(CUID: string; Email: boolean; CallEvent: boolean; CallDuration: integer; Comment: string; EmailReminder, EmailAutoStat, EmailManuStat: boolean; EventLog: boolean; ShouldLock: boolean = true {OPTION});
        destructor  Destroy; override;
    end;

    /// <summary>
    ///     Write general comment int database table.
    /// </summary>

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
        var FEventLog:     boolean;
    public
        property    IDThd:  integer read FIDThd;
        constructor Create(CUID: string; FixedComment: string; FollowUp: string; Free1: string; Free2: string; EventLog: boolean);
        destructor  Destroy; override;
    end;

    /// <summary>
    ///     Send account statement.
    /// </summary>

    TTSendAccountStatement = class(TThread)
    protected
        procedure Execute; override;
    private
        var FLock:         TCriticalSection;
        var FIDThd:        integer;
        var FSeries:       boolean;
        var FLayout:       integer;
        var FSubject:      string;
        var FSalut:        string;
        var FMess:         string;
        var FIsOverdue:    boolean;
        var FCUID:         string;
        var FSCUID:        string;
        var FCustName:     string;
        var FCoCode:       string;
        var FBranch:       string;
        var FOpenItems:    TStringGrid;
        var FCustNumber:   string;
        var FItemNo:       integer;
    public
        property    IDThd:  integer read FIDThd;
        destructor  Destroy; override;
        constructor Create(
            Series:     boolean;
            Layout:     integer;
            Subject:    string;
            Salut:      string;
            Mess:       string;
            IsOverdue:  boolean;
            OpenItems:  TStringGrid;
            SCUID:      string;
            CUID:       string;
            CustName:   string;
            CustNumber: string;
            CoCode:     string;
            Branch:     string;
            ItemNo:     integer = 0 {OPTION}
        );
    end;

    /// <summary>
    ///     Upload async. given general table. Use no locking.
    /// </summary>

    TTGeneralTables = class(TThread)
    protected
        procedure Execute; override;
    private
        var FIDThd:      integer;
        var FTableName:  string;
        var FColumns:    string;
        var FDestGrid:   TStringGrid;
        var FConditions: string;
    public
        constructor Create(TableName: string; DestGrid: TStringGrid; Columns: string = '' {=OPTION}; Conditions: string = '' {=OPTION});
    end;


implementation


uses
    Main, SQL, Model, DataBase, Settings, UAC, Mailer, AgeView, Transactions, Tracker, Actions, SendFeedback;


// --------------------------------------------------------------------------------------------------------------------------------- CHECK SERVER CONNECTION //


procedure TTCheckServerConnection.Execute;
var
    IDThd:       integer;
    DataBase:    TDataBase;
begin

    DataBase:=TDataBase.Create(False);

    try
        IDThd:=TTCheckServerConnection.CurrentThread.ThreadID;
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


// ----------------------------------------------------------------------------------------------------------------------------------------- TRACKER SCANNER //


procedure TTInvoiceTrackerScanner.Execute;
begin
    // to be done...
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
        TrackerForm.UserAlias:=pUserAlias;
        TrackerForm.Display;
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
    THDMili:    extended;
    THDSec:     extended;
    StopWatch:  TStopWatch;
    AgeView:    TAgeView;
    UserCtrl:   TUserControl;
    CanReload:  boolean;
begin

    CanReload:=False;
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    AgeView :=TAgeView.Create(MainForm.DbConnect);
    UserCtrl:=TUserControl.Create(MainForm.DbConnect);

    try
        StopWatch:=TStopWatch.StartNew;
        MainForm.ExecMessage(True, mcStatusBar, stGenerating);
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']:' + stGenerating);

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
                AgeView.Write(TblSnapshots, AgeView.ArrAgeView);
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
        MainForm.ExecMessage(True, mcStatusBar, stReady);
        THDMili:=StopWatch.ElapsedMilliseconds;
        THDSec:=THDMili / 1000;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Age View thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
        AgeView.Free;
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;
    if CanReload then
        TTReadAgeView.Create(thNullParameter, smRanges);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- READ AGE VIEW //


constructor TTReadAgeView.Create(ActionMode: integer; SortMode: integer);
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
        MainForm.ExecMessage(True, mcStatusBar, stLoading);
        Synchronize(procedure
        begin
            MainForm.LoadingAnimation(MainForm.ImgLoadingAgeView, MainForm.sgAgeView, MainForm.PanelAgeView, AnimationON);
        end);

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
                AgeView.GetDetails(MainForm.DetailsGrid);
                AgeView.MapGroup3(MainForm.sgAgeView, MainForm.sgGroup3);
                MainForm.sgAgeView.Repaint;
                MainForm.LoadingAnimation(MainForm.ImgLoadingAgeView, MainForm.sgAgeView, MainForm.PanelAgeView, AnimationOFF);
            end);

        except
            on E: Exception do
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTReadAgeView". Error has been thrown: ' + E.Message);
        end;

    finally
        MainForm.ExecMessage(True, mcStatusBar, stReady);
        THDMili:=StopWatch.ElapsedMilliseconds;
        THDSec:=THDMili / 1000;
        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Thread for selected Group Id "' + AgeView.GroupID + '" has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
        AgeView.Free;
        FLock.Release;

        // Switch on all timers
        MainForm.SwitchTimers(tmEnabled);
    end;

    // Release when finished
    FreeOnTerminate:=True;

    // Call open items if user select another age view
    if FMode = thCallOpenItems then
        TTReadOpenItems.Create(thNullParameter);

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
            ReadDateTime:=Transactions.GetDateTime(gdDateTime);
            ReadStatus:=Transactions.GetStatus(ReadDateTime);
            if ( StrToDateTime(MainForm.OpenItemsUpdate) < StrToDateTime(ReadDateTime) ) and ( ReadStatus = gsCompleted ) then
            begin
                // Switch off all of the timers
                MainForm.SwitchTimers(tmDisabled);

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
        TTReadOpenItems.Create(thCallMakeAge);

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- READ OPEN ITEMS //


constructor TTReadOpenItems.Create(ActionMode: integer);
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
        MainForm.ExecMessage(True, mcStatusBar, stDownloading);

        // Show busy animation
        Synchronize(procedure
        begin
            MainForm.LoadingAnimation(MainForm.ImgLoadingOpenItems, MainForm.sgOpenItems, MainForm.PanelOpenItems, AnimationON);
        end);

        try
            OpenItems.DestGrid   :=MainForm.sgOpenItems;
            OpenItems.SettingGrid:=MainForm.DetailsGrid;
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
        MainForm.ExecMessage(True, mcStatusBar, stReady);

        // Release VCL and set auto column width
        Synchronize(procedure
        begin
            OpenItems.DestGrid.SetColWidth(10, 20, 400);
            MainForm.LoadingAnimation(MainForm.ImgLoadingOpenItems, MainForm.sgOpenItems, MainForm.PanelOpenItems, AnimationOFF);
        end);

        OpenItems.DestGrid.Freeze(False);
        OpenItems.Free;
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;

    // Make age view from open items and send to SQL Server
    if FMode = thCallMakeAge then
    begin
        MainForm.cbDump.Checked:=False;
        TTMakeAgeView.Create(MainForm.OSAmount);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------- ADRESS BOOK //


constructor TTAddressBook.Create(ActionMode: integer; Grid: TStringGrid; SCUID, Contact, Estatement, Email, Phones: string; Conditions: string);
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
        MainForm.ExecMessage(True, mcStatusBar, stProcessing);

        // Open
        if (FMode = adOpenAll) or (FMode = adOpenForUser) then
        begin
            if Read then
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Address Book has been opened successfully.')
                    else
                        begin
                            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot open Address Book.');
                            MainForm.ExecMessage(False, mcError, 'Read function of Address Book has failed. Please contact IT support.');
                        end;
        end;

        // Update
        if FMode = adUpdate then
        begin
            if Update then
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: The Address Book has been updated successfully.')
                    else
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot insert data to Address Book, either error occured or item already exist.');
        end;

        // Insert new recodrs
        if FMode = adInsert then
        begin
            if Add then
                MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: The Address Book insertion has been executed successfully.')
                    else
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot insert data to Address Book, either error occured or item already exist.');
        end;

        // Export
        if FMode = adExport then
        begin
            FGrid.OpenThdId:=IDThd;
            FGrid.ExportCSV(MainForm.CSVExport, '|');
        end;

    finally
        MainForm.ExecMessage(True, mcStatusBar, stReady);
        FLock.Release;
    end;

    // Release when finished
    FreeOnTerminate:=True;

end;

function TTAddressBook.Read: boolean;
var
    DataTables: TDataTables;
begin
    DataTables:=TDataTables.Create(MainForm.DbConnect);
    try
        // Display busy animation
        Synchronize(procedure
        begin
            MainForm.LoadingAnimation(MainForm.ImgLoadingAddressBook, MainForm.sgAddressBook, MainForm.PanelAddressBook, AnimationON);
        end);

        // Freeze StringGrid
        FGrid.Freeze(True);

        // Column selection
        DataTables.Columns.Add(TAddressBook.USER_ALIAS);
        DataTables.Columns.Add(TAddressBook.SCUID);       { CONSTRAINT UNIQUE }
        DataTables.Columns.Add(TAddressBook.CUSTOMER_NUMBER);
        DataTables.Columns.Add(TAddressBook.CUSTOMER_NAME);
        DataTables.Columns.Add(TAddressBook.EMAILS);
        DataTables.Columns.Add(TAddressBook.ESTATEMENTS);
        DataTables.Columns.Add(TAddressBook.PHONE_NUMBERS);
        DataTables.Columns.Add(TAddressBook.CONTACT);
        DataTables.Columns.Add(TAddressBook.COCODE);
        DataTables.Columns.Add(TAddressBook.AGENT);
        DataTables.Columns.Add(TAddressBook.DIVISION);

        // Filter by User Aliaus (if given)
        if FMode = adOpenForUser then
            DataTables.CustFilter:=FConditions;

        DataTables.OpenTable(TblAddressbook);
        Result:=DataTables.SqlToGrid(FGrid, DataTables.DataSet, True, True);

    finally
        DataTables.Free;

        // Set auto column width
        Synchronize(procedure
        begin
            FGrid.SetColWidth(40, 10, 400);
            MainForm.LoadingAnimation(MainForm.ImgLoadingAddressBook, MainForm.sgAddressBook, MainForm.PanelAddressBook, AnimationOFF);
        end);

        // Release StringGrid
        FGrid.Freeze(False);
    end;

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
                    Condition:=TAddressBook.SCUID + EQUAL + FGrid.Cells[FGrid.ReturnColumn(TAddressBook.SCUID, 1, 1), FGrid.UpdatedRowsHolder[iCNT]];
                    // Columns
                    Book.Columns.Add(TAddressBook.EMAILS);
                    Book.Columns.Add(TAddressBook.PHONE_NUMBERS);
                    Book.Columns.Add(TAddressBook.CONTACT);
                    Book.Columns.Add(TAddressBook.ESTATEMENTS);
                    // Values
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.EMAILS,        1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.PHONE_NUMBERS, 1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.CONTACT,       1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                    Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.ESTATEMENTS,   1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
                end;

                Result:=Book.UpdateRecord(TblAddressbook, ttExplicit, Condition);

                // Success
                if Result then
                begin
                    FGrid.SetUpdatedRow(0);
                    MainForm.ExecMessage(False, mcInfo, 'Address Book has been updated succesfully!');
                end
                else
                // Error during post
                begin
                    MainForm.ExecMessage(False, mcError, 'Cannot update Address Book. Please contact IT support.');
                end;
            end
            else

            // No changes within Address Book string grid
            begin
                MainForm.ExecMessage(False, mcWarn, 'Nothing to update. Please make changes first and try again.');
            end;
        end;

        // Update from Action Log View
        if FGrid = nil then
        begin
            Condition:=TAddressBook.SCUID + EQUAL + QuotedStr(FSCUID);
            // Columns
            Book.Columns.Add(TAddressBook.PHONE_NUMBERS);
            Book.Columns.Add(TAddressBook.CONTACT);
            Book.Columns.Add(TAddressBook.ESTATEMENTS);
            Book.Columns.Add(TAddressBook.EMAILS);
            // Values
            Book.Values.Add(FPhones);
            Book.Values.Add(FContact);
            Book.Values.Add(FEstatement);
            Book.Values.Add(FEmail);

            Result:=Book.UpdateRecord(TblAddressbook, ttExplicit, Condition);

            // Ending
            if Result then
                MainForm.ExecMessage(False, mcInfo, 'Address Book has been updated succesfully!')
                    else
                        MainForm.ExecMessage(False, mcError, 'Cannot update Address Book. Please contact IT support.');
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
    AddrBook:   TLists;
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
            if FGrid.RowHeights[iCNT] <> sgRowHidden then
            begin
                // Build SCUID
                SCUID:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), iCNT] +
                    MainForm.ConvertName(
                        FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCO_CODE, 1, 1), iCNT],
                        'F',
                        3
                    );
                Book.CleanUp;
                Book.Columns.Add(TAddressBook.SCUID);
                Book.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
                Book.OpenTable(TblAddressbook);

                // Add to array if not exists
                if Book.DataSet.RecordCount = 0 then
                begin
                    Inc(Check);
                    AddrBook[jCNT,  0]:=UpperCase(MainForm.WinUserName);
                    AddrBook[jCNT,  1]:=SCUID;
                    AddrBook[jCNT,  2]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), iCNT];
                    AddrBook[jCNT,  3]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), iCNT];
                    AddrBook[jCNT,  8]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fAGENT,           1, 1), iCNT];
                    AddrBook[jCNT,  9]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fDIVISION,        1, 1), iCNT];
                    AddrBook[jCNT, 10]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), iCNT];
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
            Book.Columns.Add(TAddressBook.USER_ALIAS);
            Book.Columns.Add(TAddressBook.SCUID);
            Book.Columns.Add(TAddressBook.CUSTOMER_NUMBER);
            Book.Columns.Add(TAddressBook.CUSTOMER_NAME);
            Book.Columns.Add(TAddressBook.EMAILS);
            Book.Columns.Add(TAddressBook.PHONE_NUMBERS);
            Book.Columns.Add(TAddressBook.CONTACT);
            Book.Columns.Add(TAddressBook.ESTATEMENTS);
            Book.Columns.Add(TAddressBook.AGENT);
            Book.Columns.Add(TAddressBook.DIVISION);
            Book.Columns.Add(TAddressBook.COCODE);
            try

                Book.InsertInto(TblAddressbook, ttExplicit, nil, AddrBook);

                if Book.RowsAffected > 0 then
                begin
                    MainForm.ExecMessage(False, mcInfo, 'Address Book has been successfully populated by selected item(s).');
                    Result:=True;
                end
                else
                begin
                    MainForm.ExecMessage(False, mcWarn, 'Cannot update Address Book. Please contact IT support.');
                end;
            except
                on E: Exception do
                begin
                    MainForm.ExecMessage(False, mcError, 'Cannot save selected item(s). Exception has been thrown: ' + E.Message);
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
        MainForm.ExecMessage(False, mcWarn, 'Selected customers are already in Address Book.');
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- USER FEEDBACK //


constructor TTSendBugReport.Create;
begin
    inherited Create(False);
    FLock :=TCriticalSection.Create;
    FIDThd:=0;
end;

destructor TTSendBugReport.Destroy;
begin
    FLock.Free;
end;

procedure TTSendBugReport.Execute;
begin
    FLock.Acquire;
    FIDThd:=GetCurrentThreadId;

    try
        if ReportForm.SendReport then
        begin
            MainForm.ExecMessage(False, mcInfo, 'Report has been sent successfully!');
            Synchronize(ReportForm.ReportMemo.Clear);
            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Bug Report has been successfully sent by the user.');
        end
        else
        begin
            MainForm.ExecMessage(False, mcError, 'Cannot send Bug Report. Please contact IT support.');
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
        MainForm.ExecMessage(True, mcStatusBar, stExportXLS);

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
        MainForm.ExecMessage(True, mcStatusBar, stReady);
    end;

    FreeOnTerminate:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------------- WRITE DAILY COMMENT //


constructor TTDailyComment.Create(CUID: string; Email: boolean; CallEvent: boolean; CallDuration: integer; Comment: string; EmailReminder, EmailAutoStat, EmailManuStat: boolean; EventLog: boolean; ShouldLock: boolean = true {OPTION});
begin
    inherited Create(False);
    FShouldLock   :=ShouldLock;
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

    if FShouldLock then
        FLock:=TCriticalSection.Create;

end;

destructor TTDailyComment.Destroy;
begin

    if Assigned(FLock) then
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
begin
    FIDThd:=CurrentThread.ThreadID;

    if Assigned(FLock) then
        FLock.Acquire;

    try
        DailyText:=TDataTables.Create(MainForm.DbConnect);
        try
            Condition:=TDaily.CUID + EQUAL + QuotedStr(FCUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
            DataCheckSum:=FCUID + StringReplace(MainForm.AgeDateSel, '-', '', [rfReplaceAll]);
            DailyText.CustFilter:=WHERE + Condition;
            DailyText.OpenTable(TblDaily);

            // Update exisiting comment
            if not (DailyText.DataSet.RecordCount = 0) then
            begin
                DailyText.CleanUp;

                // Define columns, values and conditions
                DailyText.Columns.Add(TDaily.STAMP);
                DailyText.Values.Add(DateTimeToStr(Now));
                DailyText.Columns.Add(TDaily.USER_ALIAS);
                DailyText.Values.Add(UpperCase(MainForm.WinUserName));

                if FEmail then
                begin
                    Email:=IntToStr(StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL].Value), 0));
                    DailyText.Columns.Add(TDaily.EMAIL);
                    DailyText.Values.Add(Email);
                end;

                // Call event and call duration always comes together
                if FCallEvent then
                begin
                    CallEvent:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.CALLEVENT].Value), 0);
                    Inc(CallEvent);
                    DailyText.Columns.Add(TDaily.CALLEVENT);
                    DailyText.Values.Add(IntToStr(CallEvent));
                    DailyText.Columns.Add(TDaily.CALLDURATION);
                    DailyText.Values.Add(FCallDuration);
                end;

                if FEmailReminder then
                begin
                    EmailReminder:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_Reminder].Value), 0);
                    Inc(EmailReminder);
                    DailyText.Columns.Add(TDaily.EMAIL_Reminder);
                    DailyText.Values.Add(IntToStr(EmailReminder));
                end;

                if FEmailAutoStat then
                begin
                    EmailAutoStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_AutoStat].Value), 0);
                    Inc(EmailAutoStat);
                    DailyText.Columns.Add(TDaily.EMAIL_AutoStat);
                    DailyText.Values.Add(IntToStr(EmailAutoStat));
                end;

                if FEmailManuStat then
                begin
                    EmailManuStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_ManuStat].Value), 0);
                    Inc(EmailManuStat);
                    DailyText.Columns.Add(TDaily.EMAIL_ManuStat);
                    DailyText.Values.Add(IntToStr(EmailManuStat));
                end;

                if not(FFixedComment = '') then
                begin
                    DailyText.Columns.Add(TDaily.FIXCOMMENT);
                    DailyText.Values.Add(FFixedComment);
                end;

                // Execute
                if (DailyText.UpdateRecord(TblDaily, ttExplicit, Condition)) and (DailyText.RowsAffected > 0) then
                begin
                    // Refresh history grid
                    Synchronize(procedure
                    begin
                        ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                    end);

                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''DailyComment'' table has been updated (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update daily comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '. Error message received: ' + DailyText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, mcError, 'Cannot update daily comment into database.' +  CRLF + 'Error message received: ' + DailyText.LastErrorMsg + CRLF + 'Please contact IT support.');
                end;
            end
            else

            // nsert new record
            begin
                DailyText.CleanUp;

                // Define columns and values
                DailyText.Columns.Add(TDaily.GROUP_ID);       DailyText.Values.Add(MainForm.GroupIdSel);
                DailyText.Columns.Add(TDaily.CUID);           DailyText.Values.Add(FCUID);
                DailyText.Columns.Add(TDaily.AGEDATE);        DailyText.Values.Add(MainForm.AgeDateSel);
                DailyText.Columns.Add(TDaily.STAMP);          DailyText.Values.Add(DateTimeToStr(Now));
                DailyText.Columns.Add(TDaily.USER_ALIAS);     DailyText.Values.Add(UpperCase(MainForm.WinUserName));
                DailyText.Columns.Add(TDaily.DATACHECKSUM);   DailyText.Values.Add(DataCheckSum);

                if FEmail then
                begin
                    DailyText.Columns.Add(TDaily.EMAIL);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDaily.EMAIL);
                    DailyText.Values.Add('0');
                end;

                if FEmailReminder then
                begin
                    DailyText.Columns.Add(TDaily.EMAIL_Reminder);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDaily.EMAIL_Reminder);
                    DailyText.Values.Add('0');
                end;

                if FEmailAutoStat then
                begin
                    DailyText.Columns.Add(TDaily.EMAIL_AutoStat);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDaily.EMAIL_AutoStat);
                    DailyText.Values.Add('0');
                end;

                if FEmailManuStat then
                begin
                    DailyText.Columns.Add(TDaily.EMAIL_ManuStat);
                    DailyText.Values.Add('1');
                end
                else
                begin
                    DailyText.Columns.Add(TDaily.EMAIL_ManuStat);
                    DailyText.Values.Add('0');
                end;

                if FCallEvent then
                begin
                    DailyText.Columns.Add(TDaily.CALLEVENT);
                    DailyText.Values.Add('1');
                    DailyText.Columns.Add(TDaily.CALLDURATION);
                    DailyText.Values.Add(FCallDuration);
                end
                else
                begin
                    DailyText.Columns.Add(TDaily.CALLEVENT);
                    DailyText.Values.Add('0');
                    DailyText.Columns.Add(TDaily.CALLDURATION);
                    DailyText.Values.Add('0');
                end;

                DailyText.Columns.Add(TDaily.FIXCOMMENT);
                DailyText.Values.Add(FFixedComment);

                // Execute
                if (DailyText.InsertInto(TblDaily, ttExplicit)) and (DailyText.RowsAffected > 0) then
                begin
                    // Refresh history grid
                    Synchronize(procedure
                    begin
                        ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                    end);

                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''DailyComment'' table has been posted (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update daily comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '. Error message received: ' + DailyText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, mcError, 'Cannot post daily comment into database.' +  CRLF + 'Error message received: ' + DailyText.LastErrorMsg + CRLF + 'Please contact IT support.');
                end;
            end;

        finally
            DailyText.Free;
        end;

    finally
        if Assigned(FLock) then
            FLock.Release;
    end;

    FreeOnTerminate:=True;

end;


// ----------------------------------------------------------------------------------------------------------------------------------- WRITE GENERAL COMMENT //


constructor TTGeneralComment.Create(CUID: string; FixedComment: string; FollowUp: string; Free1: string; Free2: string; EventLog: boolean);
begin
    inherited Create(False);
    FLock        :=TCriticalSection.Create;
    FIDThd       :=0;
    FCUID        :=CUID;
    FFixedComment:=FixedComment;
    FFollowUp    :=FollowUp;
    FFree1       :=Free1;
    FFree2       :=Free2;
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
            Condition:=TGeneral.CUID + EQUAL + QuotedStr(FCUID);
            GenText.CustFilter:=WHERE + Condition;
            GenText.OpenTable(TblGeneral);

            // Update
            if not (GenText.DataSet.RecordCount = 0) then
            begin
                GenText.CleanUp;

                // Define columns, vaues and conditions
                GenText.Columns.Add(TGeneral.STAMP);
                GenText.Values.Add(DateTimeToStr(Now));
                GenText.Columns.Add(TGeneral.USER_ALIAS);
                GenText.Values.Add(UpperCase(MainForm.WinUserName));

                if not(FFixedComment = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.FIXCOMMENT);
                    GenText.Values.Add(FFixedComment);
                end;

                if not(FFollowUp = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.FOLLOWUP);
                    GenText.Values.Add(FFollowUp);
                end;

                if not(FFree1 = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.Free1);
                    GenText.Values.Add(FFree1);
                end;

                if not(FFree2 = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.Free2);
                    GenText.Values.Add(FFree2);
                end;

                // Execute
                if (GenText.UpdateRecord(TblGeneral, ttExplicit, Condition)) and (GenText.RowsAffected > 0) then
                begin
                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''GeneralComment'' table has been updated (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update general comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '. Error message received: ' + GenText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, mcError, 'Cannot update general comment into database.' +  CRLF + 'Error message received: ' + GenText.LastErrorMsg + CRLF + 'Please contact IT support.');
                end;
            end
            else

            // Insert new
            begin
                GenText.CleanUp;
                // Define columns and values
                GenText.Columns.Add(TGeneral.CUID);       GenText.Values.Add(FCUID);
                GenText.Columns.Add(TGeneral.STAMP);      GenText.Values.Add(DateTimeToStr(Now));
                GenText.Columns.Add(TGeneral.USER_ALIAS); GenText.Values.Add(UpperCase(MainForm.WinUserName));

                if not(FFixedComment = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.FIXCOMMENT);
                    GenText.Values.Add(FFixedComment);
                end
                else
                begin
                    GenText.Columns.Add(TGeneral.FIXCOMMENT);
                    GenText.Values.Add('');
                end;

                if not(FFollowUp = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.FOLLOWUP);
                    GenText.Values.Add(FFollowUp);
                end
                else
                begin
                    GenText.Columns.Add(TGeneral.FOLLOWUP);
                    GenText.Values.Add('');
                end;

                if not(FFree1 = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.Free1);
                    GenText.Values.Add(FFree1);
                end
                else
                begin
                    GenText.Columns.Add(TGeneral.Free1);
                    GenText.Values.Add('');
                end;

                if not(FFree2 = strNULL) then
                begin
                    GenText.Columns.Add(TGeneral.Free2);
                    GenText.Values.Add(FFree2);
                end
                else
                begin
                    GenText.Columns.Add(TGeneral.Free2);
                    GenText.Values.Add('');
                end;

                // Execute
                if (GenText.InsertInto(TblGeneral, ttExplicit)) and (GenText.RowsAffected > 0) then
                begin
                    if FEventLog then
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''GeneralComment'' table has been posted (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
                end
                else
                begin
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update general comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '. Error message received: ' + GenText.LastErrorMsg + '.');
                    MainForm.ExecMessage(False, mcError, 'Cannot update general comment into database.' +  CRLF + 'Error message received: ' + GenText.LastErrorMsg + CRLF + 'Please contact IT support.');
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


constructor TTSendAccountStatement.Create(Series: boolean; Layout: integer; Subject: string; Salut: string; Mess: string; IsOverdue: boolean; OpenItems: TStringGrid; SCUID: string; CUID: string; CustName: string; CustNumber: string; CoCode: string; Branch: string; ItemNo: integer = 0 {OPTION});
begin
    inherited Create(False);
    FLock      :=TCriticalSection.Create;
    FIDThd     :=0;
    FSeries    :=Series;
    FLayout    :=Layout;
    FSubject   :=Subject;
    FSalut     :=Salut;
    FMess      :=Mess;
    FIsOverdue :=IsOverdue;
    FOpenItems :=OpenItems;
    FCUID      :=CUID;
    FSCUID     :=SCUID;
    FCustName  :=CustName;
    FCustNumber:=CustNumber;
    FCoCode    :=CoCode;
    FBranch    :=Branch;
    FItemNo    :=ItemNo;
end;

destructor TTSendAccountStatement.Destroy;
begin
    FLock.Free;
end;

/// <remarks>
///     If execute in paraller, then slow down a bit the worker thread to avoid large CPU consumption.
/// </remarks>

procedure TTSendAccountStatement.Execute;
var
    Statement:   TDocument;
    Settings:    ISettings;
    DailyText:   TDataTables;
    Status:      string;
    Condition:   string;
begin
    FIDThd:=CurrentThread.ThreadID;
    FLock.Acquire;

    try
        Statement:=TDocument.Create;
        Settings :=TSettings.Create;
        try

            // Setup
            Statement.CUID     :=FCUID;
            Statement.SCUID    :=FSCUID;
            Statement.CustName :=FCustName;
            Statement.CoCode   :=FCoCode;
            Statement.Branch   :=FBranch;
            Statement.CustSalut:=FSalut;
            Statement.CustMess :=FMess;
            Statement.IsOverdue:=FIsOverdue;
            Statement.OpenItems:=FOpenItems;
            Statement.DocType  :=dcStatement;  { FIXED FLAG }

            // Use fully pre-defined template
            if FLayout = maDefined then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(Layouts, 'SINGLE2', ''));

            // Use pre-defined template with two custom fields
            if FLayout = maCustom then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(Layouts, 'SINGLE3', ''));

            // Send statement
            Statement.MailSubject:=FSubject + ' - ' + FCustName + ' - ' + FCustNumber;

            if FSeries then CurrentThread.Sleep(50);

            if Statement.SendDocument then
            begin

                // Get current daily comment for given CUID and extend it by adding status
                DailyText:=TDataTables.Create(MainForm.DbConnect);
                try
                    DailyText.Columns.Add(TDaily.FIXCOMMENT);
                    Condition:=TDaily.CUID + EQUAL + QuotedStr(FCUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
                    DailyText.CustFilter:=WHERE + Condition;
                    DailyText.OpenTable(TblDaily);

                    if not (DailyText.DataSet.RecordCount = 0) then
                    begin
                        Status:=DailyText.DataSet.Fields[TDaily.FIXCOMMENT].Value;
                        Status:=Status + CRLF + 'New account statement has been sent to the customer.';
                    end
                    else
                    begin
                        Status:='New account statement has been sent to the customer.';
                    end;
                finally
                    DailyText.Free;
                end;

                // Single email
                if not FSeries then
                begin
                    // Register action (use thread locking by default)
                    if FLayout = maDefined then TTDailyComment.Create(FCUID, False, False, 0, Status, False, True, False, True);
                    if FLayout = maCustom  then TTDailyComment.Create(FCUID, False, False, 0, Status, False, False, True, True);
                    MainForm.ExecMessage(False, mcInfo, 'Account Statement has been sent successfully!');
                end
                else
                // Many emails
                begin
                    if FLayout = maDefined then TTDailyComment.Create(FCUID, False, False, 0, Status, False, True, False, True, False {no locking});
                    if FLayout = maCustom  then TTDailyComment.Create(FCUID, False, False, 0, Status, False, False, True, True, False {no locking});
                    MainForm.ExecMessage(False, mmSendMany, IntToStr(FItemNo));
                end;

            end
            else
            begin

                // Fail to send an e-mail(s)
                if not FSeries then
                    MainForm.ExecMessage(False, mcError, 'Account Statement cannot be sent. Please contact IT support.')
                        else
                            MainForm.ExecMessage(False, mmSendMany, '-1');

            end;

        finally
            Statement.Free;
        end;

    finally
        FLock.Release;
    end;

    FreeOnTerminate:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------- LOAD ASYNC. GENERAL TABLE //


constructor TTGeneralTables.Create(TableName: string; DestGrid: TStringGrid; Columns: string = '' {OPTION}; Conditions: string = '' {OPTION});
begin
    inherited Create(False);
    FTableName :=TableName;
    FDestGrid  :=DestGrid;
    FColumns   :=Columns;
    FConditions:=Conditions;
end;

procedure TTGeneralTables.Execute;
var
    IDThd:       cardinal;
    DataTables:  TDataTables;
begin

    IDThd:=TTGeneralTables.CurrentThread.ThreadID;  { USE FOR ERROR HANDLING AND LOGGING }
    DataTables:=TDataTables.Create(MainForm.DbConnect);

    try
        try
            DataTables.CleanUp;

            if  not(string.IsNullOrEmpty(FColumns)) then
                DataTables.Columns.Text:=FColumns;

            if not(string.IsNullOrEmpty(FConditions)) then
                DataTables.CustFilter:=FConditions;

            if DataTables.OpenTable(FTableName) then
                DataTables.SqlToGrid(FDestGrid, DataTables.DataSet, False, True);

        except
            on E: Exception do
                { ERROR HANDLER HERE }
        end;
    finally
        DataTables.Free;
        { LOGGING HERE }
    end;

    FreeOnTerminate:=True;

end;


end.

