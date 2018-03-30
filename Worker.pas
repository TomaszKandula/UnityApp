{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Worker;

interface

uses
  Main, ReportBug, Windows, Messages, SysUtils, Classes, Diagnostics, Graphics, ADODB, ComObj, SyncObjs, Dialogs;

{ ----------------------------------------------------------- ! SEPARATE CPU THREADS ! ---------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------------------- SEND BUG REPORT }
type
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

{ ----------------------------------------------------------------------------------------------------------------------------------- INVOICE TRACKER SCANNER }
type
  TTInvoiceTrackerScanner = class(TThread)
  protected
    procedure Execute; override;
  public
//    function CanSendStatement: boolean;
//    function CanSendReminder: boolean;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------ INVOICE TRACKER LIST REFRESH }
type
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

{ ----------------------------------------------------------------------------------------------------------------------------------- CHECK SERVER CONNECTION }
type
  TTCheckServerConnection = class(TThread)
  protected
    procedure Execute; override;
  end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- MAKE AGE VIEW }
type
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

{ ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS SCANNER }
type
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

{ --------------------------------------------------------------------------------------------------------------------------------------------- READ AGE VIEW }
type
  TTReadAgeView = class(TThread)
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

{ ------------------------------------------------------------------------------------------------------------------------------------------- READ OPEN ITEMS }
type
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

{ ------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK READ AND WRITE }
type
  TTAddressBook = class(TThread)
  protected
    procedure Execute; override;
  private
    var FLock:  TCriticalSection;
    var FMode:  integer;
    var FGrid:  TStringGrid;
    var FIDThd: integer;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(ActionMode: integer; Grid: TStringGrid);
    destructor  Destroy; override;
    function    Read     : boolean;
    function    Write    : boolean;
    function    Add      : boolean;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- EXPORT TO EXCEL }
type
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

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, DataBase, Settings, UAC, Mailer, AgeView, Transactions, Tracker;

{ ############################################################ ! SEPARATE CPU THREADS ! ##################################################################### }

{ ################################################################# ! BUG REPORT ! ########################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTSendBugReport.Create;
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTSendBugReport.Destroy;
begin
  FLock.Free;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
procedure TTSendBugReport.Execute;
begin
  FLock.Acquire;
  FIDThd:=GetCurrentThreadId;
  try
    if ReportForm.SendReport then
    begin
      MainForm.ExecMessage(False, mcInfo, 'Report has been sent successfully!');
      Synchronize(ReportForm.ReportMemo.Clear);
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Bug Report has been successfully sent by the user.');
    end
    else
    begin
      MainForm.ExecMessage(False, mcError, 'Cannot send Bug Report. Please contact IT support.');
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot send Bug Report.');
    end;
  finally
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ ############################################################## ! TRACKER SCANNER ! ######################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
procedure TTInvoiceTrackerScanner.Execute;
begin
  //
end;

{ ############################################################ ! INVOICE TRACKER LIST ! ##################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
constructor TTInvoiceTrackerRefresh.Create(UserAlias: string);
begin
  inherited Create(False);
  pUserAlias:=UserAlias;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
procedure TTInvoiceTrackerRefresh.Execute;
begin
  FIDThd:=CurrentThread.ThreadID;
  try
    TrackerForm.UserAlias:=pUserAlias;
    TrackerForm.Show;
  except
    on E: Exception do
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Execution of this tread work has been stopped. Error has been thrown: ' + E.Message + ' (TInvoiceTracker).');
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ ################################################################ ! NETWORK SCANNER ! ###################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
procedure TTCheckServerConnection.Execute;
var
  IDThd:       integer;
  DataBase:    TDataBase;
  ConnStatus:  integer;
begin
  DataBase:=TDataBase.Create(False);
  try
    IDThd:=TTCheckServerConnection.CurrentThread.ThreadID;
    ConnStatus:=DataBase.Check;
    if (MainForm.ConnLastError <> 0) and (ConnStatus = 0) then
    begin
      Synchronize(procedure
      begin
        DataBase.InitializeConnection(IDThd, False, MainForm.DbConnect);
        MainForm.ConnLastError:=ConnStatus;
        MainForm.InvoiceScanTimer.Enabled:=True;
        MainForm.OILoader.Enabled        :=True;
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Connection with SQL Server database has been re-established.');
      end);
    end;
    if ConnStatus <> 0 then
    begin
      MainForm.ConnLastError:=ConnStatus;
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Connection with SQL Server database has been lost, awaiting to reconnect...');
    end;
  finally
    DataBase.Free;
  end;
  FreeOnTerminate:=True;
end;

{ ################################################################ ! MAKE AGE VIEW ! ######################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTMakeAgeView.Create(OpenAmount: double);
begin
  inherited Create(False);
  FLock      :=TCriticalSection.Create;
  FOpenAmount:=OpenAmount;
  FIDThd     :=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTMakeAgeView.Destroy;
begin
  FLock.Free;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- EXECUTE THREAD WORK }
procedure TTMakeAgeView.Execute;
var
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  AgeView:    TAgeView;
  UserCtrl:   TUserControl;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  AgeView :=TAgeView.Create(MainForm.DbConnect);
  UserCtrl:=TUserControl.Create(MainForm.DbConnect);
  try
    StopWatch:=TStopWatch.StartNew;
    MainForm.ExecMessage(True, mcStatusBar, stGenerating);
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Generating age view...');
    try
      AgeView.idThd:=IDThd;
      { ASYNC }
      if MainForm.EditGroupID.Text = MainForm.GroupIdSel then AgeView.GroupID:=MainForm.GroupIdSel
        else
          if MainForm.EditGroupID.Text <> '' then AgeView.GroupID:=MainForm.EditGroupID.Text
            else
              AgeView.GroupID:=MainForm.GroupIdSel;
      { GENERATE AGING }
      AgeView.Make(MainForm.OSAmount);
      { CSV OR SERVER? }
      if MainForm.cbDump.Checked then
      begin
        if MainForm.CSVExport.Execute then
          AgeView.ExportToCSV(MainForm.CSVExport.FileName, AgeView.ArrAgeView);
      end
      else
      { SEND TO SQL SERVER, UPDATE AGE DATE LIST AND RELOAD AGE VIEW ON MAIN TAB }
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
        TTReadAgeView.Create(thNullParameter);
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTMakeAgeView". Error has been thrown: ' + E.Message);
    end;
  finally
    MainForm.ExecMessage(True, mcStatusBar, stReady);
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Age View thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
    AgeView.Free;
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ ############################################################## ! OPEN ITEMS SCANNER ! ##################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTOpenItemsScanner.Create;
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTOpenItemsScanner.Destroy;
begin
  FLock.Free;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS SCANNER }
procedure TTOpenItemsScanner.Execute;
var
  Transactions: TTransactions;
  ReadDateTime: string;
begin
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  Transactions:=TTransactions.Create(MainForm.DbConnect);
  try
    try
      ReadDateTime:=Transactions.GetDateTime(gdDateTime);
      if StrToDateTime(MainForm.OpenItemsUpdate) < StrToDateTime(ReadDateTime) then
      begin
        { SWITCH OFF ALL TIMERS }
        MainForm.SwitchTimers(tmDisabled);
        { REFRESH OPEN ITEMS AND MAKE NEW AGING VIEW }
        MainForm.OpenItemsUpdate:=ReadDateTime;
        TTReadOpenItems.Create(thCallMakeAge);
      end;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTOpenItemsScanner". Error has been thrown: ' + E.Message);
    end;
  finally
    Transactions.Free;
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ ################################################################ ! READ AGE VIEW ! ######################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTReadAgeView.Create(ActionMode: integer);
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FMode :=ActionMode;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTReadAgeView.Destroy;
begin
  FLock.Free;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
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
    try
      { SYNC }
      Synchronize(AgeView.ClearSummary);
      { ASYNC }
      AgeView.idThd  :=IDThd;
      AgeView.GroupID:=MainForm.GroupIdSel;
      AgeView.AgeDate:=MainForm.AgeDateSel;
      AgeView.Read(MainForm.sgAgeView);
      { SYNC }
      Synchronize(AgeView.UpdateSummary);
      Synchronize(procedure
                  begin
                    AgeView.Details(MainForm.DetailsGrid);
                  end);
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTReadAgeView". Error has been thrown: ' + E.Message);
    end;
  finally
    MainForm.ExecMessage(True, mcStatusBar, stReady);
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Thread for selected Group Id "' + AgeView.GroupID + '" has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
    AgeView.Free;
    FLock.Release;
    { SWITCH ON ALL TIMERS }
    MainForm.SwitchTimers(tmEnabled);
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  { CALL OPEN ITEMS IF USER SELECT ANOTHER AGE VIEW }
  if FMode = thCallOpenItems then TTReadOpenItems.Create(thNullParameter);
end;

{ ################################################################ ! OPEN ITEMS ! ########################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTReadOpenItems.Create(ActionMode: integer);
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FMode :=ActionMode;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTReadOpenItems.Destroy;
begin
  FLock.Free;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- EXECUTE THREAD WORK }
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
    try
      { ASSIGN }
      OpenItems.DestGrid   :=MainForm.sgOpenItems;
      OpenItems.SettingGrid:=MainForm.DetailsGrid;
      { FREEZE }
      OpenItems.DestGrid.Freeze(True);
      { SYNC WITH GUI }
      Synchronize(OpenItems.ClearSummary);
      { ASYNC }
      OpenItems.LoadToGrid;
      OpenItems.UpdateSummary;
    except
      on E: Exception do
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(FIDThd) + ']: Cannot execute "TTReadOpenItems". Error has been thorwn: ' + E.Message);
    end;
  finally
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(FIDThd) + ']: Open Items loading thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
    MainForm.ExecMessage(True, mcStatusBar, stReady);
    { RELEASE VCL }
    Synchronize(procedure
                begin
                  OpenItems.DestGrid.SetColWidth(10, 20);
                end);
    OpenItems.DestGrid.Freeze(False);
    OpenItems.Free;
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  { MAKE AGE VIEW FROM OPEN ITEMS AND SEND TO SQL SERVER }
  if FMode = thCallMakeAge then
  begin
    MainForm.cbDump.Checked:=False;
    TTMakeAgeView.Create(MainForm.OSAmount);
  end;
end;

{ ############################################################### ! ADRESS BOOK ! ########################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTAddressBook.Create(ActionMode: integer; Grid: TStringGrid);
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FMode :=ActionMode;
  FGrid :=Grid;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTAddressBook.Destroy;
begin
  FLock.Free;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- EXECUTE THREAD WORK }
procedure TTAddressBook.Execute;
begin
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  try
    MainForm.ExecMessage(True, mcStatusBar, stProcessing);
    { -------------------------------------------------------------------------------------------------------------------------------------------------- OPEN }
    if (FMode = adOpenAll) or (FMode = adOpenForUser) then
    begin
      if Read then LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Address Book has been opened successfully.')
      else
      begin
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot open Address Book.');
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Read function of Address Book has failed. Please contact IT support.')));
      end;
    end;
    { ---------------------------------------------------------------------------------------------------------------------------------------------- SAVE NEW }
    if FMode = adSaveNew then
    begin
      if Write then
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: The Address Book has been saved successfully.')
          else
            LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot save to Address Book.');
    end;
    { ------------------------------------------------------------------------------------------------------------------------------------------------ IMPORT }
    if FMode = adImport then
    begin
      FGrid.OpenThdId:=IDThd;
      FGrid.ImportCSV(MainForm.CSVImport, '|');
    end;
    { ------------------------------------------------------------------------------------------------------------------------------------------------ EXPORT }
    if FMode = adExport then
    begin
      FGrid.OpenThdId:=IDThd;
      FGrid.ExportCSV(MainForm.CSVExport, '|');
    end;
  finally
    { ------------------------------------------------------------------------------------------------------------------------------------------- RELEASE ALL }
    MainForm.ExecMessage(True, mcStatusBar, stReady);
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ READ }
function TTAddressBook.Read: boolean;
var
  DataTables: TDataTables;
begin
  DataTables:=TDataTables.Create(MainForm.DbConnect);
  try
    { FREEZE STRING GRID }
    FGrid.Freeze(True);
    { COLUMN SELECTION }
    DataTables.Columns.Add(TAddressBook.USER_ALIAS);
    DataTables.Columns.Add(TAddressBook.CUID);
    DataTables.Columns.Add(TAddressBook.CUSTNUMBER);
    DataTables.Columns.Add(TAddressBook.CUSTNAME);
    DataTables.Columns.Add(TAddressBook.EMAILS);
    DataTables.Columns.Add(TAddressBook.ESTATEMENTS);
    DataTables.Columns.Add(TAddressBook.TELEPHONE);
    DataTables.Columns.Add(TAddressBook.CONTACT);
    DataTables.Columns.Add(TAddressBook.CUSTADDR);
    { FILTER BY USER ALIAS IF GIVEN }
    if FMode = adOpenForUser then DataTables.CustFilter:=WHERE + TAddressBook.USER_ALIAS + EQUAL + QuotedStr(MainForm.WinUserName);
    DataTables.OpenTable(TblAddressbook);
    Result:=DataTables.SqlToGrid(FGrid, DataTables.DataSet, True, True);
  finally
    FGrid.SetColWidth(40, 10);
    FGrid.Freeze(False);
    DataTables.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- WRITE }
function TTAddressBook.Write: boolean;
var
  DataTables: TDataTables;
  iCNT:       integer;
  Start:      integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result :=False;
  Start  :=0;
  DataTables:=TDataTables.Create(MainForm.DbConnect);
  try
    { FREEZE STRING GRID }
    FGrid.Freeze(True);
    { COLUMN SELECTION }
    DataTables.Columns.Add(TAddressBook.USER_ALIAS);
    DataTables.Columns.Add(TAddressBook.CUID);  { CONSTRAINT UNIQUE }
    DataTables.Columns.Add(TAddressBook.CUSTNUMBER);
    DataTables.Columns.Add(TAddressBook.CUSTNAME);
    DataTables.Columns.Add(TAddressBook.EMAILS);
    DataTables.Columns.Add(TAddressBook.ESTATEMENTS);
    DataTables.Columns.Add(TAddressBook.TELEPHONE);
    DataTables.Columns.Add(TAddressBook.CONTACT);
    DataTables.Columns.Add(TAddressBook.CUSTADDR);
    { PERFORM INSERT ON NEWLY ADDED ROWS ONLY }
    for iCNT:=1 to FGrid.RowCount - 1 do
    begin
      if FGrid.Cells[0, iCNT] = '' then
      begin
        Start:=iCNT;
        Break;
      end;
    end;
    { IF NEW ROWS EXISTS, THEN SAVE }
    if Start > 0 then
    begin
      try
        DataTables.ClearSQL;
        DataTables.StrSQL:=DataTables.GridToSql(FGrid, TblAddressbook, DataTables.ColumnsToList(DataTables.Columns, enQuotesOff), Start, 1);
        if not (DataTables.ExecSQL = nil) then
        begin
          { RE-DO LIST POSITION }
          for iCNT:=1 to FGrid.RowCount - 1 do FGrid.Cells[0, iCNT]:= IntToStr(iCNT);
          Result:=True;
        end;
      except
        on E: Exception do
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot add new record(s). Error has been thrown: ' + E.Message + '.');
          MainForm.ExecMessage(False, mcError, 'Cannot add new record(s). Please contact IT support.');
        end;
      end;
    end
    else
    begin
      MainForm.ExecMessage(False, mcWarn, 'No new records have beed found. Process has been stopped.');
    end;
  finally
    FGrid.Freeze(False);
    DataTables.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------- ADD SELECTED ITEM(S) TO ADDRESS BOOK }
function TTAddressBook.Add: boolean;
begin
  Result:=False;
end;

{ ############################################################## ! EXPORT TO EXCEL ! ######################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTExcelExport.Create;
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTExcelExport.Destroy;
begin
  FLock.Free;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- EXPORT AGE VIEW TO EXCEL }
procedure TTExcelExport.Execute;
var
  FileName:  string;
  Temp:      TStringGrid;
begin
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  try
    MainForm.ExecMessage(True, mcStatusBar, stExportXLS);
    { SAVE DIALOG BOX }
    Synchronize(procedure
                begin
                  if MainForm.XLExport.Execute then
                    FileName:=MainForm.XLExport.FileName
                      else
                        FileName:='';
                end);
    { GENERATE AND SAVE }
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

end.
