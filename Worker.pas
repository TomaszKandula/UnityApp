{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Worker;

interface

uses
  Main, ReportBug, Windows, Messages, SysUtils, Classes, Diagnostics, Graphics, ADODB, ComObj, SyncObjs, Dialogs;

{ ----------------------------------------------------------- ! SEPARATE CPU THREADS ! ---------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- CHECK SERVER CONNECTION }
type
  TTCheckServerConnection = class(TThread)
  protected
    procedure Execute; override;
  end;

{ ----------------------------------------------------------------------------------------------------------------------------------- INVOICE TRACKER SCANNER }
type
  TTInvoiceTrackerScanner = class(TThread)
  protected
    procedure Execute; override;
  public

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

{ --------------------------------------------------------------------------------------------------------------------------------------- FREE COLUMN EDITING }
type
  TTFreeColumn = class(TThread)
  protected
    procedure Execute; override;
  private
    var FLock:       TCriticalSection;
    var FIDThd:      integer;
    var FField:      integer;
    var FFreeText1:  string;
    var FFreeText2:  string;
    var FCUID:       string;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(FreeText: string; Field: integer; CUID: string);
    destructor  Destroy; override;
  end;

{ --------------------------------------------------------------------------------------------------------------------------------------- WRITE DAILY COMMENT }
type
  TTDailyComment = class(TThread)
  protected
    procedure Execute; override;
  private
    var FLock:       TCriticalSection;
    var FIDThd:      integer;
    var FComment:    string;
    var FCUID:       string;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(Comment: string; CUID: string);
    destructor  Destroy; override;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------------- WRITE GENERAL COMMENT }
type
  TTGeneralComment = class(TThread)
  protected
    procedure Execute; override;
  private
    var FLock:       TCriticalSection;
    var FIDThd:      integer;
    var FComment:    string;
    var FCUID:       string;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(Comment: string; CUID: string);
    destructor  Destroy; override;
  end;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, DataBase, Settings, UAC, Mailer, AgeView, Transactions, Tracker, Actions;

{ ############################################################ ! SEPARATE CPU THREADS ! ##################################################################### }

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
    TrackerForm.Display;
  except
    on E: Exception do
      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Execution of this tread work has been stopped. Error has been thrown: ' + E.Message + ' (TInvoiceTracker).');
  end;
  { RELEASE THREAD WHEN DONE }
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
  CanReload:  boolean;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  CanReload:=False;
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
        CanReload:=True;
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
  if CanReload then TTReadAgeView.Create(thNullParameter);
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
    { PREP }
    StopWatch:=TStopWatch.StartNew;
    MainForm.ExecMessage(True, mcStatusBar, stLoading);
    Synchronize(procedure
                begin
                  MainForm.LoadingAnimation(MainForm.ImgLoadingAgeView, MainForm.sgAgeView, AnimationON);
                end);
    try
      { SYNC }
      Synchronize(AgeView.ClearSummary);
      { ASYNC }
      AgeView.idThd  :=IDThd;
      AgeView.GroupID:=MainForm.GroupIdSel;
      AgeView.AgeDate:=MainForm.AgeDateSel;
      AgeView.Read(MainForm.sgAgeView);
      { SYNC }
      Synchronize(procedure
                  begin
                    AgeView.ComputeAgeSummary(MainForm.sgAgeView);
                    AgeView.ComputeAndShowRCA(MainForm.sgAgeView);
                    AgeView.UpdateSummary;
                    AgeView.GetDetails(MainForm.DetailsGrid);
                    MainForm.sgAgeView.Repaint;
                    MainForm.LoadingAnimation(MainForm.ImgLoadingAgeView, MainForm.sgAgeView, AnimationOFF);
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
        { SWITCH OFF ALL TIMERS }
        MainForm.SwitchTimers(tmDisabled);
        { REFRESH OPEN ITEMS AND MAKE NEW AGING VIEW }
        MainForm.OpenItemsUpdate:=ReadDateTime;
        MainForm.OpenItemsStatus:='';
        CanMakeAge:=True;
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
  if CanMakeAge then TTReadOpenItems.Create(thCallMakeAge);
end;

{ ############################################################# ! READ OPEN ITEMS ! ######################################################################### }

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
    { BUSY ANIMATION ON }
    Synchronize(procedure
                begin
                  MainForm.LoadingAnimation(MainForm.ImgLoadingOpenItems, MainForm.sgOpenItems, AnimationON);
                end);
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
    { RELEASE VCL AND SET AUTO COLUMN WIDTH }
    Synchronize(procedure
                begin
                  OpenItems.DestGrid.SetColWidth(10, 20);
                  MainForm.LoadingAnimation(MainForm.ImgLoadingOpenItems, MainForm.sgOpenItems, AnimationOFF);
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
    { BUSY ANIMATION ON }
    Synchronize(procedure
                begin
                  MainForm.LoadingAnimation(MainForm.ImgLoadingAddressBook, MainForm.sgAddressBook, AnimationON);
                end);
    { FREEZE STRING GRID }
    FGrid.Freeze(True);
    { COLUMN SELECTION }
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
    { FILTER BY USER ALIAS IF GIVEN }
    if FMode = adOpenForUser then DataTables.CustFilter:=WHERE + TAddressBook.USER_ALIAS + EQUAL + QuotedStr(MainForm.WinUserName);
    DataTables.OpenTable(TblAddressbook);
    Result:=DataTables.SqlToGrid(FGrid, DataTables.DataSet, True, True);
  finally
    DataTables.Free;
    { SET AUTO COLUMN WIDTH }
    Synchronize(procedure
                begin
                  FGrid.SetColWidth(40, 10);
                  MainForm.LoadingAnimation(MainForm.ImgLoadingAddressBook, MainForm.sgAddressBook, AnimationOFF);
                end);
    { RELEASE }
    FGrid.Freeze(False);
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- WRITE }
function TTAddressBook.Write: boolean;
var
  DataTables: TDataTables;
  iCNT:       integer;
  Start:      integer;
  TotalRows:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result   :=False;
  Start    :=0;
  TotalRows:=0;
  DataTables:=TDataTables.Create(MainForm.DbConnect);
  try
    { FREEZE STRING GRID }
    FGrid.Freeze(True);
    { COLUMN SELECTION }
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
    { PERFORM INSERT ON NEWLY ADDED ROWS ONLY }
    for iCNT:=1 to FGrid.RowCount - 1 do
    begin
      if (FGrid.Cells[0, iCNT] = '') and (FGrid.Cells[1, iCNT] <> '') and (FGrid.Cells[2, iCNT] <> '') then
      begin
        Start:=iCNT;
        TotalRows:=(FGrid.RowCount - 1) - Start;
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
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Address Book has been saved. Number of new lines written: ' + IntToStr(TotalRows) + '.');
        MainForm.ExecMessage(False, mcInfo, 'Address Book has been saved successfully!');
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

{ ############################################################ ! FREE COLUMN EDITING ! ###################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTFreeColumn.Create(FreeText: string; Field: integer; CUID: string);
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FIDThd:=0;
  FField:=Field;
  FCUID:=CUID;
  if FField = colFree1 then FFreeText1:=FreeText;
  if FField = colFree2 then FFreeText2:=FreeText;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTFreeColumn.Destroy;
begin
  FLock.Free;
end;

{ ---------------------------------------------------------------------------------------------------------------------- PERFORM INSERT/UPDATE ON FREE COLUMN }
procedure TTFreeColumn.Execute;
var
  GenText:   TDataTables;
  Condition: string;
begin
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  try
    GenText:=TDataTables.Create(MainForm.DbConnect);
    try
      GenText.OpenTable(TblGeneral);
      Condition:=TGeneral.CUID + EQUAL + QuotedStr(FCUID);
      GenText.DataSet.Filter:=Condition;
      { UPDATE }
      if not (GenText.DataSet.RecordCount = 0) then
      begin
        GenText.CleanUp;
        { DEFINE COLUMNS, VALUES AND CONDITIONS }
        GenText.Columns.Add(TGeneral.STAMP);        GenText.Values.Add(DateTimeToStr(Now));              GenText.Conditions.Add(Condition);
        GenText.Columns.Add(TGeneral.USER_ALIAS);   GenText.Values.Add(UpperCase(MainForm.WinUserName)); GenText.Conditions.Add(Condition);
        if FField = colFree1 then
        begin
          GenText.Columns.Add(TGeneral.Free1);
          GenText.Values.Add(FFreeText1);
        end;
        if FField = colFree2 then
        begin
          GenText.Columns.Add(TGeneral.Free2);
          GenText.Values.Add(FFreeText2);
        end;
        GenText.Conditions.Add(Condition);
        { EXECUTE }
        GenText.UpdateRecord(TblGeneral);
      end
      else
      { INSERT NEW }
      begin
        GenText.CleanUp;
        { DEFINE COLUMNS AND VALUES }
        GenText.Columns.Add(TGeneral.CUID);       GenText.Values.Add(FCUID);
        GenText.Columns.Add(TGeneral.STAMP);      GenText.Values.Add(DateTimeToStr(Now));
        GenText.Columns.Add(TGeneral.USER_ALIAS); GenText.Values.Add(UpperCase(MainForm.WinUserName));
        GenText.Columns.Add(TGeneral.FIXCOMMENT); GenText.Values.Add('');
        GenText.Columns.Add(TGeneral.FOLLOWUP);   GenText.Values.Add('');
        if FField = colFree1 then
        begin
          GenText.Columns.Add(TGeneral.Free1);      GenText.Values.Add(FFreeText1);
          GenText.Columns.Add(TGeneral.Free2);      GenText.Values.Add('');
        end;
        if FField = colFree2 then
        begin
          GenText.Columns.Add(TGeneral.Free1);      GenText.Values.Add('');
          GenText.Columns.Add(TGeneral.Free2);      GenText.Values.Add(FFreeText2);
        end;
        { EXECUTE }
        if GenText.InsertInto(TblGeneral) then
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Free Column has been posted for CUID: ' + FCUID + '.');
        end
        else
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot post "' + FFreeText1 + '" or "' + FFreeText2 + '" for CUID: ' + FCUID + '.');
          MainForm.ExecMessage(False, mcError, 'Cannot post Free Column into database. Please contact IT support.');
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

{ ############################################################# ! WRITE DAILY COMMENT ! ##################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTDailyComment.Create(Comment: string; CUID: string);
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FIDThd:=0;
  FComment:=Comment;
  FCUID:=CUID;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTDailyComment.Destroy;
begin
  FLock.Free;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ WRITE DATA }
procedure TTDailyComment.Execute;
var
  DailyText: TDataTables;
  UpdateOK:  boolean;
  InsertOK:  boolean;
  Condition: string;
begin
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  try
    UpdateOK:=False;
    InsertOK:=False;
    DailyText:=TDataTables.Create(MainForm.DbConnect);
    try
      DailyText.OpenTable(TblDaily);
      Condition:=TDaily.CUID + EQUAL + QuotedStr(FCUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
      DailyText.DataSet.Filter:=Condition;
      { UPDATE EXISTING COMMENT }
      if not (DailyText.DataSet.RecordCount = 0) then
      begin
        DailyText.CleanUp;
        { DEFINE COLUMNS, VALUES AND CONDITIONS }
        DailyText.Columns.Add(TDaily.STAMP);       DailyText.Values.Add(DateTimeToStr(Now));               DailyText.Conditions.Add(Condition);
        DailyText.Columns.Add(TDaily.USER_ALIAS);  DailyText.Values.Add(UpperCase(MainForm.WinUserName));  DailyText.Conditions.Add(Condition);
        DailyText.Columns.Add(TDaily.FIXCOMMENT);  DailyText.Values.Add(FComment);                         DailyText.Conditions.Add(Condition);
        { EXECUTE }
        UpdateOK:=DailyText.UpdateRecord(TblDaily);
      end
      else
      { INSERT NEW RECORD }
      begin
        DailyText.CleanUp;
        { DEFINE COLUMNS AND VALUES }
        DailyText.Columns.Add(TDaily.GROUP_ID);       DailyText.Values.Add(MainForm.GroupIdSel);
        DailyText.Columns.Add(TDaily.CUID);           DailyText.Values.Add(FCUID);
        DailyText.Columns.Add(TDaily.AGEDATE);        DailyText.Values.Add(MainForm.AgeDateSel);
        DailyText.Columns.Add(TDaily.STAMP);          DailyText.Values.Add(DateTimeToStr(Now));
        DailyText.Columns.Add(TDaily.USER_ALIAS);     DailyText.Values.Add(UpperCase(MainForm.WinUserName));
        DailyText.Columns.Add(TDaily.EMAIL);          DailyText.Values.Add('0');
        DailyText.Columns.Add(TDaily.CALLEVENT);      DailyText.Values.Add('0');
        DailyText.Columns.Add(TDaily.CALLDURATION);   DailyText.Values.Add('0');
        DailyText.Columns.Add(TDaily.FIXCOMMENT);     DailyText.Values.Add(FComment);
        DailyText.Columns.Add(TDaily.EMAIL_Reminder); DailyText.Values.Add('0');
        DailyText.Columns.Add(TDaily.EMAIL_AutoStat); DailyText.Values.Add('0');
        DailyText.Columns.Add(TDaily.EMAIL_ManuStat); DailyText.Values.Add('0');
        { EXECUTE }
        InsertOK:=DailyText.InsertInto(TblDaily);
      end;
      { REFRESH HISTORY GRID }
      Synchronize(procedure
                  begin
                    { OK }
                    if (InsertOK) or (UpdateOK) then
                    begin
                      ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Daily comment has been posted for CUID: ' + FCUID + '.');
                    end
                    { ERROR }
                    else
                    begin
                      LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot post daily comment for CUID: ' + FCUID + '.');
                      MainForm.ExecMessage(False, mcError, 'Cannot post daily comment into database. Please contact IT support.');
                    end;
                  end);
    finally
      DailyText.Free;
    end;
  finally
    FLock.Release;
  end;
  FreeOnTerminate:=True;
end;

{ ############################################################ ! WRITE GENERAL COMMENT ! #################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTGeneralComment.Create(Comment: string; CUID: string);
begin
  inherited Create(False);
  FLock :=TCriticalSection.Create;
  FIDThd:=0;
  FComment:=Comment;
  FCUID:=CUID;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTGeneralComment.Destroy;
begin
  FLock.Free;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ WRITE DATA }
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
      GenText.OpenTable(TblGeneral);
      Condition:=TGeneral.CUID + EQUAL + QuotedStr(FCUID);
      GenText.DataSet.Filter:=Condition;
      { UPDATE }
      if not (GenText.DataSet.RecordCount = 0) then
      begin
        GenText.CleanUp;
        { DEFINE COLUMNS, VALUES AND CONDITIONS }
        GenText.Columns.Add(TGeneral.STAMP);        GenText.Values.Add(DateTimeToStr(Now));             GenText.Conditions.Add(Condition);
        GenText.Columns.Add(TGeneral.USER_ALIAS);   GenText.Values.Add(UpperCase(MainForm.WinUserName));GenText.Conditions.Add(Condition);
        GenText.Columns.Add(TGeneral.FIXCOMMENT);   GenText.Values.Add(FComment);                       GenText.Conditions.Add(Condition);
        { EXECUTE }
        GenText.UpdateRecord(TblGeneral);
      end
      else
      { INSERT NEW }
      begin
        GenText.CleanUp;
        { DEFINE COLUMNS AND VALUES }
        GenText.Columns.Add(TGeneral.CUID);       GenText.Values.Add(FCUID);
        GenText.Columns.Add(TGeneral.STAMP);      GenText.Values.Add(DateTimeToStr(Now));
        GenText.Columns.Add(TGeneral.USER_ALIAS); GenText.Values.Add(UpperCase(MainForm.WinUserName));
        GenText.Columns.Add(TGeneral.FIXCOMMENT); GenText.Values.Add(FComment);
        GenText.Columns.Add(TGeneral.FOLLOWUP);   GenText.Values.Add('');
        GenText.Columns.Add(TGeneral.Free1);      GenText.Values.Add('');
        GenText.Columns.Add(TGeneral.Free2);      GenText.Values.Add('');
        { EXECUTE }
        if GenText.InsertInto(TblGeneral) then
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: General comment has been posted for CUID: ' + FCUID + '.');
        end
        else
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot post general comment for CUID: ' + FCUID + '.');
          MainForm.ExecMessage(False, mcError, 'Cannot post general comment into database. Please contact IT support.');
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

end.
