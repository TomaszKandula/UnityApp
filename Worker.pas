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
  Main, ReportBug, Windows, Messages, SysUtils, Classes, Diagnostics, Graphics, ADODB, ComObj, SyncObjs, Dialogs, DB;

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
    var FLock:       TCriticalSection;
    var FMode:       integer;
    var FGrid:       TStringGrid;
    var FIDThd:      integer;
    var FContact:    string;
    var FEstatement: string;
    var FPhones:     string;
    var FSCUID:      string;
    var FConditions: string;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(ActionMode: integer; Grid: TStringGrid; SCUID, Contact, Estatement, Phones: string; Conditions: string);
    destructor  Destroy; override;
    function    Read     : boolean;
    function    Update   : boolean;
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

{ --------------------------------------------------------------------------------------------------------------------------------------- WRITE DAILY COMMENT }
type
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
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(CUID: string; Email: boolean; CallEvent: boolean; CallDuration: integer; Comment: string; EmailReminder, EmailAutoStat, EmailManuStat: boolean);
    destructor  Destroy; override;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------------- WRITE GENERAL COMMENT }
type
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
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(CUID: string; FixedComment: string; FollowUp: string; Free1: string; Free2: string);
    destructor  Destroy; override;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------------ SEND ACCOUNT STATEMENT }
type
  TTSendAccountStatement = class(TThread)
  protected
    procedure Execute; override;
  private
    var FLock:         TCriticalSection;
    var FIDThd:        integer;
    var FLayout:       integer;
    var FSalut:        string;
    var FMess:         string;
    var FIsOverdue:    boolean;
    var FCUID:         string;
    var FCustName:     string;
    var FCoCode:       string;
    var FBranch:       string;
    var FSCUID:        string;
    var FOpenItems:    TStringGrid;
    var FCustNumber:   string;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(Layout: integer; Salut: string; Mess: string; IsOverdue: boolean; OpenItems: TStringGrid; SCUID: string; CUID: string; CustName: string; CustNumber: string; CoCode: string; Branch: string);
    destructor  Destroy; override;
  end;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, DataBase, Settings, UAC, Mailer, AgeView, Transactions, Tracker, Actions;

{ ############################################################# ! SEPARATE CPU THREADS ! #################################################################### }

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
                  MainForm.LoadingAnimation(MainForm.ImgLoadingAgeView, MainForm.sgAgeView, MainForm.PanelAgeView, AnimationON);
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
                    AgeView.MapGroup3(MainForm.sgAgeView, MainForm.sgGroup3);
                    MainForm.sgAgeView.Repaint;
                    MainForm.LoadingAnimation(MainForm.ImgLoadingAgeView, MainForm.sgAgeView, MainForm.PanelAgeView, AnimationOFF);
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
                  MainForm.LoadingAnimation(MainForm.ImgLoadingOpenItems, MainForm.sgOpenItems, MainForm.PanelOpenItems, AnimationON);
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
                  OpenItems.DestGrid.SetColWidth(10, 20, 400);
                  MainForm.LoadingAnimation(MainForm.ImgLoadingOpenItems, MainForm.sgOpenItems, MainForm.PanelOpenItems, AnimationOFF);
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
constructor TTAddressBook.Create(ActionMode: integer; Grid: TStringGrid; SCUID, Contact, Estatement, Phones: string; Conditions: string);
begin
  inherited Create(False);
  FLock      :=TCriticalSection.Create;
  FMode      :=ActionMode;
  FGrid      :=Grid;
  FContact   :=Contact;
  FEstatement:=Estatement;
  FPhones    :=Phones;
  FSCUID     :=SCUID;
  FIDThd     :=0;
  FConditions:=Conditions;
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
        MainForm.ExecMessage(False, mcError, 'Read function of Address Book has failed. Please contact IT support.');
      end;
    end;
    { ------------------------------------------------------------------------------------------------------------------------------------------------ UPDATE }
    if FMode = adUpdate then
    begin
      if Update then
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: The Address Book has been updated successfully.')
          else
            LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update data in Address Book.');
    end;
    { ------------------------------------------------------------------------------------------------------------------------------------ INSERT NEW RECORDS }
    if FMode = adInsert then
    begin
      if Add then
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: The Address Book insertion has been executed successfully.')
          else
            LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot insert data to Address Book.');
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
                  MainForm.LoadingAnimation(MainForm.ImgLoadingAddressBook, MainForm.sgAddressBook, MainForm.PanelAddressBook, AnimationON);
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
    if FMode = adOpenForUser then DataTables.CustFilter:=FConditions;
    DataTables.OpenTable(TblAddressbook);
    Result:=DataTables.SqlToGrid(FGrid, DataTables.DataSet, True, True);
  finally
    DataTables.Free;
    { SET AUTO COLUMN WIDTH }
    Synchronize(procedure
                begin
                  FGrid.SetColWidth(40, 10, 400);
                  MainForm.LoadingAnimation(MainForm.ImgLoadingAddressBook, MainForm.sgAddressBook, MainForm.PanelAddressBook, AnimationOFF);
                end);
    { RELEASE }
    FGrid.Freeze(False);
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- WRITE }
function TTAddressBook.Update: boolean;
var
  Book:       TDataTables;
  iCNT:       integer;
  Condition:  string;
begin
  Result:=False;
  Book:=TDataTables.Create(MainForm.DbConnect);
  try
    { UPDATE FROM ADDRESS BOOK STRING GRID }
    if FGrid <> nil then
    begin
      if FGrid.UpdatedRowsHolder <> nil then
      begin
        for iCNT:=low(FGrid.UpdatedRowsHolder) to high(FGrid.UpdatedRowsHolder) do
        begin
          Condition:=TAddressBook.SCUID + EQUAL + FGrid.Cells[FGrid.ReturnColumn(TAddressBook.SCUID, 1, 1), FGrid.UpdatedRowsHolder[iCNT]];
          { COLUMNS }
          Book.Columns.Add(TAddressBook.EMAILS);
          Book.Columns.Add(TAddressBook.PHONE_NUMBERS);
          Book.Columns.Add(TAddressBook.CONTACT);
          Book.Columns.Add(TAddressBook.ESTATEMENTS);
          { VALUES }
          Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.EMAILS,        1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
          Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.PHONE_NUMBERS, 1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
          Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.CONTACT,       1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
          Book.Values.Add(FGrid.Cells[FGrid.ReturnColumn(TAddressBook.ESTATEMENTS,   1, 1), FGrid.UpdatedRowsHolder[iCNT]]);
          { CONDITIONS }
          Book.Conditions.Add(Condition);
          Book.Conditions.Add(Condition);
          Book.Conditions.Add(Condition);
          Book.Conditions.Add(Condition);
        end;
        Result:=Book.UpdateRecord(TblAddressbook);
        { SUCCESS }
        if Result then
        begin
          FGrid.SetUpdatedRow(0);
          MainForm.ExecMessage(False, mcInfo, 'Address Book has been updated succesfully!');
        end
        else
        { ERROR DURING POST }
        begin
          MainForm.ExecMessage(False, mcError, 'Cannot update Address Book. Please contact IT support.');
        end;
      end
      else
      { NO CHANGES WITHIN ADDRESS BOOK STRING GRID }
      begin
        MainForm.ExecMessage(False, mcWarn, 'Nothing to update. Please make changes first and try again.');
      end;
    end;
    { UPDATE FROM ACTION LOG VIEW }
    if FGrid = nil then
    begin
      Condition:=TAddressBook.SCUID + EQUAL + QuotedStr(FSCUID);
      { COLUMNS }
      Book.Columns.Add(TAddressBook.PHONE_NUMBERS);
      Book.Columns.Add(TAddressBook.CONTACT);
      Book.Columns.Add(TAddressBook.ESTATEMENTS);
      { VALUES }
      Book.Values.Add(FPhones);
      Book.Values.Add(FContact);
      Book.Values.Add(FEstatement);
      { CONDITIONS }
      Book.Conditions.Add(Condition);
      Book.Conditions.Add(Condition);
      Book.Conditions.Add(Condition);
      { EXECUTE }
      Result:=Book.UpdateRecord(TblAddressbook);
      { ENDING }
      if Result then
        MainForm.ExecMessage(False, mcInfo, 'Address Book has been updated succesfully!')
          else
            MainForm.ExecMessage(False, mcError, 'Cannot update Address Book. Please contact IT support.');
      end;
  finally
    Book.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------- ADD SELECTED ITEM(S) TO ADDRESS BOOK }
function TTAddressBook.Add: boolean;
var
  iCNT:     integer;
  jCNT:     integer;
  SCUID:    string;
  AddrBook: TLists;
  Book:     TDataTables;
begin
  Result:=False;
  SetLength(AddrBook, 1, 11);
  jCNT:=0;
  { ------------------------------------------------------------------------------------------------------------------------------- GET DATA FROM STRING GRID }
  Book:=TDataTables.Create(MainForm.DbConnect);
  try
    for iCNT:=FGrid.Selection.Top to FGrid.Selection.Bottom do
    begin
      if FGrid.RowHeights[iCNT] <> sgRowHidden then
      begin
        { BUILD CUID }
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
        { ADD TO ARRAY IF NOT EXISTS }
        if Book.DataSet.RecordCount = 0 then
        begin
          { BUILD ARRAY }
          AddrBook[jCNT,  0]:=UpperCase(MainForm.WinUserName);
          AddrBook[jCNT,  1]:=SCUID;
          AddrBook[jCNT,  2]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), iCNT];
          AddrBook[jCNT,  3]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), iCNT];
          AddrBook[jCNT,  8]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fAGENT,           1, 1), iCNT];
          AddrBook[jCNT,  9]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fDIVISION,        1, 1), iCNT];
          AddrBook[jCNT, 10]:=FGrid.Cells[FGrid.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), iCNT];
          { MOVE NEXT }
          Inc(jCNT);
          SetLength(AddrBook, jCNT + 1, 11);
        end;
      end;
    end;
  finally
    Book.Free;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- SEND TO DATABASE }
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
      Book.StrSQL:=Book.ArrayToSql(AddrBook, TblAddressbook, Book.ColumnsToList(Book.Columns, enQuotesOff));
      Book.ExecSQL;
      if Book.RowsAffected > 0 then
      begin
        MainForm.ExecMessage(False, mcInfo, 'Address Book has been successfully populated by selected item(s).');
        Result:=True;
      end
      else
      begin
        MainForm.ExecMessage(False, mcError, 'Address Book cannot be updated. Please contact IT support.');
        Result:=False;
      end;
    except
      on E: Exception do
      begin
        MainForm.ExecMessage(False, mcError, 'Cannot save selected item(s). Exception has been thrown: ' + E.Message);
        LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Cannot write Address Book item(s) into database. Error: ' + E.Message);
      end;
    end;
  finally
    Book.Free;
    AddrBook:=nil;
  end;
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

{ ############################################################# ! WRITE DAILY COMMENT ! ##################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTDailyComment.Create(CUID: string; Email: boolean; CallEvent: boolean; CallDuration: integer; Comment: string; EmailReminder, EmailAutoStat, EmailManuStat: boolean);
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
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTDailyComment.Destroy;
begin
  FLock.Free;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ WRITE DATA }
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
  FLock.Acquire;
  try
    DailyText:=TDataTables.Create(MainForm.DbConnect);
    try
      Condition:=TDaily.CUID + EQUAL + QuotedStr(FCUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
      DataCheckSum:=FCUID + StringReplace(MainForm.AgeDateSel, '-', '', [rfReplaceAll]);
      DailyText.CustFilter:=WHERE + Condition;
      DailyText.OpenTable(TblDaily);
      { UPDATE EXISTING COMMENT }
      if not (DailyText.DataSet.RecordCount = 0) then
      begin
        DailyText.CleanUp;
        { DEFINE COLUMNS, VALUES AND CONDITIONS }
        DailyText.Columns.Add(TDaily.STAMP);
        DailyText.Values.Add(DateTimeToStr(Now));
        DailyText.Conditions.Add(Condition);
        DailyText.Columns.Add(TDaily.USER_ALIAS);
        DailyText.Values.Add(UpperCase(MainForm.WinUserName));
        DailyText.Conditions.Add(Condition);
        if FEmail then
        begin
          Email:=IntToStr(StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL].Value), 0));
          DailyText.Columns.Add(TDaily.EMAIL);
          DailyText.Values.Add(Email);
          DailyText.Conditions.Add(Condition);
        end;
        { CALL EVENT AND CALL DURATION ALWAYS COMES TOGETHER }
        if FCallEvent then
        begin
          CallEvent:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.CALLEVENT].Value), 0);
          Inc(CallEvent);
          DailyText.Columns.Add(TDaily.CALLEVENT);
          DailyText.Values.Add(IntToStr(CallEvent));
          DailyText.Conditions.Add(Condition);
          DailyText.Columns.Add(TDaily.CALLDURATION);
          DailyText.Values.Add(FCallDuration);
          DailyText.Conditions.Add(Condition);
        end;
        if FEmailReminder then
        begin
          EmailReminder:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_Reminder].Value), 0);
          Inc(EmailReminder);
          DailyText.Columns.Add(TDaily.EMAIL_Reminder);
          DailyText.Values.Add(IntToStr(EmailReminder));
          DailyText.Conditions.Add(Condition);
        end;
        if FEmailAutoStat then
        begin
          EmailAutoStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_AutoStat].Value), 0);
          Inc(EmailAutoStat);
          DailyText.Columns.Add(TDaily.EMAIL_AutoStat);
          DailyText.Values.Add(IntToStr(EmailAutoStat));
          DailyText.Conditions.Add(Condition);
        end;
        if FEmailManuStat then
        begin
          EmailManuStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_ManuStat].Value), 0);
          Inc(EmailManuStat);
          DailyText.Columns.Add(TDaily.EMAIL_ManuStat);
          DailyText.Values.Add(IntToStr(EmailManuStat));
          DailyText.Conditions.Add(Condition);
        end;
        if not(FFixedComment = '') then
        begin
          DailyText.Columns.Add(TDaily.FIXCOMMENT);
          DailyText.Values.Add(FFixedComment);
          DailyText.Conditions.Add(Condition);
        end;
        { EXECUTE }
        if (DailyText.UpdateRecord(TblDaily)) and (DailyText.RowsAffected > 0) then
        begin
          { REFRESH HISTORY GRID }
          Synchronize(procedure
                      begin
                        ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                      end);
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''DailyComment'' table has been updated (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
        end
        else
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update daily comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
          MainForm.ExecMessage(False, mcError, 'Cannot update daily comment into database. Please contact IT support.');
        end;
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
        { EXECUTE }
        if (DailyText.InsertInto(TblDaily)) and (DailyText.RowsAffected > 0) then
        begin
          { REFRESH HISTORY GRID }
          Synchronize(procedure
                      begin
                        ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
                      end);
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''DailyComment'' table has been posted (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
        end
        else
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot post daily comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(DailyText.RowsAffected) + '.');
          MainForm.ExecMessage(False, mcError, 'Cannot post daily comment into database. Please contact IT support.');
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

{ ############################################################ ! WRITE GENERAL COMMENT ! #################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTGeneralComment.Create(CUID: string; FixedComment: string; FollowUp: string; Free1: string; Free2: string);
begin
  inherited Create(False);
  FLock        :=TCriticalSection.Create;
  FIDThd       :=0;
  FCUID        :=CUID;
  FFixedComment:=FixedComment;
  FFollowUp    :=FollowUp;
  FFree1       :=Free1;
  FFree2       :=Free2;
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
      Condition:=TGeneral.CUID + EQUAL + QuotedStr(FCUID);
      GenText.CustFilter:=WHERE + Condition;
      GenText.OpenTable(TblGeneral);
      { UPDATE }
      if not (GenText.DataSet.RecordCount = 0) then
      begin
        GenText.CleanUp;
        { DEFINE COLUMNS, VALUES AND CONDITIONS }
        GenText.Columns.Add(TGeneral.STAMP);
        GenText.Values.Add(DateTimeToStr(Now));
        GenText.Conditions.Add(Condition);
        GenText.Columns.Add(TGeneral.USER_ALIAS);
        GenText.Values.Add(UpperCase(MainForm.WinUserName));
        GenText.Conditions.Add(Condition);
        if not(FFixedComment = strNULL) then
        begin
          GenText.Columns.Add(TGeneral.FIXCOMMENT);
          GenText.Values.Add(FFixedComment);
          GenText.Conditions.Add(Condition);
        end;
        if not(FFollowUp = strNULL) then
        begin
          GenText.Columns.Add(TGeneral.FOLLOWUP);
          GenText.Values.Add(FFollowUp);
          GenText.Conditions.Add(Condition);
        end;
        if not(FFree1 = strNULL) then
        begin
          GenText.Columns.Add(TGeneral.Free1);
          GenText.Values.Add(FFree1);
          GenText.Conditions.Add(Condition);
        end;
        if not(FFree2 = strNULL) then
        begin
          GenText.Columns.Add(TGeneral.Free2);
          GenText.Values.Add(FFree2);
          GenText.Conditions.Add(Condition);
        end;
        { EXECUTE }
        if (GenText.UpdateRecord(TblGeneral)) and (GenText.RowsAffected > 0) then
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''GeneralComment'' table has been updated (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
        end
        else
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot update general comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
          MainForm.ExecMessage(False, mcError, 'Cannot update general comment into database. Please contact IT support.');
        end;
      end
      else
      { INSERT NEW }
      begin
        GenText.CleanUp;
        { DEFINE COLUMNS AND VALUES }
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
        if not(FFree1 = strNULL) then
        begin
          GenText.Columns.Add(TGeneral.Free2);
          GenText.Values.Add(FFree2);
        end
        else
        begin
          GenText.Columns.Add(TGeneral.Free2);
          GenText.Values.Add('');
        end;
        { EXECUTE }
        if (GenText.InsertInto(TblGeneral)) and (GenText.RowsAffected > 0) then
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: ''GeneralComment'' table has been posted (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
        end
        else
        begin
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot post general comment (CUID: ' + FCUID + '). Rows affected: ' + IntToStr(GenText.RowsAffected) + '.');
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

{ ########################################################### ! SEND ACCOUNT STATEMENT ! #################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTSendAccountStatement.Create(Layout: integer; Salut: string; Mess: string; IsOverdue: boolean; OpenItems: TStringGrid; SCUID: string; CUID: string; CustName: string; CustNumber: string; CoCode: string; Branch: string);
begin
  inherited Create(False);
  FLock      :=TCriticalSection.Create;
  FIDThd     :=0;
  FLayout    :=Layout;
  FSalut     :=Salut;
  FMess      :=Mess;
  FIsOverdue :=IsOverdue;
  FOpenItems :=OpenItems;
  FSCUID     :=SCUID;
  FCUID      :=CUID;
  FCustName  :=CustName;
  FCustNumber:=CustNumber;
  FCoCode    :=CoCode;
  FBranch    :=Branch;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTSendAccountStatement.Destroy;
begin
  FLock.Free;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ SEND ACCOUNT STATEMENT }
procedure TTSendAccountStatement.Execute;
var
  Statement:   TDocument;
  AppSettings: TSettings;
begin
  FIDThd:=CurrentThread.ThreadID;
  FLock.Acquire;
  try
    Statement  :=TDocument.Create;
    AppSettings:=TSettings.Create;
    try
      { SETUP DETAILS }
      Statement.SCUID    :=FSCUID;
      Statement.CUID     :=FCUID;
      Statement.CustName :=FCustName;
      Statement.CoCode   :=FCoCode;
      Statement.Branch   :=FBranch;
      Statement.CustSalut:=FSalut;
      Statement.CustMess :=FMess;
      Statement.IsOverdue:=FIsOverdue;
      Statement.OpenItems:=FOpenItems;
      Statement.DocType  :=dcStatement;  { FIXED FLAG }

      { USE FULLY PRE-DEFINED TEMPLATE }
      if FLayout = maDefined then
      begin
        Statement.HTMLLayout:=Statement.LoadTemplate(AppSettings.FLayoutDir + AppSettings.TMIG.ReadString(VariousLayouts, 'STATEMENT', '') + '.html');
      end;

      { USE PRE-DEFINED TEMPLATE WITH TWO CUSTOM FILEDS }
      if FLayout = maCustom then
      begin
        Statement.HTMLLayout:=Statement.LoadTemplate(AppSettings.FLayoutDir + AppSettings.TMIG.ReadString(VariousLayouts, 'CUSTSTATEMENT', '') + '.html');
      end;

      { SEND STATEMENT }
      Statement.MailSubject:='Account Statement - ' + FCustName + ' - ' + FCustNumber;
      if Statement.SendDocument then
      begin
        { REGISTER ACTION }
        if FLayout = maDefined then TTDailyComment.Create(FCUID, False, False, 0, 'Automatic account statement has been sent.', False, True, False);
        if FLayout = maCustom  then TTDailyComment.Create(FCUID, False, False, 0, 'Custom defined account statement has been sent.', False, False, True);
        MainForm.ExecMessage(False, mcInfo, 'Account Statement has been sent successfully!')
      end
      else
      begin
        MainForm.ExecMessage(False, mcError, 'Account Statement cannot be sent. Please contact IT support.');
      end;

    finally
      AppSettings.Free;
      Statement.Free;
    end;
  finally
    FLock.Release;
  end;
  FreeOnTerminate:=True;
end;

end.

