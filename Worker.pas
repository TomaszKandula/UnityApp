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
  Main, Windows, Messages, SysUtils, Classes, Diagnostics, Graphics, ADODB, ComObj, SyncObjs, Dialogs;

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
  end;


{ ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS SCANNER }
type
  TTOpenItemsScanner = class(TThread)
  protected
    procedure Execute; override;
  end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- MAKE AGE VIEW }
type
  TTMakeAgeView = class(TThread)
  protected
    procedure Execute; override;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------ INVOICE TRACKER LIST REFRESH }
type
  TTInvoiceTrackerRefresh = class(TThread)
  protected
    procedure Execute; override;    //remove modes!
  private
    pMode: string;
  public
    { MODES:                                    }
    {   'ALL'    = SHOW ALL REGISTERED INVOICES }
    {   'ADD'    = ADD NEW CUSTOMER             }
    {   'REMOVE' = REMOVE SELECTED CUSTOMER     }
    constructor Create(cMode: string);
  end;

(*
  { ------------------------------------------------------------------------------------------------------------------------------------- COMMENTARY HANDLING }
  TTCommentary = class(TThread)
    protected
      procedure Execute; override;
    private
      pMode:  string;
      pValue: string;
    public
      { MODES:                                    }
      {   '0' = GENERAL COMMENT                   }
      {   '1' = DAILY COMMENT                     }
      {   '2' = DAILY COMMENT + UPDATE VIEW       }
      {   '3' = FOLLOW UP DATE                    }
      constructor Create(cMode: string; dValue: string);
  end;
*)






{ --------------------------------------------------------------------------------------------------------------------------------------------- READ AGE VIEW }
type
  TTReadAgeView = class(TThread)
  protected
    procedure Execute; override;
  private
    var FMode:  integer;
    var FLock:  TCriticalSection;
    var FIDThd:  integer;
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
    var FIDThd:  integer;
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
    var FLock:   TCriticalSection;
    var FMode:   integer;
    var FGrid:   TStringGrid;
    var FIDThd:  integer;
  public
    property    IDThd:  integer read FIDThd;
    constructor Create(ActionMode: integer; Grid: TStringGrid);
    destructor  Destroy; override;
    function    Read     : boolean;
    function    Write    : boolean;
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
  Model, DataBase, Settings, UAC, Mailer, AgeView, Transactions;

{ ############################################################ ! SEPARATE CPU THREADS ! ##################################################################### }

{ ----------------------------------------------------------------------------------------------------------------------------------- CHECK SERVER CONNECTION }

(* DO NOT RUN THIS THREAD BEFORE 'INITIALIZECONNECTION' METHOD IS CALLED *)

procedure TTCheckServerConnection.Execute;  (* ASYNC *)
var
  IDThread:  integer;
  DataBase:  TDataBase;
begin
  DataBase:=TDataBase.Create(False);
  try
    IDThread:=TTCheckServerConnection.CurrentThread.ThreadID;
    DataBase.InitializeConnection(IDThread, False, MainForm.FDbConnect);
  finally
    DataBase.Free;
  end;
  FreeOnTerminate:=True;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- INVOICE TRACKER SCANNER }
procedure TTInvoiceTrackerScanner.Execute;  (* ASYNC *)
var
  IDThread:   integer;
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  InvoiceTracker: TInvoiceTracker;
  DataBase:   TDataBase;
begin
  IDThread:=TTInvoiceTrackerScanner.CurrentThread.ThreadID;
  InvoiceTracker:=TInvoiceTracker.Create;
  DataBase:=TDataBase.Create(False);
  try
    StopWatch:=TStopWatch.StartNew;
    try
      { ------------------------------------------------------- ! PRE | GUI UPDATE ! ------------------------------------------------------------------------ }
      if DataBase.Check = 0 then
      begin
        { SYNCHRONIZED WITH THE MAIN THREAD }
        Synchronize(procedure begin InvoiceTracker.Refresh(MainForm.sgInvoiceTracker, UpperCase(MainForm.FUserName)) end);
        { ------------------------------------------------- ! HEAVY DUTY TASK (RUN ASYNC) ! ----------------------------------------------------------------- }
        { CHECK CONDITIONS AND SEND E-MAILS }
        InvoiceTracker.Scanner(IDThread);
      end;

    except
      on E: Exception do
      begin
        //PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TInvoiceScanner). Exit Code = ' + IntToStr(ExitCode) + '.');
      end;
    end;
  finally
    DataBase.Free;
    InvoiceTracker.Free;
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Scanning invoices and sending reminders (if any) executed within: ' +
            FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS SCANNER }
procedure TTOpenItemsScanner.Execute;  (* ASYNC *)
//var
//  IDThread:  integer;
begin
//  IDThread:=TTOpenItemsScanner.CurrentThread.ThreadID;

  //...

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;


{ --------------------------------------------------------------------------------------------------------------------------------------------- MAKE AGE VIEW }
procedure TTMakeAgeView.Execute;  (* ASYNC & SYNC *)
var
  IDThread:   integer;
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  SL:         TStringList;
  iCNT:       integer;
  jCNT:       integer;
  temp:       string;
  DataBase: TDataBase;
//  UserControl: TUserControl;
  AgeView: TAgeView;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  IDThread:=TTMakeAgeView.CurrentThread.ThreadID;
  DataBase:=TDataBase.Create(False);

  if DataBase.Check = 0 then
  begin
    try
      StopWatch:=TStopWatch.StartNew;
      try
        { ----------------------------------------------------------------------------------------------------------------------- MAKE AGING REPORT TO AN ARRAY }
        AgeView:=TAgeView.Create(MainForm.FDbConnect);
        try
//          AgeView.Make(AgeView.GetCoCode(MainForm.GroupListBox.ItemIndex, 0, 2), OpenItems.OSamt, IDThread);

        { ---------------------------------------------------------------------------------------------------------------------------------- SAVE OUTPUT TO CSV }

        if MainForm.cbDump.Checked then
        begin

          { ---------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
          SL:=TStringList.Create;
          SL.Clear;
          temp:='';

          { ------------------------------------------------------------------------------------------------------------ OPEN SAVE DIALOG AND GENERATE CSV DATA }
          try
            for iCNT:=0 to high(AgeView.ArrAgeView) - 1 do
            begin
              for jCNT:=0 to 29 { 30 COLUMNS } do temp:=temp + AgeView.ArrAgeView[iCNT, jCNT] + ';';
              SL.Add(temp);
              temp:='';
            end;

             { ---------------------------------------------------------------------------------------------------------------------------------------- SAVE ALL }
            if MainForm.CSVExport.Execute then SL.SaveToFile(MainForm.CSVExport.FileName);

          finally
            SL.Free;
            PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
            LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Generating age view... done, saved to CSV file ' + MainForm.CSVExport.FileName + '.');
          end;

        end;

        { ------------------------------------------------------------------------------------------------------------------------------------------ SQL UPDATE }
        if not (MainForm.cbDump.Checked) then
        begin
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Generating age view... done, passing to SQL database...');
          PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Performing SQL transaction...')));

          AgeView.Write('tbl_snapshots', IDThread);

          { UPDATE THE LIST }
          Synchronize
            (procedure begin

//              UserControl:=TUserControl.Create;
//              try
//                UserControl.UACAgeDates(MainForm.ArrGroupList[MainForm.GroupListBox.ItemIndex, 0]);
//              finally
//                UserControl.Free;
//              end;

              MainForm.GroupListDates.ItemIndex:=MainForm.GroupListDates.Items.Count - 1;
            end);

          { REFRESH AGE VIEW }
          TTReadAgeView.Create(0);
        end;

        finally
          AgeView.Free;
        end;

      except
        on E: Exception do
        begin
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Cannot generate age view properly. Please contact IT support.')));
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TDBAge).');
          PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
        end;
      end;

    finally
      THDMili:=StopWatch.ElapsedMilliseconds;
      THDSec:=THDMili / 1000;
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Age View thread has been executed within ' +
                       FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
    end;
  end;

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  FreeAndNil(DataBase);

end;

{ -------------------------------------------------------------------------------------------------------------------------------------- TRACKER LIST REFRESH }
constructor TTInvoiceTrackerRefresh.Create(cMode: string);
begin
  inherited Create(False);
  pMode:=cMode;
end;

procedure TTInvoiceTrackerRefresh.Execute;  (* SYNC *)
var
  IDThread:  integer;
  InvoiceTracker: TInvoiceTracker;
  DataBase: TDataBase;
begin
  IDThread:=TTInvoiceTrackerRefresh.CurrentThread.ThreadID;
  InvoiceTracker:=TInvoiceTracker.Create;
  DataBase:=TDataBase.Create(False);

  try

    try
      if Database.Check = 0 then InvoiceTracker.Refresh(MainForm.sgInvoiceTracker, pMode)
        else
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Cannot refresh Invoice Tracker list. Database connection has been lost.');
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Refresh Invoice Tracker list with mode = ' + pMode + ' has ended successfully.');

    except
      on E: Exception do
      begin
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Cannot refresh Invoice Tracker''s list. Please contact IT support.')));
        PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TInvoiceTracker).');
      end;

    end;

  finally
    InvoiceTracker.Free;
  end;

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  FreeAndNil(DataBase);
end;

(*
{ ------------------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENTARY }
constructor TTCommentary.Create(cMode: string; dValue: string);
begin
  inherited Create(False);
  pMode :=cMode;
  pValue:=dValue;
end;

procedure TTCommentary.Execute;  { ASYNC }
var
  IDThread:  integer;
//  Commentary:  TCommentary;
begin
  IDThread:=TTCommentary.CurrentThread.ThreadID;

  try
    ActiveThreads[6]:=True;

    try
      if DataBase.LastError = 0 then
      begin
        //Commentary:=TCommentary.Create;
        //Commentary.idThd   :=IDThread;
        //Commentary.Mode    :=pMode;
        //Commentary.FollowUp:=pValue;
        //Commentary.Write;
      end;

    except
      on E: Exception do
      begin
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Cannot execute write function. Please contact IT support.')));
        PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
        LogText(Settings.AppDir + Settings.LOGFILE, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TDBCommentUpdate).');
      end;
    end;

  finally
    ActiveThreads[6]:=False;
  end;

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;
*)




{ ################################################################ ! READ AGE VIEW ! ######################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTReadAgeView.Create(ActionMode: integer);
begin
  inherited Create(False);
  FLock:=TCriticalSection.Create;
  FMode:=ActionMode;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTReadAgeView.Destroy;
begin
  FreeAndNil(FLock);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE WORKER THREAD }
Procedure TTReadAgeView.Execute;
var
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  AgeView:    TAgeView;
begin
  FIDThd:=TTReadAgeView.CurrentThread.ThreadID;
  FLock.Acquire;
  AgeView:=TAgeView.Create(MainForm.FDbConnect);
  try
    StopWatch:=TStopWatch.StartNew;
    MainForm.ExecMessage(True, WM_GETINFO, 10, stLoading);
    try
      { SYNC }
      Synchronize(AgeView.ClearSummary);
      { ASYNC }
      AgeView.idThd  :=IDThd;
      AgeView.GroupID:=MainForm.GroupNmSel;
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
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot execute "TTReadAgeView". Error has been thrown: ' + E.Message);
    end;
  finally
    MainForm.ExecMessage(True, WM_GETINFO, 10, stReady);
    AgeView.Free;
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Thread for selected group "' + MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 1] + '" has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  { CALL OPEN ITEMS IF USER SELECT ANOTHER AGE VIEW }
  if FMode = 1 then TTReadOpenItems.Create(0);
end;

{ ################################################################ ! OPEN ITEMS ! ########################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTReadOpenItems.Create(ActionMode: integer);
begin
  inherited Create(False);
  FLock:=TCriticalSection.Create;
  FMode:=ActionMode;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTReadOpenItems.Destroy;
begin
  FreeAndNil(FLock);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- EXECUTE THREAD WORK }
procedure TTReadOpenItems.Execute;
var
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  OpenItems:  TTransactions;
begin
  FIDThd:=TTReadOpenItems.CurrentThread.ThreadID;
  FLock.Acquire;
  OpenItems:=TTransactions.Create(MainForm.FDbConnect);
  try
    StopWatch:=TStopWatch.StartNew;
    MainForm.ExecMessage(True, WM_GETINFO, 10, stDownloading);
    try
      Synchronize(OpenItems.ClearSummary);
      OpenItems.LoadToGrid(MainForm.sgOpenItems, MainForm.DetailsGrid);
      Synchronize(OpenItems.UpdateSummary);
      Synchronize(procedure
                  begin
                    MainForm.sgOpenItems.SetColWidth(10, 20);
                  end);
    except
      on E: Exception do
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(FIDThd) + ']: Cannot load open items. Error has been thorwn: ' + E.Message);
    end;
  finally
    OpenItems.Free;
    MainForm.ExecMessage(True, WM_GETINFO, 10, stReady);
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(FIDThd) + ']: Open Items loading thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
    FLock.Release;
  end;
  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  { MAKE AGE VIEW FROM OPEN ITEMS AND SEND TO SQL SERVER }
  if FMode = 1 then
  begin
    MainForm.cbDump.Checked:=False;
    TTMakeAgeView.Create(False);
  end;
end;

{ ############################################################### ! ADRESS BOOK ! ########################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTAddressBook.Create(ActionMode: integer; Grid: TStringGrid);
begin
  inherited Create(False);
  FLock:=TCriticalSection.Create;
  FMode:=ActionMode;
  FGrid:=Grid;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTAddressBook.Destroy;
begin
  FreeAndNil(FLock);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- EXECUTE THREAD WORK }
procedure TTAddressBook.Execute;
begin
  FIDThd:=TTAddressBook.CurrentThread.ThreadID;
  FLock.Acquire;
  try
    MainForm.ExecMessage(True, WM_GETINFO, 10, stProcessing);
    { -------------------------------------------------------------------------------------------------------------------------------------------------- OPEN }
    if (FMode = adOpenAll) or (FMode = adOpenForUser) then
    begin
      if Read then LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Address Book has been opened successfully.')
      else
      begin
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot open Address Book.');
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Read function of Address Book has failed. Please contact IT support.')));
      end;
    end;
    { ---------------------------------------------------------------------------------------------------------------------------------------------- SAVE NEW }
    if FMode = adSaveNew then
    begin
      if Write then
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: The Address Book has been saved successfully.')
          else
            LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot save to Address Book.');
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
    MainForm.ExecMessage(True, WM_GETINFO, 10, stReady);
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
  DataTables:=TDataTables.Create(MainForm.FDbConnect);
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
    if FMode = adOpenForUser then DataTables.CustFilter:=TAddressBook.USER_ALIAS + EQUAL + QuotedStr(MainForm.FUserName);
    DataTables.OpenTable(TblAddressbook);
    Result:=DataTables.SqlToGrid(FGrid, DataTables.DataSet, True, True);
  finally
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
  DataTables:=TDataTables.Create(MainForm.FDbConnect);
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
        DataTables.StrSQL:=DataTables.GridToSql(FGrid, TblAddressbook, DataTables.ColumnsToList, Start, 1);
        if not (DataTables.ExecSQL = nil) then
        begin
          { RE-DO LIST POSITION }
          for iCNT:=1 to FGrid.RowCount - 1 do FGrid.Cells[0, iCNT]:= IntToStr(iCNT);
          Result:=True;
        end;
      except
        on E: Exception do
        begin
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThd) + ']: Cannot add new record(s). Error has been thrown: ' + E.Message + '.');
          SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('Cannot add new record(s). Please contact IT support.')));
        end;
      end;
    end
    else
    begin
      SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('No new records have beed found. Process has been stopped.')));
    end;
  finally
    FGrid.Freeze(False);
    DataTables.Free;
  end;
end;

{ ############################################################## ! EXPORT TO EXCEL ! ######################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TTExcelExport.Create;
begin
  inherited Create(False);
  FLock:=TCriticalSection.Create;
  FIDThd:=0;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTExcelExport.Destroy;
begin
  FreeAndNil(FLock);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- EXPORT AGE VIEW TO EXCEL }
procedure TTExcelExport.Execute;
var
  FileName:  string;
  Temp:      TStringGrid;
begin
  FIDThd:=TTExcelExport.CurrentThread.ThreadID;
  FLock.Acquire;
  try
    MainForm.ExecMessage(True, WM_GETINFO, 10, stExportXLS);
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
    MainForm.ExecMessage(True, WM_GETINFO, 10, stReady);
  end;
  FreeOnTerminate:=True;
end;

end.
