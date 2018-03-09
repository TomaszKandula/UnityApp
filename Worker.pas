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

{ --------------------------------------------------------------------------------------------------------------------------------------------- READ AGE VIEW }
type
  TTReadAgeView = class(TThread)
  protected
    procedure Execute; override;    //remove modes!!
  private
    pMode: string;
  public
    { MODES:                                    }
    {   '0' = LEAVE OPEN ITEMS AS IT IS         }
    {   '1' = READ OPEN ITEMS                   }
    constructor Create(cMode: string);
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

{ ------------------------------------------------------------------------------------------------------------------------------------------- READ OPEN ITEMS }
type
  TTReadOpenItems = class(TThread)
  protected
    procedure Execute; override; //remove modes!!
  private
    pMode: string;
  public
    { MODES:                                    }
    {   '0' = JUST OPEN ITEMS REFRESH           }
    {   '1' = START AGEING REPORT               }
    constructor Create(cMode: string);
  end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK }
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
  public
    constructor Create;
    destructor  Destroy; override;
  end;


///below array to be removed!!!

{ ---------------------------------------------------------------------------------------------------------------------- ACCESSIBLE VARIABLES FOR OTHER UNITS }
var
  ActiveThreads: array[0..9] of boolean = (False, False, False, False, False, False, False, False, False, False);

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
    ActiveThreads[0]:=True;
    DataBase.InitializeConnection(IDThread, False, MainForm.FDbConnect);
    ActiveThreads[0]:=False;
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
  ActiveThreads[1]:=True;
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
    ActiveThreads[1]:=False;
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
var
  IDThread:  integer;
begin
  IDThread:=TTOpenItemsScanner.CurrentThread.ThreadID;

  //...

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- READ AGE VIEW }
constructor TTReadAgeView.Create(cMode: string);
begin
  inherited Create(False);
  pMode:=cMode;
end;

Procedure TTReadAgeView.Execute;  (* ASYNC & SYNC *)
var
  IDThread:   integer;
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  DataBase: TDataBase;
  AgeView:  TAgeView;
begin
  IDThread:=TTReadAgeView.CurrentThread.ThreadID;
  ActiveThreads[3]:=True;
  DataBase:=TDataBase.Create(False);

  try
    StopWatch:=TStopWatch.StartNew;

    (* HEAVY DUTY TASK MUST BE ALWAYS RUN BETWEEN 'PRE-' AND 'POST-' GUI UPDATE *)

    try

      { ------------------------------------------------------- ! PRE | GUI UPDATE ! ------------------------------------------------------------------------ }
      Synchronize
        (procedure begin
          { -------------------------------------------------------------------------------------------------------------------------- CLEAR OPEN ITEMS BOXES }

          { RESET OPEN ITEMS DETAIS }
(*
          MainForm.COC1.Text:='0';
          MainForm.COC2.Text:='0';
          MainForm.COC3.Text:='0';
          MainForm.COC4.Text:='0';
          MainForm.CUR1.Text:='N/A';
          MainForm.CUR2.Text:='N/A';
          MainForm.CUR3.Text:='N/A';
          MainForm.CUR4.Text:='N/A';
          MainForm.INT1.Text:='0';
          MainForm.INT2.Text:='0';
          MainForm.INT3.Text:='0';
          MainForm.INT4.Text:='0';
          MainForm.AGT1.Text:='N/A';
          MainForm.AGT2.Text:='N/A';
          MainForm.AGT3.Text:='N/A';
          MainForm.AGT4.Text:='N/A';
*)
          { --------------------------------------------------------------------------------------------------------------------------------- DISPLAY MESSAGE }
          MainForm.StatBar_TXT1.Caption :='Loading aging view..., please wait.';

          { ----------------------------------------------------------------------------------------------------------------------------------- CLEAR DETAILS }

          { TOP }
          MainForm.tcCOCODE.Caption     :='n/a';
          MainForm.tcCURRENCY.Caption   :='n/a';
          MainForm.tcTOTAL.Caption      :='0';
          MainForm.tcNumCalls.Caption   :='0';
          MainForm.tcNumEmails.Caption  :='0';

          { TRADE RECEIVABLES SUMMARY }
          MainForm.valND.Caption        :='0';
          MainForm.valR1.Caption        :='0';
          MainForm.valR2.Caption        :='0';
          MainForm.valR3.Caption        :='0';
          MainForm.valR4.Caption        :='0';
          MainForm.valR5.Caption        :='0';
          MainForm.valR6.Caption        :='0';
          MainForm.valTAMT.Caption      :='0';

          { PERCENTAGE }
          MainForm.procND.Caption       :='0';
          MainForm.procR1.Caption       :='0';
          MainForm.procR2.Caption       :='0';
          MainForm.procR3.Caption       :='0';
          MainForm.procR4.Caption       :='0';
          MainForm.procR5.Caption       :='0';
          MainForm.procR6.Caption       :='0';

          { EXCEEDERS }
          MainForm.valEXCEEDERS.Caption :='0';
          MainForm.valTEXCEES.Caption   :='0';
          MainForm.valTLIMITS.Caption   :='0';

          { NOT DUE | PAST DUE | DEFAULTED }
          MainForm.valTND.Caption       :='0';
          MainForm.valPASTDUE.Caption   :='0';
          MainForm.valDEFAULTED.Caption :='0';

          { ----------------------------------------------------------------------------------------------------------------------------- TURN OFF GIVEN VCL }
          MainForm.GroupListBox.Enabled  :=False;
          MainForm.GroupListDates.Enabled:=False;
          MainForm.sgAgeView.Enabled     :=False;
        end);

      { -------------------------------------------------- ! HEAVY DUTY TASK (RUN ASYNC) ! ------------------------------------------------------------------ }
      AgeView:=TAgeView.Create;
      try
        AgeView.Read(MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 0], StrToDate(MainForm.GroupListDates.Text), IDThread);
      { ------------------------------------------------------- ! POST | GUI UPDATE ! ----------------------------------------------------------------------- }
      Synchronize
        (procedure begin

          { FIND CO CODE, CURRENCY CODE, INTEREST RATE AND AGENT }
          AgeView.Details(MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 0], StrToDate(MainForm.GroupListDates.Text), IDThread);

          { -------------------------------------------------------------------------------------------------------------------------- TOP | AGE VIEW DETAILS }
          MainForm.tcTOTAL.Caption    :=IntToStr(AgeView.CustAll);
          MainForm.tcNumCalls.Caption :=IntToStr(AgeView.CallsAll);
          MainForm.tcNumEmails.Caption:=IntToStr(AgeView.EmailsAll);

          { -------------------------------------------------------------------------------------------------------------- BOTTOM | TRADE RECEIVABLES SUMMARY }

          { VALUES }
          MainForm.valND.Caption     :=FormatFloat('#,##0.00', AgeView.NotDue);
          MainForm.valR1.Caption     :=FormatFloat('#,##0.00', AgeView.Range1);
          MainForm.valR2.Caption     :=FormatFloat('#,##0.00', AgeView.Range2);
          MainForm.valR3.Caption     :=FormatFloat('#,##0.00', AgeView.Range3);
          MainForm.valR4.Caption     :=FormatFloat('#,##0.00', AgeView.Range4);
          MainForm.valR5.Caption     :=FormatFloat('#,##0.00', AgeView.Range5);
          MainForm.valR6.Caption     :=FormatFloat('#,##0.00', AgeView.Range6);
          MainForm.valTAMT.Caption   :=FormatFloat('#,##0.00', AgeView.Balance);

          { PERCENTAGE }
          MainForm.procND.Caption    :=FormatFloat('0.00', ( (AgeView.NotDue / AgeView.Balance) * 100 )) + '%';
          MainForm.procR1.Caption    :=FormatFloat('0.00', ( (AgeView.Range1 / AgeView.Balance) * 100 )) + '%';
          MainForm.procR2.Caption    :=FormatFloat('0.00', ( (AgeView.Range2 / AgeView.Balance) * 100 )) + '%';
          MainForm.procR3.Caption    :=FormatFloat('0.00', ( (AgeView.Range3 / AgeView.Balance) * 100 )) + '%';
          MainForm.procR4.Caption    :=FormatFloat('0.00', ( (AgeView.Range4 / AgeView.Balance) * 100 )) + '%';
          MainForm.procR5.Caption    :=FormatFloat('0.00', ( (AgeView.Range5 / AgeView.Balance) * 100 )) + '%';
          MainForm.procR6.Caption    :=FormatFloat('0.00', ( (AgeView.Range6 / AgeView.Balance) * 100 )) + '%';
          MainForm.procTAMT.Caption  :=FormatFloat('0.00', ( ( (AgeView.NotDue / AgeView.Balance) +
                                                               (AgeView.Range1 / AgeView.Balance) +
                                                               (AgeView.Range2 / AgeView.Balance) +
                                                               (AgeView.Range3 / AgeView.Balance) +
                                                               (AgeView.Range4 / AgeView.Balance) +
                                                               (AgeView.Range5 / AgeView.Balance) +
                                                               (AgeView.Range6 / AgeView.Balance) ) * 100 ) ) + '%';

          { ------------------------------------------------------------------------------------------------------------------------------------ RICK CLASSES }
          MainForm.valRISKA.Caption:=FormatFloat('#,##0.00', AgeView.RCA);
          MainForm.valRISKB.Caption:=FormatFloat('#,##0.00', AgeView.RCB);
          MainForm.valRISKC.Caption:=FormatFloat('#,##0.00', AgeView.RCC);

          { ------------------------------------------------------------------------------------------------------------------------------ BOTTOM | EXCEEDERS }
          MainForm.valEXCEEDERS.Caption :=IntToStr(AgeView.Exceeders);
          MainForm.valTEXCEES.Caption   :=FormatFloat('#,##0.00', AgeView.TotalExceed);
          MainForm.valTLIMITS.Caption   :=FormatFloat('#,##0.00', AgeView.Limits);

          { --------------------------------------------------------------------------------------------------------- BOTTOM | NOT DUE | PAST DUE | DEFAULTED }
          MainForm.valTND.Caption       :=MainForm.valND.Caption;
          MainForm.valPASTDUE.Caption   :=FormatFloat('#,##0.00', (AgeView.Range1 + AgeView.Range2 + AgeView.Range3));
          MainForm.valDEFAULTED.Caption :=FormatFloat('#,##0.00', (AgeView.Range4 + AgeView.Range5 + AgeView.Range6));

          { ----------------------------------------------------------------------------------------------------------------------------------------- MESSAGE }
          MainForm.StatBar_TXT1.Caption:='Ready.';

        end);

      finally
        AgeView.Free;
      end;

    except
      on E: Exception do
      begin
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Cannot execute ''TTReadAgeView''. Please contact IT support.')));
        PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TReadFromDB).');
      end;
    end;

  finally

    Synchronize(procedure begin
      { TURN ON COMPONENTS }
      MainForm.GroupListBox.Enabled  :=True;
      MainForm.sgAgeView.Enabled     :=True;
      if (MainForm.GroupListDates.Text <> '') and (MainForm.AccessLevel = 'AD')
        then MainForm.GroupListDates.Enabled:=True
          else MainForm.GroupListDates.Enabled:=False;
    end);

    ActiveThreads[3]:=False;
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Thread for selected group "' + MainForm.FGroupList[MainForm.GroupListBox.ItemIndex, 1] + '" has been executed within ' +
                     FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
  end;

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  DataBase.Free;

  { REFRESH OPEN ITEMS WITH ZERO ARGUMENT FOR NO 'AGE-MAKE'   }
  { DO IT ONLY IF USE CLICK RELOAD BUTTON                     }
  { PROGRAM REFRESH AGING ONLY IF NEW OPEN ITEMS ARE RELOADED }
  { THEREFORE, NO NEED TO RELOAD OPEN ITEMS AGAIN             }
  { WE WILL REFRESH OPEN ITEMS ONLY WHEN USER CHANGES GROUP   }
  if pMode = '1' then TTReadOpenItems.Create('0');
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
  ActiveThreads[4]:=True;
  DataBase:=TDataBase.Create(False);

  if DataBase.Check = 0 then
  begin
    try
      StopWatch:=TStopWatch.StartNew;
      try
        { ----------------------------------------------------------------------------------------------------------------------- MAKE AGING REPORT TO AN ARRAY }
        AgeView:=TAgeView.Create;
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
          TTReadAgeView.Create('0');
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
      ActiveThreads[4]:=False;
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
    ActiveThreads[5]:=True;

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
    ActiveThreads[5]:=False;
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

{ ------------------------------------------------------------------------------------------------------------------------------------------- READ OPEN ITEMS }
constructor TTReadOpenItems.Create(cMode: string);
begin
  inherited Create(False);
  pMode:=cMode;
end;

procedure TTReadOpenItems.Execute;  (* ASYNC & SYNC *)
var
  IDThread:   integer;
  THDMili:    extended;
  THDSec:     extended;
  StopWatch:  TStopWatch;
  AppSettings:  TSettings;

procedure test;
begin
  AppSettings:=TSettings.Create;
  AppSettings.Free;
end;

begin
  IDThread:=TTReadOpenItems.CurrentThread.ThreadID;
  ActiveThreads[7]:=True;

  test;

  try
    StopWatch:=TStopWatch.StartNew;

    (* HEAVY DUTY TASK MUST BE ALWAYS RUN BETWEEN 'PRE-' AND 'POST-' GUI UPDATE *)

    try
      { -------------------------------------------------------- ! PRE | GUI UPDATE ! ----------------------------------------------------------------------- }

      Synchronize
        (procedure begin
          { ---------------------------------------------------------------------------------------------------------------------------------- CLEAR CAPTIONS }
          MainForm.tcOpenItems.Caption     :='0';
          MainForm.tcOverdue.Caption       :='0';
          MainForm.tcInvoices.Caption      :='0';
          MainForm.tcDecAmt.Caption        :='0';
          MainForm.tcDisAmt.Caption        :='0';
          MainForm.tcRecovery.Caption      :='0';
          MainForm.tcOSAmt.Caption         :='0';
          MainForm.tcUNamt.Caption         :='0';
          MainForm.tcOvdAmt.Caption        :='0';
          MainForm.tcOverdueRatio.Caption  :='0';
          MainForm.tcKPIoverdue.Caption    :='0';
          MainForm.tcKPIunallocated.Caption:='0';
          { --------------------------------------------------------------------------------------------------------------------------------------- EVENT LOG }
//          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Exclude accounts above or equal ' + AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER' + IntToStr(AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFPOS', 0)), '') + ' number: ' + IntToStr(AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFNUM', 0)) + '.');
//          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Exclude accounts that contains "' + AppSettings.TMIG.ReadString(OpenItemsData, 'TXCUTOFFTXT', '') + '" in column ' + AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER' + IntToStr(AppSettings.TMIG.ReadInteger(OpenItemsData, 'TXCUTOFFPOS', 0)), '') + '.');
        end);

      { -------------------------------------------------- ! HEAVY DUTY TASK (RUN ASYNC) ! ------------------------------------------------------------------ }
//      OpenItems.Load(IDThread);
(*
      { ------------------------------------------------------- ! POST | GUI UPDATE ! ----------------------------------------------------------------------- }
      Synchronize
        (procedure begin
          if OpenItems.FileExist then
          begin
            { ---------------------------------------------------------------------------------------------------------------------------------- KPI READ OUT }
            if MainForm.COC1.Text <> '0' then OpenItems.KPI_overdue:=OpenItems.KPI_overdue + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC1.Text, 0);
            if MainForm.COC2.Text <> '0' then OpenItems.KPI_overdue:=OpenItems.KPI_overdue + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC2.Text, 0);
            if MainForm.COC3.Text <> '0' then OpenItems.KPI_overdue:=OpenItems.KPI_overdue + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC3.Text, 0);
            if MainForm.COC4.Text <> '0' then OpenItems.KPI_overdue:=OpenItems.KPI_overdue + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC4.Text, 0);
            if MainForm.COC1.Text <> '0' then OpenItems.KPI_unalloc:=OpenItems.KPI_unalloc + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC1.Text, 1);
            if MainForm.COC2.Text <> '0' then OpenItems.KPI_unalloc:=OpenItems.KPI_unalloc + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC2.Text, 1);
            if MainForm.COC3.Text <> '0' then OpenItems.KPI_unalloc:=OpenItems.KPI_unalloc + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC3.Text, 1);
            if MainForm.COC4.Text <> '0' then OpenItems.KPI_unalloc:=OpenItems.KPI_unalloc + OpenItems.ReturnKPI(MainForm.sgCoCodes, MainForm.COC4.Text, 1);
            { -------------------------------------------------------------------------------------------------------------------------------- DISPLAY VALUES }
            MainForm.tcOpenItems.Caption     :=FormatFloat('### ###',  MainForm.sgOpenItems.RowCount - 1);
            MainForm.tcInvoices.Caption      :=FormatFloat('### ###',  OpenItems.nInvoices);
            MainForm.tcOverdue.Caption       :=FormatFloat('### ###',  OpenItems.Overdue);
            MainForm.tcOverdueRatio.Caption  :=FormatFloat('0.00',     (( (OpenItems.Overdue / OpenItems.nInvoices) * 100 ))) + '%';
            MainForm.tcDisAmt.Caption        :=FormatFloat('#,##0.00', OpenItems.cDiscountedAmt);
            MainForm.tcDecAmt.Caption        :=FormatFloat('#,##0.00', OpenItems.cDecreaseAmt);
            MainForm.tcRecovery.Caption      :=FormatFloat('#,##0.00', OpenItems.cRecoveryAmt);
            MainForm.tcOSAmt.Caption         :=FormatFloat('#,##0.00', OpenItems.OSamt);
            MainForm.tcOvdAmt.Caption        :=FormatFloat('#,##0.00', OpenItems.OverdueAmt);
            MainForm.tcUNAmt.Caption         :=FormatFloat('#,##0.00', abs(OpenItems.UNamt));
            MainForm.tcKPIoverdue.Caption    :=FormatFloat('#,##0.00', OpenItems.KPI_overdue);
            MainForm.tcKPIunallocated.Caption:=FormatFloat('#,##0.00', OpenItems.KPI_unalloc);
          end;
        end);
*)
    except
      on E: Exception do
      begin
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Cannot load open items. Please contact IT support.')));
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TOIThread).');
      end;
    end;

  finally
    ActiveThreads[7]:=False;
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
    THDMili:=StopWatch.ElapsedMilliseconds;
    THDSec:=THDMili / 1000;
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Open Items loading thread has been executed within ' +
                     FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
  end;

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
//  FreeAndNil(AppSettings);

  { SEND TO SQL SERVER }
(*
  if (pMode = '1') and (OpenItems.FileExist) then
  begin
    MainForm.cbDump.Checked:=False;
    TTMakeAgeView.Create(False);
  end;
*)
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
    SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR(stProcessing)));
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
      FGrid.OpenThdId:=FIDThd;
      FGrid.ImportCSV(MainForm.CSVImport, '|');
    end;
    { ------------------------------------------------------------------------------------------------------------------------------------------------ EXPORT }
    if FMode = adExport then
    begin
      FGrid.OpenThdId:=FIDThd;
      FGrid.ExportCSV(MainForm.CSVExport, '|');
    end;
  finally
    { ------------------------------------------------------------------------------------------------------------------------------------------- RELEASE ALL }
    SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR(stReady)));
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
    Result:=DataTables.SqlToGrid(FGrid, DataTables.DataSet, True);
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
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TTExcelExport.Destroy;
begin
  FreeAndNil(FLock);
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- EXPORT AGE VIEW TO EXCEL }
procedure TTExcelExport.Execute;
var
  IDThread:  integer;
  FileName:  string;
  Temp:      TStringGrid;
begin
  IDThread:=TTExcelExport.CurrentThread.ThreadID;
  try
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR(stExportXLS)));
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
      Temp.OpenThdId:=IDThread;
      Temp.ToExcel('Age Report', FileName);
    finally
      Temp.Free;
    end;
  finally
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR(stReady)));
  end;
  FreeOnTerminate:=True;
end;

end.
