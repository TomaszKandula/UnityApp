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
  Main, Windows, Messages, SysUtils, Classes, Diagnostics, Graphics, ADODB, ComObj;

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

(*
type
  TAddressBook = class //remove!!!
  {$TYPEINFO ON}
  private
    pidThd      : integer;
    pDelimiter  : string;
    pUserName   : string;
  public
    property    idThd      : integer read pidThd     write pidThd;
    property    Delimiter  : string  read pDelimiter write pDelimiter;
    property    UserName   : string  read pUserName  write pUserName;
  published
    constructor Create(UseDelimiter: string; UserNameDef: string);
  end;
*)

type
  TTAddressBook = class(TThread)
  protected
    procedure Execute; override;
  private
    pMode:  string;
    pUser:  string;
  public
    constructor Create(cMode: string; cUser: string);
    function    Read     : boolean;
    function    Write    : boolean;
    function    ImportCSV: boolean;
    function    ExportCSV: boolean;
  end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- EXPORT TO EXCEL }
type
  TTExcelExport = class(TThread)
    protected
      procedure Execute; override;
  end;


  ///below array to be removed!!!

{ ---------------------------------------------------------------------------------------------------------------------- ACCESSIBLE VARIABLES FOR OTHER UNITS }
var
  { THREADS ORDER IN ARRAY:        }
  {   0. CHECK SERVER CONNECTION   }
  {   1. INVOICE TRACKER SCANNER   }
  {   2. OPEN ITEMS SCANNER        }
  {   3. READ AGE VIEW             }
  {   4. MAKE AGE VIEW             }
  {   5. TRACKER LIST REFRESH      }
  {   6. SAVE COMMENTARY           }    //temporary off
  {   7. READ OPEN ITEMS           }
  {   8. ADDRESS BOOK              }
  {   9. EXPORT TO EXCEL           }
  ActiveThreads: array[0..9] of boolean = (False, False, False, False, False, False, False, False, False, False);

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  DataBase, Settings, UAC, Mailer, AgeView, Transactions;

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
  ActiveThreads[2]:=True;

  { IF ALL FILES HAS BEEN UPDATED, THEN EXECUTE SEPARATE THREAD FOR OPEN ITEMS LOAD }

(*
  if OpenItems.Scan(1) then
  begin
    try
      try
        TTReadOpenItems.Create('1');

      except
        on E: Exception do
        begin
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Cannot scan open items source. Please contact IT support.')));
          LogText(MainForm.EventLogPath, 'Thread [' + IntToStr(IDThread) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TOIScan).');
          SendMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
        end;
      end;

    finally
      ActiveThreads[2]:=False;
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: TOIThread method called with paremeter = 1.');
    end;
  end
    else
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(IDThread) + ']: No changes in open items have been found.');

*)
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
  UserControl: TUserControl;
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

{ ########################################################## ! ADRESSBOOK MAILER CLASS ! #################################################################### }

{ ---------------------------------------------------------------------------------------------------------------------------------------------- ADDRESS BOOK }
constructor TTAddressBook.Create(cMode: string; cUser: string);
begin
  inherited Create(False);
  pMode:=cMode;
  pUser:=cUser;
end;

procedure TTAddressBook.Execute;  (* ASYNC & SYNC *)
var
  DataBase:    TDataBase;
begin
  AddressBook:=TAddressBook.Create('|', pUser);
  AddressBook.idThd:=TTAddressBook.CurrentThread.ThreadID;
  ActiveThreads[8]:=True;
  DataBase:=TDataBase.Create(False);
  if DataBase.Check = 0 then
  begin
    try
      PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Processing..., please wait.')));

      (* HEAVY DUTY TASK MUST BE ALWAYS RUN BETWEEN 'PRE-' AND 'POST-' GUI UPDATE *)

      try
        { -------------------------------------------------------- ! PRE | GUI UPDATE ! ----------------------------------------------------------------------- }
        Synchronize
          (procedure begin
            with MainForm.sgAddressBook do SendMessage(Handle, WM_SETREDRAW, 0, 0);
          end);

        { --------------------------------------------------- ! HEAVY DUTY TASK (RUN ASYNC) ! ----------------------------------------------------------------- }

        if pMode = '0' then AddressBook.Write;
        if pMode = '1' then AddressBook.Read;
        if pMode = '2' then AddressBook.ImportCSV;
        if pMode = '3' then AddressBook.ExportCSV;

      except
        on E: Exception do
        begin
          SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PCHAR('Read/Write function of Address Book failed. Please contact IT support.')));
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(AddressBook.idThd) + ']: Execution of this tread work has been stopped. Error thrown: ' + E.Message + ' (TABThread).');
        end;
      end;

    finally
      { --------------------------------------------------------- ! POST | GUI UPDATE ! --------------------------------------------------------------------- }
      Synchronize
        (procedure begin
          with MainForm.sgAddressBook do SendMessage(Handle, WM_SETREDRAW, 1, 0);
          MainForm.sgAddressBook.Repaint;
          MainForm.sgAddressBook.SetColWidth(30, 25);
        end);
      AddressBook.Free;
      ActiveThreads[8]:=False;
      PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
    end;
  end;

  { RELEASE THREAD WHEN DONE }
  FreeOnTerminate:=True;
  FreeAndNil(DataBase);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- CONSTRUCTOR }
constructor TAddressBook.Create(UseDelimiter: string; UserNameDef: string);
begin
  (* INITIALIZE WITH GIVEN PARAMETERS *)
  pDelimiter:=UseDelimiter;
  pUserName :=UserNameDef;
  pidThd    :=0;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------------ READ }
function TAddressBook.Read: boolean;
var
  Columns:  string;
  StrSQL:   string;
  MSSQL:    TMSSQL;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  MSSQL:=TMSSQL.Create(MainForm.FDbConnect);
  { ------------------------------------------------------------------------------------------------------------------------------------------- COLUMNS SETUP }
  Columns:='USER_ALIAS,'     +
           'CUID,'           +
           'CUSTNUMBER,'     +
           'CUSTNAME,'       +
           'EMAILS      ,'   +
           'ESTATEMENTS ,'   +
           'TELEPHONE   ,'   +
           'CONTACT     ,'   +
           'CUSTADDR    '    ;
  { ----------------------------------------------------------------------------------------------------------------------------------------------- SQL QUERY }
  if UserName = '' then
    StrSQL:='SELECT ' + Columns + ' FROM tbl_addressbook ORDER BY ID ASC'
      else
        StrSQL:='SELECT ' + Columns + ' FROM tbl_addressbook WHERE USER_ALIAS = ' + QuotedStr(UserName) + ' ORDER BY ID ASC';
  { ------------------------------------------------------------------------------------------------------------------------------------- EXECUTE SQL COMMAND }
  try
    MSSQL.StrSQL:=StrSQL;
    Result:=MSSQL.SqlToGrid(MainForm.sgAddressBook, MSSQL.ExecSQL, True);
  finally
    { -------------------------------------------------------------------------------------------------------------------------- EVENT LOG AND MESSAGE WINDOW }
    if not (Result) then
    begin
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Unexpected error. Cannot open Address Book.');
      SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('Cannot open Address Book for user "' + UserName + '". Please contact IT support.')));
    end;
    MSSQL.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- WRITE }
function TAddressBook.Write: boolean;
var
  Columns:  string;
  MSSQL:    TMSSQL;
  iCNT:     integer;
  Start:    integer;
  RS:       _Recordset;
  TheSame:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result :=False;
  Start  :=0;
  TheSame:=0;
  MSSQL  :=TMSSQL.Create(MainForm.FDbConnect);
  { ------------------------------------------------------------------------------------------------------------------------------------------------- COLUMNS }

  (* NOTE: COLUMN ORDER MUST BE THE SAME AS IN GIVEN STRING GRID *)

  Columns:='USER_ALIAS,' +
           'CUID,'       +
           'CUSTNUMBER,' +
           'CUSTNAME,'   +
           'EMAILS,'     +
           'ESTATEMENTS,'+
           'TELEPHONE,'  +
           'CONTACT,'    +
           'CUSTADDR'    ;
  { ----------------------------------------------------------------------------------------------------------------- PERFORM INSERT ON NEWLY ADDED ROWS ONLY }
  for iCNT:=1 to MainForm.sgAddressBook.RowCount - 1 do
    if MainForm.sgAddressBook.Cells[0, iCNT] = '' then
    begin
      Start:=iCNT;
      Break;
    end;
  { -------------------------------------------------------------------------------------------------------------------------- MAKE SURE THAT NEW ROWS EXISTS }
  if not (Start = 0) then begin
    { ---------------------------------------------------------------------------------------------------------------------------- CHECK IF CUID ALREADY EXISTS }
    for iCNT:=Start to MainForm.sgAddressBook.RowCount - 1 do
    begin
      MSSQL.StrSQL:='SELECT CUID FROM tbl_AddressBook WHERE CUID = ' + MSSQL.CleanStr(MainForm.sgAddressBook.Cells[2, iCNT], True);
      RS:=MSSQL.ExecSQL;
      if RS.RecordCount > 0 then Inc(TheSame);
    end;
    { ----------------------------------------------------------------------------------------------------------------------------------- PROCESS BUILT QUERY }
    if TheSame = 0 then
    begin
      try
        MSSQL.ClearSQL;
        MSSQL.StrSQL:=MSSQL.GridToSql(MainForm.sgAddressBook, 'tbl_AddressBook', Columns, Start, 1);
        { RE-DO LIST POSITION }
        for iCNT:=1 to MainForm.sgAddressBook.RowCount - 1 do MainForm.sgAddressBook.Cells[0, iCNT]:= IntToStr(iCNT);
        if not (MSSQL.ExecSQL = nil) then Result:=True;
      finally
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: New records have beed successfully added to Address Book.');
        SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('New records have beed saved successfully!')));
        MSSQL.Free;
      end;
    end
      else
        SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('Cannot add already existing customers.' + #13#10 + 'Please re-check and remove doubles before next attempt.')));
  end
    else
      SendMessage(MainForm.Handle, WM_GETINFO, 2, LPARAM(PChar('No new records have beed found. Process has been stopped.')));
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- IMPORT }
function TAddressBook.ImportCSV: boolean;
var
  iCNT:       integer;
  jCNT:       integer;
  Count:      integer;
  Data:       TStringList;
  Transit:    TStringList;
  fPath:      string;
  IsError:    boolean;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  IsError:=False;
  Count:=0;
  { ----------------------------------------------------------------------------------------------------------------------------- GET THE FILE PATH AND PARSE }
  if MainForm.CSVImport.Execute = True then
  begin
    fPath:=MainForm.CSVImport.FileName;
    Data:=TStringList.Create;
    Transit:=TStringList.Create;
    try
      { LOAD DATA }
      Data.LoadFromFile(fPath);
      { COUNT ALL COLUMNS }
      for iCNT:=0 to Length(Data[0]) do if copy(Data[0], iCNT, 1) = Delimiter then inc(Count);
      { COUNT ALL ROWS & SETUP OFFSET }
      MainForm.sgAddressBook.RowCount:=Data.Count + 1;
      { SETUP TRANSIT THAT WILL HOLD SPLIT LINE }
      Transit.StrictDelimiter:=True;
      Transit.Delimiter:=Delimiter[1];
      { ITERATE THROUGH ALL ROWS }
      try
        for iCNT:= 0 to Data.Count - 1 do
        begin
          { SPLIT STRING USING GIVEN DELIMITER }
          Transit.DelimitedText:=Data[iCNT];
          for jCNT:=1 to Count do MainForm.sgAddressBook.Cells[jCNT, iCNT + 1]:=Transit[jCNT - 1];
          MainForm.sgAddressBook.Cells[0, iCNT + 1]:=IntToStr((iCNT + 1));
          Transit.Clear;
        end;
        Result:=True;
      { ---------------------------------------------------------------------------------------------------------------------------------------- ON EXCEPTION }
      except
        on E: Exception do
        begin
          LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: CSV Import faild: ' + ExtractFileName(fPath));
          SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('CSV Import faild. Please check the file and try again.')));
          IsError:=True;
        end;
      end;
    { ----------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
    finally
      PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
      if not IsError then
      begin
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Data has been imported successfully!');
        SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Data has been imported successfully!')));
      end;
      Data.Free;
      Transit.Free;
    end;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- EXPORT }
function TAddressBook.ExportCSV: boolean;
var
  iCNT:       integer;
  jCNT:       integer;
  fPath:      string;
  CSVData:    TStringList;
  MyStr:      string;
  CleanStr:   string;
  IsError:    boolean;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  IsError:=False;
  CSVData:=TStringList.Create;
  { ------------------------------------------------------------------------------------------------------------------------------------------ WRITE CSV FILE }
  try
    { ADD ROWS AND COLUMNS WITH DELIMITER }
    for iCNT:=1 to MainForm.sgAddressBook.RowCount - 1 do
    begin
      for jCNT:= 1 to MainForm.sgAddressBook.ColCount - 1 do
      begin
        CleanStr :=MainForm.sgAddressBook.Cells[jCNT, iCNT];
        CleanStr :=StringReplace(CleanStr, #13#10, ' ', [rfReplaceAll]);
        MyStr    :=MyStr + CleanStr + Delimiter;
      end;
      CSVData.Add(MyStr);
      MyStr:='';
    end;
    { SAVE TO FILE AS PLAIN TEXT }
    try
      if MainForm.CSVExport.Execute = True then CSVData.SaveToFile(MainForm.CSVExport.FileName);
      Result:=True;
    { ------------------------------------------------------------------------------------------------------------------------------------------ ON EXCEPTION }
    except
      on E: Exception do
      begin
        LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot saved file: ' + ExtractFileName(fPath));
        SendMessage(MainForm.Handle, WM_GETINFO, 3, LPARAM(PChar('Cannot save the file in the given location.')));
        IsError:=True;
      end;
    end;
  { ------------------------------------------------------------------------------------------------------------------------------------- RELEASE FROM MEMORY }
  finally
    if not IsError then
    begin
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Data has been exported successfully!');
      SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Address Book have been exported successfully!')));
    end;
    CSVData.Free;
  end;
end;







{ ---------------------------------------------------------------------------------------------------------------------------------- EXPORT AGE VIEW TO EXCEL }
procedure TTExcelExport.Execute;  (* ASYNC *)
var
  IDThread:  integer;
  FileName:  string;
  Temp:      TStringGrid;
begin
  IDThread:=TTCheckServerConnection.CurrentThread.ThreadID;
  ActiveThreads[9]:=True;
  try
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Exporting to Excel..., please wait.')));

    Synchronize(procedure begin
      if MainForm.XLExport.Execute then FileName:=MainForm.XLExport.FileName;
    end);

    Temp:=TStringGrid.Create(nil);
    try
      Temp.ToExcel('Sheet1', FileName, IDThread);
    finally
      Temp.Free;
    end;

  finally
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
  end;
  ActiveThreads[9]:=False;
  FreeOnTerminate:=True;
end;

end.
