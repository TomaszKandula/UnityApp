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
unit Transactions;

interface

uses
  Main, Model, StrUtils, SysUtils, StdCtrls, Classes, Windows, Messages, ADODB;

{ ------------------------------------------------------------- ! OPEN ITEMS CLASS ! ------------------------------------------------------------------------ }
type                                                  (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TTransactions = class(TDataTables)
  {$TYPEINFO ON}
  private
    { EMPTY }
  public
    { EMPTY }
  published
    function  GetDateTime:  TDateTime;
    function  LoadToGrid(DestGrid: TStringGrid; SettingGrid: TStringGrid):  boolean;
    function  ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
    procedure ClearSummary;
    procedure UpdateSummary;
  end;

implementation

uses
  Settings;

{ ############################################################## ! OPEN ITEMS CLASS ! ####################################################################### }

{ --------------------------------------------------------------------------------------------------------------------------------- GET CURRENT DATE AND TIME }
function TTransactions.GetDateTime: TDateTime;
begin
  Result:=StrToDate('2018-03-11');
  //
end;

{ ----------------------------------------------------------------------------------------------------------------------------- LOAD OPEN ITEMS FROM DATABASE }
function TTransactions.LoadToGrid(DestGrid: TStringGrid; SettingGrid: TStringGrid): boolean;
var
  DataTables:  TDataTables;
  AppSettings: TSettings;
  CutOff:      string;
  INF4:        string;
  Agents:      string;
  SortPos:     integer;
  iCNT:        integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  AppSettings:=TSettings.Create;
  try
    { PARAMETERS FOR SQL PROCEDURE }
    CutOff :=IntToStr(AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFNUM', 0));
    INF4   :=AppSettings.TMIG.ReadString(OpenItemsData, 'TXCUTOFFTXT', '');
    { FOR STRING GRID SORTING }
    SortPos:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'SORTPOS', 0);
  finally
    AppSettings.Free;
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------------- AGENT ON/OFF }

  (* WARNING! "SettingGrid" HAS FIXED DIMENSIONS *)

  for iCNT:=0 to 3 do
  begin
    if SettingGrid.Cells[iCNT, 3] = 'OFF' then Agents:='N';
    if SettingGrid.Cells[iCNT, 3] = 'ON'  then Agents:='Y';
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------- EXECUTE STORED SQL }
  DataTables:=TDataTables.Create(MainForm.FDbConnect);
  try

    (* WARNING! DO NOT USE "cmdStoredProc" TO EXECUTE STORED PROCEDURE WITH ADODB *)
    (*          USE ORDINARY "cmdText" WITH "EXEC" STATEMENT JUST LIKE YOU WOULD  *)
    (*          USE IT IN MICROSOFT MANAGEMENT STUDIO. ALTERNATIVELY, USE FIREDAC *)
    (*          FROM EMBARCADERO INSTEAD OF ADODB AS IT IS MORE ROBUST LIBRARY    *)

    DataTables.CmdType:=cmdText;
    DataTables.StrSQL:=EXEC + QueryOpenItems                                   + SPACE +
                       QuotedStr('2018-03-11')                                 + COMMA +
                       QuotedStr(ConvertName(SettingGrid.Cells[0, 0], 'F', 0)) + COMMA +
                       QuotedStr(ConvertName(SettingGrid.Cells[1, 0], 'F', 0)) + COMMA +
                       QuotedStr(ConvertName(SettingGrid.Cells[2, 0], 'F', 0)) + COMMA +
                       QuotedStr(ConvertName(SettingGrid.Cells[3, 0], 'F', 0)) + COMMA +
                       QuotedStr(CutOff)                                       + COMMA +
                       QuotedStr(Agents)                                       + COMMA +
                       QuotedStr(INF4);
    DataTables.ExecSQL;
    Result:=DataTables.SqlToGrid(DestGrid, DataTables.ExecSQL, False, False);
    { ----------------------------------------------------------------------------------------------------------------------------------------- SORT VIA CUID }
    DestGrid.MSort(SortPos, 2, True);
  finally
    DataTables.Free;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------- CLEAR ALL SUMMARY DETAILS }
procedure TTransactions.ClearSummary;
begin
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
end;

{ ---------------------------------------------------------------------------------------------------------------------- DISPLAY UPDATED SUMMARY FOR THE USER }
procedure TTransactions.UpdateSummary;
begin
  { COMPUTE }

  //

  { DISPLAY }
  MainForm.tcOpenItems.Caption     :=FormatFloat('### ###',  MainForm.sgOpenItems.RowCount - 1);
(*
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
*)
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- CO CODE NAME CONVERTION }
function TTransactions.ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
var
  iCNT:  integer;
begin
  { INITIALIZE }
  Result:= '';

  (* USED ONLY FOR OPEN ITEMS AND AGING VIEW  *)

  { ALLOW TO CONVERT '2020' TO 'F2020', ETC. }
  if mode = 0 then
  begin
    if Length(CoNumber) = 4 then Result:=Prefix + CoNumber;
    if Length(CoNumber) = 3 then Result:=Prefix + '0'  + CoNumber;
    if Length(CoNumber) = 2 then Result:=Prefix + '00' + CoNumber;
  end;

  (* USED ONLY TO BUILD GROUP_ID            *)

  { CONVERTS FROM     }
  {  1. 2020 TO 02020 }
  {  2. 340  TO 00340 }
  {  3. 43   TO 00043 }
  {  4. 0    TO 00000 }
  if mode = 1 then
  begin
    if Length(CoNumber) = 4 then Result:='0'   + CoNumber;
    if Length(CoNumber) = 3 then Result:='00'  + CoNumber;
    if Length(CoNumber) = 2 then Result:='000' + CoNumber;
    if Length(CoNumber) = 1 then Result:='00000';
  end;

  { CONVERTS FROM 02020 TO 2020 }
  if mode = 2 then
  begin
    for iCNT:= 1 to Length(CoNumber) do
    begin
      if CoNumber[iCNT] <> '0' then
      begin
        Result:=Copy(CoNumber, iCNT, MaxInt);
        Exit;
      end;
    end;
  end;
end;

end.
