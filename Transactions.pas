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
  Main, Model, StrUtils, SysUtils, StdCtrls, Classes, Windows, Messages;

{ ------------------------------------------------------------- ! OPEN ITEMS CLASS ! ------------------------------------------------------------------------ }
type                                                  (* RUN EITHER IN WORKER OR MAIN THREAD *)
  TTransactions = class(TDataTables)
  {$TYPEINFO ON}
  public

    //below to be removed

    OpenItemsDir  : string;                      { PATH TO OPEN ITEMS DIRECTORY                  }
    OpenItemsFor  : string;                      { FORMAT OF FILE WITH OPEN ITEMS                }
    FileExist     : boolean;                     { INDICATE IF SOURCE FILE FOR OPEN ITEMS EXISTS }
    ArrOpenItems  : array of array of string;    { INFORMATION ON SOURCES OF OPEN ITEMS          }

    nInvoices     : integer;
    Overdue       : integer;
    OSamt         : double;
    UNamt         : double;
    OverdueAmt    : double;
    cDiscountedAmt: double;
    cDecreaseAmt  : double;
    cRecoveryAmt  : double;
    KPI_overdue   : double;
    KPI_unalloc   : double;
  published

    //below to be removed
    procedure   Load(idThd: integer);            { RUN IN WORKER THREAD ONLY }
    function    ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
    function    ReturnKPI(SG: TStringGrid; StrCoCode: string; mode: integer): double;


    //new

    function  GetDateTime: TDateTime;
    function  LoadintoGrid: boolean;

  end;

implementation

uses
  Settings;

{ ############################################################## ! OPEN ITEMS CLASS ! ####################################################################### }










{ ------------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS LOAD }
procedure TTransactions.Load(idThd: integer);
(*
var
  { SETTINGS ACCESSIBLE FOR INNER AND MAIN BLOCK }
  AppSettings:  TSettings;

{ --------------------------------------------------------------- ! INNER METHODS ! ------------------------------------------------------------------------- }

{ --------------------------------------------------------------- ! CONVERT DATE ! -------------------------------------------------------------------------- }
function ConvDate(sDate: string): string;
{ INPUT FORMAT:  YYYYMMDD   }
{ OUTPUT FORMAT: YYYY-MM-DD }
begin
  Result:=LeftStr(sDate, 4) + '-' + MidStr(sDate, 5, 2) + '-' + RightStr(sDate, 2);
end;

{ -------------------------------------------------------------- ! CONVERT FLOAT ! -------------------------------------------------------------------------- }
function IsNumeric(str, mode: string): boolean;
begin
  try
    if mode = UpperCase('float')   then StrToFloat(str);
    if mode = UpperCase('integer') then StrToInt(str);
    Result:=True;
  except
    Result:=False;
  end;
end;

{ -------------------------------------------------------------- ! DAY CALCULATIONS ! ----------------------------------------------------------------------- }
function HowManyDays(refDate1, refDate2: string): string;
var
  diff: extended;
begin
  try
    diff:=StrToDateTime(refDate1) - StrToDateTime(refDate2);
    Result:=IntToStr(Round(diff));
  except
    Result:='N/A';
  end;
end;

{ ----------------------------------------------------------- ! LOOK FOR VOUCHER TYPE ! --------------------------------------------------------------------- }
function IsVoType(VoType: string): boolean;
var
  tsVAL:     TStringList;
  AppSettings: TSettings;
  iCNT:      integer;
begin
  Result:=False;
  tsVAL:=TStringList.Create;
  AppSettings:=TSettings.Create;
  try
    AppSettings.TMIG.ReadSectionValues(InvoiceTypes, tsVAL);
    for iCNT:=0 to tsVAL.Count - 1 do
      if VoType = MidStr(tsVAL.Strings[iCNT], AnsiPos('=', tsVAL.Strings[iCNT]) + 1, 255) then
      begin
        Result:=True;
        break;
      end;
  finally
    AppSettings.Free;
    tsVAL.Free;
  end;
end;

{ ------------------------------------------------------------- ! LOAD DATA FROM DB ! ----------------------------------------------------------------------- }
function LoadFromDB(COC: string; OffSet: integer; CoPos: integer): integer;
var
  { COUNTERS }
  iCNT:             integer;
  jCNT:             integer;
  Count:            integer;
  Inner:            integer;
  { CSV DATA AND DELIMITER }
  Delimiter:        Char;
  Data:             TStringList;
  Transit:          TStringList;
  { FILE }
  fSource, fPath:   string;
  { AGENT COLUMN NUMBER }
  AgentCol:         integer;
  { CUT-OFF }
  NrCutOffPos:      integer;             { WHAT COLUMN HOLDS THE VALUE TO BE CHECKED }
  NrCutOffNum:      integer;             { NUMERIC CONDITION                         }
  TxCutOffPos:      integer;             { WHAT COLUMN HOLDS THE VALUE TO BE CHECKED }
  TxCutOffTxt:      string;              { TEXT CONDITION                            }
begin
  { ----------------------------------------------------------------------------------------------------------------------------------------------- INITIAIZE }
  Result:=0;
  Count :=0;
  Inner :=1;
  FileExist:=True;
  { ------------------------------------------------------------------------------------------------------------- CHECK IF GIVEN COMPANY HAVE AGENT ON OR OFF }
  AgentCol:=-100;
  if (CoPos = 1) and (MainForm.AGT1.Text = 'OFF') then AgentCol:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'AGENTCOLUMN', 0);
  if (CoPos = 2) and (MainForm.AGT2.Text = 'OFF') then AgentCol:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'AGENTCOLUMN', 0);
  if (CoPos = 3) and (MainForm.AGT3.Text = 'OFF') then AgentCol:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'AGENTCOLUMN', 0);
  if (CoPos = 4) and (MainForm.AGT4.Text = 'OFF') then AgentCol:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'AGENTCOLUMN', 0);
  { ----------------------------------------------------------------------------------------------------------------------------- FILE PATH FOR GIVEN CO CODE }
  fSource:=ConvertName(COC, '', 0);
  fPath:=OpenItemsDir + OpenItemsFor;
  fPath:=StringReplace(fPath, '{NUM}', fSource, [rfReplaceAll]);
  { ----------------------------------------------------------------------------------------------------------------------------- CHECK IF SOURCE FILE EXISTS }
  if FileExists(fPath) = False then
  begin
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Cannot find ' + fSource + ' in database. Process halted automatically.');
    SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Cannot find ' + fSource + ' in database. Process halted automatically.')));
    FileExist:=False;
  end else
  begin
    { -------------------------------------------------------------------------------------------------------------- PROCESS THE SOURCE FILE | PARSE CSV DATA }
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Downloading company number ' + COC + '...');
    Data   :=TStringList.Create;
    Transit:=TStringList.Create;
    try
      { LOAD DATA }
      Data.LoadFromFile(fPath);
      { SETUP DELIMITER }
      Delimiter:=AppSettings.TMIG.ReadString(OpenItemsData, 'DELIMITER', ';')[1];
      { COUNT ALL COLUMNS }
      for iCNT:=0 to Length(Data[0]) do if copy(Data[0], iCNT, 1) = delimiter then inc(Count);
      Count:=Count + 1;
      { COUNT ALL ROWS & SETUP OFFSET }
      MainForm.sgOpenItems.RowCount:=Data.Count + OffSet;
      { SETUP TRANSIT THAT WILL HOLD SPLIT LINE }
      Transit.StrictDelimiter:=True;
      Transit.Delimiter:=Delimiter;
      { ASSIGN CONDITIONS }
      NrCutOffPos:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFPOS', 0);
      NrCutOffNum:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'NRCUTOFFNUM', 0);
      TxCutOffPos:=AppSettings.TMIG.ReadInteger(OpenItemsData, 'TXCUTOFFPOS', 0);
      TxCutOffTxt:=AppSettings.TMIG.ReadString(OpenItemsData,  'TXCUTOFFTXT', '');
      { ITERATE VIA ALL ROWS }
      for iCNT:= 1 to Data.Count - 1 do
      begin
        try
          { SPLIT STRING USING GIVEN DELIMITER }
          Transit.DelimitedText:=Data[iCNT];

          { CHECK IF THERE IS INVALID DUE DATE VALUE, IF SO PUT DEFAULT DATE 1901-01-01 }
          if Transit[11]='0' then Transit[11]:='19010101';

          { CONVERT DECIMAL SEPARATOR | COMMA TO POINT }
          if (AppSettings.TMIG.ReadString(OpenItemsData, 'DECIMAL_SEPARATOR', ',') = ',') and (FormatSettings.DecimalSeparator = '.') then
          begin
            Transit[4] :=StringReplace(Transit[4],  ',', '.', [rfReplaceAll]);
            Transit[5] :=StringReplace(Transit[5],  ',', '.', [rfReplaceAll]);
            Transit[8] :=StringReplace(Transit[8],  ',', '.', [rfReplaceAll]);
            Transit[9] :=StringReplace(Transit[9],  ',', '.', [rfReplaceAll]);
            Transit[14]:=StringReplace(Transit[14], ',', '.', [rfReplaceAll]);
          end;

          { CONVERT DECIMAL SEPARATOR | POINT TO COMMA }
          if (AppSettings.TMIG.ReadString(OpenItemsData, 'DECIMAL_SEPARATOR', ',') = '.') and (FormatSettings.DecimalSeparator = ',') then
          begin
            Transit[4] :=StringReplace(Transit[4],  '.', ',', [rfReplaceAll]);
            Transit[5] :=StringReplace(Transit[5],  '.', ',', [rfReplaceAll]);
            Transit[8] :=StringReplace(Transit[8],  '.', ',', [rfReplaceAll]);
            Transit[9] :=StringReplace(Transit[9],  '.', ',', [rfReplaceAll]);
            Transit[14]:=StringReplace(Transit[14], '.', ',', [rfReplaceAll]);
          end;

          { ALLOW ONLY ROW THAT MEET THE REQUIREMENTS FOUND IN COLUMN INICATED BY CUTOFFPOS }
          { WE HAVE TWO BUILT-IN CONDITIONS:                   }
          {   1. NRCUTOFF - NUMERIC CONDITION 'LESS OR EQUAL'  }
          {   2. TXCUTOFF - TEXT CONDITION    'DIFFERENT THAN' }
          if (StrToInt(Transit[NrCutOffPos - 1]) <= NrCutOffNum) and (UpperCase(Transit[TxCutOffPos - 1]) <> UpperCase(TxCutOffTxt)) then
          begin
            { PUT EACH COLUMN INTO STRING GRID FOR GIVEN ROW }
            for jCNT:=1 to Count do
            begin
              { MAKE AGENT COLUMN EMPTY IF 'AGENTCOL' IS POSITIVE }
              if AgentCol < 0 then MainForm.sgOpenItems.Cells[jCNT, Inner + OffSet]:=Transit[jCNT - 1];
              if AgentCol > 0 then
              begin
                if jCNT <> AgentCol then MainForm.sgOpenItems.Cells[jCNT, Inner + OffSet]:=Transit[jCNT - 1];
                if jCNT = AgentCol  then MainForm.sgOpenItems.Cells[jCNT, Inner + OffSet]:='';
              end;
            end;

            { UID COLUMN CONTAINS WITH 'CUSTOMER NUMBER' AND 'CO CODE' AND 'AGENT NUMBER'             }
            { THIS WILL EVENTUALLY LEAD TO LARGE NUMBER, EXTENDED TYPE SHOULD BE USED FOR THIS COLUMN }
            { IT ALLOWS TO HAVE ABSOLUTE UNIQUE NUMBER FOR GIVEN SNAPSHOT DAY                         }
            MainForm.sgOpenItems.Cells[38, Inner + OffSet]:=MainForm.sgOpenItems.Cells[2, Inner + OffSet] +
                                                 MidStr(MainForm.sgOpenItems.Cells[1, Inner + OffSet], 2, 5) +
                                                 (IntToStr(StrToIntDef(MainForm.sgOpenItems.Cells[19, Inner + OffSet], 0)));
            { MOVE ON }
            inc(Inner);
            { CLEAR IT }
            Transit.Clear;
          end;
        { THERE MAY BE A SITUATION WHEN CSV DATA IS BROKEN AND HAVE INCORRECT NUMBER OF DELIMITER IN SINGLE ROW }
        Except
          on E: Exception do
          begin
            { LOG ERROR AND THROW MESSAGE ON MAIN TAB }
            LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: The source file: ' + ExtractFileName(fPath) + ' (line number: ' + IntToStr(iCNT) + ') is broken.');
          end;
        end;
      end;
    finally
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Downloading company number ' + COC + '..., ' + IntToStr((Data.Count - 1)) + ' row(s) has been processed.');
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Downloading company number ' + COC + '..., ' + IntToStr((Inner - 1)) + ' row(s) has been affected.');
      Result:=Inner;
      MainForm.sgOpenItems.RowCount:=Inner + OffSet;
      Data.Free;
      Transit.Free;
    end;
  end;
end;

{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
var
  { COUNTERS }
  iCNT:             integer;
  TotalRows:        integer;
  { TIME DIFFERENCE }
  TimeDiff:         integer;
  { REFLECTS TIME VALUE OF MONEY: DISCOUNTED AMOUNT, DECREASE IN AMOUNT AND RECOVERY AMOUNT }
  DiscountedAmt:    double;
  DecreaseAmt:      double;
  RecoveryAmt:      double;
  { INTEREST RATE FOR GIVEN COMPANY CODE }
  InterestRate:     double;
  { AMOUNT OF OUTSTANDING INVOICES ONLY }
  InvoiceAmt:       double;
*)
begin
(*
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }

  AppSettings.Create;

  nInvoices     :=0;
  Overdue       :=0;
  OverdueAmt    :=0;
  OSamt         :=0;
  UNamt         :=0;
  cDiscountedAmt:=0;
  cDecreaseAmt  :=0;
  cRecoveryAmt  :=0;
  KPI_overdue   :=0;
  KPI_unalloc   :=0;
  InterestRate  :=0;
  TotalRows     :=0;
  { ----------------------------------------------------------------------------------------------- STOP IF THERE IS NO COMPANIES SELECTED, OTHERWISE EXECUTE }
  if StrToInt(MainForm.COC1.Text) + StrToInt(MainForm.COC2.Text) + StrToInt(MainForm.COC3.Text) + StrToInt(MainForm.COC4.Text)  <= 0 then
  begin
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Incorrect companies combination.');
    LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Downloading halted automatically.');
    SendMessage(MainForm.Handle, WM_GETINFO, 1, LPARAM(PChar('Incorrect companies combination. Downloading halted automatically.')));
  end else
  begin

    { -------------------------------------------------- ! SETTING-UP AND LOADING OPEN ITEMS ! -------------------------------------------------------------- }
                                                                    { MAIN BLOCK }

    { ----------------------------------------------------------------------------------------------------------------------------------------- CHANGE STATUS }
    PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Downloading Open Items...')));
    { ------------------------------------------------------------------------------------------------------------------------------ DO NOT DRAW 'STRINGGRID' }
    MainForm.sgOpenItems.Freeze(True);
    { ------------------------------------------------------------------------------------------------- LOAD ALL OPEN ITEMS INTO 'STRINGGRID'AND SORT VIA UID }
    if MainForm.COC1.Text > '0' then TotalRows:=LoadFromDB(MainForm.COC1.Text, 0, 1);
    if MainForm.COC2.Text > '0' then TotalRows:=TotalRows + LoadFromDB(MainForm.COC2.Text, MainForm.sgOpenItems.RowCount - 1, 2);
    if MainForm.COC3.Text > '0' then TotalRows:=TotalRows + LoadFromDB(MainForm.COC3.Text, MainForm.sgOpenItems.RowCount - 1, 3);
    if MainForm.COC4.Text > '0' then TotalRows:=TotalRows + LoadFromDB(MainForm.COC4.Text, MainForm.sgOpenItems.RowCount - 1, 4);
    { --------------------------------------------------------------------------------------------------------------------------- PROCEED IF OPEN ITEMS EXIST }
    if FileExist then
    begin
      { --------------------------------------------------------------------------------------------------------------------------------------------- SORT IT }
      MainForm.sgOpenItems.MSort(AppSettings.TMIG.ReadInteger(OpenItemsData, 'SORTPOS', 0), 2, True);

      { ---------------------------------------------------------------------------------------------------------------------------------------------- LOG IT }
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Open Items sorted by ''UID'' column (ascending).');
      LogText(MainForm.FEventLogPath, 'Thread [' + IntToStr(idThd) + ']: Open Items downloaded. ' + IntToStr(TotalRows) + ' row(s) have been affected in total.');

      { -------------------------------------------- ! LP, MAPPING, FORMATTING AND RE-CALCULATIONS ! -------------------------------------------------------- }
                                                            { MAIN LOOP THROUGH 'STRINGGRID' }

      for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
      begin

        { ------------------------------------------------------------------------------------------------------------------------------------------------ LP }
        MainForm.sgOpenItems.Cells[0, iCNT]:=IntToStr(iCNT);

        { --------------------------------------------------------------------------------------------- CHECK SEPARATOR AND ADD ZERO BEFORE DECIMAL SEPARATOR }
        { POSITIVE VALUES }
        if (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[5,  iCNT]:= '0' + MainForm.sgOpenItems.Cells[5,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[6,  iCNT]:= '0' + MainForm.sgOpenItems.Cells[6,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[9,  iCNT]:= '0' + MainForm.sgOpenItems.Cells[9,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[10, iCNT]:= '0' + MainForm.sgOpenItems.Cells[10, iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = ',') or (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = '.') then MainForm.sgOpenItems.Cells[15, iCNT]:= '0' + MainForm.sgOpenItems.Cells[15, iCNT];

        { NEGATIVE VALUES }
        if (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[5,  iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[5,  iCNT]:= '-0' + MainForm.sgOpenItems.Cells[5,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[6,  iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[6,  iCNT]:= '-0' + MainForm.sgOpenItems.Cells[6,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[9,  iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[9,  iCNT]:= '-0' + MainForm.sgOpenItems.Cells[9,  iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[10, iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[10, iCNT]:= '-0' + MainForm.sgOpenItems.Cells[10, iCNT];
        if (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = '-,') or (LeftStr(MainForm.sgOpenItems.Cells[15, iCNT], 1) = '-.') then MainForm.sgOpenItems.Cells[15, iCNT]:= '-0' + MainForm.sgOpenItems.Cells[15, iCNT];

        { ----------------------------------------------------------------------------------------------------------- DATE FORMAT FROM YYYYMMDD TO YYYY-MM-DD }
        MainForm.sgOpenItems.Cells[4,  iCNT]:=ConvDate(MainForm.sgOpenItems.Cells[4,  iCNT]);
        MainForm.sgOpenItems.Cells[12, iCNT]:=ConvDate(MainForm.sgOpenItems.Cells[12, iCNT]);
        MainForm.sgOpenItems.Cells[27, iCNT]:=ConvDate(MainForm.sgOpenItems.Cells[27, iCNT]);

        { ----------------------------------------------------------------------------------------------------------------------- PAYMENT STATUS CALCULATIONS }
        MainForm.sgOpenItems.Cells[34, iCNT]:=HowManyDays(MainForm.sgOpenItems.Cells[12, iCNT], DateTimeToStr(Now));
        TimeDiff:=StrToInt(HowManyDays(MainForm.sgOpenItems.Cells[12, iCNT], DateTimeToStr(Now)));

        { ------------------------------------------------------------------------ RE-CALCULATIONS FOR: DISCOUNTED AMOUNT, DECREASE IN AMOUNT, RECOVERY VALUE }
        InvoiceAmt:=StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT]);
        if MainForm.sgOpenItems.Cells[1, iCNT] = ConvertName(MainForm.COC1.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT1.Text);
        if MainForm.sgOpenItems.Cells[1, iCNT] = ConvertName(MainForm.COC2.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT2.Text);
        if MainForm.sgOpenItems.Cells[1, iCNT] = ConvertName(MainForm.COC3.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT3.Text);
        if MainForm.sgOpenItems.Cells[1, iCNT] = ConvertName(MainForm.COC4.Text, 'F', 0) then InterestRate:=StrToFloat(MainForm.INT4.Text);
        if (TimeDiff < 0) and (InterestRate > 0) and (InvoiceAmt > 0) and (IsVoType(MainForm.sgOpenItems.Cells[3, iCNT]) = True) then
        begin
          DiscountedAmt:=InvoiceAmt / ( 1 + ( (InterestRate / 365) * ABS(TimeDiff)) );
          DecreaseAmt:=InvoiceAmt - DiscountedAmt;
          RecoveryAmt:=InvoiceAmt + DecreaseAmt;
          MainForm.sgOpenItems.Cells[35, iCNT]:=FormatFloat('###0.00', DiscountedAmt);
          MainForm.sgOpenItems.Cells[36, iCNT]:=FormatFloat('###0.00', DecreaseAmt);
          MainForm.sgOpenItems.Cells[37, iCNT]:=FormatFloat('###0.00', RecoveryAmt);
          cDiscountedAmt:=cDiscountedAmt + DiscountedAmt;
          cDecreaseAmt:=cDecreaseAmt + DecreaseAmt;
          cRecoveryAmt:=cRecoveryAmt + RecoveryAmt;
        end else
        begin
          MainForm.sgOpenItems.Cells[35, iCNT]:='0';
          MainForm.sgOpenItems.Cells[36, iCNT]:='0';
          MainForm.sgOpenItems.Cells[37, iCNT]:='0';
        end;

        { -------------------------------------------------------------------------------------------------------------------------------- COUNT ALL INVOICES }
        { DEPENDS ON INVOICE TYPE DEFINED IN THE GENERAL SETTINGS }
        if IsVoType(MainForm.sgOpenItems.Cells[3, iCNT]) = True then inc(nInvoices);

        { ---------------------------------------------------------------------------------------------------------- COUNT ALL OVERDUE INVOICES AND IT AMOUNT }
        { CHECK DIFFERENCE BETWEEN CURRENT DATE AND VOUCHER DATE }
        { VOUCHER TYPE TELLS IF WE HAVE INVOICE OR OTHER ITEM    }
        { WE COUNT ONLY INVOICES                                 }
        if (MainForm.sgOpenItems.Cells[34, iCNT] < '0') and (IsVoType(MainForm.sgOpenItems.Cells[3, iCNT]) = True) then
        begin
          inc(Overdue);
          OverdueAmt:=OverdueAmt + StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT])
        end;

        { ---------------------------------------------------------------------------------------------------------------------------------- COUNT O/S AMOUNT }
        if IsNumeric(MainForm.sgOpenItems.Cells[6, iCNT], 'float') = True then OSamt:=OSamt + StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT]);

        { ------------------------------------------------------------------------------------------------------------------------------ UNALLOCATED PAYMENTS }
        { WE TAKE INTO CONSIDERATION NEGATIVE AMOUNTS }
        { AND VOUCHER THAT INDICATE BANK POSTINGS     }
        if (MainForm.sgOpenItems.Cells[6, iCNT] < '0') and (MainForm.sgOpenItems.Cells[3, iCNT] = AppSettings.TMIG.ReadString(Unallocated, 'VOUCHER_NUM', '')) then
          UNamt:=UNamt + StrToFloat(MainForm.sgOpenItems.Cells[6, iCNT]);

      end;

      { ----------------------------------------------- ! REPAINT 'STRINGGRID' AND END PROCESS ! ------------------------------------------------------------ }
      MainForm.sgOpenItems.Freeze(False);
      PostMessage(MainForm.Handle, WM_GETINFO, 10, LPARAM(PCHAR('Ready.')));
      FreeAndNil(AppSettings);
    end;
  end;

*)
end;

(*

{ -------------------------------------------------------------------------------------------------------------------------------------- SOURCE FILE SCANNING }
function TTransactions.Scan(mode: integer): boolean;
var
  iCNT:      integer;
  SR:        TSearchRec;
  TDate:     TDateTime;
  TTime1:    TDateTime;
  TTime2:    TDateTime;
  CheckSum:  integer;
begin
  Result:=False;
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  if mode = 0 then
  begin
    { IT IS ABSOLUTE NECESSARY FOR FILE-TIME SCANNING   }
    { IF SOURCES FILES ARE UPDATED BY THE SERVER        }
    { THEN WE WILL COMPARE TIMES AND IF ALL ARE CHANGED }
    { THEN WE START THREAD FOR OPEN ITEMS LOAD          }
    if FindFirst(OpenItemsDir + '*.txt', faAnyFile, SR) = 0 then
    begin
      iCNT:=0;
      SetLength(ArrOpenItems, iCNT + 1, 4);
      repeat
        if (SR.Attr <> faDirectory) then
        begin
          ArrOpenItems[iCNT, 0]:=SR.Name;
          FileAge(OpenItemsDir + SR.Name, TDate);
          ArrOpenItems[iCNT, 1]:=LeftStr(DateTimeToStr(TDate), 10);
          ArrOpenItems[iCNT, 2]:=RightStr(DateTimeToStr(TDate), 8);
          ArrOpenItems[iCNT, 3]:='0';
          // DebugMsg(OpenItemsSources[iCNT, 0] + ' | ' + OpenItemsSources[iCNT, 1] + ' | ' + OpenItemsSources[iCNT, 2] + ' | ' + OpenItemsSources[iCNT, 3]); DEBUG LINE
          inc(iCNT);
          SetLength(ArrOpenItems, iCNT + 1, 4);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------- SCAN AND COMPARE }
  if mode = 1 then
  begin
    { INITIALIZE }
    iCNT:=0;
    CheckSum:=0;
    { CHECK IF TIME-STAMP HAS CHANGED WITHIN A GIVEN FILE THAT WAS PRE-LOADED AT STARTUP }
    while iCNT < (High(ArrOpenItems) - 1) do
    begin
      FileAge(OpenItemsDir + ArrOpenItems[iCNT, 0], TDate);
      ArrOpenItems[iCNT, 3]:=RightStr(DateTimeToStr(TDate), 8);
      TTime1:=StrToTime(ArrOpenItems[iCNT, 2]);
      TTime2:=StrToTime(ArrOpenItems[iCNT, 3]);
      if ((TTime1 - TTime2) <> 0) then inc(CheckSum);
      // DebugMsg(OpenItemsSources[iCNT, 0] + ' | ' + OpenItemsSources[iCNT, 1] + ' | ' + OpenItemsSources[iCNT, 2] + ' | ' + OpenItemsSources[iCNT, 3]); DEBUG LINE
      inc(iCNT);
    end;
    if iCNT = CheckSum then
    begin
      for iCNT:=0 to (High(ArrOpenItems) - 1) do ArrOpenItems[iCNT, 2]:=ArrOpenItems[iCNT, 3];
      Result:=True;
    end;
  end;
end;

*)

{ ----------------------------------------------------------------------------------------------------------------------------------- CO CODE NAME CONVERTION }
function TTransactions.ConvertName(CoNumber: string; Prefix: string; mode: integer): string;
var
  iCNT:  integer;
begin
  { INITIALIZE }
  Result:= '';
  { ALLOW TO CONVERT '2020' TO 'F2020', ETC. }
  { USED ONLY FOR OPEN ITEMS AND AGING VIEW  }
  if mode = 0 then
  begin
    if Length(CoNumber) = 4 then Result:=Prefix + CoNumber;
    if Length(CoNumber) = 3 then Result:=Prefix + '0'  + CoNumber;
    if Length(CoNumber) = 2 then Result:=Prefix + '00' + CoNumber;
  end;
  { CONVERTS FROM                            }
  {  1. 2020 TO 02020                        }
  {  2. 340  TO 00340                        }
  {  3. 43   TO 00043                        }
  {  4. 0    TO 00000                        }
  { USED ONLY TO BUILD GROUP_ID              }
  if mode = 1 then
  begin
    if Length(CoNumber) = 4 then Result:='0'   + CoNumber;
    if Length(CoNumber) = 3 then Result:='00'  + CoNumber;
    if Length(CoNumber) = 2 then Result:='000' + CoNumber;
    if Length(CoNumber) = 1 then Result:='00000';
  end;
  { CONVERTS FROM 02020 TO 2020              }
  if mode = 2 then
  begin
    for iCNT:= 1 to Length(CoNumber) do
      if CoNumber[iCNT] <> '0' then
      begin
        Result:=Copy(CoNumber, iCNT, MaxInt);
        Exit;
      end;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------ RETURN KPI FOR GIVEN COMPANY }
function TTransactions.ReturnKPI(SG: TStringGrid; StrCoCode: string; mode: integer): double;
{ MODE = 0 ===> KPI | OVERDUE FOR GIVEN CO CODE     }
{ MODE = 1 ===> KPI | UNALLOCATED FOR GIVEN CO CODE }
var
  iCNT:  integer;
begin
   { INITIALIZE }
   Result:=0;
   { LOOK FOR 'COCODE' AND SUM NUMBERS }
   for iCNT:=1 to SG.RowCount - 1 do
   begin
     if (mode = 0) and (StrCoCode = SG.Cells[1, iCNT]) then Result:=Result + StrToFloat(SG.Cells[15, iCNT]);
     if (mode = 1) and (StrCoCode = SG.Cells[1, iCNT]) then Result:=Result + StrToFloat(SG.Cells[16, iCNT]);
   end;
end;

end.
