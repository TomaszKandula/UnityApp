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
unit Model;

interface

uses
  Main, SQL, Windows, Classes, SysUtils, StrUtils, ADODB;

{ ---------------------------------------------------------- ! DATABASE TABLES HANDLING ! ------------------------------------------------------------------- }
type
  TDataTables = class(TMSSQL)
  {$TYPEINFO ON}
  private
    var pConnStr   :  string;
  public
    var idThd      :  integer;
    var CustFilter :  string;
    var DataSet    :  _Recordset;
    var Columns    :  TStringList;
    var Values     :  TStringList;
    var Conditions :  TStringList;
    property ConnStr : string read pConnStr;
  published
    constructor Create(Connector: TADOConnection); overload;
    destructor  Destroy; override;
    function    BracketStr(Expression: string; BracketType: integer): string;
    function    ColumnsToList(Holder: TStringList; Quoted: integer): string;
    procedure   CleanUp;
    function    OpenTable(TableName: string): boolean;
    function    InsertInto(TableName: string): boolean;
    function    UpdateRecord(TableName: string): boolean;
    function    DeleteRecord(TableName: string; KeyName: string; ID: cardinal): boolean;
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ -------------------------------------------------------------- ! TBL_COMPANY ! ---------------------------------------------------------------------------- }
type
  TCompany = class(TDataTables)  { MANY-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID                     : string = 'Id';           { PRIMARY KEY }
    const CO_CODE                : string = 'CoCode';
    const DBNAME                 : string = 'DbName';
    const BRANCH                 : string = 'Branch';
    const CONAME                 : string = 'CoName';
    const COCURRENCY             : string = 'CoCurrency';
    const COTYPE                 : string = 'CoType';
    const COUNTRY                : string = 'Country';
    const CITY                   : string = 'City';
    const FMANAGER               : string = 'FinManager';
    const INTEREST_RATE          : string = 'InterestRate';
    const VATNO                  : string = 'VatNo';
    const COADDRESS              : string = 'CoAddress';
    const AGENTS                 : string = 'Agents';
    const KPI_OVERDUE_TARGET     : string = 'KpiOverdueTarget';
    const KPI_UNALLOCATED_TARGET : string = 'KpiUnallocatedTarget';
    const SEND_NOTE_FROM         : string = 'SendNoteFrom';
    const LEGALTO                : string = 'LegalTo';
    const BANKDETAILS            : string = 'BankAccounts';
    const STAT_EXCEPT            : string = 'StatementExcept';
    const FIRST_STATEMENT        : string = 'FirstStatement';
    const SECOND_STATEMENT       : string = 'SecondStatement';
    const REM_EX1                : string = 'ReminderException1';
    const REM_EX2                : string = 'ReminderException2';
    const REM_EX3                : string = 'ReminderException3';
    const REM_EX4                : string = 'ReminderException4';
    const REM_EX5                : string = 'ReminderException5';
    const DUNS                   : string = 'Duns';
    const Telephone              : string = 'TelephoneNumbers';
    const MAN_ID                 : string = 'IdManager';           { FOREIGN KEY -> PRIMARY KEY IN "TBL_MANAGERS"    }
    const TL_ID                  : string = 'IdTeamleader';        { FOREIGN KEY -> PRIMARY KEY IN "TBL_TEAMLEADERS" }
    const DIVISIONS              : string = 'Divisions';
  end;

{ -------------------------------------------------------------- ! TBL_MANAGERS ! --------------------------------------------------------------------------- }
type
  TManagers = class(TDataTables)  { MANY-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID        : string = 'Id';            { PRIMARY KEY <- FOREGIN KEY FROM "TBL_COMPANY" }
    const ManagerAR : string = 'ManagerAR';
    const ManagerAP : string = 'ManagerAP';
    const ManagerGL : string = 'ManagerGL';
  end;

{ ------------------------------------------------------------- ! TBL_TEAMLEADERS ! ------------------------------------------------------------------------- }
type
  TTeamleaders = class(TDataTables)  { MANY-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID  :  string = 'Id';                 { PRIMARY KEY <- FOREIGN KEY FROM "TBL_COMPANY" }
    const AP1 :  string = 'TeamleaderAp1';
    const AP2 :  string = 'TeamleaderAp2';
    const AR1 :  string = 'TeamleaderAr1';
    const AR2 :  string = 'TeamleaderAr2';
    const GL1 :  string = 'TeamleaderGl1';
    const GL2 :  string = 'TeamleaderGl2';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ ------------------------------------------------------------ ! TBL_ADDRESSBOOK ! -------------------------------------------------------------------------- }
type
  TAddressBook = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID               : string = 'Id';           { PRIMARY KEY }
    const USER_ALIAS       : string = 'UserAlias';
    const SCUID            : string = 'Scuid';        { CONSTRAINT UNIQUE }
    const CUSTOMER_NUMBER  : string = 'CustomerNumber';
    const CUSTOMER_NAME    : string = 'CustomerName';
    const EMAILS           : string = 'Emails';
    const PHONE_NUMBERS    : string = 'PhoneNumbers';
    const CONTACT          : string = 'Contact';
    const ESTATEMENTS      : string = 'Estatements';
    const AGENT            : string = 'Agent';
    const DIVISION         : string = 'Division';
    const COCODE           : string = 'CoCode';
  end;

{ --------------------------------------------------------------- ! TBL_DAILY ! ----------------------------------------------------------------------------- }
type
  TDaily = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';        { PRIMARY KEY }
    const GROUP_ID      : string = 'GroupId';
    const CUID          : string = 'Cuid';
    const AGEDATE       : string = 'AgeDate';
    const STAMP         : string = 'Stamp';
    const USER_ALIAS    : string = 'UserAlias';
    const EMAIL         : string = 'Email';
    const CALLEVENT     : string = 'CallEvent';
    const CALLDURATION  : string = 'CallDuration';
    const FIXCOMMENT    : string = 'FixedComment';
    const EMAIL_Reminder: string = 'EmailReminder';
    const EMAIL_AutoStat: string = 'EmailAutoStat';
    const EMAIL_ManuStat: string = 'EmailManuStat';
  end;

{ --------------------------------------------------------------- ! TBL_GENERAL ! --------------------------------------------------------------------------- }
type
  TGeneral = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID          : string = 'Id';          { PRIMARY KEY }
    const CUID        : string = 'Cuid';        { CONSTRAINT UNIQUE }
    const STAMP       : string = 'Stamp';
    const USER_ALIAS  : string = 'UserAlias';
    const FIXCOMMENT  : string = 'FixedComment';
    const FOLLOWUP    : string = 'FollowUp';

    (* USER FRIENDLY NAME FOR GIVEN COLUMNS *)

    const fFOLLOWUP   : string = 'Follow Up';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ -------------------------------------------------------------- ! TBL_OPENITEMS ! --------------------------------------------------------------------------- }
type
  TOpenitems = class(TDataTables)                               (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID               : string = 'Id';     { PRIMARY KEY }
    const SourceDBName     : string = 'SourceDBName';
    const CustNo           : string = 'CustNo';
    const VoTp             : string = 'VoTp';
    const OpenCurAm        : string = 'OpenCurAm';
    const OpenAm           : string = 'OpenAm';
    const Nm               : string = 'Nm';
    const ISO              : string = 'ISO';
    const CurAm            : string = 'CurAm';
    const Am               : string = 'Am';
    const InvoNo           : string = 'InvoNo';
    const DueDt            : string = 'DueDt';
    const Inf4             : string = 'Inf4';
    const Inf7             : string = 'Inf7';
    const CrLmt            : string = 'CrLmt';
    const Ctry             : string = 'Ctry';
    const CPmtTrm          : string = 'CPmtTrm';
    const PdSts            : string = 'PdSts';
    const Agent            : string = 'Agent';
    const Ctrl             : string = 'Ctrl';
    const Ad1              : string = 'Ad1';
    const Ad2              : string = 'Ad2';
    const Ad3              : string = 'Ad3';
    const Pno              : string = 'Pno';
    const PArea            : string = 'PArea';
    const GenAcNo          : string = 'GenAcNo';
    const ValDt            : string = 'ValDt';
    const R1               : string = 'R1';
    const Gr3              : string = 'Gr3';
    const Txt              : string = 'Txt';
    const R8               : string = 'R8';
    const DirDeb           : string = 'DirDeb';
    const AddTxt           : string = 'AddTxt';
    const ExtractDateStamp : string = 'ExtractDateStamp';
    const ProcessBatchKey  : string = 'ProcessBatchKey';

    (* "CUID" AND "PMTSTAT" ARE ADDITIONAL COLUMNS CALCULATED 'ON THE FLY' *)
    const PmtStat          : string = 'PmtStat';
    const CUID             : string = 'Cuid';

  end;

{ -------------------------------------------------------------- ! TBL_SNAPSHOTS ! -------------------------------------------------------------------------- }
type
  TSnapshots = class(TDataTables)
  {$TYPEINFO ON}
  public

    (* REFLECTS TABLE COLUMNS IN GIVEN DATABASE *)

    const ID              : string = 'Id';
    const GROUP_ID        : string = 'GroupId';
    const AGE_DATE        : string = 'AgeDate';
    const SNAPSHOT_DT     : string = 'SnapshotDt';
    const CUSTOMER_NAME   : string = 'CustomerName';
    const CUSTOMER_NUMBER : string = 'CustomerNumber';
    const COUNTRY_CODE    : string = 'CountryCode';
    const NOT_DUE         : string = 'NotDue';
    const RANGE1          : string = 'Range1';
    const RANGE2          : string = 'Range2';
    const RANGE3          : string = 'Range3';
    const RANGE4          : string = 'Range4';
    const RANGE5          : string = 'Range5';
    const RANGE6          : string = 'Range6';
    const OVERDUE         : string = 'Overdue';
    const TOTAL           : string = 'Total';
    const CREDIT_LIMIT    : string = 'CreditLimit';
    const EXCEEDED_AMOUNT : string = 'ExceededAmount';
    const PAYMENT_TERMS   : string = 'PaymentTerms';
    const AGENT           : string = 'Agent';
    const DIVISION        : string = 'Division';
    const CO_CODE         : string = 'CoCode';
    const LEDGER_ISO      : string = 'LedgerIso';
    const INF4            : string = 'Inf4';
    const INF7            : string = 'Inf7';
    const PERSON          : string = 'Person';
    const GROUP3          : string = 'Group3';
    const RISK_CLASS      : string = 'RiskClass';
    const CUID            : string = 'Cuid';
    const FREE1           : string = 'Free1';
    const FREE2           : string = 'Free2';

    (* REFLECTS "FRIENDLY" COLUMN NAMES USED IN THE APPLICATION *)
    (* THE BELOW GIVEN NAMES ARE USED IN BOTH SETTING FILES     *)
    (* WARNING! THE NAMES ARE CASE SENSITIVE                    *)

    const fCUSTOMER_NAME   = 'Customer Name';
    const fCUSTOMER_NUMBER = 'Customer Number';
    const fNOT_DUE         = 'Not Due';
    const fCOUNTRY_CODE    = 'Country Code';
    const fRANGE1          = '1 - 7';
    const fRANGE2          = '8 - 30';
    const fRANGE3          = '31 - 60';
    const fRANGE4          = '61 - 90';
    const fRANGE5          = '91 - 120';
    const fRANGE6          = '121 - oo';
    const fTOTAL           = 'Total';
    const fOVERDUE         = 'Overdue';
    const fCREDIT_LIMIT    = 'Credit Limit';
    const fEXCEEDED_AMOUNT = 'Exceeded Amount';
    const fAGENT           = 'Agent';
    const fCO_CODE         = 'Co Code';
    const fPAYMENT_TERMS   = 'Payment Terms';
    const fDIVISION        = 'Division';
    const fLEDGER_ISO      = 'Ledger Iso';
    const fINF4            = 'Inf4';
    const fINF7            = 'Inf7';
    const fPERSON          = 'Person';
    const fGROUP3          = 'Group3';
    const fRISK_CLASS      = 'Risk Class';
    const fCUID            = 'Cuid';
    const fFREE1           = 'Free1';
    const fFREE2           = 'Free2';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ -------------------------------------------------------------- ! TBL_PAIDINFO ! --------------------------------------------------------------------------- }
type
  TPaidinfo = class(TDataTables)                            (* FIXED FOR ALL ENTITIES *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';        { PRIMARY KEY }
    const ERP_CODE      : string = 'ErpCode';
    const DESCRIPTION   : string = 'Description';
    const STAMP         : string = 'ExtractDateStamp';
    const KEY           : string = 'ProcessBatchKey';
  end;

{ --------------------------------------------------------------- ! TBL_PERSON ! ---------------------------------------------------------------------------- }
type
  TPerson = class(TDataTables)                                  (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';        { PRIMARY KEY }
    const ERP_CODE      : string = 'ErpCode';
    const DESCRIPTION   : string = 'Description';
    const STAMP         : string = 'ExtractDateStamp';
    const KEY           : string = 'ProcessBatchKey';
    const COCODE        : string = 'Entity';
  end;

{ --------------------------------------------------------------- ! TBL_GROUP3 ! ---------------------------------------------------------------------------- }
type
  TGroup3 = class(TDataTables)                                  (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';        { PRIMARY KEY }
    const ERP_CODE      : string = 'ErpCode';
    const DESCRIPTION   : string = 'Description';
    const STAMP         : string = 'ExtractDateStamp';
    const KEY           : string = 'ProcessBatchKey';
    const COCODE        : string = 'Entity';
  end;

{ -------------------------------------------------------------- ! TBL_PMTTERMS ! --------------------------------------------------------------------------- }
type
  TPmtterms = class(TDataTables)                               (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';           { PRIMARY KEY }
    const ERP_CODE      : string = 'ErpCode';
    const DESCRIPTION   : string = 'Description';
    const I_MONTH       : string = 'Month';
    const I_DAYS        : string = 'Days';
    const I_DAYS_NET    : string = 'DaysNet';
    const I_USING       : string = 'Using';
    const STAMP         : string = 'ExtractDateStamp';
    const KEY           : string = 'ProcessBatchKey';
    const COCODE        : string = 'Entity';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ --------------------------------------------------------------- ! TBL_TRACKER ! --------------------------------------------------------------------------- }
type
  TTracker = class(TDataTables)  { MANY-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID         : string = 'Id';           { PRIMARY KEY -> FOREIGN KEY IN "TBL_INVOICES" }
    const USER_ALIAS : string = 'UserAlias';
    const CUID       : string = 'Cuid';         { CONSTRAINT UNIQUE }
    const CO_CODE    : string = 'CoCode';
    const BRANCH     : string = 'Branch';
    const CUSTNAME   : string = 'CustomerName';
    const STAMP      : string = 'Stamp';
    const INDV_REM1  : string = 'SendReminder1';
    const INDV_REM2  : string = 'SendReminder2';
    const INDV_REM3  : string = 'SendReminder3';
    const INDV_REM4  : string = 'SendReminder4';
    const EXP_REM2   : string = 'ExceptionReminder2';
    const EXP_REM3   : string = 'ExceptionReminder3';
    const LAYOUT_ID  : string = 'LayoutId';    { FOREIGN KEY -> PRIMARY KEY IN "TBL_REMINDERLAYOUTS" }
  end;

{ -------------------------------------------------------------- ! TBL_INVOICES ! --------------------------------------------------------------------------- }
type
  TInvoices = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID           : string = 'Id';         { PRIMARY KEY }
    const SK           : string = 'Sk';         { FOREIGN KEY -> PRIMARY KEY IN "TBL_TRACKER" }
    const CUID         : string = 'Cuid';
    const INVOICENO    : string = 'InvoiceNo';
    const INVOICESTATE : string = 'InvoiceState';
    const STAMP        : string = 'Stamp';
  end;

{ ------------------------------------------------------------ ! TBL_REMINERLAYOUTS ! ----------------------------------------------------------------------- }
type
  TReminderLayouts = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID           : string = 'Id';         { PRIMARY KEY }
    const LAYOUTNAME   : string = 'LayoutName';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ ----------------------------------------------------------------- ! TBL_UAC ! ----------------------------------------------------------------------------- }
type
  TUAC = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID           : string = 'Id';         { PRIMARY KEY -> FOREIGN KEY IN "TBL_GROUPS" }
    const USERNAME     : string = 'UserName';   { CONSTRAINT UNIQUE }
    const ACCESS_LEVEL : string = 'AccessLevel';
    const ACCESS_MODE  : string = 'AccessMode';
  end;


{ ---------------------------------------------------------------- ! TBL_GROUPS ! --------------------------------------------------------------------------- }
type
  TGroups = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';        { PRIMARY KEY }
    const GROUP_ID      : string = 'GroupId';
    const GROUP_NAME    : string = 'GroupName';
    const FID           : string = 'Fid';       { FOREIGN KEY -> PRIMARY KEY IN "TBL_UAC" }
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ -------------------------------------------------------------- ! TBL_FXRATES ! ---------------------------------------------------------------------------- }
type
  TFxRates = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID            : string = 'Id';
    const ISO           : string = 'Iso';
    const EXRATE        : string = 'ExRate';
    const KEY           : string = 'ProcessBatchKey';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

{ ################################################################# ! MAIN CLASS ! ########################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TDataTables.Create(Connector: TADOConnection);
begin
  idThd     :=0;
  CustFilter:='';
  DataSet   :=nil;
  Columns   :=TStringList.Create;
  Values    :=TStringList.Create;
  Conditions:=TStringList.Create;
  inherited;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- RELEASE }
destructor TDataTables.Destroy;
begin
  Columns.Free;
  Values.Free;
  Conditions.Free;
  DataSet:=nil;
  inherited Destroy;
end;

{ ------------------------------------------------------------------------------------------------------------ HELPER METHOD TO SURROUND STRING WITH BRACKETS }
function TDataTables.BracketStr(Expression: string; BracketType: integer): string;
begin
  Result:='';
  if BracketType = brRound  then Result:='(' + Expression + ')';
  if BracketType = brSquare then Result:='[' + Expression + ']';
  if BracketType = brCurly  then Result:='{' + Expression + '}';
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- TRANSPOSE COLUMNS TO ROW }
function TDataTables.ColumnsToList(Holder: TStringList; Quoted: integer): string;
var
  iCNT:   integer;
begin
  Result:=ALL;
  { PERFORM }
  if (Holder.Text <> '') and (Holder.Count > 0) then
  begin
    Result:='';
    for iCNT:=0 to Holder.Count - 1 do
      if Result = '' then
      begin
        if Quoted = enQuotesOff then Result:=Holder.Strings[iCNT];
        if Quoted = enQuotesOn  then Result:=QuotedStr(Holder.Strings[iCNT]);
      end
      else
      begin
        if Quoted = enQuotesOff then Result:=Result + COMMA + Holder.Strings[iCNT];
        if Quoted = enQuotesOn  then Result:=Result + COMMA + QuotedStr(Holder.Strings[iCNT]);
      end;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- CLEAR ALL LISTS }
procedure TDataTables.CleanUp;
begin
  Columns.Clear;
  Values.Clear;
  Conditions.Clear;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- OPEN TABLE TO RECORDSET }
function TDataTables.OpenTable(TableName: string): boolean;
begin
  Result:=True;
  { EXECUTE QUERY }
  try
    if CustFilter =  '' then StrSQL:=SELECT + ColumnsToList(Columns, enQuotesOff) + FROM + TableName;
    if CustFilter <> '' then StrSQL:=SELECT + ColumnsToList(Columns, enQuotesOff) + FROM + TableName + CustFilter; { WARNING! REQUIRE WHERE CLAUSE }
    DataSet:=ExecSQL;
  except
    Result:=False;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------- INSERT SINGE ROW INTO TABLE }
function TDataTables.InsertInto(TableName: string): boolean;
begin
  Result:=True;
  { BUILD AND EXECUTE }
  try
    if (Values.Text <> '') and (Columns.Text <> '') then
    begin
      StrSQL:=INSERT +
                TableName + SPACE + BracketStr(ColumnsToList(Columns, enQuotesOff), brRound) +
              VAL +
                BracketStr(ColumnsToList(Values, enQuotesOn), brRound);
      ExecSQL;
    end;
  except
    Result:=False;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------- PERFORM UPDATING ON GIVEN COLUMNS }
function TDataTables.UpdateRecord(TableName: string): boolean;
var
  iCNT:  integer;
  Temp:  string;
begin
  Result:=False;
  if (Columns.Text = '') or (Values.Text = '') or (Conditions.Text = '') then Exit;
  { EXECUTE UPDATE FOR EACH COLUMN }
  try
    for iCNT:=0 to Columns.Count - 1 do
    begin
      Temp:=Temp + (
                      _UPDATE +
                        TableName +
                      _SET +
                        Columns.Strings[iCNT] +
                      EQUAL +
                        QuotedStr(Values.Strings[iCNT]) +
                      WHERE +
                        Conditions.Strings[iCNT] + ';'
                   );
    end;
    StrSQL:=Temp;
    ExecSQL;
    Result:=True;
  except
    Result:=False;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- DELETE SINGLE RECORD }
function TDataTables.DeleteRecord(TableName: string; KeyName: string; ID: Cardinal): boolean;
begin
  Result:=False;
  if (TableName = '') and (ID = 0) then Exit;
  try
    StrSQL:=DELETE_FROM + TableName + WHERE + KeyName + EQUAL + QuotedStr(IntToStr(ID));
    ExecSQL;
    Result:=True;
  except
    Result:=False;
  end;
end;

end.
