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
    const ID                     : string = 'ID';  { PRIMARY KEY }
    const CO_CODE                : string = 'CO_CODE';
    const DBNAME                 : string = 'DBNAME';
    const BRANCH                 : string = 'BRANCH';
    const CONAME                 : string = 'CONAME';
    const COCURRENCY             : string = 'COCURRENCY';
    const COTYPE                 : string = 'COTYPE';
    const COUNTRY                : string = 'COUNTRY';
    const CITY                   : string = 'CITY';
    const FMANAGER               : string = 'FMANAGER';
    const INTEREST_RATE          : string = 'INTEREST_RATE';
    const VATNO                  : string = 'VATNO';
    const COADDRESS              : string = 'COADDRESS';
    const AGENTS                 : string = 'AGENTS';
    const KPI_OVERDUE_TARGET     : string = 'KPI_OVERDUE_TARGET';
    const KPI_UNALLOCATED_TARGET : string = 'KPI_UNALLOCATED_TARGET';
    const SEND_NOTE_FROM         : string = 'SEND_NOTE_FROM';
    const LEGALTO                : string = 'LEGALTO';
    const BANKDETAILS            : string = 'BANKDETAILS';
    const STAT_EXCEPT            : string = 'STAT_EXCEPT';
    const FIRST_STATEMENT        : string = 'FIRST_STATEMENT';
    const SECOND_STATEMENT       : string = 'SECOND_STATEMENT';
    const REM_EX1                : string = 'REM_EX1';
    const REM_EX2                : string = 'REM_EX2';
    const REM_EX3                : string = 'REM_EX3';
    const REM_EX4                : string = 'REM_EX4';
    const REM_EX5                : string = 'REM_EX5';
    const DUNS                   : string = 'DUNS';
    const Telephone              : string = 'Telephone';
    const MAN_ID                 : string = 'MAN_ID';  { FOREIGN KEY -> PRIMARY KEY IN "TBL_MANAGERS"    }
    const TL_ID                  : string = 'TL_ID';   { FOREIGN KEY -> PRIMARY KEY IN "TBL_TEAMLEADERS" }
  end;

{ -------------------------------------------------------------- ! TBL_MANAGERS ! --------------------------------------------------------------------------- }
type
  TManagers = class(TDataTables)  { MANY-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID        : string = 'ID';  { PRIMARY KEY <- FOREGIN KEY FROM "TBL_COMPANY" }
    const ManagerAR : string = 'ManagerAR';
    const ManagerAP : string = 'ManagerAP';
    const ManagerGL : string = 'ManagerGL';
  end;

{ ------------------------------------------------------------- ! TBL_TEAMLEADERS ! ------------------------------------------------------------------------- }
type
  TTeamleaders = class(TDataTables)  { MANY-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID  :  string = 'ID';  { PRIMARY KEY <- FOREIGN KEY FROM "TBL_COMPANY" }
    const AP1 :  string = 'AP1';
    const AP2 :  string = 'AP2';
    const AR1 :  string = 'AR1';
    const AR2 :  string = 'AR2';
    const GL1 :  string = 'GL1';
    const GL2 :  string = 'GL2';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ ------------------------------------------------------------ ! TBL_ADDRESSBOOK ! -------------------------------------------------------------------------- }
type
  TAddressBook = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID         : string = 'ID';  { PRIMARY KEY }
    const USER_ALIAS : string = 'USER_ALIAS';
    const CUID       : string = 'CUID';  { CONSTRAINT UNIQUE }
    const CUSTNUMBER : string = 'CUSTNUMBER';
    const CUSTNAME   : string = 'CUSTNAME';
    const EMAILS     : string = 'EMAILS';
    const TELEPHONE  : string = 'TELEPHONE';
    const CONTACT    : string = 'CONTACT';
    const CUSTADDR   : string = 'CUSTADDR';
    const ESTATEMENTS: string = 'ESTATEMENTS';
  end;

{ --------------------------------------------------------------- ! TBL_DAILY ! ----------------------------------------------------------------------------- }
type
  TDaily = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID            : string = 'ID';  { PRIMARY KEY }
    const GROUP_ID      : string = 'GROUP_ID';
    const CUID          : string = 'CUID';
    const AGEDATE       : string = 'AGEDATE';
    const STAMP         : string = 'STAMP';
    const USER_ALIAS    : string = 'USER_ALIAS';
    const EMAIL         : string = 'EMAIL';
    const CALLEVENT     : string = 'CALLEVENT';
    const CALLDURATION  : string = 'CALLDURATION';
    const FIXCOMMENT    : string = 'FIXCOMMENT';
    const EMAIL_Reminder: string = 'EMAIL_Reminder';
    const EMAIL_AutoStat: string = 'EMAIL_AutoStat';
    const EMAIL_ManuStat: string = 'EMAIL_ManuStat';
  end;

{ --------------------------------------------------------------- ! TBL_GENERAL ! --------------------------------------------------------------------------- }
type
  TGeneral = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID          : string = 'ID';  { PRIMARY KEY }
    const CUID        : string = 'CUID';  { CONSTRAINT UNIQUE }
    const STAMP       : string = 'STAMP';
    const USER_ALIAS  : string = 'USER_ALIAS';
    const FIXCOMMENT  : string = 'FIXCOMMENT';
    const FOLLOWUP    : string = 'FOLLOWUP';

    (* USER FRIENDLY NAME FOR GIVEN COLUMNS *)

    const fFOLLOWUP   : string = 'FOLLOW UP';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ -------------------------------------------------------------- ! TBL_OPENITEMS ! --------------------------------------------------------------------------- }
type
  TOpenitems = class(TDataTables)                               (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID               : string = 'ID';  { PRIMARY KEY }
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
  end;

{ -------------------------------------------------------------- ! TBL_SNAPSHOTS ! -------------------------------------------------------------------------- }
type
  TSnapshots = class(TDataTables)
  {$TYPEINFO ON}
  public

    (* REFLECTS TABLE COLUMNS IN GIVEN DATABASE *)

    const ID              : string = 'ID';
    const GROUP_ID        : string = 'GROUP_ID';
    const AGE_DATE        : string = 'AGE_DATE';
    const SNAPSHOT_DT     : string = 'SNAPSHOT_DT';
    const CUSTOMER_NAME   : string = 'CUSTOMER_NAME';
    const CUSTOMER_NUMBER : string = 'CUSTOMER_NUMBER';
    const COUNTRY_CODE    : string = 'COUNTRY_CODE';
    const NOT_DUE         : string = 'NOT_DUE';
    const RANGE1          : string = 'RANGE1';
    const RANGE2          : string = 'RANGE2';
    const RANGE3          : string = 'RANGE3';
    const RANGE4          : string = 'RANGE4';
    const RANGE5          : string = 'RANGE5';
    const RANGE6          : string = 'RANGE6';
    const OVERDUE         : string = 'OVERDUE';
    const TOTAL           : string = 'TOTAL';
    const CREDIT_LIMIT    : string = 'CREDIT_LIMIT';
    const EXCEEDED_AMOUNT : string = 'EXCEEDED_AMOUNT';
    const PAYMENT_TERMS   : string = 'PAYMENT_TERMS';
    const AGENT           : string = 'AGENT';
    const DIVISION        : string = 'DIVISION';
    const CO_CODE         : string = 'CO_CODE';
    const LEDGER_ISO      : string = 'LEDGER_ISO';
    const INF4            : string = 'INF4';
    const INF7            : string = 'INF7';
    const PERSON          : string = 'PERSON';
    const GROUP3          : string = 'GROUP3';
    const RISK_CLASS      : string = 'RISK_CLASS';
    const QUALITY_IDX     : string = 'QUALITY_IDX';
    const WALLET_SHARE    : string = 'WALLET_SHARE';
    const CUID            : string = 'CUID';

    (* REFLECTS "FRIENDLY" COLUMN NAMES USED IN THE APPLICATION *)
    (* THE BELOW GIVEN NAMES ARE USED IN BOTH SETTING FILES     *)
    (* WARNING! THE NAMES ARE CASE SENSITIVE                    *)

    const fCUSTOMER_NAME   = 'CUSTOMER NAME';
    const fCUSTOMER_NUMBER = 'CUSTOMER NUMBER';
    const fFOLLOWUP        = 'FOLLOW UP';
    const fNOT_DUE         = 'NOT DUE';
    const fCOUNTRY_CODE    = 'COUNTRY CODE';
    const fRANGE1          = '1 - 7';
    const fRANGE2          = '8 - 30';
    const fRANGE3          = '31 - 60';
    const fRANGE4          = '61 - 90';
    const fRANGE5          = '91 - 120';
    const fRANGE6          = '121 - oo';
    const fTOTAL           = 'TOTAL';
    const fOVERDUE         = 'OVERDUE';
    const fCREDIT_LIMIT    = 'CREDIT LIMIT';
    const fEXCEEDED_AMOUNT = 'EXCEEDED AMOUNT';
    const fAGENT           = 'AGENT';
    const fCO_CODE         = 'CO CODE';
    const fPAYMENT_TERMS   = 'PAYMENT TERMS';
    const fDIVISION        = 'DIVISION';
    const fLEDGER_ISO      = 'LEDGER ISO';
    const fINF4            = 'INF4';
    const fINF7            = 'INF7';
    const fPERSON          = 'PERSON';
    const fGROUP3          = 'GROUP3';
    const fRISK_CLASS      = 'RISK CLASS';
    const fQUALITY_IDX     = 'QUALITY IDX';
    const fWALLET_SHARE    = 'WALLET SHARE';
    const fCUID            = 'CUID';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ -------------------------------------------------------------- ! TBL_PAIDINFO ! --------------------------------------------------------------------------- }
type
  TPaidinfo = class(TDataTables)                                (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'ID';  { PRIMARY KEY }
    const ERP_CODE      : string = 'ERP_CODE';
    const DESCRIPTION   : string = 'DESCRIPTION';
  end;

{ --------------------------------------------------------------- ! TBL_PERSON ! ---------------------------------------------------------------------------- }
type
  TPerson = class(TDataTables)                                  (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'ID';  { PRIMARY KEY }
    const ERP_CODE      : string = 'ERP_CODE';
    const PERSON_NAME   : string = 'PERSON_NAME';
  end;

{ --------------------------------------------------------------- ! TBL_GROUP3 ! ---------------------------------------------------------------------------- }
type
  TGroup3 = class(TDataTables)                                  (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID            : string = 'ID';  { PRIMARY KEY }
    const ERP_CODE      : string = 'ERP_CODE';
    const SALESMEN_NAME : string = 'SALESMEN_NAME';
  end;

{ -------------------------------------------------------------- ! TBL_PMTTERMS ! --------------------------------------------------------------------------- }
type
  TPmtterms = class(TDataTables)                               (* FEED FROM ERP *)
  {$TYPEINFO ON}
  public
    const ID         : string = 'ID';  { PRIMARY KEY }
    const ERP_CODE   : string = 'ERP_CODE';
    const TEXT_DESC  : string = 'TEXT_DESC';
    const I_MONTH    : string = 'I_MONTH';
    const I_DAYS     : string = 'I_DAYS';
    const I_DAYS_NET : string = 'I_DAYS_NET';
    const I_USING    : string = 'I_USING';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ --------------------------------------------------------------- ! TBL_TRACKER ! --------------------------------------------------------------------------- }
type
  TTracker = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID         : string = 'ID';   { PRIMARY KEY -> FOREIGN KEY IN "TBL_INVOICES" }
    const USER_ALIAS : string = 'USER_ALIAS';
    const CUID       : string = 'CUID';  { CONSTRAINT UNIQUE }
    const CO_CODE    : string = 'CO_CODE';
    const BRANCH     : string = 'BRANCH';
    const CUSTNAME   : string = 'CUSTNAME';
    const LAYOUT     : string = 'LAYOUT';
    const STAMP      : string = 'STAMP';
    const INDV_REM1  : string = 'INDV_REM1';
    const INDV_REM2  : string = 'INDV_REM2';
    const INDV_REM3  : string = 'INDV_REM3';
    const INDV_REM4  : string = 'INDV_REM4';
  end;


{ -------------------------------------------------------------- ! TBL_INVOICES ! --------------------------------------------------------------------------- }
type
  TInvoices = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID           : string = 'ID';  { PRIMARY KEY }
    const SK           : string = 'SK';  { FOREIGN KEY -> PRIMARY KEY IN "TBL_TRACKER" }
    const CUID         : string = 'CUID';
    const INVOICENO    : string = 'INVOICENO';
    const INVOICESTATE : string = 'INVOICESTATE';
    const STAMP        : string = 'STAMP';
  end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ ----------------------------------------------------------------- ! TBL_UAC ! ----------------------------------------------------------------------------- }
type
  TUAC = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID           : string = 'ID';  { PRIMARY KEY -> FOREIGN KEY IN "TBL_GROUPS" }
    const USERNAME     : string = 'USERNAME';  { CONSTRAINT UNIQUE }
    const ACCESS_LEVEL : string = 'ACCESS_LEVEL';
    const ACCESS_MODE  : string = 'ACCESS_MODE';
  end;


{ ---------------------------------------------------------------- ! TBL_GROUPS ! --------------------------------------------------------------------------- }
type
  TGroups = class(TDataTables)  { ONE-TO-MANY }
  {$TYPEINFO ON}
  public
    const ID            : string = 'ID';   { PRIMARY KEY }
    const GROUP_ID      : string = 'GROUP_ID';
    const GROUP_NAME    : string = 'GROUP_NAME';
    const FID           : string = 'FID';  { FOREIGN KEY -> PRIMARY KEY IN "TBL_UAC" }
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
