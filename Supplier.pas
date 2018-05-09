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
unit Supplier;

interface

uses
  Main, Settings, Model, ADODB, SysUtils, StdCtrls, Math;

{ ------------------------------------------------------------ ! SUPPLIER FORM CLASS ! ---------------------------------------------------------------------- }
type
  TSupplierForm = class(TDataTables)
  {$TYPEINFO ON}
  private
    {  }
  public
    { RETRIVE LISTS }
    function  GetAllEntities: _Recordset;
    function  GetAllAgents(EntityName: string): _Recordset;
    function  GetAllTerms(EntityName: string):  _Recordset;
    function  GetAllCurrencies: _Recordset;
    function  GetAllSupplierTypes: _Recordset;
    { DATA TO GIVEN VIEW COMPONENT }
    procedure InitSupplierRequestForm(CompanyList, CurrencyList, SupplierTypeList: TComboBox);
    { WRITE TO DATABASE }
    function  WriteRequest(IsSertica: integer; LegalEntity: string; Currency: string; SupplierType: string; Branch: string): boolean;
    { SUPPORTING METHODS }
    function  GenerateTicket: integer;
    //procedure SendEmailToSupplier;
  end;

implementation

{ ------------------------------------------------------------ ! BASE CLASS METHODS ! ----------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------------ GET ALL ENTITIES NAMES }
function TSupplierForm.GetAllEntities: _Recordset;
begin
  CleanUp;
  Columns.Add(DISTINCT + TCompany.CONAME);
  OpenTable(TblCompany);
  Result:=DataSet;
end;

{ ---------------------------------------------------------------------------------------------------------------------- GET ALL AGENTS FOR GIVEN ENTITY NAME }
function TSupplierForm.GetAllAgents(EntityName: string): _Recordset;
begin
  CleanUp;
  Columns.Add(TCompany.BRANCH);
  CustFilter:=WHERE + TCompany.CONAME + EQUAL + QuotedStr(EntityName);
  OpenTable(TblCompany);
  Result:=DataSet;
end;

{ --------------------------------------------------------------------------------------------------------------- GET ALL PAYMENT TERMS FOR GIVEN ENTITY NAME }
function TSupplierForm.GetAllTerms(EntityName: string): _Recordset;
begin
  CleanUp;
  StrSQL:=SELECT +
            TPmtterms.DESCRIPTION +
          FROM +
            TblPmtterms +
          WHERE +
            TPmtterms.COCODE +
          EQUAL +
            BracketStr(SELECT_DIS +
                         TCompany.CO_CODE +
                       FROM +
                         TblCompany +
                       WHERE +
                         TCompany.CONAME +
                       EQUAL +
                         QuotedStr(EntityName),
                       brRound
                      );
  Result:=ExecSQL;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- GET ALL CURRENCIES }
function TSupplierForm.GetAllCurrencies: _Recordset;
begin
  CleanUp;
  Columns.Add(DISTINCT + TCurrencies.Iso);
  OpenTable(TblCurrencies);
  Result:=DataSet;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ GET ALL SUPPLIER TYPES }
function TSupplierForm.GetAllSupplierTypes: _Recordset;
begin
  CleanUp;
  Columns.Add(TSupplierType.SupplierType);
  OpenTable(TblSupplierType);
  Result:=DataSet;
end;

{ -------------------------------------------------------- ! METHODS THAT UPDATE VIEW ! --------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
procedure TSupplierForm.InitSupplierRequestForm(CompanyList, CurrencyList, SupplierTypeList: TComboBox);
begin
  if SqlToSimpleList(CompanyList,      GetAllEntities)      then CompanyList.ItemIndex:=0;
  if SqlToSimpleList(CurrencyList,     GetAllCurrencies)    then CurrencyList.ItemIndex:=0;
  if SqlToSimpleList(SupplierTypeList, GetAllSupplierTypes) then SupplierTypeList.ItemIndex:=0;
end;

{ ---------------------------------------------------------------- ! TICKET ! ------------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------- GENERATE UNIQUE TICKET NUMBER }
function TSupplierForm.GenerateTicket: integer;
var
  TicketNumber:  integer;
  Check:         integer;
  Limit:         integer;
  iCNT:          integer;
begin
  Result:=0;
  Check :=-100;
  Limit :=10;
  iCNT  :=0;
  while Check <> 0 do
  begin
    { GENERATE TICKET }
    TicketNumber:=RandomRange(1000000000, 2100100100);
    { COUNT NUMBER OF LOOPS }
    Inc(iCNT);
    CleanUp;
    Columns.Add(TSupplierRequest.TicketNumber);
    CustFilter:=WHERE + TSupplierRequest.TicketNumber + EQUAL + QuotedStr(IntToStr(TicketNumber));
    OpenTable(TblSupplierRequest);
    { END LOOP IF DOES NOT EXIST }
    Check:=DataSet.RecordCount;
    { EXIT IF CANNOT GENERATE UNIQUE VALUE }
    if iCNT = Limit then Break;
  end;
  if check = 0 then Result:=TicketNumber;
end;

{ ------------------------------------------------------------ ! WRITE TO DATABASE ! ------------------------------------------------------------------------ }

{ ------------------------------------------------------------------------------------------------------------------------------------ WRITE PREPARED REQUEST }
function TSupplierForm.WriteRequest(IsSertica: integer; LegalEntity: string; Currency: string; SupplierType: string; Branch: string): boolean;
var
  LegalEntityRef:  string;
  CurrencyRef:     string;
  SupplierTypeRef: string;
  GenTicket:       string;
begin

  { INITIALIZE }
  GenTicket:=IntToStr(GenerateTicket);
  Result:=False;

  { FIND NECESSARY KEYS }

  { LEGAL ENTITY }
  CleanUp;
  Columns.Add(TCompany.ID);
  CustFilter:=WHERE + TCompany.CONAME + EQUAL + QuotedStr(LegalEntity) + _AND + TCompany.BRANCH + EQUAL + QuotedStr(Branch);
  OpenTable(TblCompany);
  if DataSet.RecordCount = 1 then LegalEntityRef:=DataSet.Fields[TCompany.ID].Value;

  { CURRENCY }
  CleanUp;
  Columns.Add(TCurrencies.Id);
  CustFilter:=WHERE + TCurrencies.Iso + EQUAL + QuotedStr(Currency);
  OpenTable(TblCurrencies);
  if DataSet.RecordCount > 0 then CurrencyRef:=DataSet.Fields[TCurrencies.Id].Value;      ///!!!

  { SUPPLIER TYPE }
  CleanUp;
  Columns.Add(TSupplierType.Id);
  CustFilter:=WHERE + TSupplierType.SupplierType + EQUAL + QuotedStr(SupplierType);
  OpenTable(TblSupplierType);
  if DataSet.RecordCount = 1 then SupplierTypeRef:=DataSet.Fields[TSupplierType.Id].Value;

  { EXIT CONDITIONS }
  if (LegalEntityRef = '') or (CurrencyRef = '') or (SupplierTypeRef = '') then Exit;

  { REQUIRED }
  CleanUp;
  Columns.Add(TSupplierRequest.Stamp);            Values.Add(DateTimeToStr(Now));
  Columns.Add(TSupplierRequest.UserAlias);        Values.Add(UpperCase(MainForm.WinUserName));
  Columns.Add(TSupplierRequest.LegalEntityRef);   Values.Add(LegalEntityRef);
  Columns.Add(TSupplierRequest.CurrencyRef);      Values.Add(CurrencyRef);
  Columns.Add(TSupplierRequest.SupplierTypeRef);  Values.Add(SupplierTypeRef);
  Columns.Add(TSupplierRequest.PaymentTerm);      Values.Add(MainForm.cbPaymentTerms.Text);
  Columns.Add(TSupplierRequest.POD);              Values.Add(MainForm.cbPOD.Text);
  Columns.Add(TSupplierRequest.AddComment);       Values.Add(MainForm.editAddComment.Text);
  Columns.Add(TSupplierRequest.EmailAddress);     Values.Add(MainForm.editEmailAddress.Text);
  Columns.Add(TSupplierRequest.TicketStatus);     Values.Add('OPEN');
  Columns.Add(TSupplierRequest.TicketNumber);     Values.Add(GenTicket);
  Columns.Add(TSupplierRequest.RequestStatus);    Values.Add('PENDING');

  { OPTIONAL }
  if IsSertica = onSertica then
  begin
    Columns.Add(TSupplierRequest.SerticaUnits);   Values.Add(MainForm.editSerticaUnits.Text);
    Columns.Add(TSupplierRequest.SerticaHandle);  Values.Add(MainForm.editSerticaHandlingOrder.Text);
    Columns.Add(TSupplierRequest.SerticaPrice);   Values.Add(MainForm.editSerticaBuyOrder.Text);
    Columns.Add(TSupplierRequest.SerticaTerms);   Values.Add(MainForm.editSerticaTerms.Text);
  end;

  { INSERT NEW RECORD }
  Result:=InsertInto(TblSupplierRequest);
end;

end.
