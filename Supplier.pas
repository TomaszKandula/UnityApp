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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// SUPPLIER FORM DEMO ! TO BE REMOVED !

unit Supplier;

interface

uses
  ADODB, SysUtils, StdCtrls, Math, Model;

{ ------------------------------------------------------------ ! SUPPLIER FORM CLASS ! ---------------------------------------------------------------------- }
type
  TSupplierForm = class(TDataTables)
  {$TYPEINFO ON}
  private

  public

    { RETRIVE LISTS }
    function  GetAllEntities: _Recordset;
    function  GetAllAgents(EntityName: string): _Recordset;
    function  GetAllTerms(EntityName: string):  _Recordset;
    function  GetAllCurrencies: _Recordset;
    function  GetAllSupplierTypes: _Recordset;
    function  GetAllOpenPending: _Recordset;
    function  GetSelectedTicket(TicketNumberRef, LegalEntityRef, CurrencyRef: string): _Recordset;

    { DATA TO GIVEN VIEW COMPONENT }
    procedure InitSupplierRequestForm(CompanyList, CurrencyList, SupplierTypeList: TComboBox);

    { WRITE TO DATABASE }
    function  WriteRequest(IsSertica: integer; LegalEntity: string; Currency: string; SupplierType: string; Branch: string): boolean;
    function  TicketDecision(TicketNumber, NewRequestStatus: string): boolean;

    { SUPPORTING METHODS }
    function  SendEmailToSupplier(CustomerName, LegalEntityName: string; IsSertica: integer; EmailTo: string): boolean;
    function  GenerateTicket: integer;
    var       GetTicket: string;
  end;

implementation

uses
  Main, Settings, Mailer;

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

{ -------------------------------------------------------------------------------------------------------------------------- GET ALL OPEN AND PENDING TICKETS }
function TSupplierForm.GetAllOpenPending: _Recordset;
begin
  CleanUp;
  StrSQL:=SELECT +
            TblSupplierRequest + '.' + TSupplierRequest.TicketNumber + COMMA +
            TblSupplierData    + '.' + TSupplierData.CustomerName + COMMA +
            TblSupplierData    + '.' + TSupplierData.TicketNumberRef + COMMA +
            TblSupplierRequest + '.' + TSupplierRequest.LegalEntityRef + COMMA +
            TblSupplierRequest + '.' + TSupplierRequest.CurrencyRef +
          FROM +
            TblSupplierRequest +
          LEFT_JOIN +
            TblSupplierData +
          _ON +
            TblSupplierRequest + '.' + TSupplierRequest.id +
          EQUAL +
            TblSupplierData    + '.' + TSupplierData.TicketNumberRef +
          WHERE +
            TblSupplierRequest + '.' + TSupplierRequest.TicketStatus +
          EQUAL +
            QuotedStr(sdCLOSE) +
          _AND +
            TblSupplierRequest + '.' + TSupplierRequest.RequestStatus +
          EQUAL +
            QuotedStr(sdPENDING);
  Result:=ExecSQL;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- GET ONLY SELECTED TICKET }
function TSupplierForm.GetSelectedTicket(TicketNumberRef, LegalEntityRef, CurrencyRef: string): _Recordset;
begin
  CleanUp;
  StrSQL:=EXECUTE + SupplierDataView + SPACE + QuotedStr(TicketNumberRef) + COMMA + QuotedStr(LegalEntityRef) + COMMA + QuotedStr(CurrencyRef);
  DataSet:=ExecSQL;
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
  TicketNumber:=0;
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
begin

  { INITIALIZE }
  GetTicket:=IntToStr(GenerateTicket);
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
  if DataSet.RecordCount = 1 then CurrencyRef:=DataSet.Fields[TCurrencies.Id].Value;

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
  Columns.Add(TSupplierRequest.TicketNumber);     Values.Add(GetTicket);
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

{ ------------------------------------------------------------------------------------------------------------------------------------- UPDATE REQUEST STATUS }
function TSupplierForm.TicketDecision(TicketNumber: string; NewRequestStatus: string): boolean;
var
  Condition:  string;
begin
  CleanUp;
  OpenTable(TblSupplierRequest);
  Condition:=TSupplierRequest.TicketNumber + EQUAL + QuotedStr(TicketNumber);
  DataSet.Filter:=Condition;
  Columns.Add(TSupplierRequest.RequestStatus);
  Values.Add(NewRequestStatus);
  Conditions.Add(Condition);
  Result:=UpdateRecord(TblSupplierRequest);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SEND EMAIL NOTIFICATION }
function TSupplierForm.SendEmailToSupplier(CustomerName, LegalEntityName: string; IsSertica: integer; EmailTo: string): boolean;
var
  Mail:      TMailer;
  AppSet:    TSettings;
  Doc:       TDocument;
  HTMLBody:  string;
begin
  { PROCEED }
  AppSet:=TSettings.Create;
  Mail  :=TMailer.Create;
  Doc   :=TDocument.Create;
  try
    { SET EMAIL DETAILS }
    if AppSet.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerNTLM  then
    begin
      //Mail.XMailer    :=AppSet.TMIG.ReadString(MailerNTLM, 'FROM', '');
      //Mail.MailTo     :=AppSet.TMIG.ReadString(MailerNTLM, 'TO', '');
      //Mail.MailRt     :=AppSet.TMIG.ReadString(MailerNTLM, 'REPLY-TO', '');
    end;
    if AppSet.TMIG.ReadString(MailerSetup, 'ACTIVE', '') = MailerBASIC then
    begin
      //Mail.XMailer    :=AppSet.TMIG.ReadString(MailerBASIC, 'FROM', '');
      //Mail.MailTo     :=AppSet.TMIG.ReadString(MailerBASIC, 'TO', '');
      //Mail.MailRt     :=AppSet.TMIG.ReadString(MailerBASIC, 'REPLY-TO', '');
    end;
    Mail.MailFrom   :=MainForm.WinUserName + '@' + AppSet.TMIG.ReadString(ApplicationDetails, 'MAIL_DOMAIN', '');
    Mail.XMailer    :=Mail.MailFrom;
    Mail.MailTo     :=EmailTo;
    Mail.MailRt     :='';
    Mail.MailCc     :='';
    Mail.MailBcc    :='';
    Mail.MailSubject:='New Supplier Request'; //+ CustomerName;
    { PLAIN TEXT TO HTML TEMPLATE }
    HTMLBody        :=Doc.LoadTemplate(AppSet.FLayoutDir + AppSet.TMIG.ReadString(VariousLayouts, 'SUPPLIERNOTF', '') + '.html');
    HTMLBody        :=StringReplace(HTMLBody, '{ADDR_LBU}',  LegalEntityName, [rfReplaceAll]);
    HTMLBody        :=StringReplace(HTMLBody, '{ADDR_DATA}', CustomerName,    [rfReplaceAll]);
    HTMLBody        :=StringReplace(HTMLBody, '{LINK}',      AppSet.TMIG.ReadString(VariousLayouts, 'TICKET_PATH', '') + GetTicket, [rfReplaceAll]);
    { ASSIGN PREPARED HTML }
    Mail.MailBody   :=HTMLBody;
    { SEND }
    Result:=Mail.SendNow;
    //Result:=True;
  finally
    AppSet.Free;
    Mail.Free;
    Doc.Free;
  end;
end;

end.
