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
  Main, Settings, Model, ADODB, SysUtils, StdCtrls; //Windows, Messages, , Classes, StdCtrls

{ ------------------------------------------------------------ ! SUPPLIER FORM CLASS ! ---------------------------------------------------------------------- }
type
  TSupplierForm = class(TDataTables)
  {$TYPEINFO ON}
  private

  public
    function  GetAllEntities: _Recordset;
    function  GetAllAgents(EntityName: string): _Recordset;
    function  GetAllTerms(EntityName: string): _Recordset;
    function  GetAllCurrencies: _Recordset;
    function  GetAllSupplierTypes: _Recordset;
    procedure InitSupplierRequestForm(CompanyList, CurrencyList, SupplierTypeList: TComboBox);
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
  CustFilter:=WHERE + EQUAL + QuotedStr(EntityName);
  OpenTable(TblCompany);
  Result:=DataSet;
end;

{ --------------------------------------------------------------------------------------------------------------- GET ALL PAYMENT TERMS FOR GIVEN ENTITY NAME }
function TSupplierForm.GetAllTerms(EntityName: string): _Recordset;
begin
  CleanUp;
  StrSQL:=SELECT + TPmtterms.DESCRIPTION + FROM + TblPmtterms + WHERE + TPmtterms.COCODE + EQUAL +
          (SELECT_DIS + TCompany.CO_CODE + FROM + TblCompany + WHERE + TCompany.CONAME + EQUAL + QuotedStr(EntityName));
  Result:=ExecSQL;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- GET ALL CURRENCIES }
function TSupplierForm.GetAllCurrencies: _Recordset;
begin
  CleanUp;
  Columns.Add(TCurrencies.Iso);
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

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
procedure TSupplierForm.InitSupplierRequestForm(CompanyList, CurrencyList, SupplierTypeList: TComboBox);
begin
  if SqlToSimpleList(CompanyList,      GetAllEntities)      then CompanyList.ItemIndex:=0;
  if SqlToSimpleList(CurrencyList,     GetAllCurrencies)    then CurrencyList.ItemIndex:=0;
  if SqlToSimpleList(SupplierTypeList, GetAllSupplierTypes) then SupplierTypeList.ItemIndex:=0;
end;




end.
