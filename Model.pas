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
unit Model;

interface

uses
  Main, SQL, Windows, Classes, SysUtils, StrUtils, ADODB;

{ ---------------------------------------------------------- ! DATABASE TABLES HANDLING ! ------------------------------------------------------------------- }
type
  TDataTables = class(TMSSQL)
  {$TYPEINFO ON}
  private
    pidThd   :  integer;
    pDataSet :  _Recordset;
    pConnStr :  string;
  public
    property idThd   :  integer    read pidThd;
    property DataSet :  _Recordset read pDataSet;
    property ConnStr :  string     read pConnStr;
  published
    constructor Create(Connector: TADOConnection); overload;
    destructor  Destroy; override;
    function    Open(TableName: string) :  boolean;
  end;

{ ----------------------------------------------------------------- ! TBL_DAILY ! --------------------------------------------------------------------------- }
type
  TDaily = class(TDataTables)
  {$TYPEINFO ON}
  public
    const ID          : string = 'ID';
    const GROUP_ID    : string = 'GROUP_ID';
    const CUID        : string = 'CUID';
    const AGEDATE     : string = 'AGEDATE';
    const STAMP       : string = 'STAMP';
    const USER_ALIAS  : string = 'USER_ALIAS';
    const EMAIL       : string = 'EMAIL';
    const CALLEVENT   : string = 'CALLEVENT';
    const CALLDURATION: string = 'CALLDURATION';
    const FIXCOMMENT  : string = 'FIXCOMMENT';
  end;



{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

{ ################################################################# ! BASE CLASS ! ########################################################################## }

{ ------------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE }
constructor TDataTables.Create(Connector: TADOConnection);
begin
  pidThd  :=0;
  pConnStr:=Connector.ConnectionString;
  pDataSet:=nil;
end;

destructor TDataTables.Destroy;
begin
  FreeAndNil(pDataSet);
  inherited;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- OPEN TO RECORDSET }
function TDataTables.Open(TableName: string): boolean;
begin
  Result:=True;
  try
    DataSet.Open(TableName, ConnStr, adOpenKeyset, adLockOptimistic, adCmdTable);
  except
    Result:=False;
  end;
end;

end.
