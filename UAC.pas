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
unit UAC;

interface

uses
  Main, Model, SysUtils, Windows, StdCtrls, Classes, ADODB, StrUtils, Variants;

type
  TUserControl = class(TDataTables)
  {$TYPEINFO ON}
  public
    var UserName: string;
    function GetAccessData(DataType: integer): string;
    function GetGroupList(var List: TLists; GroupListBox: TComboBox): boolean;
    function GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
  end;

implementation

{ ----------------------------------------------------------------------------------------------------------------- READ THE ACCESS DATA FOR GIVEN USER ALIAS }
function TUserControl.GetAccessData(DataType: integer): string;
begin
  Result:='';
  if UserName = '' then Exit;
  try
    OpenTable(TblUAC);
    DataSet.Filter:=TUAC.USERNAME + EQUAL + QuotedStr(UserName);
    { USERNAME IS CONSTRAINED, THUS EXCEPTING ONLY ONE ROW IF FOUND }
    if not DataSet.EOF then
    begin
      if DataType = adAccessLevel then Result:=DataSet.Fields.Item[TUAC.ACCESS_LEVEL].Value;
      if DataType = adAccessMode  then Result:=DataSet.Fields.Item[TUAC.ACCESS_MODE].Value;
      if DataType = adUserKeyID   then Result:=DataSet.Fields.Item[TUAC.ID].Value;
      DataSet.Filter:=adFilterNone;
    end;
  except
    Result:='';
  end;
end;

{ ---------------------------------------------------------------------------------------------------------- READ GROUP ID AND GROUP NAME FOR GIVEN USER NAME }
function TUserControl.GetGroupList(var List: TLists; GroupListBox: TComboBox): boolean;
var
  iCNT:     integer;
  UserKey:  string;
begin
  Result:=True;
  iCNT  :=0;
  try
    UserKey:=GetAccessData(adUserKeyID);
    OpenTable(TblGroups);
    DataSet.Filter:=TGroups.FID + EQUAL + QuotedStr(UserKey);
    DataSet.Sort:=TGroups.GROUP_NAME + ASC;
    { FOR GIVEN USER ID, WE CAN HAVE MANY ITEMS }
    if DataSet.RecordCount > 0 then
    begin
      SetLength(List, 2, 2);
      while not DataSet.EOF do
      begin
        List[iCNT, 0]:=DataSet.Fields[TGroups.GROUP_ID].Value;
        List[iCNT, 1]:=DataSet.Fields[TGroups.GROUP_NAME].Value;
        inc(iCNT);
        DataSet.MoveNext;
        SetLength(List, iCNT + 1, 2);
      end;
      DataSet.Filter:=adFilterNone;
      { DISPLAY GROUP NAME IN COMBO BOX }
      GroupListBox.Clear;
      for iCNT:=0 to high(List) - 1 do GroupListBox.Items.Add(List[iCNT, 1]);
      GroupListBox.ItemIndex:=0;
    end;
  except
    Result:=False;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------- AGE DATES FOR SELECTED GROUP ID }
function TUserControl.GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
begin
  Result:=True;
  StrSQL:=SELECT_DIS + TSnapshots.AGE_DATE + FROM + TblSnapshots + WHERE + TSnapshots.GROUP_ID + EQUAL + QuotedStr(GroupID);
  try
    DataSet:=ExecSQL;
    AgeDatesBox.Clear;
    if DataSet.RecordCount > 0 then
    begin
      while not DataSet.EOF do
      begin
        AgeDatesBox.Items.Add(DataSet.Fields[TSnapshots.AGE_DATE].Value);
        DataSet.MoveNext;
      end;
      { DISPLAY LAST DATE FROM THE LIST }
      AgeDatesBox.ItemIndex:=AgeDatesBox.Items.Count - 1;
    end;
  except
    Result:=False;
  end;
end;

end.
