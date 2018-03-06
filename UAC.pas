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
  private
    var pUserName: string;
  public
    property  UserName: string read pUserName write pUserName;
    function  GetAccessData(DataType: integer): string;
    procedure GetGroupList(var List: TStrArray; GroupListBox: TComboBox);
    procedure GetAgeDates(AgeDatesBox: TComboBox; GroupID: string);
  end;

implementation

{ ----------------------------------------------------------------------------------------------------------------- READ THE ACCESS DATA FOR GIVEN USER ALIAS }
function TUserControl.GetAccessData(DataType: integer): string;
begin
  Result:='';
  if pUserName = '' then Exit;
  try
    OpenTable(TblUAC);
    DataSet.Filter:=TUAC.USERNAME + EQUAL + QuotedStr(pUserName);
    { USERNAME IS CONSTRAINED, THUS EXCEPTING ONLY ONE ROW IF FOUND }
    if not DataSet.EOF then
    begin
      if DataType = adAccessLevel then Result:=DataSet.Fields.Item[TUAC.ACCESS_LEVEL].Value;
      if DataType = adAccessMode  then Result:=DataSet.Fields.Item[TUAC.ACCESS_MODE].Value;
      if DataType = adUserKeyID   then Result:=DataSet.Fields.Item[TUAC.ID].Value;
      DataSet.Filter:=adFilterNone;
    end;
  finally
    DataSet.Close;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------- READ GROUP ID AND GROUP NAME FOR GIVEN USER NAME }
procedure TUserControl.GetGroupList(var List: TStrArray; GroupListBox: TComboBox);
var
  iCNT:     integer;
  UserKey:  string;
begin
  iCNT  :=0;
  try
    UserKey:=GetAccessData(adUserKeyID);
    OpenTable(TblGroups);
    DataSet.Filter:=TGroups.FID + EQUAL + QuotedStr(UserKey);
    { FOR GIVEN USER ID, WE CAN HAVE MANY ITEMS }
    if DataSet.RecordCount > 0 then
    begin
      SetLength(List, 2, 2);
      GroupListBox.Enabled:=False;
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
      GroupListBox.Enabled:=True;
    end;
  finally
    DataSet.Close;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------- AGE DATES FOR SELECTED GROUP ID }
procedure TUserControl.GetAgeDates(AgeDatesBox: TComboBox; GroupID: string);
begin
  StrSQL:='SELECT DISTINCT ' + TSnapshots.AGE_DATE + ' FROM ' + TblSnapshots + ' WHERE ' + TSnapshots.GROUP_ID + ' = ' + QuotedStr(GroupID);
  try
    DataSet:=ExecSQL;
    if DataSet.RecordCount > 0 then
    begin
      AgeDatesBox.Clear;
      AgeDatesBox.Enabled:=False;
      while not DataSet.EOF do
      begin
        AgeDatesBox.Items.Add(DataSet.Fields[TSnapshots.AGE_DATE].Value);
        DataSet.MoveNext;
      end;
      { DISPLAY LAST DATE FROM THE LIST }
      AgeDatesBox.ItemIndex:=AgeDatesBox.Items.Count - 1;
      if GetAccessData(adAccessLevel) = acADMIN then AgeDatesBox.Enabled:=True;
    end;
  finally
    DataSet.Close;
  end;
end;

end.
