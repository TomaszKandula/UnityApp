
{$I \Include\Header.inc}

unit UAC;

interface

uses
    Main, Model, SQL, SysUtils, Windows, StdCtrls, Classes, ADODB, StrUtils, Variants;

type

    /// <summary>
    ///
    /// </summary>

    TUserControl = class(TDataTables)
    {$TYPEINFO ON}
    public
        var UserName: string;
        function GetAccessData(DataType: integer): string;
        function GetGroupList(var List: TLists; GroupListBox: TComboBox): boolean;
        function GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
    end;

implementation


// --------------------------------------------------------------------------------------------------------------- READ THE ACCESS DATA FOR GIVEN USER ALIAS //


/// <summary>
///
/// </summary>

function TUserControl.GetAccessData(DataType: integer): string;
begin
    Result:='';
    if UserName = '' then Exit;

    try
        OpenTable(TblUAC);
        DataSet.Filter:=TUAC.USERNAME + EQUAL + QuotedStr(UserName);

        // USERNAME IS CONSTRAINED, THUS EXCEPTING ONLY ONE ROW IF FOUND
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

/// <summary>
///     Read group ID and Group Name for given User Name.
/// </summary>

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

            // Into ComboBox list
            GroupListBox.Clear;
            for iCNT:=0 to high(List) - 1 do GroupListBox.Items.Add(List[iCNT, 1]);
            GroupListBox.ItemIndex:=0;
        end;

    except
        Result:=False;
    end;

end;

/// <summary>
///     Age dates for FOR selected groups ID.
/// </summary>

function TUserControl.GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
var
    Date: string;
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
                // Make sure that we get "yyyy-mm-dd" format
                Date:= FormatDateTime(gdDateFormat, DataSet.Fields[TSnapshots.AGE_DATE].Value);
                AgeDatesBox.Items.Add(Date);
                DataSet.MoveNext;
            end;

            // Display last date from the sorted list
            AgeDatesBox.ItemIndex:=AgeDatesBox.Items.Count - 1;

        end;

    except
        Result:=False;
    end;

end;

end.
