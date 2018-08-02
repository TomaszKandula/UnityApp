
{$I .\Include\Header.inc}

unit UAC;

interface

uses
    Arrays, InterposerClasses, Model, SQL, SysUtils, Windows, StdCtrls, Classes, ADODB, StrUtils, Variants;

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

uses
    Main;

// --------------------------------------------------------------------------------------------------------------- READ THE ACCESS DATA FOR GIVEN USER ALIAS //


/// <summary>
///
/// </summary>

function TUserControl.GetAccessData(DataType: integer): string;
begin
    Result:='';
    if UserName = '' then Exit;

    try
        CustFilter:=WHERE + TUAC.USERNAME + EQUAL + QuotedStr(UserName);
        OpenTable(TblUAC);

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
        CustFilter:=WHERE + TGroups.FID + EQUAL + QuotedStr(UserKey) + ORDER + TGroups.GROUP_NAME + ASC;
        OpenTable(TblGroups);

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

    Columns.Add(DISTINCT + TSnapshots.AGE_DATE);
    CustFilter:=WHERE + TSnapshots.GROUP_ID + EQUAL + QuotedStr(GroupID);
    OpenTable(TblSnapshots);

    try
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
