
{$I .\Include\Header.inc}

unit UAC;


interface


uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    System.StrUtils,
    System.Variants,
    Vcl.StdCtrls,
    Data.Win.ADODB,
    Arrays,
    InterposerClasses,
    Model,
    SQL;

type


    TUserControl = class(TDataTables)
    {$TYPEINFO ON}
    private
        var FUserName: string;
    public
        property UserName: string read FUserName write FUserName;
        function GetAccessData(DataType: integer): string;
        function GetGroupList(var List: TALists; GroupListBox: TComboBox): boolean;
        function GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
    end;


implementation


uses
    Main;

// --------------------------------------------------------------------------------------------------------------- READ THE ACCESS DATA FOR GIVEN USER ALIAS //


function TUserControl.GetAccessData(DataType: integer): string;
begin

    Result:='';
    if UserName = '' then Exit;

    try
        CustFilter:=WHERE + TUAC.UserName + EQUAL + QuotedStr(UserName);
        OpenTable(TUAC.UAC);

        // User name is unique, thus expecting only one row (if found)
        if not DataSet.EOF then
        begin
            if DataType = adAccessLevel then Result:=DataSet.Fields.Item[TUAC.AccessLevel].Value;
            if DataType = adAccessMode  then Result:=DataSet.Fields.Item[TUAC.AccessMode].Value;
            if DataType = adUserKeyID   then Result:=DataSet.Fields.Item[TUAC.Id].Value;
            DataSet.Filter:=adFilterNone;
        end;

    except
        Result:='';
    end;

end;


function TUserControl.GetGroupList(var List: TALists; GroupListBox: TComboBox): boolean;
var
    iCNT:     integer;
    UserKey:  string;
begin
    Result:=True;
    iCNT  :=0;

    try
        UserKey:=GetAccessData(adUserKeyID);
        CustFilter:=WHERE + TGroups.Fid + EQUAL + QuotedStr(UserKey) + ORDER + TGroups.GroupName + ASC;
        OpenTable(TGroups.Groups);

        if DataSet.RecordCount > 0 then
        begin
            SetLength(List, 2, 2);
            while not DataSet.EOF do
            begin
                List[iCNT, 0]:=DataSet.Fields[TGroups.GroupId].Value;
                List[iCNT, 1]:=DataSet.Fields[TGroups.GroupName].Value;
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


function TUserControl.GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
var
    Date: string;
begin

    Result:=True;

    Columns.Add(DISTINCT + TSnapshots.AgeDate);
    CustFilter:=WHERE + TSnapshots.GroupId + EQUAL + QuotedStr(GroupID);
    OpenTable(TSnapshots.Snapshots);

    try
        AgeDatesBox.Clear;

        if DataSet.RecordCount > 0 then
        begin
            while not DataSet.EOF do
            begin
                // Make sure that we get "yyyy-mm-dd" format
                Date:= FormatDateTime(gdDateFormat, DataSet.Fields[TSnapshots.AgeDate].Value);
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
