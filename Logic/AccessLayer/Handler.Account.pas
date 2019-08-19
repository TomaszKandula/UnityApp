unit Handler.Account;

// ----------------------------------------
// Application logic, access layers.
// Can be referenced by anyone.
// Cannot hold references to View.
// ----------------------------------------

interface


uses
    System.SysUtils,
    Vcl.StdCtrls,
    Handler.Sql,
    Unity.UserAccess,
    Unity.Arrays;


    // legacy code - to be reintroduced after REST is implemented, do not modify


type


    TUserControl = class(TDataTables)
    {$TYPEINFO ON}
    private
        var FUserName: string;
    public
        property UserName: string read FUserName write FUserName;
        function GetAccessData(DataType: TUserAccess.TTypes): string;
        function GetGroupList(var List: TALists; FGroupListBox: TComboBox): boolean;
        function GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
    end;


implementation


uses
    Unity.DateTimeFormats,
    Unity.AdoDb,
    Unity.Sql,
    View.Main,
    DbModel;


// --------------------------------------------------------------------------------------------------------------- READ THE ACCESS DATA FOR GIVEN USER ALIAS //


function TUserControl.GetAccessData(DataType: TUserAccess.TTypes): string;
begin

    Result:='';
    if UserName = '' then Exit;

    try
        CustFilter:=TSql.WHERE + TUAC.UserName + TSql.EQUAL + QuotedStr(UserName);
        OpenTable(TUAC.UAC);

        // User name is unique, thus expecting only one row (if found)
        if not DataSet.EOF then
        begin

            case DataType of

                AccessLevel: Result:=DataSet.Fields.Item[TUAC.AccessLevel].Value;
                AccessMode:  Result:=DataSet.Fields.Item[TUAC.AccessMode].Value;
                UserKeyId:   Result:=DataSet.Fields.Item[TUAC.Id].Value;

            end;

            DataSet.Filter:=adFilterNone;

        end;

    except
        Result:='';
    end;

end;


function TUserControl.GetGroupList(var List: TALists; FGroupListBox: TComboBox): boolean;
begin

    Result:=True;
    try

        var UserKey: string:=GetAccessData(UserKeyId);
        CustFilter:=TSql.WHERE + TGroups.Fid + TSql.EQUAL + QuotedStr(UserKey) + TSql.ORDER + TGroups.GroupName + TSql.ASC;
        OpenTable(TGroups.Groups);

        if DataSet.RecordCount > 0 then
        begin
            SetLength(List, 2, 2);

            var iCNT: integer:=0;
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
            FGroupListBox.Clear;
            for iCNT:=0 to high(List) - 1 do FGroupListBox.Items.Add(List[iCNT, 1]);
            FGroupListBox.ItemIndex:=0;

        end;

    except
        Result:=False;
    end;

end;


function TUserControl.GetAgeDates(AgeDatesBox: TComboBox; GroupID: string): boolean;
begin

    Result:=True;

    Columns.Add(TSql.DISTINCT + TSnapshots.AgeDate);
    CustFilter:=TSql.WHERE + TSnapshots.GroupId + TSql.EQUAL + QuotedStr(GroupID);
    OpenTable(TSnapshots.Snapshots);

    try
        AgeDatesBox.Clear;

        if DataSet.RecordCount > 0 then
        begin
            while not DataSet.EOF do
            begin
                // Make sure that we get "yyyy-mm-dd" format
                var Date: string:=FormatDateTime(TDateTimeFormats.DateFormat, DataSet.Fields[TSnapshots.AgeDate].Value);
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
