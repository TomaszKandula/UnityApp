unit Transactions;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


// Legacy unit - to be removed


uses
    Winapi.Windows,
    Winapi.Messages,
    System.Variants,
    System.StrUtils,
    System.SysUtils,
    System.Classes,
    Vcl.StdCtrls,
    Data.Win.ADODB,
    DbModel,
    Handler.Sql,
    Unity.Grid,
    Unity.Arrays,
    Unity.Enums;


type


    TTransactions = class(TDataTables)
    {$TYPEINFO ON}
    public
        // move to async.openitems
        var DestGrid:    TStringGrid;
        var SettingGrid: TStringGrid;
        function GetDateTime(Return: TCalendar): string;
        function GetStatus(DateTime: string): string;
        function LoadToGrid: boolean;
    end;


implementation


uses
    View.Main,
    Unity.Sql,
    Unity.Chars,
    Unity.Helpers,
    Unity.DateTimeFormats,
    Unity.Settings,
    Unity.StatusBar,
    Unity.Messaging,
    Unity.Unknown,
    Unity.EventLogger;


// ---------------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS CLASS //


/// <summary>
/// Get date and time from SSISMaster table.
/// </summary>

function TTransactions.GetDateTime(Return: TCalendar): string;
begin

    CleanUp;

    // Get latest date and time
    Columns.Add(
        TSql.MAX +
            BracketStr(TSSISMaster.StartDateTime, TBrackets.Round) +
        TSql._AS +
            QuotedStr(TSSISMaster.StartDateTime)
    );

    // Open column with function applied
    OpenTable(TSSISMaster.SSISMaster);

    // Examine received data
    if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
    begin

        var Value: string:=VarToStr(DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);
        if Value <> '' then
        begin

            case Return of

              TCalendar.TimeOnly: Result:=FormatDateTime(TDateTimeFormats.TimeFormat, VarToDateTime(Value));
              TCalendar.DateOnly: Result:=FormatDateTime(TDateTimeFormats.DateFormat, VarToDateTime(Value));
              TCalendar.DateTime: Result:=FormatDateTime(TDateTimeFormats.DateTimeFormat, VarToDateTime(Value));

            end;

        end;

    end;

end;


/// <summary>
/// Get status code from SSIS Master table for given datetime.
/// </summary>

function TTransactions.GetStatus(DateTime: string): string;
begin

    CleanUp;
    Columns.Add(TSSISMaster.StatusCode);
    CustFilter:=TSql.WHERE + TSSISMaster.StartDateTime + TSql.EQUAL + QuotedStr(DateTime);
    OpenTable(TSSISMaster.SSISMaster);

    if (not (DataSet = nil)) and (DataSet.RecordCount = 1) then
        Result:=VarToStr(DataSet.Fields.Item[TSSISMaster.StatusCode].Value);

end;


/// <summary>
/// Load open items from database table into string grid.
/// </summary>

function TTransactions.LoadToGrid: boolean;
begin

    // Parameters for SQL stored procedure
    var Settings:  ISettings:=TSettings.Create;
    var CutOff:    string:='71150';
    var INF4:      string:='cards';
    var Agents:    string;
    var Divisions: string;

    /// <remarks>
    /// SettingGrid has fixed dimensions.
    /// </remarks>

    for var iCNT: integer:=0 to 3 do
    begin

        /// <remarks>
        /// To stack companies all agent information must be the same
        /// thus set agent per last found the same principle applies for division.
        /// </remarks>

        if SettingGrid.Cells[iCNT, 3] = 'OFF' then
            Agents:='OFF';

        if SettingGrid.Cells[iCNT, 3] = 'ON' then
            Agents:='ON';

        if SettingGrid.Cells[iCNT, 2] = 'OFF' then
            Divisions:='OFF';

        if SettingGrid.Cells[iCNT, 2] = 'ON'  then
            Divisions:='ON';

    end;

    /// <remarks>
    /// Do not use "cmdstoredproc" to execute stored procedure with adodb
    /// use ordinary "cmdtext" with "exec" statement just like you would
    /// use it in Microsoft SQL Management Studio. Alternatively, use
    /// firedac from embarcadero instead of adodb as it is more robust library.
    /// </remarks>

    CmdType:=cmdText;
    StrSQL:=TSql.EXECUTE + QueryOpenItems                                        + TChars.SPACE +
              QuotedStr(GetDateTime(DateOnly))                                   + TChars.COMMA +
              QuotedStr(THelpers.ConvertCoCode(SettingGrid.Cells[0, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(THelpers.ConvertCoCode(SettingGrid.Cells[1, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(THelpers.ConvertCoCode(SettingGrid.Cells[2, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(THelpers.ConvertCoCode(SettingGrid.Cells[3, 0], 'F', 0)) + TChars.COMMA +
              QuotedStr(CutOff)                                                  + TChars.COMMA +
              QuotedStr(Agents)                                                  + TChars.COMMA +
              QuotedStr(Divisions)                                               + TChars.COMMA +
              QuotedStr(INF4);

    Result:=SqlToGrid(DestGrid, ExecSQL, False, True);

    // Sort via CUID
    DestGrid.MSort(DestGrid.ReturnColumn(TOpenitems.Cuid, 1 , 1), 2, True);

end;


end.
