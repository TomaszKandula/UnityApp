unit Async.OpenItems;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.StrUtils,
    System.Classes,
    System.Diagnostics,
    System.Win.ComObj,
    System.SyncObjs,
    System.Threading,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.ComCtrls,
    Vcl.Dialogs,
    Data.Win.ADODB,
    Data.DB,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


    /// <summary>
    /// Callback signature (delegate) for scanning SSIS master table to check if open items have been updated.
    /// </summary>
    TScanOpenItems = procedure(CanMakeAge: boolean; ReadDateTime: string; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for reading open items. Payload returned contains summary data from loaded invoices.
    /// </summary>
    TReadOpenItems = procedure(OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse) of object;


    IOpenItems = interface(IInterface)
    ['{CD6AC138-D2A4-4C6B-A3F1-07F904BA44B1}']

        /// <summary>
        /// Returns latest open items date and time of SSIS data extract (query SSIS master database table).
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDateTimeAwaited(Return: TCalendar): string;

        /// <summary>
        /// Returns status code from SSIS master table for given date and time stamp.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetStatusAwaited(DateTime: string): string;

        /// <summary>
        /// Allow to async. check SSIS master table to check if open items have been updated.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);

        /// <summary>
        /// Allow to async. load current open items from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadOpenItemsAsync(OpenItemsGrid: TStringGrid; CoCodeList: string; Callback: TReadOpenItems);

    end;


    TOpenItems = class(TInterfacedObject, IOpenItems)
    {$TYPEINFO ON}
    strict private
        function  FLoadToGrid(OpenItemsGrid: TStringGrid; CoCodeList: string): boolean;
        procedure FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
    public

        /// <summary>
        /// Returns latest open items date and time of SSIS data extract (query SSIS master database table).
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetDateTimeAwaited(Return: TCalendar): string;

        /// <summary>
        /// Returns status code from SSIS master table for given date and time stamp.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetStatusAwaited(DateTime: string): string;

        /// <summary>
        /// Allow to async. check SSIS master table to check if open items have been updated.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);

        /// <summary>
        /// Allow to async. load current open items from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadOpenItemsAsync(OpenItemsGrid: TStringGrid; CoCodeList: string; Callback: TReadOpenItems);

    end;


implementation


uses
    System.Variants,
    Handler.Sql{legacy},
    Handler.Database{legacy},
    DbModel{legacy},
    Unity.Settings,
    Unity.Helpers,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Sql{Legacy},
    Unity.Chars,
    Unity.DateTimeFormats,
    Unity.Unknown,
    Sync.Documents,
    Async.Debtors;


procedure TOpenItems.ScanOpenItemsAsync(OpenItemsUpdate: string; Callback: TScanOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var ReadStatus: string;
        var ReadDateTime: string;
        var CallResponse: TCallResponse;
        var CanMakeAge: boolean:=False;

        try

            ReadDateTime:=GetDateTimeAwaited(DateTime);
            ReadStatus:=GetStatusAwaited(ReadDateTime);

            if ( StrToDateTime(OpenItemsUpdate) < StrToDateTime(ReadDateTime) )
                and ( ReadStatus = 'Completed' ) then CanMakeAge:=True;

            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ScanOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CanMakeAge, ReadDateTime, CallResponse);
        end);

    end);

    NewTask.Start;

end;


procedure TOpenItems.ReadOpenItemsAsync(OpenItemsGrid: TStringGrid; CoCodeList: string; Callback: TReadOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var OpenItemsData: TOpenItemsPayLoad;

        try

            FLoadToGrid(OpenItemsGrid, CoCodeList);
            FCalculateOpenItems(OpenItemsGrid, OpenItemsData);
            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ReadOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(OpenItemsData, CallResponse);
        end);

    end);

    NewTask.Start;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


function TOpenItems.GetDateTimeAwaited(Return: TCalendar): string;
begin

    var NewResult: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            DataTables.Columns.Add
            (
                TSql.MAX +
                    DataTables.BracketStr(TSSISMaster.StartDateTime, TBrackets.Round) +
                TSql._AS +
                    QuotedStr(TSSISMaster.StartDateTime)
            );

            DataTables.OpenTable(TSSISMaster.SSISMaster);

            if (not (DataTables.DataSet = nil)) and (DataTables.DataSet.RecordCount = 1) then
            begin

                var Value: string:=VarToStr(DataTables.DataSet.Fields.Item[TSSISMaster.StartDateTime].Value);

                if Value <> '' then
                begin

                    case Return of

                        TCalendar.TimeOnly: NewResult:=FormatDateTime(TDateTimeFormats.TimeFormat, VarToDateTime(Value));
                        TCalendar.DateOnly: NewResult:=FormatDateTime(TDateTimeFormats.DateFormat, VarToDateTime(Value));
                        TCalendar.DateTime: NewResult:=FormatDateTime(TDateTimeFormats.DateTimeFormat, VarToDateTime(Value));

                    end;

                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewResult;

end;


function TOpenItems.GetStatusAwaited(DateTime: string): string;
begin

    var NewResult: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            DataTables.CleanUp();
            DataTables.Columns.Add(TSSISMaster.StatusCode);
            DataTables.CustFilter:=TSql.WHERE + TSSISMaster.StartDateTime + TSql.EQUAL + QuotedStr(DateTime);
            DataTables.OpenTable(TSSISMaster.SSISMaster);

            if (not (DataTables.DataSet = nil)) and (DataTables.DataSet.RecordCount = 1) then
                NewResult:=VarToStr(DataTables.DataSet.Fields.Item[TSSISMaster.StatusCode].Value);

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewResult;

end;


function TOpenItems.FLoadToGrid(OpenItemsGrid: TStringGrid; CoCodeList: string): boolean;
begin

    var Settings:  ISettings:=TSettings.Create;
    var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
    try

        DataTables.CmdType:=cmdText;
        DataTables.StrSQL:=TSql.EXECUTE + 'Customer.QueryOpenItemsAlt2' + TChars.SPACE +
            QuotedStr(GetDateTimeAwaited(DateOnly))                     + TChars.COMMA +
            QuotedStr(CoCodeList);

        Result:=DataTables.SqlToGrid(OpenItemsGrid, DataTables.ExecSQL, False, True);

    finally
        DataTables.Free();
    end;

end;


procedure TOpenItems.FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
begin

    var Settings: ISettings:=TSettings.Create;
    var VoucherNumber: string:=Settings.GetStringValue(TConfigSections.Unallocated, 'VOUCHER_NUM', '0');

    var VoTpCol   :=InputGrid.GetCol(DbModel.TOpenitems.VoTp);
    var OpenAmCol :=InputGrid.GetCol(DbModel.TOpenitems.OpenAm);
    var PmtStatCol:=InputGrid.GetCol(DbModel.TOpenitems.PmtStat);

    for var iCNT: integer:=1 to InputGrid.RowCount - 1 do
    begin

        var InvoiceAmt: double:=StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);
        OutputData.OsAmount:=OutputData.OsAmount + InvoiceAmt;

        if THelpers.IsVoType(InputGrid.Cells[VoTpCol, iCNT]) = True then inc(OutputData.NumOfInvoices);

        if (StrToIntDef(InputGrid.Cells[PmtStatCol, iCNT], 0) < 0) and (THelpers.IsVoType(InputGrid.Cells[VoTpCol, iCNT]) = True) then
        begin
            inc(OutputData.OverdueItems);
            OutputData.OvdAmount:=OutputData.OvdAmount + StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);
        end;

        if (StrToFloat(InputGrid.Cells[OpenAmCol, iCNT]) < 0) and (InputGrid.Cells[VoTpCol, iCNT] = VoucherNumber) then
            OutputData.UnallocatedAmt:=OutputData.UnallocatedAmt + StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);

    end;

    OutputData.UnallocatedAmt:=Abs(OutputData.UnallocatedAmt);
    OutputData.TotalItems:=InputGrid.RowCount - 1;

end;


end.

