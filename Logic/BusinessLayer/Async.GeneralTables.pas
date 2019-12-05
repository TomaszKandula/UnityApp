unit Async.GeneralTables;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    System.Diagnostics,
    System.Win.ComObj,
    System.SyncObjs,
    System.Threading,
    System.Generics.Collections,
    Data.Win.ADODB,
    Data.DB,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for getting results from updating general tables.
    /// </summary>
    TGetTables = procedure(CallResponse: TCallResponse) of object;


    IGeneralTables = interface(IInterface)
    ['{C96D4BF6-9BB3-47EB-B081-A07417E07014}']

        /// <summary>
        /// Allow to async. load general tables to provided TStringGrids. This method can be executed without waiting to complete
        /// the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGetTables; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);

    end;


    TGeneralTables = class(TInterfacedObject, IGeneralTables)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Allow to async. load general tables to provided TStringGrids. This method can be executed without waiting to complete
        /// the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetTablesAsync(TableName: string; DestGrid: TStringGrid; Callback: TGetTables; Columns: string = '';
            Conditions: string = ''; WaitToComplete: boolean = False);

    end;


implementation


uses
    Handler.Database{Legacy}, //remove
    Handler.Sql{Legacy}, //remove
    Unity.Sql{Legacy}, //remove
    Unity.Helpers,
    Unity.Settings,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Common,
    Unity.DateTimeFormats,
    Sync.Documents,
    Bcrypt,
    DbModel{Legacy}; //remove


procedure TGeneralTables.GetTablesAsync(
    TableName: string; DestGrid: TStringGrid; Callback: TGetTables; Columns: string = '';
    Conditions: string = ''; WaitToComplete: boolean = False); // replace with REST
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                DataTables.CleanUp;

                if not(string.IsNullOrEmpty(Columns)) then
                    DataTables.Columns.Text:=Columns;

                if not(string.IsNullOrEmpty(Conditions)) then
                    DataTables.CustFilter:=Conditions;

                if DataTables.OpenTable(TableName) then
                    DataTables.SqlToGrid(DestGrid, DataTables.DataSet, False, True);

            finally
                DataTables.Free;
            end;

            CallResponse.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GeneralTablesAsync]: Cannot execute. Error has been thrown: ' + CallResponse.LastMessage;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


end.
