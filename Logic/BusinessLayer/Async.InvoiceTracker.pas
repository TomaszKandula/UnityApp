unit Async.InvoiceTracker;

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
    Vcl.Graphics,
    Vcl.ComCtrls,
    Vcl.Dialogs,
    Data.Win.ADODB,
    Data.DB,
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    /// <summary>
    /// Callback signature (delegate) for getting invoice list.
    /// </summary>
    TGetInvoiceList = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;


    IInvoiceTracker = interface(IInterface)
    ['{7C1248FC-8FD2-4D71-BC6E-11B605E1CC4B}']

        /// <summary>
        /// Load async. list of invoices recorded for given CUID and return it via given callback method
        /// that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetInvoiceList(CUID: string; Callback: TGetInvoiceList);

        /// <summary>
        /// Save async. tracker data for given pay laod (list provided in TStringGrid). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveTrackerDataAwaited(PayLoad: TStringGrid): TCallResponse;

    end;


    TInvoiceTracker = class(TInterfacedObject, IInvoiceTracker)
    {$TYPEINFO ON}

        /// <summary>
        /// Load async. list of invoices recorded for given CUID and return it via given callback method
        /// that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetInvoiceList(CUID: string; Callback: TGetInvoiceList);

        /// <summary>
        /// Save async. tracker data for given pay laod (list provided in TStringGrid). There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function SaveTrackerDataAwaited(PayLoad: TStringGrid): TCallResponse;

    end;


implementation


uses
    Handler.Database,
    Handler.Account,
    Unity.Chars,
    Unity.Sql,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Sync.Documents,
    Handler.Sql{Legacy},
    DbModel{Legacy};


procedure TInvoiceTracker.GetInvoiceList(CUID: string; Callback: TGetInvoiceList);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var ReturnedData:=TStringGrid.Create(nil);
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                DataTables.StrSQL:=TSql.SELECT                           +
                                        TTrackerInvoices.InvoiceNo       + TChars.COMMA +
                                        TTrackerInvoices.InvoiceState    + TChars.COMMA +
                                        TTrackerInvoices.Stamp           +
                                    TSql.FROM                            +
                                        TTrackerInvoices.TrackerInvoices +
                                    TSql.WHERE                           +
                                        TTrackerInvoices.Cuid            +
                                    TSql.EQUAL                           +
                                        QuotedStr(CUID);

                if not(DataTables.SqlToGrid(ReturnedData, DataTables.ExecSQL, False, True)) then
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='No results found in the database.';
                end
                else
                begin
                    CallResponse.IsSucceeded:=True;
                end;

            except
                on E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log('[GetInvoiceList]: Cannot execute. Error has been thrown: ' + E.Message);
                end;

            end;

        finally
            DataTables.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnedData, CallResponse);
            if Assigned(ReturnedData) then ReturnedData.Free();
        end);

    end);

    NewTask.Start();

end;


function TInvoiceTracker.SaveTrackerDataAwaited(PayLoad: TStringGrid): TCallResponse;
begin

    var NewResult: TCallResponse;
    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                if Assigned(PayLoad) then
                begin

                    DataTables.Columns.Add(TTrackerData.UserAlias);
                    DataTables.Columns.Add(TTrackerData.Cuid);
                    DataTables.Columns.Add(TTrackerData.CoCode);
                    DataTables.Columns.Add(TTrackerData.Branch);
                    DataTables.Columns.Add(TTrackerData.CustomerName);
                    DataTables.Columns.Add(TTrackerData.Stamp);
                    DataTables.Columns.Add(TTrackerData.SendReminder1);
                    DataTables.Columns.Add(TTrackerData.SendReminder2);
                    DataTables.Columns.Add(TTrackerData.SendReminder3);
                    DataTables.Columns.Add(TTrackerData.SendReminder4);
                    DataTables.Columns.Add(TTrackerData.Sciud);
                    DataTables.Columns.Add(TTrackerData.ReminderLayout);
                    DataTables.Columns.Add(TTrackerData.PreStatement);
                    DataTables.Columns.Add(TTrackerData.SendFrom);
                    DataTables.Columns.Add(TTrackerData.StatementTo);
                    DataTables.Columns.Add(TTrackerData.ReminderTo);

                    NewResult.IsSucceeded:=DataTables.InsertInto(TTrackerData.TrackerData, True, PayLoad, nil, False);

                end
                else
                begin
                    NewResult.IsSucceeded:=False;
                    NewResult.LastMessage:='Cannot save tracker data to database. Please contact IT Support.';
                    ThreadFileLog.Log('[SaveTrackerDataAwaited]: Unexpected error. Cannot save tracker data to database.');
                end;

            except
                on E: Exception do
                begin
                    NewResult.IsSucceeded:=False;
                    NewResult.LastMessage:='[SaveTrackerDataAwaited]: Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log(NewResult.LastMessage);
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    {If under ARC / do not manually release it}
    Result:=NewResult;

end;


end.
