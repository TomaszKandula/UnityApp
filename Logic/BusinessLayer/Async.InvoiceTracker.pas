unit Async.InvoiceTracker;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.SysUtils,
    System.Classes,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for getting invoice list.
    /// </summary>
    TGetInvoiceList = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for returning reloaded data from invoice tracker database table.
    /// </summary>
    TRefreshInvoiceTracker = procedure(InvoiceList: TStringGrid; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature for removing item from invoice tracker database table.
    /// </summary>
    TDeleteFromTrackerList = procedure(CallResponse: TCallResponse) of object;


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
        /// <summary>
        /// Allow to async. re-load listed customers on invoice tracker database table.
        /// Callback provides returned data for visual component. Notification is always
        /// executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
        /// <summary>
        /// Allow to async. remove given item from invoice tracker database table.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
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
        /// <summary>
        /// Allow to async. re-load listed customers on invoice tracker database table.
        /// Callback provides returned data for visual component. Notification is always
        /// executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
        /// <summary>
        /// Allow to async. remove given item from invoice tracker database table.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
    end;


implementation


uses
    Unity.Helpers,
    Unity.Settings,
    Unity.Constants,
    Unity.EventLogger,
    Unity.SessionService;


procedure TInvoiceTracker.GetInvoiceList(CUID: string; Callback: TGetInvoiceList);
begin
end;


function TInvoiceTracker.SaveTrackerDataAwaited(PayLoad: TStringGrid): TCallResponse;
begin
    var NewResult: TCallResponse;
    Result:=NewResult;
end;


procedure TInvoiceTracker.DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
begin
end;


procedure TInvoiceTracker.RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
begin
end;


end.
