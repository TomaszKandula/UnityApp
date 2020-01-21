unit Async.Tracker;

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
    /// Callback signature for returning reloaded data from invoice tracker database table.
    /// </summary>
    TRefreshInvoiceTracker = procedure(InvoiceList: TStringGrid; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for removing item from invoice tracker database table.
    /// </summary>
    TDeleteFromTrackerList = procedure(CallResponse: TCallResponse) of object;


    ITracker = interface(IInterface)
    ['{1EE8D593-A574-4265-B3CE-1A03CFB9B0B9}']
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


    TTracker = class(TInterfacedObject, ITracker)
    {$TYPEINFO ON}
    public
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
    Unity.Constants,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService;


procedure TTracker.DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
begin
end;


procedure TTracker.RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
begin
end;


end.

