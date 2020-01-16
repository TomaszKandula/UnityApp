unit Async.GeneralTables;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    Unity.Grid,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for getting results from given database table.
    /// </summary>
    /// <remarks>
    /// Please note that "table" does not necessarily mean it returns SQL table "as is".
    /// It depends on underlaying API and table (dataset) may contain joined tables.
    /// </remarks>
    TGetTables = procedure(CallResponse: TCallResponse) of object;


    IGeneralTables = interface(IInterface)
    ['{C96D4BF6-9BB3-47EB-B081-A07417E07014}']
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompaniesAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetControlStatusAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPaidInfoAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPaymentTermsAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetSalesResponsibleAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPersonResponsibleAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetAccountTypeAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCustomerGroupAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
    end;


    TGeneralTables = class(TInterfacedObject, IGeneralTables)
    {$TYPEINFO ON}
    public
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompaniesAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Customer.ControlStatus table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetControlStatusAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Common.PaidInfo table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPaidInfoAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Erp.PaymentTerms table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPaymentTermsAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Erp.SalesResponsible table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetSalesResponsibleAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Erp.PersonResponsible table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPersonResponsibleAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Erp.AccountType table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetAccountTypeAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Erp.CustomerGroup table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCustomerGroupAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService;


procedure TGeneralTables.GetCompaniesAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin




        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetControlStatusAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetPaidInfoAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetPaymentTermsAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetSalesResponsibleAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetPersonResponsibleAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetAccountTypeAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetCustomerGroupAsync(var TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin



        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


end.

