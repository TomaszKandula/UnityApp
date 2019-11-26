unit Unity.ThreadUtilities;

// ----------------------------------------
// Application event logger.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes;


type


    EThreadStackFinalized = class(Exception);


    TSimpleThread = class;


    TThreadQueue = class
    private
        FFinalized: Boolean;
        FIOQueue: THandle;
    public

        /// <summary>
        /// Create IO Completion Queue.
        /// </summary>
        constructor Create();

        /// <summary>
        /// Destroy Completion Queue.
        /// </summary>
        destructor Destroy(); override;

        /// <summary>
        /// Post a finialize pointer on to the queue.
        /// </summary>
        procedure Finalize();

        /// <summary>
        /// If stack is not finalized, add/push a pointer on to the end of the queue.
        /// </summary>
        procedure Push(Data: Pointer);

        /// <summary>
        /// Pop will return false if the queue is completed.
        /// </summary>
        function Pop(var Data: Pointer): Boolean;

        /// <summary>
        /// Indicates whenever the thread is finilized or not.
        /// </summary>
        property Finalized: Boolean read FFinalized;

    end;


    /// <summary>
    /// Defines callback method for thread event.
    /// </summary>
    TThreadExecuteEvent = procedure(Thread: TThread) of object;


    TSimpleThread = class(TThread)
    private
        FExecuteEvent: TThreadExecuteEvent;
    protected
        procedure Execute(); override;
    public

        /// <summary>
        /// Initialize simple thread and execute thread event.
        /// </summary>
        constructor Create(CreateSuspended: Boolean; ExecuteEvent: TThreadExecuteEvent; AFreeOnTerminate: Boolean);

    end;


    TThreadPoolEvent = procedure (Data: Pointer; AThread: TThread) of Object;


    TThreadPool = class(TObject)
    private
        FThreads: TList;
        FThreadQueue: TThreadQueue;
        FHandlePoolEvent: TThreadPoolEvent;
        procedure DoHandleThreadExecute(Thread: TThread);
    public

        /// <summary>
        /// Initialize thread pool.
        /// </summary>
        constructor Create(HandlePoolEvent: TThreadPoolEvent; MaxThreads: Integer = 1); virtual;

        /// <summary>
        /// Remove from memory.
        /// </summary>
        destructor  Destroy(); override;

        /// <summary>
        /// Add to the given thread to the actual thread pool.
        /// </summary>
        procedure Add(const Data: Pointer);

    end;


implementation


// -------------------------------------------------------------------------------------------------------------------------------------------- THREAD QUEUE //


constructor TThreadQueue.Create();
begin
    FIOQueue  :=CreateIOCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
    FFinalized:=False;
end;


destructor TThreadQueue.Destroy();
begin
    if (FIOQueue <> 0) then
        CloseHandle(FIOQueue);
    inherited;
end;


procedure TThreadQueue.Finalize();
begin
    PostQueuedCompletionStatus(FIOQueue, 0, 0, Pointer($FFFFFFFF));
    FFinalized:=True;
end;


function TThreadQueue.Pop(var Data: Pointer): Boolean;
begin

    Result:=True;
    var A:  Cardinal;
    var OL: POverLapped;

    // Remove/Pop the first pointer from the queue or wait.
    if (not FFinalized) then
        GetQueuedCompletionStatus(FIOQueue, A, ULONG_PTR(Data), OL, INFINITE);

    // Check if we have finalized the queue for completion.
    if (FFinalized) or (OL = Pointer($FFFFFFFF)) then
    begin
        Data:=nil;
        Result:=False;
        Finalize;
    end;

end;


procedure TThreadQueue.Push(Data: Pointer);
begin
    if FFinalized then
        Raise EThreadStackFinalized.Create('Stack is finalized');
    PostQueuedCompletionStatus(FIOQueue, 0, Cardinal(Data), nil);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- SIMPLE THREAD //


constructor TSimpleThread.Create(CreateSuspended: Boolean; ExecuteEvent: TThreadExecuteEvent; AFreeOnTerminate: Boolean);
begin
    FreeOnTerminate:=AFreeOnTerminate;
    FExecuteEvent  :=ExecuteEvent;
    inherited Create(CreateSuspended);
end;

procedure TSimpleThread.Execute();
begin
    if Assigned(FExecuteEvent) then
        FExecuteEvent(Self);
end;


// --------------------------------------------------------------------------------------------------------------------------------------------- THREAD POOL //


procedure TThreadPool.Add(const Data: Pointer);
begin
    FThreadQueue.Push(Data);
end;


constructor TThreadPool.Create(HandlePoolEvent: TThreadPoolEvent; MaxThreads: Integer);
begin

    FHandlePoolEvent :=HandlePoolEvent;
    FThreadQueue     :=TThreadQueue.Create;
    FThreads         :=TList.Create;

    while FThreads.Count < MaxThreads do
        FThreads.Add(TSimpleThread.Create(
            False,
            DoHandleThreadExecute,
            False
        ));

end;


destructor TThreadPool.Destroy();
begin

    FThreadQueue.Finalize;

    for var iCNT: integer:=0 to FThreads.Count-1 do
        TThread(FThreads[iCNT]).Terminate;

    while (FThreads.Count > 0) do
    begin
        TThread(FThreads[0]).WaitFor;
        TThread(FThreads[0]).Free;
        FThreads.Delete(0);
    end;

    FThreadQueue.Free;
    FThreads.Free;

    inherited;

end;


procedure TThreadPool.DoHandleThreadExecute(Thread: TThread);
begin

    var Data: Pointer;

    while FThreadQueue.Pop(Data) and (not TSimpleThread(Thread).Terminated) do
    begin

        try
            FHandlePoolEvent(Data, Thread);

        except
            {Do nothing}
        end;

    end;

end;


end.
