
{$I .\Include\Header.inc}

unit ThreadUtilities;

interface

uses
    Windows, SysUtils, Classes;

    /// <remarks>
    ///
    /// </remarks>

type

    /// <summary>
    ///
    /// </summary>

    EThreadStackFinalized = class(Exception);

    /// <summary>
    ///
    /// </summary>

    TSimpleThread = class;

    /// <summary>
    ///     Thread Safe Pointer Queue.
    /// </summary>

    TThreadQueue = class
    private
        FFinalized: Boolean;
        FIOQueue: THandle;
    public
        constructor Create;
        destructor  Destroy; override;
        procedure   Finalize;
        procedure   Push(Data: Pointer);
        function    Pop(var Data: Pointer): Boolean;
        property    Finalized: Boolean read FFinalized;
    end;

    /// <summary>
    ///
    /// </summary>

    TThreadExecuteEvent = procedure (Thread: TThread) of object;

    /// <summary>
    ///
    /// </summary>

    TSimpleThread = class(TThread)
    private
        FExecuteEvent: TThreadExecuteEvent;
    protected
        procedure Execute(); override;
    public
        constructor Create(CreateSuspended: Boolean; ExecuteEvent: TThreadExecuteEvent; AFreeOnTerminate: Boolean);
    end;

    /// <summary>
    ///
    /// </summary>

    TThreadPoolEvent = procedure (Data: Pointer; AThread: TThread) of Object;

    /// <summary>
    ///
    /// </summary>

    TThreadPool = class(TObject)
    private
        FThreads: TList;
        FThreadQueue: TThreadQueue;
        FHandlePoolEvent: TThreadPoolEvent;
        procedure DoHandleThreadExecute(Thread: TThread);
    public
        constructor Create( HandlePoolEvent: TThreadPoolEvent; MaxThreads: Integer = 1); virtual;
        destructor  Destroy; override;
        procedure   Add(const Data: Pointer);
    end;


implementation


// -------------------------------------------------------------------------------------------------------------------------------------------- THREAD QUEUE //


/// <summary>
///     Create IO Completion Queue.
/// </summary>

constructor TThreadQueue.Create;
begin
    FIOQueue  :=CreateIOCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
    FFinalized:=False;
end;

/// <summary>
///     Destroy Completion Queue.
/// </summary>

destructor TThreadQueue.Destroy;
begin
    if (FIOQueue <> 0) then
        CloseHandle(FIOQueue);
    inherited;
end;

/// <summary>
///     Post a finialize pointer on to the queue.
/// </summary>

procedure TThreadQueue.Finalize;
begin
    PostQueuedCompletionStatus(FIOQueue, 0, 0, Pointer($FFFFFFFF));
    FFinalized:=True;
end;

/// <summary>
///     Pop will return false if the queue is completed.
/// </summary>

function TThreadQueue.Pop(var Data: Pointer): Boolean;
var
    A:  Cardinal;
    OL: POverLapped;
begin
    Result:=True;

    /// <remarks>
    ///     Remove/Pop the first pointer from the queue or wait.
    /// </remarks>

    if (not FFinalized) then
        GetQueuedCompletionStatus(FIOQueue, A, ULONG_PTR(Data), OL, INFINITE);

    /// <remarks>
    ///     Check if we have finalized the queue for completion.
    /// </remarks>

    if (FFinalized) or (OL = Pointer($FFFFFFFF)) then
    begin
        Data  :=nil;
        Result:=False;
        Finalize;
    end;
end;

/// <summary>
///     If stack is not finalized, add/push a pointer on to the end of the queue.
/// </summary>

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

procedure TSimpleThread.Execute;
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
        FThreads.Add(TSimpleThread.Create(False, DoHandleThreadExecute, False));

end;

destructor TThreadPool.Destroy;
var
    iCNT: Integer;
begin
    FThreadQueue.Finalize;

    for iCNT:=0 to FThreads.Count-1 do
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
var
    Data: Pointer;
begin
    while FThreadQueue.Pop(Data) and (not TSimpleThread(Thread).Terminated) do
    begin
        try
            FHandlePoolEvent(Data, Thread);
        except
            // Do nothing...
        end;
    end;
end;


end.
