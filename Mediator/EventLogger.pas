unit EventLogger;


interface


uses
    Winapi.Windows,
    System.Classes,
    System.SysUtils,
    ThreadUtilities;

type

    /// <summary>
    /// Pointer to local record.
    /// </summary>

    PLogRequest = ^TLogRequest;

    /// <summary>
    /// Local record holding filename and text to be written.
    /// </summary>

    TLogRequest = record
        LogText:  string;
        FileName: string;
    end;

    /// <summary>
    /// Logger.
    /// </summary>

    TThreadFileLog = class(TObject)
    private
        FThreadPool: TThreadPool;
        procedure HandleLogRequest(Data: Pointer; AThread: TThread);
    public
        constructor Create();
        destructor  Destroy; override;
        procedure   Log(const FileName, Text: string);
    end;


implementation


uses
    Main,
    Helpers;


// -------------------------------------------------------------------------------------------------------------------------------------------------- LOGGER //


/// <summary>
/// Local unit method for writing event log file. It also saves into variable.
/// This method is upon thread queue, thus race condition does not apply here.
/// </summary>

procedure LogToFile(const FileName, Text: String);
begin

    var GetDateTime: TDateTime:=Now;
    var CurrentDate: string:=FormatDateTime(TDateTimeFormats.DateFormat, GetDateTime);
    var CurrentTime: string:=FormatDateTime(TDateTimeFormats.TimeFormat, GetDateTime);
    var TextToLog:   string:='#' + CurrentDate + ' (' + CurrentTime + '): ' + Text;

    /// <remarks>
    /// Event log file always contains all application events.
    /// </remarks>

    var EventLog: TextFile;
    AssignFile(EventLog, FileName);

    if not FileExists(FileName) then
        Rewrite(EventLog)
            else
                Append(EventLog);

    try
        Writeln(EventLog, TextToLog);
    finally
        CloseFile(EventLog);
    end;

    /// <remarks>
    /// CurrentEvents variable holds only current session events without application start up events.
    /// It is send to database on close event
    /// </remarks>

    if Assigned(MainForm) then
        MainForm.CurrentEvents:=MainForm.CurrentEvents + TextToLog;

end;


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TThreadFileLog.Create();
begin
    FThreadPool:=TThreadPool.Create(HandleLogRequest, 1);
end;


destructor TThreadFileLog.Destroy;
begin
    FThreadPool.Free;
    inherited;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ CREATE REQUEST //


procedure TThreadFileLog.HandleLogRequest(Data: Pointer; AThread: TThread);
begin
    var Request: PLogRequest:=Data;
    try
        LogToFile(Request^.FileName, Request^.LogText);
    finally
        Dispose(Request);
    end;
end;


// --------------------------------------------------------------------------------------------------------------------------- EXECUTE LOGGING INTO THE FILE //


/// <summary>
/// It creates new request, updates logger record with file name and text to be written,
/// then it add it to a thread pool stack (to be executed).
/// </summary>

procedure TThreadFileLog.Log(const FileName, Text: string);
begin
    var Request: PLogRequest;
    New(Request);
    Request^.LogText :=Text;
    Request^.FileName:=FileName;
    FThreadPool.Add(Request);
end;


end.

