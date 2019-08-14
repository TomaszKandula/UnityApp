unit Unity.EventLogger;

// ----------------------------------------
// Application event logger.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Winapi.Windows,
    System.Classes,
    System.SysUtils,
    System.Generics.Collections,
    Unity.ThreadUtilities;

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
    /// Logger class adding provided text to thread pool stack.
    /// Supplied text is expanded by date and time signature
    /// for each line.
    /// </summary>

    TThreadFileLog = class(TObject)
    private
        var FThreadPool: TThreadPool;
        var FSessionEventLines: TList<string>;
        procedure HandleLogRequest(Data: Pointer; AThread: TThread);
        function GetSessionEventLines: TList<string>;
    public
        constructor Create();
        destructor Destroy(); override;
        property  SessionEventLines: TList<string> read GetSessionEventLines;
        procedure Log(const FileName, Text: string);
    end;


implementation


uses
    View.Main,
    Unity.DateTimeFormats;


// -------------------------------------------------------------------------------------------------------------------------------------------------- LOGGER //


/// <summary>
/// Local unit method for writing event log file. This method is upon thread queue,
/// thus race condition does not apply here.
/// </summary>

procedure LogToFile(const FileName, Text: String);
begin

    var EventLog: TextFile;
    AssignFile(EventLog, FileName);

    if not FileExists(FileName) then
        Rewrite(EventLog)
            else
                Append(EventLog);

    try
        Writeln(EventLog, Text);
    finally
        CloseFile(EventLog);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


constructor TThreadFileLog.Create();
begin
    FSessionEventLines:=TList<string>.Create();
    FThreadPool:=TThreadPool.Create(HandleLogRequest, 1);
end;


destructor TThreadFileLog.Destroy();
begin
    FSessionEventLines.Free;
    FThreadPool.Free;
    inherited;
end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


function TThreadFileLog.GetSessionEventLines: TList<string>;
begin
    Result:=FSessionEventLines;
end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


procedure TThreadFileLog.HandleLogRequest(Data: Pointer; AThread: TThread);
begin

    var Request: PLogRequest:=Data;
    try
        LogToFile(Request^.FileName, Request^.LogText);
    finally
        Dispose(Request);
    end;

end;


// --------------------------------------------------------------------------------------------------------------------------------------------------------- //


procedure TThreadFileLog.Log(const FileName, Text: string);
begin

    var GetDateTime: TDateTime:=Now;
    var CurrentDate: string:=FormatDateTime(TDateTimeFormats.DateFormat, GetDateTime);
    var CurrentTime: string:=FormatDateTime(TDateTimeFormats.TimeFormat, GetDateTime);
    var TextToLog:   string:='#' + CurrentDate + ' (' + CurrentTime + '): ' + Text;

    var Request: PLogRequest;
    New(Request);

    Request^.LogText:=TextToLog;
    Request^.FileName:=FileName;

    FSessionEventLines.Add(TextToLog);
    FThreadPool.Add(Request);

end;


end.

