unit Unity.EventLogger;

// ----------------------------------------------------------------
// Application event logger. Interface can be referenced by anyone.
// Cannot hold references to View or Logic.
// Note:
//     We do use only one instance of the class during application
//     life time because we must handle parallel calls and save
//     application events to one session file. Therefore, it is
//     necessary to use threadpools and queues where multiple
//     instances cannot be used. Use only interface instance.
// ----------------------------------------------------------------

interface


uses
    Winapi.Windows,
    System.Classes,
    System.SysUtils,
    System.Generics.Collections,
    Unity.ThreadUtilities;


type


    ILogger = interface(IInterface)
    ['{2AD7B439-1D36-4BA1-9CDC-98E242961A25}']
        function GetSessionEventLines: TList<string>;
        property SessionEventLines: TList<string> read GetSessionEventLines;
        procedure Log(Text: string);
    end;


    PLogRequest = ^TLogRequest;


    TLogRequest = record
        LogText:  string;
        FileName: string;
    end;

    /// <summary>
    /// Logger class adding provided text to thread pool stack.
    /// Supplied text is expanded by date and time signature
    /// for each line.
    /// </summary>
    TThreadFileLog = class(TInterfacedObject, ILogger)
    strict private
        var FLogFileName: string;
        var FThreadPool: TThreadPool;
        var FSessionEventLines: TList<string>;
        procedure HandleLogRequest(Data: Pointer; AThread: TThread);
        function GetSessionEventLines: TList<string>;
    public
        constructor Create(LogFileName: string);
        destructor Destroy(); override;
        property SessionEventLines: TList<string> read GetSessionEventLines;
        procedure Log(Text: string);
    end;


implementation


uses
    Unity.Constants;


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


constructor TThreadFileLog.Create(LogFileName: string);
begin
    FLogFileName:=LogFileName;
    FSessionEventLines:=TList<string>.Create();
    FThreadPool:=TThreadPool.Create(HandleLogRequest, 1);
end;


destructor TThreadFileLog.Destroy();
begin
    FSessionEventLines.Free();
    FThreadPool.Free();
    inherited;
end;


function TThreadFileLog.GetSessionEventLines: TList<string>;
begin
    Result:=FSessionEventLines;
end;


procedure TThreadFileLog.HandleLogRequest(Data: Pointer; AThread: TThread);
begin

    var Request: PLogRequest:=Data;
    try
        LogToFile(Request^.FileName, Request^.LogText);
    finally
        Dispose(Request);
    end;

end;


procedure TThreadFileLog.Log(Text: string);
begin

    var GetDateTime: TDateTime:=Now;
    var CurrentDate: string:=FormatDateTime(TDtFormat.DateFormat, GetDateTime);
    var CurrentTime: string:=FormatDateTime(TDtFormat.TimeFormat, GetDateTime);
    var TextToLog:   string:='#' + CurrentDate + ' (' + CurrentTime + '): ' + Text;

    var Request: PLogRequest;
    New(Request);

    Request^.LogText:=TextToLog;
    Request^.FileName:=FLogFileName;

    FSessionEventLines.Add(TextToLog);
    FThreadPool.Add(Request);

end;


end.

