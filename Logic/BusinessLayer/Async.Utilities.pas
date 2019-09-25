unit Async.Utilities;

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
    Handler.Sql,
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    // --------------------
    // Callback signatures.
    // --------------------

    //...

    IUtilities = interface(IInterface)
    ['{0B054CF4-86F7-4770-957B-3026BE491B5A}']

        // --------------------------------
        // Undisclosed getters and setters.
        // --------------------------------

        function  FGetActiveConnection: TADOConnection;
        procedure FSetActiveConnection(NewValue: TADOConnection);

        // -------------------
        // Exposed properties.
        // -------------------

        property ActiveConnection: TADOConnection read FGetActiveConnection write FSetActiveConnection;

        // ----------------
        // Exposed methods.
        // ----------------

        procedure CheckServerConnectionAsync();
        procedure SendUserFeedback();
        procedure ExcelExport(GroupId: string; AgeDate: string);
        procedure GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);

    end;


    TUtilities = class(TInterfacedObject, IUtilities)
    {$TYPEINFO ON}
    private
        var FActiveConnection: TADOConnection;
        function FGetActiveConnection: TADOConnection;
        procedure FSetActiveConnection(NewValue: TADOConnection);
    public
        property ActiveConnection: TADOConnection read FGetActiveConnection;
        procedure CheckServerConnectionAsync();
        procedure SendUserFeedback();
        procedure ExcelExport(GroupId: string; AgeDate: string);
        procedure GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
    end;


implementation


uses
    View.Main, // <== remove
    View.UserFeedback, // <== remove
    Handler.Database,
    Handler.Account,
    Unity.Helpers,
    Unity.Messaging,
    Unity.Settings,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Sync.Documents,
    DbModel;


// ------------------------------------
// Check connection with SQL Server
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TUtilities.CheckServerConnectionAsync();
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataBase: TDataBase:=TDataBase.Create(False);
        try

            if (not(MainForm.FIsConnected)) and (DataBase.Check = 0) then
            begin

                TThread.Synchronize(nil, procedure
                begin
                    MainForm.TryInitConnection;
                    ThreadFileLog.Log('Connection with SQL Server database has been re-established.');
                end);

            end;

            if DataBase.Check <> 0 then
            begin
                MainForm.FIsConnected:=False;
                ThreadFileLog.Log('Connection with SQL Server database has been lost, waiting to reconnect...');
            end;

        finally
            DataBase.Free;
        end;

    end);

    NewTask.Start;

end;

// ------------------------------------
// Send user feedback to predefine
// email address in settings file
// ------------------------------------

procedure TUtilities.SendUserFeedback();
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        if FeedbackForm.SendReport then
        begin
            TThread.Synchronize(nil, FeedbackForm.ReportMemo.Clear);
            THelpers.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Report has been sent successfully!', MainForm);
            ThreadFileLog.Log('Feedback Report has been successfully sent by the user.');
        end
        else
        begin
            THelpers.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot send Feedback Report. Please contact IT support.', MainForm);
            ThreadFileLog.Log('Cannot send Feedback Report.');
        end;

    end);

    NewTask.Start;

end;


// -------------------------------------
// Generate Excel report asynchronously
// to not to block application usability
// -------------------------------------

procedure TUtilities.ExcelExport(GroupId: string; AgeDate: string);
begin

    if not Assigned(FActiveConnection) then Exit();

    var NewTask: ITask:=TTask.Create(procedure
    begin

        THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.ExportXLS, MainForm);
        try

            var FileName: string;
            TThread.Synchronize(nil, procedure
            begin
                if MainForm.XLExport.Execute then
                    FileName:=MainForm.XLExport.FileName
                        else FileName:='';
            end);

            var Temp: TStringGrid:=TStringGrid.Create(nil);
            try
                Temp.ToExcel('Age Report', FileName, GroupId, AgeDate, FActiveConnection);
            finally
                Temp.Free;
            end;

        finally
            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);
        end;

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load async. given general table.
// ------------------------------------

procedure TUtilities.GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try
            try
                DataTables.CleanUp;

                if not(string.IsNullOrEmpty(Columns)) then
                    DataTables.Columns.Text:=Columns;

                if not(string.IsNullOrEmpty(Conditions)) then
                    DataTables.CustFilter:=Conditions;

                if DataTables.OpenTable(TableName) then
                    DataTables.SqlToGrid(DestGrid, DataTables.DataSet, False, True);

            except
                on E: Exception do
                    //MainForm.FAppEvents.Log(MainForm.EventLogPath, 'Cannot load general table, error has been thrown: ' + E.Message);
            end;
        finally
            DataTables.Free;
        end;

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------- RETURN VALUES //


function TUtilities.FGetActiveConnection: TADOConnection;
begin
    Result:=FActiveConnection;
end;


procedure TUtilities.FSetActiveConnection(NewValue: TADOConnection);
begin
    FActiveConnection:=NewValue;
end;


end.

