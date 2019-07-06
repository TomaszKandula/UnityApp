unit Async.Utilities;


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
    SqlHandler,
    Unity.Interposer,
    Unity.Statics,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    IUtilities = interface(IInterface)
    ['{0B054CF4-86F7-4770-957B-3026BE491B5A}']
        procedure CheckServerConnectionAsync();
        procedure SendUserFeedback();
        procedure ExcelExport();
        procedure GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
    end;


    TUtilities = class(TInterfacedObject, IUtilities)
    {$TYPEINFO ON}
    public
        procedure CheckServerConnectionAsync();
        procedure SendUserFeedback();
        procedure ExcelExport();
        procedure GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
    end;


implementation


uses
    Main,
    DatabaseHandler,
    DbModel,
    Settings,
    AccountHandler,
    Sync.Documents,
    AgeView,
    Transactions,
    Tracker,
    Actions,
    Feedback;


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

            if (not(MainForm.IsConnected)) and (DataBase.Check = 0) then
            begin

                TThread.Synchronize(nil, procedure
                begin
                    MainForm.TryInitConnection;
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Connection with SQL Server database has been re-established.');
                end);

            end;

            if DataBase.Check <> 0 then
            begin
                MainForm.IsConnected:=False;
                MainForm.LogText.Log(MainForm.EventLogPath, 'Connection with SQL Server database has been lost, waiting to reconnect...');
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
            MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Report has been sent successfully!');
            MainForm.LogText.Log(MainForm.EventLogPath, 'Feedback Report has been successfully sent by the user.');
        end
        else
        begin
            MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot send Feedback Report. Please contact IT support.');
            MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot send Feedback Report.');
        end;

    end);

    NewTask.Start;

end;


// -------------------------------------
// Generate Excel report asynchronously
// to not to block application usability
// -------------------------------------

procedure TUtilities.ExcelExport();
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.ExportXLS);
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
                Temp.ToExcel('Age Report', FileName);

            finally
                Temp.Free;

            end;

        finally
            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);
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

        var DataTables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
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
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot load general table, error has been thrown: ' + E.Message);
            end;
        finally
            DataTables.Free;
        end;

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


end.

