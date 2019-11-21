unit Async.Debtors;

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
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    /// <summary>
    /// Callback signature (delegate) for reading current age report from SQL database.
    /// </summary>
    TReadAgeView = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;


    IDebtors = interface(IInterface)
    ['{194FE2BE-386E-499E-93FB-0299DA53A70A}']

        /// <summary>
        /// Allow to read async. current age report from SQL database. Notification is always executed in main thread
        /// as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadAgeViewAsync(SelectedCoCodes: string; SortMode: integer; Callback: TReadAgeView);

    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Allow to read async. current age report from SQL database. Notification is always executed in main thread
        /// as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadAgeViewAsync(SelectedCoCodes: string; SortMode: integer; Callback: TReadAgeView);

    end;


implementation


uses
    System.StrUtils,
    Handler.Database{legacy},
    Handler.Sql{legacy},
    DbModel{legacy},
    Unity.Helpers,
    Unity.Settings,
    Unity.StatusBar,
    Unity.Sorting,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Sql{Legacy},
    Sync.Documents;


procedure TDebtors.ReadAgeViewAsync(SelectedCoCodes: string; SortMode: integer; Callback: TReadAgeView);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var Grid: TStringGrid:=TStringGrid.Create(nil);
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            CallResponse.IsSucceeded:=True;
            try

                var StrCol: string;
                var CheckColumns:=Grid.LoadLayout(
                    StrCol,
                    TConfigSections.ColumnWidthName,
                    TConfigSections.ColumnOrderName,
                    TConfigSections.ColumnNames,
                    TConfigSections.ColumnPrefix
                );

                if not CheckColumns then
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[ReadAgeViewAsync]: Cannot load columns. Please contact IT support.';
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end
                else
                begin

                    DataTables.CmdType:=cmdText;
                    DataTables.StrSQL:='select ' + StrCol + ' from Customer.Snapshots where	AgeDate = (select max(AgeDate) from Customer.Snapshots) and CoCode in (' + SelectedCoCodes + ')';

                    DataTables.SqlToGrid(Grid, DataTables.ExecSQL, False, False);
                    ThreadFileLog.Log('SQL statement applied [' + DataTables.StrSQL + '].');

                end;

            except
                on E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[ReadAgeViewAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            DataTables.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(Grid, CallResponse);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start();

end;


end.

