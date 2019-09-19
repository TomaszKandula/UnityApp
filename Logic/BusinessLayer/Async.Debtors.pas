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
    Handler.Sql,
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;

type


    // --------------------
    // Callback signatures.
    // --------------------

    TMakeAgeViewAsync = procedure(LastError: TLastError) of object;
    TReadAgeViewAsync = procedure(ActionMode: TLoading; ReturnedData: TStringGrid; LastError: TLastError) of object;


    IDebtors = interface(IInterface)
    ['{194FE2BE-386E-499E-93FB-0299DA53A70A}']
        procedure MakeAgeViewAsync(OpenAmount: double; Callback: TMakeAgeViewAsync);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    public
        procedure MakeAgeViewAsync(OpenAmount: double; Callback: TMakeAgeViewAsync);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
    end;


implementation


uses
    View.Main,             // remove!
    View.InvoiceTracker,   // remove!
    View.Actions,          // remove!
    View.UserFeedback,     // remove!
    Handler.Database,
    DbModel,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.Sorting,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Sql,
    Handler.Account,
    Sync.Documents,
    Async.OpenItems,
    AgeView,
    Transactions;


// ------------------------------------
// Make aging report for main view
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TDebtors.MakeAgeViewAsync(OpenAmount: double; Callback: TMakeAgeViewAsync);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var CanReload: boolean:=False;
        var AgeView: TAgeView:=TAgeView.Create(SessionService.FDbConnect);
        var StopWatch: TStopWatch:=TStopWatch.StartNew;

        try

            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Generating, MainForm);
            ThreadFileLog.Log(TStatusBar.Generating);

            try

                // Async
                if MainForm.EditGroupID.Text = MainForm.FGroupIdSel then AgeView.GroupID:=MainForm.FGroupIdSel
                    else
                        if MainForm.EditGroupID.Text <> '' then AgeView.GroupID:=MainForm.EditGroupID.Text
                            else
                                AgeView.GroupID:=MainForm.FGroupIdSel;

                // Generate aging
                AgeView.Make(MainForm.FOSAmount);

                // CSV or server?
                if MainForm.cbDump.Checked then
                begin
                    if MainForm.CSVExport.Execute then
                    THelpers.ExportToCSV(MainForm.CSVExport.FileName, AgeView.ArrAgeView);
                end
                else
                begin

                    // Send to SQL Server, update age date list and reload age view on main tabsheet
                    AgeView.Write(TSnapshots.Snapshots, AgeView.ArrAgeView);
                    CanReload:=True;

                end;

            except
                on E: Exception do
                    ThreadFileLog.Log('Cannot execute [MakeAgeView]. Error has been thrown: ' + E.Message);
            end;

        finally

            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
            var THDSec:  extended:=THDMili / 1000;

            ThreadFileLog.Log('Age View thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
            AgeView.Free;

        end;

        if CanReload then //ReadAgeViewAsync(TLoading.NullParameter, TSorting.TMode.Ranges, MainForm.FGroupIdSel, MainForm.FAgeDateSel, MainForm.ReadAgeViewAsync_Callback);

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load aging report for main view
// *Change when SQL is replaced by API
// ------------------------------------

procedure TDebtors.ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer; GroupIdSel: string; AgeDateSel: string; Callback: TReadAgeViewAsync);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var Grid: TStringGrid:=TStringGrid.Create(nil);
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            LastError.IsSucceeded:=True;
            try

                var StrCol: string;
                var CheckColumns:=Grid.LoadLayout(StrCol, TConfigSections.ColumnWidthName, TConfigSections.ColumnOrderName, TConfigSections.ColumnNames, TConfigSections.ColumnPrefix);

                if not CheckColumns then
                begin
                    LastError.IsSucceeded:=False;
                    LastError.ErrorMessage:='[ReadAgeViewAsync] Cannot load columns. Please contact IT support.';
                    ThreadFileLog.Log('[ReadAgeViewAsync] Cannot load columns. Please contact IT support.');
                end
                else
                begin

                    DataTables.CmdType:=cmdText;
                    DataTables.StrSQL:=
                        TSql.EXECUTE             +
                        'Customer.AgeViewReport' +
                        TChars.SPACE             +
                        QuotedStr(StrCol)        +
                        TChars.COMMA             +
                        QuotedStr(GroupIdSel)    +
                        TChars.COMMA             +
                        QuotedStr(AgeDateSel)    +
                        TChars.COMMA             +
                        QuotedStr(SortMode.ToString);

                    DataTables.SqlToGrid(Grid, DataTables.ExecSQL, False, False);
                    ThreadFileLog.Log('SQL statement applied [' + DataTables.StrSQL + '].');

                end;

            except
                on E: Exception do
                begin
                    LastError.IsSucceeded:=False;
                    LastError.ErrorMessage:='[ReadAgeViewAsync] Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log('[ReadAgeViewAsync] Cannot execute. Error has been thrown: ' + E.Message);
                end;

            end;

        finally
            DataTables.Free;
        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(ActionMode, Grid, LastError);
        end);

    end);

    NewTask.Start;

end;


end.

