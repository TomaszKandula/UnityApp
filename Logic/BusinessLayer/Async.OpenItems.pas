unit Async.OpenItems;

// ----------------------------------
// Application logic, business layer.
// Can be referenced by anyone.
// Cannot hold references to View.
// ----------------------------------

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
    Unity.Interposer,
    Unity.Statics,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    IOpenItems = interface(IInterface)
    ['{CD6AC138-D2A4-4C6B-A3F1-07F904BA44B1}']
        procedure ScanOpenItemsAsync();
        procedure ReadOpenItemsAsync(ActionMode: TLoading);
    end;


    TOpenItems = class(TInterfacedObject, IOpenItems)
    {$TYPEINFO ON}
    public
        procedure ScanOpenItemsAsync();
        procedure ReadOpenItemsAsync(ActionMode: TLoading);

    end;


implementation


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Database,
    DbModel,
    Unity.Settings,
    Handler.Account,
    Sync.Documents,
    AgeView,
    Transactions,
    Async.Debtors;


// ------------------------------------
// Load aging report for main view
// *Change when SQL is replaced by API
// ------------------------------------

procedure TOpenItems.ScanOpenItemsAsync();
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CanMakeAge: boolean:=False;
        var Transactions: TTransactions:=TTransactions.Create(MainForm.DbConnect);
        try

            try

                var ReadDateTime: string:=Transactions.GetDateTime(DateTime);
                var ReadStatus:   string:=Transactions.GetStatus(ReadDateTime);

                if ( StrToDateTime(MainForm.OpenItemsUpdate) < StrToDateTime(ReadDateTime) ) and ( ReadStatus = 'Completed' ) then
                begin

                    // Switch off all of the timers
                    MainForm.SwitchTimers(TurnedOff);

                    // Refresh open items and make new aging view
                    MainForm.OpenItemsUpdate:=ReadDateTime;
                    MainForm.OpenItemsStatus:='';
                    CanMakeAge:=True;

                end;

            except
                on E: Exception do
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot execute [OpenItemsScanner]. Error has been thrown: ' + E.Message);
            end;

        finally
            Transactions.Free;
        end;

        if CanMakeAge then ReadOpenItemsAsync(TLoading.CallMakeAge);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load open items into TStringGrid
// *Change when SQL is replaced by API
// ------------------------------------

procedure TOpenItems.ReadOpenItemsAsync(ActionMode: TLoading);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var OpenItems: TTransactions:=TTransactions.Create(MainForm.DbConnect);
        var StopWatch: TStopWatch:=TStopWatch.StartNew;
        try

            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Downloading);
            try

                OpenItems.DestGrid   :=MainForm.sgOpenItems;
                OpenItems.SettingGrid:=MainForm.sgCompanyData;
                OpenItems.DestGrid.Freeze(True);

                // Sync with GUI
                TThread.Synchronize(nil, OpenItems.ClearSummary);

                // Async
                OpenItems.LoadToGrid;
                OpenItems.UpdateSummary;

            except
                on E: Exception do
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot execute [ReadOpenItems]. Error has been thorwn: ' + E.Message);
            end;

        finally

            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
            var THDSec:  extended:=THDMili / 1000;

            MainForm.LogText.Log(MainForm.EventLogPath, 'Open Items loading thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);

            // Release VCL and set auto column width
            TThread.Synchronize(nil, procedure
            begin
                OpenItems.DestGrid.SetColWidth(10, 20, 400);
            end);

            OpenItems.DestGrid.Freeze(False);
            OpenItems.Free;

        end;

        // Make age view from open items and send to SQL Server
        if ActionMode = CallMakeAge then
        begin

            MainForm.cbDump.Checked:=False;

            var Debtors: IDebtors:=TDebtors.Create();
            Debtors.MakeAgeViewAsync(MainForm.OSAmount);

        end;

    end);

    NewTask.Start;

end;


end.

