unit Async.OpenItems;

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
    Unity.Helpers,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.EventLogger,
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
        var Transactions: TTransactions:=TTransactions.Create(MainForm.FDbConnect);
        try

            try

                var ReadDateTime: string:=Transactions.GetDateTime(DateTime);
                var ReadStatus:   string:=Transactions.GetStatus(ReadDateTime);

                if ( StrToDateTime(MainForm.FOpenItemsUpdate) < StrToDateTime(ReadDateTime) ) and ( ReadStatus = 'Completed' ) then
                begin

                    // Switch off all of the timers
                    MainForm.SwitchTimers(TurnedOff);

                    // Refresh open items and make new aging view
                    MainForm.FOpenItemsUpdate:=ReadDateTime;
                    MainForm.FOpenItemsStatus:='';
                    CanMakeAge:=True;

                end;

            except
                on E: Exception do
                    ThreadFileLog.Log('Cannot execute [OpenItemsScanner]. Error has been thrown: ' + E.Message);
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

        var OpenItems: TTransactions:=TTransactions.Create(MainForm.FDbConnect);
        var StopWatch: TStopWatch:=TStopWatch.StartNew;
        try

            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Downloading, MainForm);
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
                    ThreadFileLog.Log('Cannot execute [ReadOpenItems]. Error has been thorwn: ' + E.Message);
            end;

        finally

            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
            var THDSec:  extended:=THDMili / 1000;

            ThreadFileLog.Log('Open Items loading thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

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
            Debtors.MakeAgeViewAsync(MainForm.FOSAmount);

        end;

    end);

    NewTask.Start;

end;


end.

