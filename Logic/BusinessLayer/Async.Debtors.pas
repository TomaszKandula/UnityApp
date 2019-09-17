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

    //...

    IDebtors = interface(IInterface)
    ['{194FE2BE-386E-499E-93FB-0299DA53A70A}']
        procedure MakeAgeViewAsync(OpenAmount: double);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer);
    end;


    TDebtors = class(TInterfacedObject, IDebtors)
    {$TYPEINFO ON}
    public
        procedure MakeAgeViewAsync(OpenAmount: double);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer);
    end;


implementation


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Database,
    DbModel,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.Sorting,
    Unity.EventLogger,
    Unity.SessionService,
    Handler.Account,
    Sync.Documents,
    Async.OpenItems,
    AgeView,
    Transactions;


// ------------------------------------
// Make aging report for main view
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TDebtors.MakeAgeViewAsync(OpenAmount: double);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

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
                    AgeView.ExportToCSV(MainForm.CSVExport.FileName, AgeView.ArrAgeView);
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

        if CanReload then ReadAgeViewAsync(TLoading.NullParameter, TSorting.TMode.Ranges);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load aging report for main view
// *Change when SQL is replaced by API
// ------------------------------------

procedure TDebtors.ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var AgeView: TAgeView:=TAgeView.Create(SessionService.FDbConnect);
        var StopWatch: TStopWatch:=TStopWatch.StartNew;
        try

            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading, MainForm);
            THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString, MainForm);

            try
                // Sync
                TThread.Synchronize(nil, AgeView.ClearSummary);

                // Async
                AgeView.idThd:=0;
                AgeView.GroupID:=MainForm.FGroupIdSel;
                AgeView.AgeDate:=MainForm.FAgeDateSel;
                AgeView.Read(MainForm.sgAgeView, SortMode);

                // Sync
                TThread.Synchronize(nil, procedure
                begin

                    AgeView.ComputeAgeSummary(MainForm.sgAgeView);
                    AgeView.ComputeAndShowRCA(MainForm.sgAgeView);
                    AgeView.UpdateSummary;
                    AgeView.GetDetails(MainForm.sgCompanyData);

                    // Map data (source General Tables tabsheet)
                    AgeView.MapGroup3(MainForm.sgAgeView, MainForm.sgGroup3);
                    AgeView.MapTable1(MainForm.sgAgeView, MainForm.sgPersonResp);
                    AgeView.MapTable2(MainForm.sgAgeView, MainForm.sgSalesResp);
                    AgeView.MapTable3(MainForm.sgAgeView, MainForm.sgAccountType);
                    AgeView.MapTable4(MainForm.sgAgeView, MainForm.sgCustomerGr);

                    MainForm.sgAgeView.Repaint;

                end);

            except
                on E: Exception do
                    ThreadFileLog.Log('Cannot execute [ReadAgeView]. Error has been thrown: ' + E.Message);
            end;

        finally

            THelpers.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready, MainForm);

            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
            var THDSec:  extended:=THDMili / 1000;

            ThreadFileLog.Log('Thread for selected Group Id "' + AgeView.GroupID + '" has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
            AgeView.Free;

            MainForm.SwitchTimers(TurnedOn);
            THelpers.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString, MainForm);

        end;

        if ActionMode = CallOpenItems then
        begin

            var OpenItems: IOpenItems:=TOpenItems.Create();
            OpenItems.ReadOpenItemsAsync(TLoading.NullParameter);

        end;

    end);

    NewTask.Start;

end;


end.

