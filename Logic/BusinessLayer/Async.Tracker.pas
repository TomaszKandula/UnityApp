unit Async.Tracker;

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
    Unity.Grid,
    Unity.Enums,
    Unity.Records,
    Unity.Arrays;


type


    ITracker = interface(IInterface)
    ['{1EE8D593-A574-4265-B3CE-1A03CFB9B0B9}']
        procedure RefreshInvoiceTrackerAsync(UserAlias: string);
        procedure DeleteFromTrackerList(CUID: string);
    end;


    TTracker = class(TInterfacedObject, ITracker)
    {$TYPEINFO ON}
    private
        procedure UpdateTrackerList(UserAlias: string);
    public
        procedure RefreshInvoiceTrackerAsync(UserAlias: string);
        procedure DeleteFromTrackerList(CUID: string);
    end;


implementation   // refactor!!!


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Database,
    DbModel,
    Unity.Sql,
    Unity.Settings,
    Handler.Account,
    Sync.Documents,
    AgeView,
    Transactions;


// ------------------------------------
//
// ------------------------------------

procedure TTracker.UpdateTrackerList(UserAlias: string);
begin

    var TrackerData: TDataTables:=TDataTables.Create(MainForm.FDbConnect);
    try

        if not(String.IsNullOrEmpty(UserAlias)) then
        begin
            TrackerData.CustFilter:=TSql.WHERE + TTrackerData.UserAlias + TSql.EQUAL + QuotedStr(UserAlias);
        end;

        TrackerData.Columns.Add(TTrackerData.Cuid);
        TrackerData.Columns.Add(TTrackerData.UserAlias);
        TrackerData.Columns.Add(TTrackerData.CustomerName);
        TrackerData.Columns.Add(TTrackerData.Stamp);
        TrackerData.Columns.Add(TTrackerData.SendReminder1);
        TrackerData.Columns.Add(TTrackerData.SendReminder2);
        TrackerData.Columns.Add(TTrackerData.SendReminder3);
        TrackerData.Columns.Add(TTrackerData.SendReminder4);
        TrackerData.Columns.Add(TTrackerData.ReminderLayout);
        TrackerData.Columns.Add(TTrackerData.SendFrom);
        TrackerData.Columns.Add(TTrackerData.PreStatement);
        TrackerData.Columns.Add(TTrackerData.StatementTo);
        TrackerData.Columns.Add(TTrackerData.ReminderTo);

        TrackerData.OpenTable(TTrackerData.TrackerData);
        TrackerData.SqlToGrid(MainForm.sgInvoiceTracker, TrackerData.DataSet, False, True);

        if MainForm.sgInvoiceTracker.RowCount > 1 then
        begin
            MainForm.sgInvoiceTracker.SetColWidth(10, 20, 400);
            MainForm.sgInvoiceTracker.Visible:=True;
        end
        else
        begin
            MainForm.sgInvoiceTracker.Visible:=False;
        end;

    finally
        TrackerData.Free;
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TTracker.DeleteFromTrackerList(CUID: string);
begin

    var TrackerData: TDataTables:=TDataTables.Create(MainForm.FDbConnect);
    try

        var PrimaryTable: string:=TSql.DELETE_FROM + TTrackerData.TrackerData + TSql.WHERE + TTrackerData.Cuid  + TSql.EQUAL + QuotedStr(CUID);  {HOLDS RECORDED CUSTOMERS}
        var ForeignTable: string:=TSql.DELETE_FROM + TTrackerInvoices.TrackerInvoices + TSql.WHERE + TTrackerInvoices.Cuid + TSql.EQUAL + QuotedStr(CUID);  {HOLDS CUSTOMERS INVOICES}

        TrackerData.StrSQL:=ForeignTable + ';' + PrimaryTable;
        TrackerData.ExecSQL;

        MainForm.sgInvoiceTracker.DeleteRowFrom(1, 1);

    finally

        TrackerData.Free;

    end;

end;


// ------------------------------------
// Refresh invoice tracker list
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TTracker.RefreshInvoiceTrackerAsync(UserAlias: string);
begin

    try

        var NewTask: ITask:=TTask.Create(procedure
        begin
            UpdateTrackerList(UserAlias)
        end);

        NewTask.Start;

    except
        on E: Exception do
            MainForm.FAppEvents.Log(MainForm.EventLogPath, 'Execution of this tread work has been stopped. Error has been thrown: ' + E.Message + ' (TInvoiceTracker).');
    end;

end;


end.

