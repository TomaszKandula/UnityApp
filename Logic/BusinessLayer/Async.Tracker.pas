unit Async.Tracker;

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

    TRefreshInvoiceTracker = procedure(InvoiceList: TStringGrid; LastError: TLastError) of object;
    TDeleteFromTrackerList = procedure(LastError: TLastError) of object;

    ITracker = interface(IInterface)
    ['{1EE8D593-A574-4265-B3CE-1A03CFB9B0B9}']
        procedure RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
        procedure DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
    end;


    TTracker = class(TInterfacedObject, ITracker)
    {$TYPEINFO ON}
    public
        procedure RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
        procedure DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
    end;


implementation


uses
    Handler.Database,
    DbModel,
    Unity.Sql,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService;


// -----------------------------------------
// Remove invoice from tracker list.
// *Change this when SQL is replaced by API.
// -----------------------------------------

procedure TTracker.DeleteFromTrackerListAsync(CUID: string; Callback: TDeleteFromTrackerList);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        try

            var TrackerData: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                // Recorded customers
                var PrimaryTable: string:=TSql.DELETE_FROM + TTrackerData.TrackerData + TSql.WHERE + TTrackerData.Cuid  + TSql.EQUAL + QuotedStr(CUID);
                // Recorded invoices
                var ForeignTable: string:=TSql.DELETE_FROM + TTrackerInvoices.TrackerInvoices + TSql.WHERE + TTrackerInvoices.Cuid + TSql.EQUAL + QuotedStr(CUID);

                TrackerData.StrSQL:=ForeignTable + ';' + PrimaryTable;
                TrackerData.ExecSQL;

            finally
                TrackerData.Free;
            end;

            LastError.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[DeleteFromTrackerListAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(LastError);
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Refresh invoice tracker list.
// *Remove when SQL is replaced by API.
// ------------------------------------

procedure TTracker.RefreshInvoiceTrackerAsync(UserAlias: string; Callback: TRefreshInvoiceTracker);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LastError: TLastError;
        var InvoiceList: TStringGrid:=TStringGrid.Create(nil);
        try

            var TrackerData: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                if not(String.IsNullOrEmpty(UserAlias)) then
                    TrackerData.CustFilter:=TSql.WHERE + TTrackerData.UserAlias + TSql.EQUAL + QuotedStr(UserAlias);

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
                TrackerData.SqlToGrid(InvoiceList, TrackerData.DataSet, False, True);

            finally
                TrackerData.Free;
            end;

            LastError.IsSucceeded:=True;

        except
            on E: Exception do
            begin
                LastError.IsSucceeded:=False;
                LastError.ErrorMessage:='[DeleteFromTrackerListAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(LastError.ErrorMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            Callback(InvoiceList, LastError);
            if Assigned(InvoiceList) then InvoiceList.Free();
        end);

    end);

    NewTask.Start;

end;


end.

