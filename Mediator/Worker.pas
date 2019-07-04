unit Worker;


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


    IThreading = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']
        procedure CheckServerConnectionAsync();
        procedure RefreshInvoiceTrackerAsync(UserAlias: string);
        procedure MakeAgeViewAsync(OpenAmount: double);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer);
        procedure ScanOpenItemsAsync();
        procedure ReadOpenItemsAsync(ActionMode: TLoading);
        procedure OpenAddressBookAsync(UserAlias: string; SourceGrid: TStringGrid; OptionalCondition: string = '');
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields);
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid);
        procedure SendUserFeedback();
        procedure ExcelExport();
        procedure GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
        procedure EditDailyComment(Fields: TDailyCommentFields);
        procedure EditGeneralComment(Fields: TGeneralCommentFields);
        procedure SendAccountStatement(Fields: TSendAccountStatementFields; WaitToComplete: boolean = False);
        procedure SendAccountStatements(Fields: TSendAccountStatementFields);
    end;


    TThreading = class(TInterfacedObject, IThreading)
    {$TYPEINFO ON}
    protected
        { Empty }
    private
        procedure FInsertDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields);
        procedure FUpdateDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; Condition: string);
        procedure FInsertGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields);
        procedure FUpdateGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields; Condition: string);
    public
        procedure CheckServerConnectionAsync();
        procedure RefreshInvoiceTrackerAsync(UserAlias: string);
        procedure MakeAgeViewAsync(OpenAmount: double);
        procedure ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer);
        procedure ScanOpenItemsAsync();
        procedure ReadOpenItemsAsync(ActionMode: TLoading);
        procedure OpenAddressBookAsync(UserAlias: string; SourceGrid: TStringGrid; OptionalCondition: string = '');
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields);
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid);
        procedure SendUserFeedback();
        procedure ExcelExport();
        procedure GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
        procedure EditDailyComment(Fields: TDailyCommentFields);
        procedure EditGeneralComment(Fields: TGeneralCommentFields);
        procedure SendAccountStatement(Fields: TSendAccountStatementFields; WaitToComplete: boolean = False);
        procedure SendAccountStatements(Fields: TSendAccountStatementFields);
    end;


implementation


uses
    Main,
    DbHandler,
    DbModel,
    Settings,
    UAC,
    Mailer,
    AgeView,
    Transactions,
    Tracker,
    Actions,
    Feedback;


// ------------------------------------
// Check connection with SQL Server
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TThreading.CheckServerConnectionAsync();
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
// Refresh invoice tracker list
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TThreading.RefreshInvoiceTrackerAsync(UserAlias: string);
begin

    try

        var NewTask: ITask:=TTask.Create(procedure
        begin
            MainForm.UpdateTrackerList(UserAlias)
        end);

        NewTask.Start;

    except
        on E: Exception do
            MainForm.LogText.Log(MainForm.EventLogPath, 'Execution of this tread work has been stopped. Error has been thrown: ' + E.Message + ' (TInvoiceTracker).');
    end;

end;


// ------------------------------------
// Make aging report for main view
// *Remove when SQL is replaced by API
// ------------------------------------

procedure TThreading.MakeAgeViewAsync(OpenAmount: double);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CanReload: boolean:=False;
        var AgeView: TAgeView:=TAgeView.Create(MainForm.DbConnect);
        var UserCtrl: TUserControl:=TUserControl.Create(MainForm.DbConnect);
        var StopWatch: TStopWatch:=TStopWatch.StartNew;

        try

            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Generating);
            MainForm.LogText.Log(MainForm.EventLogPath, TStatusBar.Generating);

            try

                // Async
                if MainForm.EditGroupID.Text = MainForm.GroupIdSel then AgeView.GroupID:=MainForm.GroupIdSel
                    else
                        if MainForm.EditGroupID.Text <> '' then AgeView.GroupID:=MainForm.EditGroupID.Text
                            else
                                AgeView.GroupID:=MainForm.GroupIdSel;

                // Generate aging
                AgeView.Make(MainForm.OSAmount);

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

                    TThread.Synchronize(nil, procedure
                    begin
                        try
                            UserCtrl.GetAgeDates(MainForm.GroupListDates, MainForm.GroupList[0, 0]);
                            MainForm.AgeDateSel:=MainForm.GroupListDates.Text;
                        finally
                            UserCtrl.Free;
                        end;
                    end);

                    CanReload:=True;

                end;

            except
                on E: Exception do
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot execute [MakeAgeView]. Error has been thrown: ' + E.Message);
            end;

        finally

            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);

            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
            var THDSec:  extended:=THDMili / 1000;

            MainForm.LogText.Log(MainForm.EventLogPath, 'Age View thread has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
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

procedure TThreading.ReadAgeViewAsync(ActionMode: TLoading; SortMode: integer);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var AgeView: TAgeView:=TAgeView.Create(MainForm.DbConnect);
        var StopWatch: TStopWatch:=TStopWatch.StartNew;
        try

            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Loading);
            MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString);

            try
                // Sync
                TThread.Synchronize(nil, AgeView.ClearSummary);

                // Async
                AgeView.idThd  :=0;
                AgeView.GroupID:=MainForm.GroupIdSel;
                AgeView.AgeDate:=MainForm.AgeDateSel;
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
                    MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot execute [ReadAgeView]. Error has been thrown: ' + E.Message);
            end;

        finally

            MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);

            var THDMili: extended:=StopWatch.ElapsedMilliseconds;
            var THDSec:  extended:=THDMili / 1000;

            MainForm.LogText.Log(MainForm.EventLogPath, 'Thread for selected Group Id "' + AgeView.GroupID + '" has been executed within ' + FormatFloat('0', THDMili) + ' milliseconds (' + FormatFloat('0.00', THDSec) + ' seconds).');
            AgeView.Free;

            MainForm.SwitchTimers(TurnedOn);
            MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);

        end;

        if ActionMode = CallOpenItems then ReadOpenItemsAsync(TLoading.NullParameter);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load aging report for main view
// *Change when SQL is replaced by API
// ------------------------------------

procedure TThreading.ScanOpenItemsAsync();
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

procedure TThreading.ReadOpenItemsAsync(ActionMode: TLoading);
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
            MakeAgeViewAsync(MainForm.OSAmount);
        end;

    end);

    NewTask.Start;

end;


// ------------------------------------
// Load Address Book into TStringGrid
// *Change when SQL is replaced by API
// ------------------------------------

procedure TThreading.OpenAddressBookAsync(UserAlias: string; SourceGrid: TStringGrid; OptionalCondition: string = '');
begin

    SourceGrid.Freeze(True);
    MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Processing);
    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString);

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            try

                DataTables.Columns.Add(TAddressBook.UserAlias);
                DataTables.Columns.Add(TAddressBook.Scuid);
                DataTables.Columns.Add(TAddressBook.CustomerNumber);
                DataTables.Columns.Add(TAddressBook.CustomerName);
                DataTables.Columns.Add(TAddressBook.Emails);
                DataTables.Columns.Add(TAddressBook.Estatements);
                DataTables.Columns.Add(TAddressBook.PhoneNumbers);
                DataTables.Columns.Add(TAddressBook.Contact);
                DataTables.Columns.Add(TAddressBook.CoCode);
                DataTables.Columns.Add(TAddressBook.Agent);
                DataTables.Columns.Add(TAddressBook.Division);

                if String.IsNullOrEmpty(UserAlias) and not(String.IsNullOrEmpty(OptionalCondition)) then
                    DataTables.CustFilter:=TSql.WHERE + OptionalCondition;

                if not(String.IsNullOrEmpty(UserAlias)) and String.IsNullOrEmpty(OptionalCondition) then
                    DataTables.CustFilter:=TSql.WHERE + TAddressBook.UserAlias + TSql.EQUAL + QuotedStr(UserAlias);

                if not(String.IsNullOrEmpty(UserAlias)) and not(String.IsNullOrEmpty(OptionalCondition)) then
                    DataTables.CustFilter:=TSql.WHERE + TAddressBook.UserAlias + TSql.EQUAL + QuotedStr(UserAlias) + TSql._AND + OptionalCondition;

                DataTables.OpenTable(TAddressBook.AddressBook);

                if not(DataTables.SqlToGrid(SourceGrid, DataTables.DataSet, True, True)) then
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'No results found in the database.');

            except
                on E: Exception do
                    MainForm.LogText.Log(MainForm.EventLogPath, E.Message);

            end;

        finally

            DataTables.Free;

            TThread.Synchronize(nil, procedure
            begin
                SourceGrid.SetColWidth(40, 10, 400);
                SourceGrid.Freeze(False);
                MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Ready);
                MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
            end);

        end;

    end);

    NewTask.Start;

end;


// ------------------------------------
// Update data in Address Book
// *Change when SQL is replaced by API
// ------------------------------------

procedure TThreading.UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Book: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            // Update from Address Book String Grid
            var Condition: string;
            if SourceGrid <> nil then
            begin

                if SourceGrid.UpdatedRowsHolder <> nil then
                begin

                    for var iCNT: integer:=low(SourceGrid.UpdatedRowsHolder) to high(SourceGrid.UpdatedRowsHolder) do
                    begin

                        Condition:=TAddressBook.Scuid + TSql.EQUAL + SourceGrid.Cells[SourceGrid.ReturnColumn(TAddressBook.Scuid, 1, 1), SourceGrid.UpdatedRowsHolder[iCNT]];

                        Book.Columns.Add(TAddressBook.Emails);
                        Book.Columns.Add(TAddressBook.PhoneNumbers);
                        Book.Columns.Add(TAddressBook.Contact);
                        Book.Columns.Add(TAddressBook.Estatements);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(TAddressBook.Emails,       1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(TAddressBook.PhoneNumbers, 1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(TAddressBook.Contact,      1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(TAddressBook.Estatements,  1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);

                    end;

                    // Success
                    if Book.UpdateRecord(TAddressBook.AddressBook, True, Condition) then
                    begin
                        SourceGrid.SetUpdatedRow(0);
                        MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Address Book has been updated succesfully!');
                    end
                    else
                    begin
                        // Error during post
                        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update Address Book. Please contact IT support.');
                    end;

                end
                else
                begin
                    // No changes within Address Book string grid
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Nothing to update. Please make changes first and try again.');
                end;

            end;

            // Update from Action Log View
            if SourceGrid = nil then
            begin

                Condition:=TAddressBook.Scuid + TSql.EQUAL + QuotedStr(UpdateValues.Scuid);

                Book.Columns.Add(TAddressBook.PhoneNumbers);
                Book.Columns.Add(TAddressBook.Contact);
                Book.Columns.Add(TAddressBook.Estatements);
                Book.Columns.Add(TAddressBook.Emails);
                Book.Values.Add(UpdateValues.Phones);
                Book.Values.Add(UpdateValues.Contact);
                Book.Values.Add(UpdateValues.Estatement);
                Book.Values.Add(UpdateValues.Email);

                if Book.UpdateRecord(TAddressBook.AddressBook, True, Condition) then
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Address Book has been updated succesfully!')
                else
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update Address Book. Please contact IT support.');

            end;

        finally
            Book.Free;
        end;

    end);

    NewTask.Start;

end;


// ------------------------------------
// Add data to Address Book
// *Change when SQL is replaced by API
// ------------------------------------

procedure TThreading.AddToAddressBookAsync(SourceGrid: TStringGrid);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var jCNT: integer:=0;
        var Check: cardinal:=0;
        var AddrBook: TALists;
        SetLength(AddrBook, 1, 11);

        // Get data from String Grid
        var Book: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            for var iCNT: integer:=SourceGrid.Selection.Top to SourceGrid.Selection.Bottom do
            begin
                if SourceGrid.RowHeights[iCNT] <> SourceGrid.sgRowHidden then
                begin
                    // Build SCUID
                    var SCUID: string:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), iCNT] +
                        MainForm.ConvertCoCode(
                            SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fCoCode, 1, 1), iCNT],
                            'F',
                            3
                        );
                    Book.CleanUp;
                    Book.Columns.Add(TAddressBook.Scuid);
                    Book.CustFilter:=TSql.WHERE + TAddressBook.Scuid + TSql.EQUAL + QuotedStr(SCUID);
                    Book.OpenTable(TAddressBook.AddressBook);

                    // Add to array if not exists
                    if Book.DataSet.RecordCount = 0 then
                    begin
                        Inc(Check);
                        AddrBook[jCNT,  0]:=UpperCase(MainForm.WinUserName);
                        AddrBook[jCNT,  1]:=SCUID;
                        AddrBook[jCNT,  2]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fCustomerNumber,1, 1), iCNT];
                        AddrBook[jCNT,  3]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fCustomerName,  1, 1), iCNT];
                        AddrBook[jCNT,  8]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fAgent,         1, 1), iCNT];
                        AddrBook[jCNT,  9]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fDivision,      1, 1), iCNT];
                        AddrBook[jCNT, 10]:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fCoCode,        1, 1), iCNT];
                        Inc(jCNT);
                        SetLength(AddrBook, jCNT + 1, 11);
                    end;
                end;
            end;

        finally
            Book.Free;
        end;

        // Send to database
        if Check > 0 then
        begin
            Book:=TDataTables.Create(MainForm.DbConnect);
            try

                Book.Columns.Add(TAddressBook.UserAlias);
                Book.Columns.Add(TAddressBook.Scuid);
                Book.Columns.Add(TAddressBook.CustomerNumber);
                Book.Columns.Add(TAddressBook.CustomerName);
                Book.Columns.Add(TAddressBook.Emails);
                Book.Columns.Add(TAddressBook.PhoneNumbers);
                Book.Columns.Add(TAddressBook.Contact);
                Book.Columns.Add(TAddressBook.Estatements);
                Book.Columns.Add(TAddressBook.Agent);
                Book.Columns.Add(TAddressBook.Division);
                Book.Columns.Add(TAddressBook.CoCode);

                try

                    Book.InsertInto(TAddressBook.AddressBook, True, nil, AddrBook);

                    if Book.RowsAffected > 0 then
                    begin
                        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
                        MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Address Book has been successfully populated by selected item(s).');
                    end
                    else
                    begin
                        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
                        MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Cannot update Address Book. Please contact IT support.');
                    end;

                except
                    on E: Exception do
                    begin
                        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);
                        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot save selected item(s). Exception has been thrown: ' + E.Message);
                        MainForm.LogText.Log(MainForm.EventLogPath, 'Thread [' + IntToStr(MainThreadID) + ']: Cannot write Address Book item(s) into database. Error: ' + E.Message);
                    end;

                end;
            finally
                Book.Free;
            end;
        end
        else
        begin
            MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Selected customers are already in Address Book.');
        end;

    end);

    NewTask.Start;

end;


// ------------------------------------
// Send user feedback to predefine
// email address in settings file
// ------------------------------------

procedure TThreading.SendUserFeedback();
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

procedure TThreading.ExcelExport();
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.ExportXLS);
        try

            var FileName: string;
            TThread.Synchronize(nil, procedure
            begin
                if MainForm.XLExport.Execute then FileName:=MainForm.XLExport.FileName else FileName:='';
            end);

            var Temp: TStringGrid:=TStringGrid.Create(nil);
            try
                Temp.OpenThdId:=0;
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

procedure TThreading.GeneralTables(TableName: string; DestGrid: TStringGrid; Columns: string = ''; Conditions: string = ''; WaitToComplete: boolean = False);
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


// ------------------------------------
// Perform SQL "insert into" command
// ------------------------------------

procedure TThreading.FInsertDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields);
begin

    if Fields.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add('0');
    end;

    if Fields.EmailReminder then
    begin
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add('0');
    end;

    if Fields.EmailAutoStat then
    begin
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add('0');
    end;

    if Fields.EmailManuStat then
    begin
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add('1');
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add('0');
    end;

    if Fields.CallEvent then
    begin
        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add('1');
        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(Fields.CallDuration.ToString());
    end
    else
    begin
        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add('0');
        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add('0');
    end;

    DailyText.Columns.Add(TDailyComment.FixedComment);
    DailyText.Values.Add(Fields.Comment);

    if (DailyText.InsertInto(TDailyComment.DailyComment, True)) and (DailyText.RowsAffected > 0) then
    begin

        if Fields.UpdateGrid then TThread.Synchronize(nil, procedure
        begin
            ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
        end);

        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"DailyComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot post daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
// Perform SQL "update" command
// ------------------------------------

procedure TThreading.FUpdateDailyComment(var DailyText: TDataTables; var Fields: TDailyCommentFields; Condition: string);
begin

    if Fields.Email then
    begin
        DailyText.Columns.Add(TDailyComment.Email);
        DailyText.Values.Add(IntToStr(StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.Email].Value), 0)));
    end;

    // Call event and call duration always comes together
    if Fields.CallEvent then
    begin

        var LCallEvent: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.CallEvent].Value), 0);
        Inc(LCallEvent);

        DailyText.Columns.Add(TDailyComment.CallEvent);
        DailyText.Values.Add(LCallEvent.ToString());

        DailyText.Columns.Add(TDailyComment.CallDuration);
        DailyText.Values.Add(Fields.CallDuration.ToString());

    end;

    if Fields.EmailReminder then
    begin
        var LEmailReminder: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailReminder].Value), 0);
        Inc(LEmailReminder);
        DailyText.Columns.Add(TDailyComment.EmailReminder);
        DailyText.Values.Add(LEmailReminder.ToString());
    end;

    if Fields.EmailAutoStat then
    begin
        var LEmailAutoStat: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailAutoStat].Value), 0);
        Inc(LEmailAutoStat);
        DailyText.Columns.Add(TDailyComment.EmailAutoStat);
        DailyText.Values.Add(LEmailAutoStat.ToString());
    end;

    if Fields.EmailManuStat then
    begin
        var LEmailManuStat: integer:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDailyComment.EmailManuStat].Value), 0);
        Inc(LEmailManuStat);
        DailyText.Columns.Add(TDailyComment.EmailManuStat);
        DailyText.Values.Add(LEmailManuStat.ToString());
    end;

    if not(Fields.Comment = '') then
    begin
        DailyText.Columns.Add(TDailyComment.FixedComment);
        DailyText.Values.Add(Fields.Comment);
    end;

    if (DailyText.UpdateRecord(TDailyComment.DailyComment, True, Condition)) and (DailyText.RowsAffected > 0) then
    begin

        if Fields.UpdateGrid then TThread.Synchronize(nil, procedure
        begin
            ActionsForm.UpdateHistory(ActionsForm.HistoryGrid);
        end);

        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"DailyComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update daily comment (CUID: ' + Fields.CUID + '). Rows affected: ' + DailyText.RowsAffected.ToString() + '. Error message received: ' + DailyText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update daily comment into database.' +  TChars.CRLF + 'Error message received: ' + DailyText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TThreading.EditDailyComment(Fields: TDailyCommentFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DailyText: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            var Condition:    string:=TDailyComment.Cuid + TSql.EQUAL + QuotedStr(Fields.CUID) + TSql._AND + TDailyComment.AgeDate + TSql.EQUAL + QuotedStr(MainForm.AgeDateSel);
            var DataCheckSum: string:=Fields.CUID + StringReplace(MainForm.AgeDateSel, '-', '', [rfReplaceAll]);

            DailyText.CustFilter:=TSql.WHERE + Condition;
            DailyText.OpenTable(TDailyComment.DailyComment);

            if not (DailyText.DataSet.RecordCount = 0) then
            begin

                // ------------------------
                // Update exisiting comment
                // ------------------------

                // Allow to extend comment by adding to existing wording a new comment line
                if Fields.ExtendComment then
                    Fields.Comment:=DailyText.DataSet.Fields[TDailyComment.FixedComment].Value + TChars.CRLF + Fields.Comment;

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(MainForm.WinUserName));

                FUpdateDailyComment(DailyText, Fields, Condition);

            end
            else
            begin

                // -----------------
                // Insert new record
                // -----------------

                DailyText.CleanUp;

                DailyText.Columns.Add(TDailyComment.GroupId);
                DailyText.Values.Add(MainForm.GroupIdSel);

                DailyText.Columns.Add(TDailyComment.Cuid);
                DailyText.Values.Add(Fields.CUID);

                DailyText.Columns.Add(TDailyComment.AgeDate);
                DailyText.Values.Add(MainForm.AgeDateSel);

                DailyText.Columns.Add(TDailyComment.Stamp);
                DailyText.Values.Add(DateTimeToStr(Now));

                DailyText.Columns.Add(TDailyComment.UserAlias);
                DailyText.Values.Add(UpperCase(MainForm.WinUserName));

                DailyText.Columns.Add(TDailyComment.DataCheckSum);
                DailyText.Values.Add(DataCheckSum);

                FInsertDailyComment(DailyText, Fields);

            end;

        finally
            DailyText.Free;
        end;

    end);

    NewTask.Start();

end;


// ------------------------------------
//
// ------------------------------------

procedure TThreading.FInsertGeneralComment(var GenText: TDataTables; var Fields: TGeneralCommentFields);
begin

    if not(Fields.FixedComment = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add(Fields.FixedComment);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add('');
    end;

    if not(Fields.FollowUp = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add(Fields.FollowUp);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add('');
    end;

    if not(Fields.Free1 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add(Fields.Free1);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add('');
    end;

    if not(Fields.Free2 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add(Fields.Free2);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add('');
    end;

    if not(Fields.Free3 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add(Fields.Free3);
    end
    else
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add('');
    end;

    if (GenText.InsertInto(TGeneralComment.GeneralComment, True)) and (GenText.RowsAffected > 0) then
    begin
        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"GeneralComment" table has been posted (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update general comment into database.' +  TChars.CRLF + 'Error message received: ' + GenText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TThreading.FUpdateGeneralComment(var GenText: TDataTables ;var Fields: TGeneralCommentFields; Condition: string);
begin

    if not(Fields.FixedComment = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FixedComment);
        GenText.Values.Add(Fields.FixedComment);
    end;

    if not(Fields.FollowUp = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.FollowUp);
        GenText.Values.Add(Fields.FollowUp);
    end;

    if not(Fields.Free1 = TUnknown.Null) then
    begin
        GenText.Columns.Add(TGeneralComment.Free1);
        GenText.Values.Add(Fields.Free1);
    end;

    if not(Fields.Free2 = TUnknown.NULL) then
    begin
        GenText.Columns.Add(TGeneralComment.Free2);
        GenText.Values.Add(Fields.Free2);
    end;

    if not(Fields.Free3 = TUnknown.Null) then
    begin
        GenText.Columns.Add(TGeneralComment.Free3);
        GenText.Values.Add(Fields.Free3);
    end;

    if (GenText.UpdateRecord(TGeneralComment.GeneralComment, True, Condition)) and (GenText.RowsAffected > 0) then
    begin
        if Fields.EventLog then
            MainForm.LogText.Log(MainForm.EventLogPath, '"GeneralComment" table has been updated (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '.');
    end
    else
    begin
        MainForm.LogText.Log(MainForm.EventLogPath, 'Cannot update general comment (CUID: ' + Fields.CUID + '). Rows affected: ' + GenText.RowsAffected.ToString() + '. Error message received: ' + GenText.LastErrorMsg + '.');
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Cannot update general comment into database.' +  TChars.CRLF + 'Error message received: ' + GenText.LastErrorMsg + TChars.CRLF + 'Please contact IT support.');
    end;

end;


// ------------------------------------
//
// ------------------------------------

procedure TThreading.EditGeneralComment(Fields: TGeneralCommentFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var GenText: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            var Condition: string:=TGeneralComment.Cuid + TSql.EQUAL + QuotedStr(Fields.CUID);
            GenText.CustFilter:=TSql.WHERE + Condition;
            GenText.OpenTable(TGeneralComment.GeneralComment);

            if not (GenText.DataSet.RecordCount = 0) then
            begin

                // -----------------
                // Update record
                // -----------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(MainForm.WinUserName));

                FUpdateGeneralComment(GenText, Fields, Condition);

            end
            else
            begin

                // -----------------
                // Insert new record
                // -----------------

                GenText.CleanUp;

                GenText.Columns.Add(TGeneralComment.Cuid);
                GenText.Values.Add(Fields.CUID);

                GenText.Columns.Add(TGeneralComment.Stamp);
                GenText.Values.Add(DateTimeToStr(Now));

                GenText.Columns.Add(TGeneralComment.UserAlias);
                GenText.Values.Add(UpperCase(MainForm.WinUserName));

                FInsertGeneralComment(GenText, Fields);

            end;

        finally
            GenText.Free;
        end;

    end);

    NewTask.Start();

end;


// ------------------------------------
//
// ------------------------------------

procedure TThreading.SendAccountStatement(Fields: TSendAccountStatementFields; WaitToComplete: boolean = False);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Settings: ISettings:=TSettings.Create;
        var Statement: TDocument:=TDocument.Create;
        try

            Statement.CUID       :=Fields.CUID;
            Statement.MailFrom   :=Fields.SendFrom;
            Statement.MailTo     :=Fields.MailTo;
            Statement.CustName   :=Fields.CustName;
            Statement.LBUName    :=Fields.LBUName;
            Statement.LBUAddress :=Fields.LBUAddress;
            Statement.Telephone  :=Fields.Telephone;
            Statement.BankDetails:=Fields.BankDetails;
            Statement.CustMess   :=Fields.Mess;
            Statement.OpenItems  :=Fields.OpenItems;
            Statement.InvFilter  :=Fields.InvFilter;
            Statement.BeginWith  :=Fields.BeginDate;
            Statement.EndWith    :=Fields.EndDate;

            // quick fix - to be refactored - data should be taken from the table
            Statement.REM_EX1:='9999';
            Statement.REM_EX2:='9999';
            Statement.REM_EX3:='9999';
            Statement.REM_EX4:='9999';
            Statement.REM_EX5:='514';

            Statement.MailSubject:=Fields.Subject + ' - ' + Fields.CustName + ' - ' + Fields.CustNumber;

            /// <remarks>
            /// Load either fixed template or customizable template.
            /// </remarks>
            /// <param name="FLayout">
            /// Use maDefined for fully pre-defined template.
            /// Use maCustom for customised template. It requires FSalut, FMess and FSubject to be provided.
            /// </param>

            if Fields.Layout = TDocMode.Defined then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE2', ''));

            if Fields.Layout = TDocMode.Custom then
                Statement.HTMLLayout:=Statement.LoadTemplate(Settings.GetLayoutDir + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE3', ''));

            if Statement.SendDocument then
            begin

                var DailyCommentFields: TDailyCommentFields;
                DailyCommentFields.CUID         :=Fields.CUID;
                DailyCommentFields.Email        :=False;
                DailyCommentFields.CallEvent    :=False;
                DailyCommentFields.CallDuration :=0;
                DailyCommentFields.Comment      :='New communication has been sent to the customer.';
                DailyCommentFields.UpdateGrid   :=not Fields.Series;
                DailyCommentFields.EmailReminder:=False;
                DailyCommentFields.EventLog     :=False;
                DailyCommentFields.ExtendComment:=True;

                /// <summary>
                /// Register sent email either as manual statement or automatic statement.
                /// </summary>

                if Fields.Layout = TDocMode.Defined then
                begin

                    DailyCommentFields.EmailAutoStat:=True;
                    DailyCommentFields.EmailManuStat:=False;

                    var Job: IThreading:=TThreading.Create;
                    Job.EditDailyComment(DailyCommentFields);

                end;

                if Fields.Layout = TDocMode.Custom then
                begin

                    DailyCommentFields.EmailAutoStat:=False;
                    DailyCommentFields.EmailManuStat:=True;

                    var Job: IThreading:=TThreading.Create;
                    Job.EditDailyComment(DailyCommentFields);

                end;

                /// <remarks>
                /// Either single email (manual by user) or executed by mass mailer (multiple emails).
                /// </remarks>

                if Fields.Series then
                    MainForm.ExecMessage(False, TMessaging.TWParams.MailerReportItem, Fields.ItemNo.ToString)
                else
                    MainForm.ExecMessage(False, TMessaging.TWParams.MessageInfo, 'Account Statement has been sent successfully!');

            end
            else if not(Fields.Series) then
                MainForm.ExecMessage(False, TMessaging.TWParams.MessageError, 'Account Statement cannot be sent. Please contact IT support.')

        finally
            Statement.Free;
        end;

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAll(NewTask);

end;


// ------------------------------------
//
// ------------------------------------

procedure TThreading.SendAccountStatements(Fields: TSendAccountStatementFields);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        Fields.Series:=True;
        Fields.ItemNo:=0;

        var Job: IThreading:=TThreading.Create;
        for var iCNT: integer:=0 to Fields.MailerList.Items.Count - 1 do
        begin

            if Fields.MailerList.Items[iCNT].SubItems[4] <> 'Not found!' then
            begin

                Fields.CUID       :=Fields.MailerList.Items[iCNT].SubItems[10]; // cuid
                Fields.SendFrom   :=Fields.MailerList.Items[iCNT].SubItems[3];  // send from
                Fields.MailTo     :=Fields.MailerList.Items[iCNT].SubItems[4];  // send to
                Fields.CustName   :=Fields.MailerList.Items[iCNT].SubItems[0];  // cust name
                Fields.CustNumber :=Fields.MailerList.Items[iCNT].SubItems[1];  // cust number
                Fields.LBUName    :=Fields.MailerList.Items[iCNT].SubItems[5];  // lbu name
                Fields.LBUAddress :=Fields.MailerList.Items[iCNT].SubItems[6];  // lbu address
                Fields.Telephone  :=Fields.MailerList.Items[iCNT].SubItems[7];  // lbu phone
                Fields.BankDetails:=Fields.MailerList.Items[iCNT].SubItems[12]; // bank html
                Fields.ItemNo:=iCNT;

                Job.SendAccountStatement(Fields, True);

            end;

        end;

        MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Hide.ToString);

    end);

    NewTask.Start();

end;


end.

