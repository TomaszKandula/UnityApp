unit Async.AddressBook;

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


    IAddressBook = interface(IInterface)
    ['{56D68733-5DF0-4D44-9A66-69CB5DE587E4}']
        procedure OpenAddressBookAsync(UserAlias: string; SourceGrid: TStringGrid; OptionalCondition: string = '');
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields);
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid);
    end;


    TAddressBook = class(TInterfacedObject, IAddressBook)
    {$TYPEINFO ON}
    public
        procedure OpenAddressBookAsync(UserAlias: string; SourceGrid: TStringGrid; OptionalCondition: string = '');
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields);
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid);
    end;


implementation


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    View.UserFeedback,
    Handler.Database,
    Handler.Account,
    Unity.Settings,
    Sync.Documents,
    DbModel,
    AgeView,
    Transactions;


// ------------------------------------
// Load Address Book into TStringGrid
// *Change when SQL is replaced by API
// ------------------------------------

procedure TAddressBook.OpenAddressBookAsync(UserAlias: string; SourceGrid: TStringGrid; OptionalCondition: string = '');
begin

    SourceGrid.Freeze(True);
    MainForm.ExecMessage(True, TMessaging.TWParams.StatusBar, TStatusBar.Processing);
    MainForm.ExecMessage(False, TMessaging.TWParams.AwaitForm, TMessaging.TAwaitForm.Show.ToString);

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(MainForm.DbConnect);
        try

            try

                DataTables.Columns.Add(DbModel.TAddressBook.UserAlias);
                DataTables.Columns.Add(DbModel.TAddressBook.Scuid);
                DataTables.Columns.Add(DbModel.TAddressBook.CustomerNumber);
                DataTables.Columns.Add(DbModel.TAddressBook.CustomerName);
                DataTables.Columns.Add(DbModel.TAddressBook.Emails);
                DataTables.Columns.Add(DbModel.TAddressBook.Estatements);
                DataTables.Columns.Add(DbModel.TAddressBook.PhoneNumbers);
                DataTables.Columns.Add(DbModel.TAddressBook.Contact);
                DataTables.Columns.Add(DbModel.TAddressBook.CoCode);
                DataTables.Columns.Add(DbModel.TAddressBook.Agent);
                DataTables.Columns.Add(DbModel.TAddressBook.Division);

                if String.IsNullOrEmpty(UserAlias) and not(String.IsNullOrEmpty(OptionalCondition)) then
                    DataTables.CustFilter:=TSql.WHERE + OptionalCondition;

                if not(String.IsNullOrEmpty(UserAlias)) and String.IsNullOrEmpty(OptionalCondition) then
                    DataTables.CustFilter:=TSql.WHERE + DbModel.TAddressBook.UserAlias + TSql.EQUAL + QuotedStr(UserAlias);

                if not(String.IsNullOrEmpty(UserAlias)) and not(String.IsNullOrEmpty(OptionalCondition)) then
                    DataTables.CustFilter:=TSql.WHERE + DbModel.TAddressBook.UserAlias + TSql.EQUAL + QuotedStr(UserAlias) + TSql._AND + OptionalCondition;

                DataTables.OpenTable(DbModel.TAddressBook.AddressBook);

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

procedure TAddressBook.UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields);
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

                        Condition:=DbModel.TAddressBook.Scuid + TSql.EQUAL + SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TAddressBook.Scuid, 1, 1), SourceGrid.UpdatedRowsHolder[iCNT]];

                        Book.Columns.Add(DbModel.TAddressBook.Emails);
                        Book.Columns.Add(DbModel.TAddressBook.PhoneNumbers);
                        Book.Columns.Add(DbModel.TAddressBook.Contact);
                        Book.Columns.Add(DbModel.TAddressBook.Estatements);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TAddressBook.Emails,       1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TAddressBook.PhoneNumbers, 1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TAddressBook.Contact,      1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TAddressBook.Estatements,  1, 1), SourceGrid.UpdatedRowsHolder[iCNT]]);

                    end;

                    // Success
                    if Book.UpdateRecord(DbModel.TAddressBook.AddressBook, True, Condition) then
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

                Condition:=DbModel.TAddressBook.Scuid + TSql.EQUAL + QuotedStr(UpdateValues.Scuid);

                Book.Columns.Add(DbModel.TAddressBook.PhoneNumbers);
                Book.Columns.Add(DbModel.TAddressBook.Contact);
                Book.Columns.Add(DbModel.TAddressBook.Estatements);
                Book.Columns.Add(DbModel.TAddressBook.Emails);
                Book.Values.Add(UpdateValues.Phones);
                Book.Values.Add(UpdateValues.Contact);
                Book.Values.Add(UpdateValues.Estatement);
                Book.Values.Add(UpdateValues.Email);

                if Book.UpdateRecord(DbModel.TAddressBook.AddressBook, True, Condition) then
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

procedure TAddressBook.AddToAddressBookAsync(SourceGrid: TStringGrid);
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
                    Book.Columns.Add(DbModel.TAddressBook.Scuid);
                    Book.CustFilter:=TSql.WHERE + DbModel.TAddressBook.Scuid + TSql.EQUAL + QuotedStr(SCUID);
                    Book.OpenTable(DbModel.TAddressBook.AddressBook);

                    // Add to array if not exists
                    if Book.DataSet.RecordCount = 0 then
                    begin
                        Inc(Check);
                        AddrBook[jCNT,  0]:=UpperCase(MainForm.WinUserName);
                        AddrBook[jCNT,  1]:=SCUID;
                        AddrBook[jCNT,  2]:=SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TSnapshots.fCustomerNumber,1, 1), iCNT];
                        AddrBook[jCNT,  3]:=SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TSnapshots.fCustomerName,  1, 1), iCNT];
                        AddrBook[jCNT,  8]:=SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TSnapshots.fAgent,         1, 1), iCNT];
                        AddrBook[jCNT,  9]:=SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TSnapshots.fDivision,      1, 1), iCNT];
                        AddrBook[jCNT, 10]:=SourceGrid.Cells[SourceGrid.ReturnColumn(DbModel.TSnapshots.fCoCode,        1, 1), iCNT];
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

                Book.Columns.Add(DbModel.TAddressBook.UserAlias);
                Book.Columns.Add(DbModel.TAddressBook.Scuid);
                Book.Columns.Add(DbModel.TAddressBook.CustomerNumber);
                Book.Columns.Add(DbModel.TAddressBook.CustomerName);
                Book.Columns.Add(DbModel.TAddressBook.Emails);
                Book.Columns.Add(DbModel.TAddressBook.PhoneNumbers);
                Book.Columns.Add(DbModel.TAddressBook.Contact);
                Book.Columns.Add(DbModel.TAddressBook.Estatements);
                Book.Columns.Add(DbModel.TAddressBook.Agent);
                Book.Columns.Add(DbModel.TAddressBook.Division);
                Book.Columns.Add(DbModel.TAddressBook.CoCode);

                try

                    Book.InsertInto(DbModel.TAddressBook.AddressBook, True, nil, AddrBook);

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


end.

