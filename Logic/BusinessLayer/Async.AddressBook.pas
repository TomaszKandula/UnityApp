unit Async.AddressBook;

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


    /// <summary>
    /// Callback signature (delegate) for address book open action.
    /// </summary>
    TOpenAddressBook = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for address book update action.
    /// </summary>
    TUpdateAddressBook = procedure(CallResponse: TCallResponse) of object;

    /// <summary>
    /// Callback signature (delegate) for address book insert action.
    /// </summary>
    TAddToAddressBook = procedure(CallResponse: TCallResponse) of object;


    IAddressBook = interface(IInterface)
    ['{56D68733-5DF0-4D44-9A66-69CB5DE587E4}']

        /// <summary>
        /// Load async. address book content and return it via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; OptionalCondition: string = '');

        /// <summary>
        /// Update async. address book content and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields; Callback: TUpdateAddressBook);

        /// <summary>
        /// Insert async. address book new data and return notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid; Callback: TAddToAddressBook);

    end;


    TAddressBook = class(TInterfacedObject, IAddressBook)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Load async. address book content and return it via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; OptionalCondition: string = '');

        /// <summary>
        /// Update async. address book content and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields; Callback: TUpdateAddressBook);

        /// <summary>
        /// Insert async. address book new data and return notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for Callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid; Callback: TAddToAddressBook);

    end;


implementation


uses
    Handler.Database,
    Handler.Account,
    Unity.Sql,
    Unity.Helpers,
    Unity.Settings,
    Unity.Messaging,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Sync.Documents,
    DbModel;


// ------------------------------------
// Load Address Book into TStringGrid
// *Change when SQL is replaced by API.
// ------------------------------------

procedure TAddressBook.OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; OptionalCondition: string = '');
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var ReturnedData:=TStringGrid.Create(nil);
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
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

                if not(DataTables.SqlToGrid(ReturnedData, DataTables.DataSet, True, True)) then
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='No results found in the database.';
                end
                else
                begin
                    CallResponse.IsSucceeded:=True;
                end;

            except
                on E: Exception do
                begin
                    CallResponse.LastMessage:=E.Message;
                    CallResponse.IsSucceeded:=False;
                    ThreadFileLog.Log(E.Message);
                end;

            end;

        finally
            DataTables.Free;
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnedData, CallResponse);
            if Assigned(ReturnedData) then ReturnedData.Free();
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Update data in Address Book
// *Change when SQL is replaced by API.
// ------------------------------------

procedure TAddressBook.UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields; Callback: TUpdateAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var Book: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
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
                        CallResponse.IsSucceeded:=True;
                        CallResponse.LastMessage:='Address Book has been updated succesfully!';
                    end
                    else
                    begin
                        CallResponse.IsSucceeded:=False;
                        CallResponse.LastMessage:='Cannot update Address Book. Please contact IT support.';
                    end;

                end
                else
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='Cannot update nothing. Please make changes first and try again.';
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
                begin
                    CallResponse.IsSucceeded:=True;
                    CallResponse.LastMessage:='Address Book has been updated succesfully!';
                end
                else
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='Cannot update Address Book. Please contact IT support.';
                end;

            end;

        finally
            Book.Free;
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start;

end;


// ------------------------------------
// Add data to Address Book
// *Change when SQL is replaced by API.
// ------------------------------------

procedure TAddressBook.AddToAddressBookAsync(SourceGrid: TStringGrid; Callback: TAddToAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var jCNT: integer:=0;
        var Check: cardinal:=0;
        var AddrBook: TALists;
        var CallResponse: TCallResponse;
        SetLength(AddrBook, 1, 11);

        // Get data from String Grid
        var Book: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            for var iCNT: integer:=SourceGrid.Selection.Top to SourceGrid.Selection.Bottom do
            begin

                if SourceGrid.RowHeights[iCNT] <> SourceGrid.sgRowHidden then
                begin

                    // Build SCUID
                    var SCUID: string:=SourceGrid.Cells[SourceGrid.ReturnColumn(TSnapshots.fCustomerNumber, 1, 1), iCNT] +
                        THelpers.ConvertCoCode(
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
                        AddrBook[jCNT,  0]:=UpperCase(SessionService.SessionUser);
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

            Book:=TDataTables.Create(SessionService.FDbConnect);
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
                        CallResponse.IsSucceeded:=True;
                        CallResponse.LastMessage:='Address Book has been successfully populated by selected item(s).';
                    end
                    else
                    begin
                        CallResponse.IsSucceeded:=False;
                        CallResponse.LastMessage:='Cannot update Address Book. Please contact IT support.';
                    end;

                except
                    on E: Exception do
                    begin
                        CallResponse.IsSucceeded:=False;
                        CallResponse.LastMessage:='Cannot save selected item(s). Exception has been thrown: ' + E.Message;
                        ThreadFileLog.Log('Cannot write Address Book item(s) into database. Error: ' + E.Message);
                    end;

                end;

            finally
                Book.Free;
            end;

        end
        else
        begin
            CallResponse.IsSucceeded:=True;
            CallResponse.LastMessage:='Selected customers are already in Address Book.';
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start;

end;


end.

