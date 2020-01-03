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
    Unity.Records;


type


    /// <summary>
    /// Callback signature for getting results of address book open action.
    /// </summary>
    TOpenAddressBook = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results of address book update action.
    /// </summary>
    TUpdateAddressBook = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results of address book insert action.
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
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook);
        /// <summary>
        /// Update async. address book content and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields; Callback: TUpdateAddressBook);
        /// <summary>
        /// Insert async. address book new data and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid; Callback: TAddToAddressBook);
        /// <summary>
        /// Allow to asynchronously remove data from Address Book for given Scuid. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function DelFromAddressBookAwaited(Scuid: string): boolean;
        /// <summary>
        /// Load async. address book customer data only for given SCUID. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustomerDetailsAwaited(SCUID: string): TCustomerDetails;
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
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook);
        /// <summary>
        /// Update async. address book content and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure UpdateAddressBookAsync(SourceGrid: TStringGrid; UpdateValues: TAddressBookUpdateFields; Callback: TUpdateAddressBook);
        /// <summary>
        /// Insert async. address book new data and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for Callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddToAddressBookAsync(SourceGrid: TStringGrid; Callback: TAddToAddressBook);
        /// <summary>
        /// Allow to asynchronously remove data from Address Book for given Scuid. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function DelFromAddressBookAwaited(Scuid: string): boolean;
        /// <summary>
        /// Load async. address book customer data only for given SCUID. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustomerDetailsAwaited(SCUID: string): TCustomerDetails;
    end;


implementation


uses
    Handler.Database{Legacy},
    Unity.Constants,
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Sync.Document,
    DbModel{Legacy};


procedure TAddressBook.OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin






    end);

    NewTask.Start();

end;


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

                        Condition:=DbModel.TAddressBook.Scuid + TSql.EQUAL + SourceGrid.Cells[SourceGrid.GetCol(DbModel.TAddressBook.Scuid), SourceGrid.UpdatedRowsHolder[iCNT]];

                        Book.Columns.Add(DbModel.TAddressBook.Emails);
                        Book.Columns.Add(DbModel.TAddressBook.PhoneNumbers);
                        Book.Columns.Add(DbModel.TAddressBook.Contact);
                        Book.Columns.Add(DbModel.TAddressBook.Estatements);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.GetCol(DbModel.TAddressBook.Emails), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.GetCol(DbModel.TAddressBook.PhoneNumbers), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.GetCol(DbModel.TAddressBook.Contact), SourceGrid.UpdatedRowsHolder[iCNT]]);
                        Book.Values.Add(SourceGrid.Cells[SourceGrid.GetCol(DbModel.TAddressBook.Estatements), SourceGrid.UpdatedRowsHolder[iCNT]]);

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
            Book.Free();
        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


procedure TAddressBook.AddToAddressBookAsync(SourceGrid: TStringGrid; Callback: TAddToAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var jCNT: integer:=0;
        var Check: cardinal:=0;
        var AddrBook: TArray<TArray<string>>;
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
                    var SCUID: string:=SourceGrid.Cells[SourceGrid.GetCol(TSnapshots.fCustomerNumber), iCNT] +
                        THelpers.CoConvert(SourceGrid.Cells[SourceGrid.GetCol(TSnapshots.fCoCode), iCNT]);

                    Book.CleanUp();
                    Book.Columns.Add(DbModel.TAddressBook.Scuid);
                    Book.CustFilter:=TSql.WHERE + DbModel.TAddressBook.Scuid + TSql.EQUAL + QuotedStr(SCUID);
                    Book.OpenTable(DbModel.TAddressBook.AddressBook);

                    // Add to array if not exists
                    if Book.DataSet.RecordCount = 0 then
                    begin
                        Inc(Check);
                        AddrBook[jCNT,  0]:=UpperCase(SessionService.SessionData.AliasName);
                        AddrBook[jCNT,  1]:=SCUID;
                        AddrBook[jCNT,  2]:=SourceGrid.Cells[SourceGrid.GetCol(DbModel.TSnapshots.fCustomerNumber), iCNT];
                        AddrBook[jCNT,  3]:=SourceGrid.Cells[SourceGrid.GetCol(DbModel.TSnapshots.fCustomerName), iCNT];
                        AddrBook[jCNT,  8]:=SourceGrid.Cells[SourceGrid.GetCol(DbModel.TSnapshots.fAgent), iCNT];
                        AddrBook[jCNT,  9]:=SourceGrid.Cells[SourceGrid.GetCol(DbModel.TSnapshots.fDivision), iCNT];
                        AddrBook[jCNT, 10]:=SourceGrid.Cells[SourceGrid.GetCol(DbModel.TSnapshots.fCoCode), iCNT];
                        Inc(jCNT);
                        SetLength(AddrBook, jCNT + 1, 11);
                    end;

                end;

            end;

        finally
            Book.Free();
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
                        ThreadFileLog.Log('[AddToAddressBookAsync]: Cannot write Address Book item(s) into database. Error: ' + E.Message);
                    end;

                end;

            finally
                Book.Free();
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

    NewTask.Start();

end;


function TAddressBook.DelFromAddressBookAwaited(Scuid: string): boolean;
begin

    var NewResult: boolean;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                NewResult:=DataTables.DeleteRecord(
                    DbModel.TAddressBook.AddressBook,
                    DbModel.TAddressBook.Scuid,
                    DataTables.CleanStr(Scuid, False),
                    True
                );

            except
                on E: Exception do
                begin
                    ThreadFileLog.Log('[DelFromAddressBookAwaited]: Cannot execute. Error has been thrown: ' + E.Message);
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=NewResult;

end;


function TAddressBook.GetCustomerDetailsAwaited(SCUID: string): TCustomerDetails;
begin

    var CustomerDetails: TCustomerDetails;
    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                DataTables.Columns.Add(DbModel.TAddressBook.Contact);
                DataTables.Columns.Add(DbModel.TAddressBook.Emails);
                DataTables.Columns.Add(DbModel.TAddressBook.Estatements);
                DataTables.Columns.Add(DbModel.TAddressBook.PhoneNumbers);
                DataTables.CustFilter:=TSql.WHERE + DbModel.TAddressBook.Scuid + TSql.EQUAL + QuotedStr(SCUID);
                DataTables.OpenTable(DbModel.TAddressBook.AddressBook);

                if DataTables.DataSet.RecordCount = 1 then
                begin
                    CustomerDetails.CustPerson  :=THelpers.OleGetStr(DataTables.DataSet.Fields[DbModel.TAddressBook.Contact].Value);
                    CustomerDetails.CustMailGen :=THelpers.OleGetStr(DataTables.DataSet.Fields[DbModel.TAddressBook.Emails].Value);
                    CustomerDetails.CustMailStat:=THelpers.OleGetStr(DataTables.DataSet.Fields[DbModel.TAddressBook.Estatements].Value);
                    CustomerDetails.CustPhones  :=THelpers.OleGetStr(DataTables.DataSet.Fields[DbModel.TAddressBook.PhoneNumbers].Value);
                end;

                CallResponse.IsSucceeded:=True;

            except
                on E: Exception do
                begin
                    CallResponse.LastMessage:='Cannot execute. Error has been thrown: ' + E.Message;
                    CallResponse.IsSucceeded:=False;
                    ThreadFileLog.Log('[GetCustomerDetailsAsync]: Cannot execute. Error has been thrown: ' + E.Message);
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CustomerDetails;

end;


end.

