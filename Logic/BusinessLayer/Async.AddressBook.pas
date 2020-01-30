unit Async.AddressBook;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
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
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TList<string> = nil);
        /// <summary>
        /// Update async. address book content and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure UpdateAddressBookAsync(PayLoad: TCustomerDetails; Callback: TUpdateAddressBook);
        /// <summary>
        /// Insert async. address book new data and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddToAddressBookAsync(PayLoad: TCustomerDetails; Callback: TAddToAddressBook);
        /// <summary>
        /// Allow to asynchronously remove data from Address Book for given Scuid. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function DelFromAddressBookAwaited(Id: integer): TCallResponse;
        /// <summary>
        /// Load async. address book customer data only for given SCUID. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustomerDetailsAwaited(CustNumber: Int64; SourceDBName: string; var CustDetails: TCustomerDetails): TCallResponse;
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
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TList<string> = nil);
        /// <summary>
        /// Update async. address book content and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure UpdateAddressBookAsync(PayLoad: TCustomerDetails; Callback: TUpdateAddressBook);
        /// <summary>
        /// Insert async. address book new data and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for Callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddToAddressBookAsync(PayLoad: TCustomerDetails; Callback: TAddToAddressBook);
        /// <summary>
        /// Allow to asynchronously remove data from Address Book for given Scuid. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function DelFromAddressBookAwaited(Id: integer): TCallResponse;
        /// <summary>
        /// Load async. address book customer data only for given SCUID. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCustomerDetailsAwaited(CustNumber: Int64; SourceDBName: string; var CustDetails: TCustomerDetails): TCallResponse;
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
    Unity.Constants,
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Api.UserCompanySelection,
    Api.AddressBookList,
    Api.AddressBookItem,
    Api.AddressBookUpdate,
    Api.AddressBookUpdated,
    Api.AddressBookItemDel,
    Api.AddressBookAdd,
    Api.AddressBookAdded;


procedure TAddressBook.OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TList<string> = nil);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        if LoadedCompanies.Count > 0 then
        begin


            Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/selection/';
            Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
            ThreadFileLog.Log('[OpenAddressBookAsync]: Executing POST ' + Restful.ClientBaseURL);

            var UserCompanySelection:=TUserCompanySelection.Create();
            try
                UserCompanySelection.SelectedCoCodes:=LoadedCompanies.ToArray();
                Restful.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
            finally
                UserCompanySelection.Free();
            end;

        end
        else
        begin
            Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/';
            Restful.RequestMethod:=TRESTRequestMethod.rmGET;
            ThreadFileLog.Log('[OpenAddressBookAsync]: Executing GET ' + Restful.ClientBaseURL);
        end;

        var CallResponse: TCallResponse;
        var ReturnedData:=TStringGrid.Create(nil);
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var AddressBookList:=TJson.JsonToObject<TAddressBookList>(Restful.Content);
                try

                    ReturnedData.RowCount:=Length(AddressBookList.Id) + 1{Header};
                    ReturnedData.ColCount:=9;

                    // Setup headers
                    ReturnedData.Cells[0, 0]:='';//empty lp column
                    ReturnedData.Cells[1, 0]:=AddressBookList._Id;//hidden column
                    ReturnedData.Cells[2, 0]:=AddressBookList._SourceDbName;
                    ReturnedData.Cells[3, 0]:=AddressBookList._CustomerNumber;
                    ReturnedData.Cells[4, 0]:=AddressBookList._CustomerName;
                    ReturnedData.Cells[5, 0]:=AddressBookList._ContactPerson;
                    ReturnedData.Cells[6, 0]:=AddressBookList._RegularEmails;
                    ReturnedData.Cells[7, 0]:=AddressBookList._StatementEmails;
                    ReturnedData.Cells[8, 0]:=AddressBookList._PhoneNumbers;

                    for var iCNT:=1 to ReturnedData.RowCount - 1 do
                    begin
                        ReturnedData.Cells[1, iCNT]:=AddressBookList.Id[iCNT - 1].ToString();
                        ReturnedData.Cells[2, iCNT]:=AddressBookList.SourceDbName[iCNT - 1];
                        ReturnedData.Cells[3, iCNT]:=AddressBookList.CustomerNumber[iCNT - 1].ToString();
                        ReturnedData.Cells[4, iCNT]:=AddressBookList.CustomerName[iCNT - 1];
                        ReturnedData.Cells[5, iCNT]:=AddressBookList.ContactPerson[iCNT - 1];
                        ReturnedData.Cells[6, iCNT]:=AddressBookList.RegularEmails[iCNT - 1];
                        ReturnedData.Cells[7, iCNT]:=AddressBookList.StatementEmails[iCNT - 1];
                        ReturnedData.Cells[8, iCNT]:=AddressBookList.PhoneNumbers[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    ThreadFileLog.Log('[OpenAddressBookAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    AddressBookList.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[OpenAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[OpenAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[OpenAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[OpenAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnedData, CallResponse);
            if Assigned(ReturnedData) then ReturnedData.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TAddressBook.UpdateAddressBookAsync(PayLoad: TCustomerDetails; Callback: TUpdateAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/' + PayLoad.Id.ToString();
        Restful.RequestMethod:=TRESTRequestMethod.rmPATCH;
        ThreadFileLog.Log('[UpdateAddressBookAsync]: Executing PATCH ' + Restful.ClientBaseURL);

        var AddressBookUpdate:=TAddressBookUpdate.Create();
        try

            AddressBookUpdate.ContactPerson  :=PayLoad.ContactPerson;
            AddressBookUpdate.RegularEmails  :=PayLoad.RegularEmails;
            AddressBookUpdate.StatementEmails:=PayLoad.StatementEmails;
            AddressBookUpdate.PhoneNumbers   :=PayLoad.PhoneNumbers;

            Restful.CustomBody:=TJson.ObjectToJsonString(AddressBookUpdate);

        finally
            AddressBookUpdate.Free();
        end;

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var AddressBookUpdated:=TJson.JsonToObject<TAddressBookUpdated>(Restful.Content);
                try
                    CallResponse.IsSucceeded :=AddressBookUpdated.IsSucceeded;
                    CallResponse.ErrorNumber :=AddressBookUpdated.Error.ErrorNum;
                    CallResponse.LastMessage :=AddressBookUpdated.Error.ErrorDesc;
                finally
                    AddressBookUpdated.Free();
                end;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                ThreadFileLog.Log('[UpdateAddressBookAsync]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[UpdateAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[UpdateAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[UpdateAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[UpdateAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


procedure TAddressBook.AddToAddressBookAsync(PayLoad: TCustomerDetails; Callback: TAddToAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[AddToAddressBookAsync]: Executing POST ' + Restful.ClientBaseURL);

        var AddressBookAdd:=TAddressBookAdd.Create();
        try

            AddressBookAdd.SourceDbName   :=PayLoad.SourceDBName;
            AddressBookAdd.CustomerNumber :=PayLoad.CustomerNumber;
            AddressBookAdd.CustomerName   :=PayLoad.CustomerName;
            AddressBookAdd.ContactPerson  :=PayLoad.ContactPerson;
            AddressBookAdd.RegularEmails  :=PayLoad.RegularEmails;
            AddressBookAdd.StatementEmails:=PayLoad.StatementEmails;
            AddressBookAdd.PhoneNumbers   :=PayLoad.PhoneNumbers;

            Restful.CustomBody:=TJson.ObjectToJsonString(AddressBookAdd);

        finally
            AddressBookAdd.Free();
        end;

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var AddressBookAdded:=TJson.JsonToObject<TAddressBookAdded>(Restful.Content);
                try
                    CallResponse.IsSucceeded:=AddressBookAdded.IsSucceeded;
                    CallResponse.ErrorNumber:=AddressBookAdded.Error.ErrorNum;
                    CallResponse.LastMessage:=AddressBookAdded.Error.ErrorDesc;
                finally
                    AddressBookAdded.Free();
                end;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                ThreadFileLog.Log('[AddToAddressBookAsync]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[AddToAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[AddToAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[AddToAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[AddToAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TAddressBook.DelFromAddressBookAwaited(Id: integer): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/' + Id.ToString();
        Restful.RequestMethod:=TRESTRequestMethod.rmDELETE;
        ThreadFileLog.Log('[DelFromAddressBookAwaited]: Executing DELETE ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var AddressBookItemDel:=TJson.JsonToObject<TAddressBookItemDel>(Restful.Content);
                try
                    CallResponse.IsSucceeded:=AddressBookItemDel.IsSucceeded;
                    CallResponse.LastMessage:=AddressBookItemDel.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=AddressBookItemDel.Error.ErrorNum;
                finally
                    AddressBookItemDel.Free();
                end;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                ThreadFileLog.Log('[DelFromAddressBookAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[DelFromAddressBookAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[DelFromAddressBookAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[DelFromAddressBookAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[DelFromAddressBookAwaited]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


function TAddressBook.GetCustomerDetailsAwaited(CustNumber: Int64; SourceDBName: string; var CustDetails: TCustomerDetails): TCallResponse;
begin

    var LCustDetails: TCustomerDetails;
    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'addressbook/'
            + SourceDBName
            + '/'
            + CustNumber.ToString()
            + '/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        ThreadFileLog.Log('[GetCustomerDetailsAwaited]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var AddressBookItem:=TJson.JsonToObject<TAddressBookItem>(Restful.Content);
                try

                    LCustDetails.Id             :=AddressBookItem.Id;
                    LCustDetails.ContactPerson  :=AddressBookItem.ContactPerson;
                    LCustDetails.RegularEmails  :=AddressBookItem.RegularEmails;
                    LCustDetails.StatementEmails:=AddressBookItem.StatementEmails;
                    LCustDetails.PhoneNumbers   :=AddressBookItem.PhoneNumbers;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    ThreadFileLog.Log('[GetCustomerDetailsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    AddressBookItem.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetCustomerDetailsAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetCustomerDetailsAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCustomerDetailsAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
                CallResponse.IsSucceeded:=False;
                ThreadFileLog.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCustomerDetailsAwaited]: Cannot execute the request. Description: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    CustDetails:=LCustDetails;
    Result:=CallResponse;

end;


end.

