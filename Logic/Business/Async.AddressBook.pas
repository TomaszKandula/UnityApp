unit Async.AddressBook;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
    Unity.Types,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


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
        /// Allow to asynchronously remove data from Address Book for given arguments. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function DelFromAddressBookAwaited(Id: integer): TCallResponse;
        /// <summary>
        /// Load async. address book customer data only for given arguments.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// It is not recommended to use nil in this method.
        /// </remarks>
        procedure GetCustomerDetailsAsync(CustNumber: Int64; SourceDBName: string; Callback: TGetCustomerDetails);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TAddressBook = class(TInterfacedObject, IAddressBook)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TList<string> = nil); virtual;
        procedure UpdateAddressBookAsync(PayLoad: TCustomerDetails; Callback: TUpdateAddressBook); virtual;
        procedure AddToAddressBookAsync(PayLoad: TCustomerDetails; Callback: TAddToAddressBook); virtual;
        function DelFromAddressBookAwaited(Id: integer): TCallResponse; virtual;
        procedure GetCustomerDetailsAsync(CustNumber: Int64; SourceDBName: string; Callback: TGetCustomerDetails); virtual;
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.Constants,
    Unity.Helpers,
    Unity.Service,
    Api.UserCompanySelection,
    Api.AddressBookList,
    Api.AddressBookItem,
    Api.AddressBookUpdate,
    Api.AddressBookUpdated,
    Api.AddressBookItemDel,
    Api.AddressBookAdd,
    Api.AddressBookAdded;


constructor TAddressBook.Create();
begin
end;


destructor TAddressBook.Destroy();
begin
    inherited;
end;


procedure TAddressBook.OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TList<string> = nil);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        if LoadedCompanies.Count > 0 then
        begin


            Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/selection/';
            Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
            Service.Logger.Log('[OpenAddressBookAsync]: Executing POST ' + Rest.ClientBaseURL);

            var UserCompanySelection:=TUserCompanySelection.Create();
            try
                UserCompanySelection.SelectedCoCodes:=LoadedCompanies.ToArray();
                Rest.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
            finally
                UserCompanySelection.Free();
            end;

        end
        else
        begin
            Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/';
            Rest.RequestMethod:=TRESTRequestMethod.rmGET;
            Service.Logger.Log('[OpenAddressBookAsync]: Executing GET ' + Rest.ClientBaseURL);
        end;

        var CallResponse: TCallResponse;
        var ReturnedData:=TStringGrid.Create(nil);
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var AddressBookList:=TJson.JsonToObject<TAddressBookList>(Rest.Content);
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
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[OpenAddressBookAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    AddressBookList.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[OpenAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[OpenAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[OpenAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[OpenAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
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

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/' + PayLoad.Id.ToString();
        Rest.RequestMethod:=TRESTRequestMethod.rmPATCH;
        Service.Logger.Log('[UpdateAddressBookAsync]: Executing PATCH ' + Rest.ClientBaseURL);

        var LAddressBookUpdate:=TAddressBookUpdate.Create();
        try

            LAddressBookUpdate.ContactPerson  :=PayLoad.ContactPerson;
            LAddressBookUpdate.RegularEmails  :=PayLoad.RegularEmails;
            LAddressBookUpdate.StatementEmails:=PayLoad.StatementEmails;
            LAddressBookUpdate.PhoneNumbers   :=PayLoad.PhoneNumbers;

            Rest.CustomBody:=TJson.ObjectToJsonString(LAddressBookUpdate);

        finally
            LAddressBookUpdate.Free();
        end;

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var AddressBookUpdated:=TJson.JsonToObject<TAddressBookUpdated>(Rest.Content);
                try
                    CallResponse.IsSucceeded :=AddressBookUpdated.IsSucceeded;
                    CallResponse.ErrorCode   :=AddressBookUpdated.Error.ErrorCode;
                    CallResponse.LastMessage :=AddressBookUpdated.Error.ErrorDesc;
                finally
                    AddressBookUpdated.Free();
                end;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                Service.Logger.Log('[UpdateAddressBookAsync]: Returned status code is ' + Rest.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[UpdateAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[UpdateAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[UpdateAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[UpdateAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
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

    var ReturnedId: integer;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[AddToAddressBookAsync]: Executing POST ' + Rest.ClientBaseURL);

        var LAddressBookAdd:=TAddressBookAdd.Create();
        try

            LAddressBookAdd.SourceDbName   :=PayLoad.SourceDBName;
            LAddressBookAdd.CustomerNumber :=PayLoad.CustomerNumber;
            LAddressBookAdd.CustomerName   :=PayLoad.CustomerName;
            LAddressBookAdd.ContactPerson  :=PayLoad.ContactPerson;
            LAddressBookAdd.RegularEmails  :=PayLoad.RegularEmails;
            LAddressBookAdd.StatementEmails:=PayLoad.StatementEmails;
            LAddressBookAdd.PhoneNumbers   :=PayLoad.PhoneNumbers;

            Rest.CustomBody:=TJson.ObjectToJsonString(LAddressBookAdd);

        finally
            LAddressBookAdd.Free();
        end;

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var LAddressBookAdded:=TJson.JsonToObject<TAddressBookAdded>(Rest.Content);
                try
                    ReturnedId:=LAddressBookAdded.Id;
                    CallResponse.IsSucceeded:=LAddressBookAdded.IsSucceeded;
                    CallResponse.ErrorCode  :=LAddressBookAdded.Error.ErrorCode;
                    CallResponse.LastMessage:=LAddressBookAdded.Error.ErrorDesc;
                finally
                    LAddressBookAdded.Free();
                end;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                Service.Logger.Log('[AddToAddressBookAsync]: Returned status code is ' + Rest.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[AddToAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[AddToAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[AddToAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[AddToAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnedId, CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TAddressBook.DelFromAddressBookAwaited(Id: integer): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/' + Id.ToString();
        Rest.RequestMethod:=TRESTRequestMethod.rmDELETE;
        Service.Logger.Log('[DelFromAddressBookAwaited]: Executing DELETE ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var AddressBookItemDel:=TJson.JsonToObject<TAddressBookItemDel>(Rest.Content);
                try
                    CallResponse.IsSucceeded:=AddressBookItemDel.IsSucceeded;
                    CallResponse.LastMessage:=AddressBookItemDel.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=AddressBookItemDel.Error.ErrorCode;
                finally
                    AddressBookItemDel.Free();
                end;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                Service.Logger.Log('[DelFromAddressBookAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[DelFromAddressBookAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[DelFromAddressBookAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[DelFromAddressBookAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[DelFromAddressBookAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CallResponse;

end;


procedure TAddressBook.GetCustomerDetailsAsync(CustNumber: Int64; SourceDBName: string; Callback: TGetCustomerDetails);
begin

    var LCustDetails: TCustomerDetails;
    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
            + 'addressbook/'
            + SourceDBName
            + '/'
            + CustNumber.ToString()
            + '/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCustomerDetailsAsync]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var AddressBookItem:=TJson.JsonToObject<TAddressBookItem>(Rest.Content);
                try

                    LCustDetails.Id             :=AddressBookItem.Id;
                    LCustDetails.ContactPerson  :=AddressBookItem.ContactPerson;
                    LCustDetails.RegularEmails  :=AddressBookItem.RegularEmails;
                    LCustDetails.StatementEmails:=AddressBookItem.StatementEmails;
                    LCustDetails.PhoneNumbers   :=AddressBookItem.PhoneNumbers;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetCustomerDetailsAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    AddressBookItem.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetCustomerDetailsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetCustomerDetailsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCustomerDetailsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCustomerDetailsAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(LCustDetails, CallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

