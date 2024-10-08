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
        procedure OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TArray<string> = nil);
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
        /// <summary>
        /// Insert async. address book listed new companies and notify via given callback method that is always executed in main thread.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure AddBulkToAddressBookAsync(PayLoad: TList<TCustomerDetails>; Callback: TAddBulkToAddressBook);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and extend them.
    /// </remarks>
    TAddressBook = class(TInterfacedObject, IAddressBook)
    public
        constructor Create();
        destructor  Destroy(); override;
        procedure   OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TArray<string> = nil); virtual;
        procedure   UpdateAddressBookAsync(PayLoad: TCustomerDetails; Callback: TUpdateAddressBook); virtual;
        procedure   AddToAddressBookAsync(PayLoad: TCustomerDetails; Callback: TAddToAddressBook); virtual;
        function    DelFromAddressBookAwaited(Id: integer): TCallResponse; virtual;
        procedure   GetCustomerDetailsAsync(CustNumber: Int64; SourceDBName: string; Callback: TGetCustomerDetails); virtual;
        procedure   AddBulkToAddressBookAsync(PayLoad: TList<TCustomerDetails>; Callback: TAddBulkToAddressBook); virtual;
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
    Api.AddressBookFields,
    Api.AddressBookItem,
    Api.AddressBookUpdate,
    Api.AddressBookUpdated,
    Api.AddressBookItemDel,
    Api.AddressBookAdd,
    Api.AddressBookAdded,
    Api.AddressBookAddBulk,
    Api.AddressBookAddedBulk;


constructor TAddressBook.Create();
begin
    inherited;
end;


destructor TAddressBook.Destroy();
begin
    inherited;
end;


procedure TAddressBook.OpenAddressBookAsync(UserAlias: string; Callback: TOpenAddressBook; LoadedCompanies: TArray<string> = nil);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        if Length(LoadedCompanies) > 0 then
        begin


            Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/selection/';
            Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
            Service.Logger.Log('[OpenAddressBookAsync]: Executing POST ' + Rest.ClientBaseURL);

            var UserCompanySelection:=TUserCompanySelection.Create();
            try
                UserCompanySelection.SelectedCoCodes:=LoadedCompanies;
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

        var AddressBookList: TAddressBookList;
        try

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
            begin
                AddressBookList:=TJson.JsonToObject<TAddressBookList>(Rest.Content);
                Service.Logger.Log('[OpenAddressBookAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                AddressBookList:=TAddressBookList.Create();

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    AddressBookList.Error.ErrorDesc:='[OpenAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        AddressBookList.Error.ErrorDesc:='[OpenAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        AddressBookList.Error.ErrorDesc:='[OpenAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(AddressBookList.Error.ErrorDesc);

            end;

        except on
            E: Exception do
            begin
                AddressBookList:=TAddressBookList.Create();
                AddressBookList.Error.ErrorDesc:='[OpenAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(AddressBookList.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(AddressBookList);
            if Assigned(AddressBookList) then AddressBookList.Free();
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

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
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

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
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
            if Assigned(Callback) then Callback(CallResponse, ReturnedId);
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

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
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

        var AddressBookItem: TAddressBookItem;
        try

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
            begin
                AddressBookItem:=TJson.JsonToObject<TAddressBookItem>(Rest.Content);
                Service.Logger.Log('[GetCustomerDetailsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                AddressBookItem:=TAddressBookItem.Create();

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    AddressBookItem.Error.ErrorDesc:='[GetCustomerDetailsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        AddressBookItem.Error.ErrorDesc:='[GetCustomerDetailsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        AddressBookItem.Error.ErrorDesc:='[GetCustomerDetailsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(AddressBookItem.Error.ErrorDesc);

            end;

        except on
            E: Exception do
            begin
                AddressBookItem:=TAddressBookItem.Create();
                AddressBookItem.Error.ErrorDesc:='[GetCustomerDetailsAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(AddressBookItem.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(AddressBookItem);
            if Assigned(AddressBookItem) then AddressBookItem.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TAddressBook.AddBulkToAddressBookAsync(PayLoad: TList<TCustomerDetails>; Callback: TAddBulkToAddressBook);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'addressbook/bulk/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[AddBulkToAddressBookAsync]: Executing POST ' + Rest.ClientBaseURL);

        var Request:=TAddressBookAddBulk.Create(PayLoad.Count);
        try

            for var Index:=0 to PayLoad.Count - 1 do
            begin
                Request.AddressBookData[Index].SourceDbName  :=PayLoad[Index].SourceDBName;
                Request.AddressBookData[Index].CustomerNumber:=PayLoad[Index].CustomerNumber;
                Request.AddressBookData[Index].CustomerName  :=PayLoad[Index].CustomerName;
            end;

            Request.UserAlias:=Service.SessionData.AliasName;
            Rest.CustomBody:=TJson.ObjectToJsonString(Request);

        finally
            Request.Free();
            PayLoad.Free();
        end;

        var LCallResponse: TCallResponse;
        try

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
            begin

                var Response:=TJson.JsonToObject<TAddressBookAddedBulk>(Rest.Content);
                try
                    LCallResponse.IsSucceeded:=True;
                    LCallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[AddBulkToAddressBookAsync]: Returned status code is ' + Rest.StatusCode.ToString());
                finally
                    Response.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    LCallResponse.LastMessage:='[AddBulkToAddressBookAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        LCallResponse.LastMessage:='[AddBulkToAddressBookAsync]: Invalid server response. Please contact IT Support.'
                    else
                        LCallResponse.LastMessage:='[AddBulkToAddressBookAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                LCallResponse.ReturnedCode:=Rest.StatusCode;
                LCallResponse.IsSucceeded:=False;
                Service.Logger.Log(LCallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                LCallResponse.IsSucceeded:=False;
                LCallResponse.LastMessage:='[AddBulkToAddressBookAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(LCallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(LCallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

