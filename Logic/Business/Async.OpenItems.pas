unit Async.OpenItems;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Generics.Collections,
    System.Classes,
    Unity.Types,
    Unity.Grid,
    Unity.Enums,
    Unity.Records;


type


    IOpenItems = interface(IInterface)
    ['{CD6AC138-D2A4-4C6B-A3F1-07F904BA44B1}']
        /// <summary>
        /// Returns date and time of the SSIS package transfer from SSIS master database table alongside with transfer status.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetSSISDataAwaited(var DateTime: string; var Status: string): TCallResponse;
        /// <summary>
        /// Allow to async. load open items from database for given company and customer number.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetOpenItemsAsync(SourceDbName: string; CustomerNumber: Int64; Callback: TGetOpenItems);
        /// <summary>
        /// Allow to async. load all open items from database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadOpenItemsAsync(PageNumber: integer; LoadedCompanies: TArray<string>; Callback: TReadOpenItems);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and and extend them.
    /// </remarks>
    TOpenItems = class(TInterfacedObject, IOpenItems)
    public
        constructor Create();
        destructor  Destroy(); override;
        function    GetSSISDataAwaited(var DateTime: string; var Status: string): TCallResponse; virtual;
        procedure   GetOpenItemsAsync(SourceDbName: string; CustomerNumber: Int64; Callback: TGetOpenItems); virtual;
        procedure   ReadOpenItemsAsync(PageNumber: integer; LoadedCompanies: TArray<string>; Callback: TReadOpenItems); virtual;
    end;


implementation


uses
    System.Threading,
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.Constants,
    Unity.Helpers,
    Unity.Service,
    Api.UserCompanySelection,
    Api.ReturnSsisData,
    Api.ReturnOpenItems,
    Api.OpenItemsFields,
    Layout.AgeViewModel;


constructor TOpenItems.Create();
begin
    inherited;
end;


destructor TOpenItems.Destroy();
begin
    inherited;
end;


function TOpenItems.GetSSISDataAwaited(var DateTime: string; var Status: string): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var HaveDateTime: string;
    var HaveStatus:   string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'automation/customers/openitems/ssis/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetSSISDataAwaited]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
            begin

                var ReturnSsisData:=TJson.JsonToObject<TReturnSsisData>(Rest.Content);
                try

                    HaveDateTime:=ReturnSsisData.CustExtractDt;
                    HaveStatus:=ReturnSsisData.StatusCode;

                    CallResponse.LastMessage:=ReturnSsisData.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=ReturnSsisData.Error.ErrorCode;

                    CallResponse.IsSucceeded:=True;
                    Service.Logger.Log('[GetSSISDataAwaited]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnSsisData.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetSSISDataAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetSSISDataAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetSSISDataAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetSSISDataAwaited]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    Result  :=CallResponse;
    DateTime:=HaveDateTime;
    Status  :=HaveStatus;

end;


procedure TOpenItems.GetOpenItemsAsync(SourceDbName: string; CustomerNumber: Int64; Callback: TGetOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI')
        + 'openitems/customers/'
        + SourceDbName
        + '/'
        + CustomerNumber.ToString()
        + '/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetOpenItemsAsync]: Executing GET ' + Rest.ClientBaseURL);

        var ReturnOpenItems: TReturnOpenItems;
        try

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
            begin
                ReturnOpenItems:=TJson.JsonToObject<TReturnOpenItems>(Rest.Content);
                Service.Logger.Log('[GetOpenItemsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                ReturnOpenItems:=TReturnOpenItems.Create();

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    ReturnOpenItems.Error.ErrorDesc:='[GetOpenItemsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        ReturnOpenItems.Error.ErrorDesc:='[GetOpenItemsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        ReturnOpenItems.Error.ErrorDesc:='[GetOpenItemsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(ReturnOpenItems.Error.ErrorDesc);

            end;

        except
            on E: Exception do
            begin
                ReturnOpenItems:=TReturnOpenItems.Create();
                ReturnOpenItems.Error.ErrorDesc:='[GetOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(ReturnOpenItems.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnOpenItems);
            if Assigned(ReturnOpenItems) then ReturnOpenItems.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TOpenItems.ReadOpenItemsAsync(PageNumber: integer; LoadedCompanies: TArray<string>; Callback: TReadOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
	    Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'openitems/customers/?page=' + PageNumber.ToString();
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[ReadOpenItemsAsync]: Executing POST ' + Rest.ClientBaseURL);

        var ReturnOpenItems: TReturnOpenItems;
        try

            var UserCompanySelection:=TUserCompanySelection.Create();
            try
                UserCompanySelection.SelectedCoCodes:=LoadedCompanies;
                Rest.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
            finally
                UserCompanySelection.Free();
            end;

            if (Rest.Execute) and ( (Rest.StatusCode >= 200) and (Rest.StatusCode <= 226) ) then
            begin
                ReturnOpenItems:=TJson.JsonToObject<TReturnOpenItems>(Rest.Content);
                Service.Logger.Log('[ReadOpenItemsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                ReturnOpenItems:=TReturnOpenItems.Create();

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    ReturnOpenItems.Error.ErrorDesc:='[ReadOpenItemsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        ReturnOpenItems.Error.ErrorDesc:='[ReadOpenItemsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        ReturnOpenItems.Error.ErrorDesc:='[ReadOpenItemsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(ReturnOpenItems.Error.ErrorDesc);

            end;

        except
            on E: Exception do
            begin
                ReturnOpenItems:=TReturnOpenItems.Create();
                ReturnOpenItems.Error.ErrorDesc:='[ReadOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(ReturnOpenItems.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnOpenItems);
            if Assigned(ReturnOpenItems) then ReturnOpenItems.Free();
        end);

    end);

    NewTask.Start();

end;


end.

