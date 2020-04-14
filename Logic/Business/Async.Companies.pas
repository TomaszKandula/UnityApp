unit Async.Companies;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Generics.Collections,
    System.Classes,
    Unity.Types,
    Unity.Records,
    Api.RegisteredEmails,
    Api.BankDetails;


type


    ICompanies = interface(IInterface)
    ['{E7616759-7564-44A4-BEEF-BDD220040E1E}']
        /// <summary>
        /// Allow to load async. company data as name, address, phone etc.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompanySpecificsAsync(SourceDBName: string; Callback: TGetCompanySpecifics);
        /// <summary>
        /// Allow to load async. lists of company data as name, address, phone etc.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompanyDetailsAsync(SelectedCompanies: TList<string>; Callback: TGetCompanyDetails);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TCompanies = class(TInterfacedObject, ICompanies)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure GetCompanySpecificsAsync(SourceDBName: string; Callback: TGetCompanySpecifics); virtual;
        procedure GetCompanyDetailsAsync(SelectedCompanies: TList<string>; Callback: TGetCompanyDetails); virtual;
    end;


implementation


uses
    System.Threading,
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.Service,
    Api.ReturnCompanyData,
    Api.ReturnCompanyDetails,
    Api.CompanyDetails,
    Api.UserCompanySelection;


constructor TCompanies.Create();
begin
end;


destructor TCompanies.Destroy();
begin
    inherited;
end;


procedure TCompanies.GetCompanySpecificsAsync(SourceDBName: string; Callback: TGetCompanySpecifics);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'companies/' + SourceDBName;
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCompanyDetailsAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        var ReturnCompanyData: TReturnCompanyData;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                ReturnCompanyData:=TJson.JsonToObject<TReturnCompanyData>(Rest.Content);
                CallResponse.IsSucceeded:=True;
                Service.Logger.Log('[GetCompanyDetailsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCompanyDetailsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnCompanyData, CallResponse);
            if Assigned(ReturnCompanyData) then ReturnCompanyData.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TCompanies.GetCompanyDetailsAsync(SelectedCompanies: TList<string>; Callback: TGetCompanyDetails);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'companies/return/details/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[GetCompanyListingsAsync]: Executing POST ' + Rest.ClientBaseURL);

        var UserCompanySelection:=TUserCompanySelection.Create();
        try
            UserCompanySelection.SelectedCoCodes:=SelectedCompanies.ToArray();
            Rest.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
        finally
            UserCompanySelection.Free();
        end;

        var CallResponse: TCallResponse;
        var ReturnCompanyDetails: TReturnCompanyDetails;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                ReturnCompanyDetails:=TJson.JsonToObject<TReturnCompanyDetails>(Rest.Content);
                CallResponse.LastMessage:=ReturnCompanyDetails.Error.ErrorDesc;
                CallResponse.ErrorCode  :=ReturnCompanyDetails.Error.ErrorCode;
                CallResponse.IsSucceeded:=True;
                Service.Logger.Log('[GetCompanyListingsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetCompanyListingsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetCompanyListingsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCompanyListingsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCompanyListingsAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnCompanyDetails, CallResponse);
            if Assigned(ReturnCompanyDetails) then ReturnCompanyDetails.Free();
        end);

    end);

    NewTask.Start();

end;


end.

