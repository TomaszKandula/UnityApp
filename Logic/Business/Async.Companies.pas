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
    Unity.Records;


type


    ICompanies = interface(IInterface)
    ['{E7616759-7564-44A4-BEEF-BDD220040E1E}']
        /// <summary>
        /// Allow to load async. lists of company data as name, address, phone etc.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompanyEmailsAsync(SelectedCompanies: TArray<string>; Callback: TGetCompanyEmails);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and extend them.
    /// </remarks>
    TCompanies = class(TInterfacedObject, ICompanies)
    public
        constructor Create();
        destructor  Destroy(); override;
        procedure   GetCompanyEmailsAsync(SelectedCompanies: TArray<string>; Callback: TGetCompanyEmails); virtual;
    end;


implementation


uses
    System.Threading,
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.Helpers,
    Unity.Service,
    Api.ReturnCompanyEmails,
    Api.UserCompanySelection;


constructor TCompanies.Create();
begin
    inherited;
end;


destructor TCompanies.Destroy();
begin
    inherited;
end;


procedure TCompanies.GetCompanyEmailsAsync(SelectedCompanies: TArray<string>; Callback: TGetCompanyEmails);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'companies/emails/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[GetCompanyDetailsAsync]: Executing POST ' + Rest.ClientBaseURL);

        var ReturnCompanyEmails: TReturnCompanyEmails;
        try

            var UserCompanySelection:=TUserCompanySelection.Create();
            try
                UserCompanySelection.SelectedCoCodes:=SelectedCompanies;
                Rest.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
            finally
                UserCompanySelection.Free();
            end;

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                ReturnCompanyEmails:=TJson.JsonToObject<TReturnCompanyEmails>(Rest.Content);
                Service.Logger.Log('[GetCompanyDetailsAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    ReturnCompanyEmails.Error.ErrorDesc:='[GetCompanyDetailsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        ReturnCompanyEmails.Error.ErrorDesc:='[GetCompanyDetailsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        ReturnCompanyEmails.Error.ErrorDesc:='[GetCompanyDetailsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                Service.Logger.Log(ReturnCompanyEmails.Error.ErrorDesc);

            end;

        except on
            E: Exception do
            begin
                ReturnCompanyEmails.Error.ErrorDesc:='[GetCompanyDetailsAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(ReturnCompanyEmails.Error.ErrorDesc);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(ReturnCompanyEmails);
            if Assigned(ReturnCompanyEmails) then ReturnCompanyEmails.Free();
        end);

    end);

    NewTask.Start();

end;


end.

