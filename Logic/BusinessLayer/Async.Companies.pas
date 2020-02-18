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
        /// Allow to load async. some company data like name, address, phone etc. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyDetailsAwaited(SourceDBName: string; var CompanyDetails: TCompanyDetails): TCallResponse;
        /// <summary>
        /// Allow to load async. some company data like name, address, phone etc.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompanyDetailsAsync(SourceDBName: string; Callback: TGetCompanyDetails);
        /// <summary>
        /// Allow to load async. list of emails for given CoCodes. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyEmailsAwaited(SourceList: TArray<string>; var TargetList: TArray<TRegisteredEmails>): TCallResponse;
    end;


    TCompanies = class(TInterfacedObject, ICompanies)
    {$TYPEINFO ON}
    strict private
        procedure FSetCompanyDetails(Source: TArray<TBankDetails>; var Target: TArray<TBankDetails>);
        procedure FSetCompanyEmails(Source: TArray<TRegisteredEmails>; var Target: TArray<TRegisteredEmails>);
    public
        constructor Create();
        destructor Destroy(); override;
        /// <summary>
        /// Allow to load async. some company data like name, address, phone etc. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyDetailsAwaited(SourceDBName: string; var CompanyDetails: TCompanyDetails): TCallResponse;
        /// <summary>
        /// Allow to load async. some company data like name, address, phone etc.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompanyDetailsAsync(SourceDBName: string; Callback: TGetCompanyDetails);
        /// <summary>
        /// Allow to load async. list of primary email(s) for given Company Codes. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyEmailsAwaited(SourceList: TArray<string>; var TargetList: TArray<TRegisteredEmails>): TCallResponse;
    end;


implementation


uses
    System.Threading,
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
    Unity.Helpers,
    Unity.Service,
    Api.CompanyData,
    Api.CompanyCodesList,
    Api.CompanyEmailsList;


constructor TCompanies.Create();
begin
    {Empty}
end;


destructor TCompanies.Destroy();
begin
    {Empty}
    inherited;
end;


// remove it after mass mailer refactored!
// use only async version!
function TCompanies.GetCompanyDetailsAwaited(SourceDBName: string; var CompanyDetails: TCompanyDetails): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var CompanyData: TCompanyData;
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

            Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'companies/' + SourceDBName;
            Restful.RequestMethod:=TRESTRequestMethod.rmGET;
            Service.Logger.Log('[GetCompanyDetailsAwaited]: Executing GET ' + Restful.ClientBaseURL);

            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin
                    CompanyData:=TJson.JsonToObject<TCompanyData>(Restful.Content);
                    CallResponse.IsSucceeded:=True;
                    Service.Logger.Log('[GetCompanyDetailsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                end
                else
                begin

                    if not String.IsNullOrEmpty(Restful.ExecuteError) then
                        CallResponse.LastMessage:='[GetCompanyDetailsAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                    else
                        if String.IsNullOrEmpty(Restful.Content) then
                            CallResponse.LastMessage:='[GetCompanyDetailsAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[GetCompanyDetailsAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    Service.Logger.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetCompanyDetailsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    Service.Logger.Log(CallResponse.LastMessage);
                end;

            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);

        CompanyDetails.LbuName   :=CompanyData.CompanyName;
        CompanyDetails.LbuAddress:=CompanyData.CompanyAddress;
        CompanyDetails.LbuPhones :=CompanyData.CompanyPhones;
        CompanyDetails.LbuEmails :=CompanyData.CompanyEmails;
        CompanyDetails.Exclusions:=CompanyData.Exclusions;
        FSetCompanyDetails(CompanyData.CompanyBanks, CompanyDetails.LbuBanks);

    finally
        CompanyData.Free();
        Result:=CallResponse;
    end;

end;


procedure TCompanies.GetCompanyDetailsAsync(SourceDBName: string; Callback: TGetCompanyDetails);
begin

    var CallResponse: TCallResponse;
    var CompanyDetails: TCompanyDetails;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'companies/' + SourceDBName;
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCompanyDetailsAsync]: Executing GET ' + Restful.ClientBaseURL);

        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var CompanyData:=TJson.JsonToObject<TCompanyData>(Restful.Content);
                try
                    CompanyDetails.LbuName   :=CompanyData.CompanyName;
                    CompanyDetails.LbuAddress:=CompanyData.CompanyAddress;
                    CompanyDetails.LbuPhones :=CompanyData.CompanyPhones;
                    CompanyDetails.LbuEmails :=CompanyData.CompanyEmails;
                    CompanyDetails.Exclusions:=CompanyData.Exclusions;
                    FSetCompanyDetails(CompanyData.CompanyBanks, CompanyDetails.LbuBanks);
                finally
                    CompanyData.Free();
                end;

                CallResponse.IsSucceeded:=True;
                Service.Logger.Log('[GetCompanyDetailsAsync]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCompanyDetailsAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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
            if Assigned(Callback) then Callback(CompanyDetails, CallResponse);
        end);

    end);

    NewTask.Start();

end;


function TCompanies.GetCompanyEmailsAwaited(SourceList: TArray<string>; var TargetList: TArray<TRegisteredEmails>): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var CompanyEmailsList: TCompanyEmailsList;
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

            Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'companies/return/emails/';
            Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
            Service.Logger.Log('[GetCompanyEmailsAwaited]: Executing POST ' + Restful.ClientBaseURL);

            try

                var CompanyCodesList:=TCompanyCodesList.Create();
                CompanyCodesList.SourceDBName:=SourceList;
                Restful.CustomBody:=TJson.ObjectToJsonString(CompanyCodesList);

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin
                    CompanyEmailsList:=TJson.JsonToObject<TCompanyEmailsList>(Restful.Content);
                    CallResponse.IsSucceeded:=True;
                    Service.Logger.Log('[GetCompanyEmailsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
                end
                else
                begin

                    if not String.IsNullOrEmpty(Restful.ExecuteError) then
                        CallResponse.LastMessage:='[GetCompanyEmailsAwaited]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                    else
                        if String.IsNullOrEmpty(Restful.Content) then
                            CallResponse.LastMessage:='[GetCompanyEmailsAwaited]: Invalid server response. Please contact IT Support.'
                        else
                            CallResponse.LastMessage:='[GetCompanyEmailsAwaited]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    CallResponse.IsSucceeded:=False;
                    Service.Logger.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetCompanyEmailsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    Service.Logger.Log(CallResponse.LastMessage);
                end;

            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        FSetCompanyEmails(CompanyEmailsList.EmailList, TargetList);

    finally
        Result:=CallResponse;
        CompanyEmailsList.Free();
    end;

end;


procedure TCompanies.FSetCompanyDetails(Source: TArray<TBankDetails>; var Target: TArray<TBankDetails>);
begin

    SetLength(Target, Length(Source));
    for var iCNT:=0 to Length(Source) - 1 do
    begin

        if not Assigned(Target[iCNT]) then
            Target[iCNT]:=TBankDetails.Create();

        Target[iCNT].BankName:=Source[iCNT].BankName;
        Target[iCNT].BankAcc :=Source[iCNT].BankAcc;
        Target[iCNT].BankCode:=Source[iCNT].BankCode;
        Target[iCNT].BankIso :=Source[iCNT].BankIso;

    end;

end;


procedure TCompanies.FSetCompanyEmails(Source: TArray<TRegisteredEmails>; var Target: TArray<TRegisteredEmails>);
begin

    SetLength(Target, Length(Source));
    for var iCNT:=0 to Length(Source) - 1 do
    begin

        if not Assigned(Target[iCNT]) then
            Target[iCNT]:=TRegisteredEmails.Create();

        Target[iCNT].CompanyCode :=Source[iCNT].CompanyCode;
        Target[iCNT].CompanyEmail:=Source[iCNT].CompanyEmail;

    end;

end;


end.

