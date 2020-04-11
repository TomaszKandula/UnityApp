unit Async.GeneralTables;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    Unity.Types,
    Unity.Grid,
    Unity.Records;


type


    IGeneralTables = interface(IInterface)
    ['{C96D4BF6-9BB3-47EB-B081-A07417E07014}']
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCompaniesAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetControlStatusAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPaidInfoAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPaymentTermsAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetSalesResponsibleAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetPersonResponsibleAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetAccountTypeAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
        /// <summary>
        /// Allow to load async. Permission.Companies table to provided TStringGrids. This method can be executed without
        /// waiting to complete the task, thus allowing parallel execution.
        /// Notification is always executed in main thread as long as callback is provided.
        /// Warning! Please make sure that TargetGrid (if visible component) is not painted durng the processing.
        /// Repaint control after the thread work is done.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure GetCustomerGroupAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TGeneralTables = class(TInterfacedObject, IGeneralTables)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure GetCompaniesAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetControlStatusAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetPaidInfoAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetPaymentTermsAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetSalesResponsibleAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetPersonResponsibleAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetAccountTypeAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
        procedure GetCustomerGroupAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False); virtual;
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
    Api.ReturnCompanies,
    Api.CompaniesFields,
    Api.ReturnControlStatus,
    Api.ControlStatusFields,
    Api.ReturnPaidInfo,
    Api.PaidInfoFields,
    Api.ReturnPaymentTerms,
    Api.PaymentTermsFields,
    Api.ReturnSalesResponsible,
    Api.SalesResponsibleFields,
    Api.ReturnPersonResponsible,
    Api.PersonResponsibleFields,
    Api.ReturnAccountType,
    Api.AccountTypeFields,
    Api.ReturnCustomerGroup,
    Api.CustomerGroupFields;


constructor TGeneralTables.Create();
begin
end;


destructor TGeneralTables.Destroy();
begin
    inherited;
end;


procedure TGeneralTables.GetCompaniesAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/companies/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCompaniesAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnCompanies:=TJson.JsonToObject<TReturnCompanies>(Rest.Content);
                try

                    var RowCount:=Length(ReturnCompanies.Companies);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=15;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TCompaniesFields._SourceDbName;
                    TargetGrid.Cells[2, 0]:=TCompaniesFields._CompanyCode;
                    TargetGrid.Cells[3, 0]:=TCompaniesFields._CompanyName;
                    TargetGrid.Cells[4, 0]:=TCompaniesFields._CompanyAddress;
                    TargetGrid.Cells[5, 0]:=TCompaniesFields._CompanyVat;
                    TargetGrid.Cells[6, 0]:=TCompaniesFields._CompanyDuns;
                    TargetGrid.Cells[7, 0]:=TCompaniesFields._CompanyRate;
                    TargetGrid.Cells[8, 0]:=TCompaniesFields._KpiOvdTarget;
                    TargetGrid.Cells[9, 0]:=TCompaniesFields._KpiUnallTarget;
                    TargetGrid.Cells[10,0]:=TCompaniesFields._StatBeforeRem;
                    TargetGrid.Cells[11,0]:=TCompaniesFields._LbuType;
                    TargetGrid.Cells[12,0]:=TCompaniesFields._Currency;
                    TargetGrid.Cells[13,0]:=TCompaniesFields._City;
                    TargetGrid.Cells[14,0]:=TCompaniesFields._Country;

                    for var Index:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, Index]:=ReturnCompanies.Companies[Index - 1].SourceDbName;
                        TargetGrid.Cells[2, Index]:=ReturnCompanies.Companies[Index - 1].CompanyCode.ToString();
                        TargetGrid.Cells[3, Index]:=ReturnCompanies.Companies[Index - 1].CompanyName;
                        TargetGrid.Cells[4, Index]:=ReturnCompanies.Companies[Index - 1].CompanyAddress;
                        TargetGrid.Cells[5, Index]:=ReturnCompanies.Companies[Index - 1].CompanyVat;
                        TargetGrid.Cells[6, Index]:=ReturnCompanies.Companies[Index - 1].CompanyDuns.ToString();
                        TargetGrid.Cells[7, Index]:=ReturnCompanies.Companies[Index - 1].CompanyRate.ToString();
                        TargetGrid.Cells[8, Index]:=ReturnCompanies.Companies[Index - 1].KpiOvdTarget.ToString();
                        TargetGrid.Cells[9, Index]:=ReturnCompanies.Companies[Index - 1].KpiUnallTarget.ToString();
                        TargetGrid.Cells[10,Index]:=ReturnCompanies.Companies[Index - 1].StatBeforeRem.ToString();
                        TargetGrid.Cells[11,Index]:=ReturnCompanies.Companies[Index - 1].LbuType;
                        TargetGrid.Cells[12,Index]:=ReturnCompanies.Companies[Index - 1].Currency;
                        TargetGrid.Cells[13,Index]:=ReturnCompanies.Companies[Index - 1].City;
                        TargetGrid.Cells[14,Index]:=ReturnCompanies.Companies[Index - 1].Country;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetCompaniesAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnCompanies.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetCompaniesAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetCompaniesAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCompaniesAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCompaniesAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetControlStatusAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/controlstatus/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetControlStatusAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnControlStatus:=TJson.JsonToObject<TReturnControlStatus>(Rest.Content);
                try

                    var RowCount:=Length(ReturnControlStatus.ControlStatus);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TControlStatusFields._Id;
                    TargetGrid.Cells[2, 0]:=TControlStatusFields._Code;
                    TargetGrid.Cells[3, 0]:=TControlStatusFields._Text;
                    TargetGrid.Cells[4, 0]:=TControlStatusFields._Description;

                    for var iCNT:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnControlStatus.ControlStatus[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnControlStatus.ControlStatus[iCNT - 1].Code.ToString();
                        TargetGrid.Cells[3, iCNT]:=ReturnControlStatus.ControlStatus[iCNT - 1].Text;
                        TargetGrid.Cells[4, iCNT]:=ReturnControlStatus.ControlStatus[iCNT - 1].Description;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetControlStatusAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnControlStatus.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetControlStatusAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetControlStatusAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetControlStatusAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetControlStatusAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetPaidInfoAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/paidinfo/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetPaidInfoAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnPaidInfo:=TJson.JsonToObject<TReturnPaidInfo>(Rest.Content);
                try

                    var RowCount:=Length(ReturnPaidInfo.PaidInfo);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=4;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TPaidInfoFields._Id;
                    TargetGrid.Cells[2, 0]:=TPaidInfoFields._ErpCode;
                    TargetGrid.Cells[3, 0]:=TPaidInfoFields._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnPaidInfo.PaidInfo[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnPaidInfo.PaidInfo[iCNT - 1].ErpCode;
                        TargetGrid.Cells[3, iCNT]:=ReturnPaidInfo.PaidInfo[iCNT - 1].Description;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetPaidInfoAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnPaidInfo.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetPaidInfoAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetPaidInfoAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetPaidInfoAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetPaidInfoAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetPaymentTermsAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/paymentterms/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetPaymentTermsAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnPaymentTerms:=TJson.JsonToObject<TReturnPaymentTerms>(Rest.Content);
                try

                    var RowCount:=Length(ReturnPaymentTerms.PaymentTerms);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=9;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TPaymentTermsFields._Id;
                    TargetGrid.Cells[2, 0]:=TPaymentTermsFields._ErpCode;
                    TargetGrid.Cells[3, 0]:=TPaymentTermsFields._Description;
                    TargetGrid.Cells[4, 0]:=TPaymentTermsFields._Month;
                    TargetGrid.Cells[5, 0]:=TPaymentTermsFields._Days;
                    TargetGrid.Cells[6, 0]:=TPaymentTermsFields._DaysNet;
                    TargetGrid.Cells[7, 0]:=TPaymentTermsFields._Using;
                    TargetGrid.Cells[8, 0]:=TPaymentTermsFields._Entity;

                    for var iCNT:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].ErpCode.ToString();
                        TargetGrid.Cells[3, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].Description;
                        TargetGrid.Cells[4, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].Month.ToString();
                        TargetGrid.Cells[5, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].Days.ToString();
                        TargetGrid.Cells[6, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].DaysNet.ToString();
                        TargetGrid.Cells[7, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].Using.ToString();
                        TargetGrid.Cells[8, iCNT]:=ReturnPaymentTerms.PaymentTerms[iCNT - 1].Entity.ToString();
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetPaymentTermsAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnPaymentTerms.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetPaymentTermsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetPaymentTermsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetPaymentTermsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetPaymentTermsAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetSalesResponsibleAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/salesresponsible/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetSalesResponsibleAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnSalesResponsible:=TJson.JsonToObject<TReturnSalesResponsible>(Rest.Content);
                try

                    var RowCount:=Length(ReturnSalesResponsible.SalesResponsible);
                    TargetGrid.RowCount:=RowCount + 1; // Add headers
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TSalesResponsibleFields._Id;
                    TargetGrid.Cells[2, 0]:=TSalesResponsibleFields._SourceDbName;
                    TargetGrid.Cells[3, 0]:=TSalesResponsibleFields._ErpCode;
                    TargetGrid.Cells[4, 0]:=TSalesResponsibleFields._Description;

                    for var iCNT:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnSalesResponsible.SalesResponsible[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnSalesResponsible.SalesResponsible[iCNT - 1].SourceDbName;
                        TargetGrid.Cells[3, iCNT]:=ReturnSalesResponsible.SalesResponsible[iCNT - 1].ErpCode;
                        TargetGrid.Cells[4, iCNT]:=ReturnSalesResponsible.SalesResponsible[iCNT - 1].Description;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetSalesResponsibleAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnSalesResponsible.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetSalesResponsibleAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetSalesResponsibleAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetSalesResponsibleAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetSalesResponsibleAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetPersonResponsibleAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/personresponsible/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetPersonResponsibleAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnPersonResponsible:=TJson.JsonToObject<TReturnPersonResponsible>(Rest.Content);
                try

                    var RowCount:=Length(ReturnPersonResponsible.PersonResponsible);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TPersonResponsibleFields._Id;
                    TargetGrid.Cells[2, 0]:=TPersonResponsibleFields._SourceDbName;
                    TargetGrid.Cells[3, 0]:=TPersonResponsibleFields._ErpCode;
                    TargetGrid.Cells[4, 0]:=TPersonResponsibleFields._Description;

                    for var iCNT:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnPersonResponsible.PersonResponsible[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnPersonResponsible.PersonResponsible[iCNT - 1].SourceDbName;
                        TargetGrid.Cells[3, iCNT]:=ReturnPersonResponsible.PersonResponsible[iCNT - 1].ErpCode;
                        TargetGrid.Cells[4, iCNT]:=ReturnPersonResponsible.PersonResponsible[iCNT - 1].Description;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetPersonResponsibleAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnPersonResponsible.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetPersonResponsibleAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetPersonResponsibleAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetPersonResponsibleAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetPersonResponsibleAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetAccountTypeAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/accounttype/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetAccountTypeAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnAccountType:=TJson.JsonToObject<TReturnAccountType>(Rest.Content);
                try

                    var RowCount:=Length(ReturnAccountType.AccountType);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TAccountTypeFields._Id;
                    TargetGrid.Cells[2, 0]:=TAccountTypeFields._SourceDbName;
                    TargetGrid.Cells[3, 0]:=TAccountTypeFields._ErpCode;
                    TargetGrid.Cells[4, 0]:=TAccountTypeFields._Description;

                    for var iCNT:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnAccountType.AccountType[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnAccountType.AccountType[iCNT - 1].SourceDbName;
                        TargetGrid.Cells[3, iCNT]:=ReturnAccountType.AccountType[iCNT - 1].ErpCode;
                        TargetGrid.Cells[4, iCNT]:=ReturnAccountType.AccountType[iCNT - 1].Description;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetAccountTypeAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnAccountType.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetAccountTypeAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetAccountTypeAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetAccountTypeAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetAccountTypeAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


procedure TGeneralTables.GetCustomerGroupAsync(TargetGrid: TStringGrid; Callback: TGetTables; WaitToComplete: boolean = False);
begin

    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/customergroup/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCustomerGroupAsync]: Executing GET ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnCustomerGroup:=TJson.JsonToObject<TReturnCustomerGroup>(Rest.Content);
                try

                    var RowCount:=Length(ReturnCustomerGroup.CustomerGroup);
                    TargetGrid.RowCount:=RowCount + 1; // Add header
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=TCustomerGroupFields._Id;
                    TargetGrid.Cells[2, 0]:=TCustomerGroupFields._SourceDbName;
                    TargetGrid.Cells[3, 0]:=TCustomerGroupFields._ErpCode;
                    TargetGrid.Cells[4, 0]:=TCustomerGroupFields._Description;

                    for var iCNT:=1 to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnCustomerGroup.CustomerGroup[iCNT - 1].Id.ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnCustomerGroup.CustomerGroup[iCNT - 1].SourceDbName;
                        TargetGrid.Cells[3, iCNT]:=ReturnCustomerGroup.CustomerGroup[iCNT - 1].ErpCode;
                        TargetGrid.Cells[4, iCNT]:=ReturnCustomerGroup.CustomerGroup[iCNT - 1].Description;
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetCustomerGroupAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnCustomerGroup.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetCustomerGroupAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetCustomerGroupAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCustomerGroupAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCustomerGroupAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();
    if WaitToComplete then TTask.WaitForAny(NewTask);

end;


end.

