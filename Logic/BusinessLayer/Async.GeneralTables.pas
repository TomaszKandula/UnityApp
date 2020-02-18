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
    /// Concrete implementation. Never call it directly, you can inherit from and extend upon.
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
    Unity.RestWrapper,
    Unity.Helpers,
    Unity.Service,
    Api.ReturnCompanies,
    Api.ReturnAccountType,
    Api.ReturnPaidInfo,
    Api.ReturnControlStatus,
    Api.ReturnPersonResponsible,
    Api.ReturnSalesResponsible,
    Api.ReturnPaymentTerms,
    Api.ReturnCustomerGroup;


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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/companies/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCompaniesAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnCompanies:=TJson.JsonToObject<TReturnCompanies>(Restful.Content);
                try

                    var RowCount:=Length(ReturnCompanies.SourceDbName);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=15;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnCompanies._SourceDbName;
                    TargetGrid.Cells[2, 0]:=ReturnCompanies._CompanyCode;
                    TargetGrid.Cells[3, 0]:=ReturnCompanies._CompanyName;
                    TargetGrid.Cells[4, 0]:=ReturnCompanies._CompanyAddress;
                    TargetGrid.Cells[5, 0]:=ReturnCompanies._CompanyVat;
                    TargetGrid.Cells[6, 0]:=ReturnCompanies._CompanyDuns;
                    TargetGrid.Cells[7, 0]:=ReturnCompanies._CompanyRate;
                    TargetGrid.Cells[8, 0]:=ReturnCompanies._KpiOvdTarget;
                    TargetGrid.Cells[9, 0]:=ReturnCompanies._KpiUnallTarget;
                    TargetGrid.Cells[10,0]:=ReturnCompanies._StatBeforeRem;
                    TargetGrid.Cells[11,0]:=ReturnCompanies._LbuType;
                    TargetGrid.Cells[12,0]:=ReturnCompanies._Currency;
                    TargetGrid.Cells[13,0]:=ReturnCompanies._City;
                    TargetGrid.Cells[14,0]:=ReturnCompanies._Country;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnCompanies.SourceDbName[iCNT - 1];
                        TargetGrid.Cells[2, iCNT]:=ReturnCompanies.CompanyCode[iCNT - 1].ToString();
                        TargetGrid.Cells[3, iCNT]:=ReturnCompanies.CompanyName[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnCompanies.CompanyAddress[iCNT - 1];
                        TargetGrid.Cells[5, iCNT]:=ReturnCompanies.CompanyVat[iCNT - 1];
                        TargetGrid.Cells[6, iCNT]:=ReturnCompanies.CompanyDuns[iCNT - 1].ToString();
                        TargetGrid.Cells[7, iCNT]:=ReturnCompanies.CompanyRate[iCNT - 1].ToString();
                        TargetGrid.Cells[8, iCNT]:=ReturnCompanies.KpiOvdTarget[iCNT - 1].ToString();
                        TargetGrid.Cells[9, iCNT]:=ReturnCompanies.KpiUnallTarget[iCNT - 1].ToString();
                        TargetGrid.Cells[10,iCNT]:=ReturnCompanies.StatBeforeRem[iCNT - 1].ToString();
                        TargetGrid.Cells[11,iCNT]:=ReturnCompanies.LbuType[iCNT - 1];
                        TargetGrid.Cells[12,iCNT]:=ReturnCompanies.Currency[iCNT - 1];
                        TargetGrid.Cells[13,iCNT]:=ReturnCompanies.City[iCNT - 1];
                        TargetGrid.Cells[14,iCNT]:=ReturnCompanies.Country[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetCompaniesAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnCompanies.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetCompaniesAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetCompaniesAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCompaniesAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/controlstatus/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetControlStatusAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnControlStatus:=TJson.JsonToObject<TReturnControlStatus>(Restful.Content);
                try

                    var RowCount:=Length(ReturnControlStatus.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnControlStatus._Id;
                    TargetGrid.Cells[2, 0]:=ReturnControlStatus._Code;
                    TargetGrid.Cells[3, 0]:=ReturnControlStatus._Text;
                    TargetGrid.Cells[4, 0]:=ReturnControlStatus._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnControlStatus.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnControlStatus.Code[iCNT - 1].ToString();
                        TargetGrid.Cells[3, iCNT]:=ReturnControlStatus.Text[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnControlStatus.Description[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetControlStatusAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnControlStatus.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetControlStatusAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetControlStatusAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetControlStatusAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/paidinfo/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetPaidInfoAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnPaidInfo:=TJson.JsonToObject<TReturnPaidInfo>(Restful.Content);
                try

                    var RowCount:=Length(ReturnPaidInfo.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=4;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnPaidInfo._Id;
                    TargetGrid.Cells[2, 0]:=ReturnPaidInfo._ErpCode;
                    TargetGrid.Cells[3, 0]:=ReturnPaidInfo._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnPaidInfo.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnPaidInfo.ErpCode[iCNT - 1];
                        TargetGrid.Cells[3, iCNT]:=ReturnPaidInfo.Description[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetPaidInfoAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnPaidInfo.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetPaidInfoAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetPaidInfoAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetPaidInfoAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/paymentterms/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetPaymentTermsAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnPaymentTerms:=TJson.JsonToObject<TReturnPaymentTerms>(Restful.Content);
                try

                    var RowCount:=Length(ReturnPaymentTerms.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=9;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnPaymentTerms._Id;
                    TargetGrid.Cells[2, 0]:=ReturnPaymentTerms._ErpCode;
                    TargetGrid.Cells[3, 0]:=ReturnPaymentTerms._Description;
                    TargetGrid.Cells[4, 0]:=ReturnPaymentTerms._Month;
                    TargetGrid.Cells[5, 0]:=ReturnPaymentTerms._Days;
                    TargetGrid.Cells[6, 0]:=ReturnPaymentTerms._DaysNet;
                    TargetGrid.Cells[7, 0]:=ReturnPaymentTerms._Using;
                    TargetGrid.Cells[8, 0]:=ReturnPaymentTerms._Entity;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnPaymentTerms.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnPaymentTerms.ErpCode[iCNT - 1].ToString();
                        TargetGrid.Cells[3, iCNT]:=ReturnPaymentTerms.Description[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnPaymentTerms.Month[iCNT - 1].ToString();
                        TargetGrid.Cells[5, iCNT]:=ReturnPaymentTerms.Days[iCNT - 1].ToString();
                        TargetGrid.Cells[6, iCNT]:=ReturnPaymentTerms.DaysNet[iCNT - 1].ToString();
                        TargetGrid.Cells[7, iCNT]:=ReturnPaymentTerms.Using[iCNT - 1].ToString();
                        TargetGrid.Cells[8, iCNT]:=ReturnPaymentTerms.Entity[iCNT - 1].ToString();
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetPaymentTermsAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnPaymentTerms.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetPaymentTermsAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetPaymentTermsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetPaymentTermsAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/salesresponsible/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetSalesResponsibleAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnSalesResponsible:=TJson.JsonToObject<TReturnSalesResponsible>(Restful.Content);
                try

                    var RowCount:=Length(ReturnSalesResponsible.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnSalesResponsible._Id;
                    TargetGrid.Cells[2, 0]:=ReturnSalesResponsible._SourceDbName;
                    TargetGrid.Cells[3, 0]:=ReturnSalesResponsible._ErpCode;
                    TargetGrid.Cells[4, 0]:=ReturnSalesResponsible._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnSalesResponsible.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnSalesResponsible.SourceDbName[iCNT - 1];
                        TargetGrid.Cells[3, iCNT]:=ReturnSalesResponsible.ErpCode[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnSalesResponsible.Description[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetSalesResponsibleAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnSalesResponsible.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetSalesResponsibleAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetSalesResponsibleAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetSalesResponsibleAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/personresponsible/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetPersonResponsibleAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnPersonResponsible:=TJson.JsonToObject<TReturnPersonResponsible>(Restful.Content);
                try

                    var RowCount:=Length(ReturnPersonResponsible.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnPersonResponsible._Id;
                    TargetGrid.Cells[2, 0]:=ReturnPersonResponsible._SourceDbName;
                    TargetGrid.Cells[3, 0]:=ReturnPersonResponsible._ErpCode;
                    TargetGrid.Cells[4, 0]:=ReturnPersonResponsible._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnPersonResponsible.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnPersonResponsible.SourceDbName[iCNT - 1];
                        TargetGrid.Cells[3, iCNT]:=ReturnPersonResponsible.ErpCode[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnPersonResponsible.Description[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetPersonResponsibleAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnPersonResponsible.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetPersonResponsibleAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetPersonResponsibleAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetPersonResponsibleAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/accounttype/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetAccountTypeAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnAccountType:=TJson.JsonToObject<TReturnAccountType>(Restful.Content);
                try

                    var RowCount:=Length(ReturnAccountType.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnAccountType._Id;
                    TargetGrid.Cells[2, 0]:=ReturnAccountType._SourceDbName;
                    TargetGrid.Cells[3, 0]:=ReturnAccountType._ErpCode;
                    TargetGrid.Cells[4, 0]:=ReturnAccountType._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnAccountType.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnAccountType.SourceDbName[iCNT - 1];
                        TargetGrid.Cells[3, iCNT]:=ReturnAccountType.ErpCode[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnAccountType.Description[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetAccountTypeAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnAccountType.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetAccountTypeAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetAccountTypeAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetAccountTypeAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

        var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

        Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'generaltables/customergroup/';
        Restful.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetCustomerGroupAsync]: Executing GET ' + Restful.ClientBaseURL);

        var CallResponse: TCallResponse;
        try

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var ReturnCustomerGroup:=TJson.JsonToObject<TReturnCustomerGroup>(Restful.Content);
                try

                    var RowCount:=Length(ReturnCustomerGroup.Id);
                    TargetGrid.RowCount:=RowCount;
                    TargetGrid.ColCount:=5;

                    TargetGrid.Cells[0, 0]:='';
                    TargetGrid.Cells[1, 0]:=ReturnCustomerGroup._Id;
                    TargetGrid.Cells[2, 0]:=ReturnCustomerGroup._SourceDbName;
                    TargetGrid.Cells[3, 0]:=ReturnCustomerGroup._ErpCode;
                    TargetGrid.Cells[4, 0]:=ReturnCustomerGroup._Description;

                    for var iCNT:=1{Skip header} to RowCount do
                    begin
                        TargetGrid.Cells[1, iCNT]:=ReturnCustomerGroup.Id[iCNT - 1].ToString();
                        TargetGrid.Cells[2, iCNT]:=ReturnCustomerGroup.SourceDbName[iCNT - 1];
                        TargetGrid.Cells[3, iCNT]:=ReturnCustomerGroup.ErpCode[iCNT - 1];
                        TargetGrid.Cells[4, iCNT]:=ReturnCustomerGroup.Description[iCNT - 1];
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Restful.StatusCode;
                    Service.Logger.Log('[GetCustomerGroupAsync]: Returned status code is ' + Restful.StatusCode.ToString());

                finally
                    ReturnCustomerGroup.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[GetCustomerGroupAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[GetCustomerGroupAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetCustomerGroupAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

