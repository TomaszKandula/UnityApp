unit Async.Companies;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Generics.Collections,
    System.Classes,
    Unity.Records,
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
        function GetCompanyDetailsAwaited(CompanyCode: string; var CompanyDetails: TCompanyDetails): TCallResponse;

        /// <summary>
        /// Allow to load async. list of emails for given CoCodes. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList);

    end;


    TCompanies = class(TInterfacedObject, ICompanies)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Allow to load async. some company data like name, address, phone etc. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyDetailsAwaited(CompanyCode: string; var CompanyDetails: TCompanyDetails): TCallResponse;

        /// <summary>
        /// Allow to load async. list of emails for given CoCodes. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        procedure GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList);

    end;


implementation


uses
    System.Threading,
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
    Unity.Helpers,
    Unity.EventLogger,
    Unity.SessionService,
    Api.CompanyData;


function TCompanies.GetCompanyDetailsAwaited(CompanyCode: string; var CompanyDetails: TCompanyDetails): TCallResponse;//refactor!
begin

    var CompanyData: TCompanyData;
    var CallResponse: TCallResponse;
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
            Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'companies/' + CompanyCode;
            Restful.RequestMethod:=TRESTRequestMethod.rmGET;
            ThreadFileLog.Log('[GetCompanyDetailsAwaited]: Executing GET ' + Restful.ClientBaseURL);

            try

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin
                    CompanyData:=TJson.JsonToObject<TCompanyData>(Restful.Content);
                    CallResponse.IsSucceeded:=True;
                    ThreadFileLog.Log('[GetCompanyDetailsAwaited]: Returned status code is ' + Restful.StatusCode.ToString());
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
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            except on
                E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetCompanyDetailsAwaited]: Cannot execute the request. Description: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);

        if Assigned(CompanyData) then
        begin

            CompanyDetails.LbuName   :=CompanyData.CompanyName;
            CompanyDetails.LbuAddress:=CompanyData.CompanyAddress;
            CompanyDetails.LbuPhones :=CompanyData.CompanyPhones;
            CompanyDetails.LbuEmails :=CompanyData.CompanyEmails;

            SetLength(CompanyDetails.LbuBanks, Length(CompanyData.CompanyBanks));
            for var iCNT:=0 to Length(CompanyData.CompanyBanks) - 1 do
            begin

                if not Assigned(CompanyDetails.LbuBanks[iCNT]) then
                    CompanyDetails.LbuBanks[iCNT]:=TBankDetails.Create();

                CompanyDetails.LbuBanks[iCNT].BankName:=CompanyData.CompanyBanks[iCNT].BankName;
                CompanyDetails.LbuBanks[iCNT].BankAcc :=CompanyData.CompanyBanks[iCNT].BankAcc;
                CompanyDetails.LbuBanks[iCNT].BankCode:=CompanyData.CompanyBanks[iCNT].BankCode;
                CompanyDetails.LbuBanks[iCNT].BankIso :=CompanyData.CompanyBanks[iCNT].BankIso;

            end;

        end;

    finally
        CompanyData.Free();
        Result:=CallResponse;
    end;

end;


function TCompanies.GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList): TCallResponse; // replace with rest
begin

    if (not SourceList.Count > 0) or (not Assigned(TargetList)) then Exit();

    var CallResponse: TCallResponse;
    var EmailList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin











        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        TargetList.AddStrings(EmailList);

    finally
        EmailList.Free();
        Result:=CallResponse;
    end;

end;


end.
