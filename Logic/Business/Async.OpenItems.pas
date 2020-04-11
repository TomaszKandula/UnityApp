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
        /// Allow to async. load current open items from SQL database.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure ReadOpenItemsAsync(OpenItemsGrid: TStringGrid; LoadedCompanies: TList<string>; Callback: TReadOpenItems);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TOpenItems = class(TInterfacedObject, IOpenItems)
    strict private
        function  FLoadToGridSync(OpenItemsGrid: TStringGrid; LoadedCompanies: TList<string>): TCallResponse;
        procedure FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
    public
        constructor Create();
        destructor Destroy(); override;
        function GetSSISDataAwaited(var DateTime: string; var Status: string): TCallResponse; virtual;
        procedure ReadOpenItemsAsync(OpenItemsGrid: TStringGrid; LoadedCompanies: TList<string>; Callback: TReadOpenItems); virtual;
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
    Api.OpenItemsFields;


constructor TOpenItems.Create();
begin
end;


destructor TOpenItems.Destroy();
begin
    inherited;
end;


function TOpenItems.GetSSISDataAwaited(var DateTime: string; var Status: string): TCallResponse;
begin

    var CallResponse: TCallResponse;
    var GotDateTime: string;
    var GotStatus: string;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'automation/customers/openitems/ssis/';
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;
        Service.Logger.Log('[GetSSISDataAwaited]: Executing GET ' + Rest.ClientBaseURL);

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnSsisData:=TJson.JsonToObject<TReturnSsisData>(Rest.Content);
                try

                    GotDateTime:=ReturnSsisData.CustExtractDt;
                    GotStatus:=ReturnSsisData.StatusCode;
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
    DateTime:=GotDateTime;
    Status  :=GotStatus;

end;


procedure TOpenItems.ReadOpenItemsAsync(OpenItemsGrid: TStringGrid; LoadedCompanies: TList<string>; Callback: TReadOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var OpenItemsData: TOpenItemsPayLoad;

        try
            FLoadToGridSync(OpenItemsGrid, LoadedCompanies);
            FCalculateOpenItems(OpenItemsGrid, OpenItemsData);
            CallResponse.IsSucceeded:=True;
        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[ReadOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(OpenItemsData, CallResponse);
        end);

    end);

    NewTask.Start;

end;


function TOpenItems.FLoadToGridSync(OpenItemsGrid: TStringGrid; LoadedCompanies: TList<string>): TCallResponse;
begin

    var CallResponse: TCallResponse;

    var Rest:=Service.InvokeRest();
	Rest.AccessToken:=Service.AccessToken;
    Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

    Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'openitems/customers/';
    Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
    Service.Logger.Log('[FLoadToGrid]: Executing POST ' + Rest.ClientBaseURL);

    try

        var UserCompanySelection:=TUserCompanySelection.Create();
        try
            UserCompanySelection.SelectedCoCodes:=LoadedCompanies.ToArray();
            Rest.CustomBody:=TJson.ObjectToJsonString(UserCompanySelection);
        finally
            UserCompanySelection.Free();
        end;

        if (Rest.Execute) and (Rest.StatusCode = 200) then
        begin

            var ReturnOpenItems:=TJson.JsonToObject<TReturnOpenItems>(Rest.Content);
            try

                var RowCount:=Length(ReturnOpenItems.OpenItems);
                OpenItemsGrid.RowCount:=RowCount + 1; // Add header
                OpenItemsGrid.ColCount:=37;

                OpenItemsGrid.Cells[0, 0]:='';
                OpenItemsGrid.Cells[1, 0]:=TOpenItemsFields._SourceDbName;
                OpenItemsGrid.Cells[2, 0]:=TOpenItemsFields._CustNumber;
                OpenItemsGrid.Cells[3, 0]:=TOpenItemsFields._VoucherType;
                OpenItemsGrid.Cells[4, 0]:=TOpenItemsFields._OpenCurAmount;
                OpenItemsGrid.Cells[5, 0]:=TOpenItemsFields._OpenAmount;
                OpenItemsGrid.Cells[6, 0]:=TOpenItemsFields._CustName;
                OpenItemsGrid.Cells[7, 0]:=TOpenItemsFields._Iso;
                OpenItemsGrid.Cells[8, 0]:=TOpenItemsFields._CurAmount;
                OpenItemsGrid.Cells[9, 0]:=TOpenItemsFields._Amount;
                OpenItemsGrid.Cells[10,0]:=TOpenItemsFields._InvoiceNumber;
                OpenItemsGrid.Cells[11,0]:=TOpenItemsFields._DueDate;
                OpenItemsGrid.Cells[12,0]:=TOpenItemsFields._Inf4;
                OpenItemsGrid.Cells[13,0]:=TOpenItemsFields._Inf7;
                OpenItemsGrid.Cells[14,0]:=TOpenItemsFields._CreditLimit;
                OpenItemsGrid.Cells[15,0]:=TOpenItemsFields._Country;
                OpenItemsGrid.Cells[16,0]:=TOpenItemsFields._PmtTerms;
                OpenItemsGrid.Cells[17,0]:=TOpenItemsFields._PmtStatus;
                OpenItemsGrid.Cells[18,0]:=TOpenItemsFields._Agent;
                OpenItemsGrid.Cells[19,0]:=TOpenItemsFields._ControlStatus;
                OpenItemsGrid.Cells[20,0]:=TOpenItemsFields._Address1;
                OpenItemsGrid.Cells[21,0]:=TOpenItemsFields._Address2;
                OpenItemsGrid.Cells[22,0]:=TOpenItemsFields._Address3;
                OpenItemsGrid.Cells[23,0]:=TOpenItemsFields._PostalNumber;
                OpenItemsGrid.Cells[24,0]:=TOpenItemsFields._PostalArea;
                OpenItemsGrid.Cells[25,0]:=TOpenItemsFields._GenAccNumber;
                OpenItemsGrid.Cells[26,0]:=TOpenItemsFields._ValueDate;
                OpenItemsGrid.Cells[27,0]:=TOpenItemsFields._Division;
                OpenItemsGrid.Cells[28,0]:=TOpenItemsFields._Text;
                OpenItemsGrid.Cells[29,0]:=TOpenItemsFields._DirectDebit;
                OpenItemsGrid.Cells[30,0]:=TOpenItemsFields._AdditionalText;
                OpenItemsGrid.Cells[31,0]:=TOpenItemsFields._SalesResponsible;
                OpenItemsGrid.Cells[32,0]:=TOpenItemsFields._CustomerGroup;
                OpenItemsGrid.Cells[33,0]:=TOpenItemsFields._PersonResponsible;
                OpenItemsGrid.Cells[34,0]:=TOpenItemsFields._AccountType;
                OpenItemsGrid.Cells[35,0]:=TOpenItemsFields._VoucherNumber;
                OpenItemsGrid.Cells[36,0]:=TOpenItemsFields._VoucherDate;

                for var iCNT:=1 to RowCount do
                begin
                    OpenItemsGrid.Cells[1, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].SourceDbName;
                    OpenItemsGrid.Cells[2, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].CustNumber.ToString();
                    OpenItemsGrid.Cells[3, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].VoucherType.ToString();
                    OpenItemsGrid.Cells[4, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].OpenCurAmount.ToString();
                    OpenItemsGrid.Cells[5, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].OpenAmount.ToString();
                    OpenItemsGrid.Cells[6, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].CustName;
                    OpenItemsGrid.Cells[7, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Iso;
                    OpenItemsGrid.Cells[8, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].CurAmount.ToString();
                    OpenItemsGrid.Cells[9, iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Amount.ToString();
                    OpenItemsGrid.Cells[10,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].InvoiceNumber;
                    OpenItemsGrid.Cells[11,iCNT]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[iCNT - 1].DueDate, TCalendar.DateOnly);
                    OpenItemsGrid.Cells[12,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Inf4;
                    OpenItemsGrid.Cells[13,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Inf7;
                    OpenItemsGrid.Cells[14,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].CreditLimit.ToString();
                    OpenItemsGrid.Cells[15,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Country.ToString();
                    OpenItemsGrid.Cells[16,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].PmtTerms.ToString();
                    OpenItemsGrid.Cells[17,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].PmtStatus.ToString();
                    OpenItemsGrid.Cells[18,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Agent;
                    OpenItemsGrid.Cells[19,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].ControlStatus.ToString();
                    OpenItemsGrid.Cells[20,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Address1;
                    OpenItemsGrid.Cells[21,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Address2;
                    OpenItemsGrid.Cells[22,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Address3;
                    OpenItemsGrid.Cells[23,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].PostalNumber;
                    OpenItemsGrid.Cells[24,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].PostalArea;
                    OpenItemsGrid.Cells[25,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].GenAccNumber.ToString();
                    OpenItemsGrid.Cells[26,iCNT]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[iCNT - 1].ValueDate, TCalendar.DateOnly);
                    OpenItemsGrid.Cells[27,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Division.ToString();
                    OpenItemsGrid.Cells[28,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].Text;
                    OpenItemsGrid.Cells[29,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].DirectDebit;
                    OpenItemsGrid.Cells[30,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].AdditionalText;
                    OpenItemsGrid.Cells[31,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].SalesResponsible;
                    OpenItemsGrid.Cells[32,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].CustomerGroup;
                    OpenItemsGrid.Cells[33,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].PersonResponsible;
                    OpenItemsGrid.Cells[34,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].AccountType;
                    OpenItemsGrid.Cells[35,iCNT]:=ReturnOpenItems.OpenItems[iCNT - 1].VoucherNumber.ToString();
                    OpenItemsGrid.Cells[36,iCNT]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[iCNT - 1].VoucherDate, TCalendar.DateOnly);
                end;

                CallResponse.IsSucceeded:=True;
                CallResponse.ReturnedCode:=Rest.StatusCode;
                Service.Logger.Log('[FLoadToGrid]: Returned status code is ' + Rest.StatusCode.ToString());

            finally
                ReturnOpenItems.Free();
            end;

        end
        else
        begin

            if not String.IsNullOrEmpty(Rest.ExecuteError) then
                CallResponse.LastMessage:='[FLoadToGrid]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
            else
                if String.IsNullOrEmpty(Rest.Content) then
                    CallResponse.LastMessage:='[FLoadToGrid]: Invalid server response. Please contact IT Support.'
                else
                    CallResponse.LastMessage:='[FLoadToGrid]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

            CallResponse.ReturnedCode:=Rest.StatusCode;
            CallResponse.IsSucceeded:=False;
            Service.Logger.Log(CallResponse.LastMessage);

        end;

    except on
        E: Exception do
        begin
            CallResponse.IsSucceeded:=False;
            CallResponse.LastMessage:='[FLoadToGrid]: Cannot execute the request. Description: ' + E.Message;
            Service.Logger.Log(CallResponse.LastMessage);
        end;

    end;

    Result:=CallResponse;

end;


procedure TOpenItems.FCalculateOpenItems(var InputGrid: TStringGrid; var OutputData: TOpenItemsPayLoad);
begin

    var VoucherNumber:=Service.Settings.GetStringValue(TConfigSections.Unallocated, 'VOUCHER_NUM', '0');

    var VoTpCol   :=InputGrid.GetCol(TOpenItemsFields._VoucherType);
    var OpenAmCol :=InputGrid.GetCol(TOpenItemsFields._OpenAmount);
    var PmtStatCol:=InputGrid.GetCol(TOpenItemsFields._PmtStatus);

    for var iCNT:=1 to InputGrid.RowCount - 1 do
    begin

        var InvoiceAmt: double:=StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);
        OutputData.OsAmount:=OutputData.OsAmount + InvoiceAmt;

        if THelpers.IsVoType(InputGrid.Cells[VoTpCol, iCNT]) = True then inc(OutputData.NumOfInvoices);

        if (StrToIntDef(InputGrid.Cells[PmtStatCol, iCNT], 0) < 0) and (THelpers.IsVoType(InputGrid.Cells[VoTpCol, iCNT]) = True) then
        begin
            inc(OutputData.OverdueItems);
            OutputData.OvdAmount:=OutputData.OvdAmount + StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);
        end;

        if (StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0) < 0) and (InputGrid.Cells[VoTpCol, iCNT] = VoucherNumber) then
            OutputData.UnallocatedAmt:=OutputData.UnallocatedAmt + StrToFloatDef(InputGrid.Cells[OpenAmCol, iCNT], 0);

    end;

    OutputData.UnallocatedAmt:=Abs(OutputData.UnallocatedAmt);
    OutputData.TotalItems:=InputGrid.RowCount - 1;

end;


end.

