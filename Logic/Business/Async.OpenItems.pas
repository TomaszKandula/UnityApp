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
    Api.ReturnOpenItems;


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

                var RowCount:=Length(ReturnOpenItems.SourceDbName);
                OpenItemsGrid.RowCount:=RowCount;
                OpenItemsGrid.ColCount:=37;

                OpenItemsGrid.Cells[0, 0]:='';
                OpenItemsGrid.Cells[1, 0]:=ReturnOpenItems._SourceDbName;
                OpenItemsGrid.Cells[2, 0]:=ReturnOpenItems._CustNumber;
                OpenItemsGrid.Cells[3, 0]:=ReturnOpenItems._VoucherType;
                OpenItemsGrid.Cells[4, 0]:=ReturnOpenItems._OpenCurAmount;
                OpenItemsGrid.Cells[5, 0]:=ReturnOpenItems._OpenAmount;
                OpenItemsGrid.Cells[6, 0]:=ReturnOpenItems._CustName;
                OpenItemsGrid.Cells[7, 0]:=ReturnOpenItems._Iso;
                OpenItemsGrid.Cells[8, 0]:=ReturnOpenItems._CurAmount;
                OpenItemsGrid.Cells[9, 0]:=ReturnOpenItems._Amount;
                OpenItemsGrid.Cells[10,0]:=ReturnOpenItems._InvoiceNumber;
                OpenItemsGrid.Cells[11,0]:=ReturnOpenItems._DueDate;
                OpenItemsGrid.Cells[12,0]:=ReturnOpenItems._Inf4;
                OpenItemsGrid.Cells[13,0]:=ReturnOpenItems._Inf7;
                OpenItemsGrid.Cells[14,0]:=ReturnOpenItems._CreditLimit;
                OpenItemsGrid.Cells[15,0]:=ReturnOpenItems._Country;
                OpenItemsGrid.Cells[16,0]:=ReturnOpenItems._PmtTerms;
                OpenItemsGrid.Cells[17,0]:=ReturnOpenItems._PmtStatus;
                OpenItemsGrid.Cells[18,0]:=ReturnOpenItems._Agent;
                OpenItemsGrid.Cells[19,0]:=ReturnOpenItems._ControlStatus;
                OpenItemsGrid.Cells[20,0]:=ReturnOpenItems._Address1;
                OpenItemsGrid.Cells[21,0]:=ReturnOpenItems._Address2;
                OpenItemsGrid.Cells[22,0]:=ReturnOpenItems._Address3;
                OpenItemsGrid.Cells[23,0]:=ReturnOpenItems._PostalNumber;
                OpenItemsGrid.Cells[24,0]:=ReturnOpenItems._PostalArea;
                OpenItemsGrid.Cells[25,0]:=ReturnOpenItems._GenAccNumber;
                OpenItemsGrid.Cells[26,0]:=ReturnOpenItems._ValueDate;
                OpenItemsGrid.Cells[27,0]:=ReturnOpenItems._Division;
                OpenItemsGrid.Cells[28,0]:=ReturnOpenItems._Text;
                OpenItemsGrid.Cells[29,0]:=ReturnOpenItems._DirectDebit;
                OpenItemsGrid.Cells[30,0]:=ReturnOpenItems._AdditionalText;
                OpenItemsGrid.Cells[31,0]:=ReturnOpenItems._SalesResponsible;
                OpenItemsGrid.Cells[32,0]:=ReturnOpenItems._CustomerGroup;
                OpenItemsGrid.Cells[33,0]:=ReturnOpenItems._PersonResponsible;
                OpenItemsGrid.Cells[34,0]:=ReturnOpenItems._AccountType;
                OpenItemsGrid.Cells[35,0]:=ReturnOpenItems._VoucherNumber;
                OpenItemsGrid.Cells[36,0]:=ReturnOpenItems._VoucherDate;

                for var iCNT:=1{Skip header} to RowCount do
                begin
                    OpenItemsGrid.Cells[1, iCNT]:=ReturnOpenItems.SourceDbName[iCNT - 1];
                    OpenItemsGrid.Cells[2, iCNT]:=ReturnOpenItems.CustNumber[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[3, iCNT]:=ReturnOpenItems.VoucherType[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[4, iCNT]:=ReturnOpenItems.OpenCurAmount[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[5, iCNT]:=ReturnOpenItems.OpenAmount[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[6, iCNT]:=ReturnOpenItems.CustName[iCNT - 1];
                    OpenItemsGrid.Cells[7, iCNT]:=ReturnOpenItems.Iso[iCNT - 1];
                    OpenItemsGrid.Cells[8, iCNT]:=ReturnOpenItems.CurAmount[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[9, iCNT]:=ReturnOpenItems.Amount[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[10,iCNT]:=ReturnOpenItems.InvoiceNumber[iCNT - 1];
                    OpenItemsGrid.Cells[11,iCNT]:=THelpers.FormatDateTime(ReturnOpenItems.DueDate[iCNT - 1], TCalendar.DateOnly);
                    OpenItemsGrid.Cells[12,iCNT]:=ReturnOpenItems.Inf4[iCNT - 1];
                    OpenItemsGrid.Cells[13,iCNT]:=ReturnOpenItems.Inf7[iCNT - 1];
                    OpenItemsGrid.Cells[14,iCNT]:=ReturnOpenItems.CreditLimit[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[15,iCNT]:=ReturnOpenItems.Country[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[16,iCNT]:=ReturnOpenItems.PmtTerms[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[17,iCNT]:=ReturnOpenItems.PmtStatus[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[18,iCNT]:=ReturnOpenItems.Agent[iCNT - 1];
                    OpenItemsGrid.Cells[19,iCNT]:=ReturnOpenItems.ControlStatus[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[20,iCNT]:=ReturnOpenItems.Address1[iCNT - 1];
                    OpenItemsGrid.Cells[21,iCNT]:=ReturnOpenItems.Address2[iCNT - 1];
                    OpenItemsGrid.Cells[22,iCNT]:=ReturnOpenItems.Address3[iCNT - 1];
                    OpenItemsGrid.Cells[23,iCNT]:=ReturnOpenItems.PostalNumber[iCNT - 1];
                    OpenItemsGrid.Cells[24,iCNT]:=ReturnOpenItems.PostalArea[iCNT - 1];
                    OpenItemsGrid.Cells[25,iCNT]:=ReturnOpenItems.GenAccNumber[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[26,iCNT]:=THelpers.FormatDateTime(ReturnOpenItems.ValueDate[iCNT - 1], TCalendar.DateOnly);
                    OpenItemsGrid.Cells[27,iCNT]:=ReturnOpenItems.Division[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[28,iCNT]:=ReturnOpenItems.Text[iCNT - 1];
                    OpenItemsGrid.Cells[29,iCNT]:=ReturnOpenItems.DirectDebit[iCNT - 1];
                    OpenItemsGrid.Cells[30,iCNT]:=ReturnOpenItems.AdditionalText[iCNT - 1];
                    OpenItemsGrid.Cells[31,iCNT]:=ReturnOpenItems.SalesResponsible[iCNT - 1];
                    OpenItemsGrid.Cells[32,iCNT]:=ReturnOpenItems.CustomerGroup[iCNT - 1];
                    OpenItemsGrid.Cells[33,iCNT]:=ReturnOpenItems.PersonResponsible[iCNT - 1];
                    OpenItemsGrid.Cells[34,iCNT]:=ReturnOpenItems.AccountType[iCNT - 1];
                    OpenItemsGrid.Cells[35,iCNT]:=ReturnOpenItems.VoucherNumber[iCNT - 1].ToString();
                    OpenItemsGrid.Cells[36,iCNT]:=THelpers.FormatDateTime(ReturnOpenItems.VoucherDate[iCNT - 1], TCalendar.DateOnly);
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

    var VoTpCol   :=InputGrid.GetCol(TReturnOpenItems._VoucherType);
    var OpenAmCol :=InputGrid.GetCol(TReturnOpenItems._OpenAmount);
    var PmtStatCol:=InputGrid.GetCol(TReturnOpenItems._PmtStatus);

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

