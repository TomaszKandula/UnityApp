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
        procedure ReadOpenItemsAsync(PageNumber: integer;LoadedCompanies: TList<string>; Callback: TReadOpenItems);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TOpenItems = class(TInterfacedObject, IOpenItems)
    strict private
        procedure SetDefaultHeader(var SourceGrid: TStringGrid);
    public
        constructor Create();
        destructor Destroy(); override;
        function GetSSISDataAwaited(var DateTime: string; var Status: string): TCallResponse; virtual;
        procedure GetOpenItemsAsync(SourceDbName: string; CustomerNumber: Int64; Callback: TGetOpenItems); virtual;
        procedure ReadOpenItemsAsync(PageNumber: integer;LoadedCompanies: TList<string>; Callback: TReadOpenItems); virtual;
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
end;


destructor TOpenItems.Destroy();
begin
    inherited;
end;


procedure TOpenItems.SetDefaultHeader(var SourceGrid: TStringGrid);
begin
    SourceGrid.Cells[0, 0]:='';
    SourceGrid.Cells[1 ,0]:=TOpenItemsFields._InvoiceNumber;
    SourceGrid.Cells[2 ,0]:=TOpenItemsFields._Text;
    SourceGrid.Cells[3 ,0]:=TOpenItemsFields._AdditionalText;
    SourceGrid.Cells[4, 0]:=TOpenItemsFields._OpenAmount;
    SourceGrid.Cells[5, 0]:=TOpenItemsFields._Amount;
    SourceGrid.Cells[6, 0]:=TOpenItemsFields._OpenCurAmount;
    SourceGrid.Cells[7, 0]:=TOpenItemsFields._CurAmount;
    SourceGrid.Cells[8, 0]:=TOpenItemsFields._Iso;
    SourceGrid.Cells[9 ,0]:=TOpenItemsFields._DueDate;
    SourceGrid.Cells[10,0]:=TOpenItemsFields._ValueDate;
    SourceGrid.Cells[11,0]:=TOpenItemsFields._ControlStatus;
    SourceGrid.Cells[12,0]:=TOpenItemsFields._PmtStatus;
    SourceGrid.Cells[13,0]:=TOpenItemsFields._Address1;
    SourceGrid.Cells[14,0]:=TOpenItemsFields._Address2;
    SourceGrid.Cells[15,0]:=TOpenItemsFields._Address3;
    SourceGrid.Cells[16,0]:=TOpenItemsFields._PostalNumber;
    SourceGrid.Cells[17,0]:=TOpenItemsFields._PostalArea;
    SourceGrid.Cells[18,0]:=TOpenItemsFields._SourceDbName;
    SourceGrid.Cells[19,0]:=TOpenItemsFields._CustNumber;
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

            if (Rest.Execute) and (Rest.StatusCode = 200) then
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

        var CallResponse: TCallResponse;
        var Grid:=TStringGrid.Create(nil);
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var ReturnOpenItems:=TJson.JsonToObject<TReturnOpenItems>(Rest.Content);
                try

                    var RowCount:=Length(ReturnOpenItems.OpenItems);
                    Grid.RowCount:=RowCount + 1; // Add header
                    Grid.ColCount:=20;

                    if not FileExists(Service.Settings.DirLayouts + TCommon.OpenItemsLayout) then
                    begin
                        SetDefaultHeader(Grid);
                        Grid.SetColWidth(10, 20, 400);
                    end
                    else
                    begin

                        var LLayoutColumns: TLayoutColumns;
                        try

                            var LResponse:=Service.Mediator.Utility.LoadAgeLayoutSync(
                                Service.Settings.DirLayouts + TCommon.OpenItemsLayout,
                                LLayoutColumns
                            );

                            if not LResponse.IsSucceeded then
                            begin
                                SetDefaultHeader(Grid);
                                Grid.SetColWidth(10, 20, 400);
                            end
                            else
                            begin

                                Grid.ColCount:=Length(LLayoutColumns.Columns);

                                for var Index:=0 to Grid.ColCount - 1 do
                                begin
                                    var ColumnNumber:=LLayoutColumns.Columns[Index].Number;
                                    Grid.Cells[ColumnNumber, 0] :=LLayoutColumns.Columns[Index].Name;
                                    Grid.ColWidths[ColumnNumber]:=LLayoutColumns.Columns[Index].Width;
                                end;

                            end;

                        finally
                            LLayoutColumns.Free();
                        end;

                    end;

                    for var Index:=1 to RowCount do
                    begin
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._InvoiceNumber), Index]:=ReturnOpenItems.OpenItems[Index - 1].InvoiceNumber;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._Text), Index]:=ReturnOpenItems.OpenItems[Index - 1].Text;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._AdditionalText), Index]:=ReturnOpenItems.OpenItems[Index - 1].AdditionalText;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._OpenAmount), Index]:=ReturnOpenItems.OpenItems[Index - 1].OpenAmount.ToString();
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._Amount), Index]:=ReturnOpenItems.OpenItems[Index - 1].Amount.ToString();
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._OpenCurAmount), Index]:=ReturnOpenItems.OpenItems[Index - 1].OpenCurAmount.ToString();
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._CurAmount), Index]:=ReturnOpenItems.OpenItems[Index - 1].CurAmount.ToString();
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._Iso), Index]:=ReturnOpenItems.OpenItems[Index - 1].Iso;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._DueDate), Index]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[Index - 1].DueDate, TCalendar.DateOnly);
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._ValueDate),Index]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[Index - 1].ValueDate, TCalendar.DateOnly);
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._ControlStatus),Index]:=ReturnOpenItems.OpenItems[Index - 1].ControlStatus.ToString();
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._PmtStatus),Index]:=ReturnOpenItems.OpenItems[Index - 1].PmtStatus.ToString();
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._Address1),Index]:=ReturnOpenItems.OpenItems[Index - 1].Address1;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._Address2),Index]:=ReturnOpenItems.OpenItems[Index - 1].Address2;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._Address3),Index]:=ReturnOpenItems.OpenItems[Index - 1].Address3;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._PostalNumber),Index]:=ReturnOpenItems.OpenItems[Index - 1].PostalNumber;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._PostalArea),Index]:=ReturnOpenItems.OpenItems[Index - 1].PostalArea;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._SourceDbName),Index]:=ReturnOpenItems.OpenItems[Index - 1].SourceDbName;
                        Grid.Cells[Grid.GetCol(TOpenItemsFields._CustNumber),Index]:=ReturnOpenItems.OpenItems[Index - 1].CustNumber.ToString();
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[GetOpenItemsAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnOpenItems.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[GetOpenItemsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[GetOpenItemsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[GetOpenItemsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetOpenItemsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(Grid, CallResponse);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TOpenItems.ReadOpenItemsAsync(PageNumber: integer; LoadedCompanies: TList<string>; Callback: TReadOpenItems);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest:=Service.InvokeRest();
	    Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'openitems/customers/?page=' + PageNumber.ToString();
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[ReadOpenItemsAsync]: Executing POST ' + Rest.ClientBaseURL);

        var CallResponse: TCallResponse;
        var Grid:=TStringGrid.Create(nil);
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
                    Grid.RowCount:=RowCount + 1; // Add header
                    Grid.ColCount:=37;

                    Grid.Cells[0, 0]:='';
                    Grid.Cells[1, 0]:=TOpenItemsFields._SourceDbName;
                    Grid.Cells[2, 0]:=TOpenItemsFields._CustNumber;
                    Grid.Cells[3, 0]:=TOpenItemsFields._VoucherType;
                    Grid.Cells[4, 0]:=TOpenItemsFields._OpenCurAmount;
                    Grid.Cells[5, 0]:=TOpenItemsFields._OpenAmount;
                    Grid.Cells[6, 0]:=TOpenItemsFields._CustName;
                    Grid.Cells[7, 0]:=TOpenItemsFields._Iso;
                    Grid.Cells[8, 0]:=TOpenItemsFields._CurAmount;
                    Grid.Cells[9, 0]:=TOpenItemsFields._Amount;
                    Grid.Cells[10,0]:=TOpenItemsFields._InvoiceNumber;
                    Grid.Cells[11,0]:=TOpenItemsFields._DueDate;
                    Grid.Cells[12,0]:=TOpenItemsFields._Inf4;
                    Grid.Cells[13,0]:=TOpenItemsFields._Inf7;
                    Grid.Cells[14,0]:=TOpenItemsFields._CreditLimit;
                    Grid.Cells[15,0]:=TOpenItemsFields._Country;
                    Grid.Cells[16,0]:=TOpenItemsFields._PmtTerms;
                    Grid.Cells[17,0]:=TOpenItemsFields._PmtStatus;
                    Grid.Cells[18,0]:=TOpenItemsFields._Agent;
                    Grid.Cells[19,0]:=TOpenItemsFields._ControlStatus;
                    Grid.Cells[20,0]:=TOpenItemsFields._Address1;
                    Grid.Cells[21,0]:=TOpenItemsFields._Address2;
                    Grid.Cells[22,0]:=TOpenItemsFields._Address3;
                    Grid.Cells[23,0]:=TOpenItemsFields._PostalNumber;
                    Grid.Cells[24,0]:=TOpenItemsFields._PostalArea;
                    Grid.Cells[25,0]:=TOpenItemsFields._GenAccNumber;
                    Grid.Cells[26,0]:=TOpenItemsFields._ValueDate;
                    Grid.Cells[27,0]:=TOpenItemsFields._Division;
                    Grid.Cells[28,0]:=TOpenItemsFields._Text;
                    Grid.Cells[29,0]:=TOpenItemsFields._DirectDebit;
                    Grid.Cells[30,0]:=TOpenItemsFields._AdditionalText;
                    Grid.Cells[31,0]:=TOpenItemsFields._SalesResponsible;
                    Grid.Cells[32,0]:=TOpenItemsFields._CustomerGroup;
                    Grid.Cells[33,0]:=TOpenItemsFields._PersonResponsible;
                    Grid.Cells[34,0]:=TOpenItemsFields._AccountType;
                    Grid.Cells[35,0]:=TOpenItemsFields._VoucherNumber;
                    Grid.Cells[36,0]:=TOpenItemsFields._VoucherDate;

                    for var Index:=1 to RowCount do
                    begin
                        Grid.Cells[1, Index]:=ReturnOpenItems.OpenItems[Index - 1].SourceDbName;
                        Grid.Cells[2, Index]:=ReturnOpenItems.OpenItems[Index - 1].CustNumber.ToString();
                        Grid.Cells[3, Index]:=ReturnOpenItems.OpenItems[Index - 1].VoucherType.ToString();
                        Grid.Cells[4, Index]:=ReturnOpenItems.OpenItems[Index - 1].OpenCurAmount.ToString();
                        Grid.Cells[5, Index]:=ReturnOpenItems.OpenItems[Index - 1].OpenAmount.ToString();
                        Grid.Cells[6, Index]:=ReturnOpenItems.OpenItems[Index - 1].CustName;
                        Grid.Cells[7, Index]:=ReturnOpenItems.OpenItems[Index - 1].Iso;
                        Grid.Cells[8, Index]:=ReturnOpenItems.OpenItems[Index - 1].CurAmount.ToString();
                        Grid.Cells[9, Index]:=ReturnOpenItems.OpenItems[Index - 1].Amount.ToString();
                        Grid.Cells[10,Index]:=ReturnOpenItems.OpenItems[Index - 1].InvoiceNumber;
                        Grid.Cells[11,Index]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[Index - 1].DueDate, TCalendar.DateOnly);
                        Grid.Cells[12,Index]:=ReturnOpenItems.OpenItems[Index - 1].Inf4;
                        Grid.Cells[13,Index]:=ReturnOpenItems.OpenItems[Index - 1].Inf7;
                        Grid.Cells[14,Index]:=ReturnOpenItems.OpenItems[Index - 1].CreditLimit.ToString();
                        Grid.Cells[15,Index]:=ReturnOpenItems.OpenItems[Index - 1].Country.ToString();
                        Grid.Cells[16,Index]:=ReturnOpenItems.OpenItems[Index - 1].PmtTerms.ToString();
                        Grid.Cells[17,Index]:=ReturnOpenItems.OpenItems[Index - 1].PmtStatus.ToString();
                        Grid.Cells[18,Index]:=ReturnOpenItems.OpenItems[Index - 1].Agent;
                        Grid.Cells[19,Index]:=ReturnOpenItems.OpenItems[Index - 1].ControlStatus.ToString();
                        Grid.Cells[20,Index]:=ReturnOpenItems.OpenItems[Index - 1].Address1;
                        Grid.Cells[21,Index]:=ReturnOpenItems.OpenItems[Index - 1].Address2;
                        Grid.Cells[22,Index]:=ReturnOpenItems.OpenItems[Index - 1].Address3;
                        Grid.Cells[23,Index]:=ReturnOpenItems.OpenItems[Index - 1].PostalNumber;
                        Grid.Cells[24,Index]:=ReturnOpenItems.OpenItems[Index - 1].PostalArea;
                        Grid.Cells[25,Index]:=ReturnOpenItems.OpenItems[Index - 1].GenAccNumber.ToString();
                        Grid.Cells[26,Index]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[Index - 1].ValueDate, TCalendar.DateOnly);
                        Grid.Cells[27,Index]:=ReturnOpenItems.OpenItems[Index - 1].Division.ToString();
                        Grid.Cells[28,Index]:=ReturnOpenItems.OpenItems[Index - 1].Text;
                        Grid.Cells[29,Index]:=ReturnOpenItems.OpenItems[Index - 1].DirectDebit;
                        Grid.Cells[30,Index]:=ReturnOpenItems.OpenItems[Index - 1].AdditionalText;
                        Grid.Cells[31,Index]:=ReturnOpenItems.OpenItems[Index - 1].SalesResponsible;
                        Grid.Cells[32,Index]:=ReturnOpenItems.OpenItems[Index - 1].CustomerGroup;
                        Grid.Cells[33,Index]:=ReturnOpenItems.OpenItems[Index - 1].PersonResponsible;
                        Grid.Cells[34,Index]:=ReturnOpenItems.OpenItems[Index - 1].AccountType;
                        Grid.Cells[35,Index]:=ReturnOpenItems.OpenItems[Index - 1].VoucherNumber.ToString();
                        Grid.Cells[36,Index]:=THelpers.FormatDateTime(ReturnOpenItems.OpenItems[Index - 1].VoucherDate, TCalendar.DateOnly);
                    end;

                    CallResponse.IsSucceeded:=True;
                    CallResponse.ReturnedCode:=Rest.StatusCode;
                    Service.Logger.Log('[ReadOpenItemsAsync]: Returned status code is ' + Rest.StatusCode.ToString());

                finally
                    ReturnOpenItems.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[ReadOpenItemsAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[ReadOpenItemsAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[ReadOpenItemsAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

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
            if Assigned(Callback) then Callback(Grid, CallResponse);
            if Assigned(Grid) then Grid.Free();
        end);

    end);

    NewTask.Start();

end;


end.

