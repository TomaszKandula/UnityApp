unit Async.Companies;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    Unity.Records;


type


    ICompanies = interface(IInterface)
    ['{E7616759-7564-44A4-BEEF-BDD220040E1E}']

        /// <summary>
        /// Allow to load async. some company data like name, address, phone etc. There is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method always awaits for task to be completed and makes no callback to main thread.
        /// </remarks>
        function GetCompanyDetailsAwaited(CoCode: string; var CompanyDetails: TCompanyDetails): TCallResponse;

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
        function GetCompanyDetailsAwaited(CoCode: string; var CompanyDetails: TCompanyDetails): TCallResponse;

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
    Unity.Helpers,
    Unity.EventLogger,
    Unity.SessionService;


function TCompanies.GetCompanyDetailsAwaited(CoCode: string; var CompanyDetails: TCompanyDetails): TCallResponse;
begin

    var ReturnCompanyDetails: TCompanyDetails;
    var CallResponse: TCallResponse;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        try




        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);

    CompanyDetails:=ReturnCompanyDetails;
    Result:=CallResponse;

end;


procedure TCompanies.GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList); // replace with rest
begin

    if (not SourceList.Count > 0) or (not Assigned(TargetList)) then Exit();

    var EmailList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

//            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
//            try
//
//                try
//
//                    DataTables.Columns.Add(TSql.DISTINCT + TCompanyData.SendNoteFrom);
//
//                    var CoCodeList: string;
//                    for var iCNT:=0 to SourceList.Count - 1 do
//                    begin
//
//                        if iCNT < (SourceList.Count - 1) then
//                            CoCodeList:=CoCodeList + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(SourceList.Strings[iCNT]) + TSql._OR
//                        else
//                            CoCodeList:=CoCodeList + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(SourceList.Strings[iCNT]);
//
//                    end;
//
//                    DataTables.CustFilter:=TSql.WHERE + CoCodeList;
//                    DataTables.OpenTable(TCompanyData.CompanyData);
//
//                    while not DataTables.DataSet.EOF do
//                    begin
//                        EmailList.Add(DataTables.DataSet.Fields[0].Value);
//                        DataTables.DataSet.MoveNext;
//                     end;
//
//                except
//                    on E: Exception do
//                        ThreadFileLog.Log('[GetCompanyEmailsAwaited]: Cannot execute. Error has been thrown: ' + E.Message);
//                end;
//
//            finally
//                DataTables.Free();
//            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        TargetList.AddStrings(EmailList);

    finally
        EmailList.Free();
    end;

end;


end.
