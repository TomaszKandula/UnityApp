unit Async.Companies;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Classes,
    System.Diagnostics,
    System.Win.ComObj,
    System.SyncObjs,
    System.Threading,
    System.Generics.Collections,
    Data.Win.ADODB,
    Data.DB,
    Unity.Grid,
    Unity.Enums,
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
        function GetCompanyDetailsAwaited(CoCode: string; Branch: string): TCompanyDetails;

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
        function GetCompanyDetailsAwaited(CoCode: string; Branch: string): TCompanyDetails;

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
    Handler.Database{Legacy}, //remove
    Handler.Sql{Legacy}, //remove
    Unity.Sql{Legacy}, //remove
    Unity.Helpers,
    Unity.Settings,
    Unity.StatusBar,
    Unity.EventLogger,
    Unity.SessionService,
    Unity.Chars,
    Unity.Common,
    Unity.DateTimeFormats,
    Sync.Documents,
    Bcrypt,
    DbModel{Legacy}; //remove


function TCompanies.GetCompanyDetailsAwaited(CoCode: string; Branch: string): TCompanyDetails; // replace with rest
begin

    var CompanyDetails: TCompanyDetails;
    var NewTask: ITask:=TTask.Create(procedure
    begin

        var LbuName:    string;
        var LbuAddress: string;
        var LbuPhone:   string;
        var LbuEmail:   string;
        var BanksData:  string;

        var CallResponse: TCallResponse;
        var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
        try

            try

                DataTables.Columns.Add(TCompanyData.CoName);
                DataTables.Columns.Add(TCompanyData.CoAddress);
                DataTables.Columns.Add(TCompanyData.TelephoneNumbers);
                DataTables.Columns.Add(TCompanyData.SendNoteFrom);
                DataTables.Columns.Add(TCompanyData.BankAccounts);
                DataTables.CustFilter:=TSql.WHERE + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(CoCode) + TSql._AND + TCompanyData.Branch + TSql.EQUAL + QuotedStr(Branch);
                DataTables.OpenTable(TCompanyData.CompanyData);

                if DataTables.DataSet.RecordCount = 1 then
                begin
                    CompanyDetails.LbuName   :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.CoName].Value);
                    CompanyDetails.LbuAddress:=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.CoAddress].Value);
                    CompanyDetails.LbuPhone  :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.TelephoneNumbers].Value);
                    CompanyDetails.LbuEmail  :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.SendNoteFrom].Value);
                    CompanyDetails.LbuBanks  :=THelpers.OleGetStr(DataTables.DataSet.Fields[TCompanyData.BankAccounts].Value);
                end;

                CallResponse.IsSucceeded:=True;

            except
                on E: Exception do
                begin
                    CallResponse.IsSucceeded:=False;
                    CallResponse.LastMessage:='[GetCompanyDetailsAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                    ThreadFileLog.Log(CallResponse.LastMessage);
                end;

            end;

        finally
            DataTables.Free();
        end;

    end);

    NewTask.Start();
    TTask.WaitForAll(NewTask);
    Result:=CompanyDetails;

end;


procedure TCompanies.GetCompanyEmailsAwaited(SourceList: TStringList; var TargetList: TStringList); // replace with rest
begin

    if (not SourceList.Count > 0) or (not Assigned(TargetList)) then Exit();

    var EmailList:=TStringList.Create();
    try

        var NewTask: ITask:=TTask.Create(procedure
        begin

            var DataTables: TDataTables:=TDataTables.Create(SessionService.FDbConnect);
            try

                try

                    DataTables.Columns.Add(TSql.DISTINCT + TCompanyData.SendNoteFrom);

                    var CoCodeList: string;
                    for var iCNT:=0 to SourceList.Count - 1 do
                    begin

                        if iCNT < (SourceList.Count - 1) then
                            CoCodeList:=CoCodeList + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(SourceList.Strings[iCNT]) + TSql._OR
                        else
                            CoCodeList:=CoCodeList + TCompanyData.CoCode + TSql.EQUAL + QuotedStr(SourceList.Strings[iCNT]);

                    end;

                    DataTables.CustFilter:=TSql.WHERE + CoCodeList;
                    DataTables.OpenTable(TCompanyData.CompanyData);

                    while not DataTables.DataSet.EOF do
                    begin
                        EmailList.Add(DataTables.DataSet.Fields[0].Value);
                        DataTables.DataSet.MoveNext;
                     end;

                except
                    on E: Exception do
                        ThreadFileLog.Log('[GetCompanyEmailsAwaited]: Cannot execute. Error has been thrown: ' + E.Message);
                end;

            finally
                DataTables.Free();
            end;

        end);

        NewTask.Start();
        TTask.WaitForAll(NewTask);
        TargetList.AddStrings(EmailList);

    finally
        EmailList.Free();
    end;

end;


end.
