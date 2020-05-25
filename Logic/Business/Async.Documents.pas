unit Async.Documents;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.SysUtils,
    Unity.Types,
    Unity.Records,
    Api.SendDocument;


type


    IDocuments = interface(IInterface)
    ['{14BBF3F3-945A-4A61-94BA-6A2EE10530A2}']
        /// <summary>
        /// Allow to send async. account statement(s) or reminder(s) from assigned email to specific customer(s).
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendAccountDocumentAsync(SendDocument: TSendDocument; Callback: TSendAccountDocument);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or and extend them.
    /// </remarks>
    TDocuments = class(TInterfacedObject, IDocuments)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure SendAccountDocumentAsync(SendDocument: TSendDocument; Callback: TSendAccountDocument);
    end;


implementation


uses
    System.Classes,
    System.Threading,
    System.Generics.Collections,
    REST.Types,
    REST.Json,
    Unity.Constants,
    Unity.Enums,
    Unity.Helpers,
    Unity.Service,
    Api.SentDocument;


constructor TDocuments.Create();
begin
end;


destructor TDocuments.Destroy();
begin
    inherited;
end;


procedure TDocuments.SendAccountDocumentAsync(SendDocument: TSendDocument; Callback: TSendAccountDocument);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        var Response: TSentDocument;
        var Rest:=Service.InvokeRest();

		Rest.AccessToken:=Service.AccessToken;
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);
        Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'mailer/document/';
        Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[SendAccountDocumentAsync]: Executing POST ' + Rest.ClientBaseURL);

        Rest.CustomBody:=TJson.ObjectToJsonString(SendDocument);
        SendDocument.Free();

        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                Response:=TJson.JsonToObject<TSentDocument>(Rest.Content);

                CallResponse.IsSucceeded:=Response.IsSucceeded;
                CallResponse.LastMessage:=Response.Error.ErrorDesc;
                CallResponse.ErrorCode  :=Response.Error.ErrorCode;

                Service.Logger.Log('[SendAccountDocumentAsync]: Returned status code is ' + Rest.StatusCode.ToString());
            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[SendAccountDocumentAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[SendAccountDocumentAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SendAccountDocumentAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except on
            E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendAccountDocumentAsync]: Cannot execute the request. Description: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse, Response);
            if Assigned(Response) then Response.Free();
        end);

    end);

    NewTask.Start();

end;


end.

