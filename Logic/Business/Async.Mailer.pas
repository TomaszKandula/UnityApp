unit Async.Mailer;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    Unity.Types,
    Unity.Records;


type


    IMailer = interface(IInterface)
    ['{9C75EDA4-5B69-40C2-8796-9F87202720B6}']
        /// <summary>
        /// Async. sending email to CI Team with user feedback.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and and extend them.
    /// </remarks>
    TMailer = class(TInterfacedObject, IMailer)
    public
        constructor Create();
        destructor  Destroy(); override;
        procedure   SendFeedbackAsync(Text: string; Callback: TSendUserFeedback); virtual;
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
    Api.SendEmail,
    Api.SentEmail;


constructor TMailer.Create();
begin
    inherited;
end;


destructor TMailer.Destroy();
begin
    inherited;
end;


procedure TMailer.SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var Rest:=Service.InvokeRest();
			Rest.AccessToken:=Service.AccessToken;
            Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

            Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'mailer/feedback/';
            Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
            Service.Logger.Log('[InitiateAwaited]: Executing POST ' + Rest.ClientBaseURL);

            var SendFrom:=Service.Settings.GetStringValue(TConfigSections.UserFeedback, 'SendFrom', '');
            var SendTo  :=Service.Settings.GetStringValue(TConfigSections.UserFeedback, 'SendTo', '');

            var ListTo:=TArray<string>.Create(SendTo);
            var ListCc:=TArray<string>.Create(Service.SessionData.EmailAddress);

            var SendEmail:=TSendEmail.Create();
            try

                SendEmail.UserId   :=Service.SessionData.UnityUserId;
                SendEmail.SessionId:=Service.SessionId;
                SendEmail.AliasName:=Service.SessionData.AliasName;
                SendEmail.From     :=SendFrom;
                SendEmail.&To      :=ListTo;
                SendEmail.Cc       :=ListCc;
                SendEmail.Subject  :='User feedback (Unity Platform, ver. ' + THelpers.GetBuildInfoAsString + ')';
                SendEmail.HtmlBody :=Text.Replace(#13#10,'<br>');

                Rest.CustomBody:=TJson.ObjectToJsonString(SendEmail);

            finally

            end;

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin

                var SentEmail: TSentEmail:=TJson.JsonToObject<TSentEmail>(Rest.Content);
                try
                    CallResponse.IsSucceeded:=SentEmail.IsSucceeded;
                    CallResponse.LastMessage:=SentEmail.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=SentEmail.Error.ErrorCode;
                    Service.Logger.Log('[SendFeedbackAsync]: Returned status code is ' + Rest.StatusCode.ToString());
                finally
                    SentEmail.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    CallResponse.LastMessage:='[SendFeedbackAsync]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        CallResponse.LastMessage:='[SendFeedbackAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SendFeedbackAsync]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                CallResponse.ReturnedCode:=Rest.StatusCode;
                CallResponse.IsSucceeded:=False;
                Service.Logger.Log(CallResponse.LastMessage);

            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendFeedbackAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                Service.Logger.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start();

end;


end.

