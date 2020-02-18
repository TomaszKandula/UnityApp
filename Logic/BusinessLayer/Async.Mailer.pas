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
    /// Concrete implementation. Never call it directly, you can inherit from and extend upon.
    /// </remarks>
    TMailer = class(TInterfacedObject, IMailer)
    public
        constructor Create();
        destructor Destroy(); override;
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback); virtual;
    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
    Unity.Constants,
    Unity.Helpers,
    Unity.Service,
    Api.SendEmail,
    Api.SentEmail;


constructor TMailer.Create();
begin
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

            var Restful: IRESTful:=TRESTful.Create(Service.AccessToken);

            Restful.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'mailer/feedback/';
            Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
            Service.Logger.Log('[InitiateAwaited]: Executing POST ' + Restful.ClientBaseURL);

            var SendFrom:=Service.Settings.GetStringValue(TConfigSections.UserFeedback, 'SendFrom', '');
            var SendTo  :=Service.Settings.GetStringValue(TConfigSections.UserFeedback, 'SendTo', '');

            var ListTo:=TArray<string>.Create(SendTo);
            var ListCc:=TArray<string>.Create(Service.SessionData.EmailAddress);

            var SendEmail:=TSendEmail.Create();
            try

                SendEmail.UserId   :=Service.SessionData.UnityUserId.ToString();
                SendEmail.SessionId:=Service.SessionId;
                SendEmail.AliasName:=Service.SessionData.AliasName;
                SendEmail.From     :=SendFrom;
                SendEmail.&To      :=ListTo;
                SendEmail.Cc       :=ListCc;
                SendEmail.Subject  :='User feedback (Unity Platform, ver. ' + THelpers.GetBuildInfoAsString + ')';
                SendEmail.HtmlBody :=Text.Replace(#13#10,'<br>');

                Restful.CustomBody:=TJson.ObjectToJsonString(SendEmail);

            finally

            end;

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var SentEmail: TSentEmail:=TJson.JsonToObject<TSentEmail>(Restful.Content);
                try
                    CallResponse.IsSucceeded:=SentEmail.IsSucceeded;
                    CallResponse.LastMessage:=SentEmail.Error.ErrorDesc;
                    CallResponse.ErrorCode  :=SentEmail.Error.ErrorCode;
                    Service.Logger.Log('[SendFeedbackAsync]: Returned status code is ' + Restful.StatusCode.ToString());
                finally
                    SentEmail.Free();
                end;

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[SendFeedbackAsync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[SendFeedbackAsync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SendFeedbackAsync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

                CallResponse.ReturnedCode:=Restful.StatusCode;
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

