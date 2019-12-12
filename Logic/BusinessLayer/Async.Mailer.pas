unit Async.Mailer;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    Unity.Records;


type


    /// <summary>
    /// Callback signature for getting results from sending user email with feedback message.
    /// </summary>
    TSendUserFeedback = procedure(CallResponse: TCallResponse) of object;


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


    TMailer = class(TInterfacedObject, IMailer)
    {$TYPEINFO ON}
    public

        /// <summary>
        /// Async. sending email to CI Team with user feedback.
        /// Notification is always executed in main thread as long as callback is provided.
        /// </summary>
        /// <remarks>
        /// Provide nil for callback parameter if you want to execute async. method without returning any results to main thread.
        /// </remarks>
        procedure SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);

    end;


implementation


uses
    System.SysUtils,
    System.Threading,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Api.SendEmail,
    Api.SentEmail;


procedure TMailer.SendFeedbackAsync(Text: string; Callback: TSendUserFeedback);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

            var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
            Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'mailer/feedback/';
            Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
            ThreadFileLog.Log('[InitiateAwaited]: Executing POST ' + Restful.ClientBaseURL);

            var SendEmail:=TSendEmail.Create();
            try

                var Settings: ISettings:=TSettings.Create();
                var SendFrom:=Settings.GetStringValue(TConfigSections.UserFeedback, 'SendFrom', '');
                var SendTo  :=Settings.GetStringValue(TConfigSections.UserFeedback, 'SendTo', '');

                var ListTo:=TArray<string>.Create(SendTo);
                var ListCc:=TArray<string>.Create(SessionService.SessionData.EmailAddress);

                SendEmail.UserId   :=SessionService.SessionData.UnityUserId.ToString();
                SendEmail.SessionId:=SessionService.SessionId;
                SendEmail.AliasName:=SessionService.SessionData.AliasName;
                SendEmail.From     :=SendFrom;
                SendEmail.&To      :=ListTo;
                SendEmail.Cc       :=ListCc;
                SendEmail.Subject  :='User feedback (Unity Platform, ver. ' + THelpers.GetBuildInfoAsString + ')';
                SendEmail.HtmlBody :=Text.Replace(#13#10,'<br>');

                Restful.CustomBody:=TJson.ObjectToJsonString(SendEmail);

                if (Restful.Execute) and (Restful.StatusCode = 200) then
                begin

                    var SentEmail: TSentEmail:=TJson.JsonToObject<TSentEmail>(Restful.Content);

                    CallResponse.IsSucceeded:=SentEmail.IsSucceeded;
                    CallResponse.LastMessage:=SentEmail.Error.ErrorDesc;
                    CallResponse.ErrorNumber:=SentEmail.Error.ErrorNum;

                    SentEmail.Free();
                    ThreadFileLog.Log('[SendFeedbackAsync]: Returned status code is ' + Restful.StatusCode.ToString());

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
                    ThreadFileLog.Log(CallResponse.LastMessage);

                end;

            finally
                SendEmail.Free();
            end;

        except
            on E: Exception do
            begin
                CallResponse.IsSucceeded:=False;
                CallResponse.LastMessage:='[SendFeedbackAsync]: Cannot execute. Error has been thrown: ' + E.Message;
                ThreadFileLog.Log(CallResponse.LastMessage);
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(CallResponse);
        end);

    end);

    NewTask.Start;

end;


end.

