unit Sync.Mailer;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
    Unity.Records;


type


    /// <summary>
    /// This interface exposes methods and properties allowing to send an email via REST API.
    /// It is recommended to use this method in asynchronous call, so the main thread is not
    /// blocked.
    /// </summary>
    IMailer = Interface(IInterface)
    ['{3D803B98-BE4F-49A4-A2B5-7F323772E5B4}']

        /// <summary>
        /// Setting new email "from" field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailFrom(NewValue: string);

        /// <summary>
        /// Setting new email "to" field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailTo(NewValue: TArray<string>);

        /// <summary>
        /// Setting new email carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailCc(NewValue: TArray<string>);

        /// <summary>
        /// Setting new email blind carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailBcc(NewValue: TArray<string>);

        /// <summary>
        /// Setting new email subject field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailSubject(NewValue: string);

        /// <summary>
        /// Setting new email body field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailBody(NewValue: string);

        /// <summary>
        /// Returns email "from" field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailFrom(): string;

        /// <summary>
        /// Returns email "to" field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailTo(): TArray<string>;

        /// <summary>
        /// Returns email carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailCc(): TArray<string>;

        /// <summary>
        /// Returns email blind carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailBcc(): TArray<string>;

        /// <summary>
        /// returns email subject.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailSubject(): string;

        /// <summary>
        /// Returns email body.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailBody(): string;

        /// <summary>
        /// Addresser (sender) field.
        /// </summary>
        property MailFrom: string read GetMailFrom write SetMailFrom;

        /// <summary>
        /// Addressee field.
        /// </summary>
        property MailTo: TArray<string> read GetMailTo write SetMailTo;

        /// <summary>
        /// Carbon copy field.
        /// </summary>
        property MailCc: TArray<string> read GetMailCc write SetMailCc;

        /// <summary>
        /// Blind carbon copy field.
        /// </summary>
        property MailBcc: TArray<string> read GetMailBcc write SetMailBcc;

        /// <summary>
        /// Subject of the email.
        /// </summary>
        property MailSubject: string read GetMailSubject write SetMailSubject;

        /// <summary>
        /// Body content of the email.
        /// </summary>
        property MailBody: string read GetMailBody write SetMailBody;

        /// <summary>
        /// Send email if the settings fields are configured properly.
        /// </summary>
        function SendNowSync(): TCallResponse;

    end;


    /// <summary>
    /// This class exposes methods and properties allowing to configure fields and send email via CDOSYS using basic auth or NTLM.
    /// </summary>
    TMailer = class(TInterfacedObject, IMailer)
    {$TYPEINFO ON}
    strict private
        var FMailFrom: string;
        var FMailTo: TArray<string>;
        var FMailCc: TArray<string>;
        var FMailBcc: TArray<string>;
        var FMailSubject: string;
        var FMailBody: string;
        procedure SetMailFrom(NewValue: string);
        procedure SetMailTo(NewValue: TArray<string>);
        procedure SetMailCc(NewValue: TArray<string>);
        procedure SetMailBcc(NewValue: TArray<string>);
        procedure SetMailSubject(NewValue: string);
        procedure SetMailBody(NewValue: string);
        function GetMailFrom(): string;
        function GetMailTo(): TArray<string>;
        function GetMailCc(): TArray<string>;
        function GetMailBcc(): TArray<string>;
        function GetMailSubject(): string;
        function GetMailBody(): string;
    public

        /// <summary>
        /// Addresser (sender) field.
        /// </summary>
        property MailFrom: string read GetMailFrom write SetMailFrom;

        /// <summary>
        /// Addressee field.
        /// </summary>
        property MailTo: TArray<string> read GetMailTo write SetMailTo;

        /// <summary>
        /// Carbon copy field.
        /// </summary>
        property MailCc: TArray<string> read GetMailCc write SetMailCc;

        /// <summary>
        /// Blind carbon copy field.
        /// </summary>
        property MailBcc: TArray<string> read GetMailBcc write SetMailBcc;

        /// <summary>
        /// Subject of the email.
        /// </summary>
        property MailSubject: string read GetMailSubject write SetMailSubject;

        /// <summary>
        /// Body content of the email.
        /// </summary>
        property MailBody: string read GetMailBody write SetMailBody;

        /// <summary>
        /// Send email used configured fields.
        /// </summary>
        function SendNowSync(): TCallResponse;

    end;


implementation


uses
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Api.SendEmail,
    Api.SentEmail;


function TMailer.SendNowSync(): TCallResponse;
begin

    var CallResponse: TCallResponse;
    try

        var Restful: IRESTful:=TRESTful.Create(TRestAuth.apiUserName, TRestAuth.apiPassword);
        Restful.ClientBaseURL:=TRestAuth.restApiBaseUrl + 'mailer/send/';
        Restful.RequestMethod:=TRESTRequestMethod.rmPOST;
        ThreadFileLog.Log('[SendNowSync]: Executing POST ' + Restful.ClientBaseURL);

        var SendEmail:=TSendEmail.Create();
        try

            SendEmail.UserId   :=SessionService.SessionData.UnityUserId.ToString();
            SendEmail.SessionId:=SessionService.SessionId;
            SendEmail.AliasName:=SessionService.SessionData.AliasName;
            SendEmail.From     :=FMailFrom;
            SendEmail.&To      :=FMailTo;
            SendEmail.Cc       :=FMailCc;
            SendEmail.Bcc      :=FMailBcc;
            SendEmail.Subject  :=MailSubject;
            SendEmail.HtmlBody :=MailBody;

            Restful.CustomBody:=TJson.ObjectToJsonString(SendEmail);

            if (Restful.Execute) and (Restful.StatusCode = 200) then
            begin

                var SentEmail: TSentEmail:=TJson.JsonToObject<TSentEmail>(Restful.Content);

                CallResponse.IsSucceeded:=SentEmail.IsSucceeded;
                CallResponse.LastMessage:=SentEmail.Error.ErrorDesc;
                CallResponse.ErrorNumber:=SentEmail.Error.ErrorNum;

                SentEmail.Free();
                ThreadFileLog.Log('[SendNowSync]: Returned status code is ' + Restful.StatusCode.ToString());

            end
            else
            begin

                if not String.IsNullOrEmpty(Restful.ExecuteError) then
                    CallResponse.LastMessage:='[SendNowSync]: Critical error. Please contact IT Support. Description: ' + Restful.ExecuteError
                else
                    if String.IsNullOrEmpty(Restful.Content) then
                        CallResponse.LastMessage:='[SendNowSync]: Invalid server response. Please contact IT Support.'
                    else
                        CallResponse.LastMessage:='[SendNowSync]: An error has occured. Please contact IT Support. Description: ' + Restful.Content;

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
            CallResponse.LastMessage:='[SendNowSync]: Cannot execute. Error has been thrown: ' + E.Message;
            ThreadFileLog.Log(CallResponse.LastMessage);
        end;

    end;

    Result:=CallResponse;

end;


function TMailer.GetMailFrom(): string;
begin
    Result:=FMailFrom;
end;


function TMailer.GetMailTo(): TArray<string>;
begin
    Result:=FMailTo;
end;


function TMailer.GetMailCc(): TArray<string>;
begin
    Result:=FMailCc;
end;


function TMailer.GetMailBcc(): TArray<string>;
begin
    Result:=FMailBcc;
end;


function TMailer.GetMailSubject(): string;
begin
    Result:=FMailSubject;
end;


function TMailer.GetMailBody(): string;
begin
    Result:=FMailBody;
end;


procedure TMailer.SetMailFrom(NewValue: string);
begin
    FMailFrom:=NewValue;
end;


procedure TMailer.SetMailTo(NewValue: TArray<string>);
begin
    FMailTo:=NewValue;
end;


procedure TMailer.SetMailCc(NewValue: TArray<string>);
begin
    FMailCc:=NewValue;
end;


procedure TMailer.SetMailBcc(NewValue: TArray<string>);
begin
    FMailBcc:=NewValue;
end;


procedure TMailer.SetMailSubject(NewValue: string);
begin
    FMailSubject:=NewValue;
end;


procedure TMailer.SetMailBody(NewValue: string);
begin
    FMailBody:=NewValue;
end;


end.

