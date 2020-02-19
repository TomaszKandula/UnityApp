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
    /// Exposes synchronous methods and properties that allows to configure fields and send email via REST API.
    /// It is recommended to use this method in asynchronous call, so the main thread is not blocked during sending.
    /// </summary>
    IMailer = Interface(IInterface)
    ['{3D803B98-BE4F-49A4-A2B5-7F323772E5B4}']
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
        property MailFrom: string read GetMailFrom write SetMailFrom;
        property MailTo: TArray<string> read GetMailTo write SetMailTo;
        property MailCc: TArray<string> read GetMailCc write SetMailCc;
        property MailBcc: TArray<string> read GetMailBcc write SetMailBcc;
        property MailSubject: string read GetMailSubject write SetMailSubject;
        property MailBody: string read GetMailBody write SetMailBody;
        function SendNowSync(): TCallResponse;
    end;


    /// <summary>
    /// Implements method and properties that allows to configure fields and send email via REST API.
    /// Do not use direct implementation.
    /// </summary>
    TMailer = class(TInterfacedObject, IMailer)
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
        constructor Create();
        destructor Destroy(); override;
        property MailFrom: string read GetMailFrom write SetMailFrom;
        property MailTo: TArray<string> read GetMailTo write SetMailTo;
        property MailCc: TArray<string> read GetMailCc write SetMailCc;
        property MailBcc: TArray<string> read GetMailBcc write SetMailBcc;
        property MailSubject: string read GetMailSubject write SetMailSubject;
        property MailBody: string read GetMailBody write SetMailBody;
        function SendNowSync(): TCallResponse;
    end;


implementation


uses
    System.SysUtils,
    REST.Types,
    REST.Json,
    Unity.RestWrapper,
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


function TMailer.SendNowSync(): TCallResponse;
begin

    var CallResponse: TCallResponse;
    try

        Service.Rest.AccessToken:=Service.AccessToken;
        Service.Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Service.Rest.ClientBaseURL:=Service.Settings.GetStringValue('API_ENDPOINTS', 'BASE_API_URI') + 'mailer/send/';
        Service.Rest.RequestMethod:=TRESTRequestMethod.rmPOST;
        Service.Logger.Log('[SendNowSync]: Executing POST ' + Service.Rest.ClientBaseURL);

        var SendEmail:=TSendEmail.Create();
        try
            SendEmail.UserId   :=Service.SessionData.UnityUserId.ToString();
            SendEmail.SessionId:=Service.SessionId;
            SendEmail.AliasName:=Service.SessionData.AliasName;
            SendEmail.From     :=FMailFrom;
            SendEmail.&To      :=FMailTo;
            SendEmail.Cc       :=FMailCc;
            SendEmail.Bcc      :=FMailBcc;
            SendEmail.Subject  :=MailSubject;
            SendEmail.HtmlBody :=MailBody;
            Service.Rest.CustomBody :=TJson.ObjectToJsonString(SendEmail);
        finally
            SendEmail.Free();
        end;

        if (Service.Rest.Execute) and (Service.Rest.StatusCode = 200) then
        begin

            var SentEmail: TSentEmail:=TJson.JsonToObject<TSentEmail>(Service.Rest.Content);
            try
                CallResponse.IsSucceeded:=SentEmail.IsSucceeded;
                CallResponse.LastMessage:=SentEmail.Error.ErrorDesc;
                CallResponse.ErrorCode  :=SentEmail.Error.ErrorCode;
                Service.Logger.Log('[SendNowSync]: Returned status code is ' + Service.Rest.StatusCode.ToString());
            finally
                SentEmail.Free();
            end;

        end
        else
        begin

            if not String.IsNullOrEmpty(Service.Rest.ExecuteError) then
                CallResponse.LastMessage:='[SendNowSync]: Critical error. Please contact IT Support. Description: ' + Service.Rest.ExecuteError
            else
                if String.IsNullOrEmpty(Service.Rest.Content) then
                    CallResponse.LastMessage:='[SendNowSync]: Invalid server response. Please contact IT Support.'
                else
                    CallResponse.LastMessage:='[SendNowSync]: An error has occured. Please contact IT Support. Description: ' + Service.Rest.Content;

            CallResponse.ReturnedCode:=Service.Rest.StatusCode;
            CallResponse.IsSucceeded:=False;
            Service.Logger.Log(CallResponse.LastMessage);

        end;

    except
        on E: Exception do
        begin
            CallResponse.IsSucceeded:=False;
            CallResponse.LastMessage:='[SendNowSync]: Cannot execute. Error has been thrown: ' + E.Message;
            Service.Logger.Log(CallResponse.LastMessage);
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

