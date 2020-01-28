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
    /// Exposes synchronous method with list of properties that allows to setup and send an email via REST API.
    /// It is recommended to use this method in asynchronous call, so the main thread is not blocked.
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
    /// Exposes method with list of properties that allows to configure fields and send email via REST API.
    /// Do not use direct implementation.
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
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService,
    Api.SendEmail,
    Api.SentEmail;


function TMailer.SendNowSync(): TCallResponse;
begin

    var CallResponse: TCallResponse;
    try

        var Restful: IRESTful:=TRESTful.Create(SessionService.AccessToken);
        var Settings: ISettings:=TSettings.Create();

        Restful.ClientBaseURL:=Settings.GetStringValue('APPLICATION', 'BASE_API_URI') + 'mailer/send/';
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
            Restful.CustomBody :=TJson.ObjectToJsonString(SendEmail);
        finally
            SendEmail.Free();
        end;

        if (Restful.Execute) and (Restful.StatusCode = 200) then
        begin

            var SentEmail: TSentEmail:=TJson.JsonToObject<TSentEmail>(Restful.Content);
            try
                CallResponse.IsSucceeded:=SentEmail.IsSucceeded;
                CallResponse.LastMessage:=SentEmail.Error.ErrorDesc;
                CallResponse.ErrorNumber:=SentEmail.Error.ErrorNum;
                ThreadFileLog.Log('[SendNowSync]: Returned status code is ' + Restful.StatusCode.ToString());
            finally
                SentEmail.Free();
            end;

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

