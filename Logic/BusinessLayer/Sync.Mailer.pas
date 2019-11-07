unit Sync.Mailer;

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
    System.StrUtils,
    System.Variants,
    System.Generics.Collections,
    Vcl.StdCtrls,
    Vcl.Grids,
    CDO_TLB,
    Unity.Enums,
    Unity.Arrays,
    Unity.Grid;


type


    /// <summary>
    /// This interface exposes methods and properties allowing to configure fields and send email via CDOSYS using basic auth or NTLM.
    /// </summary>
    IMailer = Interface(IInterface)
    ['{3D803B98-BE4F-49A4-A2B5-7F323772E5B4}']

        /// <summary>
        /// Deprecated field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetXMailer(NewValue: string);

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
        procedure SetMailTo(NewValue: string);

        /// <summary>
        /// Setting new email carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailCc(NewValue: string);

        /// <summary>
        /// Setting new email blind carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailBcc(NewValue: string);

        /// <summary>
        /// Setting new email "reply-to" field.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetMailRt(NewValue: string);

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
        /// Setting new list of file paths of the files to be attched to the email.
        /// </summary>
        /// <remarks>
        /// Undisclosed setter under interface.
        /// </remarks>
        procedure SetAttachments(NewValue: TList<string>);

        /// <summary>
        /// Deprecated field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetXMailer(): string;

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
        function GetMailTo(): string;

        /// <summary>
        /// Returns email carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailCc(): string;

        /// <summary>
        /// Returns email blind carbon copy field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailBcc(): string;

        /// <summary>
        /// Returns email reply-to field.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetMailRt(): string;

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
        /// Returns list of the attachements.
        /// </summary>
        /// <remarks>
        /// Undisclosed getter under interface.
        /// </remarks>
        function GetAttachments(): TList<string>;

        /// <summary>
        /// Deprecated field.
        /// </summary>
        property XMailer: string read GetXMailer write SetXMailer;

        /// <summary>
        /// Addresser (sender) field.
        /// </summary>
        property MailFrom: string read GetMailFrom write SetMailFrom;

        /// <summary>
        /// Addressee field.
        /// </summary>
        property MailTo: string read GetMailTo write SetMailTo;

        /// <summary>
        /// Carbon copy field.
        /// </summary>
        property MailCc: string read GetMailCc write SetMailCc;

        /// <summary>
        /// Blind carbon copy field.
        /// </summary>
        property MailBcc: string read GetMailBcc write SetMailBcc;

        /// <summary>
        /// Reply-To field.
        /// </summary>
        property MailRt: string read GetMailRt write SetMailRt;

        /// <summary>
        /// Subject of the email.
        /// </summary>
        property MailSubject: string read GetMailSubject write SetMailSubject;

        /// <summary>
        /// Body content of the email.
        /// </summary>
        property MailBody: string read GetMailBody write SetMailBody;

        /// <summary>
        /// List of paths to files to be attached to the email.
        /// </summary>
        property Attachments: TList<string> read GetAttachments write SetAttachments;

        /// <summary>
        /// Send email if the settings fields are configured properly.
        /// </summary>
        function SendNow(): boolean;

    end;


    /// <summary>
    /// This class exposes methods and properties allowing to configure fields and send email via CDOSYS using basic auth or NTLM.
    /// </summary>
    TMailer = class(TInterfacedObject, IMailer)
    {$TYPEINFO ON}
    strict private
        var FXMailer:     string;
        var FMailFrom:    string;
        var FMailTo:      string;
        var FMailCc:      string;
        var FMailBcc:     string;
        var FMailRt:      string;
        var FMailSubject: string;
        var FMailBody:    string;
        var FAttachments: TList<string>;
        procedure SetXMailer(NewValue:     string);
        procedure SetMailFrom(NewValue:    string);
        procedure SetMailTo(NewValue:      string);
        procedure SetMailCc(NewValue:      string);
        procedure SetMailBcc(NewValue:     string);
        procedure SetMailRt(NewValue:      string);
        procedure SetMailSubject(NewValue: string);
        procedure SetMailBody(NewValue:    string);
        procedure SetAttachments(NewValue: TList<string>);
        function GetXMailer():     string;
        function GetMailFrom():    string;
        function GetMailTo():      string;
        function GetMailCc():      string;
        function GetMailBcc():     string;
        function GetMailRt():      string;
        function GetMailSubject(): string;
        function GetMailBody():    string;
        function GetAttachments(): TList<string>;
        function SendEmail(OAuth: TAuthTypes): boolean;
    public

        /// <summary>
        /// Deprecated field.
        /// </summary>
        property XMailer: string read GetXMailer write SetXMailer;

        /// <summary>
        /// Addresser (sender) field.
        /// </summary>
        property MailFrom: string read GetMailFrom write SetMailFrom;

        /// <summary>
        /// Addressee field.
        /// </summary>
        property MailTo: string read GetMailTo write SetMailTo;

        /// <summary>
        /// Carbon copy field.
        /// </summary>
        property MailCc: string read GetMailCc write SetMailCc;

        /// <summary>
        /// Blind carbon copy field.
        /// </summary>
        property MailBcc: string read GetMailBcc write SetMailBcc;

        /// <summary>
        /// Reply-To field.
        /// </summary>
        property MailRt: string read GetMailRt write SetMailRt;

        /// <summary>
        /// Subject of the email.
        /// </summary>
        property MailSubject: string read GetMailSubject write SetMailSubject;

        /// <summary>
        /// Body content of the email.
        /// </summary>
        property MailBody: string read GetMailBody write SetMailBody;

        /// <summary>
        /// List of paths to files to be attached to the email.
        /// </summary>
        property Attachments: TList<string> read GetAttachments write SetAttachments;

        /// <summary>
        /// Send email used configured fields.
        /// </summary>
        function SendNow(): boolean;

        /// <summary>
        /// Create TList for FAttachements when class is instantiated.
        /// </summary>
        constructor Create();

        /// <summary>
        /// Release FAttachments.
        /// </summary>
        destructor Destroy(); override;

    end;


implementation


uses
    Data.Win.ADODB,
    DbModel,
    Unity.Settings,
    Unity.EventLogger;


constructor TMailer.Create();
begin
     FAttachments:=TList<string>.Create();
end;


destructor TMailer.Destroy();
begin
    if Assigned(FAttachments) then FAttachments.Free();
end;


function TMailer.SendEmail(OAuth: TAuthTypes): boolean;
begin

    Result:=False;

    var CdoMessage: CDO_TLB.IMessage:=CDO_TLB.CoMessage.Create;
    CdoMessage.From:=MailFrom;
    CdoMessage.To_ :=MailTo;
    CdoMessage.CC  :=MailCc;

    if MailBcc <> '' then CdoMessage.BCC:=MailBcc;
    if MailRt  <> '' then CdoMessage.ReplyTo:=MailRt;

    CdoMessage.Subject :=MailSubject;
    CdoMessage.HTMLBody:=MailBody;

    // Configure
    var Settings: ISettings:=TSettings.Create;
    var Schema: string:='http://schemas.microsoft.com/cdo/configuration/';

    if oauth = TAuthTypes.cdoNTLM then
    begin
        CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=TAuthUsing.cdoSendUsingPort;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=TAuthTypes.cdoNTLM;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'SMTP', '');
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'PORT', '');
    end;

    if oauth = TAuthTypes.cdoBasic then
    begin
        CdoMessage.Configuration.Fields.item[Schema + 'sendusing'       ].Value:=TAuthUsing.cdoSendUsingPort;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpauthenticate'].Value:=TAuthTypes.cdoBasic;
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserver'      ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'SMTP', '');
        CdoMessage.Configuration.Fields.item[Schema + 'smtpserverport'  ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'PORT', '');
        CdoMessage.Configuration.Fields.item[Schema + 'sendusername'    ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'USERNAME', '');
        CdoMessage.Configuration.Fields.item[Schema + 'sendpassword'    ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'PASSWORD', '');
        CdoMessage.Configuration.Fields.item[Schema + 'smtpusessl'      ].Value:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'SSL', '');
    end;

    CdoMessage.Configuration.Fields.item[Schema + 'NNTPAccountName' ].Value:=XMailer;
    CdoMessage.Configuration.Fields.update;

    try

        // Add attachments (if any)
        if Attachments.Count > 0 then
        begin
            for var iCNT: integer:=0 to Attachments.Count - 1 do
                CdoMessage.AddAttachment(Attachments.Items[iCNT],'','');
        end;

        CdoMessage.BodyPart.Charset:='utf-8';
        CdoMessage.Send;
        Result:=True;
        ThreadFileLog.Log('E-mail has been sent successfully.');

    except
        on E: Exception do
            ThreadFileLog.Log('Cannot send an e-mail. Error message has been thrown: ' + E.Message);
    end;

end;


function TMailer.SendNow(): boolean;
begin

    Result:=False;
    var Settings: ISettings:=TSettings.Create;

    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM then
        Result:=SendEmail(TAuthTypes.cdoNTLM);

    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then
        Result:=SendEmail(TAuthTypes.cdoBasic);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- GETTERS //


function TMailer.GetXMailer(): string;
begin
    Result:=FXMailer;
end;


function TMailer.GetMailFrom(): string;
begin
    Result:=FMailFrom;
end;


function TMailer.GetMailTo(): string;
begin
    Result:=FMailTo;
end;


function TMailer.GetMailCc(): string;
begin
    Result:=FMailCc;
end;


function TMailer.GetMailBcc(): string;
begin
    Result:=FMailBcc;
end;


function TMailer.GetMailRt(): string;
begin
    Result:=FMailRt;
end;


function TMailer.GetMailSubject(): string;
begin
    Result:=FMailSubject;
end;


function TMailer.GetMailBody(): string;
begin
    Result:=FMailBody;
end;


function TMailer.GetAttachments(): TList<string>;
begin
    Result:=FAttachments;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- SETTERS //


procedure TMailer.SetXMailer(NewValue: string);
begin
    FXMailer:=NewValue;
end;


procedure TMailer.SetMailFrom(NewValue: string);
begin
    FMailFrom:=NewValue;
end;


procedure TMailer.SetMailTo(NewValue: string);
begin
    FMailTo:=NewValue;
end;


procedure TMailer.SetMailCc(NewValue: string);
begin
    FMailCc:=NewValue;
end;


procedure TMailer.SetMailBcc(NewValue: string);
begin
    FMailBcc:=NewValue;
end;


procedure TMailer.SetMailRt(NewValue: string);
begin
    FMailRt:=NewValue;
end;


procedure TMailer.SetMailSubject(NewValue: string);
begin
    FMailSubject:=NewValue;
end;


procedure TMailer.SetMailBody(NewValue: string);
begin
    FMailBody:=NewValue;
end;


procedure TMailer.SetAttachments(NewValue: TList<string>);
begin
    FAttachments:=NewValue;
end;


end.

