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


    // --------------------
    // Callback signatures.
    // --------------------

    //...

    IMailer = Interface(IInterface)
    ['{3D803B98-BE4F-49A4-A2B5-7F323772E5B4}']

        // ----------------------------
        // Undisclosed getters/setters.
        // ----------------------------

        procedure SetXMailer(NewValue:     string);
        procedure SetMailFrom(NewValue:    string);
        procedure SetMailTo(NewValue:      string);
        procedure SetMailCc(NewValue:      string);
        procedure SetMailBcc(NewValue:     string);
        procedure SetMailRt(NewValue:      string);
        procedure SetMailSubject(NewValue: string);
        procedure SetMailBody(NewValue:    string);
        procedure SetAttachments(NewValue: TList<string>);
        function GetXMailer:     string;
        function GetMailFrom:    string;
        function GetMailTo:      string;
        function GetMailCc:      string;
        function GetMailBcc:     string;
        function GetMailRt:      string;
        function GetMailSubject: string;
        function GetMailBody:    string;
        function GetAttachments: TList<string>;

        // ----------------------------
        // Exposed properties.
        // ----------------------------

        property XMailer:     string        read GetXMailer     write SetXMailer;
        property MailFrom:    string        read GetMailFrom    write SetMailFrom;
        property MailTo:      string        read GetMailTo      write SetMailTo;
        property MailCc:      string        read GetMailCc      write SetMailCc;
        property MailBcc:     string        read GetMailBcc     write SetMailBcc;
        property MailRt:      string        read GetMailRt      write SetMailRt;
        property MailSubject: string        read GetMailSubject write SetMailSubject;
        property MailBody:    string        read GetMailBody    write SetMailBody;
        property Attachments: TList<string> read GetAttachments write SetAttachments;

        // ----------------------------
        // Exposed methods.
        // ----------------------------

        function SendNow: boolean;

    end;


    TMailer = class(TInterfacedObject, IMailer)
    {$TYPEINFO ON}
    private
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
        function GetXMailer:     string;
        function GetMailFrom:    string;
        function GetMailTo:      string;
        function GetMailCc:      string;
        function GetMailBcc:     string;
        function GetMailRt:      string;
        function GetMailSubject: string;
        function GetMailBody:    string;
        function GetAttachments: TList<string>;
        function SendEmail(OAuth: TAuthTypes): boolean;
    public
        property XMailer:     string        read GetXMailer     write SetXMailer;
        property MailFrom:    string        read GetMailFrom    write SetMailFrom;
        property MailTo:      string        read GetMailTo      write SetMailTo;
        property MailCc:      string        read GetMailCc      write SetMailCc;
        property MailBcc:     string        read GetMailBcc     write SetMailBcc;
        property MailRt:      string        read GetMailRt      write SetMailRt;
        property MailSubject: string        read GetMailSubject write SetMailSubject;
        property MailBody:    string        read GetMailBody    write SetMailBody;
        property Attachments: TList<string> read GetAttachments write SetAttachments;
        function SendNow: boolean;
        constructor Create;
        destructor Destroy; override;
    end;


implementation


uses
    View.Main,
    View.InvoiceTracker,
    View.Actions,
    Data.Win.ADODB,
    DbModel,
    Unity.Settings,
    Unity.EventLogger;


// -------------------------------
// Initialize.
// -------------------------------

constructor TMailer.Create;
begin
     FAttachments:=TList<string>.Create;
end;


// -------------------------------
// Release object.
// -------------------------------

destructor TMailer.Destroy;
begin
    if Assigned(FAttachments) then FAttachments.Free;
end;


// -------------------------------
// Send an email using CDOSYS.
// -------------------------------

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


// ---------------------------------
// Simple wrapper for email sending.
// ---------------------------------

function TMailer.SendNow: boolean;
begin

    Result:=False;
    var Settings: ISettings:=TSettings.Create;

    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM then Result:=SendEmail(TAuthTypes.cdoNTLM);
    if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then Result:=SendEmail(TAuthTypes.cdoBasic);

end;


// -------------------------------
// Property's getters.
// -------------------------------


function TMailer.GetXMailer: string;
begin
    Result:=FXMailer;
end;


function TMailer.GetMailFrom: string;
begin
    Result:=FMailFrom;
end;


function TMailer.GetMailTo: string;
begin
    Result:=FMailTo;
end;


function TMailer.GetMailCc: string;
begin
    Result:=FMailCc;
end;


function TMailer.GetMailBcc: string;
begin
    Result:=FMailBcc;
end;


function TMailer.GetMailRt: string;
begin
    Result:=FMailRt;
end;


function TMailer.GetMailSubject: string;
begin
    Result:=FMailSubject;
end;


function TMailer.GetMailBody: string;
begin
    Result:=FMailBody;
end;


function TMailer.GetAttachments: TList<string>;
begin
    Result:=FAttachments;
end;


// -------------------------------
// Property's setters.
// -------------------------------


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

