unit Async.Mailer;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    System.SysUtils,
    System.Classes,
    System.Threading,
    System.Generics.Collections,
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
    Unity.Helpers,
    Unity.Settings,
    Unity.EventLogger,
    Unity.SessionService;


procedure TMailer.SendFeedbackAsync(Text: string; Callback: TSendUserFeedback); // replace with rest / EWS
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var CallResponse: TCallResponse;
        try

//            var Settings: ISettings:=TSettings.Create();
//            var Mail: IDocument:=TDocument.Create();
//
//            var AppName: string:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'VALUE', '');
//            var AppVer: string:=THelpers.GetBuildInfoAsString;
//
//            // --------------------------
//            // Get and set email details.
//            // --------------------------
//            if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerNTLM  then
//            begin
//                Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerNTLM, 'FROM',     '');
//                Mail.MailTo :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'TO',       '');
//                Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerNTLM, 'REPLY-TO', '');
//            end;
//
//            if Settings.GetStringValue(TConfigSections.MailerSetup, 'ACTIVE', '') = TConfigSections.MailerBASIC then
//            begin
//                Mail.XMailer:=Settings.GetStringValue(TConfigSections.MailerBASIC, 'FROM',     '');
//                Mail.MailTo :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'TO',       '');
//                Mail.MailRt :=Settings.GetStringValue(TConfigSections.MailerBASIC, 'REPLY-TO', '');
//            end;
//
//            Mail.MailFrom   :=Mail.XMailer;
//            Mail.MailCc     :=SessionService.SessionData.EmailAddress;
//            Mail.MailBcc    :='';
//            Mail.MailSubject:='Unity - User feedback (' + UpperCase(SessionService.SessionData.AliasName) + ')';
//
//            // ----------------------------------
//            // Plain text to HTML using template.
//            // ----------------------------------
//            var Transfer: string:=Text;
//            Transfer:=StringReplace(Transfer, TChars.CRLF, '<br>', [rfReplaceAll]);
//
//            var HTMLBody: string:=Mail.LoadTemplate(Settings.DirLayouts + Settings.GetStringValue(TConfigSections.Layouts, 'SINGLE4', ''), False);
//            HTMLBody:=StringReplace(HTMLBody, '{TEXT_HOLER}',  Transfer,       [rfReplaceAll]);
//            HTMLBody:=StringReplace(HTMLBody, '{APPNAME}',     AppName,        [rfReplaceAll]);
//            HTMLBody:=StringReplace(HTMLBody, '{BUILD}',       AppVer,         [rfReplaceAll]);
//            HTMLBody:=StringReplace(HTMLBody, '{REPORT_DATE}', DateToStr(Now), [rfReplaceAll]);
//            HTMLBody:=StringReplace(HTMLBody, '{REPORT_TIME}', TimeToStr(Now), [rfReplaceAll]);
//
//            Mail.MailBody:=HTMLBody;
//
//            if Mail.SendNow then
//            begin
//                CallResponse.IsSucceeded:=True;
//                CallResponse.LastMessage:='[SendFeedbackAsync]: User feedback has been sent.';
//                ThreadFileLog.Log(CallResponse.LastMessage);
//            end
//            else
//            begin
//                CallResponse.IsSucceeded:=False;
//                CallResponse.LastMessage:='[SendFeedbackAsync]: Cannot send email. Please contact IT Support.';
//                ThreadFileLog.Log(CallResponse.LastMessage);
//            end;

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

