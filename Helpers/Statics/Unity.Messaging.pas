unit Unity.Messaging;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TMessaging = class abstract

        {WParams}
        type TWParams = class abstract
            const AwaitForm        = 1;
            const StatusBar        = 2;
            const MessageInfo      = 3;
            const MessageWarn      = 4;
            const MessageError     = 5;
            const MessageQuestion1 = 6;
            const MessageQuestion2 = 7;
            const ConnectionOk     = 8;
            const ConnectionError  = 9;
            const MailerReportItem = 10;
        end;

        {LParams}
        type TAwaitForm = class abstract
            const Show = 1;
            const Hide = 2;
        end;

    end;


implementation


end.

