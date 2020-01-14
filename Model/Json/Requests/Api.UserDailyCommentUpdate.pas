unit Api.UserDailyCommentUpdate;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type


    TUserDailyCommentUpdate = class
    {$TYPEINFO ON}
    strict private
        var FCommentId:            integer;
        var FCallEvent:            integer;
        var FCallDuration:         integer;
        var FFixedStatementsSent:  integer;
        var FCustomStatementsSent: integer;
        var FFixedRemindersSent:   integer;
        var FCustomRemindersSent:  integer;
        var FUserComment:          string;
    public
        const _CommentId            = 'CommentId';
        const _CallEvent            = 'CallEvent';
        const _CallDuration         = 'CallDuration';
        const _FixedStatementsSent  = 'FixedStatementsSent';
        const _CustomStatementsSent = 'CustomStatementsSent';
        const _FixedRemindersSent   = 'FixedRemindersSent';
        const _CustomRemindersSent  = 'CustomRemindersSent';
        const _UserComment          = 'UserComment';
    published
        property CommentId:            integer read FCommentId            write FCommentId;
        property CallEvent:            integer read FCallEvent            write FCallEvent;
        property CallDuration:         integer read FCallDuration         write FCallDuration;
        property FixedStatementsSent:  integer read FFixedStatementsSent  write FFixedStatementsSent;
        property CustomStatementsSent: integer read FCustomStatementsSent write FCustomStatementsSent;
        property FixedRemindersSent:   integer read FFixedRemindersSent   write FFixedRemindersSent;
        property CustomRemindersSent:  integer read FCustomRemindersSent  write FCustomRemindersSent;
        property UserComment:          string  read FUserComment          write FUserComment;
    end;


implementation


end.

