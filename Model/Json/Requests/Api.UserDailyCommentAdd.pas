unit Api.UserDailyCommentAdd;

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


    TUserDailyCommentAdd = class
    {$TYPEINFO ON}
    strict private
        var FAgeDate:              string;
        var FCallEvent:            boolean;
        var FCallDuration:         integer;
        var FFixedStatementsSent:  boolean;
        var FCustomStatementsSent: boolean;
        var FFixedRemindersSent:   boolean;
        var FCustomRemindersSent:  boolean;
        var FUserComment:          string;
    public
        property AgeDate:              string  read FAgeDate              write FAgeDate;
        property CallEvent:            boolean read FCallEvent            write FCallEvent;
        property CallDuration:         integer read FCallDuration         write FCallDuration;
        property FixedStatementsSent:  boolean read FFixedStatementsSent  write FFixedStatementsSent;
        property CustomStatementsSent: boolean read FCustomStatementsSent write FCustomStatementsSent;
        property FixedRemindersSent:   boolean read FFixedRemindersSent   write FFixedRemindersSent;
        property CustomRemindersSent:  boolean read FCustomRemindersSent  write FCustomRemindersSent;
        property UserComment:          string  read FUserComment          write FUserComment;
    end;


implementation


end.

