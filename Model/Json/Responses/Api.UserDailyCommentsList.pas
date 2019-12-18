unit Api.UserDailyCommentsList;

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
    Rest.Json,
    Api.ErrorHandler;


type


    TUserDailyCommentsList = class
    {$TYPEINFO ON}
    strict private
        var FCommentId:            TArray<integer>;
        var FCompanyCode:          TArray<string>;
        var FCustomerNumber:       TArray<string>;
        var FAgeDate:              TArray<string>;
        var FCallEvent:            TArray<boolean>;
        var FCallDuration:         TArray<integer>;
        var FFixedStatementsSent:  TArray<boolean>;
        var FCustomStatementsSent: TArray<boolean>;
        var FFixedRemindersSent:   TArray<boolean>;
        var FCustomRemindersSent:  TArray<boolean>;
        var FUserComment:          TArray<string>;
        var FUserAlias:            TArray<string>;
        var FEntryDateTime:        TArray<string>;
        var FIsSucceeded:          boolean;
        var FError:                TErrorHandler;
    public
        destructor Destroy(); override;
        property CommentId:            TArray<integer> read FCommentId            write FCommentId;
        property CompanyCode:          TArray<string>  read FCompanyCode          write FCompanyCode;
        property CustomerNumber:       TArray<string>  read FCustomerNumber       write FCustomerNumber;
        property AgeDate:              TArray<string>  read FAgeDate              write FAgeDate;
        property CallEvent:            TArray<boolean> read FCallEvent            write FCallEvent;
        property CallDuration:         TArray<integer> read FCallDuration         write FCallDuration;
        property FixedStatementsSent:  TArray<boolean> read FFixedStatementsSent  write FFixedStatementsSent;
        property CustomStatementsSent: TArray<boolean> read FCustomStatementsSent write FCustomStatementsSent;
        property FixedRemindersSent:   TArray<boolean> read FFixedRemindersSent   write FFixedRemindersSent;
        property CustomRemindersSent:  TArray<boolean> read FCustomRemindersSent  write FCustomRemindersSent;
        property UserComment:          TArray<string>  read FUserComment          write FUserComment;
        property UserAlias:            TArray<string>  read FUserAlias            write FUserAlias;
        property EntryDateTime:        TArray<string>  read FEntryDateTime        write FEntryDateTime;
        property IsSucceeded:          boolean         read FIsSucceeded          write FIsSucceeded;
        property Error:                TErrorHandler   read FError                write FError;
    end;


implementation


destructor TUserDailyCommentsList.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.

