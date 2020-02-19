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
    Api.ErrorHandler,
    Api.MetaData;


type


    TUserDailyCommentsList = class
    strict private
        var FCommentId:            TArray<integer>;
        var FSourceDBName:         TArray<string>;
        var FCustomerNumber:       TArray<integer>;
        var FAgeDate:              TArray<string>;
        var FCallEvent:            TArray<integer>;
        var FCallDuration:         TArray<integer>;
        var FFixedStatementsSent:  TArray<integer>;
        var FCustomStatementsSent: TArray<integer>;
        var FFixedRemindersSent:   TArray<integer>;
        var FCustomRemindersSent:  TArray<integer>;
        var FUserComment:          TArray<string>;
        var FUserAlias:            TArray<string>;
        var FEntryDateTime:        TArray<string>;
        var FIsSucceeded:          boolean;
        var FError:                TErrorHandler;
        var FMeta:                 TMetaData;
    public
        destructor Destroy(); override;
        const _CommentId            = 'CommentId';
        const _SourceDBName         = 'SourceDBName';
        const _CustomerNumber       = 'CustomerNumber';
        const _AgeDate              = 'AgeDate';
        const _CallEvent            = 'CallEvent';
        const _CallDuration         = 'CallDuration';
        const _FixedStatementsSent  = 'FixedStatementsSent';
        const _CustomStatementsSent = 'CustomStatementsSent';
        const _FixedRemindersSent   = 'FixedRemindersSent';
        const _CustomRemindersSent  = 'CustomRemindersSent';
        const _UserComment          = 'UserComment';
        const _UserAlias            = 'UserAlias';
        const _EntryDateTime        = 'EntryDateTime';
        const _IsSucceeded          = 'IsSucceeded';
        const _Error                = 'Error';
        const _Meta                 = 'Meta';
        property CommentId:            TArray<integer> read FCommentId            write FCommentId;
        property SourceDBName:         TArray<string>  read FSourceDBName         write FSourceDBName;
        property CustomerNumber:       TArray<integer> read FCustomerNumber       write FCustomerNumber;
        property AgeDate:              TArray<string>  read FAgeDate              write FAgeDate;
        property CallEvent:            TArray<integer> read FCallEvent            write FCallEvent;
        property CallDuration:         TArray<integer> read FCallDuration         write FCallDuration;
        property FixedStatementsSent:  TArray<integer> read FFixedStatementsSent  write FFixedStatementsSent;
        property CustomStatementsSent: TArray<integer> read FCustomStatementsSent write FCustomStatementsSent;
        property FixedRemindersSent:   TArray<integer> read FFixedRemindersSent   write FFixedRemindersSent;
        property CustomRemindersSent:  TArray<integer> read FCustomRemindersSent  write FCustomRemindersSent;
        property UserComment:          TArray<string>  read FUserComment          write FUserComment;
        property UserAlias:            TArray<string>  read FUserAlias            write FUserAlias;
        property EntryDateTime:        TArray<string>  read FEntryDateTime        write FEntryDateTime;
        property IsSucceeded:          boolean         read FIsSucceeded          write FIsSucceeded;
        property Error:                TErrorHandler   read FError                write FError;
        property Meta:                 TMetaData       read FMeta                 write FMeta;
    end;


implementation


destructor TUserDailyCommentsList.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.

