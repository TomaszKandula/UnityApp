unit Api.UserGeneralCommentCheck;

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


	TUserGeneralCommentCheck = class
    {$TYPEINFO ON}
	strict private
        var FDoesCommentExists: boolean;
        var FCommentId:         integer;
        var FUserComment:       string;
        var FIsSucceeded:       boolean;
        var FError:             TErrorHandler;
	public
        const _DoesCommentExists = 'DoesCommentExists';
        const _CommentId         = 'CommentId';
        const _UserComment       = 'UserComment';
        const _IsSucceeded       = 'IsSucceeded';
        const _Error             = 'Error';
        destructor Destroy(); override;
    published
        property DoesCommentExists: boolean       read FDoesCommentExists write FDoesCommentExists;
        property CommentId:         integer       read FCommentId         write FCommentId;
        property UserComment:       string        read FUserComment       write FUserComment;
        property IsSucceeded:       boolean       read FIsSucceeded       write FIsSucceeded;
        property Error:             TErrorHandler read FError             write FError;
	end;


implementation


destructor TUserGeneralCommentCheck.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.

