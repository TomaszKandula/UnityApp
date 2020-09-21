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
    Api.ErrorHandler,
    Api.MetaData;


type


	TUserGeneralCommentCheck = class
	strict private
        var FDoesCommentExists: boolean;
        var FCommentId:         integer;
        var FUserComment:       string;
        var FIsSucceeded:       boolean;
        var FError:             TErrorHandler;
        var FMeta:              TMetaData;
	public
        constructor Create();
        destructor Destroy(); override;
        const _DoesCommentExists = 'DoesCommentExists';
        const _CommentId         = 'CommentId';
        const _UserComment       = 'UserComment';
        const _IsSucceeded       = 'IsSucceeded';
        const _Error             = 'Error';
        const _Meta              = 'Meta';
        property DoesCommentExists: boolean       read FDoesCommentExists write FDoesCommentExists;
        property CommentId:         integer       read FCommentId         write FCommentId;
        property UserComment:       string        read FUserComment       write FUserComment;
        property IsSucceeded:       boolean       read FIsSucceeded       write FIsSucceeded;
        property Error:             TErrorHandler read FError             write FError;
        property Meta:              TMetaData     read FMeta              write FMeta;
	end;


implementation


constructor TUserGeneralCommentCheck.Create();
begin
    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();
end;


destructor TUserGeneralCommentCheck.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta)  then FMeta.Free();
    inherited;
end;


end.

