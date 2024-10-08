unit Api.UserGeneralCommentUpdate;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


	TUserGeneralCommentUpdate = class
	strict private
        var FCommentId:   integer;
        var FFollowUp:    string;
        var FFree1:       string;
        var FFree2:       string;
        var FFree3:       string;
        var FUserComment: string;
	public
        const _CommentId   = 'CommentId';
        const _FollowUp    = 'FollowUp';
        const _Free1       = 'Free1';
        const _Free2       = 'Free2';
        const _Free3       = 'Free3';
        const _UserComment = 'UserComment';
        property CommentId:   integer read FCommentId   write FCommentId;
        property FollowUp:    string  read FFollowUp    write FFollowUp;
        property Free1:       string  read FFree1       write FFree1;
        property Free2:       string  read FFree2       write FFree2;
        property Free3:       string  read FFree3       write FFree3;
        property UserComment: string  read FUserComment write FUserComment;
	end;


implementation


end.

