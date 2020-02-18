unit Api.UserRatingAdd;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TUserRatingAdd = class
    strict private
        var FUserRating: integer;
        var FComment:    string;
    public
        const _UserRating = 'UserRating';
        const _Comment    = 'Comment';
        property UserRating: integer read FUserRating write FUserRating;
        property Comment:    string  read FComment    write FComment;
    end;


implementation


end.
