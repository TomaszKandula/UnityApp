unit Api.UserRatingAdd;

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


    TUserRatingAdd = class
    {$TYPEINFO ON}
    strict private
        var FUserRating: integer;
        var FComment:    string;
    public
        property UserRating: integer read FUserRating write FUserRating;
        property Comment:    string  read FComment    write FComment;
    end;


implementation


end.
