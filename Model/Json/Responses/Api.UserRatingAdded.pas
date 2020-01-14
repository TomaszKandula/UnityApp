unit Api.UserRatingAdded;

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


    TUserRatingAdded = class
    {$TYPEINFO ON}
    strict private
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
    public
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
    published
        destructor Destroy(); override;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
    end;


implementation


destructor TUserRatingAdded.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.

