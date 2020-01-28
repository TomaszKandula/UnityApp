unit Api.TokenGranted;

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


    TTokenGranted = class
    {$TYPEINFO ON}
    strict private
        var FAccessToken: string;
        var FTokenType:   string;
        var FGrantType:   string;
        var FExpiresIn:   string;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
    public
        const _AccessToken   = 'AccessToken';
        const _TokenType     = 'TokenType';
        const _GrantType     = 'GrantType';
        const _ExpiresIn     = 'ExpiresIn';
        const _IsSucceeded   = 'IsSucceeded';
        const _Error         = 'Error';
        destructor Destroy(); override;
    published
        property AccessToken: string        read FAccessToken write FAccessToken;
        property TokenType:   string        read FTokenType   write FTokenType;
        property GrantType:   string        read FGrantType   write FGrantType;
        property ExpiresIn:   string        read FExpiresIn   write FExpiresIn;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
    end;


implementation


destructor TTokenGranted.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.
