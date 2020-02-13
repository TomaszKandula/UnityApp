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
    Api.ErrorHandler,
    Api.MetaData;


type


    TTokenGranted = class
    {$TYPEINFO ON}
    strict private
        var FAccessToken: string;
        var FTokenType:   string;
        var FGrantType:   string;
        var FExpiresIn:   integer;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
        var FMeta:        TMetaData;
    public
        const _AccessToken   = 'AccessToken';
        const _TokenType     = 'TokenType';
        const _GrantType     = 'GrantType';
        const _ExpiresIn     = 'ExpiresIn';
        const _IsSucceeded   = 'IsSucceeded';
        const _Error         = 'Error';
        const _Meta          = 'Meta';
        destructor Destroy(); override;
    published
        property AccessToken: string        read FAccessToken write FAccessToken;
        property TokenType:   string        read FTokenType   write FTokenType;
        property GrantType:   string        read FGrantType   write FGrantType;
        property ExpiresIn:   integer       read FExpiresIn   write FExpiresIn;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
        property Meta:        TMetaData     read FMeta        write FMeta;
    end;


implementation


destructor TTokenGranted.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.
