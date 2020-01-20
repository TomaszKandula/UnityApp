unit Api.ReturnControlStatus;

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


    TReturnControlStatus = class
    {$TYPEINFO ON}
    strict private
        var FId:          TArray<integer>;
        var FCode:        TArray<integer>;
        var FText:        TArray<string>;
        var FDescription: TArray<string>;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
    public
        const _Id          = 'Id';
        const _Code        = 'Code';
        const _Text        = 'Text';
        const _Description = 'Description';
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        destructor Destroy(); override;
    published
        property Id:          TArray<integer> read FId          write FId;
        property Code:        TArray<integer> read FCode        write FCode;
        property Text:        TArray<string>  read FText        write FText;
        property Description: TArray<string>  read FDescription write FDescription;
        property IsSucceeded: boolean         read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler   read FError       write FError;
    end;


implementation


destructor TReturnControlStatus.Destroy();
begin
    if Assigned(FError) then FError.Free();
    inherited;
end;


end.
