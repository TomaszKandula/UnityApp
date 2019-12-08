unit Api.ErrorHandler;

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


    TErrorHandler = class
    {$TYPEINFO ON}
    strict private
        var FErrorDesc: string;
        var FErrorNum: integer;
    public
        property ErrorDesc: string  read FErrorDesc write FErrorDesc;
        property ErrorNum:  integer read FErrorNum  write FErrorNum;
    end;


implementation


end.

