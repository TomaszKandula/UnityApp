unit Api.ErrorHandler;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TErrorHandler = class
    strict private
        var FErrorDesc: string;
        var FErrorCode: string;
    public
        const _ErrorDesc = 'ErrorDesc';
        const _ErrorCode = 'ErrorCode';
        property ErrorDesc: string read FErrorDesc write FErrorDesc;
        property ErrorCode: string read FErrorCode write FErrorCode;
    end;


implementation


end.

