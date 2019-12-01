unit Api.ErrorHandler;

// -------------------------------------------------
// JSON model for REST. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// Cannot have any implementation other than fields.
// -------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type


    TErrorHandler = class
    strict private
        var FErrorDesc: string;
        var FErrorNum: integer;
    public
        property ErrorDesc: string  read FErrorDesc write FErrorDesc;
        property ErrorNum:  integer read FErrorNum  write FErrorNum;
    end;


implementation


end.
