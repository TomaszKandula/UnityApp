unit Api.ControlStatusFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TControlStatusFields = class
    strict private
        var FId:          integer;
        var FCode:        integer;
        var FText:        string;
        var FDescription: string;
    public
        const _Id          = 'Id';
        const _Code        = 'Code';
        const _Text        = 'Text';
        const _Description = 'Description';
        property Id:          integer read FId          write FId;
        property Code:        integer read FCode        write FCode;
        property Text:        string  read FText        write FText;
        property Description: string  read FDescription write FDescription;
    end;


implementation


end.
