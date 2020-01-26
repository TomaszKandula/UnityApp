unit Api.UserCustSnapshotList;

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


    TUserCustSnapshotList = class
    {$TYPEINFO ON}
    strict private
        var FSelectedCoCodes: TArray<string>;
        var FSortMode:        string;
    public
        const _SelectedCoCodes = 'SelectedCoCodes';
        const _SortMode        = 'SortMode';
    published
        property SelectedCoCodes: TArray<string> read FSelectedCoCodes write FSelectedCoCodes;
        property SortMode:        string         read FSortMode        write FSortMode;
    end;


implementation


end.

