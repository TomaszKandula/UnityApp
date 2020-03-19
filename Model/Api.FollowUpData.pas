unit Api.FollowUpData;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TFollowUpData = class
    strict private
        var FFollowUps:     string;
        var FSourceDbNames: string;
        var FCustNumbers:   Int64;
    public
        const _FollowUps     = 'FollowUps';
        const _SourceDbNames = 'SourceDbNames';
        const _CustNumbers   = 'CustNumbers';
        property FollowUps:     string read FFollowUps     write FFollowUps;
        property SourceDbNames: string read FSourceDbNames write FSourceDbNames;
        property CustNumbers:   Int64  read FCustNumbers   write FCustNumbers;
    end;


implementation


end.
