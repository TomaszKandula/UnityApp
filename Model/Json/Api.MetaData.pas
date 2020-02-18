unit Api.MetaData;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TMetaData = class
    strict private
        var FRowsAffected:       integer;
        var FProcessingTimeSpan: string;
        var FRequesterIpAddress: string;
    public
        const _RowsAffected       = 'RowsAffected';
        const _ProcessingTimeSpan = 'ProcessingTimeSpan';
        const _RequesterIpAddress = 'RequesterIpAddress';
        property RowsAffected:       integer read FRowsAffected       write FRowsAffected;
        property ProcessingTimeSpan: string  read FProcessingTimeSpan write FProcessingTimeSpan;
        property RequesterIpAddress: string  read FRequesterIpAddress write FRequesterIpAddress;
    end;


implementation


end.
