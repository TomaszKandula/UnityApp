unit Api.FollowUpsUpdate;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.FollowUpData;


type


    TFollowUpsUpdate = class
	strict private
        var FUserAlias:  string;
        var FFollowUpsData: TArray<TFollowUpData>;
    public
        constructor Create(Count: cardinal);
        destructor Destroy(); override;
        const _UserAlias     = 'UserAlias';
        const _FollowUpsData = 'FreeFields';
        property UserAlias:  string  read FUserAlias  write FUserAlias;
        property FollowUpsData: TArray<TFollowUpData> read FFollowUpsData write FFollowUpsData;
    end;


implementation


constructor TFollowUpsUpdate.Create(Count: cardinal);
begin
    SetLength(FFollowUpsData, Count);
    for var Index:=0 to Count - 1 do FFollowUpsData[Index]:=TFollowUpData.Create();
end;


destructor TFollowUpsUpdate.Destroy();
begin
    for var FollowUpsData: TFollowUpData in FFollowUpsData do
        if Assigned(FollowUpsData) then FollowUpsData.Free();
    inherited;
end;


end.
