unit Api.UpdateFreeFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.FreeFields;


type


	TUpdateFreeFields = class
	strict private
        var FUserAlias:  string;
        var FFreeFields: TArray<TFreeFields>;
	public
        constructor Create(Count: cardinal = 0);
        destructor Destroy(); override;
        const _UserAlias  = 'UserAlias';
        const _FreeFields = 'FreeFields';
        property UserAlias:  string              read FUserAlias  write FUserAlias;
        property FreeFields: TArray<TFreeFields> read FFreeFields write FFreeFields;
	end;


implementation


constructor TUpdateFreeFields.Create(Count: cardinal = 0);
begin
    if Count = 0 then Exit();
	SetLength(FFreeFields, Count);
    for var Index:=0 to Count - 1 do FFreeFields[Index]:=TFreeFields.Create();
end;


destructor TUpdateFreeFields.Destroy();
begin
    for var FreeFields: TFreeFields in FFreeFields do if Assigned(FreeFields) then FreeFields.Free();
    inherited;
end;


end.

