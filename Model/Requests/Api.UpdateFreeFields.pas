unit Api.UpdateFreeFields;


interface


uses
    Api.FreeFields;


type


	TUpdateFreeFields = class
	strict private
        var FUserAlias:  string;
        var FFreeFields: TArray<TFreeFields>;
	public
        destructor Destroy(); override;
        const _UserAlias  = 'UserAlias';
        const _FreeFields = 'FreeFields';
        property UserAlias:  string              read FUserAlias  write FUserAlias;
        property FreeFields: TArray<TFreeFields> read FFreeFields write FFreeFields;
	end;


implementation


destructor TUpdateFreeFields.Destroy();
begin
    for var FreeFields: TFreeFields in FFreeFields do FreeFields.Free();
    inherited;
end;


end.

