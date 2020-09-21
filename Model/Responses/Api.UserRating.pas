unit Api.UserRating;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.ErrorHandler,
    Api.MetaData;


type


    TUserRating = class
    strict private
        var FRating:      integer;
        var FComment:     string;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
        var FMeta:        TMetaData;
    public
        constructor Create();
        destructor Destroy(); override;
        const _Rating      ='Rating';
        const _Comment     ='Comment';
        const _IsSucceeded ='IsSucceeded';
        const _Error       ='Error';
        const _Meta        = 'Meta';
        property Rating:      integer       read FRating      write FRating;
        property Comment:     string        read FComment     write FComment;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
        property Meta:        TMetaData     read FMeta        write FMeta;
    end;


implementation


constructor TUserRating.Create();
begin
    if not Assigned(FError) then FError:=TErrorHandler.Create();
    if not Assigned(FMeta)  then FMeta :=TMetaData.Create();
end;


destructor TUserRating.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.

