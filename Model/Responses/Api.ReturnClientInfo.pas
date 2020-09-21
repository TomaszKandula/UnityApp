unit Api.ReturnClientInfo;

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


    TReturnClientInfo = class
    strict private
        var FVersion:     string;
        var FDate:        string;
        var FStatus:      string;
        var FIsSucceeded: boolean;
        var FError:       TErrorHandler;
        var FMeta:        TMetaData;
    public
        constructor Create();
        destructor Destroy(); override;
        const _Version     = 'Version';
        const _Date        = 'Date';
        const _Status      = 'Status';
        const _IsSucceeded = 'IsSucceeded';
        const _Error       = 'Error';
        const _Meta        = 'Meta';
        property Version:     string        read FVersion     write FVersion;
        property Date:        string        read FDate        write FDate;
        property Status:      string        read FStatus      write FStatus;
        property IsSucceeded: boolean       read FIsSucceeded write FIsSucceeded;
        property Error:       TErrorHandler read FError       write FError;
        property Meta:        TMetaData     read FMeta        write FMeta;
    end;


implementation


constructor TReturnClientInfo.Create();
begin
    if not Assigned(Error) then Error:=TErrorHandler.Create();
    if not Assigned(Meta)  then Meta :=TMetaData.Create();
end;


destructor TReturnClientInfo.Destroy();
begin
    if Assigned(FError) then FError.Free();
    if Assigned(FMeta) then FMeta.Free();
    inherited;
end;


end.
