unit Unity.Service;

// ----------------------------------------------------------------
// Application session service module. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------------------------------

interface


uses
    System.Classes,
    System.SysUtils,
    Unity.Enums,
    Unity.Records,
    Unity.RestWrapper,
    Unity.EventLogger,
    Unity.Settings,
    Unity.Mediator,
    Api.UserPermissions;


type


    TService = class(TObject)
    strict private
        var FSessionId:       string;
        var FSessionLog:      string;
        var FSessionData:     TSessionData;
        var FAccessToken:     string;
        var FMediator:        IMediator;
        var FLogger:          ILogger;
        var FSettings:        ISettings;
        var FPermissionArray: TArray<TArray<integer>>;
        procedure RegisterAccessToken(NewValue: string);
    public
        constructor Create();
        destructor Destroy(); override;
        property AccessToken:  string       read FAccessToken write RegisterAccessToken;
        property SessionId:    string       read FSessionId;
        property SessionLog:   string       read FSessionLog;
        property SessionData:  TSessionData read FSessionData;
        property Mediator:     IMediator    read FMediator;
        property Logger:       ILogger      read FLogger;
        property Settings:     ISettings    read FSettings;
        function InvokeRest(): IRESTFul;
        procedure InitializeSession(GenSessionId: string; SessionLog: string);
        procedure UpdateUserData(UnityUserId: integer; Department: string; AliasName: string; DisplayName: string; EmailAddress: string);
        procedure UpdateUserPermissions(UserPermissions: TArray<TUserPermissions>);
        function GetUserPermission(Module: TModules): TPermissions;
    end;


    function Service(): TService;
    procedure UnloadService();


implementation


var
    VService: TService;


function Service(): TService;
begin
    if not(Assigned(VService)) then VService:=TService.Create();
    Result:=VService;
end;


procedure UnloadService();
begin
    if Assigned(VService) then FreeAndNil(VService);
end;


constructor TService.Create();
begin

    if not Assigned(FMediator) then
    begin
        FMediator:=TMediator.Create();
        FMediator.ForceInitialize();
    end;

    if not Assigned(FSettings) then FSettings:=TSettings.Create();

end;


destructor TService.Destroy();
begin
    inherited;
end;


function TService.InvokeRest(): IRESTFul;
begin
    Result:=TRESTful.Create();
end;


procedure TService.InitializeSession(GenSessionId: string; SessionLog: string);
begin
    FSessionId :=GenSessionId;
    FSessionLog:=SessionLog;
    if not Assigned(FLogger) then FLogger:=TThreadFileLog.Create(FSessionLog);
end;


procedure TService.RegisterAccessToken(NewValue: string);
begin
    if FAccessToken = String.Empty then
        FAccessToken:=NewValue
    else
        raise Exception.Create('Access token has been registered already.');
end;


procedure TService.UpdateUserData(UnityUserId: integer; Department: string; AliasName: string; DisplayName: string; EmailAddress: string);
begin
    FSessionData.UnityUserId :=UnityUserId;
    FSessionData.Department  :=Department;
    FSessionData.AliasName   :=AliasName;
    FSessionData.DisplayName :=DisplayName;
    FSessionData.EmailAddress:=EmailAddress;
end;


procedure TService.UpdateUserPermissions(UserPermissions: TArray<TUserPermissions>);
begin

    var Counts:=Length(UserPermissions);
    SetLength(FPermissionArray, Counts, 2);

    for var Index:=0 to Counts - 1 do
    begin
        FPermissionArray[Index, 0]:=UserPermissions[Index].ModuleId;
        FPermissionArray[Index, 1]:=UserPermissions[Index].PermissionId;
    end;

end;


function TService.GetUserPermission(Module: TModules): TPermissions;
begin

    Result:=TPermissions.Undefined;
    if not Assigned(FPermissionArray) then Exit();

    for var Index:=0 to Length(FPermissionArray) - 1 do
    begin

        if FPermissionArray[Index, 0] = Ord(Module) then
        begin

            case FPermissionArray[Index, 1] of
                10: Result:=TPermissions.Read;
                11: Result:=TPermissions.ReadWrite;
                12: Result:=TPermissions.Deny;
            end;

            Break;

        end;

    end;

end;


end.

