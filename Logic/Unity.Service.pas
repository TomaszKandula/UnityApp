unit Unity.Service;

// ----------------------------------------------------------------
// Application session service module. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------------------------------

interface


uses
    System.Classes,
    System.SysUtils,
    Unity.Records,
    Unity.RestWrapper,
    Unity.EventLogger,
    Unity.Settings,
    Unity.Mediator;


type


    TService = class(TObject)
    strict private
        var FSessionId:    string;
        var FSessionLog:   string;
        var FSessionData:  TSessionData;
        var FAccessToken:  string;
        var FMediator:     IMediator;
        var FLogger:       ILogger;
        var FSettings:     ISettings;
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


end.

