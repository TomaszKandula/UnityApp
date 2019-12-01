unit Unity.SessionService;

// ----------------------------------------------------------------
// Application session service module. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------------------------------

interface


uses
    System.Classes,
    System.SysUtils,
    Unity.Records,
    Data.Win.ADODB {Legacy};


type


    TSessionService = class(TObject)
    {$TYPEINFO ON}
    strict private
        var FSessionId:   string;
        var FSessionLog:  string;
        var FSessionData: TSessionData;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure InitializeSession(GenSessionId: string; SessionLog: string);
        procedure UpdateUserData(UnityUserId: integer; Department: string; AliasName: string; DisplayName: string; EmailAddress: string);
        property SessionId:   string       read FSessionId;
        property SessionLog:  string       read FSessionLog;
        property SessionData: TSessionData read FSessionData;
        var FDbConnect: TADOConnection; {Legacy}
    end;


    function SessionService(): TSessionService;
    procedure DestroySessionService();


implementation


var
    VSessionService: TSessionService;


function SessionService(): TSessionService;
begin
    if not(Assigned(VSessionService)) then VSessionService:=TSessionService.Create();
    Result:=VSessionService;
end;


procedure DestroySessionService();
begin
    if Assigned(VSessionService) then FreeAndNil(VSessionService);
end;


constructor TSessionService.Create();
begin
    {Do nothing}
end;


destructor TSessionService.Destroy();
begin
    {Do nothing}
    inherited;
end;


procedure TSessionService.InitializeSession(GenSessionId: string; SessionLog: string);
begin
    FSessionId  :=GenSessionId;
    FSessionLog :=SessionLog;
end;


procedure TSessionService.UpdateUserData(UnityUserId: integer; Department: string; AliasName: string; DisplayName: string; EmailAddress: string);
begin
    FSessionData.UnityUserId :=UnityUserId;
    FSessionData.Department  :=Department;
    FSessionData.AliasName   :=AliasName;
    FSessionData.DisplayName :=DisplayName;
    FSessionData.EmailAddress:=EmailAddress;
end;


end.

