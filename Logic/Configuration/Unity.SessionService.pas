unit Unity.SessionService;


interface


uses
    System.Classes,
    System.SysUtils,
    Data.Win.ADODB {legacy};


type


    TSessionService = class(TObject)
    {$TYPEINFO ON}
    private
        var FSessionUser: string;
        var FSessionId:   string;
        var FSessionLog:  string;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure InitializeSession(CurrentUser: string; GenSessionId: string; SessionLog: string);
        property SessionUser: string read FSessionUser;
        property SessionId:   string read FSessionId;
        property SessionLog:  string read FSessionLog;
        var FDbConnect: TADOConnection; {Temporary holder for sql connection}
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



// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TSessionService.Create();
begin
    {Do nothing}
end;


destructor TSessionService.Destroy();
begin
    {Do nothing}
    inherited;
end;


// ----------------------------------------------------------------------------------------------------------------------------------- INITIALIZE PROPERTIES //


procedure TSessionService.InitializeSession(CurrentUser: string; GenSessionId: string; SessionLog: string);
begin
    FSessionUser:=CurrentUser;
    FSessionId  :=GenSessionId;
    FSessionLog :=SessionLog;
end;


end.

