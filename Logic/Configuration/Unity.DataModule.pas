unit Unity.DataModule;


interface


uses
    System.Classes,
    Unity.EventLogger;


type


    TDataModule = class(TObject)
    {$TYPEINFO ON}
    private
        var FAppEvents: TThreadFileLog;
    public
        constructor Create();
        destructor Destroy(); //override;
    end;


implementation


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


constructor TDataModule.Create();
begin
    if not Assigned(FAppEvents) then FAppEvents:=TThreadFileLog.Create();
end;


destructor TDataModule.Destroy(); //override;
begin
    if Assigned(FAppEvents) then FAppEvents.Free();
    inherited;
end;


end.

