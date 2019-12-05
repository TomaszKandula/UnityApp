unit Api.UserSessionLogs;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type


	TUserSessionLogs = class
    {$TYPEINFO ON}
	strict private
        var FUserAlias: string;
        var FAppEventLog: string;
        var FAppName: string;
	public
        property UserAlias: string read FUserAlias write FUserAlias;
        property AppEventLog: string read FAppEventLog write FAppEventLog;
        property AppName: string read FAppName write FAppName;
	end;


implementation


end.
