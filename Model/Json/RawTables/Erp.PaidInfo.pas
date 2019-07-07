unit Erp.PaidInfo;

// ----------------------------------------
// Database model for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type
    TPaidInfo = class
    private
	    FId:          TArray<integer>;
	    FErpCode:     TArray<string>;
    	FDescription: TArray<string>;
    public
	    property Id:          TArray<integer> read FId          write FId;
	    property ErpCode:     TArray<string>  read FErpCode     write FErpCode;
    	property Description: TArray<string>  read FDescription write FDescription;
    end;


implementation


end.

