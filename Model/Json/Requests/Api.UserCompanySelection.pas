unit Api.UserCompanySelection;

// -------------------------------------------------
// JSON model for REST. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// Cannot have any implementation other than fields.
// -------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type


	TUserCompanySelection = class
    {$TYPEINFO ON}
	strict private
        var FSelectedCoCodes: TList<integer>;
	public
        property SelectedCoCodes: TList<integer> read FSelectedCoCodes write FSelectedCoCodes;
	end;


implementation


end.
