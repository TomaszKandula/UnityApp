unit Customer.ControlStatus;

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


	TControlStatus = class
	private
		FId:			TArray<integer>;
		FCode:			TArray<integer>;
		FText:			TArray<string>;
		FDescription:	TArray<string>;
	public
		property Id:			TArray<integer>	read FId 			write FId;
		property Code:			TArray<integer>	read FCode			write FCode;
		property Text:			TArray<string>	read FText			write FText;
		property Description:	TArray<string>	read FDescription	write FDescription;
	end;

	
implementation


end.
