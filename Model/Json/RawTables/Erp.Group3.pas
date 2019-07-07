unit Erp.Group3;

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


	TGroup3 = class
	private
		FId:				TArray<integer>;
		FErpCode:			TArray<integer>;
		FDescription:		TArray<string>;
		FExtractDateStamp:	TArray<TDateTime>;
		FProcessBatchKey:	TArray<integer>;
		FEntity:			TArray<integer>;
	public
		property Id:				TArray<integer>		read FId 				write FId; 
		property ErpCode:			TArray<integer>		read FErpCode			write FErpCode;
		property Description:		TArray<string>		read FDescription		write FDescription;
		property ExtractDateStamp:	TArray<TDateTime>	read FExtractDateStamp	write FExtractDateStamp;
		property ProcessBatchKey:	TArray<integer>		read FProcessBatchKey	write FProcessBatchKey;
		property Entity:			TArray<integer>		read FEntity			write FEntity;
	end;

	
implementation


end.
