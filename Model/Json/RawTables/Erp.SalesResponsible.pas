unit Erp.SalesResponsible;

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


	TSalesResponsible = class
	private
		FId:				TArray<integer>;
		FSourceDBName:		TArray<string>;
		FErpCode:			TArray<string>;
		FDescription:		TArray<string>;
		FExtractDateStamp:	TArray<TDateTime>;
		FProcessBatchKey:	TArray<integer>;
	public
		property Id:				TArray<integer>		read FId				write FId;
		property SourceDBName:		TArray<string>		read FSourceDBName		write FSourceDBName;
		property ErpCode:			TArray<string>		read FErpCode			write FErpCode;
		property Description:		TArray<string>		read FDescription		write FDescription;
		property ExtractDateStamp:	TArray<TDateTime>	read FExtractDateStamp	write FExtractDateStamp;
		property ProcessBatchKey:	TArray<integer>		read FProcessBatchKey	write FProcessBatchKey;
	end;

	
implementation


end.
