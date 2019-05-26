unit Erp.Person;


interface


uses
    Generics.Collections,
    Rest.Json;

	
type


	TPerson = class
	private
		FId:				TArray<integer>;
		FErpCode:			TArray<string>;
		FDescription:		TArray<string>;
		FExtractDateStamp:	TArray<TDateTime>;
		FProcessBatchKey:	TArray<integer>;
		FEntity:			TArray<integer>;
	public
		property Id:				TArray<integer> 	read FId				write FId;
		property ErpCode:			TArray<string>		read FErpCode			write FErpCode;
		property Description:		TArray<string>		read FDescription		write FDescription;
		property ExtractDateStamp:	TArray<TDateTime>	read FExtractDateStamp	write FExtractDateStamp;
		property ProcessBatchKey:	TArray<integer>		read FProcessBatchKey	write FProcessBatchKey;
		property Entity:			TArray<integer>		read FEntity			write FEntity;
	end;

	
implementation


end.
