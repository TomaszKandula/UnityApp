unit Erp.PaymentTerms;

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


	TPaymentTerms = class
	private
		FId:				TArray<integer>;
		FErpCode:			TArray<integer>;
		FDescription:		TArray<string>;
		FMonth:				TArray<integer>;
		FDays:				TArray<integer>;
		FDaysNet:			TArray<integer>;
		FUsing:				TArray<integer>;
		FExtractDateStamp:	TArray<TDateTime>;
		FProcessBatchKey:	TArray<integer>;
		FEntity:			TArray<integer>;
	public
		property Id:				TArray<integer>		read FId 				write FId;
		property ErpCode:			TArray<integer>		read FErpCode			write FErpCode;
		property Description:		TArray<string>		read FDescription		write FDescription;
		property Month:				TArray<integer>		read FMonth				write FMonth;
		property Days:				TArray<integer>		read FDays				write FDays;
		property DaysNet:			TArray<integer>		read FDaysNet			write FDaysNet;
		property Using:				TArray<integer> 	read FUsing				write FUsing;
		property ExtractDateStamp:	TArray<TDateTime> 	read FExtractDateStamp	write FExtractDateStamp;
		property ProcessBatchKey:	TArray<integer>		read FProcessBatchKey	write FProcessBatchKey;
		property Entity:			TArray<integer>		read FEntity			write FEntity;
	end;

	
implementation


end.
