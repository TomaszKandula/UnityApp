unit Customer.TrackerInvoices;

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


	TTrackerInvoices = class
	private
		FId:			TArray<integer>;
		FSk:			TArray<integer>;
		FCuid:			TArray<LongInt>;
		FInvoiceNo:		TArray<double>;
		FInvoiceState:	TArray<integer>;
		FStamp:			TArray<TDateTime>;
	public
		property Id:			TArray<integer>		read FId			write FId;
		property Sk:			TArray<integer>		read FSk			write FSk;
		property Cuid:			TArray<LongInt>		read FCuid			write FCuid;
		property InvoiceNo:		TArray<double>		read FInvoiceNo		write FInvoiceNo;
		property InvoiceState:	TArray<integer>		read FInvoiceState	write FInvoiceState;
		property Stamp:			TArray<TDateTime>	read FStamp			write FStamp;
	end;

	
implementation


end.
