unit Customer.AddressBook;


interface


uses
    Generics.Collections,
    Rest.Json;

	
type


	TAddressBook = class
	private
		FId:				TArray<integer>;
		FUserAlias:			TArray<string>;
		FScuid:				TArray<LongInt>;
		FCustomerNumber:	TArray<integer>;
		FCustomerName:		TArray<string>;
		FEmails:			TArray<string>;
		FPhoneNumbers:		TArray<string>;
		FContact:			TArray<string>;
		FEstatements:		TArray<string>;
		FAgent:				TArray<string>;
		FDivision:			TArray<integer>;
		FCoCode:			TArray<integer>;
	public
		property Id:				TArray<integer>	read FId				write FId;
		property UserAlias:			TArray<string>	read FUserAlias			write FUserAlias;
		property Scuid:				TArray<LongInt>	read FScuid				write FScuid;
		property CustomerNumber:	TArray<integer>	read FCustomerNumber	write FCustomerNumber;
		property CustomerName:		TArray<string>	read FCustomerName		write FCustomerName;
		property Emails:			TArray<string>	read FEmails			write FEmails;
		property PhoneNumbers:		TArray<string>	read FPhoneNumbers		write FPhoneNumbers;
		property Contact:			TArray<string>	read FContact			write FContact;
		property Estatements:		TArray<string>	read FEstatements		write FEstatements;
		property Agent:				TArray<string>	read FAgent				write FAgent;
		property Division:			TArray<integer>	read FDivision			write FDivision;
		property CoCode:			TArray<integer>	read FCoCode			write FCoCode;
	end;

	
implementation


end.
