unit Customer.Snapshots;


interface


uses
    Generics.Collections,
    Rest.Json;

	
type


	TSnapshots = class
	private
		FId:				TArray<integer>;
		FAgeDate:			TArray<TDate>;
		FSnapshotDt:		TArray<TDateTime>;
		FCustomerName:		TArray<string>;
		FCustomerNumber:	TArray<integer>;
		FCountryCode:		TArray<integer>;
		FNotDue:			TArray<double>;
		FRange1:			TArray<double>;
		FRange2:			TArray<double>;
		FRange3:			TArray<double>;
		FRange4:			TArray<double>;
		FRange5:			TArray<double>;
		FRange6:			TArray<double>;
		FOverdue:			TArray<double>;
		FTotal:				TArray<double>;
		FCreditLimit:		TArray<double>;
		FExceededAmount:	TArray<double>;
		FPaymentTerms:		TArray<string>;
		FAgent:				TArray<string>;
		FDivision:			TArray<integer>;
		FCoCode:			TArray<string>;
		FLedgerIso:			TArray<string>;
		FInf4:				TArray<string>;
		FInf7:				TArray<string>;
		FPerson:			TArray<string>;
		FGroup3:			TArray<string>;
		FRiskClass:			TArray<string>;
		FCuid:				TArray<LongInt>;
		FSalesResponsible:	TArray<string>;
		FCustomerGroup:		TArray<string>;
		FPersonResponsible:	TArray<string>;
		FAccountType:		TArray<string>;
	public
		property Id:				TArray<integer>     read FId           		write FId;
		property AgeDate:			TArray<TDate>	    read FAgeDate      		write FAgeDate;
		property SnapshotDt:		TArray<TDateTime>	read FSnapshotDt   		write FSnapshotDt;
		property CustomerName:		TArray<string>	    read FCustomerName 		write FCustomerName;
		property CustomerNumber:	TArray<integer>	    read FCustomerNumber 	write FCustomerNumber;
		property CountryCode:		TArray<integer>     read FCountryCode		write FCountryCode;
		property NotDue:			TArray<double>	    read FNotDue			write FNotDue;
		property Range1:			TArray<double>	    read FRange1			write FRange1;
		property Range2:			TArray<double>	    read FRange2			write FRange2;
		property Range3:			TArray<double>	    read FRange3			write FRange3;
		property Range4:			TArray<double>	    read FRange4			write FRange4;
		property Range5:			TArray<double>	    read FRange5			write FRange5;
		property Range6:			TArray<double>	    read FRange6			write FRange6;
		property Overdue:			TArray<double>	    read FOverdue			write FOverdue;
		property Total:				TArray<double>	    read FTotal				write FTotal;
		property CreditLimit:		TArray<double>	    read FCreditLimit		write FCreditLimit;
		property ExceededAmount:	TArray<double>	    read FExceededAmount	write FExceededAmount;
		property PaymentTerms:		TArray<string>	    read FPaymentTerms		write FPaymentTerms;
		property Agent:				TArray<string>	    read FAgent				write FAgent;
		property Division:			TArray<integer>	    read FDivision			write FDivision;
		property CoCode:			TArray<string>	    read FCoCode			write FCoCode;
		property LedgerIso:			TArray<string>	    read FLedgerIso			write FLedgerIso;
		property Inf4:				TArray<string>	    read FInf4				write FInf4;
		property Inf7:				TArray<string>	    read FInf7				write FInf7;
		property Person:			TArray<string>	    read FPerson			write FPerson;
		property Group3:			TArray<string>	    read FGroup3			write FGroup3;
		property RiskClass:			TArray<string>	    read FRiskClass			write FRiskClass;
		property Cuid:				TArray<LongInt>	    read FCuid				write FCuid;
		property SalesResponsible:	TArray<string>	    read FSalesResponsible	write FSalesResponsible;
		property CustomerGroup:		TArray<string>	    read FCustomerGroup		write FCustomerGroup;
		property PersonResponsible:	TArray<string>	    read FPersonResponsible	write FPersonResponsible;
		property AccountType:		TArray<string>	    read FAccountType		write FAccountType;
	end;

	
implementation


end.
