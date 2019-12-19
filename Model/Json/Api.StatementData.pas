unit Api.StatementData;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;


type


	TStatementData = class
    {$TYPEINFO ON}
	strict private
        var FReportedAggrAmount: double;
        var FReportedAgeDate:    string;
	public
        property ReportedAggrAmount: double read FReportedAggrAmount write FReportedAggrAmount;
        property ReportedAgeDate:    string read FReportedAgeDate    write FReportedAgeDate;
	end;


implementation


end.

