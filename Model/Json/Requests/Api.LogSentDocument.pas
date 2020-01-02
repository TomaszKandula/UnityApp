unit Api.LogSentDocument;

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


	TLogSentDocument = class
    {$TYPEINFO ON}
	strict private
        var FUserAlias:          string;
        var FReportedCustomer:   integer;
        var FReportedAggrAmount: double;
        var FReportedAgeDate:    string;
        var FPreservedEmail:     string;
        var FDocumentType:       string;
	public
        property UserAlias:          string  read FUserAlias          write FUserAlias;
        property ReportedCustomer:   integer read FReportedCustomer   write FReportedCustomer;
        property ReportedAggrAmount: double  read FReportedAggrAmount write FReportedAggrAmount;
        property ReportedAgeDate:    string  read FReportedAgeDate    write FReportedAgeDate;
        property PreservedEmail:     string  read FPreservedEmail     write FPreservedEmail;
        property DocumentType:       string  read FDocumentType       write FDocumentType;
	end;


implementation


end.
