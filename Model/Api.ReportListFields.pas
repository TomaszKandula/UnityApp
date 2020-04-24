unit Api.ReportListFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.ErrorHandler,
    Api.MetaData;


type


    TReportListFields = class
    strict private
        var FReportName: string;
        var FReportDesc: string;
        var FReportLink: string;
    public
        const _ReportName = 'ReportName';
        const _ReportDesc = 'ReportDesc';
        const _ReportLink = 'ReportLink';
        property ReportName: string read FReportName write FReportName;
        property ReportDesc: string read FReportDesc write FReportDesc;
        property ReportLink: string read FReportLink write FReportLink;
    end;


implementation


end.
