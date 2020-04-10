unit Unity.Types;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface

// -----------------------------------------------------------------------------
// Note:
//    Some types may have the same signature. The reason why we have individual
//    types for the same signature rather than one common is that, it is easier
//    to extend, it is enough to just add argument to the existing signature for
//    given method, so it can be used right away by given implementation and
//    by callback consumer.
// -----------------------------------------------------------------------------

uses
    Unity.Grid,
    Unity.Records,
    Api.ReturnCompanyDetails;


type

    /// <summary>
    /// Define custom type for anonymous method to be passed as parameter.
    /// </summary>
    TInputMethod = reference to procedure;
    /// <summary>
    /// Array definition for SID number calculation. Note that it has 261 characters.
    /// </summary>
    TSidArray = array[0..260] of Char;
    /// <summary>
    /// Callback signature for inserting new user rating.
    /// </summary>
    TSubmitRating = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for updating existing user rating.
    /// </summary>
    TUpdateRating = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from address book open action.
    /// </summary>
    TOpenAddressBook = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from address book update action.
    /// </summary>
    TUpdateAddressBook = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from address book insert action.
    /// </summary>
    TAddToAddressBook = procedure(ReturnedId: integer; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from customer details retrieval.
    /// </summary>
    TGetCustomerDetails = procedure(CustDetails: TCustomerDetails; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for updating/inserting data for daily comment.
    /// </summary>
    TEditDailyComment = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for updating/inserting data for general comment.
    /// </summary>
    TEditGeneralComment = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from daily comments list request.
    /// </summary>
    TGetDailyComments = procedure(Comments: TArray<TDailyCommentFields>; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results for general comment single dataset.
    /// </summary>
    TGetGeneralComments = procedure(Comments: TGeneralCommentFields; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting list of details for given companies.
    /// </summary>
    TGetCompanyDetails = procedure(CompanyDetails: TReturnCompanyDetails; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results for company details.
    /// </summary>
    TGetCompanySpecifics = procedure(CompanySpecifics: TCompanySpecifics; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for reading current age report from SQL database.
    /// </summary>
    TReadAgeView = procedure(ReturnedData: TStringGrid; PayLoad: TAgingPayLoad; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for returning information from sending single account statement.
    /// </summary>
    TSendAccDocument = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for returning information from sending many account statements.
    /// </summary>
    TSendAccDocuments = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from given database table.
    /// </summary>
    /// <remarks>
    /// Please note that "table" does not necessarily mean that we will get table "as is".
    /// It depends on underlaying API and the dataset may contain joined tables.
    /// </remarks>
    TGetTables = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting invoice list.
    /// </summary>
    TGetInvoiceList = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for returning reloaded data from invoice tracker database table.
    /// </summary>
    TRefreshInvoiceTracker = procedure(InvoiceList: TStringGrid; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for removing item from invoice tracker database table.
    /// </summary>
    TDeleteFromTrackerList = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from sending user email with feedback message.
    /// </summary>
    TSendUserFeedback = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for scanning SSIS master table to check if open items have been updated.
    /// </summary>
    TScanOpenItems = procedure(CanGetAge: boolean; ReadDateTime: string; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for reading open items. Payload returned contains summary data from loaded invoices.
    /// </summary>
    TReadOpenItems = procedure(OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from exporting data grid to Excel file.
    /// </summary>
    TExcelExport = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from checking supplied local administrator password.
    /// </summary>
    TCheckGivenPassword = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for getting results from setting new local administrator password.
    /// </summary>
    TSetNewPassword = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for method returning information on the lates available version of Unity Platform.
    /// </summary>
    TCheckRelease = procedure(ClientInfo: TClientInfo; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for method updating free fields in General Comment table.
    /// </summary>
    TFreeFieldsUpdate = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for method updating FollowUp field in General Comment table.
    /// </summary>
    TFollowUpUpdate = procedure(CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for method recalculating age summary and risk class change after age view has been filtered.
    /// </summary>
    TRecalcAgeViewSummary = procedure(PayLoad: TAgingPayLoad; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for method getting aging report for given companies.
    /// </summary>
    TGetAgingReport = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for method getting user rating of Unity Platform.
    /// </summary>
    TLoadRating = procedure(UserRating: TRating; CallResponse: TCallResponse) of object;
    /// <summary>
    /// Callback signature for scanning Azure Jobs table to check if snapshots have been updated.
    /// </summary>
    TScanSnapshots = procedure(CanGetAge: boolean; ReceivedTime: string; CallResponse: TCallResponse) of object;


implementation


end.
