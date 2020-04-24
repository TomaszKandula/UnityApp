unit Unity.Types;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface

// -------------------------------------------------------------------------------
// Note:
//    Some callback signatures may have the same signature. The reason why we have
//    individual types for the same signature rather than one common is that,
//    it is easier to extend, it is enough to just add argument to the existing
//    signature for given method, so it can be used right away by given
//    implementation and by callback consumer.
// -------------------------------------------------------------------------------

uses
    Unity.Grid,
    Unity.Records,
    Api.ReturnCompanyDetails,
    Api.ReturnCompanyData,
    Api.UserCompanyList;


type

    // Custom types
    TInputMethod = reference to procedure;
    TSidArray = array[0..260] of Char;

    // Callback signatures
    TSubmitRating          = procedure(CallResponse: TCallResponse) of object;
    TUpdateRating          = procedure(CallResponse: TCallResponse) of object;
    TOpenAddressBook       = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    TUpdateAddressBook     = procedure(CallResponse: TCallResponse) of object;
    TAddToAddressBook      = procedure(ReturnedId: integer; CallResponse: TCallResponse) of object;
    TGetCustomerDetails    = procedure(CustDetails: TCustomerDetails; CallResponse: TCallResponse) of object;
    TEditDailyComment      = procedure(CallResponse: TCallResponse) of object;
    TEditGeneralComment    = procedure(CallResponse: TCallResponse) of object;
    TGetDailyComments      = procedure(Comments: TArray<TDailyCommentFields>; CallResponse: TCallResponse) of object;
    TGetGeneralComments    = procedure(Comments: TGeneralCommentFields; CallResponse: TCallResponse) of object;
    TGetCompanyDetails     = procedure(CompanyDetails: TReturnCompanyDetails; CallResponse: TCallResponse) of object;
    TGetCompanySpecifics   = procedure(CompanySpecifics: TReturnCompanyData; CallResponse: TCallResponse) of object;
    TReadAgeView           = procedure(ReturnedData: TStringGrid; PayLoad: TAgingPayLoad; CallResponse: TCallResponse) of object;
    TSendAccDocument       = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;
    TSendAccDocuments      = procedure(ProcessingItemNo: integer; CallResponse: TCallResponse) of object;
    TGetTables             = procedure(CallResponse: TCallResponse) of object;
    TGetInvoiceList        = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    TRefreshInvoiceTracker = procedure(InvoiceList: TStringGrid; CallResponse: TCallResponse) of object;
    TDeleteFromTrackerList = procedure(CallResponse: TCallResponse) of object;
    TSendUserFeedback      = procedure(CallResponse: TCallResponse) of object;
    TScanOpenItems         = procedure(CanGetAge: boolean; ReadDateTime: string; CallResponse: TCallResponse) of object;
    TReadOpenItems         = procedure(OpenItemsData: TOpenItemsPayLoad; CallResponse: TCallResponse) of object;
    TExcelExport           = procedure(CallResponse: TCallResponse) of object;
    TCheckGivenPassword    = procedure(CallResponse: TCallResponse) of object;
    TSetNewPassword        = procedure(CallResponse: TCallResponse) of object;
    TCheckRelease          = procedure(ClientInfo: TClientInfo; CallResponse: TCallResponse) of object;
    TFreeFieldsUpdate      = procedure(CallResponse: TCallResponse) of object;
    TFollowUpUpdate        = procedure(CallResponse: TCallResponse) of object;
    TRecalcAgeViewSummary  = procedure(PayLoad: TAgingPayLoad; CallResponse: TCallResponse) of object;
    TGetAgingReport        = procedure(ReturnedData: TStringGrid; CallResponse: TCallResponse) of object;
    TLoadRating            = procedure(UserRating: TRating; CallResponse: TCallResponse) of object;
    TScanSnapshots         = procedure(CanGetAge: boolean; ReceivedTime: string; CallResponse: TCallResponse) of object;
    TGetUserCompanyList    = procedure(PayLoad: TUserCompanyList; CallResponse: TCallResponse) of object;
    TSetUserCompanyList    = procedure(CallResponse: TCallResponse) of object;
    TGetOpenItems          = procedure(PayLoad: TStringGrid; CallResponse: TCallResponse) of object;
    TGetBiReports          = procedure(PayLoad: TStringGrid; CallResponse: TCallResponse) of object;


implementation


end.

