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
//    it is easier to extend and it is enough to just add new argument to the
//    existing signature for given method. Thus, it can be used immediately by
//    given implementation and by callback consumer.
// -------------------------------------------------------------------------------

uses
    Unity.Grid,
    Unity.Records,
    Api.SentDocument,
    Api.UserCompanyList,
    Api.ReturnCompanyEmails,
    Api.ReturnCustomerReport,
    Api.ReturnReportList,
    Api.AddressBookItem,
    Api.UserDailyCommentsList,
    Api.UserGeneralComment,
    Api.ReturnOpenItems,
    Api.UserRating,
    Api.AddressBookList,
    Api.ReturnCustomerSnapshots;


type

    // Custom types
    TInputMethod = reference to procedure;
    TSidArray = array[0..260] of Char;

    // Callback signatures
    TAddBulkToAddressBook  = procedure(CallResponse: TCallResponse) of object;
    TAddToAddressBook      = procedure(CallResponse: TCallResponse; ReturnedId: integer) of object;
    TCheckGivenPassword    = procedure(CallResponse: TCallResponse) of object;
    TDeleteFromTrackerList = procedure(CallResponse: TCallResponse) of object;
    TEditDailyComment      = procedure(CallResponse: TCallResponse) of object;
    TEditGeneralComment    = procedure(CallResponse: TCallResponse) of object;
    TExcelExport           = procedure(CallResponse: TCallResponse) of object;
    TFollowUpUpdate        = procedure(CallResponse: TCallResponse) of object;
    TFreeFieldsUpdate      = procedure(CallResponse: TCallResponse) of object;
    TGetAgingReport        = procedure(CallResponse: TCallResponse) of object;
    TGetBiReports          = procedure(PayLoad: TReturnReportList) of object;
    TGetCompanyEmails      = procedure(PayLoad: TReturnCompanyEmails) of object;
    TGetCustomerDetails    = procedure(PayLoad: TAddressBookItem) of object;
    TGetDailyComments      = procedure(PayLoad: TUserDailyCommentsList) of object;
    TGetGeneralComments    = procedure(PayLoad: TUserGeneralComment) of object;
    TGetOpenItems          = procedure(PayLoad: TReturnOpenItems) of object;
    TGetTables             = procedure(CallResponse: TCallResponse) of object;
    TGetUserCompanyList    = procedure(PayLoad: TUserCompanyList) of object;
    TLoadRating            = procedure(PayLoad: TUserRating) of object;
    TOpenAddressBook       = procedure(PayLoad: TAddressBookList) of object;
    TReadAgeView           = procedure(PayLoad: TReturnCustomerSnapshots) of object;
    TReadOpenItems         = procedure(PayLoad: TReturnOpenItems) of object;
    TRecalcAgeViewSummary  = procedure(CallResponse: TCallResponse; PayLoad: TAgingSummary) of object;
    TScanSnapshots         = procedure(CallResponse: TCallResponse; CanGetAge: boolean; ReceivedTime: string) of object;
    TSendAccountDocument   = procedure(PayLoad: TSentDocument) of object;
    TSendUserFeedback      = procedure(CallResponse: TCallResponse) of object;
    TSetNewPassword        = procedure(CallResponse: TCallResponse) of object;
    TSetUserCompanyList    = procedure(CallResponse: TCallResponse) of object;
    TSubmitRating          = procedure(CallResponse: TCallResponse) of object;
    TUpdateAddressBook     = procedure(CallResponse: TCallResponse) of object;
    TUpdateRating          = procedure(CallResponse: TCallResponse) of object;


implementation


end.

