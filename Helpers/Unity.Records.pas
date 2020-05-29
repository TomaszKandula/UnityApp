unit Unity.Records;

// ---------------------------------------------------------------------
// Extension unit for application.
// Can be referenced by anyone. Cannot hold references to View or Logic.
// We use records instead of classes because we only use them to group
// specific types of data ("variable of variables") to pass where it
// needs to be passed as a parameter. It acts usually as a payload for
// both "request to" and "response from"; but do not overuse records.
// ---------------------------------------------------------------------

interface


uses
    Unity.Enums,
    Unity.Grid,
    Unity.ListView,
    Api.BankDetails;


// -----------------------------------
// CAUTION! Do not use PACKED records.
// -----------------------------------


type


    /// <summary>
    /// Group of variabes that carries information on column filtering.
    /// </summary>
    TColumnData = record
        ColumnName:   string;
        ColumnNumber: integer;
        IsFiltered:   boolean;
        UniqueItems:  TArray<TArray<string>>;
    end;

    /// <summary>
    /// Group of variables that carries last response that have been returned after processing
    /// given requested. Returned Code field is present for failed REST calls (status code returned).
    /// </summary>
    TCallResponse = record
        LastMessage:  string;
        ErrorCode:    string;
        IsSucceeded:  boolean;
        ReturnedCode: integer;
    end;

    /// <summary>
    /// Group of variables that carries information about latest available release of the Unity Platform.
    /// </summary>
    TClientInfo = record
        Version: string;
        Date:    string;
        Status:  string;
    end;

    /// <summary>
    /// Group of variables that carries user rating with optional comment.
    /// </summary>
    TRating = record
        UserRating: integer;
        UserComment: string;
    end;

    /// <summary>
    /// Group of variables that carries user data per session (from Active Directory).
    /// </summary>
    TSessionData = record
        UnityUserId:  integer;
        Department:   string;
        AliasName:    string;
        DisplayName:  string;
        EmailAddress: string;
    end;

    /// <summary>
    /// Carries a group of variables to be received back when awaited async. task is done.
    /// </summary>
    TCustomerDetails = record
        Id:              integer;
        SourceDBName:    string;
        CustomerNumber:  Int64;
        CustomerName:    string;
        ContactPerson:   string;
        RegularEmails:   string;
        StatementEmails: string;
        PhoneNumbers:    string;
    end;

    /// <summary>
    /// Carries a group of variables for DailyComments/GeneralComments table operations.
    /// </summary>
    TCommentExists = record
        DoesCommentExists: boolean;
        CommentId:         integer;
        UserComment:       string;
    end;

    /// <summary>
    /// Carries a group of variables for DailyComments table operations.
    /// </summary>
    TDailyCommentFields = record
        CommentId:            integer;
        SourceDBName:         string;
        CustomerNumber:       integer;
        AgeDate:              string;
        CallEvent:            integer;
        CallDuration:         integer;
        FixedStatementsSent:  integer;
        CustomStatementsSent: integer;
        FixedRemindersSent:   integer;
        CustomRemindersSent:  integer;
        UserComment:          string;
        UserAlias:            string;
        EntryDateTime:        string;
    end;

    /// <summary>
    /// Carries a group of variables for updating FollowUp field stored in GeneralComment table.
    /// </summary>
    TFollowUpsPayLoad = record
        SourceDBNames:   TArray<string>;
        CustomerNumbers: TArray<Int64>;
        FollowUps:       TArray<string>;
        procedure Initialize(Count: cardinal);
    end;

    /// <summary>
    /// Carries a group of variables for updating Free1..3 fields stored in GeneralComment table.
    /// </summary>
    TFreeFieldsPayLoad = record
        SourceDBNames:   TArray<string>;
        CustomerNumbers: TArray<Int64>;
        Free1:           TArray<string>;
        Free2:           TArray<string>;
        Free3:           TArray<string>;
        procedure Initialize(Count: cardinal);
    end;

    /// <summary>
    /// Carries a group of variables for GeneralComment table.
    /// </summary>
    TGeneralCommentFields = record
        CommentId:      integer;
        SourceDBName:   string;
        CustomerNumber: integer;
        FollowUp:       string;
        Free1:          string;
        Free2:          string;
        Free3:          string;
        UserComment:    string;
        UserAlias:      string;
    end;

    /// <summary>
    /// Carries a group of variables of Risk Classes.
    /// </summary>
    TRiskClassGroup = record
        Class_A: double;
        Class_B: double;
        Class_C: double;
    end;

    /// <summary>
    /// Carries a group of variables for update ageing summary.
    /// </summary>
    TAgingSummary = record
        AgeDate:     string;
        CustAll:     integer;
        ANotDue:     extended;
        ARange1:     extended;
        ARange2:     extended;
        ARange3:     extended;
        ARange4:     extended;
        ARange5:     extended;
        ARange6:     extended;
        Balance:     extended;
        Limits:      extended;
        Exceeders:   integer;
        TotalExceed: extended;
        RCA:         extended;
        RCB:         extended;
        RCC:         extended;
        RCAcount:    cardinal;
        RCBcount:    cardinal;
        RCCcount:    cardinal;
    end;

    /// <summary>
    /// Carries a group of variables for open items summary with ledger currency and other currency.
    /// </summary>
    TOpenItemsTotal = record
        OpenAm:    double;
        Am:        double;
        OpenCurAm: double;
        CurAm:     double;
    end;


implementation


procedure TFollowUpsPayLoad.Initialize(Count: cardinal);
begin
    SetLength(SourceDBNames, Count);
    SetLength(CustomerNumbers, Count);
    SetLength(FollowUps, Count);
end;


procedure TFreeFieldsPayLoad.Initialize(Count: cardinal);
begin
    SetLength(SourceDBNames, Count);
    SetLength(CustomerNumbers, Count);
    SetLength(Free1, Count);
    SetLength(Free2, Count);
    SetLength(Free3, Count);
end;


end.

