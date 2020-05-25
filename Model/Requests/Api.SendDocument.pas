unit Api.SendDocument;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


uses
    Api.DocumentFields;


type


    TSendDocument = class
    strict private
        var FLayoutType:      string;
        var FReportedAgeDate: string;
        var FSubject:         string;
        var FMessage:         string;
        var FUserEmail:       string;
        var FInvoiceFilter:   string;
        var FBeginDate:       string;
        var FEndDate:         string;
        var FIsCtrlStatus:    boolean;
        var FIsUserInCopy:    boolean;
        var FIsSourceInCopy:  boolean;
        var FDocuments: TArray<TDocumentFields>;
    public
        const _LayoutType      = 'Layout Type';
        const _ReportedAgeDate = 'Reported Age Date';
        const _Subject         = 'Subject';
        const _Message         = 'Message';
        const _UserEmail       = 'User Email';
        const _InvoiceFilter   = 'Invoice Filter';
        const _BeginDate       = 'Begin Date';
        const _EndDate         = 'End Date';
        const _IsCtrlStatus    = 'IsCtrlStatus';
        const _IsUserInCopy    = 'IsUserInCopy';
        const _IsSourceInCopy  = 'IsSourceInCopy';
        constructor Create(Count: cardinal);
        destructor Destroy(); override;
        property LayoutType:      string  read FLayoutType      write FLayoutType;
        property ReportedAgeDate: string  read FReportedAgeDate write FReportedAgeDate;
        property Subject:         string  read FSubject         write FSubject;
        property &Message:        string  read FMessage         write FMessage;
        property UserEmail:       string  read FUserEmail       write FUserEmail;
        property InvoiceFilter:   string  read FInvoiceFilter   write FInvoiceFilter;
        property BeginDate:       string  read FBeginDate       write FBeginDate;
        property EndDate:         string  read FEndDate         write FEndDate;
        property IsCtrlStatus:    boolean read FIsCtrlStatus    write FIsCtrlStatus;
        property IsUserInCopy:    boolean read FIsUserInCopy    write FIsUserInCopy;
        property IsSourceInCopy:  boolean read FIsSourceInCopy  write FIsSourceInCopy;
        property Documents: TArray<TDocumentFields> read FDocuments write FDocuments;
    end;


implementation


constructor TSendDocument.Create(Count: cardinal);
begin
    SetLength(FDocuments, Count);
    for var Index:=0 to Count - 1 do FDocuments[Index]:=TDocumentFields.Create();
end;


destructor TSendDocument.Destroy();
begin

    for var Documents: TDocumentFields in FDocuments do
        if Assigned(Documents) then Documents.Free();

    inherited;

end;


end.
