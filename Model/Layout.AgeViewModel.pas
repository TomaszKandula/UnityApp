unit Layout.AgeViewModel;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


    TDetails = class
    strict private
        var FNumber: integer;
        var FName:   string;
        var FWidth:  integer;
    public
        property Number: integer read FNumber write FNumber;
        property Name:   string  read FName   write FName;
        property Width:  integer read FWidth  write FWidth;
    end;


    TLayoutColumns = class
    strict private
        var FColumns: TArray<TDetails>;
    public
        constructor Create(Columns: cardinal = 0);
        destructor Destroy; override;
        property Columns: TArray<TDetails> read FColumns write FColumns;
    end;


implementation


constructor TLayoutColumns.Create(Columns: cardinal);
begin
    if Columns > 0 then SetLength(FColumns, Columns);
end;


destructor TLayoutColumns.Destroy();
begin
    for var Details: TDetails in FColumns do
        if Assigned(Details) then Details.Free();
end;


end.
