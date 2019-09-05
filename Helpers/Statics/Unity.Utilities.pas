unit Unity.Utilities;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    System.Classes,
    System.SysUtils;


type


    TUtilities = class abstract
    public
        class function LoadFileToStr(const FileName: TFileName): AnsiString; static;
    end;


implementation


class function TUtilities.LoadFileToStr(const FileName: TFileName): AnsiString;
begin

    var FileStream: TFileStream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    try

        if FileStream.Size > 0 then
        begin
            SetLength(Result, FileStream.Size);
            FileStream.Read(Pointer(Result)^, FileStream.Size);
        end;

    finally
        FileStream.Free;
    end;

end;


end.

