unit Unity.Sorting;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Unity.Enums,
    Unity.Grid;


type


    TSorting = class abstract
        class procedure QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean); static;
        class procedure MergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol: integer; datatype: TDataType; ascending: boolean); static;
    end;


implementation


uses
    System.SysUtils,
    Unity.EventLogger;


class procedure TSorting.QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean);
begin

    // ----------------------------------------------------------------------------------------------------------------
    // 'A' variable holds numerical data to be sorted. 'L' variable is associated column with original list position.
    // The second associated column follows 'A' column, but it is not sorted. It allows to assign sorted values back to
    // original list position after computation is done. This is to be used when sorting is necessary before applaying
    // computation and after which we must put values back to its original positions.
    // ----------------------------------------------------------------------------------------------------------------

    var Lo:    integer;
    var Hi:    integer;
    var Pivot: double;
    var T1:    double;  // For sorting column
    var T2:    integer; // For associated column

    Lo:=iLo;
    Hi:=iHi;
    Pivot:=A[(Lo + Hi) div 2];

    repeat

        // Ascending
        if ASC then begin
            while A[Lo] < Pivot do Inc(Lo);
            while A[Hi] > Pivot do Dec(Hi);
        end;

        // Descending
        if not ASC then begin
            while A[Lo] > Pivot do Inc(Lo);
            while A[Hi] < Pivot do Dec(Hi);
        end;

        // Moving positions
        if Lo <= Hi then begin
            T1:=A[Lo];
            T2:=L[Lo];

            // Sorting column
            A[Lo]:= A[Hi];
            A[Hi]:= T1;

            // Associated column
            L[Lo]:= L[Hi];
            L[Hi]:= T2;

            // Move next
            Inc(Lo);
            Dec(Hi);

        end;

    until Lo > Hi;

    if Hi > iLo then QuickSort(A, L, iLo, Hi, ASC);
    if Lo < iHi then QuickSort(A, L, Lo, iHi, ASC);

end;


class procedure TSorting.MergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol: integer; datatype: TDataType; ascending: boolean);

    // Temporary shared local array for integers.
    var Avals:  array of integer;

    // Helper nested method for comparision.
    function compare(val1, val2: string): integer;
    begin

        case datatype of

            TDataType.TString: result:=ANSIComparetext(val1, val2);

            TDataType.TInteger:
            begin

                var int1: int64:=strtointdef(val1, 0);
                var int2: int64:=strtointdef(val2, 0);

                if (int1 > int2) then result:= 1
                else if int1 < int2 then result:= -1 else result:=0;

            end;

            TDataType.TFloat:
            begin

                var errcode: integer;
                var float1:  extended;
                var float2:  extended;

                val(val1, float1, errcode);

                if errcode <> 0 then float1:=0;

                val(val2, float2, errcode);

                if errcode <> 0 then float2:=0;

                if float1 > float2 then result:= 1
                else if float1 < float2 then result:= -1 else result:=0;

            end;

            else result:=0;

        end;
    end;

    // Heper nested merge method.
    procedure Merge(ALo, AMid, AHi: integer);
    begin

        var j: integer;
        var k: integer;
        var m: integer;
        var n: integer;

        var i: integer:=0;

        // Copy lower half of "vals" into temporary array "avals"
        SetLength(Avals, AMid - ALo + 1);
        for j:=ALo to AMid do begin
            AVals[i]:=Vals[j];
            inc(i);
        end;

        // Initialize
        i:=0;
        j:=AMid + 1;
        k:=ALo;

        // ----------------------------------------------------------------------------
        // Compare upper half to copied version of the lower half and move
        // the appropriate value (smallest for ascending, largest for descending) into
        // the lower half positions, for equals use 'avals' to preserve original order.
        // ----------------------------------------------------------------------------

        // Execute moving
        while ((k < j) and (j <= AHi)) do
        begin
            with grid do n:=compare(Cells[sortcol, Vals[j]], Cells[sortcol, AVals[i]]);
            if ascending and (n >= 0) or ((not ascending) and (n <= 0)) then
            begin
                Vals[k]:=AVals[i];
                inc(i);
                inc(k);
            end
            else
            begin
                Vals[k]:=Vals[j];
                inc(k);
                inc(j);
            end;
        end;

        // Copy any remaning, unsorted elements
        for m:=k to j - 1 do begin
            Vals[m]:=AVals[i];
            inc(i);
        end;

    end;

    // Recursively split the value into two pieces and merge them back together as we unwind the recursion.
    procedure PerformMergeSort(ALo, AHi:Integer);
    begin

        var AMid:Integer;

        if (ALo < AHi) then
        begin
            AMid:=(ALo + AHi) shr 1;
            PerformMergeSort(ALo, AMid);
            PerformMergeSort(AMid + 1, AHi);
            Merge(ALo, AMid, AHi);
        end;

    end;

begin
    PerformMergeSort(0, high(Vals));
end;


end.

