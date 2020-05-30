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
    strict private
        class procedure FMergeSort(Grid: TStringGrid; var Vals: array of integer; sortcol: integer; datatype: TDataType; ascending: boolean); static;
    public
        class procedure QuickSort(var A: array of double; var L: array of integer; iLo, iHi: integer; ASC: boolean); static;
        class procedure MergeSort(var SourceGrid: TStringGrid; const SortCol: integer; const DataType: TDataType; const Ascending: boolean); static;
    end;


implementation


uses
    System.SysUtils;


class procedure TSorting.FMergeSort(Grid: TStringGrid; var Vals: array of integer; SortCol: integer; DataType: TDataType; Ascending: boolean);

    // Temporary shared local array for integers.
    var AVals:  array of integer;

    // Helper nested method for comparision.
    function Compare(Val1, Val2: string): integer;
    begin

        case DataType of

            TDataType.TString: Result:=ANSIComparetext(Val1, Val2);

            TDataType.TInteger:
            begin

                var Int1: int64:=StrToIntDef(val1, 0);
                var Int2: int64:=StrToIntDef(val2, 0);

                if (Int1 > Int2) then Result:= 1
                else if Int1 < Int2 then Result:= -1 else Result:=0;

            end;

            TDataType.TFloat:
            begin

                var ErrCode: integer;
                var Float1:  extended;
                var Float2:  extended;

                Val(Val1, Float1, ErrCode);

                if ErrCode <> 0 then Float1:=0;

                Val(Val2, Float2, ErrCode);

                if ErrCode <> 0 then Float2:=0;

                if Float1 > Float2 then Result:= 1
                else if Float1 < Float2 then Result:= -1 else Result:=0;

            end;

            else Result:=0;

        end;

    end;

    // Heper nested merge method.
    procedure Merge(ALo: integer; AMid: integer; AHi: integer);
    begin

        var j: integer;
        var k: integer;
        var m: integer;
        var n: integer;

        var i:=0;

        // Copy lower half of "vals" into temporary array "avals"
        SetLength(Avals, AMid - ALo + 1);

        for j:=ALo to AMid do
        begin
            AVals[i]:=Vals[j];
            Inc(i);
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

            with Grid do
                n:=compare(Cells[SortCol, Vals[j]], Cells[SortCol, AVals[i]]);

            if ascending and (n >= 0) or ((not ascending) and (n <= 0)) then
            begin
                Vals[k]:=AVals[i];
                Inc(i);
                Inc(k);
            end
            else
            begin
                Vals[k]:=Vals[j];
                Inc(k);
                Inc(j);
            end;

        end;

        // Copy any remaning, unsorted elements
        for m:=k to j - 1 do
        begin
            Vals[m]:=AVals[i];
            Inc(i);
        end;

    end;

    // Recursively split the value into two pieces and merge them back together as we unwind the recursion.
    procedure PerformMergeSort(ALo: integer; AHi: integer);
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
    PerformMergeSort(0, High(Vals));
end;


class procedure TSorting.MergeSort(var SourceGrid: TStringGrid; const SortCol: integer; const DataType: TDataType; const Ascending: boolean);
begin

    var List: TArray<integer>;
    var TempGrid: TStringGrid:=TStringGrid.create(nil);
    try

        TempGrid.RowCount :=SourceGrid.RowCount;
        TempGrid.ColCount :=SourceGrid.ColCount;
        TempGrid.FixedRows:=SourceGrid.FixedRows;
        SetLength(List, SourceGrid.RowCount - SourceGrid.FixedRows);

        for var Index:=SourceGrid.FixedRows to SourceGrid.RowCount - 1 do
        begin
            List[Index - SourceGrid.FixedRows]:=Index;
            TempGrid.Rows[Index].Assign(SourceGrid.Rows[Index]);
        end;

        TSorting.FMergeSort(SourceGrid, List, SortCol, DataType, Ascending);

        for var Index:=0 to SourceGrid.RowCount - SourceGrid.FixedRows - 1 do
            SourceGrid.Rows[Index + SourceGrid.FixedRows].Assign(TempGrid.Rows[List[Index]]);

        SourceGrid.Row:=SourceGrid.FixedRows;

    finally
        TempGrid.Free();
    end;

    SetLength(List, 0);

end;


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


end.

