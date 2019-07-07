unit Unity.Arrays;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    System.Generics.Defaults;


type

    /// <remarks>
    /// Reference to two dimensional string array.
    /// </remarks>

    TALists = array of array of string;

    /// <remarks>
    /// Reference to one dimensional string array.
    /// </remarks>

    TAStrings = array of string;

    /// <remarks>
    /// Reference to one dimensional integer array.
    /// </remarks>

    TAIntigers = array of integer;

    /// <remarks>
    /// Reference to one dimensional double array.
    /// </remarks>

    TADoubles = array of double;

    /// <remarks>
    /// Encapsulated in a class, function that checks whether an item is contained in an array.
    /// </remarks>

    TArrayUtils<T> = class
    public
        class function Contains(const x: T; const anArray: array of T): boolean;
    end;


implementation


// -------------------------------------------------
// Usage: TArrayUtils<integer>.Contains(3, [1,2,3]).
// -------------------------------------------------

class function TArrayUtils<T>.Contains(const x: T; const anArray: array of T): boolean;
begin

    var y: T;
    var lComparer: IEqualityComparer<T>:=TEqualityComparer<T>.Default;

    for y in anArray do
        if lComparer.Equals(x, y) then
            Exit(True);

    Exit(False);

end;


end.

