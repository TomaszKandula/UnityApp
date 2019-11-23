unit Unity.Arrays;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    System.Generics.Defaults;


type

    /// <summary>
    /// Reference to two dimensional string array.
    /// </summary>
    TALists = array of array of string;

    /// <summary>
    /// Reference to one dimensional string array.
    /// </summary>
    TAStrings = array of string;

    /// <summary>
    /// Reference to one dimensional integer array.
    /// </summary>
    TAIntigers = array of integer;

    /// <summary>
    /// Reference to one dimensional double array.
    /// </summary>
    TADoubles = array of double;

    /// <summary>
    /// Encapsulated in a class, function that checks whether an item is contained in an array.
    /// </summary>
    TArrayUtils<T> = class
    public
        class function Contains(const x: T; const anArray: array of T): boolean;
    end;


implementation


class function TArrayUtils<T>.Contains(const x: T; const anArray: array of T): boolean;
begin

    // Usage: TArrayUtils<integer>.Contains(3, [1,2,3]).
    var y: T;
    var lComparer: IEqualityComparer<T>:=TEqualityComparer<T>.Default;

    for y in anArray do
        if lComparer.Equals(x, y) then
            Exit(True);

    Exit(False);

end;


end.

