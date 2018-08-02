
{$I .\Include\Header.inc}

unit Arrays;

interface

    /// <summary>
    ///     This class defines different array types.
    /// </summary>

type

    /// <remarks>
    ///     Reference to two dimensional string array.
    /// </remarks>

    TLists    = array of array of string;

    /// <remarks>
    ///     Reference to one dimensional string array.
    /// </remarks>

    TStrings  = array of string;

    /// <remarks>
    ///     Reference to one dimensional integer array.
    /// </remarks>

    TIntigers = array of integer;

implementation

    // Leave it empty.

end.
