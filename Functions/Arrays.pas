
{$I .\Include\Header.inc}

unit Arrays;


interface

    /// <summary>
    /// This class defines different array and record types.
    /// </summary>

type

    /// <remarks>
    /// Reference to two dimensional string array.
    /// </remarks>

    TLists = array of array of string;

    /// <remarks>
    /// Reference to one dimensional string array.
    /// </remarks>

    TStrings = array of string;

    /// <remarks>
    /// Reference to one dimensional integer array.
    /// </remarks>

    TIntigers = array of integer;

    /// <remarks>
    /// These records definition allows to hold column numbers for given column name. This is necessary as column order may change.
    /// Normally we would use "ReturnColumn" extension method, but in case of multithreading, we must preset
    /// them before many threads use them at the same time (VCL components are not thread safe).
    /// </remarks>

    TOpenItemsRefs = record
        Ad1Col:         integer;
        Ad2Col:         integer;
        Ad3Col:         integer;
        PnoCol:         integer;
        PAreaCol:       integer;
        CuidCol:        integer;
        OpenAmCol:      integer;
        PmtStatCol:     integer;
        CtrlCol:        integer;
        InvoNoCol:      integer;
        ValDtCol:       integer;
        DueDtCol:       integer;
        ISOCol:         integer;
        CurAmCol:       integer;
        OpenCurAmCol:   integer;
    end;

    TControlStatusRefs = record
        Id:             integer;
        Code:           integer;
        Text:           integer;
        Description:    integer;
    end;



implementation


    // Leave implementation section empty


end.

