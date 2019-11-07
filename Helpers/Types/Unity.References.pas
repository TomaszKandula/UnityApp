unit Unity.References;

// ---------------------------------------------------------------------
// Extension unit for application.
// Can be referenced by anyone. Cannot hold references to View or Logic.
// We use records instead of classes because we only use them to group
// specific types of data ("variable of variables") to pass where it
// needs to be passed as a parameter. It acts usually as a pay load for
// both "request to" and "response from".
// ---------------------------------------------------------------------

interface


// -----------------------------------
// CAUTION! Do not use PACKED records.
// -----------------------------------


type

    /// <summary>
    /// Groups column numbers for given column names of open items presented in visual grid (TStringGrid).
    /// </summary>
    /// <remarks>
    /// This is necessary as column order may change.
    /// Normally we would use "ReturnColumn" extension method, but in case of multithreading, we must pre-set them before many threads
    /// use it at the same time (VCL components are not thread safe). Having record with fields simplify things.
    /// </remarks>
    TFOpenItemsRefs = record
        Ad1Col:       integer;
        Ad2Col:       integer;
        Ad3Col:       integer;
        PnoCol:       integer;
        PAreaCol:     integer;
        CuidCol:      integer;
        OpenAmCol:    integer;
        PmtStatCol:   integer;
        CtrlCol:      integer;
        InvoNoCol:    integer;
        ValDtCol:     integer;
        DueDtCol:     integer;
        ISOCol:       integer;
        CurAmCol:     integer;
        OpenCurAmCol: integer;
        Text:         integer;
    end;

    /// <summary>
    /// Groups column numbers for given column names of control status presented in visual grid (TStringGrid).
    /// </summary>
    /// <remarks>
    /// This is necessary as column order may change.
    /// Normally we would use "ReturnColumn" extension method, but in case of multithreading, we must pre-set them before many threads
    /// use it at the same time (VCL components are not thread safe). Having record with fields simplify things.
    /// </remarks>
    TFCtrlStatusRefs = record
        Id:          integer;
        Code:        integer;
        Text:        integer;
        Description: integer;
    end;


implementation


end.
