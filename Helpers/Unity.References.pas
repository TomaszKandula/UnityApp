unit Unity.References;

// ---------------------------------------------------------------------
// Extension unit for application.
// Can be referenced by anyone. Cannot hold references to View or Logic.
// We use records instead of classes because we only use them to group
// specific types of data ("variable of variables") to pass where it
// needs to be passed as a parameter. It acts usually as a payload for
// both "request to" and "response from"; but do not overuse records.
// ---------------------------------------------------------------------

interface


uses
    Unity.Grid;


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
        CoCodeCol:    integer;
        CustNumCol:   integer;
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
        procedure InitWith(SourceGrid: TStringGrid);
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
        procedure InitWith(SourceGrid: TStringGrid);
    end;


implementation


uses
    Api.ReturnControlStatus,
    Api.ReturnOpenItems,
    Api.ControlStatusFields;


procedure TFOpenItemsRefs.InitWith(SourceGrid: TStringGrid);
begin
    // ---------------------------------------------------------------------------
    // Get column reference on demand for Open Items string grid. The reason is,
    // despite we do not change columns order at run time programatically, it
    // may be changed on server-side and that will be immediatelly reflected in
    // Open Items string grid that serves the user and the application as the
    // source of data. Additional purpose of the code is - to get the columns
    // at once instead using ReturnColumn multiple times in given method,
    // this increase the overall performance of the code and decreases complexity.
    // ---------------------------------------------------------------------------
    // The nature of open items is that, it changes continuously, but due to ERP
    // database workload during the day we have decided to update the data in
    // Open Items table few times a day (on regular basis).
    // ---------------------------------------------------------------------------
    CoCodeCol   :=SourceGrid.GetCol(TReturnOpenItems._SourceDbName);
    CustNumCol  :=SourceGrid.GetCol(TReturnOpenItems._CustNumber);
    OpenAmCol   :=SourceGrid.GetCol(TReturnOpenItems._OpenAmount);
    PmtStatCol  :=SourceGrid.GetCol(TReturnOpenItems._PmtStatus);
    CtrlCol     :=SourceGrid.GetCol(TReturnOpenItems._ControlStatus);
    InvoNoCol   :=SourceGrid.GetCol(TReturnOpenItems._InvoiceNumber);
    ValDtCol    :=SourceGrid.GetCol(TReturnOpenItems._ValueDate);
    DueDtCol    :=SourceGrid.GetCol(TReturnOpenItems._DueDate);
    ISOCol      :=SourceGrid.GetCol(TReturnOpenItems._Iso);
    CurAmCol    :=SourceGrid.GetCol(TReturnOpenItems._CurAmount);
    OpenCurAmCol:=SourceGrid.GetCol(TReturnOpenItems._OpenCurAmount);
    Ad1Col      :=SourceGrid.GetCol(TReturnOpenItems._Address1);
    Ad2Col      :=SourceGrid.GetCol(TReturnOpenItems._Address2);
    Ad3Col      :=SourceGrid.GetCol(TReturnOpenItems._Address3);
    PnoCol      :=SourceGrid.GetCol(TReturnOpenItems._PostalNumber);
    PAreaCol    :=SourceGrid.GetCol(TReturnOpenItems._PostalArea);
    Text        :=SourceGrid.GetCol(TReturnOpenItems._Text);
end;


procedure TFCtrlStatusRefs.InitWith(SourceGrid: TStringGrid);
begin
    // -----------------------------------------------------------------------
    // Get column reference of Control Status table located in General Tables.
    // Similarly to the "UpdateFOpenItemsRefs" method,
    // we use it to decrease level of usage of ReturnColumn method.
    // -----------------------------------------------------------------------
    Id         :=SourceGrid.GetCol(TControlStatusFields._Id);
    Code       :=SourceGrid.GetCol(TControlStatusFields._Code);
    Text       :=SourceGrid.GetCol(TControlStatusFields._Text);
    Description:=SourceGrid.GetCol(TControlStatusFields._Description);
end;


end.

