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
    Unity.Grid,
    Api.OpenItemsFields;


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
        InvoNoCol:    integer;
        Text:         integer;
        AddText:      integer;
        OpenAmCol:    integer;
        Amount:       integer;
        OpenCurAmCol: integer;
        CurAmCol:     integer;
        ISOCol:       integer;
        DueDtCol:     integer;
        ValDtCol:     integer;
        CtrlCol:      integer;
        PmtStatCol:   integer;
        Ad1Col:       integer;
        Ad2Col:       integer;
        Ad3Col:       integer;
        PnoCol:       integer;
        PAreaCol:     integer;
        CoCodeCol:    integer;
        CustNumCol:   integer;
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
    // cached source data. Additional purpose of the code is - to get the columns
    // at once instead using ReturnColumn multiple times in given method,
    // this increase the overall performance of the code and decreases complexity.
    // ---------------------------------------------------------------------------
    // The nature of open items is that, it changes continuously, but due to ERP
    // database workload during the day we have decided to update the data in
    // Open Items table few times a day (on regular basis).
    // ---------------------------------------------------------------------------
    InvoNoCol   :=SourceGrid.GetCol(TOpenItemsFields._InvoiceNumber);
    Text        :=SourceGrid.GetCol(TOpenItemsFields._Text);
    AddText     :=SourceGrid.GetCol(TOpenItemsFields._AdditionalText);
    OpenAmCol   :=SourceGrid.GetCol(TOpenItemsFields._OpenAmount);
    Amount      :=SourceGrid.GetCol(TOpenItemsFields._Amount);
    OpenCurAmCol:=SourceGrid.GetCol(TOpenItemsFields._OpenCurAmount);
    CurAmCol    :=SourceGrid.GetCol(TOpenItemsFields._CurAmount);
    ISOCol      :=SourceGrid.GetCol(TOpenItemsFields._Iso);
    DueDtCol    :=SourceGrid.GetCol(TOpenItemsFields._DueDate);
    ValDtCol    :=SourceGrid.GetCol(TOpenItemsFields._ValueDate);
    CtrlCol     :=SourceGrid.GetCol(TOpenItemsFields._ControlStatus);
    PmtStatCol  :=SourceGrid.GetCol(TOpenItemsFields._PmtStatus);
    Ad1Col      :=SourceGrid.GetCol(TOpenItemsFields._Address1);
    Ad2Col      :=SourceGrid.GetCol(TOpenItemsFields._Address2);
    Ad3Col      :=SourceGrid.GetCol(TOpenItemsFields._Address3);
    PnoCol      :=SourceGrid.GetCol(TOpenItemsFields._PostalNumber);
    PAreaCol    :=SourceGrid.GetCol(TOpenItemsFields._PostalArea);
    CoCodeCol   :=SourceGrid.GetCol(TOpenItemsFields._SourceDbName);
    CustNumCol  :=SourceGrid.GetCol(TOpenItemsFields._CustNumber);
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

