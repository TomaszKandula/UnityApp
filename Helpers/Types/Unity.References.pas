unit Unity.References;

// ---------------------------------------------------------------------
// Extension unit for application.
// Can be referenced by anyone. Cannot hold references to View or Logic.
// We use records instead of classes because we only use them to group
// specific types of data ("variable of variables") to pass where it
// needs to be passed as a parameter. It acts usually as a pay load for
// both "request to" and "response from"; but do not overuse records.
// ---------------------------------------------------------------------

interface


uses
    DbModel,
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
    CoCodeCol   :=SourceGrid.GetCol(DbModel.TOpenitems.SourceDBName);
    CustNumCol  :=SourceGrid.GetCol(DbModel.TOpenitems.CustNo);
    OpenAmCol   :=SourceGrid.GetCol(DbModel.TOpenitems.OpenAm);
    PmtStatCol  :=SourceGrid.GetCol(DbModel.TOpenitems.PmtStat);
    CtrlCol     :=SourceGrid.GetCol(DbModel.TOpenitems.Ctrl);
    InvoNoCol   :=SourceGrid.GetCol(DbModel.TOpenitems.InvoNo);
    ValDtCol    :=SourceGrid.GetCol(DbModel.TOpenitems.ValDt);
    DueDtCol    :=SourceGrid.GetCol(DbModel.TOpenitems.DueDt);
    ISOCol      :=SourceGrid.GetCol(DbModel.TOpenitems.ISO);
    CurAmCol    :=SourceGrid.GetCol(DbModel.TOpenitems.CurAm);
    OpenCurAmCol:=SourceGrid.GetCol(DbModel.TOpenitems.OpenCurAm);
    Ad1Col      :=SourceGrid.GetCol(DbModel.TOpenitems.Ad1);
    Ad2Col      :=SourceGrid.GetCol(DbModel.TOpenitems.Ad2);
    Ad3Col      :=SourceGrid.GetCol(DbModel.TOpenitems.Ad3);
    PnoCol      :=SourceGrid.GetCol(DbModel.TOpenitems.Pno);
    PAreaCol    :=SourceGrid.GetCol(DbModel.TOpenitems.PArea);
    Text        :=SourceGrid.GetCol(DbModel.TOpenitems.Txt);
end;


procedure TFCtrlStatusRefs.InitWith(SourceGrid: TStringGrid);
begin
    // -----------------------------------------------------------------------
    // Get column reference of Control Status table located in General Tables.
    // Similarly to the "UpdateFOpenItemsRefs" method,
    // we use it to decrease level of usage of ReturnColumn method.
    // -----------------------------------------------------------------------
    Id         :=SourceGrid.GetCol(TControlStatus.Id);
    Code       :=SourceGrid.GetCol(TControlStatus.Code);
    Text       :=SourceGrid.GetCol(TControlStatus.Text);
    Description:=SourceGrid.GetCol(TControlStatus.Description);
end;


end.

