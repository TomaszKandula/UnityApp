unit Invoices;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Grids,
    Vcl.ComCtrls,
    Data.Win.ADODB,
    InterposerClasses;


type


    TInvoicesForm = class(TForm)
        InvoicesGrid: TStringGrid;
        StatusBar: TStatusBar;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    end;


var
    InvoicesForm: TInvoicesForm;


Implementation


uses
    Main,
    Settings,
    SqlHandler,
    DbModel,
    Helpers;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TInvoicesForm.FormCreate(Sender: TObject);
begin
    InvoicesGrid.RowCount:=2;
    InvoicesGrid.ColCount:=4;
    InvoicesGrid.SetRowHeight(InvoicesGrid.sgRowHeight, 25);
end;


procedure TInvoicesForm.FormShow(Sender: TObject);
begin
    InvoicesGrid.ClearAll(2, 1, 1, False);
    InvoicesGrid.SetColWidth(10, 20, 400);
    InvoicesGrid.AutoThumbSize;
end;


procedure TInvoicesForm.FormResize(Sender: TObject);
begin
    InvoicesGrid.AutoThumbSize;
end;


procedure TInvoicesForm.FormActivate(Sender: TObject);
var
    Tables: TDataTables;
    CUID :  string;
begin

    Tables:=TDataTables.Create(MainForm.DbConnect);
    InvoicesGrid.Freeze(True);

    try
        CUID:=MainForm.sgInvoiceTracker.Cells[MainForm.sgInvoiceTracker.ReturnColumn(TTrackerData.Cuid, 1, 1), MainForm.sgInvoiceTracker.Row];
        Tables.StrSQL:=TSql.SELECT                             +
                            TTrackerInvoices.InvoiceNo    + TUChars.COMMA +
                            TTrackerInvoices.InvoiceState + TUChars.COMMA +
                            TTrackerInvoices.Stamp        +
                        TSql.FROM                              +
                            TTrackerInvoices.TrackerInvoices +
                        TSql.WHERE                                +
                            TTrackerInvoices.Cuid            +
                        TSql.EQUAL                                +
                            QuotedStr(CUID);
        Tables.SqlToGrid(InvoicesGrid, Tables.ExecSQL, False, True);
        InvoicesGrid.Freeze(False);
    finally
        Tables.Free;
    end;

end;


// ---------------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS //


procedure TInvoicesForm.InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  InvoicesGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TUnityApp.SELCOLOR, clBlack, clWhite, True);
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TInvoicesForm.InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then InvoicesGrid.CopyCutPaste(TEnums.TActionTask.adCopy);
end;


procedure TInvoicesForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


end.

