
{$I .\Include\Header.inc}

unit Invoices;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Grids, ADODB, ComCtrls, InterposerClasses;


type

    /// <summary>
    ///     View form class with helpers for displaying invoices that has been automatically sent to the customer by Invoice Tracer.
    /// </summary>

    TInvoicesForm = class(TForm)
        InvoicesGrid: TStringGrid;
        StatusBar: TStatusBar;
        procedure FormCreate(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    end;

var
    InvoicesForm: TInvoicesForm;


Implementation


uses
    Main, Settings, SQL, Model;

{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------ START UP //


procedure TInvoicesForm.FormCreate(Sender: TObject);
var
    Settings:  ISettings;
begin
    Settings:=TSettings.Create;
    InvoicesForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_INVOICES', APPCAPTION);
    InvoicesGrid.RowCount:=2;
    InvoicesGrid.ColCount:=4;
    InvoicesGrid.SetRowHeight(sgRowHeight, 25);
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
        CUID:=MainForm.sgInvoiceTracker.Cells[MainForm.sgInvoiceTracker.ReturnColumn(TTracker.CUID, 1, 1), MainForm.sgInvoiceTracker.Row];
        Tables.StrSQL:=SELECT                      +
                            TInvoices.INVOICENO    + COMMA +
                            TInvoices.INVOICESTATE + COMMA +
                            TInvoices.STAMP        +
                        FROM                       +
                            TblInvoices            +
                        WHERE                      +
                            TInvoices.CUID         +
                        EQUAL                      +
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
  InvoicesGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TInvoicesForm.InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then InvoicesGrid.CopyCutPaste(adCopy);
end;

procedure TInvoicesForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = ESC then Close;
end;


end.
