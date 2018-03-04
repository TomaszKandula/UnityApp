{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Invoices;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Grids, ADODB, ComCtrls, Main;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TInvoicesForm = class(TForm)
    InvoicesGrid: TStringGrid;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  InvoicesForm: TInvoicesForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  SQL, Settings;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TInvoicesForm.FormCreate(Sender: TObject);
var
  AppSettings:  TSettings;
begin
  AppSettings:=TSettings.Create;
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  InvoicesForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_INVOICES', APPNAME);
  FreeAndNil(AppSettings);
  { ----------------------------------------------------------------------------------------------------------------------------- 'STRINGGRID' INITIALIZATION }
  InvoicesGrid.RowCount:=2;
  InvoicesGrid.ColCount:=4;
  { HIDE UNNECESSARY COLUMNS }
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TInvoicesForm.FormShow(Sender: TObject);
begin
  InvoicesGrid.ClearAll(2, 1, 1, False);
  InvoicesGrid.AutoThumbSize;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON RESIZE }
procedure TInvoicesForm.FormResize(Sender: TObject);
begin
  InvoicesGrid.AutoThumbSize;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE }
procedure TInvoicesForm.FormActivate(Sender: TObject); // refactor!! make model for tbl_invoices
var
  MSSQL:  TMSSQL;
  CUID :  string;
begin
  { CHECK DATABASE CONNECTION AND PROCEED ACCORDINGLY }
  if DataBase.LastError = 0 then
  begin
    Sleep(100);
    MSSQL:=TMSSQL.Create(Database.ADOConnect);
    try
      { PREPARE FOR QUERY }
      CUID:=MainForm.sgInvoiceTracker.Cells[MainForm.sgInvoiceTracker.ReturnColumn('CUID', 1, 1), MainForm.sgInvoiceTracker.Row];
      MSSQL.StrSQL:='SELECT INVOICENO, INVOICESTATE, STAMP FROM tbl_invoices WHERE CUID = ' + QuotedStr(CUID);
      InvoicesGrid.Freeze(True);
      { CLEAR, QUERY AND SHOW DATA IN STRING GRID }
      MSSQL.SqlToGrid(InvoicesGrid, MSSQL.ExecSQL, False);
      InvoicesGrid.Freeze(False);
    finally
      { DISPOSE }
      MSSQL.Free;
    end;
  end else
  begin
    StatusBar.SimpleText:='Cannot connect with database. Please contact IT support.';
  end;
end;

{ -------------------------------------------------------- ! COMPONENT EVENTS | GRID ! ---------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------------- DRAW SELECTED ROW }
procedure TInvoicesForm.InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  InvoicesGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

{ ------------------------------------------------------------ ! KEYBOARD EVENTS ! -------------------------------------------------------------------------- }

procedure TInvoicesForm.InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then InvoicesGrid.CopyCutPaste(1);
end;

end.
