{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// SUPPLIER FORM DEMO ! TO BE REMOVED !

unit TicketList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, Grids, Main;

{ ------------------------------------------------------------------ ! MAIN CLASS ! ------------------------------------------------------------------------- }
type
  TTicketForm = class(TForm)
    AppMain: TShape;
    btnSelect: TSpeedButton;
    btnCancel: TSpeedButton;
    sgTicketList: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  TicketForm: TTicketForm;

{ ------------------------------------------------------------ ! IMPLEMENTATION ZONE ! ---------------------------------------------------------------------- }

implementation

uses
  Settings, Supplier, Model;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ ------------------------------------------------------------- ! EXECUTE ON CREATE ! ----------------------------------------------------------------------- }
procedure TTicketForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { ----------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
    TicketForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_TICKETLIST', APPNAME);
  finally
    AppSettings.Free;
  end;
  { HIDE FEW COLUMNS }
  sgTicketList.ColWidths[3]:=-1;
  sgTicketList.ColWidths[4]:=-1;
  sgTicketList.ColWidths[5]:=-1;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TTicketForm.FormShow(Sender: TObject);
var
  Vendor: TSupplierForm;
begin
  Vendor:=TSupplierForm.Create(MainForm.DbConnect);
  try
    if not(Vendor.SqlToGrid(sgTicketList, Vendor.GetAllOpenPending, False, True)) then
    begin
      MainForm.MsgCall(mcWarn, 'The are no tickets on the list to approve/reject.');
      Close;
    end
    else
    begin
      sgTicketList.SetColWidth(10, 30, 400);
    end;
  finally
    Vendor.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- PROCESS GIVEN TICKET }
procedure TTicketForm.btnSelectClick(Sender: TObject);
var
  Vendor:          TSupplierForm;
  TicketIdRef:     string;
  LegalEntityRef:  string;
  CurrencyRef:     string;
begin
  { GET THE SELECTED TICKET NUMBER REFERENCE }
  MainForm.TextSelectedTicket.Caption:=sgTicketList.Cells[sgTicketList.ReturnColumn(TSupplierRequest.TicketNumber, 1, 1), sgTicketList.Row];
  TicketIdRef:=sgTicketList.Cells[sgTicketList.ReturnColumn(TSupplierData.TicketNumberRef, 1, 1), sgTicketList.Row];
  LegalEntityRef:=sgTicketList.Cells[sgTicketList.ReturnColumn(TSupplierRequest.LegalEntityRef, 1, 1), sgTicketList.Row];
  CurrencyRef:=sgTicketList.Cells[sgTicketList.ReturnColumn(TSupplierRequest.CurrencyRef, 1, 1), sgTicketList.Row];
  Vendor:=TSupplierForm.Create(MainForm.DbConnect);
  try
    { GET DATA }
    Vendor.GetSelectedTicket(TicketIdRef, LegalEntityRef, CurrencyRef);
    { FILL ALL THE FIELDS }
    if Vendor.DataSet.RecordCount = 1 then
    begin
      { SUPPLIER REQUEST }
      MainForm.edtUserAlias.Text   :=Vendor.DataSet.Fields[TSupplierRequest.UserAlias].Value;
      MainForm.edtTerms.Text       :=Vendor.DataSet.Fields[TSupplierRequest.PaymentTerm].Value;
      MainForm.ReadAddComment.Text :=Vendor.DataSet.Fields[TSupplierRequest.AddComment].Value;
      { SUBQUERIED COLUMNS }
      MainForm.edtCompany.Text     :=Vendor.DataSet.Fields['CompanyName'].Value;
      MainForm.edtAgent.Text       :=Vendor.DataSet.Fields['CompanyAgent'].Value;
      MainForm.edtCurrency.Text    :=Vendor.DataSet.Fields['Currency'].Value;
      { SUPPLIER DATA }
      MainForm.edtCustomerName.Text:=Vendor.DataSet.Fields[TSupplierData.CustomerName].Value;
      MainForm.edtAddress.Text     :=Vendor.DataSet.Fields[TSupplierData.AddressLine].Value;
      MainForm.edtTown.Text        :=Vendor.DataSet.Fields[TSupplierData.Town].Value;
      MainForm.edtCountry.Text     :=Vendor.DataSet.Fields[TSupplierData.Country].Value;
      MainForm.edtPostal.Text      :=Vendor.DataSet.Fields[TSupplierData.PostalCode].Value;
      MainForm.edtVAT.Text         :=Vendor.DataSet.Fields[TSupplierData.VATnumber].Value;
      MainForm.edtPerson.Text      :=Vendor.DataSet.Fields[TSupplierData.ContactPerson].Value;
      MainForm.edtNumber.Text      :=Vendor.DataSet.Fields[TSupplierData.MainPhone].Value;
      MainForm.edtEmail.Text       :=Vendor.DataSet.Fields[TSupplierData.MainEmail].Value;
    end;
  finally
    Vendor.Free;
  end;
  Close;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TTicketForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
