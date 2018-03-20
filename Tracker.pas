{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Tracker;

interface

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, StrUtils, ADODB, Main;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TTrackerForm = class(TForm)
    FrameBottom: TShape;
    Label6: TLabel;
    EmailFromList: TComboBox;
    TextStatTo: TLabeledEdit;
    ErrorStatTo: TLabel;
    AppMain: TShape;
    btnOK: TSpeedButton;
    btnCancel: TSpeedButton;
    FrameTop: TShape;
    TextReminder1: TLabeledEdit;
    TextReminder2: TLabeledEdit;
    TextReminder3: TLabeledEdit;
    SplitLine: TBevel;
    TextMailTo: TLabeledEdit;
    TextLegalTo: TLabeledEdit;
    LayoutList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ErrorLegalTo: TLabel;
    ErrorMailTo: TLabel;
    TextReminder4: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    ErrorEmailFrom: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    { ------------------------------------------------------------- ! HELPERS ! ----------------------------------------------------------------------------- }
  private
    var pTrackerGrid  :  TStringGrid;
    var pAgeGrid      :  TStringGrid;
  public
    var UserAlias :  string;
    var CUID      :  string;
    var CoCode    :  string;
    var Branch    :  string;
    var CustName  :  string;
    var Layout    :  string;
    var Indv_Rem1 :  string;
    var Indv_Rem2 :  string;
    var Indv_Rem3 :  string;
    var Indv_Rem4 :  string;
    { PROPERTIES }
    property TrackerGrid :  TStringGrid read pTrackerGrid;
    property AgeGrid     :  TStringGrid read pAgeGrid;
    { HELPER METHODS FOR DEALING WITH INVOICE TRACKER ITEMS }
    procedure Show;
    procedure Add;
    procedure Delete;
    { DISPLAY ALL THE NECESSARY DATA }
    procedure GetData;
  end;

var
  TrackerForm: TTrackerForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, Worker, Settings, Database;

{$R *.dfm}

{ ------------------------------------------------------------- ! SUPPORTING METHODS ! ---------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------ REFRESH INVOICE TRACKER LIST }
procedure TTrackerForm.Show;
var
  TrackerItems: TDataTables;
  Source:       TStringGrid;
begin
  TrackerItems:=TDataTables.Create(MainForm.FDbConnect);
  Source:=TrackerGrid;
  try
    TrackerGrid.Freeze(True);
    TrackerItems.StrSQL:=EXECUTE + TrackerList + SPACE + QuotedStr(UserAlias);
    TrackerItems.SqlToGrid(Source, TrackerItems.ExecSQL, False, True);
  finally
    TrackerItems.Free;
    TrackerGrid.Freeze(False);
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------- ADD NEW CUSTOMER TO INVLICE TRACKER }
procedure TTrackerForm.Add;
var
  TrackerItems: TDataTables;
begin
  TrackerItems:=TDataTables.Create(MainForm.FDbConnect);
  try
    { CLEAN UP AND FILL WITH NEW DATA }
    TrackerItems.CleanUp;
    TrackerItems.Columns.Add(TTracker.USER_ALIAS);  TrackerItems.Values.Add(UserAlias);
    TrackerItems.Columns.Add(TTracker.CUID);        TrackerItems.Values.Add(CUID);
    TrackerItems.Columns.Add(TTracker.CO_CODE);     TrackerItems.Values.Add(CoCode);
    TrackerItems.Columns.Add(TTracker.BRANCH);      TrackerItems.Values.Add(Branch);
    TrackerItems.Columns.Add(TTracker.CUSTNAME);    TrackerItems.Values.Add(CustName);
    TrackerItems.Columns.Add(TTracker.LAYOUT);      TrackerItems.Values.Add(Layout);
    TrackerItems.Columns.Add(TTracker.STAMP);       TrackerItems.Values.Add(DateTimeToStr(Now));
    TrackerItems.Columns.Add(TTracker.INDV_REM1);   TrackerItems.Values.Add(Indv_Rem1);
    TrackerItems.Columns.Add(TTracker.INDV_REM2);   TrackerItems.Values.Add(Indv_Rem2);
    TrackerItems.Columns.Add(TTracker.INDV_REM3);   TrackerItems.Values.Add(Indv_Rem3);
    TrackerItems.Columns.Add(TTracker.INDV_REM4);   TrackerItems.Values.Add(Indv_Rem4);
    { EXECUTE }
    if TrackerItems.InsertInto(TblTracker) then
      MainForm.MsgCall(mcInfo, 'Customer has been successfuly added to the Invoice Tracker.')
        else
          MainForm.MsgCall(mcError, 'Cannot execute writing to database. Please contact with IT support.');
  finally
    TrackerItems.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- DELETE GIVEN CUSTOMER }
procedure TTrackerForm.Delete;
var
  TrackerItems: TDataTables;
  PrimaryTable: string;
  ForeignTable: string;
begin
  TrackerItems:=TDataTables.Create(MainForm.FDbConnect);
  try
    { BUILD SQL }
    PrimaryTable:=DELETE_FROM + TblTracker  + WHERE + TTracker.CUID  + EQUAL + QuotedStr(CUID);  { HOLDS RECORDE CUSTOMERS  }
    ForeignTable:=DELETE_FROM + TblInvoices + WHERE + TInvoices.CUID + EQUAL + QuotedStr(CUID);  { HOLDS CUSTOMERS INVOICES }
    { REMOVE FROM FOREIGN TABLE FIRST, THEN FROM PRIMARY TABLE }
    TrackerItems.StrSQL:=ForeignTable + ';' + PrimaryTable;
    TrackerItems.ExecSQL;
    TrackerGrid.DeleteRowFrom(1, 1);
  finally
    TrackerItems.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- RETRIVE AND DISPLAY DATA }
procedure TTrackerForm.GetData;
var
  Tables: TDataTables;
  AppSettings:  TSettings;
  SetKeys:      TStringList;
  iCNT:         integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  SetKeys:=TStringList.Create();
  AppSettings:=TSettings.Create;
  CUID    :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCUID,          1, 1), AgeGrid.Row];
  CoCode  :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCO_CODE,       1, 1), AgeGrid.Row];
  Branch  :=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fAGENT,         1, 1), AgeGrid.Row];
  CustName:=AgeGrid.Cells[AgeGrid.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), AgeGrid.Row];
  { ----------------------------------------------------------------------------------------------------------------------------------------- LIST OF LAYOUTS }
  try
    AppSettings.TMIG.ReadSection(VariousLayouts, SetKeys);
    for iCNT:=0 to SetKeys.Count - 1 do
      { ADD TO LIST ONLY THOSE FOR WHICH KEY EQUALS 'TRACKER' }
      if MidStr(SetKeys.Strings[iCNT], 1, 7) = 'TRACKER' then
        if AppSettings.TMIG.ReadString(VariousLayouts, 'TRACKER' + IntToStr(iCNT), '') <> '' then
          LayoutList.Items.Add(AppSettings.TMIG.ReadString(VariousLayouts, 'TRACKER' + IntToStr(iCNT), ''));
  finally
    AppSettings.Free;
    SetKeys.Free;
    if LayoutList.Items.Count > 0 then LayoutList.ItemIndex:=0;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------------------ READ OUT }
  Tables:=TDataTables.Create(MainForm.FDbConnect);
  try
    { GET DATA FROM ADDRESS BOOK }
    Tables.CustFilter:=WHERE + TAddressBook.CUID + EQUAL + QuotedStr(CUID);
    Tables.OpenTable(TblAddressbook);
    if Tables.DataSet.RecordCount = 1 then { CUID IS UNIQUE IN ADDRESS BOOK, THUS CANNOT HAVE MORE THAN ONE ITEM PER CUID }
    begin
      TextMailTo.Text:=Tables.DataSet.Fields[TAddressBook.EMAILS].Value;
      TextStatTo.Text:=Tables.DataSet.Fields[TAddressBook.ESTATEMENTS].Value;
    end;
    { GET DATA FROM COMPANY TABLE }
    Tables.CustFilter:=WHERE +
                         TCompany.CO_CODE +
                       EQUAL +
                         QuotedStr(CoCode) +
                       _AND +
                         TCompany.BRANCH +
                       EQUAL +
                         QuotedStr(Branch);
    Tables.OpenTable(TblCompany);
    if Tables.DataSet.RecordCount = 1 then
    begin
      TextLegalTo.Text:=Tables.DataSet.Fields[TCompany.LEGALTO].Value;
      EmailFromList.Items.Add(Tables.DataSet.Fields[TCompany.SEND_NOTE_FROM].Value);
      EmailFromList.ItemIndex:=0;
    end;
  finally
    Tables.Free;
    if TextMailTo.Text    <> '' then ErrorMailTo.Visible   :=False;
    if TextLegalTo.Text   <> '' then ErrorStatTo.Visible   :=False;
    if TextStatTo.Text    <> '' then ErrorLegalTo.Visible  :=False;
    if EmailFromList.Text <> '' then ErrorEmailFrom.Visible:=False;
    if not (ErrorLegalTo.Visible) and not (ErrorMailTo.Visible) and not (ErrorStatTo.Visible) then btnOK.Enabled:=True;
    Screen.Cursor:=crDefault;
  end;
end;

{ ############################################################# ! MAIN THREAD METHODS ! ##################################################################### }

{ -------------------------------------------------------------- ! EXECUTE ON CREATE ! ---------------------------------------------------------------------- }
procedure TTrackerForm.FormCreate(Sender: TObject);
var
  AppSettings:  TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    TrackerForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_TRACKER', APPNAME);
  finally
    AppSettings.Free;
  end;
  { --------------------------------------------------------------------------------------------------------------------------- LOAD ALL INVOCE TRACKER ITEMS }
  pAgeGrid    :=MainForm.sgAgeView;
  pTrackerGrid:=MainForm.sgInvoiceTracker;
  UserAlias   :='*';
  Show;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TTrackerForm.FormShow(Sender: TObject);
begin
  LayoutList.Items.Clear;
  EmailFromList.Items.Clear;
  TextReminder1.Text    :='0';
  TextReminder2.Text    :='0';
  TextReminder3.Text    :='0';
  TextReminder4.Text    :='0';
  TextMailTo.Text       :='';
  TextStatTo.Text       :='';
  TextLegalTo.Text      :='';
  ErrorMailTo.Visible   :=True;
  ErrorStatTo.Visible   :=True;
  ErrorLegalTo.Visible  :=True;
  ErrorEmailFrom.Visible:=True;
  btnOK.Enabled         :=False;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE }
procedure TTrackerForm.FormActivate(Sender: TObject);
begin
  GetData;
end;

{ ------------------------------------------------------------- ! APPLICATION EVENTS ! ---------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------------- OK | PROCESS }
procedure TTrackerForm.btnOKClick(Sender: TObject);
begin
  { ASSIGN }
  UserAlias:=UpperCase(MainForm.FUserName);
  Layout:=LayoutList.Text;
  Indv_Rem1:=TextReminder1.Text;
  Indv_Rem2:=TextReminder2.Text;
  Indv_Rem3:=TextReminder3.Text;
  Indv_Rem4:=TextReminder4.Text;
  { DISALLOW ZERO VALUES }
  if (Indv_Rem1 = '0') or (Indv_Rem2 = '0') or (Indv_Rem3 = '0') or (Indv_Rem4 = '0') then
  begin
    MainForm.MsgCall(mcWarn, 'Zero values are disallowed. Please correct and try again.');
    Exit;
  end;
  { EXECUTE AND CLOSE }
  Add;
  Close;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- CLOSE }
procedure TTrackerForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
