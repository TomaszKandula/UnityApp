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
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Actions;  // REFCTOR ALL!!!

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,  Dialogs, Grids, Buttons, ExtCtrls, ComCtrls, StdCtrls, ADODB, StrUtils, ShellApi,
  TLHelp32, pngimage, ImgList, GIFImg, Clipbrd, Main;

{ ------------------------------------------------------------ ! DATA DISPLAY CLASS ! ----------------------------------------------------------------------- }
type
  TDataHandler = class { HELPER INSTANCE }                   //remove this class!
  {$TYPEINFO ON}
  private
    pCUID              :  string;
    pCustName          :  string;
    pCustNumber        :  string;
  public
    var         IsEdit    :  boolean;
    property    CUID      :  string read pCUID       write pCUID;
    property    CustName  :  string read pCustName   write pCustName;
    property    CustNumber:  string read pCustNumber write pCustNumber;
  published
    constructor Create;
    procedure   GetData;        //part of TActions
    procedure   EditDetails;    //should be part of addressbook class
    procedure   SaveDetails;    //should be part of addressbook class
    procedure   SendStatement;  //separate class from TMailer
    procedure   MakePhoneCall;  //method belongs to TActions
  end;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TActionsForm = class(TForm)
    OpenItemsGrid: TStringGrid;
    StatusBar: TStatusBar;
    DailyCom: TMemo;
    HistoryGrid: TStringGrid;
    InnerBox: TShape;
    btnCallCustomer: TSpeedButton;
    btnNext: TSpeedButton;
    GeneralCom: TMemo;
    Text4: TLabel;
    Text5: TLabel;
    Text6: TLabel;
    Text7: TLabel;
    Text8: TLabel;
    BevelLine: TBevel;
    Cust_Name: TLabel;
    Cust_Number: TLabel;
    btnSendStatement: TSpeedButton;
    imgEditDetails: TImage;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    Cust_Person: TEdit;
    Cust_Mail: TEdit;
    Cust_Phone: TEdit;
    imgSaveDetails: TImage;
    imgList: TImageList;
    ButtonPanel: TPanel;
    HistoryPanel: TPanel;
    HistoryTitle: TLabel;
    DailyPanel: TPanel;
    DailyTitle: TLabel;
    GeneralPanel: TPanel;
    GeneralTitle: TLabel;
    btnFeedback: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OpenItemsGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btnNextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSendStatementClick(Sender: TObject);
    procedure HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure HistoryGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure HistoryGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCallCustomerClick(Sender: TObject);
    procedure imgEditDetailsClick(Sender: TObject);
    procedure imgSaveDetailsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnSendEmailClick(Sender: TObject);
    procedure btnFeedbackClick(Sender: TObject);
    procedure Cust_NameClick(Sender: TObject);
    procedure Cust_NumberClick(Sender: TObject);
    procedure Cust_PersonClick(Sender: TObject);
    procedure Cust_MailClick(Sender: TObject);
    procedure Cust_PhoneClick(Sender: TObject);
  public
    function  GetRunningAplications(SearchName: string): boolean;
  end;

var
  ActionsForm: TActionsForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, SQL, Worker, Calendar;

const
  NA:  string = 'Not found!';

var
  DataHandler: TDataHandler;

{$R *.dfm}

{ ----------------------------------------------------------- ! DATAHANDLER METHODS ! ----------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- CREATE }
constructor TDataHandler.Create;
begin
  pCUID      :='';
  pCustName  :='';
  pCustNumber:='';
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- READ AND DISPLAY DATA }
procedure TDataHandler.GetData;
const
  { BELOW ARRAY CONTAINS WITH COLUMN NUMBERS OF 'STRINGGRID' THAT KEEPS OUR     }
  { SOURCE DATA THAT WILL BE TRANSFERRED TO 'OPENITEMSGRID'. THIS COMPONENT     }
  { SHOWS SELECTED DETAILS OF ALL REGISTERED OPEN ITEMS FOR GIVEN CUSTOMER      }
  { THAT WE IDENTIFY BY 'CUID' NUMBER.                                          }
  SrcColumns:  array[0..12] of integer = (11, 6, 10, 5, 9, 8, 12, 4, 27, 20, 30, 34, 33);
var
  iCNT:    integer;
  jCNT:    integer;
  zCNT:    integer;
  Query:   TADOQuery;

  MSSQL:        TMSSQL;
  DailyComment: TDaily;

begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  zCNT:=1;
  with ActionsForm.OpenItemsGrid do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  with ActionsForm.HistoryGrid   do SendMessage(Handle, WM_SETREDRAW, 0, 0);
  ActionsForm.OpenItemsGrid.ClearAll(2, 1, 1, False);
  ActionsForm.HistoryGrid.ClearAll(2, 1, 1, False);

  { ----------------------------------------------------------- ! LIST OF OPEN ITEMS ! ---------------------------------------------------------------------- }

  { ------------------------------------------------------------------------------------------------------------------------- LOAD OPEN ITEMS FROM 'MAINFORM' }
  if MainForm.sgOpenItems.RowCount > 2 then
  begin
    { LOOK FOR THE SAME 'CUID'  }
    for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
    begin
      if MainForm.sgOpenItems.Cells[38, iCNT] = DataHandler.CUID then
      begin
        { MOVE DATA FOR SELECTED COLUMNS AND GIVEN ROW ONCE 'CUID' IS FOUND }
        for jCNT:=low(SrcColumns) to high(SrcColumns) do ActionsForm.OpenItemsGrid.Cells[jCNT + 1, zCNT]:=MainForm.sgOpenItems.Cells[SrcColumns[jCNT], iCNT];
        { MOVE NEXT }
        inc(zCNT);
        ActionsForm.OpenItemsGrid.RowCount:=zCNT;
      end;
    end;
  end;
  { --------------------------------------------------------------------------------------------------------------------- SORT VIA PAYMENT STATUS | ASCENDING }
  ActionsForm.OpenItemsGrid.MSort(12, 1, True);
  { ---------------------------------------------------------------------------------------------------------------------------------- CUSTOMER NAME & NUMBER }
  ActionsForm.Cust_Name.Caption  :=DataHandler.CustName;
  ActionsForm.Cust_Number.Caption:=DataHandler.CustNumber;

  { --------------------------------------------------- ! OTHER CUSTOMER DATA AND USER COMMENT ! ------------------------------------------------------------ }

  { ------------------------------------------------------------------------------------------------------------------------------------------ INITIALIZE SQL }
  Query:=TADOQuery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  try
    try
      Query.SQL.Clear;
      Query.SQL.Text:='SELECT DISTINCT CONTACT, ESTATEMENTS, TELEPHONE FROM tbl_Addressbook WHERE CUID = :uParam0';
      Query.Parameters.ParamByName('uParam0').Value:=DataHandler.CUID;
      Query.Open;
      if Query.RecordCount > 0 then
      begin
        ActionsForm.Cust_Person.Text:=Query.Recordset.Fields[0].Value;
        ActionsForm.Cust_Mail.Text  :=Query.Recordset.Fields[1].Value;
        ActionsForm.Cust_Phone.Text :=Query.Recordset.Fields[2].Value;
      end;
    finally
      Query.Close;
    end;

    { ---------------------------------------------------------------------------------------------------------------------------------------- DAILY COMMENTS }

    try
      Query.SQL.Clear;
      Query.SQL.Text:='SELECT AGEDATE, STAMP, FIXCOMMENT FROM tbl_daily WHERE CUID = :uParam0';
      Query.Parameters.ParamByName('uParam0').Value:=DataHandler.CUID;
      Query.Open;
      if Query.RecordCount > 0 then
      begin
        iCNT:=1;
        ActionsForm.HistoryGrid.RowCount:=Query.RecordCount + 1;
        while not Query.Recordset.EOF do
        begin
          for jCNT:=1 to Query.Recordset.Fields.Count do ActionsForm.HistoryGrid.Cells[jCNT, iCNT]:=MainForm.OleGetStr(Query.Recordset.Fields[jCNT - 1].Value);
          Query.Recordset.MoveNext;
          inc(iCNT);
        end;
      end;
    finally
      { SORT DESCENDING VIA CREATION DATE AND TIME }
      ActionsForm.HistoryGrid.MSort(2, 0, False);
      Query.Close;
    end;


(*
      DailyComment:=TDaily.Create;
      try
        { PREPARE FOR QUERY }
        DailyComment.idThd     :=MainThreadID;
        DailyComment.CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1), MainForm.sgAgeView.Row];
        DailyComment.AGEDATE   :=MainForm.GroupListDates.Text;
        { RELOAD HISTORY CONTENT }
        MSSQL:=TMSSQL.Create(DataBase.ADOConnect);
        try
          { READ ALL LINES FOR GIVEN CUID }
          DailyComment.ManyLines:=True;
          DailyComment.Read;
          MSSQL.SqlToGrid(ActionsForm.HistoryGrid, DailyComment.DataSet, False);
        finally
          MSSQL.Free;
        end;
      finally
        DailyComment.Free;
        //ActionsForm.HistoryGrid.MSort(2, 0, False);
      end;
*)

    { --------------------------------------------------------------------------------------------------------------------------------------- GENERAL COMMENT }
    try
      Query.SQL.Clear;
      Query.SQL.Text:='SELECT FIXCOMMENT FROM tbl_general WHERE CUID = :uParam0';
      Query.Parameters.ParamByName('uParam0').Value:=DataHandler.CUID;
      Query.Open;
      { WE SHOULD HAVE ALWAYS ONE FIELD }
      if Query.RecordCount = 1 then ActionsForm.GeneralCom.Text:=MainForm.OleGetStr(Query.Recordset.Fields[0].Value);
    finally
      Query.Close;
    end;

  finally
    Query.Free;
    { ------------------------------------------------------------------------------------------------------------------------------------------ UNINITIALIZE }
    with ActionsForm.OpenItemsGrid do SendMessage(Handle, WM_SETREDRAW, 1, 0);
    with ActionsForm.HistoryGrid   do SendMessage(Handle, WM_SETREDRAW, 1, 0);
    //if AnsiPos(';', ActionsForm.Cust_Mail.Text) > 0 then ActionsForm.Cust_Mail.Text:=LeftStr((ActionsForm.Cust_Mail.Text), AnsiPos(';', ActionsForm.Cust_Mail.Text) - 1);
    ActionsForm.Cust_Phone.Text:=StringReplace(ActionsForm.Cust_Phone.Text, #13#10, ' ', [rfReplaceAll]);
    ActionsForm.OpenItemsGrid.Repaint;
    ActionsForm.HistoryGrid.Repaint;
    ActionsForm.OpenItemsGrid.AutoThumbSize;
    ActionsForm.HistoryGrid.AutoThumbSize;
    Screen.Cursor:=crDefault;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- EDIT CUSTOMER DETAILS }
procedure TDataHandler.EditDetails;
{ --------------------------------------------------------------- ! INNER BLOCK ! --------------------------------------------------------------------------- }
procedure PicChange(number: integer);
begin
  ActionsForm.imgSaveDetails.Picture.Bitmap.FreeImage;
  ActionsForm.imgSaveDetails.Picture:=nil;
  ActionsForm.imgList.GetBitmap(number, ActionsForm.imgSaveDetails.Picture.Bitmap);
  ActionsForm.imgSaveDetails.Invalidate;
end;
{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  { IF NOT FOUND THEN QUIT }
  if (ActionsForm.Cust_Person.Text = NA) and (ActionsForm.Cust_Mail.Text = NA) and (ActionsForm.Cust_Phone.Text = NA) then
  begin
    MainForm.MsgCall(1, 'This customer does not exist in Address Book. Please add to Address Book first.');
    Exit;
  end;
  { EDIT ON/OFF }
  if IsEdit then
  begin
    ActionsForm.Cust_Name.Cursor      :=crHandPoint;
    ActionsForm.Cust_Number.Cursor    :=crHandPoint;
    ActionsForm.Cust_Person.Cursor    :=crHandPoint;
    ActionsForm.Cust_Mail.Cursor      :=crHandPoint;
    ActionsForm.Cust_Phone.Cursor     :=crHandPoint;
    ActionsForm.Cust_Person.ReadOnly  :=True;
    ActionsForm.Cust_Mail.ReadOnly    :=True;
    ActionsForm.Cust_Phone.ReadOnly   :=True;
    ActionsForm.Cust_Person.Font.Color:=clBlack;
    ActionsForm.Cust_Mail.Font.Color  :=clBlack;
    ActionsForm.Cust_Phone.Font.Color :=clBlack;
    ActionsForm.Cust_Person.Color     :=clWhite;
    ActionsForm.Cust_Mail.Color       :=clWhite;
    ActionsForm.Cust_Phone.Color      :=clWhite;
    ActionsForm.imgSaveDetails.Enabled:=False;
    IsEdit:=False;
    PicChange(0);
  end else
  begin
    ActionsForm.Cust_Name.Cursor      :=crIBeam;
    ActionsForm.Cust_Number.Cursor    :=crIBeam;
    ActionsForm.Cust_Person.Cursor    :=crIBeam;
    ActionsForm.Cust_Mail.Cursor      :=crIBeam;
    ActionsForm.Cust_Phone.Cursor     :=crIBeam;
    ActionsForm.Cust_Person.ReadOnly  :=False;
    ActionsForm.Cust_Mail.ReadOnly    :=False;
    ActionsForm.Cust_Phone.ReadOnly   :=False;
    ActionsForm.Cust_Person.Font.Color:=clNavy;
    ActionsForm.Cust_Mail.Font.Color  :=clNavy;
    ActionsForm.Cust_Phone.Font.Color :=clNavy;
    ActionsForm.Cust_Person.Color     :=clCream;
    ActionsForm.Cust_Mail.Color       :=clCream;
    ActionsForm.Cust_Phone.Color      :=clCream;
    ActionsForm.imgSaveDetails.Enabled:=True;
    IsEdit:=True;
    PicChange(1);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- SAVE CUSTOMER DETAILS }
procedure TDataHandler.SaveDetails;
var
  Query:   TADOQuery;
  StrSQL:  string;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  StrSQL:='SELECT DISTINCT CONTACT, ESTATEMENTS, TELEPHONE FROM tbl_addressbook WHERE CUID = ' + QuotedStr(DataHandler.CUID);
  { ------------------------------------------------------------------------------------------------------------------------------------------------- PROCESS }
  Query:=TADOQUery.Create(nil);
  Query.Connection:=Database.ADOConnect;
  Query.SQL.Clear;
  Query.SQL.Text:=StrSQL;
  try
    try
      Query.Open;
      if Query.RecordCount = 1 then
      begin
        Query.Edit;
        Query.Recordset.Fields['CONTACT'    ].Value:=ActionsForm.Cust_Person.Text;
        Query.Recordset.Fields['ESTATEMENTS'].Value:=ActionsForm.Cust_Mail.Text;//  + ';' + Query.Recordset.Fields['ESTATEMENTS'].Value;
        Query.Recordset.Fields['TELEPHONE'  ].Value:=ActionsForm.Cust_Phone.Text;
        Query.Recordset.Update(EmptyParam, EmptyParam);
        //ActionsForm.StatusBar.SimpleText:='Customer details has been updated successfully!';
      end;
    except
      on E: Exception do
        MainForm.MsgCall(1, 'Cannot save customer details. Please contact IT support. Error thrown: ' + E.Message + '.');
    end;
  finally
    Query.Close;
    Query.Free;
  end;
  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  IsEdit:=True;
  ActionsForm.imgEditDetailsClick(Self);
  Screen.Cursor:=crDefault;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
procedure TDataHandler.SendStatement;
{ ------------------------------------------------------------- ! COMMON VARIABLES ! ------------------------------------------------------------------------ }
var
  InvoiceTracker: TInvoiceTracker;
  { COUNTERS }
  iCNT:        integer;
  { HTML FOR INVOICE LIST }
  HTMLTable:   string;
  HTMLTemp:    string;
  HTMLRow:     string;
  HTMLStat:    string;
  CustAddr:    string;
  CustName:    string;
  { FILES AND EMAILS }
  EmailFr:     string;
  EmailTo:     string;
  EmailCc:     string;
  EmailBc:     string;
  EmailRt:     string;
  EmailSub:    string;
  EmailBody:   string;
  { REAY EMAIL BODY }
  BankDetails: string;
  LBUAddress:  string;
  Telephone:   string;
  SL:          TStringList;
{ --------------------------------------------------------------------------------------------------------------------------- RETURN EMAILS FROM TRACKER LIST }
function RetriveEmails(var EmailFr: string; var EmailTo: string; SG: TStringGrid): boolean;
var
  iCNT:  integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Result:=False;
  EmailFr:='';
  EmailTo:='';
  { REFRESH INVOICE TRACKER }
  InvoiceTracker:=TInvoiceTracker.Create;
  try
    InvoiceTracker.Refresh(MainForm.sgInvoiceTracker, 'ALL');
  finally
    InvoiceTracker.Free;
  end;
  { --------------------------------------------------------------------------------------- LOOK FOR EMAILS AND BANK DETAILS ON THE INVOICE TRACKER COMPONENT }
  if MainForm.sgInvoiceTracker.RowCount > 1 then
    for iCNT:=1 to MainForm.sgInvoiceTracker.RowCount - 1 do
      if DataHandler.CUID = MainForm.sgInvoiceTracker.Cells[2, iCNT] then
      begin
        EmailFr    :=MainForm.sgInvoiceTracker.Cells[8,  iCNT];
        EmailTo    :=MainForm.sgInvoiceTracker.Cells[10, iCNT];
        BankDetails:=MainForm.sgInvoiceTracker.Cells[24, iCNT];
        LBUAddress :=MainForm.sgInvoiceTracker.Cells[25, iCNT] + #13#10 + '<br>' +
                     MainForm.sgInvoiceTracker.Cells[26, iCNT] + #13#10 + '<br>' +
                     MainForm.sgInvoiceTracker.Cells[27, iCNT];
        Telephone  :=MainForm.sgInvoiceTracker.Cells[28, iCNT];
        if (EmailFr <> '') and (EmailTo <> '') then Result:=True;
        Break;
      end;
end;
{ ------------------------------------------------------------------------------------------------------------------------ MOVE OPEN ITEMS DATA TO HTML TABLE }
procedure OpenItemsToHtmlTable(var HtmlStatement: string; var SG: TStringGrid; ActualRow: integer);
begin
  HTMLTemp:=HTMLRow;
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_NUM}', SG.Cells[1, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_DAT}', SG.Cells[8, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{DUE_DAT}', SG.Cells[7, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_CUR}', SG.Cells[6, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_AMT}', SG.Cells[5, ActualRow], [rfReplaceAll]);
  HTMLTemp:=StringReplace(HTMLTemp, '{INV_OSA}', SG.Cells[4, ActualRow], [rfReplaceAll]);
  HtmlStatement:=HtmlStatement + HTMLTemp;
end;
{ ---------------------------------------------------------------- ! MAIN BLOCK ! --------------------------------------------------------------------------- }
begin
  if (ActionsForm.Cust_Mail.Text = '') or (ActionsForm.Cust_Mail.Text = ' ') or (ActionsForm.Cust_Mail.Text = NA) then
  begin
    MainForm.MsgCall(2, 'Statement cannot be sent. There is no e-mail provided.');
    Exit;
  end;
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  CustName:=DataHandler.CustName;
  SL:=TStringList.Create;
  { -------------------------------------------------------------------------------------------------------- HTML TABLE WITH COLUMNS AND PLACEHOLDER FOR ROWS }
  HTMLTable:='<table class="data">'                   +#13#10+
             '<!-- HEADERS -->'                       +#13#10+
             '<tr>'                                   +#13#10+
             '  <!-- COLUMNS -->'                     +#13#10+
             '  <th class="col1">Invoice No.:</th>'   +#13#10+
             '  <th class="col2">Invoice Date:</th>'  +#13#10+
             '  <th class="col3">Due date:</th>'      +#13#10+
             '  <th class="col4">Currency:</th>'      +#13#10+
             '  <th class="col5">Amount:</th>'        +#13#10+
             '  <th class="col6">O/S Amount:</th>'    +#13#10+
             '</tr>'                                  +#13#10+
             '  <!-- ROWS WITH DATA -->'              +#13#10+
             '{ROWS}'                                 +#13#10+
             '</table>';
  { ------------------------------------------------------------------------------------------- HTML ROW FORM TO BE FILLED WITH DATA AND PUT INTO 'HTMLTABLE' }
  HTMLRow:='<tr>'                                     +#13#10+
           '  <td class="col1">{INV_NUM}</td>'        +#13#10+
           '  <td class="col2">{INV_DAT}</td>'        +#13#10+
           '  <td class="col3">{DUE_DAT}</td>'        +#13#10+
           '  <td class="col4">{INV_CUR}</td>'        +#13#10+
           '  <td class="col5">{INV_AMT}</td>'        +#13#10+
           '  <td class="col6">{INV_OSA}</td>'        +#13#10+
           '</tr>'                                    +#13#10;
  { ---------------------------------------------------------------------------------------------------------------------- PROCEED IF WE HAVE LISTED INVOICES }
  if ActionsForm.OpenItemsGrid.RowCount > 2 then
  begin
    { --------------------------------------------------------------------------------------------------------------------------- SKIP FIRST ROW BEING HEADER }
    for iCNT:=1 to ActionsForm.OpenItemsGrid.RowCount - 1 do
      if StrToFloatDef(ActionsForm.OpenItemsGrid.Cells[4, iCNT], 0) <> 0 then
        OpenItemsToHtmlTable(HTMLStat, ActionsForm.OpenItemsGrid, iCNT);
    { ---------------------------------------------------------------------------------------------------------------- GET CUID POSITION FROM OPEN ITEMS LIST }
    for iCNT:=1 to MainForm.sgOpenItems.RowCount - 1 do
      if MainForm.sgOpenItems.Cells[38, iCNT] = DataHandler.CUID then break;
    { ------------------------------------------------------------------------------------------------------------------ BUILD HTML CODE FOR CUSTOMER ADDRESS }
    CustAddr:='<p class="p"><b>' + CustName + '</b><br />' +#13#10;
    { ------------------------------------------------------------------------------------------------------------------------ ADD ADDRESS FIELD IF NOT EMPTY }
    if (MainForm.sgOpenItems.Cells[21, iCNT] <> '') and (MainForm.sgOpenItems.Cells[21, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[21, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[22, iCNT] <> '') and (MainForm.sgOpenItems.Cells[22, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[22, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[23, iCNT] <> '') and (MainForm.sgOpenItems.Cells[23, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[23, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[24, iCNT] <> '') and (MainForm.sgOpenItems.Cells[24, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[24, iCNT] + '<br />' +#13#10;
    if (MainForm.sgOpenItems.Cells[25, iCNT] <> '') and (MainForm.sgOpenItems.Cells[25, iCNT] <> ' ') then CustAddr:=CustAddr + MainForm.sgOpenItems.Cells[25, iCNT] + '<br />' +#13#10;
    CustAddr:=CustAddr + '</p>' +#13#10;
    { ----------------------------------------------------------------------------------------------------------------------------------- FILL THE STATEMENT  }
    try
      { --------------------------------------------------------------------------------------------------------------------------------------------- PREPARE }
      SL.LoadFromFile(Settings.LayoutDir + 'all_statement_1.html');
      { ----------------------------------------------------------------------------------------------------------------------- SEND AN E-MAIL WITH STATEMENT }
      if RetriveEmails(EmailFr, EmailTo, MainForm.sgInvoiceTracker) then
      begin
        { HTML BODY }
        HTMLTable:=StringReplace(HTMLTable,  '{ROWS}',         HTMLStat,   [rfReplaceAll]);
        EmailBody:=StringReplace(SL.Text,    '{INVOICE_LIST}', HTMLTable,  [rfReplaceAll]);
        EmailBody:=StringReplace(EmailBody,  '{ADDR_DATA}',    CustAddr,   [rfReplaceAll]);
        EmailBody:=StringReplace(EmailBody,  '{BANKS}',        BankDetails,[rfReplaceAll]);
        EmailBody:=StringReplace(EmailBody,  '{ADDR_LBU}',     LBUAddress, [rfReplaceAll]);
        EmailBody:=StringReplace(EmailBody,  '{EMAIL}',        EmailFr,    [rfReplaceAll]);
        EmailBody:=StringReplace(EmailBody,  '{TEL}',          Telephone,  [rfReplaceAll]);
        { EMAILS }
        EmailCc :=EmailFr;
        EmailBc :='';
        EmailRt :='';
        EmailSub:='Account Statement (' + CustName + ')';
        { MAILER SETUP }
        InvoiceTracker:=TInvoiceTracker.Create;
        try
          InvoiceTracker.XMailer    :=EmailFr;
          InvoiceTracker.MailFrom   :=EmailFr;
          InvoiceTracker.MailTo     :=EmailTo;
          InvoiceTracker.MailCc     :=EmailCc;
          InvoiceTracker.MailBcc    :=EmailBc;
          InvoiceTracker.MailRt     :=EmailRt;
          InvoiceTracker.MailSubject:=EmailSub;
          InvoiceTracker.MailBody   :=EmailBody;
          InvoiceTracker.idThd      :=MainThreadID;
          { TRY TO SEND }
          if InvoiceTracker.SendNow then
          begin
            //ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + #13#10 + 'E-mail statement has been sent.';
            MainForm.MsgCall(1, 'Statement has been sent successfully!')
          end
            else
              MainForm.MsgCall(2, 'Cannot send statement from e-mail address: ' + EmailFr + '. Please contact IT support.');
        finally
          InvoiceTracker.Free;
        end;
        { ----------------------------------------------------------------------------------------------------------------------- DEBUG LINES | DO NOT DELETE }
        //SL.Text:=EmailBody;
        //SL.SaveToFile('G:\test.html');
      end
        else
          MainForm.MsgCall(2, 'Cannot send statement from e-mail address: ' + EmailFr + '. Please make sure that this customer is registered on Invoice Tracker list.');
    finally
      SL.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- MAKE PHONE CALL }
procedure TDataHandler.MakePhoneCall;
begin
  { CHECK FOR 'LYNCCALL.EXE' }
  if not FileExists(Settings.AppDir + 'lynccall.exe') then
  begin
    MainForm.MsgCall(3, Settings.APPNAME + ' cannot find ''lynccall.exe''. Please contact IT support.');
    Exit;
  end;
  { CHECK IF LYNC/SKYPE IS RUNNING }
  if not ActionsForm.GetRunningAplications('lync.exe') then
  begin
    MainForm.MsgCall(3, Settings.APPNAME + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
    Exit;
  end;
  { RUN LYNC WITH GIVEN PHONE NUMBER }
  ShellExecute(ActionsForm.Handle, 'open', PChar(Settings.AppDir + 'lynccall.exe '), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);
  if ActionsForm.DailyCom.Text = '' then ActionsForm.DailyCom.Text:='Called customer today.'
    else
      ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + #13#10 + 'Called customer today.';
end;

{ ----------------------------------------------------------- ! MAIN THREAD METHODS ! ----------------------------------------------------------------------- }

{ ------------------------------------------------------------ ! SUPPORTING METHODS ! ----------------------------------------------------------------------- }
{ --------------------------------------------------------------------------------------------------------------------------- CHECK IF APPLICATION IS RUNNING }
function TActionsForm.GetRunningAplications(SearchName: string): boolean;
var
  PE:     TProcessEntry32;
  Snap:   THandle;
  fName:  string;
begin
  { INITIALIZE }
  Result:=False;
  { TAKE SNAPSHOT OF RUNNING APPLICATION }
  PE.dwSize:=SizeOf(PE);
  Snap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snap <> 0 then begin
    if Process32First(Snap, PE) then begin
      fName:=string(PE.szExeFile);
      { GO THROUGH ALL THE ITEMS AND STOP IF MATCHED }
      while Process32Next(Snap, PE) do begin
        fName:=string(PE.szExeFile);
        if LowerCase(fName) = LowerCase(SearchName) then begin
          Result:=True;
          Break;
        end;
      end;
    end;
    CloseHandle(Snap);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TActionsForm.FormCreate(Sender: TObject);
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  DataHandler:=TDataHandler.Create;
  DataHandler.IsEdit:=False;
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  ActionsForm.Caption:=Settings.TMIG.ReadString(Settings.ApplicationDetails, 'WND_TRANSACTIONS', Settings.APPNAME);
  { ---------------------------------------------------------------------------------------------------------------------- SETUP COLUMNS HEADERS | OPEN ITEMS }
  OpenItemsGrid.RowCount:=2;
  OpenItemsGrid.ColCount:=14;
  OpenItemsGrid.Cols[0].Text :='';
  OpenItemsGrid.Cols[1].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER11', 'InvoNo');
  OpenItemsGrid.Cols[2].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER6',  'OpenAm');
  OpenItemsGrid.Cols[3].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER10', 'Am');
  OpenItemsGrid.Cols[4].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER5',  'OpenCurAm');
  OpenItemsGrid.Cols[5].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER9',  'CurAm');
  OpenItemsGrid.Cols[6].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER8',  'ISO');
  OpenItemsGrid.Cols[7].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER12', 'DueDt');
  OpenItemsGrid.Cols[8].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER4',  'VoDt');
  OpenItemsGrid.Cols[9].Text :=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER27', 'ValDt');
  OpenItemsGrid.Cols[10].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER20', 'Ctrl');
  OpenItemsGrid.Cols[11].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER30', 'Txt');
  OpenItemsGrid.Cols[12].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER34', 'PmtStat');
  OpenItemsGrid.Cols[13].Text:=Settings.TMIG.ReadString(Settings.OpenItemsData, 'HEADER33', 'AddTxt');
  { -------------------------------------------------------------------------------------------------------------------- SETUP COLUMNS HEADERS | HISTORY GRID }
  HistoryGrid.RowCount:=2;
  HistoryGrid.ColCount:=11;
//  HistoryGrid.Cols[0].Text:='';
//  HistoryGrid.Cols[1].Text:='Aging date';
//  HistoryGrid.Cols[2].Text:='Comment created';
//  HistoryGrid.Cols[3].Text:='Commentary';
  { HIDE THIRD COLUMN FROM USER, IT IS NEEDED ONLY FOR SORTING PURPOSES }
  HistoryGrid.ColWidths[1]:= -1;
  HistoryGrid.ColWidths[2]:= -1;
  HistoryGrid.ColWidths[3]:= -1;
  HistoryGrid.ColWidths[7]:= -1;
  HistoryGrid.ColWidths[8]:= -1;
  HistoryGrid.ColWidths[9]:= -1;
  HistoryGrid.ColWidths[10]:= -1;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TActionsForm.FormShow(Sender: TObject);
begin
  { ---------------------------------------------------------------------------------------------------------------------------------- RESET CUSTOMER DETAILS }
  { CAPTIONS AND MEMOS }
  Cust_Name.Caption   :=NA;
  Cust_Number.Caption :=NA;
  Cust_Person.Text    :=NA;
  Cust_Mail.Text      :=NA;
  Cust_Phone.Text     :=NA;
  DailyCom.Text       :='';
  GeneralCom.Text     :='';
  StatusBar.SimpleText:='';
  { DISALLOW EDIT }
  if DataHandler.IsEdit then imgEditDetailsClick(Self);
  { ASSIGN: CUID, CUSTOMER NAME AND NUMBER }
  DataHandler.CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1), MainForm.sgAgeView.Row];
  DataHandler.CustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUSTOMER NAME',   1, 1), MainForm.sgAgeView.Row];
  DataHandler.CustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUSTOMER NUMBER', 1, 1), MainForm.sgAgeView.Row];
  { OPEN ITEMS DATE AND TIME }
  StatusBar.SimpleText:='Open items date and time: ' + OpenItems.ArrOpenItems[1, 1] + ' @ ' + OpenItems.ArrOpenItems[1, 2] + '.';
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE }
procedure TActionsForm.FormActivate(Sender: TObject);
begin
  if DataBase.LastError = 0 then
  begin
    Sleep(100);
    DataHandler.GetData;
    { THUMB SIZES }
    OpenItemsGrid.AutoThumbSize;
    OpenItemsGrid.SetColWidth(10, 40);
    HistoryGrid.AutoThumbSize;
    HistoryGrid.SetColWidth(10, 30);
  end
    else
      StatusBar.SimpleText:='Cannot connect with database. Please contact IT support.';
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ ON DESTROY }
procedure TActionsForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DataHandler);
end;

{ ---------------------------------------------------------- ! COMPONENT EVENTS | EVENTS ! ------------------------------------------------------------------ }

{ ----------------------------------------------------------------------------------------------------------------------------------------- DRAW SELECTED ROW }
procedure TActionsForm.HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);
end;

{ ------------------------------------------------------------------------------------------------------------------------------- COLOR NUMBERS AND SELECTION }
procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

  (* CALL SG_DRAWSELECTED BEFORE SG_COLORVALUES *)

  { DRAW SELECTED | SKIP HEADERS }
  OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, Settings.SELCOLOR, clBlack, clWhite, True);

  { ONLY FOR COLUMNS 2..5 }
  if ( (ACol >= 2) and (ACol <= 5) ) and (ARow > 0) then OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);

end;

{ --------------------------------------------------------------------------------------------------------------------------------- SHOW DETAIL OF OPEN ITEMS }
procedure TActionsForm.OpenItemsGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  {
  StatusBar.SimpleText:='Invoice number: '       + OpenItemsGrid.Cells[1, ARow] +
                        ' with due date as of: ' + OpenItemsGrid.Cells[7, ARow] + '. Text: ' + OpenItemsGrid.Cells[11, ARow];
  }
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SHOW DATA WHEN SELECTED }
procedure TActionsForm.HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
//var
//  DailyComment: TDaily;
begin
(*
  DailyComment:=TDaily.Create;
  try
    DailyComment.idThd  :=MainThreadID;
    DailyComment.CUID   :=DataHandler.CUID;
    DailyComment.AGEDATE:=HistoryGrid.Cells[4, ARow];
    if DailyComment.Read then
    begin
      DailyCom.Text:=DailyComment.FIXCOMMENT;
    end;
  finally
    DailyComment.Free;
  end;
*)
end;

{ --------------------------------------------------------------- ! KEYBOARD EVENTS ! ----------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENT ON <ENTER> }
procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//var
//  DailyComment: TDaily;
//  MSSQL:        TMSSQL;
begin
  { NEW LINE }
  if (Key = VK_RETURN) and (Shift=[ssALT]) then
  begin
    DailyCom.Lines.Add(#13#10);
    Exit;
  end;
  { SAVE TO DB }
  if (Key = VK_RETURN) then
(*
    if ( (DailyCom.Text <> '') and (DailyCom.Text <> ' ') ) then
    begin
      DailyComment:=TDaily.Create;
      try
        { PREPARE FOR QUERY }
        DailyComment.idThd     :=MainThreadID;
        DailyComment.CUID      :=DataHandler.CUID;
        DailyComment.AGEDATE   :=MainForm.GroupListDates.Text;
        { READ DATA FROM DATABASE }
        DailyComment.Read;
        { UPDATE ITEMS' VALUES }
        DailyComment.STAMP     :=DateTimeToStr(Now);
        DailyComment.USER_ALIAS:=UpperCase(Settings.WinUserName);
        DailyComment.FIXCOMMENT:=DailyCom.Text;
        { WRITE TO DATABASE }
        DailyComment.Write;
        { RELOAD HISTORY CONTENT }
        MSSQL:=TMSSQL.Create(DataBase.ADOConnect);
        try
          { READ ALL LINES FOR GIVEN CUID }
          DailyComment.ManyLines:=True;
          DailyComment.Read;
          MSSQL.SqlToGrid(HistoryGrid, DailyComment.DataSet, True);
        finally
          MSSQL.Free;
        end;
      finally
        DailyComment.Free;
        HistoryGrid.SetColWidth(10, 30);
      end;
    end
      else StatusBar.SimpleText:='Cannot save empty comment.';
*)
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENT ON <ENTER> }
procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//var
//  GeneralComment: TGeneral;
begin
  { NEW LINE }
  if (Key = VK_RETURN) and (Shift=[ssALT]) then
  begin
    GeneralCom.Lines.Add(#13#10);
    Exit;
  end;
  { SAVE TO DB }
  if (Key = VK_RETURN) then
(*
    if ( (GeneralCom.Text <> '') and (GeneralCom.Text <> ' ') ) then
    begin
      GeneralComment:=TGeneral.Create;
      try
        { PREPARE FOR QUERY }
        GeneralComment.idThd     :=MainThreadID;
        GeneralComment.CUID      :=DataHandler.CUID;
        { READ DATA FROM DATABASE }
        GeneralComment.Read;
        { UPDATE ITEMS' VALUES }
        GeneralComment.STAMP     :=DateTimeToStr(Now);
        GeneralComment.USER_ALIAS:=UpperCase(Settings.WinUserName);
        GeneralComment.FIXCOMMENT:=GeneralCom.Text;
        { WRITE TO DATABASE }
        GeneralComment.Write;
      finally
        GeneralComment.Free;
      end;
    end else
    begin
      StatusBar.SimpleText:='Cannot save empty comment.';
    end;
*)
end;

{ --------------------------------------------------------------------------------------------------------------------- COPY STRING GRID CONTENT TO CLIPBOARD }
procedure TActionsForm.OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then OpenItemsGrid.CopyCutPaste(1);
end;

{ --------------------------------------------------------------------------------------------------------------------- COPY STRING GRID CONTENT TO CLIPBOARD }
procedure TActionsForm.HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then HistoryGrid.CopyCutPaste(1);
end;

{ ----------------------------------------------------------------- ! MOUSE EVENTS ! ------------------------------------------------------------------------ }

{ ----------------------------------------------------------------------------------------------------------------------------------------- SCROLL BARS MOVES }
procedure TActionsForm.OpenItemsGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  OpenItemsGrid.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TActionsForm.OpenItemsGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  OpenItemsGrid.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;


procedure TActionsForm.HistoryGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  HistoryGrid.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TActionsForm.HistoryGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=True;
  HistoryGrid.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------------- COPY TO CLIPBOARD }
procedure TActionsForm.Cust_NameClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Name.Caption;
end;

procedure TActionsForm.Cust_NumberClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Number.Caption;
end;

procedure TActionsForm.Cust_PersonClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Person.Text;
end;

procedure TActionsForm.Cust_MailClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Mail.Text;
end;

procedure TActionsForm.Cust_PhoneClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Phone.Text;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ ALLOW EDIT }
procedure TActionsForm.imgEditDetailsClick(Sender: TObject);
begin
  DataHandler.EditDetails;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- SAVE CUSTOMER DETAILS }
procedure TActionsForm.imgSaveDetailsClick(Sender: TObject);
begin
  if DataBase.LastError = 0 then
  begin
    Sleep(100);
    DataHandler.SaveDetails;
  end
    else
      StatusBar.SimpleText:='Cannot connect with database. Please contact IT support.';
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
procedure TActionsForm.btnSendStatementClick(Sender: TObject);
begin
  if DataBase.LastError = 0 then
  begin
    Sleep(100);
    DataHandler.SendStatement;
  end
    else
      StatusBar.SimpleText:='Cannot connect with database. Please contact IT support.';
end;

{ ------------------------------------------------------------------------------------------------------------------------------ SELECT NEXT OVERDUE CUSTOMER }
procedure TActionsForm.btnNextClick(Sender: TObject);
var
  iCNT:  integer;
begin
  { LOOP TO THE NEAREST UNHIDDEN ROW }
  for iCNT:=(MainForm.sgAgeView.Selection.Top + 1) to MainForm.sgAgeView.RowCount - 1 do
  begin
    if (MainForm.sgAgeView.RowHeights[iCNT] <> -1) and (MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('OVERDUE', 1, 1), iCNT] <> '0') then
    begin
      MainForm.sgAgeView.Row:=iCNT;
      Break;
    end;
  end;
  { ASSIGN DATA FROM NEXT ROW }
  DataHandler.CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID', 1, 1),            MainForm.sgAgeView.Row];
  DataHandler.CustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUSTOMER NAME',   1, 1), MainForm.sgAgeView.Row];
  DataHandler.CustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUSTOMER NUMBER', 1, 1), MainForm.sgAgeView.Row];
  { CLEAR ALL }
  DailyCom.Text       :='';
  GeneralCom.Text     :='';
  StatusBar.SimpleText:='';
  { LOAD NEW DATA }
  ActionsForm.FormActivate(Self);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- FEEDBACK AFTER CALL }
procedure TActionsForm.btnFeedbackClick(Sender: TObject);
begin
  MainForm.WndCall(CalendarForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SEND EMAIL }
procedure TActionsForm.btnSendEmailClick(Sender: TObject);
begin
  // code here...
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- CALL CUSTOMER }
procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
  DataHandler.MakePhoneCall;
end;

end.
