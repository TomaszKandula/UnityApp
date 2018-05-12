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
unit Actions;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,  Dialogs, Grids, Buttons, ExtCtrls, ComCtrls, StdCtrls, ADODB, StrUtils, ShellApi,
  TLHelp32, pngimage, ImgList, GIFImg, Clipbrd, Main;

{ ------------------------------------------------------------ ! INTERPOSER CLASSES ! ----------------------------------------------------------------------- }
                                                    (* EXTEND CURRENT COMPONENTS | MAIN THREAD *)

{ --------------------------------------------------------------- ! TEDIT CLASS ! --------------------------------------------------------------------------- }
type
  TEdit = Class(StdCtrls.TEdit)
  public
    FAlignment: TAlignment;
    procedure SetAlignment(value: TAlignment);
    procedure CreateParams(var params: TCreateParams); override;
    property  Alignment: TAlignment read FAlignment write SetAlignment;
  end;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TActionsForm = class(TForm)
    OpenItemsGrid: TStringGrid;
    DailyCom: TMemo;
    HistoryGrid: TStringGrid;
    btnCallCustomer: TSpeedButton;
    btnNext: TSpeedButton;
    GeneralCom: TMemo;
    Text4: TLabel;
    Text5: TLabel;
    Text6: TLabel;
    Text7: TLabel;
    Text8: TLabel;
    Cust_Name: TLabel;
    Cust_Number: TLabel;
    btnSendStatement: TSpeedButton;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    Cust_Person: TEdit;
    Cust_Mail: TEdit;
    ButtonPanel: TPanel;
    HistoryPanel: TPanel;
    HistoryTitle: TLabel;
    DailyPanel: TPanel;
    DailyTitle: TLabel;
    GeneralPanel: TPanel;
    GeneralTitle: TLabel;
    btnFeedback: TSpeedButton;
    btnClearFollowUp: TSpeedButton;
    btnSendEmail: TSpeedButton;
    Cust_Phone: TComboBox;
    GroupCustomerDetails: TGroupBox;
    btnSaveCustDetails: TSpeedButton;
    Text9: TLabel;
    Cust_MailBack: TShape;
    Cust_PersonBack: TShape;
    Cust_NumberBack: TShape;
    Cust_NameBack: TShape;
    btnBack: TSpeedButton;
    btnEdit: TSpeedButton;
    btnCopyCustName: TSpeedButton;
    btnCopyCustNumber: TSpeedButton;
    btnCopyPerson: TSpeedButton;
    btnCopyEmail: TSpeedButton;
    PanelStatusBar: TPanel;
    SimpleText: TLabel;
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
    procedure FormDestroy(Sender: TObject);
    procedure DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnFeedbackClick(Sender: TObject);
    procedure btnClearFollowUpClick(Sender: TObject);
    procedure btnSendEmailClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveCustDetailsClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnBackClick(Sender: TObject);
    procedure btnCopyCustNameClick(Sender: TObject);
    procedure btnCopyCustNumberClick(Sender: TObject);
    procedure btnCopyPersonClick(Sender: TObject);
    procedure btnCopyEmailClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  public
    var CUID       :  string;
    var SCUID      :  string;
    var CoCode     :  string;
    var CustName   :  string;
    var CustNumber :  string;
    var SrcColumns :  array of integer;
  published
    function  GetRunningApps(SearchName: string): boolean;
    procedure GetData(OpenItemsDest: TStringGrid; HistoryDest: TStringGrid; OpenItemsSrc: TStringGrid);
    procedure UpdateHistory(Grid: TStringGrid);
    procedure SetHistoryCols(Grid: TStringGrid);
    procedure SetControls;
    procedure Initialize;
    procedure ClearAll;
    procedure MakePhoneCall;
    procedure LoadCustomer(Direction: integer);
    procedure SendAccountStatement(Layout: integer; Salut: string; Mess: string);
    procedure RegisterAction;
    procedure ClearFollowUp;
    procedure SaveCustomerDetails;
    procedure SaveGeneralComment;
    procedure SaveDailyComment;
  end;

var
  ActionsForm: TActionsForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, Worker, Calendar, Settings, Mailer, Transactions, Send;

{$R *.dfm}

{ ####################################################### ! EXTENSION OF 'TSHAPE' CLASS ! ################################################################### }

{ --------------------------------------------------------------------------------------------------------------------------- CREATE PARAMETERS FOR COMPONENT }
procedure TEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  case Alignment of
    taLeftJustify:  Params.Style:=Params.Style or ES_LEFT   and not ES_MULTILINE;
    taRightJustify: Params.Style:=Params.Style or ES_RIGHT  and not ES_MULTILINE;
    taCenter:       Params.Style:=Params.Style or ES_CENTER and not ES_MULTILINE;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------ SET THE TEXT ALIGNMENT }
procedure TEdit.SetAlignment(value: TAlignment);
begin
  if FAlignment <> value then
  begin
    FAlignment:=value;
    RecreateWnd;
  end;
end;

{ ------------------------------------------------------------- ! SUPPORTING METHODS ! ---------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------- CHECK IF APPLICATION IS RUNNING }
function TActionsForm.GetRunningApps(SearchName: string): boolean;
var
  PE:        TProcessEntry32;
  Snap:      THandle;
  FileName:  string;
begin
  { INITIALIZE }
  Result:=False;
  { TAKE SNAPSHOT OF RUNNING APPLICATION }
  PE.dwSize:=SizeOf(PE);
  Snap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snap <> 0 then begin
    if Process32First(Snap, PE) then
    begin
      FileName:=string(PE.szExeFile);
      { GO THROUGH ALL THE ITEMS AND STOP IF MATCHED }
      while Process32Next(Snap, PE) do
      begin
        FileName:=string(PE.szExeFile);
        if LowerCase(FileName) = LowerCase(SearchName) then
        begin
          Result:=True;
          Break;
        end;
      end;
    end;
    CloseHandle(Snap);
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- GET ALL RELEVANT DATA }
procedure TActionsForm.GetData(OpenItemsDest: TStringGrid; HistoryDest: TStringGrid; OpenItemsSrc: TStringGrid);
var
  iCNT       :  integer;
  jCNT       :  integer;
  kCNT       :  integer;
  GenText    :  TDataTables;
  AddrBook   :  TDataTables;
  Phones     :  string;
  SL         :  TStringList;
begin

  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crSQLWait;
  OpenItemsDest.Freeze(True);
  HistoryDest.Freeze(True);
  OpenItemsDest.ClearAll(2, 1, 1, False);
  HistoryDest.ClearAll(2, 1, 1, False);
  kCNT:=1;

  { -------------------------------------------------------------------------------------------------------------------------------------- GET SOURCE COLUMNS }
  SrcColumns[0] :=OpenItemsSrc.ReturnColumn(TOpenitems.InvoNo,    1, 1);
  SrcColumns[1] :=OpenItemsSrc.ReturnColumn(TOpenitems.Txt,       1, 1);
  SrcColumns[2] :=OpenItemsSrc.ReturnColumn(TOpenitems.AddTxt,    1, 1);
  SrcColumns[3] :=OpenItemsSrc.ReturnColumn(TOpenitems.OpenAm,    1, 1);
  SrcColumns[4] :=OpenItemsSrc.ReturnColumn(TOpenitems.Am,        1, 1);
  SrcColumns[5] :=OpenItemsSrc.ReturnColumn(TOpenitems.OpenCurAm, 1, 1);
  SrcColumns[6] :=OpenItemsSrc.ReturnColumn(TOpenitems.CurAm,     1, 1);
  SrcColumns[7] :=OpenItemsSrc.ReturnColumn(TOpenitems.ISO,       1, 1);
  SrcColumns[8] :=OpenItemsSrc.ReturnColumn(TOpenitems.DueDt,     1, 1);
  SrcColumns[9] :=OpenItemsSrc.ReturnColumn(TOpenitems.ValDt,     1, 1);
  SrcColumns[10]:=OpenItemsSrc.ReturnColumn(TOpenitems.Ctrl,      1, 1);
  SrcColumns[11]:=OpenItemsSrc.ReturnColumn(TOpenitems.PmtStat,   1, 1);

  try
    { --------------------------------------------------------- ! LIST OF OPEN ITEMS ! ---------------------------------------------------------------------- }

    { ----------------------------------------------------------------------------------------------------------------------- LOAD OPEN ITEMS FROM "MAINFORM" }
    { LOOK FOR THE SAME "CUID" }
    for iCNT:=1 to OpenItemsSrc.RowCount - 1 do
    begin
      if OpenItemsSrc.Cells[MainForm.sgOpenItems.ReturnColumn(TOpenitems.CUID, 1, 1), iCNT] = CUID then
      begin
        { MOVE DATA FOR SELECTED COLUMNS AND GIVEN ROW ONCE "CUID" IS FOUND }
        for jCNT:=Low(SrcColumns) to High(SrcColumns)
          do OpenItemsDest.Cells[jCNT + 1, kCNT]:=OpenItemsSrc.Cells[SrcColumns[jCNT], iCNT];
        { MOVE NEXT }
        inc(kCNT);
        OpenItemsDest.RowCount:=kCNT;
      end;
    end;
    { ------------------------------------------------------------------------------------------------------------------- SORT VIA PAYMENT STATUS | ASCENDING }
    OpenItemsDest.MSort(9, 0, True);
    { -------------------------------------------------------------------------------------------------------------------------------- CUSTOMER NAME & NUMBER }
    Cust_Name.Caption  :=CustName;
    Cust_Number.Caption:=CustNumber;

    { ------------------------------------------------------------- ! CUSTOMER DATA ! ----------------------------------------------------------------------- }

    AddrBook:=TDataTables.Create(MainForm.DbConnect);
    try
      AddrBook.Columns.Add(TAddressBook.CONTACT);
      AddrBook.Columns.Add(TAddressBook.ESTATEMENTS);
      AddrBook.Columns.Add(TAddressBook.PHONE_NUMBERS);
      AddrBook.CustFilter:=WHERE + TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
      AddrBook.OpenTable(TblAddressbook);
      if AddrBook.DataSet.RecordCount = 1 then
      begin
        Cust_Person.Text:=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.CONTACT].Value);
        Cust_Mail.Text  :=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.ESTATEMENTS].Value);
        Phones:=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.PHONE_NUMBERS].Value);
        if (Phones <> '') or (Phones <> ' ') then
        begin
          Cust_Phone.Clear;
          { MANY NUMBERS DELIMITED BY SEMICOLON }
          if AnsiPos(deSemicolon, Phones) > 0 then
          begin
            SL:=TStringList.Create;
            try
              SL.Delimiter:=deSemicolon;
              SL.StrictDelimiter:=True;
              SL.DelimitedText:=Phones;
              for iCNT:=0 to SL.Count - 1 do
                Cust_Phone.Items.Add(SL.Strings[iCNT]);
            finally
              SL.Free;
            end;
          end
          else
          { JUST ONE TELEPHONE NUMBER }
          begin
            Cust_Phone.Items.Add(Phones);
          end;
          Cust_Phone.ItemIndex:=0;
        end;
      end;
    finally
      AddrBook.Free;
    end;

    { ------------------------------------------------------ ! HISTORY OF DAILY COMMENTS ! ------------------------------------------------------------------ }

    UpdateHistory(HistoryDest);

    { ----------------------------------------------------------- ! GENERAL COMMENTS ! ---------------------------------------------------------------------- }

    GenText:=TDataTables.Create(MainForm.DbConnect);
    try
      GenText.OpenTable(TblGeneral);
      GenText.DataSet.Filter:=TGeneral.CUID + EQUAL + QuotedStr(CUID);
      if not (GenText.DataSet.EOF) then GeneralCom.Text:=MainForm.OleGetStr(GenText.DataSet.Fields[TGeneral.FIXCOMMENT].Value);
    finally
      GenText.Free;
    end;

  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  finally
    OpenItemsDest.AutoThumbSize;
    HistoryDest.AutoThumbSize;
    OpenItemsDest.SetColWidth(10, 20);
    HistoryDest.SetColWidth(10, 20);
    OpenItemsDest.Freeze(False);
    HistoryDest.Freeze(False);
    Screen.Cursor:=crDefault;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- REFRESH HISTORY GRID }
procedure TActionsForm.UpdateHistory(Grid: TStringGrid);
var
  DailyText: TDataTables;
begin
    DailyText:=TDataTables.Create(MainForm.DbConnect);
    try
      DailyText.OpenTable(TblDaily);
      DailyText.DataSet.Filter:=TDaily.CUID + EQUAL + QuotedStr(CUID);
      DailyText.DataSet.Sort:=TDaily.STAMP + DESC;
      if not (DailyText.DataSet.EOF) then
      begin
        DailyText.SqlToGrid(Grid, DailyText.DataSet, False, True);
        SetHistoryCols(Grid);
      end;
    finally
      DailyText.Free;
    end;
end;

{ --------------------------------------------------------------------------------------------------------------------------- HIDE IRELEVANT FOR USER COLUMNS }
procedure TActionsForm.SetHistoryCols(Grid: TStringGrid);
begin
  Grid.ColCount:=11;
  Grid.ColWidths[1] := -1;
  Grid.ColWidths[2] := -1;
  Grid.ColWidths[3] := -1;
  Grid.ColWidths[7] := -1;
  Grid.ColWidths[8] := -1;
  Grid.ColWidths[9] := -1;
  Grid.ColWidths[10]:= -1;
  Grid.SetColWidth(10, 20);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- ENABLE/DISABLE CONTROLS }
procedure TActionsForm.SetControls;
begin
  { DISABLE SAVE BUTTON IF CUSTOMER IS NOT REGISTERED }
  if
    (Cust_Person.Text = unNotFound)
  or
    (Cust_Mail.Text = unNotFound)
  then
    btnSaveCustDetails.Enabled:=False else btnSaveCustDetails.Enabled:=True;
  { DISABLE TEXT FIELDS IF CUSTOMER IS NOT REGISTERED }
  if Cust_Phone.Text  = unNotFound then Cust_Phone.Enabled :=False else Cust_Phone.Enabled :=True;
  if Cust_Mail.Text   = unNotFound then Cust_Mail.Enabled  :=False else Cust_Mail.Enabled  :=True;
  if Cust_Person.Text = unNotFound then Cust_Person.Enabled:=False else Cust_Person.Enabled:=True;
end;

{ --------------------------------------------------------------------------------------------------------------------------------- PREPARE FOR NEW DATA LOAD }
procedure TActionsForm.Initialize;
begin
  { CLEAR ALL }
  ClearAll;
  { ASSIGN DATA FOR GIVEN ROW }
  CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), MainForm.sgAgeView.Row];
  CustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), MainForm.sgAgeView.Row];
  CustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), MainForm.sgAgeView.Row];
  CoCode    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,         1, 1), MainForm.sgAgeView.Row];
  SCUID     :=CustNumber + MainForm.ConvertName(CoCode, 'F', 3);
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- CLEAR ALL DETAILS }
procedure TActionsForm.ClearAll;
begin
  Cust_Name.Caption   :=unNotFound;
  Cust_Number.Caption :=unNotFound;
  Cust_Person.Text    :=unNotFound;
  Cust_Mail.Text      :=unNotFound;
  Cust_Phone.Clear;
  Cust_Phone.Items.Add(unNotFound);
  Cust_Phone.ItemIndex:=0;
  DailyCom.Text       :='';
  GeneralCom.Text     :='';
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- MAKE PHONE CALL }
procedure TActionsForm.MakePhoneCall;
var
  AppSettings:  TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { CHECK FOR 'LYNCCALL.EXE' }
    if not FileExists(AppSettings.FAppDir + LyncCall) then
    begin
      MainForm.MsgCall(mcError, APPNAME + ' cannot find ''lynccall.exe''. Please contact IT support.');
      Exit;
    end;
    { CHECK IF LYNC/SKYPE IS RUNNING }
    if not ActionsForm.GetRunningApps('lync.exe') then
    begin
      MainForm.MsgCall(mcError, APPNAME + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
      Exit;
    end;
    { RUN LYNC WITH GIVEN PHONE NUMBER }
    ShellExecute(ActionsForm.Handle, 'open', PChar(AppSettings.FAppDir + LyncCall), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);
    if ActionsForm.DailyCom.Text = '' then ActionsForm.DailyCom.Text:='Called customer today.'
      else
        ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + CRLF + 'Called customer today.';
  finally
    AppSettings.Free;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------- LOAD NEXT OR PREVIOUS CUSTOMER }
procedure TActionsForm.LoadCustomer(Direction: integer);

  (* COMMON VARIABLES *)

  var
    iCNT:  integer;

  (* NESTED METHOD *)

  function CheckRow(iterator: integer): boolean;
  begin
    Result:=True;
    if
      (MainForm.sgAgeView.RowHeights[iterator] <> -1)
    and
      (MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fOVERDUE, 1, 1), iterator] <> '0')
    then
    begin
      MainForm.sgAgeView.Row:=iterator;
      Result:=False;
    end;
  end;

begin

  { LOOP TO THE PREVIOUS UNHIDDEN ROW }
  if Direction = loadPrev then
    for iCNT:=(MainForm.sgAgeView.Row - 1) Downto 1 do
      if not CheckRow(iCNT) then Break;

  { LOOP TO THE NEXT UNHIDDEN ROW }
  if Direction = loadNext then
    for iCNT:=(MainForm.sgAgeView.Row + 1) to MainForm.sgAgeView.RowCount - 1 do
      if not CheckRow(iCNT) then Break;

  { LOAD NEW DATA }
  Initialize;
  try
    GetData(OpenItemsGrid, HistoryGrid, MainForm.sgOpenItems);
    SetControls;
  except
    MainForm.MsgCall(mcWarn, 'Unexpected error has occured. Please close the window and try again.');
  end;

end;

{ ------------------------------------------------------------------------------------------------------------------------------------ SEND ACCOUNT STATEMENT }
procedure TActionsForm.SendAccountStatement(Layout: integer; Salut: string; Mess: string);
var
  Statement:   TDocument;
  AppSettings: TSettings;
begin
  { PROCEED }
  Statement    :=TDocument.Create;
  AppSettings  :=TSettings.Create;
  Screen.Cursor:=crSQLWait;
  try
    { SETUP CUSTOMER AND COMPANY }
    Statement.CUID    :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,          1, 1), MainForm.sgAgeView.Row];
    Statement.CustName:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME, 1, 1), MainForm.sgAgeView.Row];
    Statement.CoCode  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCO_CODE,       1, 1), MainForm.sgAgeView.Row];
    Statement.Branch  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fAGENT,         1, 1), MainForm.sgAgeView.Row];
    Statement.SCUID   :=SCUID;
    { SET OPEN ITEMS GRID }
    Statement.OpenItems:=MainForm.sgOpenItems;
    { GET HTML LAYOUT }
    Statement.DocType  :=dcStatement;
    Statement.CustSalut:=Salut;
    Statement.CustMess :=Mess;
    { USE FULLY PRE-DEFINED TEMPLATE }
    if Layout = maDefined then
    begin
      Statement.HTMLLayout:=Statement.LoadTemplate(AppSettings.FLayoutDir + AppSettings.TMIG.ReadString(VariousLayouts, 'STATEMENT', '') + '.html');
    end;
    { USE PRE-DEFINED TEMPLATE WITH TWO CUSTOM FILEDS }
    if Layout = maCustom then
    begin
      Statement.HTMLLayout:=Statement.LoadTemplate(AppSettings.FLayoutDir + AppSettings.TMIG.ReadString(VariousLayouts, 'CUSTSTATEMENT', '') + '.html');
    end;
    { SEND STATEMENT }
    Statement.MailSubject:='Account Statement (' + Statement.CustName + ')';
    if Statement.SendDocument then
      MainForm.ExecMessage(False, mcInfo, 'Account Statement has been sent successfully!')
        else
          MainForm.ExecMessage(False, mcError, 'Account Statement cannot be sent. Please contact IT support.');
    RegisterAction;
  finally
    AppSettings.Free;
    Statement.Free;
    Screen.Cursor:=crDefault;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------- REGISTER THIS ACTION IN DATABASE }
procedure TActionsForm.RegisterAction;
var
  DailyText:   TDataTables;
  Condition:   string;
  ManuStat:    integer;
begin
  { REGISTER THIS ACTION IN DATABASE }
  DailyText:=TDataTables.Create(MainForm.DbConnect);
  try
    DailyText.OpenTable(TblDaily);
    Condition:=TDaily.CUID + EQUAL + QuotedStr(CUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
    DailyText.DataSet.Filter:=Condition;
    { UPDATE EXISTING COMMENT }
    if not (DailyText.DataSet.RecordCount = 0) then
    begin
      { GET MANUAL STATEMENTS SENT AND INCREASE BY ONE }
      ManuStat:=StrToIntDef(MainForm.OleGetStr(DailyText.DataSet.Fields[TDaily.EMAIL_ManuStat].Value), 0);
      Inc(ManuStat);
      DailyText.CleanUp;
      { DEFINE COLUMNS, VALUES AND CONDITIONS }
      DailyText.Columns.Add(TDaily.STAMP);           DailyText.Values.Add(DateTimeToStr(Now));               DailyText.Conditions.Add(Condition);
      DailyText.Columns.Add(TDaily.USER_ALIAS);      DailyText.Values.Add(UpperCase(MainForm.WinUserName));  DailyText.Conditions.Add(Condition);
      DailyText.Columns.Add(TDaily.EMAIL_ManuStat);  DailyText.Values.Add(IntToStr(ManuStat));               DailyText.Conditions.Add(Condition);
      { EXECUTE }
      DailyText.UpdateRecord(TblDaily);
    end
    else
    { INSERT NEW RECORD }
    begin
      DailyText.CleanUp;
      { DEFINE COLUMNS AND VALUES }
      DailyText.Columns.Add(TDaily.GROUP_ID);       DailyText.Values.Add(MainForm.GroupIdSel);
      DailyText.Columns.Add(TDaily.CUID);           DailyText.Values.Add(CUID);
      DailyText.Columns.Add(TDaily.AGEDATE);        DailyText.Values.Add(MainForm.AgeDateSel);
      DailyText.Columns.Add(TDaily.STAMP);          DailyText.Values.Add(DateTimeToStr(Now));
      DailyText.Columns.Add(TDaily.USER_ALIAS);     DailyText.Values.Add(UpperCase(MainForm.WinUserName));
      DailyText.Columns.Add(TDaily.EMAIL);          DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.CALLEVENT);      DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.CALLDURATION);   DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.FIXCOMMENT);     DailyText.Values.Add('');
      DailyText.Columns.Add(TDaily.EMAIL_Reminder); DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.EMAIL_AutoStat); DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.EMAIL_ManuStat); DailyText.Values.Add('1');
      { EXECUTE }
      DailyText.InsertInto(TblDaily);
    end;
  finally
    DailyText.Free;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------- CLEAR FOLLOW-UP FROM GIVEN CUSTOMER }
procedure TActionsForm.ClearFollowUp;
var
  GeneralText: TDataTables;
  Condition:   string;
begin
  if MainForm.MsgCall(mcQuestion2, 'Are you sure you want to clear this follow up?') = ID_YES then
  begin
    GeneralText:=TDataTables.Create(MainForm.DbConnect);
    try
      Condition:=TGeneral.CUID + EQUAL + QuotedStr(CUID);
      GeneralText.Columns.Add(TGeneral.FOLLOWUP); GeneralText.Values.Add(SPACE); GeneralText.Conditions.Add(Condition);
      GeneralText.UpdateRecord(TblGeneral);
      MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneral.fFOLLOWUP, 1, 1), MainForm.sgAgeView.Row]:='';
    finally
      GeneralText.Free;
    end;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- SAVE SOME CUSTOMER DETAILS }
procedure TActionsForm.SaveCustomerDetails;
var
  AddrBook:  TDataTables;
  Condition: string;
begin
  AddrBook:=TDataTables.Create(MainForm.DbConnect);
  try
    AddrBook.OpenTable(TblAddressbook);
    Condition:=TAddressBook.SCUID + EQUAL + QuotedStr(SCUID);
    AddrBook.DataSet.Filter:=Condition;
    if not (AddrBook.DataSet.RecordCount = 0) then
    begin
      AddrBook.CleanUp;
      { UPDATE DATA }
      AddrBook.Columns.Add(TAddressBook.CONTACT);        AddrBook.Values.Add(Cust_Person.Text); AddrBook.Conditions.Add(Condition);
      AddrBook.Columns.Add(TAddressBook.ESTATEMENTS);    AddrBook.Values.Add(Cust_Mail.Text);   AddrBook.Conditions.Add(Condition);
      AddrBook.Columns.Add(TAddressBook.PHONE_NUMBERS);  AddrBook.Values.Add(Cust_Phone.Text);  AddrBook.Conditions.Add(Condition);
      { EXECUTE }
      if not (AddrBook.UpdateRecord(TblAddressbook)) then
        MainForm.MsgCall(mcWarn, 'Cannot save customer details. Please contact IT support.')
          else
            MainForm.MsgCall(mcInfo, 'Changes have been updated successfully.');
    end;
  finally
    AddrBook.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------ SAVE GENERAL COMMENT INTO DATABASE }
procedure TActionsForm.SaveGeneralComment;
var
  GenText:   TDataTables;
  Condition: string;
begin
  GenText:=TDataTables.Create(MainForm.DbConnect);
  try
    GenText.OpenTable(TblGeneral);
    Condition:=TGeneral.CUID + EQUAL + QuotedStr(CUID);
    GenText.DataSet.Filter:=Condition;
    { UPDATE }
    if not (GenText.DataSet.RecordCount = 0) then
    begin
      GenText.CleanUp;
      { DEFINE COLUMNS, VALUES AND CONDITIONS }
      GenText.Columns.Add(TGeneral.STAMP);        GenText.Values.Add(DateTimeToStr(Now));             GenText.Conditions.Add(Condition);
      GenText.Columns.Add(TGeneral.USER_ALIAS);   GenText.Values.Add(UpperCase(MainForm.WinUserName));GenText.Conditions.Add(Condition);
      GenText.Columns.Add(TGeneral.FIXCOMMENT);   GenText.Values.Add(GeneralCom.Text);                GenText.Conditions.Add(Condition);
      { EXECUTE }
      GenText.UpdateRecord(TblGeneral);
    end
    else
    { INSERT NEW }
    begin
      GenText.CleanUp;
      { DEFINE COLUMNS AND VALUES }
      GenText.Columns.Add(TGeneral.CUID);       GenText.Values.Add(CUID);
      GenText.Columns.Add(TGeneral.STAMP);      GenText.Values.Add(DateTimeToStr(Now));
      GenText.Columns.Add(TGeneral.USER_ALIAS); GenText.Values.Add(UpperCase(MainForm.WinUserName));
      GenText.Columns.Add(TGeneral.FIXCOMMENT); GenText.Values.Add(GeneralCom.Text);
      GenText.Columns.Add(TGeneral.FOLLOWUP);   GenText.Values.Add('');
      { EXECUTE }
      GenText.InsertInto(TblGeneral);
    end;
  finally
    GenText.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------- SAVE DAILY COMMENT INTO DATABASE }
procedure TActionsForm.SaveDailyComment;
var
  DailyText: TDataTables;
  UpdateOK:  boolean;
  InsertOK:  boolean;
  Condition: string;
begin
  UpdateOK:=False;
  InsertOK:=False;
  DailyText:=TDataTables.Create(MainForm.DbConnect);
  try
    DailyText.OpenTable(TblDaily);
    Condition:=TDaily.CUID + EQUAL + QuotedStr(CUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
    DailyText.DataSet.Filter:=Condition;
    { UPDATE EXISTING COMMENT }
    if not (DailyText.DataSet.RecordCount = 0) then
    begin
      DailyText.CleanUp;
      { DEFINE COLUMNS, VALUES AND CONDITIONS }
      DailyText.Columns.Add(TDaily.STAMP);       DailyText.Values.Add(DateTimeToStr(Now));               DailyText.Conditions.Add(Condition);
      DailyText.Columns.Add(TDaily.USER_ALIAS);  DailyText.Values.Add(UpperCase(MainForm.WinUserName));  DailyText.Conditions.Add(Condition);
      DailyText.Columns.Add(TDaily.FIXCOMMENT);  DailyText.Values.Add(DailyCom.Text);                    DailyText.Conditions.Add(Condition);
      { EXECUTE }
      UpdateOK:=DailyText.UpdateRecord(TblDaily);
    end
    else
    { INSERT NEW RECORD }
    begin
      DailyText.CleanUp;
      { DEFINE COLUMNS AND VALUES }
      DailyText.Columns.Add(TDaily.GROUP_ID);       DailyText.Values.Add(MainForm.GroupIdSel);
      DailyText.Columns.Add(TDaily.CUID);           DailyText.Values.Add(CUID);
      DailyText.Columns.Add(TDaily.AGEDATE);        DailyText.Values.Add(MainForm.AgeDateSel);
      DailyText.Columns.Add(TDaily.STAMP);          DailyText.Values.Add(DateTimeToStr(Now));
      DailyText.Columns.Add(TDaily.USER_ALIAS);     DailyText.Values.Add(UpperCase(MainForm.WinUserName));
      DailyText.Columns.Add(TDaily.EMAIL);          DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.CALLEVENT);      DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.CALLDURATION);   DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.FIXCOMMENT);     DailyText.Values.Add(DailyCom.Text);
      DailyText.Columns.Add(TDaily.EMAIL_Reminder); DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.EMAIL_AutoStat); DailyText.Values.Add('0');
      DailyText.Columns.Add(TDaily.EMAIL_ManuStat); DailyText.Values.Add('0');
      { EXECUTE }
      InsertOK:=DailyText.InsertInto(TblDaily);
    end;
    { REFRESH HISTORY GRID }
    if (InsertOK) or (UpdateOK) then UpdateHistory(HistoryGrid);
  finally
    DailyText.Free;
  end;
end;

{ ############################################################ ! MAIN THREAD EVENTS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TActionsForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { ----------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
    ActionsForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_TRANSACTIONS', APPNAME);
    { -------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS STRING GRID }
    SetLength(SrcColumns, 12);
    OpenItemsGrid.RowCount:=2;
    OpenItemsGrid.ColCount:=13;
    OpenItemsGrid.Cols[0].Text :='';
    OpenItemsGrid.Cols[1].Text :='Invoice Number';
    OpenItemsGrid.Cols[2].Text :='Text';
    OpenItemsGrid.Cols[3].Text :='Add. Text';
    OpenItemsGrid.Cols[4].Text :='Open Amount';
    OpenItemsGrid.Cols[5].Text :='Amount';
    OpenItemsGrid.Cols[6].Text :='Open Cur. Amount';
    OpenItemsGrid.Cols[7].Text :='Currency Amount';
    OpenItemsGrid.Cols[8].Text :='Currency';
    OpenItemsGrid.Cols[9].Text :='Due Date';
    OpenItemsGrid.Cols[10].Text:='Value Date';
    OpenItemsGrid.Cols[11].Text:='Control Status';
    OpenItemsGrid.Cols[12].Text:='Payment Status';
  finally
    AppSettings.Free;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------- HISTORY STRING GRID }
  HistoryGrid.RowCount:=2;
  SetHistoryCols(HistoryGrid);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TActionsForm.FormShow(Sender: TObject);
begin
  Initialize;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE }
procedure TActionsForm.FormActivate(Sender: TObject);
begin
  GetData(OpenItemsGrid, HistoryGrid, MainForm.sgOpenItems);
  SimpleText.Caption:='Open items last update: ' + MainForm.OpenItemsUpdate + '.';
  SetControls;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- QUIT EDITING ON CLOSE }
procedure TActionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=True;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ ON DESTROY }
procedure TActionsForm.FormDestroy(Sender: TObject);
begin
  { DO NOTHING }
end;

{ ---------------------------------------------------------- ! COMPONENT EVENTS | EVENTS ! ------------------------------------------------------------------ }

{ ----------------------------------------------------------------------------------------------------------------------------------------- DRAW SELECTED ROW }
procedure TActionsForm.HistoryGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  HistoryGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);
end;

{ ------------------------------------------------------------------------------------------------------------------------------- COLOR NUMBERS AND SELECTION }
procedure TActionsForm.OpenItemsGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin

  (* CALL DRAWSELECTED BEFORE COLORVALUES *)

  { DRAW SELECTED | SKIP HEADERS }
  OpenItemsGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, SELCOLOR, clBlack, clWhite, True);

  { ONLY FOR COLUMNS 2..5 AND 12 }
  if ( ( (ACol >= 3) and (ACol <= 8) or (ACol = 12) ) and (ARow > 0) ) then OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);

end;

{ ---------------------------------------------------------------------------------------------------------------------------------- OPEN ITEMS DATE AND TIME }
procedure TActionsForm.OpenItemsGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  { DO NOTHING }
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SHOW DATA WHEN SELECTED }
procedure TActionsForm.HistoryGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  DailyCom.Text:=HistoryGrid.Cells[HistoryGrid.ReturnColumn(TDaily.FIXCOMMENT, 1, 1), ARow];
end;

{ --------------------------------------------------------------- ! KEYBOARD EVENTS ! ----------------------------------------------------------------------- }

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE ON <ESC> }
procedure TActionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ESC then Close;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENT ON <ENTER> }
procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { NEW LINE }
  if (Key = VK_RETURN) and (Shift=[ssALT]) then
  begin
    DailyCom.Lines.Add(CRLF);
    Exit;
  end;
  { SAVE TO DB }
  if (Key = VK_RETURN) then
  begin
    if DailyCom.Text <> '' then SaveDailyComment;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENT ON <ENTER> }
procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { NEW LINE }
  if (Key = VK_RETURN) and (Shift=[ssALT]) then
  begin
    GeneralCom.Lines.Add(CRLF);
    Exit;
  end;
  { SAVE TO DB }
  if (Key = VK_RETURN) then
  begin
    if GeneralCom.Text <> '' then SaveGeneralComment;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------- COPY STRING GRID CONTENT TO CLIPBOARD }
procedure TActionsForm.OpenItemsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then OpenItemsGrid.CopyCutPaste(adCopy);
end;

{ --------------------------------------------------------------------------------------------------------------------- COPY STRING GRID CONTENT TO CLIPBOARD }
procedure TActionsForm.HistoryGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 67) and (Shift = [ssCtrl]) then HistoryGrid.CopyCutPaste(adCopy);
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

{ ---------------------------------------------------------------------------------------------------------------------------------------- EDIT PHONE NUMBERS }
procedure TActionsForm.btnEditClick(Sender: TObject);
begin
  //
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- SAVE CUSTOMER DETAILS }
procedure TActionsForm.btnSaveCustDetailsClick(Sender: TObject);
begin
  SaveCustomerDetails;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- COPY TO CLIPBOARD }
procedure TActionsForm.btnCopyCustNameClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Name.Caption;
end;

procedure TActionsForm.btnCopyCustNumberClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Number.Caption;
end;

procedure TActionsForm.btnCopyPersonClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Person.Text;
end;

procedure TActionsForm.btnCopyEmailClick(Sender: TObject);
begin
  ClipBoard.AsText:=Cust_Mail.Text;
end;

{ -------------------------------------------------------------------------------------------------------------------------- SELECT PREVIOUS OVERDUE CUSTOMER }
procedure TActionsForm.btnBackClick(Sender: TObject);
begin
  LoadCustomer(loadPrev);
end;

{ ------------------------------------------------------------------------------------------------------------------------------ SELECT NEXT OVERDUE CUSTOMER }
procedure TActionsForm.btnNextClick(Sender: TObject);
begin
  LoadCustomer(loadNext);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- FEEDBACK AFTER CALL }
procedure TActionsForm.btnFeedbackClick(Sender: TObject);
begin
  CalendarForm.CalendarMode:=cfDateToDB;
  MainForm.WndCall(CalendarForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- REMOVE FOLLOW UP DATE }
procedure TActionsForm.btnClearFollowUpClick(Sender: TObject);
begin
  ClearFollowUp;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- SEND MANUAL EMAIL }
procedure TActionsForm.btnSendEmailClick(Sender: TObject);
begin
  MainForm.WndCall(SendForm, 0);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
procedure TActionsForm.btnSendStatementClick(Sender: TObject);
begin
  { ASK USER BEFORE SENDING THE EMAIL }
  if MainForm.MsgCall(mcQuestion2, 'Are you absolutely sure that you really want it to be sent, right now?') = IDNO then Exit;
  SendAccountStatement(maDefined, '', '');
  RegisterAction;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- CALL CUSTOMER }
procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
  MakePhoneCall;
end;

end.
