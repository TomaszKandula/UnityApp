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
unit Actions;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,  Dialogs, Grids, Buttons, ExtCtrls, ComCtrls, StdCtrls, ADODB, StrUtils, ShellApi,
  TLHelp32, pngimage, ImgList, GIFImg, Clipbrd, Main;

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
    btnSendEmail: TSpeedButton;
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
  private
    pCUID       :  string;
    pCustName   :  string;
    pCustNumber :  string;
  public
    var         IsEdit    :  boolean;
    property    CUID      :  string read pCUID       write pCUID;
    property    CustName  :  string read pCustName   write pCustName;
    property    CustNumber:  string read pCustNumber write pCustNumber;
  published

    //procedure   EditDetails; AB
    //procedure   SaveDetails; AB

    //procedure   SendStatement; IT

    function  GetRunningApps(SearchName: string): boolean;
    procedure GetData(OpenItemsDest: TStringGrid; HistoryDest: TStringGrid; OpenItemsSrc: TStringGrid);
    procedure MakePhoneCall;

  end;

var
  ActionsForm: TActionsForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, SQL, Worker, Calendar, Settings, Database, Mailer, Transactions;

{$R *.dfm}

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
const
  { BELOW ARRAY CONTAINS WITH COLUMN NUMBERS OF 'STRINGGRID' THAT KEEPS OUR }
  { SOURCE DATA THAT WILL BE TRANSFERRED TO 'OPENITEMSGRID'. THIS COMPONENT }
  { SHOWS SELECTED DETAILS OF ALL REGISTERED OPEN ITEMS FOR GIVEN CUSTOMER  }
  { THAT WE IDENTIFY BY 'CUID' NUMBER.                                      }
  SrcColumns:  array[0..11] of integer = (10, 29, 32, 5, 9, 4, 8, 7, 11, 26, 19, 33);
var
  iCNT:      integer;
  jCNT:      integer;
  zCNT:      integer;
  DailyText: TDataTables;
  GenText:   TDataTables;
  AddrBook:  TDataTables;
begin

  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  OpenItemsDest.Freeze(True);
  HistoryDest.Freeze(True);
  OpenItemsDest.ClearAll(2, 1, 1, False);
  HistoryDest.ClearAll(2, 1, 1, False);
  Screen.Cursor:=crHourGlass;
  zCNT:=1;
  try
    { --------------------------------------------------------- ! LIST OF OPEN ITEMS ! ---------------------------------------------------------------------- }

    { ----------------------------------------------------------------------------------------------------------------------- LOAD OPEN ITEMS FROM "MAINFORM" }
    if OpenItemsSrc.RowCount > 2 then
    begin
      { LOOK FOR THE SAME "CUID" }
      for iCNT:=1 to OpenItemsSrc.RowCount - 1 do
      begin
        if OpenItemsSrc.Cells[37, iCNT] = CUID then
        begin
          { MOVE DATA FOR SELECTED COLUMNS AND GIVEN ROW ONCE "CUID" IS FOUND }
          for jCNT:=Low(SrcColumns) to High(SrcColumns)
            do OpenItemsDest.Cells[jCNT + 1, zCNT]:=OpenItemsSrc.Cells[SrcColumns[jCNT], iCNT];
          { MOVE NEXT }
          inc(zCNT);
          OpenItemsDest.RowCount:=zCNT;
        end;
      end;
    end;
    { ------------------------------------------------------------------------------------------------------------------- SORT VIA PAYMENT STATUS | ASCENDING }
    OpenItemsDest.MSort(11, 1, True);
    { -------------------------------------------------------------------------------------------------------------------------------- CUSTOMER NAME & NUMBER }
    Cust_Name.Caption  :=CustName;
    Cust_Number.Caption:=CustNumber;

    { ----------------------------------------------------------- ! CUSTOMER DATA ! ------------------------------------------------------------------------- }

    AddrBook:=TDataTables.Create(MainForm.FDbConnect);
    try
      AddrBook.Columns.Add(TAddressBook.CONTACT);
      AddrBook.Columns.Add(TAddressBook.ESTATEMENTS);
      AddrBook.Columns.Add(TAddressBook.TELEPHONE);
      AddrBook.CustFilter:=WHERE + TAddressBook.CUID + EQUAL + QuotedStr(CUID);
      AddrBook.OpenTable(TblAddressbook);
      if AddrBook.DataSet.RecordCount = 1 then
      begin
        Cust_Person.Text:=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.CONTACT].Value);
        Cust_Mail.Text  :=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.ESTATEMENTS].Value);
        Cust_Phone.Text :=MainForm.OleGetStr(AddrBook.DataSet.Fields[TAddressBook.TELEPHONE].Value);
        Cust_Phone.Text:=StringReplace(Cust_Phone.Text, CRLF, ' ', [rfReplaceAll]);
      end;
    finally
      AddrBook.Free;
    end;

    { ------------------------------------------------------ ! HISTORY OF DAILY COMMENTS ! ------------------------------------------------------------------ }

    DailyText:=TDataTables.Create(MainForm.FDbConnect);
    try
      DailyText.OpenTable(TblDaily);
      DailyText.DataSet.Filter:=TDaily.CUID + EQUAL + QuotedStr(CUID);
      DailyText.DataSet.Sort:=TDaily.STAMP + DESC;
      if not (DailyText.DataSet.EOF) then DailyText.SqlToGrid(HistoryDest, DailyText.DataSet, False, True);
    finally
      DailyText.Free;
    end;

    { ----------------------------------------------------------- ! GENERAL COMMENTS ! ---------------------------------------------------------------------- }

    GenText:=TDataTables.Create(MainForm.FDbConnect);
    try
      GenText.OpenTable(TblGeneral);
      GenText.DataSet.Filter:=TGeneral.CUID + EQUAL + QuotedStr(CUID);
      if not (GenText.DataSet.EOF) then GeneralCom.Text:=MainForm.OleGetStr(GenText.DataSet.Fields[TGeneral.FIXCOMMENT].Value);
    finally
      GenText.Free;
    end;

  { -------------------------------------------------------------------------------------------------------------------------------------------- UNINITIALIZE }
  finally
    OpenItemsDest.Freeze(False);
    HistoryDest.Freeze(False);
    OpenItemsDest.AutoThumbSize;
    HistoryDest.AutoThumbSize;
    OpenItemsDest.SetColWidth(10, 20);
    HistoryDest.SetColWidth(10, 20);
    Screen.Cursor:=crDefault;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------- MAKE PHONE CALL }
procedure TActionsForm.MakePhoneCall;
var
  AppSettings:  TSettings;
begin
  AppSettings:=TSettings.Create;
  try
    { CHECK FOR 'LYNCCALL.EXE' }
    if not FileExists(AppSettings.AppDir + LyncCall) then
    begin
      MainForm.MsgCall(3, APPNAME + ' cannot find ''lynccall.exe''. Please contact IT support.');
      Exit;
    end;
    { CHECK IF LYNC/SKYPE IS RUNNING }
    if not ActionsForm.GetRunningApps('lync.exe') then
    begin
      MainForm.MsgCall(3, APPNAME + ' cannot find running Microsoft Skype/Lync for Business. Please open it and try again.');
      Exit;
    end;
    { RUN LYNC WITH GIVEN PHONE NUMBER }
    ShellExecute(ActionsForm.Handle, 'open', PChar(AppSettings.AppDir + LyncCall), PChar(ActionsForm.Cust_Phone.Text), nil, SW_SHOWNORMAL);
    if ActionsForm.DailyCom.Text = '' then ActionsForm.DailyCom.Text:='Called customer today.'
      else
        ActionsForm.DailyCom.Text:=ActionsForm.DailyCom.Text + CRLF + 'Called customer today.';
  finally
    AppSettings.Free;
  end;
end;

{ ----------------------------------------------------------- ! MAIN THREAD METHODS ! ----------------------------------------------------------------------- }

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
    OpenItemsGrid.RowCount:=2;
    OpenItemsGrid.ColCount:=13;
    OpenItemsGrid.Cols[0].Text :='';
    OpenItemsGrid.Cols[1].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER10', 'InvoNo');
    OpenItemsGrid.Cols[2].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER29', 'Txt');
    OpenItemsGrid.Cols[3].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER32', 'AddTxt');
    OpenItemsGrid.Cols[4].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER5',  'OpenAm');
    OpenItemsGrid.Cols[5].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER9',  'Am');
    OpenItemsGrid.Cols[6].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER4',  'OpenCurAm');
    OpenItemsGrid.Cols[7].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER8',  'CurAm');
    OpenItemsGrid.Cols[8].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER7',  'ISO');
    OpenItemsGrid.Cols[9].Text :=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER11', 'DueDt');
    OpenItemsGrid.Cols[10].Text:=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER26', 'ValDt');
    OpenItemsGrid.Cols[11].Text:=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER19', 'Ctrl');
    OpenItemsGrid.Cols[12].Text:=AppSettings.TMIG.ReadString(OpenItemsData, 'HEADER33', 'PmtStat');
  finally
    AppSettings.Free;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------- HISTORY STRING GRID }
  HistoryGrid.RowCount:=2;
  HistoryGrid.ColCount:=11;
  { HIDE IRELEVANT FOR USER COLUMNS }
  HistoryGrid.ColWidths[1] := -1;
  HistoryGrid.ColWidths[2] := -1;
  HistoryGrid.ColWidths[3] := -1;
  HistoryGrid.ColWidths[7] := -1;
  HistoryGrid.ColWidths[8] := -1;
  HistoryGrid.ColWidths[9] := -1;
  HistoryGrid.ColWidths[10]:= -1;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TActionsForm.FormShow(Sender: TObject);
begin
  { CAPTIONS AND MEMO }
  Cust_Name.Caption   :=unNotFound;
  Cust_Number.Caption :=unNotFound;
  Cust_Person.Text    :=unNotFound;
  Cust_Mail.Text      :=unNotFound;
  Cust_Phone.Text     :=unNotFound;
  DailyCom.Text       :='';
  GeneralCom.Text     :='';
  StatusBar.SimpleText:='';
  { CUSTOMER KEY INFORMATION }
  CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), MainForm.sgAgeView.Row];
  CustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), MainForm.sgAgeView.Row];
  CustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), MainForm.sgAgeView.Row];
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE }
procedure TActionsForm.FormActivate(Sender: TObject);
begin
  GetData(OpenItemsGrid, HistoryGrid, MainForm.sgOpenItems);
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

  { ONLY FOR COLUMNS 2..5 }
  if ( (ACol >= 3) and (ACol <= 8) and (ARow > 0) ) then OpenItemsGrid.ColorValues(ARow, ACol, Rect, clRed, clBlack);

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
begin
  DailyCom.Text:=HistoryGrid.Cells[HistoryGrid.ReturnColumn(TDaily.FIXCOMMENT, 1, 1), ARow];
end;

{ --------------------------------------------------------------- ! KEYBOARD EVENTS ! ----------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENT ON <ENTER> }
procedure TActionsForm.DailyComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  DailyText: TDataTables;
  UpdateOK:  boolean;
  InsertOK:  boolean;
  Condition: string;
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
    if DailyCom.Text <> '' then
    begin
      UpdateOK:=False;
      InsertOK:=False;
      DailyText:=TDataTables.Create(MainForm.FDbConnect);
      try
        DailyText.OpenTable(TblDaily);
        Condition:=TDaily.CUID + EQUAL + QuotedStr(CUID) + _AND + TDaily.AGEDATE + EQUAL + QuotedStr(MainForm.AgeDateSel);
        DailyText.DataSet.Filter:=Condition;
        { UPDATE EXISTING COMMENT }
        if not (DailyText.DataSet.RecordCount = 0) then
        begin
          { STAMP }
          UpdateOK:=DailyText.UpdateRecord
          (
            TblDaily,
            TDaily.STAMP,
            DateTimeToStr(Now),
            Condition
          );
          { USER_ALIAS }
          UpdateOK:=DailyText.UpdateRecord
          (
            TblDaily,
            TDaily.USER_ALIAS,
            UpperCase(MainForm.FUserName),
            Condition
          );
          { COMMENT }
          UpdateOK:=DailyText.UpdateRecord
          (
            TblDaily,
            TDaily.FIXCOMMENT,
            DailyCom.Text,
            Condition
          );
        end
        else
        { INSERT NEW RECORD }
        begin
          DailyText.Columns.Clear;
          DailyText.Values.Clear;
          { DEFINE COLUMNS AND VALUES }
          DailyText.Columns.Add(TDaily.GROUP_ID);     DailyText.Values.Add(MainForm.GroupIdSel);
          DailyText.Columns.Add(TDaily.CUID);         DailyText.Values.Add(CUID);
          DailyText.Columns.Add(TDaily.AGEDATE);      DailyText.Values.Add(MainForm.AgeDateSel);
          DailyText.Columns.Add(TDaily.STAMP);        DailyText.Values.Add(DateTimeToStr(Now));
          DailyText.Columns.Add(TDaily.USER_ALIAS);   DailyText.Values.Add(UpperCase(MainForm.FUserName));
          DailyText.Columns.Add(TDaily.EMAIL);        DailyText.Values.Add('0');
          DailyText.Columns.Add(TDaily.CALLEVENT);    DailyText.Values.Add('0');
          DailyText.Columns.Add(TDaily.CALLDURATION); DailyText.Values.Add('0');
          DailyText.Columns.Add(TDaily.FIXCOMMENT);   DailyText.Values.Add(DailyCom.Text);
          { EXECUTE }
          InsertOK:=DailyText.InsertInto(TblDaily);
        end;
        { REFRESH HISTORY GRID }
        if (InsertOK) or (UpdateOK) then
        begin
          DailyText.OpenTable(TblDaily);
          DailyText.DataSet.Filter:=TDaily.CUID + EQUAL + QuotedStr(CUID);
          DailyText.DataSet.Sort:=TDaily.STAMP + DESC;
          DailyText.SqlToGrid(HistoryGrid, DailyText.DataSet, False, True);
        end;
      finally
        DailyText.Free;
      end;
    end;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- SAVE COMMENT ON <ENTER> }
procedure TActionsForm.GeneralComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  GenText:   TDataTables;
  Condition: string;
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
    if GeneralCom.Text <> '' then
    begin
      GenText:=TDataTables.Create(MainForm.FDbConnect);
      try
        GenText.OpenTable(TblGeneral);
        Condition:=TGeneral.CUID + EQUAL + QuotedStr(CUID);
        GenText.DataSet.Filter:=Condition;
        { UPDATE }
        if not (GenText.DataSet.RecordCount = 0) then
        begin
          { STAMP }
          GenText.UpdateRecord
          (
            TblGeneral,
            TGeneral.STAMP,
            DateTimeToStr(Now),
            Condition
          );
          { USER_ALIAS }
          GenText.UpdateRecord
          (
            TblGeneral,
            TGeneral.USER_ALIAS,
            UpperCase(MainForm.FUserName),
            Condition
          );
          { COMMENT }
          GenText.UpdateRecord
          (
            TblGeneral,
            TGeneral.FIXCOMMENT,
            GeneralCom.Text,
            Condition
          );
        end
        else
        { INSERT NEW }
        begin
          GenText.Columns.Clear;
          GenText.Values.Clear;
          { DEFINE COLUMNS AND VALUES }
          GenText.Columns.Add(TGeneral.CUID);       GenText.Values.Add(CUID);
          GenText.Columns.Add(TGeneral.STAMP);      GenText.Values.Add(DateTimeToStr(Now));
          GenText.Columns.Add(TGeneral.USER_ALIAS); GenText.Values.Add(UpperCase(MainForm.FUserName));
          GenText.Columns.Add(TGeneral.FIXCOMMENT); GenText.Values.Add(GeneralCom.Text);
          GenText.Columns.Add(TGeneral.FOLLOWUP);   GenText.Values.Add('');
          { EXECUTE }
          GenText.InsertInto(TblGeneral);
        end;
      finally
        GenText.Free;
      end;
    end;
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
//
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- SAVE CUSTOMER DETAILS }
procedure TActionsForm.imgSaveDetailsClick(Sender: TObject);
begin
//
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- SEND STATEMENT }
procedure TActionsForm.btnSendStatementClick(Sender: TObject);
begin
//
end;

{ ------------------------------------------------------------------------------------------------------------------------------ SELECT NEXT OVERDUE CUSTOMER }
procedure TActionsForm.btnNextClick(Sender: TObject);
var
  iCNT:  integer;
begin
  { LOOP TO THE NEAREST UNHIDDEN ROW }
  for iCNT:=(MainForm.sgAgeView.Selection.Top + 1) to MainForm.sgAgeView.RowCount - 1 do
  begin
    if (MainForm.sgAgeView.RowHeights[iCNT] <> -1) and (MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fOVERDUE, 1, 1), iCNT] <> '0') then
    begin
      MainForm.sgAgeView.Row:=iCNT;
      Break;
    end;
  end;
  { ASSIGN DATA FROM NEXT ROW }
  CUID      :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID,            1, 1), MainForm.sgAgeView.Row];
  CustName  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NAME,   1, 1), MainForm.sgAgeView.Row];
  CustNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUSTOMER_NUMBER, 1, 1), MainForm.sgAgeView.Row];
  { CLEAR ALL }
  DailyCom.Text       :='';
  GeneralCom.Text     :='';
  StatusBar.SimpleText:='';
  { LOAD NEW DATA }
  try
    GetData(OpenItemsGrid, HistoryGrid, MainForm.sgOpenItems);
  except
    MainForm.MsgCall(2, 'Unexpected error has occured. Please close the window and try again.');
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- FEEDBACK AFTER CALL }
procedure TActionsForm.btnFeedbackClick(Sender: TObject);
begin
  MainForm.WndCall(CalendarForm, 0);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------ SEND EMAIL }
procedure TActionsForm.btnSendEmailClick(Sender: TObject);
begin
  { CODE HERE }
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------- CALL CUSTOMER }
procedure TActionsForm.btnCallCustomerClick(Sender: TObject);
begin
  MakePhoneCall;
end;

end.
