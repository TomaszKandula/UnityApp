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
unit Calendar;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls, Main, StdCtrls;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TCalendarForm = class(TForm)
    AppMain: TShape;
    MyCalendar: TMonthCalendar;
    MainPanel: TPanel;
    DaysOne: TRadioButton;
    DaysThreen: TRadioButton;
    DaysSeven: TRadioButton;
    CalendarPanel: TPanel;
    Label1: TLabel;
    procedure MyCalendarDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DaysOneClick(Sender: TObject);
    procedure DaysThreenClick(Sender: TObject);
    procedure DaysSevenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MyCalendarClick(Sender: TObject);
  public
    function MakeMyDay(Increment: integer): TDate;
    function IsWeekend(const DT: TDateTime): Boolean;
  end;

var
  CalendarForm: TCalendarForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, Settings;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }

{ --------------------------------------------------------------------------------------------------------------------------------- CHECK IF TODAY IS WEEKEND }
function TCalendarForm.IsWeekend(const DT: TDateTime): Boolean;
begin
  Result:=SysUtils.DayOfWeek(DT) in [1, 7];
end;

{ --------------------------------------------------------------------------------------------------------------------------- MOVE TO NEXT DATE, SKIP WEEKEND }
function TCalendarForm.MakeMyDay(Increment: integer): TDate;
begin
  Result:=Now + Increment;
  if IsWeekend(Result) then
    while IsWeekend(Result) do
      Result:=Result + 1;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TCalendarForm.FormCreate(Sender: TObject);
var
  AppSettings:  TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    CalendarForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_CALENDAR', APPNAME);
  finally
    AppSettings.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------------ RESET CHECKBOXES }
procedure TCalendarForm.FormShow(Sender: TObject);
begin
  MyCalendar.Date   :=Now;
  DaysOne.Checked   :=False;
  DaysThreen.Checked:=False;
  DaysSeven.Checked :=False;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- RESET BOXES }
procedure TCalendarForm.MyCalendarClick(Sender: TObject);
begin
  DaysOne.Checked   :=False;
  DaysThreen.Checked:=False;
  DaysSeven.Checked :=False;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- SET NEXT DAY }
procedure TCalendarForm.DaysOneClick(Sender: TObject);
begin
  MyCalendar.Date:=MakeMyDay(1);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- SET NEXT THREE DAYS }
procedure TCalendarForm.DaysThreenClick(Sender: TObject);
begin
  MyCalendar.Date:=MakeMyDay(3);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- SET NEXT SEVENT DAYS }
procedure TCalendarForm.DaysSevenClick(Sender: TObject);
begin
  MyCalendar.Date:=MakeMyDay(7);
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- APPROVE SELECTED DATE }
procedure TCalendarForm.MyCalendarDblClick(Sender: TObject);
var
  GenText:    TDataTables;
  Condition:  string;
  CUID:       string;
begin
  GenText:=TDataTables.Create(MainForm.FDbConnect);
  try
    GenText.OpenTable(TblGeneral);
    CUID:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCUID, 1, 1), MainForm.sgAgeView.Row];
    Condition:=TGeneral.CUID + EQUAL + QuotedStr(CUID);
    GenText.DataSet.Filter:=Condition;
    { UPDATE }
    if not (GenText.DataSet.RecordCount = 0) then
    begin
      GenText.CleanUp;
      { DEFINE COLUMNS, VALUES AND CONDITIONS }
      GenText.Columns.Add(TGeneral.STAMP);      GenText.Values.Add(DateTimeToStr(Now));                      GenText.Conditions.Add(Condition);
      GenText.Columns.Add(TGeneral.USER_ALIAS); GenText.Values.Add(UpperCase(MainForm.FUserName));           GenText.Conditions.Add(Condition);
      GenText.Columns.Add(TGeneral.FOLLOWUP);   GenText.Values.Add(DateToStr(CalendarForm.MyCalendar.Date)); GenText.Conditions.Add(Condition);
      { EXECUTE }
      GenText.UpdateRecord(TblGeneral);
      { DISPLAY NOW ON STRING GRID }
      MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fFOLLOWUP, 1, 1), MainForm.sgAgeView.Row]:=DateToStr(CalendarForm.MyCalendar.Date);
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
      GenText.Columns.Add(TGeneral.FIXCOMMENT); GenText.Values.Add('');
      GenText.Columns.Add(TGeneral.FOLLOWUP);   GenText.Values.Add(DateToStr(CalendarForm.MyCalendar.Date));
      { EXECUTE }
      GenText.InsertInto(TblGeneral);
      MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fFOLLOWUP, 1, 1), MainForm.sgAgeView.Row]:=DateToStr(CalendarForm.MyCalendar.Date);
    end;
  finally
    GenText.Free;
  end;
  Close;
end;

end.
