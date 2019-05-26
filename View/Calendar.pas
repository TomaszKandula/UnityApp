unit Calendar;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.Variants,
    System.Classes,
    System.DateUtils,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ComCtrls,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    InterposerClasses,
    Helpers;


type

    /// <summary>
    /// View form with class helpers for calendar functionality, so the user will not have to
    /// put follow-up date manually on age view string grid.
    /// </summary>

    TCalendarForm = class(TForm)
        MyCalendar: TMonthCalendar;
        PanelActions: TPanel;
        DaysOne: TRadioButton;
        DaysThreen: TRadioButton;
        DaysSeven: TRadioButton;
        PanelCalendar: TPanel;
        Text: TLabel;
        PanelClient: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure MyCalendarDblClick(Sender: TObject);
        procedure DaysOneClick(Sender: TObject);
        procedure DaysThreenClick(Sender: TObject);
        procedure DaysSevenClick(Sender: TObject);
        procedure MyCalendarClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    private
        var FCalendarMode: TEnums.TCalendar;
        var FSelectedDate: TDateTime;
    public
        property  CalendarMode: TEnums.TCalendar read FCalendarMode write FCalendarMode;
        property  SelectedDate: TDateTime read FSelectedDate write FSelectedDate;
        function  MakeMyDay(Increment: integer): TDate;
        function  IsWeekend(const DT: TDateTime): Boolean;
        function  GetCurrentWorkingDay(WhatDay: integer): boolean;
        procedure SetFollowUp(SelectedDate: TDate; SelectedCUID: string; Row: integer);
    end;


var
    CalendarForm: TCalendarForm;


implementation


uses
    Main,
    DbModel,
    Settings,
    Worker,
    SysUtils;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Return working day in given month.
/// </summary>

function TCalendarForm.GetCurrentWorkingDay(WhatDay: Integer): boolean;
var
    Anchor: TDate;
    iCNT:   integer;
begin

    // Find working day for current month
    Anchor:=EndOfAMonth(YearOf(Now), MonthOf(Now) - 1);

    for iCNT:=1 to WhatDay do
    begin
        // If we have weekend, then move to next working day
        if IsWeekend(Anchor) then
            while IsWeekend(Anchor) do
                Anchor:=Anchor + 1;

        // Increase by one working day
        Anchor:=Anchor + 1;
    end;

    // Compare
    if Now = Anchor then
        Result:=True
            else
                Result:=False;

end;


/// <summary>
/// Check if today is weekend.
/// </summary>

function TCalendarForm.IsWeekend(const DT: TDateTime): Boolean;
begin
    Result:=System.SysUtils.DayOfWeek(DT) in [1, 7];
end;


/// <summary>
/// Move to next date, skip weekend.
/// </summary>

function TCalendarForm.MakeMyDay(Increment: integer): TDate;
begin
  Result:=Now + Increment;
  if IsWeekend(Result) then
    while IsWeekend(Result) do
      Result:=Result + 1;
end;


/// <summary>
/// Set follow-up date and register it in general comment and update age view string grid.
/// </summary>

procedure TCalendarForm.SetFollowUp(SelectedDate: TDate; SelectedCUID: string; Row: integer);
begin
  TTGeneralComment.Create(
                           SelectedCUID,
                           TNaVariants.NULL,
                           DateToStr(SelectedDate),
                           TNaVariants.NULL,
                           TNaVariants.NULL,
                           TNaVariants.NULL,
                           False
                         );
  MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1), Row]:=DateToStr(SelectedDate);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TCalendarForm.FormCreate(Sender: TObject);
begin
    SelectedDate:=TDateTimeFormats.NullDate;
    PanelActions.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
    PanelCalendar.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


procedure TCalendarForm.FormShow(Sender: TObject);
begin
    MyCalendar.Date   :=Now;
    DaysOne.Checked   :=False;
    DaysThreen.Checked:=False;
    DaysSeven.Checked :=False;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTON EVENTS //

/// <summary>
/// Reset all check boxes.
/// </summary>

procedure TCalendarForm.MyCalendarClick(Sender: TObject);
begin
    DaysOne.Checked   :=False;
    DaysThreen.Checked:=False;
    DaysSeven.Checked :=False;
end;


/// <summary>
/// Set next day.
/// </summary>

procedure TCalendarForm.DaysOneClick(Sender: TObject);
begin
    MyCalendar.Date:=MakeMyDay(1);
end;


/// <summary>
/// Set next three days.
/// </summary>

procedure TCalendarForm.DaysThreenClick(Sender: TObject);
begin
    MyCalendar.Date:=MakeMyDay(3);
end;


/// <summary>
/// Set next sevenm days.
/// </summary>

procedure TCalendarForm.DaysSevenClick(Sender: TObject);
begin
    MyCalendar.Date:=MakeMyDay(7);
end;


/// <summary>
/// Confirm selected date.
/// </summary>

procedure TCalendarForm.MyCalendarDblClick(Sender: TObject);
begin

    // Put selected date into database
    if CalendarMode = TEnums.TCalendar.cfDateToDB then
    begin
        SetFollowUp(
            CalendarForm.MyCalendar.Date,
            MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TSnapshots.fCuid, 1, 1),
            MainForm.sgAgeView.Row],
            MainForm.sgAgeView.Row
        );
        Close;
    end;

    // Just return selected date
    if CalendarMode = TEnums.TCalendar.cfGetDate then
    begin
        SelectedDate:=CalendarForm.MyCalendar.Date;
        Close;
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TCalendarForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TUChars.ESC then Close;
end;


end.

