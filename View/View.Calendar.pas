unit View.Calendar;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views use Lazy Initialization pattern.
// ------------------------------------------------------------------------------

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
    Unity.Grid,
    Unity.Panel,
    Unity.Records,
    Unity.Enums;


type


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
        var FGeneralCommentFields: TGeneralCommentFields;
        function MakeMyDay(Increment: integer): TDate;
        function IsWeekend(const DT: TDateTime): Boolean;
    public
        var FCalendarMode: TCalendar;
        var FSelectedDate: TDateTime;
        procedure SetFollowUp(SelectedDate: TDate; SelectedCUID: string; Row: integer);
    end;


    function CalendarForm: TCalendarForm;


implementation


{$R *.dfm}


uses
    System.SysUtils,
    View.Main,
    DbModel,
    Unity.Chars,
    Unity.Unknown,
    Unity.DateTimeFormats,
    Unity.Settings,
    Async.Comments;


var vCalendarForm: TCalendarForm;


function CalendarForm: TCalendarForm;
begin
    if not(Assigned(vCalendarForm)) then Application.CreateForm(TCalendarForm, vCalendarForm);
    Result:=vCalendarForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


function TCalendarForm.IsWeekend(const DT: TDateTime): Boolean;
begin
    Result:=System.SysUtils.DayOfWeek(DT) in [1, 7];
end;


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

    FGeneralCommentFields.CUID        :=SelectedCUID;
    FGeneralCommentFields.FixedComment:=TUnknown.NULL;
    FGeneralCommentFields.FollowUp    :=DateToStr(SelectedDate);
    FGeneralCommentFields.Free1       :=TUnknown.NULL;
    FGeneralCommentFields.Free2       :=TUnknown.NULL;
    FGeneralCommentFields.Free3       :=TUnknown.NULL;
    FGeneralCommentFields.EventLog    :=False;

    var Comments: IComments:=TComments.Create();
    Comments.EditGeneralComment(FGeneralCommentFields);

    MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn(TGeneralComment.fFollowUp, 1, 1), Row]:=DateToStr(SelectedDate);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TCalendarForm.FormCreate(Sender: TObject);
begin
    FSelectedDate:=TDateTimeFormats.NullDate;
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
    if FCalendarMode = TCalendar.DateToDB then
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
    if FCalendarMode = TCalendar.GetDate then
    begin
        FSelectedDate:=CalendarForm.MyCalendar.Date;
        Close;
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TCalendarForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.ESC then Close;
end;


end.

