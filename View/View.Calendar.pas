unit View.Calendar;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

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
    strict private
        var FGeneralCommentFields: TGeneralCommentFields;
        function MakeMyDay(Increment: integer): TDate;
        function IsWeekend(const DT: TDateTime): Boolean;
        procedure EditGeneralComment_Callback(CallResponse: TCallResponse);
    public
        var FCalendarMode: TCalendar;
        var FSelectedDate: TDateTime;
        procedure SetFollowUp(SelectedDate: TDate; SelectedCUID: string; Row: integer);
    end;


    function CalendarForm(): TCalendarForm;


implementation


{$R *.dfm}


uses
    System.SysUtils,
    View.Main,
    DbModel{Legacy},
    Unity.SessionService,
    Unity.Chars,
    Unity.Unknown,
    Unity.Helpers,
    Unity.DateTimeFormats,
    Unity.EventLogger,
    Unity.Settings,
    Async.Comments;


var vCalendarForm: TCalendarForm;


function CalendarForm(): TCalendarForm;
begin
    if not(Assigned(vCalendarForm)) then Application.CreateForm(TCalendarForm, vCalendarForm);
    Result:=vCalendarForm;
end;


{$REGION 'LOCAL HELPERS'}


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


procedure TCalendarForm.SetFollowUp(SelectedDate: TDate; SelectedCUID: string; Row: integer);
begin

    FGeneralCommentFields.CompanyCode   :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCoCode), Row];
    FGeneralCommentFields.CustomerNumber:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCustomerNumber), Row];
    FGeneralCommentFields.FollowUp      :=DateToStr(SelectedDate);
    FGeneralCommentFields.Free1         :=String.Empty;
    FGeneralCommentFields.Free2         :=String.Empty;
    FGeneralCommentFields.Free3         :=String.Empty;
    FGeneralCommentFields.UserComment   :=String.Empty;
    FGeneralCommentFields.UserAlias     :=SessionService.SessionData.AliasName;

    var Comments: IComments:=TComments.Create();
    Comments.EditGeneralComment(FGeneralCommentFields, EditGeneralComment_Callback);

    MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TGeneralComment.fFollowUp), Row]:=DateToStr(SelectedDate);

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TCalendarForm.EditGeneralComment_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        ThreadFileLog.Log('[EditGeneralComment_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TCalendarForm.FormCreate(Sender: TObject);
begin
    FSelectedDate:=TDateTimeFormats.NullDate;
    PanelActions.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
    PanelCalendar.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
end;


procedure TCalendarForm.FormShow(Sender: TObject);
begin
    MyCalendar.Date   :=Now;
    DaysOne.Checked   :=False;
    DaysThreen.Checked:=False;
    DaysSeven.Checked :=False;
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TCalendarForm.MyCalendarClick(Sender: TObject);
begin
    // Reset all check boxes.
    DaysOne.Checked   :=False;
    DaysThreen.Checked:=False;
    DaysSeven.Checked :=False;
end;


procedure TCalendarForm.DaysOneClick(Sender: TObject);
begin
    // Set next day.
    MyCalendar.Date:=MakeMyDay(1);
end;


procedure TCalendarForm.DaysThreenClick(Sender: TObject);
begin
    // Set next three days.
    MyCalendar.Date:=MakeMyDay(3);
end;


procedure TCalendarForm.DaysSevenClick(Sender: TObject);
begin
    // Set next sevenm days.
    MyCalendar.Date:=MakeMyDay(7);
end;


procedure TCalendarForm.MyCalendarDblClick(Sender: TObject);
begin

    // Put selected date into database
    if FCalendarMode = TCalendar.DateToDB then
    begin
        SetFollowUp(
            CalendarForm.MyCalendar.Date,
            MainForm.sgAgeView.Cells[MainForm.sgAgeView.GetCol(TSnapshots.fCuid),
            MainForm.sgAgeView.Row],
            MainForm.sgAgeView.Row
        );
        Close;
    end;

    // Just return selected date
    if FCalendarMode = TCalendar.GetDate then
    begin
        FSelectedDate:=CalendarForm.MyCalendar.Date;
        Close();
    end;

end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TCalendarForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = TChars.ESC then Close();
end;


{$ENDREGION}


end.

