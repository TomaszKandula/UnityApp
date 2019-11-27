unit View.Reports;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Vcl.ComCtrls,
    Unity.Panel;


type


    TReportsForm = class(TForm)
        PanelClient: TPanel;
        ScrollBox: TScrollBox;
        PanelContent: TPanel;
        Report1Overdue: TPanel;
        Report1Icon: TImage;
        Report1Title: TLabel;
        Report1Text: TLabel;
        Report2Exceeders: TPanel;
        Report2Icon: TImage;
        Report2Title: TLabel;
        Report2Text: TLabel;
        Report3Debtors: TPanel;
        Report3Icon: TImage;
        Report3Title: TLabel;
        Report3Text: TLabel;
        Report4Status: TPanel;
        Report4Icon: TImage;
        Report4Title: TLabel;
        Report4Text: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure Report1OverdueMouseEnter(Sender: TObject);
        procedure Report1OverdueMouseLeave(Sender: TObject);
        procedure Report2ExceedersMouseEnter(Sender: TObject);
        procedure Report2ExceedersMouseLeave(Sender: TObject);
        procedure Report3DebtorsMouseEnter(Sender: TObject);
        procedure Report3DebtorsMouseLeave(Sender: TObject);
        procedure Report4StatusMouseEnter(Sender: TObject);
        procedure Report4StatusMouseLeave(Sender: TObject);
        procedure Report1OverdueClick(Sender: TObject);
        procedure Report2ExceedersClick(Sender: TObject);
        procedure Report3DebtorsClick(Sender: TObject);
        procedure Report4StatusClick(Sender: TObject);
        procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    public
        var FSetLastSelection: TTabSheet;
    end;


    function ReportsForm(): TReportsForm;


implementation


{$R *.dfm}


uses
    System.Math,
    View.Main,
    Unity.Enums,
    Unity.EventLogger,
    Unity.Helpers;


var
    VReportsForm: TReportsForm;


function ReportsForm(): TReportsForm;
begin
    if not(Assigned(VReportsForm)) then Application.CreateForm(TReportsForm, VReportsForm);
    Result:=VReportsForm;
end;


{$REGION 'STARTUP'}


procedure TReportsForm.FormCreate(Sender: TObject);
begin
    Report1Overdue.Cursor  :=crHandPoint;
    Report2Exceeders.Cursor:=crHandPoint;
    Report3Debtors.Cursor  :=crHandPoint;
    Report4Status.Cursor   :=crHandPoint;
end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TReportsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainForm.SetActiveTabsheet(FSetLastSelection);
end;


procedure TReportsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    {Do nothing}
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TReportsForm.Report1OverdueClick(Sender: TObject);
begin

    var Return: cardinal:=THelpers.ShowReport(1, ReportsForm);

    if not(Return > 32) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Cannot execute report. Please contact with IT support.');
        ThreadFileLog.Log('ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;

end;


procedure TReportsForm.Report2ExceedersClick(Sender: TObject);
begin

    var Return: cardinal:=THelpers.ShowReport(2, ReportsForm);

    if not(Return > 32) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Cannot execute report. Please contact with IT support.');
        ThreadFileLog.Log('ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;

end;


procedure TReportsForm.Report3DebtorsClick(Sender: TObject);
begin

    var Return: cardinal:=THelpers.ShowReport(3, ReportsForm);

    if not(Return > 32) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Cannot execute report. Please contact with IT support.');
        ThreadFileLog.Log('ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;

end;


procedure TReportsForm.Report4StatusClick(Sender: TObject);
begin

    var Return: cardinal:=THelpers.ShowReport(4, ReportsForm);

    if not(Return > 32) then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Cannot execute report. Please contact with IT support.');
        ThreadFileLog.Log('ShellExecute returned ' + IntToStr(Return) + '. Report cannot be displayed.');
    end;

end;


{$ENDREGION}


{$REGION 'MOUSE MOVE EVENTS'}


procedure TReportsForm.Report1OverdueMouseEnter(Sender: TObject);
begin
    Report1Title.Font.Color:=$006433C9;
    Report1Text.Font.Color:=$006433C9;
end;


procedure TReportsForm.Report1OverdueMouseLeave(Sender: TObject);
begin
    Report1Title.Font.Color:=0;
    Report1Text.Font.Color:=0;
end;


procedure TReportsForm.Report2ExceedersMouseEnter(Sender: TObject);
begin
    Report2Title.Font.Color:=$006433C9;
    Report2Text.Font.Color:=$006433C9;
end;


procedure TReportsForm.Report2ExceedersMouseLeave(Sender: TObject);
begin
    Report2Title.Font.Color:=0;
    Report2Text.Font.Color:=0;
end;


procedure TReportsForm.Report3DebtorsMouseEnter(Sender: TObject);
begin
    Report3Title.Font.Color:=$006433C9;
    Report3Text.Font.Color:=$006433C9;
end;


procedure TReportsForm.Report3DebtorsMouseLeave(Sender: TObject);
begin
    Report3Title.Font.Color:=0;
    Report3Text.Font.Color:=0;
end;


procedure TReportsForm.Report4StatusMouseEnter(Sender: TObject);
begin
    Report4Title.Font.Color:=$006433C9;
    Report4Text.Font.Color:=$006433C9;
end;


procedure TReportsForm.Report4StatusMouseLeave(Sender: TObject);
begin
    Report4Title.Font.Color:=0;
    Report4Text.Font.Color:=0;
end;


procedure TReportsForm.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

    const Level = 2;
    var ScrollBox: TScrollBox:=TScrollBox(Sender);
    var Sensitivity:=WheelDelta div Level;

    var NewPos:=ScrollBox.VertScrollBar.Position - Sensitivity;
    NewPos:=Max(NewPos, 0);
    NewPos:=Min(NewPos, ScrollBox.VertScrollBar.Range);

    ScrollBox.VertScrollBar.Position:=NewPos;
    Handled:=True;

end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TReportsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


{$ENDREGION}


end.

