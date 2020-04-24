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
    Winapi.ShellAPI,
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
    Vcl.Buttons,
    Vcl.Grids,
    Unity.Panel,
    Unity.Records,
    Unity.Grid;


type


    TReportsForm = class(TForm)
        sgReportList: TStringGrid;
        ReportDesc: TMemo;
        btnOpenReport: TSpeedButton;
        PanelReportList: TPanel;
        PanelDesc: TPanel;
        PanelMenu: TPanel;
        ShapeDesc: TShape;
        ShapeReportList: TShape;
        procedure FormCreate(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure sgReportListClick(Sender: TObject);
        procedure btnOpenReportClick(Sender: TObject);
    public
        var FSetLastSelection: TTabSheet;
    strict private
        procedure GetBiReports_Callback(PayLoad: TStringGrid; CallResponse: TCallResponse);
    end;


    function ReportsForm(): TReportsForm;


implementation


{$R *.dfm}


uses
    System.Math,
    View.Main,
    Unity.Enums,
    Unity.Service,
    Unity.Helpers,
    Unity.Constants,
    Api.ReportListFields;


var
    VReportsForm: TReportsForm;


function ReportsForm(): TReportsForm;
begin
    if not(Assigned(VReportsForm)) then Application.CreateForm(TReportsForm, VReportsForm);
    Result:=VReportsForm;
end;


{$REGION 'CALLBACKS'}

procedure TReportsForm.GetBiReports_Callback(PayLoad: TStringGrid; CallResponse: TCallResponse);
begin

    PanelReportList.Enabled:=True;
    PanelDesc.Enabled:=True;
    PanelMenu.Enabled:=True;

    Screen.Cursor:=crDefault;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(ReportsForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Service.Logger.Log('[GetBiReports_Callback]: Error has been thrown "' + CallResponse.LastMessage + '".');
        Exit();
    end;

    sgReportList.Freeze(True);
    try

        sgReportList.RowCount:=PayLoad.RowCount;
        sgReportList.ColCount:=PayLoad.ColCount;

        for var RowIndex:=0 to sgReportList.RowCount - 1 do
            for var ColIndex:=0 to PayLoad.ColCount - 1 do
                sgReportList.Cells[ColIndex, RowIndex]:=PayLoad.Cells[ColIndex, RowIndex];

        // Hide helper columns
        sgReportList.ColWidths[sgReportList.GetCol(TReportListFields._ReportDesc)]:=sgReportList.sgRowHidden;
        sgReportList.ColWidths[sgReportList.GetCol(TReportListFields._ReportLink)]:=sgReportList.sgRowHidden;

    finally
        sgReportList.Freeze(False);
        sgReportList.SetColWidth(10, 30, 400);
    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TReportsForm.FormCreate(Sender: TObject);
begin
    ReportDesc.Clear();
end;


procedure TReportsForm.FormActivate(Sender: TObject);
begin

    PanelReportList.Enabled:=False;
    PanelDesc.Enabled:=False;
    PanelMenu.Enabled:=False;

    Screen.Cursor:=crHourGlass;

    THelpers.ExecWithDelay(500, procedure
    begin
        ReportDesc.Clear();
        sgReportList.ClearAll(2, 1, 1, True);
        Service.Mediator.Utilities.GetBiReportsAsync(GetBiReports_Callback);
    end);

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TReportsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainForm.SetActiveTabsheet(FSetLastSelection);
end;


{$ENDREGION}


{$REGION 'MOUSE AND BUTTON CLICK EVENTS'}


procedure TReportsForm.sgReportListClick(Sender: TObject);
begin
    ReportDesc.Text:=sgReportList.Cells[sgReportList.GetCOl(TReportListFields._ReportDesc), sgReportList.Row];
end;


procedure TReportsForm.btnOpenReportClick(Sender: TObject);
begin

    var ReportUrl:=sgReportList.Cells[sgReportList.GetCOl(TReportListFields._ReportLink), sgReportList.Row];

    ShellExecute(
        ReportsForm.Handle,
        'open',
        PChar(Service.Settings.DirApplication + TCommon.UnityReader),
        PChar(ReportUrl),
        nil,
        SW_SHOWNORMAL
    );

end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TReportsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


{$ENDREGION}


end.

