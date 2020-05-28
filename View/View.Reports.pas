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
    Unity.Grid,
    Api.ReturnReportList;


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
        procedure GetBiReports_Callback(PayLoad: TReturnReportList);
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

procedure TReportsForm.GetBiReports_Callback(PayLoad: TReturnReportList);
begin

    PanelReportList.Enabled:=True;
    PanelDesc.Enabled:=True;
    PanelMenu.Enabled:=True;

    Screen.Cursor:=crDefault;

    if not PayLoad.IsSucceeded then
    begin
        THelpers.MsgCall(ReportsForm.Handle, TAppMessage.Error, PayLoad.Error.ErrorDesc);
        Service.Logger.Log('[GetBiReports_Callback]: Error has been thrown "' + PayLoad.Error.ErrorDesc + '".');
        Exit();
    end;

    sgReportList.Freeze(True);
    try

        sgReportList.RowCount:=Length(PayLoad.ReportList) + 1; // add header
        sgReportList.ColCount:=4;

        sgReportList.Cells[0, 0]:='';
        sgReportList.Cells[1, 0]:=TReportListFields._ReportName;
        sgReportList.Cells[2, 0]:=TReportListFields._ReportDesc;
        sgReportList.Cells[3, 0]:=TReportListFields._ReportLink;

        for var Index:=1 to sgReportList.RowCount - 1 do
        begin
            sgReportList.Cells[1, Index]:=PayLoad.ReportList[Index - 1].ReportName;
            sgReportList.Cells[2, Index]:=PayLoad.ReportList[Index - 1].ReportDesc;
            sgReportList.Cells[3, Index]:=PayLoad.ReportList[Index - 1].ReportLink;
        end;

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

