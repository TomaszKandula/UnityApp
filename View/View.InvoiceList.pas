unit View.InvoiceList;

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
    Vcl.Grids,
    Vcl.ComCtrls,
    Data.Win.ADODB,
    Unity.Grid,
    Unity.Records;


type


    TInvoicesForm = class(TForm)
        InvoicesGrid: TStringGrid;
        StatusBar: TStatusBar;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    strict private
        var FIsLoaded: boolean;
        //procedure GetInvoiceList_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
    end;


    function InvoicesForm(): TInvoicesForm;


Implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Helpers,
    Unity.Enums,
    Unity.Constants,
    Unity.Settings,
    Unity.SessionService,
    Async.InvoiceTracker;


var vInvoicesForm: TInvoicesForm;


function InvoicesForm(): TInvoicesForm;
begin
    if not(Assigned(vInvoicesForm)) then Application.CreateForm(TInvoicesForm, vInvoicesForm);
    Result:=vInvoicesForm;
end;


{$REGION 'LOCAL HELPERS'}


//procedure TInvoicesForm.GetInvoiceList_Callback(ReturnedData: TStringGrid; CallResponse: TCallResponse);
//begin
//
//    if not CallResponse.IsSucceeded then
//    begin
//        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
//        Exit();
//    end;
//
//    InvoicesGrid.Freeze(True);
//    try
//
//        InvoicesGrid.RowCount:=ReturnedData.RowCount;
//        InvoicesGrid.ColCount:=ReturnedData.ColCount;
//
//        for var iCNT:=0 to ReturnedData.RowCount - 1 do
//            for var jCNT:=0 to ReturnedData.ColCount - 1 do
//                InvoicesGrid.Cells[jCNT, iCNT]:=ReturnedData.Cells[jCNT, iCNT];
//
//        InvoicesGrid.Freeze(False);
//        InvoicesGrid.SetColWidth(40, 10, 400);
//
//    finally
//        InvoicesGrid.Freeze(False);
//    end;
//
//end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TInvoicesForm.FormCreate(Sender: TObject);
begin
    InvoicesGrid.RowCount:=2;
    InvoicesGrid.ColCount:=4;
    InvoicesGrid.SetRowHeight(InvoicesGrid.sgRowHeight, 25);
end;


procedure TInvoicesForm.FormShow(Sender: TObject);
begin
    InvoicesGrid.ClearAll(2, 1, 1, False);
    InvoicesGrid.SetColWidth(10, 20, 400);
    InvoicesGrid.AutoThumbSize;
end;


procedure TInvoicesForm.FormResize(Sender: TObject);
begin
    InvoicesGrid.AutoThumbSize;
end;


procedure TInvoicesForm.FormActivate(Sender: TObject);
begin

    THelpers.ExecWithDelay(500, procedure
    begin

        if not FIsLoaded then
        begin
            //...
            FIsLoaded:=True;
        end;

    end);

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TInvoicesForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsLoaded:=False;
end;


procedure TInvoicesForm.InvoicesGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
    InvoicesGrid.DrawSelected(ARow, ACol, State, Rect, clBlack, TCommon.SelectionColor, clBlack, clWhite, True);
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TInvoicesForm.InvoicesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 67) and (Shift = [ssCtrl]) then InvoicesGrid.CopyCutPaste(TActions.Copy);
end;


procedure TInvoicesForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


{$ENDREGION}


end.

