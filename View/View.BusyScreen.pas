unit View.BusyScreen;

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
    Vcl.StdCtrls,
    Vcl.ExtCtrls;


type


    TBusyForm = class(TForm)
        imgBusy: TImage;
        txtBusy: TLabel;
        FormPanel: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    end;


    function BusyForm(): TBusyForm;


implementation


{$R *.dfm}


uses
    View.Main;


var VBusyForm: TBusyForm;


function BusyForm(): TBusyForm;
begin
    if not(Assigned(VBusyForm)) then Application.CreateForm(TBusyForm, VBusyForm);
    Result:=VBusyForm;
end;


{$REGION 'STARTUP'}


procedure TBusyForm.FormCreate(Sender: TObject);
begin
    Position:=poDesigned; // The form appears positioned on the screen and with the same height and width as it had at design time.
    DefaultMonitor:=dmActiveForm; // The form appears on the same monitor as the currently active form.
    FormStyle:=fsStayOnTop; // Always on top of the current window. Note: do not display any dialogs before closing this window.
end;


procedure TBusyForm.FormShow(Sender: TObject);
begin
    BusyForm.Top :=MainForm.Top  + (MainForm.Height div 2) - (BusyForm.Height div 2);
    BusyForm.Left:=MainForm.Left + (MainForm.Width  div 2) - (BusyForm.Width  div 2);
end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TBusyForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    // Empty
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TBusyForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key=VK_F4) and (Shift=[ssALT]) then Key:=0;
end;


{$ENDREGION}


end.
