unit View.EventLog;

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
    Vcl.Imaging.pngimage,
    Unity.Grid,
    Unity.Panel;


type

    TEventForm = class(TForm)
        EventMemo: TMemo;
        PanelEventMemo: TPanel;
        PanelClient: TPanel;
        PanelBottom: TPanel;
        ImageGrip: TImage;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    strict private
        procedure LoadEventLog();
    end;


    function EventForm(): TEventForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Service,
    Unity.Settings;


var vEventForm: TEventForm;


function EventForm(): TEventForm;
begin
    if not(Assigned(vEventForm)) then Application.CreateForm(TEventForm, vEventForm);
    Result:=vEventForm;
end;


{$REGION 'LOCAL HELPERS'}


procedure TEventForm.LoadEventLog();
begin

    try
        EventMemo.Lines.LoadFromFile(Service.SessionLog);
    except
        on E: Exception do
            EventMemo.Lines.Text:='Cannot load event log file. Error has been thorwn: ' + E.Message;
    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TEventForm.FormCreate(Sender: TObject);
begin
    PanelEventMemo.Borders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


procedure TEventForm.FormShow(Sender: TObject);
begin
    LoadEventLog();
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TEventForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_F5 then
    begin
        LoadEventLog;
        SendMessage(EventMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


procedure TEventForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


{$ENDREGION}


end.

