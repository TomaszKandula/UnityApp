unit View.EventLog;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views except MainForm use Lazy Loading design pattern.
// ------------------------------------------------------------------------------

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
    Unity.Interposer;


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
    private
        procedure LoadEventLog;
    end;


    function EventForm: TEventForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Settings;


var vEventForm: TEventForm;


function EventForm: TEventForm;
begin
    if not(Assigned(vEventForm)) then Application.CreateForm(TEventForm, vEventForm);
    Result:=vEventForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


procedure TEventForm.LoadEventLog;
begin

    var Settings: ISettings:=TSettings.Create;
    try
        EventMemo.Lines.LoadFromFile(Settings.GetPathEventLog);
    except
        on E: Exception do
            EventMemo.Lines.Text:='Cannot load event log file. Error has been thorwn: ' + E.Message;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TEventForm.FormCreate(Sender: TObject);
begin
    PanelEventMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


procedure TEventForm.FormShow(Sender: TObject);
begin
    LoadEventLog;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


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


end.

