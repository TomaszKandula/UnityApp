
{$I .\Include\Header.inc}

unit EventLog;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    StdCtrls,
    ComCtrls,
    pngimage,
    InterposerCLasses;


type

    /// <summary>
    ///     View form class with helpers for displaying event log content in separate window.
    /// </summary>

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

var
    EventForm: TEventForm;


implementation


uses
    Main,
    Settings;


{$R *.dfm}


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
///     Load event log from file, fixed path is provided by application settings class.
/// </summary>

procedure TEventForm.LoadEventLog;
var
    Settings: ISettings;
begin

    Settings:=TSettings.Create;

    try
        EventMemo.Lines.LoadFromFile(Settings.GetPathEventLog);
    except
        on E: Exception do
            EventMemo.Lines.Text:='Cannot load event log file. Error has been thorwn: ' + E.Message;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TEventForm.FormCreate(Sender: TObject);
var
    Settings: ISettings;
begin
    Settings:=TSettings.Create;
    EventForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_EVENTLOG', APPCAPTION);
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
    if Key = ESC then Close;
end;


end.

