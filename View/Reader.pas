/// <para>
/// 	Name:             Unity Reader Unity for Debt Management
/// 	Version:          1.0
/// 	(C)(R):           Tomasz Kandula
/// 	Originate:        27-04-2018 (Concept & GUI)
/// 	IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)
/// 	Target:           Microsoft Windows 7 or newer
/// 	Dependencies:     Synopse Zip and own libraries
/// 	NET Framework:    Required 4.6 or newer (Lync / Skype calls)
/// 	LYNC version:     2013 or newer
/// </para>
///
/// <remarks>
///     Prerequisites:
///         D:\Projects\Unity Project\Components\CEF4Delphi\bin
///         D:\Projects\Unity Project\Components\CEF4Delphi\sources
///     <seealso cref="D:\Projects\Unity Project\Components\Readme.txt"/>
/// </remarks>

unit Reader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, uCEFChromium, uCEFWindowParent, uCEFChromiumWindow,
  uCEFTypes, uCEFInterfaces, ExtCtrls;

// ----------------------------------------------------------------------------------------------------------------------------------------- MAIN FORM CLASS //

/// <remarks>
///     No MVVM or MVC pattern intended.
/// </remarks>

type
    TFormReader = class(TForm)
        ChromiumWindow: TChromiumWindow;
        StatusBar: TStatusBar;
        Timer: TTimer;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure ChromiumWindowClose(Sender: TObject);
        procedure ChromiumWindowBeforeClose(Sender: TObject);
        procedure ChromiumWindowAfterCreated(Sender: TObject);
        procedure TimerTimer(Sender: TObject);
    private
        FCanClose : boolean;
        FClosing  : boolean;
    protected
        procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean; var Result: Boolean);
        procedure WndProc(var msg: Messages.TMessage); override;
    public
        var input : string;
end;

var
    FormReader: TFormReader;

implementation

{$R *.dfm}

uses
    uCEFApplication;

// ---------------------------------------------------------------------------------------------------------------------------------------- WINDOWS MESSAGES //

/// <summary>
///     Process selected windows messages.
/// </summary>

procedure TFormReader.WndProc(var Msg: Messages.TMessage);
begin
    inherited;

    if Msg.Msg = WM_MOVE then
        if (ChromiumWindow <> nil) then
            ChromiumWindow.NotifyMoveOrResizeStarted;

    if Msg.Msg = WM_MOVING then
        if (ChromiumWindow <> nil) then
            ChromiumWindow.NotifyMoveOrResizeStarted;

    if Msg.Msg = WM_ENTERMENULOOP then
        if (Msg.wParam = 0)  and (GlobalCEFApp <> nil) then
            GlobalCEFApp.OsmodalLoop:=True;

    if Msg.Msg = WM_EXITMENULOOP  then
        if (Msg.wParam = 0)  and (GlobalCEFApp <> nil) then
            GlobalCEFApp.OsmodalLoop:=False;

end;

// --------------------------------------------------------------------------------------------------------------------------------------------- MAIN THREAD //

/// <summary>
///     Execute method statement on create event.
/// </summary>

procedure TFormReader.FormCreate(Sender: TObject);
begin
    StatusBar.Panels.Items[0].Text:='Status: no param';
    StatusBar.Panels.Items[1].Text:='URL: none';
end;

/// <summary>
///     Execute method statement on show event (after form is created and before is shown to the user).
/// </summary>

procedure TFormReader.FormShow(Sender: TObject);
begin
    ChromiumWindow.ChromiumBrowser.OnBeforePopup:=Chromium_OnBeforePopup;
    if not(ChromiumWindow.CreateBrowser) then
        Timer.Enabled:=True;
end;

/// <summary>
///     Execute method statements on "before close" event.
/// </summary>

procedure TFormReader.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    CanClose:=FCanClose;

    if not(FClosing) then
    begin
        FClosing:=True;
        Visible :=False;
        ChromiumWindow.CloseBrowser(True);
    end;
end;

// ---------------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS //

/// <summary>
///     Timer for Chromium component.
/// </summary>

procedure TFormReader.TimerTimer(Sender: TObject);
begin
    Timer.Enabled:=False;
    if not(ChromiumWindow.CreateBrowser) and not(ChromiumWindow.Initialized) then
        Timer.Enabled:=True;
end;

// -------------------------------------------------------------------------------------------------------------------------------------- CHROMIUM COMPONENT //

/// <summary>
///     Execute on "before popup" - ignore tab sheets and pop-ups.
/// </summary>
{ ----------------------------------------------------------------------------------------------------------------------------------- IGNORE TABS AND POP-UPS }
procedure TFormReader.Chromium_OnBeforePopup(Sender: TObject;
    const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
    targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
    userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
    var windowInfo: TCefWindowInfo; var client: ICefClient;
    var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean;
    var Result: Boolean);
begin
    Result:=(
                targetDisposition in
                [
                    WOD_NEW_FOREGROUND_TAB,
                    WOD_NEW_BACKGROUND_TAB,
                    WOD_NEW_POPUP,
                    WOD_NEW_WINDOW
                ]
            );
end;

/// <summary>
///     Create Chromium browser.
/// </summary>

procedure TFormReader.ChromiumWindowAfterCreated(Sender: TObject);
begin
    if not(input = '') then
    begin
        try
            try
                ChromiumWindow.LoadURL(input);
                StatusBar.Panels.Items[0].Text:='Status: OK';
            except
                StatusBar.Panels.Items[0].Text:='Status: ERROR';
            end;
        finally
            StatusBar.Panels.Items[1].Text:='URL: ' + input;
        end;
    end;
end;

/// <summary>
///     Close chromium component on "before close" event.
/// </summary>

procedure TFormReader.ChromiumWindowBeforeClose(Sender: TObject);
begin
    FCanClose:=True;
    Close;
end;

/// <summary>
///     If loaded, then close chromium component on close event.
/// </summary>

procedure TFormReader.ChromiumWindowClose(Sender: TObject);
begin
  if not(ChromiumWindow.DestroyChildWindow) then
  begin
    FCanClose:=True;
    Close;
  end;
end;

end.
