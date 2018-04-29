{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Reader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, uCEFChromium, uCEFWindowParent, uCEFChromiumWindow,
  uCEFTypes, uCEFInterfaces, ExtCtrls;

{ ----------------------------------------------------------------- ! MAIN CLASS ! -------------------------------------------------------------------------- }
type                                                            (* GUI | MAIN THREAD *)
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

{ ------------------------------------------------------------------ ! WINDOWS ! ---------------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------- PROCESS SELECTED WINDOWS MESSAGES }
procedure TFormReader.WndProc(var Msg: Messages.TMessage);
begin
  inherited;
  if Msg.Msg = WM_MOVE          then if (ChromiumWindow <> nil) then ChromiumWindow.NotifyMoveOrResizeStarted;
  if Msg.Msg = WM_MOVING        then if (ChromiumWindow <> nil) then ChromiumWindow.NotifyMoveOrResizeStarted;
  if Msg.Msg = WM_ENTERMENULOOP then if (Msg.wParam = 0)  and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop:=True;
  if Msg.Msg = WM_EXITMENULOOP  then if (Msg.wParam = 0)  and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop:=False;
end;

{ ------------------------------------------------------- ! MAIN THREAD METHODS AND EVENTS ! ---------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TFormReader.FormCreate(Sender: TObject);
begin
  StatusBar.Panels.Items[0].Text:='Status: no param';
  StatusBar.Panels.Items[1].Text:='URL: none';
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TFormReader.FormShow(Sender: TObject);
begin
  ChromiumWindow.ChromiumBrowser.OnBeforePopup:=Chromium_OnBeforePopup;
  if not(ChromiumWindow.CreateBrowser) then Timer.Enabled:=True;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- BEFORE FORM CLOSE }
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

{ ---------------------------------------------------------------------------------------------------------------------------------------- TIMER FOR CHROMIUM }
procedure TFormReader.TimerTimer(Sender: TObject);
begin
  Timer.Enabled:=False;
  if not(ChromiumWindow.CreateBrowser) and not(ChromiumWindow.Initialized) then
    Timer.Enabled:=True;
end;

{ ----------------------------------------------------------------- ! CHROMIUM ! ---------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------- IGNORE TABS AND POP-UPS }
procedure TFormReader.Chromium_OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  Result:=(targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CREATE BROWSER }
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

{ ------------------------------------------------------------------------------------------------------------------------------------- CHROMIUM BEFORE CLOSE }
procedure TFormReader.ChromiumWindowBeforeClose(Sender: TObject);
begin
  FCanClose:=True;
  Close;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------------- CLOSE CHROMIUM }
procedure TFormReader.ChromiumWindowClose(Sender: TObject);
begin
  if not(ChromiumWindow.DestroyChildWindow) then
  begin
    FCanClose:=True;
    Close;
  end;
end;

end.
