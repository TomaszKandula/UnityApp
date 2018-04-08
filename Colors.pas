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
unit Colors;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, StdCtrls;

{ ----------------------------------------------------------------- ! MAIN CLASS ! -------------------------------------------------------------------------- }
type
  TColorsForm = class(TForm)
    ColorDialog: TColorDialog;
    AppMain: TShape;
    ColorBox_Today: TGroupBox;
    ColorBox_Past: TGroupBox;
    ColorBox_Future: TGroupBox;
    ColorList1: TComboBox;
    ColorBox1: TShape;
    btnToday: TSpeedButton;
    ColorList2: TComboBox;
    ColorBox2: TShape;
    btnPast: TSpeedButton;
    ColorList3: TComboBox;
    ColorBox3: TShape;
    btnFuture: TSpeedButton;
    ColorPreview1: TLabel;
    ColorPreview2: TLabel;
    ColorPreview3: TLabel;
    procedure btnTodayClick(Sender: TObject);
    procedure btnPastClick(Sender: TObject);
    procedure btnFutureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColorList1Select(Sender: TObject);
    procedure ColorList2Select(Sender: TObject);
    procedure ColorList3Select(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  ColorsForm: TColorsForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Main, Settings;

{$R *.dfm}

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TColorsForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    ColorsForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_COLORS', APPNAME);
  finally
    AppSettings.Free;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------------ SELECT DEFAULT }
  if (ColorList1.Items.Count > 0) and (ColorList2.Items.Count > 0) and (ColorList3.Items.Count > 0) then
  begin
    ColorList1.ItemIndex:=0;
    ColorList2.ItemIndex:=0;
    ColorList3.ItemIndex:=0;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TColorsForm.FormShow(Sender: TObject);
begin
  ColorList1.Items[ColorList1.ItemIndex];
  ColorList2.Items[ColorList1.ItemIndex];
  ColorList3.Items[ColorList1.ItemIndex];
  ColorList1Select(Self);
  ColorList2Select(Self);
  ColorList3Select(Self);
end;

{ --------------------------------------------------------------- ! COMPONENT EVENTS ! ---------------------------------------------------------------------- }

{ ------------------------------------------------------------------------------------------------------------------------- COLOR SELECTION | TODAY FOLLOW-UP }
procedure TColorsForm.ColorList1Select(Sender: TObject);
begin
  if ColorList1.Text = 'Font Color'       then ColorBox1.Brush.Color:=MainForm.TodayFColor;
  if ColorList1.Text = 'Background Color' then ColorBox1.Brush.Color:=MainForm.TodayBColor;
end;

{ -------------------------------------------------------------------------------------------------------------------------- COLOR SELECTION | PAST FOLLOW-UP }
procedure TColorsForm.ColorList2Select(Sender: TObject);
begin
  if ColorList2.Text = 'Font Color'       then ColorBox2.Brush.Color:=MainForm.PastFColor;
  if ColorList2.Text = 'Background Color' then ColorBox2.Brush.Color:=MainForm.PastBColor;
end;

{ ------------------------------------------------------------------------------------------------------------------------ COLOR SELECTION | FUTURE FOLLOW-UP }
procedure TColorsForm.ColorList3Select(Sender: TObject);
begin
  if ColorList3.Text = 'Font Color'       then ColorBox3.Brush.Color:=MainForm.FutureFColor;
  if ColorList3.Text = 'Background Color' then ColorBox3.Brush.Color:=MainForm.FutureBColor;
end;

{ ----------------------------------------------------------------- ! BUTTON CALLS ! ------------------------------------------------------------------------ }

{ -------------------------------------------------------------------------------------------------------------------------------------- COLOR PICKER | TODAY }
procedure TColorsForm.btnTodayClick(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    if ColorList1.Text = 'Font Color'       then MainForm.TodayFColor:=ColorDialog.Color;
    if ColorList1.Text = 'Background Color' then MainForm.TodayBColor:=ColorDialog.Color;
    ColorBox1.Brush.Color:=ColorDialog.Color;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- COLOR PICKER | PAST }
procedure TColorsForm.btnPastClick(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    if ColorList2.Text = 'Font Color'       then MainForm.PastFColor:=ColorDialog.Color;
    if ColorList2.Text = 'Background Color' then MainForm.PastBColor:=ColorDialog.Color;
    ColorBox2.Brush.Color:=ColorDialog.Color;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- COLOR PICKER | FUTURE }
procedure TColorsForm.btnFutureClick(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    if ColorList3.Text = 'Font Color'       then MainForm.FutureFColor:=ColorDialog.Color;
    if ColorList3.Text = 'Background Color' then MainForm.FutureBColor:=ColorDialog.Color;
    ColorBox3.Brush.Color:=ColorDialog.Color;
  end;
end;

end.
