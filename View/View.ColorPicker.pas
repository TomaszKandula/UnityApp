
unit View.ColorPicker;

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
    Vcl.Buttons,
    Vcl.StdCtrls;


type

    /// <summary>
    /// Colors view displays option for colouring follow-up column (String Grid component) that contains
    /// dates assigned by credit controller. This dates indicates when Credit Controller should get back
    /// to the customer, etc.
    /// </summary>

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
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnTodayClick(Sender: TObject);
        procedure btnPastClick(Sender: TObject);
        procedure btnFutureClick(Sender: TObject);
        procedure ColorList1Select(Sender: TObject);
        procedure ColorList2Select(Sender: TObject);
        procedure ColorList3Select(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    end;


    function ColorsForm: TColorsForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Settings;


var vColorsForm: TColorsForm;


function ColorsForm: TColorsForm;
begin
    if not(Assigned(vColorsForm)) then Application.CreateForm(TColorsForm, vColorsForm);
    Result:=vColorsForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


/// <summary>
/// Setup caption and default colors for backgrounds and foregrounds.
/// </summary>

procedure TColorsForm.FormCreate(Sender: TObject);
begin
    if (ColorList1.Items.Count > 0) and (ColorList2.Items.Count > 0) and (ColorList3.Items.Count > 0) then
    begin
        ColorList1.ItemIndex:=0;
        ColorList2.ItemIndex:=0;
        ColorList3.ItemIndex:=0;
    end;
end;


procedure TColorsForm.FormShow(Sender: TObject);
begin
    ColorList1.Items[ColorList1.ItemIndex];
    ColorList2.Items[ColorList1.ItemIndex];
    ColorList3.Items[ColorList1.ItemIndex];
    ColorList1Select(Self);
    ColorList2Select(Self);
    ColorList3Select(Self);
end;


// ---------------------------------------------------------------------------------------------------------------------------------------- COMPONENT EVENTS //


/// <summary>
/// Get colors from application settings file.
/// </summary>

procedure TColorsForm.ColorList1Select(Sender: TObject);
begin
    var Settings: ISettings:=TSettings.Create;
    if ColorList1.Text = 'Font Color'       then ColorBox1.Brush.Color:=Settings.TodayFColor;
    if ColorList1.Text = 'Background Color' then ColorBox1.Brush.Color:=Settings.TodayBColor;
end;


procedure TColorsForm.ColorList2Select(Sender: TObject);
begin
    var Settings: TSettings:=TSettings.Create;
    if ColorList2.Text = 'Font Color'       then ColorBox2.Brush.Color:=Settings.PastFColor;
    if ColorList2.Text = 'Background Color' then ColorBox2.Brush.Color:=Settings.PastBColor;
end;


procedure TColorsForm.ColorList3Select(Sender: TObject);
begin
    var Settings: TSettings:=TSettings.Create;
    if ColorList3.Text = 'Font Color'       then ColorBox3.Brush.Color:=Settings.FutureFColor;
    if ColorList3.Text = 'Background Color' then ColorBox3.Brush.Color:=Settings.FutureBColor;
end;


procedure TColorsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- BUTTON CALLS //

/// <summary>
/// Set new colors into application settings file.
/// </summary>

procedure TColorsForm.btnTodayClick(Sender: TObject);
begin
    if ColorDialog.Execute then
    begin
        var Settings: TSettings:=TSettings.Create;
        if ColorList1.Text = 'Font Color'       then Settings.TodayFColor:=ColorDialog.Color;
        if ColorList1.Text = 'Background Color' then Settings.TodayBColor:=ColorDialog.Color;
        ColorBox1.Brush.Color:=ColorDialog.Color;
    end;
end;


procedure TColorsForm.btnPastClick(Sender: TObject);
begin
    if ColorDialog.Execute then
    begin
        var Settings: TSettings:=TSettings.Create;
        if ColorList2.Text = 'Font Color'       then Settings.PastFColor:=ColorDialog.Color;
        if ColorList2.Text = 'Background Color' then Settings.PastBColor:=ColorDialog.Color;
        ColorBox2.Brush.Color:=ColorDialog.Color;
    end;
end;


procedure TColorsForm.btnFutureClick(Sender: TObject);
begin
    if ColorDialog.Execute then
    begin
        var Settings: TSettings:=TSettings.Create;
        if ColorList3.Text = 'Font Color'       then Settings.FutureFColor:=ColorDialog.Color;
        if ColorList3.Text = 'Background Color' then Settings.FutureBColor:=ColorDialog.Color;
        ColorBox3.Brush.Color:=ColorDialog.Color;
    end;
end;


end.

