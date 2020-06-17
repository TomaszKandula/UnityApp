unit View.RateApp;


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
    Vcl.Imaging.pngimage,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    Vcl.ComCtrls,
    Unity.Enums,
    Unity.Panel,
    Unity.Records,
    Api.UserRating;


type


    TRateForm = class(TForm)
        PanelBottom: TPanel;
        PanelReportMemo: TPanel;
        PanelClient: TPanel;
        PanelStars: TPanel;
        PanelText: TPanel;
        ReportMemo: TMemo;
        txtCaption: TLabel;
        txtTotalWords: TLabel;
        txtWords: TLabel;
        txtComment: TLabel;
        Image1: TImage;
        Image2: TImage;
        Image3: TImage;
        Image4: TImage;
        Image5: TImage;
        btnSendRating: TSpeedButton;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
        procedure btnSendRatingClick(Sender: TObject);
        procedure ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Image1MouseEnter(Sender: TObject);
        procedure Image1MouseLeave(Sender: TObject);
        procedure Image1Click(Sender: TObject);
        procedure Image2MouseEnter(Sender: TObject);
        procedure Image2MouseLeave(Sender: TObject);
        procedure Image2Click(Sender: TObject);
        procedure Image3MouseEnter(Sender: TObject);
        procedure Image3MouseLeave(Sender: TObject);
        procedure Image3Click(Sender: TObject);
        procedure Image4MouseEnter(Sender: TObject);
        procedure Image4MouseLeave(Sender: TObject);
        procedure Image4Click(Sender: TObject);
        procedure Image5MouseEnter(Sender: TObject);
        procedure Image5MouseLeave(Sender: TObject);
        procedure Image5Click(Sender: TObject);
    strict private
        var FSelectedRating: cardinal;
        var FIsAppRated: boolean;
        var FIsDataLoaded: boolean;
        procedure SetRatingStars(RatingNumber: integer);
        procedure InsertRating();
        procedure UpdateRating();
        procedure LoadRating_Callback(PayLoad: TUserRating);
        procedure SubmitRating_Callback(CallResponse: TCallResponse);
        procedure UpdateRating_Callback(CallResponse: TCallResponse);
    public
        var FSetLastSelection: TTabSheet;
    end;


    function RateForm(): TRateForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Service,
    Unity.Helpers;


var vRateForm: TRateForm;


function RateForm(): TRateForm;
begin
    if not(Assigned(vRateForm)) then Application.CreateForm(TRateForm, vRateForm);
    Result:=vRateForm;
end;


{$REGION 'HELPERS'}


procedure TRateForm.SetRatingStars(RatingNumber: integer);
begin

    case RatingNumber of

        0:
        begin
            Image1.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image2.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image3.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image4.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
        end;

        1:
        begin
            Image1.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image3.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image4.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
        end;

        2:
        begin
            Image1.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image4.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
        end;

        3:
        begin
            Image1.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image4.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
        end;

        4:
        begin
            Image1.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image4.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image5.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating0.png');
        end;

        5:
        begin
            Image1.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image4.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
            Image5.Picture.LoadFromFile(Service.Settings.DirAssets + 'Rating1.png');
        end;

    end;

end;


procedure TRateForm.InsertRating();
begin

    var UserRating: TRating;
    UserRating.UserRating:=FSelectedRating;
    UserRating.UserComment:=ReportMemo.Text;

    Screen.Cursor:=crHourGlass;
    ReportMemo.Enabled:=False;
    btnSendRating.Enabled:=False;

    Service.Mediator.Ratings.SubmitRatingAsync(UserRating, SubmitRating_Callback);

end;


procedure TRateForm.UpdateRating();
begin

    var UserRating: TRating;
    UserRating.UserRating:=FSelectedRating;
    UserRating.UserComment:=ReportMemo.Text;

    Screen.Cursor:=crHourGlass;
    ReportMemo.Enabled:=False;
    btnSendRating.Enabled:=False;

    Service.Mediator.Ratings.UpdateRatingAsync(UserRating, UpdateRating_Callback);

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


procedure TRateForm.LoadRating_Callback(PayLoad: TUserRating);
begin

    FIsDataLoaded:=True;
    Screen.Cursor:=crDefault;
    ReportMemo.Enabled:=True;
    btnSendRating.Enabled:=True;

    if not PayLoad.IsSucceeded then
    begin
        THelpers.MsgCall(RateForm.Handle, TAppMessage.Error, PayLoad.Error.ErrorDesc);
        Exit();
    end;

    if PayLoad.Rating = 0 then Exit();

    FSelectedRating:=PayLoad.Rating;
    ReportMemo.Text:=PayLoad.Comment;
    SetRatingStars(FSelectedRating);
    FIsAppRated:=True;

end;


procedure TRateForm.SubmitRating_Callback(CallResponse: TCallResponse);
begin

    Screen.Cursor:=crDefault;
    ReportMemo.Enabled:=True;
    btnSendRating.Enabled:=True;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(RateForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(RateForm.Handle,TAppMessage.Info, 'Your rating has been submitted. Thank you!');

end;


procedure TRateForm.UpdateRating_Callback(CallResponse: TCallResponse);
begin

    Screen.Cursor:=crDefault;
    ReportMemo.Enabled:=True;
    btnSendRating.Enabled:=True;

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(RateForm.Handle, TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

    THelpers.MsgCall(RateForm.Handle, TAppMessage.Info, 'Your rating has been updated.');

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TRateForm.FormCreate(Sender: TObject);
begin
    PanelReportMemo.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
end;


procedure TRateForm.FormShow(Sender: TObject);
begin
    SetRatingStars(0);
    ReportMemo.Enabled:=False;
    btnSendRating.Enabled:=False;
end;


procedure TRateForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin
        Screen.Cursor:=crHourGlass;
        Service.Mediator.Ratings.LoadRatingAsync(LoadRating_Callback);
    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TRateForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsDataLoaded:=False;
    FIsAppRated:=False;
    FSelectedRating:=0;
    btnSendRating.Enabled:=True;
    ReportMemo.Enabled:=True;
    ReportMemo.Text:=String.Empty;
    MainForm.SetActiveTabsheet(FSetLastSelection);
end;


{$ENDREGION}


{$REGION 'BUTTON EVENTS'}


procedure TRateForm.btnSendRatingClick(Sender: TObject);
begin

    if FSelectedRating = 0 then
    begin
        THelpers.MsgCall(RateForm.Handle, TAppMessage.Warn, 'Please rate the application.');
        Exit();
    end;

    if FIsAppRated then THelpers.ExecWithDelay(300, UpdateRating)
        else THelpers.ExecWithDelay(300, InsertRating);

end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TRateForm.Image1MouseEnter(Sender: TObject);
begin
    SetRatingStars(1);
end;


procedure TRateForm.Image1MouseLeave(Sender: TObject);
begin
    SetRatingStars(FSelectedRating);
end;


procedure TRateForm.Image1Click(Sender: TObject);
begin
    FSelectedRating:=1;
end;


procedure TRateForm.Image2MouseEnter(Sender: TObject);
begin
    SetRatingStars(2);
end;


procedure TRateForm.Image2MouseLeave(Sender: TObject);
begin
    SetRatingStars(FSelectedRating);
end;


procedure TRateForm.Image2Click(Sender: TObject);
begin
    FSelectedRating:=2;
end;


procedure TRateForm.Image3MouseEnter(Sender: TObject);
begin
    SetRatingStars(3);
end;


procedure TRateForm.Image3MouseLeave(Sender: TObject);
begin
    SetRatingStars(FSelectedRating);
end;


procedure TRateForm.Image3Click(Sender: TObject);
begin
    FSelectedRating:=3;
end;


procedure TRateForm.Image4MouseEnter(Sender: TObject);
begin
    SetRatingStars(4);
end;


procedure TRateForm.Image4MouseLeave(Sender: TObject);
begin
    SetRatingStars(FSelectedRating);
end;


procedure TRateForm.Image4Click(Sender: TObject);
begin
    FSelectedRating:=4;
end;


procedure TRateForm.Image5MouseEnter(Sender: TObject);
begin
    SetRatingStars(5);
end;


procedure TRateForm.Image5MouseLeave(Sender: TObject);
begin
    SetRatingStars(FSelectedRating);
end;


procedure TRateForm.Image5Click(Sender: TObject);
begin
    SetRatingStars(5);
    FSelectedRating:=5;
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TRateForm.ReportMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    txtTotalWords.Caption:=IntToStr(THelpers.WordCount(ReportMemo.Text)) + ' / ' + IntToStr(ReportMemo.MaxLength);
end;


procedure TRateForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

