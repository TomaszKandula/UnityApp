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
    Unity.Records;


type


    TRateForm = class(TForm)
    published
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
    public
        var FSetLastSelection: TTabSheet;
    end;


    function RateForm(): TRateForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Async.Accounts,
    Unity.Settings,
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

    var Settings: ISettings:=TSettings.Create();
    case RatingNumber of

        0:
        begin
            Image1.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image2.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image3.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image4.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
        end;

        1:
        begin
            Image1.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image3.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image4.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
        end;

        2:
        begin
            Image1.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image4.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
        end;

        3:
        begin
            Image1.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image4.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
            Image5.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
        end;

        4:
        begin
            Image1.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image4.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image5.Picture.LoadFromFile(Settings.DirAssets + 'Rating0.png');
        end;

        5:
        begin
            Image1.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image2.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image3.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image4.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
            Image5.Picture.LoadFromFile(Settings.DirAssets + 'Rating1.png');
        end;

    end;

end;


{$ENDREGION}


{$REGION 'CALLBACKS'}


//...


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TRateForm.FormCreate(Sender: TObject);
begin
    PanelReportMemo.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
end;


procedure TRateForm.FormShow(Sender: TObject);
begin
    SetRatingStars(0);
end;


procedure TRateForm.FormActivate(Sender: TObject);
begin

    if not FIsDataLoaded then
    begin

        Screen.Cursor:=crHourGlass;

        THelpers.ExecWithDelay(500, procedure
        begin

            var Accounts: IAccounts:=TAccounts.Create();
            var CallResponse: TCallResponse;
            var Rating: TRating;
            CallResponse:=Accounts.GetUserRatingAwaited(Rating);

            if CallResponse.IsSucceeded then
            begin
                FSelectedRating:=Rating.UserRating;
                ReportMemo.Text:=Rating.UserComment;
                SetRatingStars(FSelectedRating);
                FIsAppRated:=True;
            end;

            Screen.Cursor:=crDefault;
            FIsDataLoaded:=True;

        end);

    end;

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TRateForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FIsDataLoaded:=False;
    FSelectedRating:=0;
    ReportMemo.Text:=String.Empty;
    MainForm.SetActiveTabsheet(FSetLastSelection);
end;


{$ENDREGION}


{$REGION 'BUTTON EVENTS'}


procedure TRateForm.btnSendRatingClick(Sender: TObject);
begin

    if FSelectedRating = 0 then
    begin
        THelpers.MsgCall(TAppMessage.Warn, 'Please rate the application.');
        Exit();
    end;

    if FIsAppRated then //...call "update"
    else //...call "new"

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
