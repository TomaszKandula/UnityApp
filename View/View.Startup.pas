unit View.Startup;

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
    System.Threading,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.Samples.Gauges,
    Vcl.StdCtrls,
    Vcl.Imaging.pngimage,
    Vcl.Imaging.jpeg,
    Unity.Records;


type


    TStartupForm = class(TForm) // Representing main application window
        MainText2: TLabel;
        ProgressBar: TGauge;
        TextFooterA: TLabel;
        TextFooterB: TLabel;
        TextStatus: TLabel;
        TextSubtitle: TLabel;
        ShapeFooter: TShape;
        ShapeBackground: TShape;
        ShapeHide: TShape;
        LabelHide: TLabel;
        imgDfds: TImage;
        imgShip: TImage;
        LabelVersion: TLabel;
        ShapeLine: TShape;
        imgCover: TImage;
        imgUnityLogo: TImage;
        LabelTest: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure LabelHideMouseEnter(Sender: TObject);
        procedure LabelHideMouseLeave(Sender: TObject);
        procedure LabelHideClick(Sender: TObject);
        procedure ShapeBackgroundMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    protected
        procedure CreateParams(var Params: TCreateParams); override;
    strict private
        var LastErrorMsg: string;
        var FIsAppInitialized: boolean;
        var FCurrentSessionLog: string;
        procedure AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);
        procedure ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);
        procedure ExitAppSync();
        procedure ApplicationStart();
        function GetAccessTokenAsync(): boolean;
        function GetScreenDataSync(): boolean;
        function GetGeneralTablesAsync(): boolean;
    public
        property IsAppInitialized: boolean read FIsAppInitialized;
        procedure SetSessionLog(SessionEventLog: string);
    end;


    function StartupForm(): TStartupForm;


implementation


{$R *.dfm}


uses
    System.Net.HttpClient,
    REST.Types,
    Unity.RestWrapper,
    Unity.Enums,
    Unity.Constants,
    Unity.Helpers,
    Unity.Service,
    View.Main;


var
    VStartupForm: TStartupForm;
    MainAppForm: View.Main.TMainForm;


function StartupForm(): TStartupForm;
begin
    if not(Assigned(VStartupForm)) then Application.CreateForm(TStartupForm, VStartupForm);
    Result:=VStartupForm;
end;


{$REGION 'LOCAL HELPERS'}


procedure TStartupForm.AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);
begin

    if Speed = 0 then Speed:=5;

    var ExecTimerAsync: ITask:=TTask.Create(procedure
    begin

        for var iCNT:=AniFrom to AniTo do
        begin

            Sleep(Speed);

            TThread.Synchronize(nil, procedure
            begin

                if AniTo > ProgressBar.MaxValue then
                    AniTo:=ProgressBar.MaxValue;

                ProgressBar.Progress:=iCNT;
                Update();

            end);

        end;

    end);

    ExecTimerAsync.Start();

end;


procedure TStartupForm.ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);
begin
    AnimateProgressBar(ProgressBar.Progress, ProgressTarget, ProgressBar);
    TextStatus.Caption:=Text;
    Update();
end;


procedure TStartupForm.SetSessionLog(SessionEventLog: string);
begin
    FCurrentSessionLog:=SessionEventLog;
end;


procedure TStartupForm.ExitAppSync();
begin

    TTask.CurrentTask.Cancel();
    TThread.Synchronize(nil, procedure
    begin
        ExitProcess(0);
    end);

end;


procedure TStartupForm.ApplicationStart();
begin

    // -------------------------------------------------------------------------------
    // We may operate on visual components from worker thread instead of main thread.
    // Although, VCL is not thread safe we should refrain from doing that,
    // but as long as we do not display visual content and/or update its visual state,
    // we can safely perform operations on VCLs from worker thread.
    // -------------------------------------------------------------------------------
    if FIsAppInitialized then Exit();

    if not Assigned(MainAppForm) then
        MainAppForm:=View.Main.MainForm;

    if not String.IsNullOrEmpty(FCurrentSessionLog) then
        MainAppForm.InitMainWnd();

    if Service.Settings.IsUsedCustomConfig then
        LabelTest.Visible:=True;

    if Service.Settings.IsTestEnvSetup then
        MainAppForm.AppNotification.Visible:=True;

    var NewTask: ITask:=TTask.Create(procedure
    begin

        // ---------------------------
        // Application initialization.
        // ---------------------------
        Sleep(50);
        ChangeProgressBar(5, 'Initializing...', ProgressBar);

        if not GetAccessTokenAsync() then
        begin
            Service.Logger.Log('Critical error has occured [GetAccessTokenSync]: ' + LastErrorMsg);
            THelpers.MsgCall(
                StartupForm.Handle,
                TAppMessage.Error,
                'An error has occured [GetAccessTokenSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.'
            );
            ExitAppSync();
        end
        else
        begin
            Service.Logger.Log('Access token has been granted.');
            ChangeProgressBar(25, 'Getting access token... done.', ProgressBar);
        end;

        // -------------------------
        // Register current session.
        // -------------------------
        Sleep(50);
        ChangeProgressBar(33, 'Registering session...', ProgressBar);

        if not GetScreenDataSync() then
        begin

            Service.Logger.Log('Critical error has occured [GetScreenDataSync]: ' + LastErrorMsg);
            THelpers.MsgCall(
                StartupForm.Handle,
                TAppMessage.Error,
                'An error has occured [GetScreenDataSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.'
            );
            ExitAppSync();

        end
        else
        begin

            // --------------------------------
            // API call to register session ID.
            // --------------------------------
            Service.Mediator.Sessions.InitiateSessionAwaited(Service.SessionId, Service.Settings.WinUserName);

            Service.Logger.Log('Unity has been boot up.');
            ChangeProgressBar(50, 'Initializing... done.', ProgressBar);

        end;

        // -------------------------
        // General tables API calls.
        // -------------------------
        Sleep(50);
        ChangeProgressBar(85, 'Calling general tables loaders...', ProgressBar);

        if not GetGeneralTablesAsync() then
        begin
            Service.Logger.Log('Critical error occured [GetGeneralTablesSync]: ' + LastErrorMsg);
            THelpers.MsgCall(
                StartupForm.Handle,
                TAppMessage.Error,
                'An error has occured [GetGeneralTablesSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.'
            );
            ExitAppSync();
        end
        else
        begin
            Service.Logger.Log('Calling general tables loaders.');
            ChangeProgressBar(95, 'Calling general tables loaders... executed.', ProgressBar);
        end;

        // ----------------------------------------
        // Hide startup view and display main view.
        // ----------------------------------------
        Sleep(50);
        ChangeProgressBar(100, 'Finalization...', ProgressBar);

        Sleep(1500);
        TThread.Synchronize(nil, procedure
        begin
            FIsAppInitialized:=True;
            AnimateWindow(StartupForm.Handle, 750, AW_BLEND or AW_HIDE);
            MainAppForm.Show();
            Service.Logger.Log('[GUI]: Main form has been called.');
        end);

    end);

    NewTask.Start();

end;


function TStartupForm.GetAccessTokenAsync(): boolean;
begin

    Result:=True;

    var LCallResponse: TCallResponse;
    var LNewAccessToken: string;

    LCallResponse:=Service.Mediator.Sessions.RequestAccessTokenAwaited(LNewAccessToken);

    if not LCallResponse.IsSucceeded then
    begin
        LastErrorMsg:=LCallResponse.LastMessage;
        Result:=False;
        Exit();
    end;

    Service.AccessToken:=LNewAccessToken;

end;


function TStartupForm.GetScreenDataSync(): boolean;
begin

    Result:=True;

    try

        // ---------------------
        // Application captions.
        // ---------------------
        MainAppForm.Caption:=Service.Settings.GetStringValue(TConfigSections.ApplicationDetails, 'WND_MAIN', TCommon.APPCAPTION);
        MainAppForm.valUpdateStamp.Caption:='';

        // ----------------------------------
        // Icon used in main aging view grid.
        // ----------------------------------
        MainAppForm.FGridPicture:=TImage.Create(MainForm);
        MainAppForm.FGridPicture.SetBounds(0, 0, 16, 16);
        //THelpers.LoadImageFromStream(View.Main.MainForm.FGridPicture, Service.Settings.DirAssets + 'Star.bmp');

        // ---------------------------------------------------------------------
        // Setup timers, some of them may be removed after introducing REST API.
        // ---------------------------------------------------------------------
        MainAppForm.TimerFollowUp.Interval:=Service.Settings.GetIntegerValue(TConfigSections.TimersSettings, 'FOLLOWUP_CHECKER', 1800000); // 30 minutes
        MainAppForm.TimerCustSnapshots.Interval:=Service.Settings.GetIntegerValue(TConfigSections.TimersSettings, 'OI_LOADER', 300000); // 5 minutes

        // ---------------------------------------------
        // Setup risk classes with proper number format.
        // ---------------------------------------------
        if FormatSettings.DecimalSeparator = ',' then
        begin

            var getRiskClassA:=Service.Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', '0');
            var getRiskClassB:=Service.Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', '0');
            var getRiskClassC:=Service.Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', '0');

            var RiskClassA:=((getRiskClassA).ToExtended * 100).ToString + '%';
            var RiskClassB:=((getRiskClassB).ToExtended * 100).ToString + '%';
            var RiskClassC:=((getRiskClassC).ToExtended * 100).ToString + '%';

            MainAppForm.valRiskClassA.Caption:=RiskClassA;
            MainAppForm.valRiskClassB.Caption:=RiskClassB;
            MainAppForm.valRiskClassC.Caption:=RiskClassC;

            MainAppForm.FRiskClassGroup.Class_A:=StrToFloat(getRiskClassA);
            MainAppForm.FRiskClassGroup.Class_B:=StrToFloat(getRiskClassB);
            MainAppForm.FRiskClassGroup.Class_C:=StrToFloat(getRiskClassC);

        end else if FormatSettings.DecimalSeparator = '.' then
        begin

            var getRiskClassA:=Service.Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', '0');
            var getRiskClassB:=Service.Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', '0');
            var getRiskClassC:=Service.Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', '0');

            var RiskClassA:=((StringReplace(getRiskClassA, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassB:=((StringReplace(getRiskClassB, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassC:=((StringReplace(getRiskClassC, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';

            MainAppForm.valRiskClassA.Caption:=RiskClassA;
            MainAppForm.valRiskClassB.Caption:=RiskClassB;
            MainAppForm.valRiskClassC.Caption:=RiskClassC;

            MainAppForm.FRiskClassGroup.Class_A:=StrToFloat(getRiskClassA.Replace(',','.'));
            MainAppForm.FRiskClassGroup.Class_B:=StrToFloat(getRiskClassB.Replace(',','.'));
            MainAppForm.FRiskClassGroup.Class_C:=StrToFloat(getRiskClassC.Replace(',','.'));

        end;

        // -----------------------------------
        // Tabsheet captions and aging ranges.
        // -----------------------------------
        MainAppForm.ShapeSelectionCap.ShapeText(10, 1, 'SELECTION', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeRiskClassCap.ShapeText(10, 1, 'RISK CLASS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeFollowsCap.ShapeText(10, 1, 'FOLLOW-UPS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeDetailsCap.ShapeText(10, 1, 'AGING DETAILS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeSummaryCap.ShapeText(10, 1, 'SUMMARY', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeExceedersCap.ShapeText(10, 1, 'CREDIT LIMITS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeReloadCap.ShapeText(10, 1, 'ACTIONS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeSumsCap.ShapeText(10, 1, 'INFO', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeAddressBookCap.ShapeText(10, 1, 'ACTIONS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeTrackerInfoCap.ShapeText(10, 1, 'INFO', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeTablesInfoCap.ShapeText(10, 1, 'INFO', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeAdminEntryCap.ShapeText(10, 1, 'ADMINISTRATOR PANEL', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeAdminPassCap.ShapeText(10, 1, 'PASSWORD CHANGE', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeAppSettingsCap.ShapeText(10, 1, 'APPLICATION SETTINGS', [fsBold], 'Tahoma', 10, clWhite);

        var getRange1A:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE1A','');
        var getRange2A:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE2A','');
        var getRange3A:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE3A','');
        var getRange4A:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE4A','');
        var getRange5A:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE5A','');
        var getRange6A:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE6A','');

        var getRange1B:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE1B','');
        var getRange2B:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE2B','');
        var getRange3B:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE3B','');
        var getRange4B:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE4B','');
        var getRange5B:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE5B','');
        var getRange6B:=Service.Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE6B','');

        MainAppForm.txtRange1.Caption:=getRange1A + ' - ' + getRange1B;
        MainAppForm.txtRange2.Caption:=getRange2A + ' - ' + getRange2B;
        MainAppForm.txtRange3.Caption:=getRange3A + ' - ' + getRange3B;
        MainAppForm.txtRange4.Caption:=getRange4A + ' - ' + getRange4B;
        MainAppForm.txtRange5.Caption:=getRange5A + ' - ' + getRange5B;
        MainAppForm.txtRange6.Caption:=getRange6A + ' - ' + getRange6B;

        MainAppForm.txtPastDue.Caption:=getRange1A + ' - ' + getRange3B + ':';
        MainAppForm.txtDefaulted.Caption:=getRange4A + ' - ' + getRange6B + ':';

    except
        on E: Exception do
        begin
            LastErrorMsg:=E.Message;
            Result:=False;
        end;

    end;

end;


function TStartupForm.GetGeneralTablesAsync(): boolean;
begin

    Result:=True;
    try
        Service.Mediator.GeneralTables.GetCompaniesAsync(MainAppForm.sgCoCodes, nil);
        Service.Mediator.GeneralTables.GetControlStatusAsync(MainAppForm.sgControlStatus, nil);
        Service.Mediator.GeneralTables.GetPaidInfoAsync(MainAppForm.sgPaidInfo, nil);
        Service.Mediator.GeneralTables.GetPaymentTermsAsync(MainAppForm.sgPmtTerms, nil);
        Service.Mediator.GeneralTables.GetSalesResponsibleAsync(MainAppForm.sgSalesResp, nil);
        Service.Mediator.GeneralTables.GetPersonResponsibleAsync(MainAppForm.sgPersonResp, nil);
        Service.Mediator.GeneralTables.GetAccountTypeAsync(MainAppForm.sgAccountType, nil);
        Service.Mediator.GeneralTables.GetCustomerGroupAsync(MainAppForm.sgCustomerGr, nil);
    except
        on E: Exception do
        begin
            LastErrorMsg:=E.Message;
            Result:=False;
        end;

    end;

end;


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TStartupForm.FormCreate(Sender: TObject);
begin
    // Empty
end;


procedure TStartupForm.FormShow(Sender: TObject);
begin
    TextStatus.Caption:='';
    LabelVersion.Caption:='Version ' + THelpers.GetBuildInfoAsString + '.';
end;


procedure TStartupForm.FormActivate(Sender: TObject);
begin

//    var ScalingFactor:=TControl(TextSubtitle).ScaleFactor;
//
//    if ScalingFactor <> 1 then
//    begin
//        THelpers.MsgCall(
//            StartupForm.Handle,
//            TAppMessage.Error,
//            'Application can work only with screen scaled at 100% (your scale factor is '
//            + (ScalingFactor * 100).ToString() + '%). Application will be closed.'
//        );
//        ExitProcess(0);
//    end;

    ApplicationStart();

end;


{$ENDREGION}


{$REGION 'MISC. EVENTS'}


procedure TStartupForm.CreateParams(var Params: TCreateParams);
begin

    inherited CreateParams(Params);

    with Params do begin
        Style:=WS_POPUP;
        WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
    end;

end;


procedure TStartupForm.FormDestroy(Sender: TObject);
begin
    UnloadService();
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TStartupForm.LabelHideClick(Sender: TObject);
begin
    StartupForm.WindowState:=wsMinimized;
end;


{$ENDREGION}


{$REGION 'MOUSE MOVE EVENTS'}


procedure TStartupForm.ShapeBackgroundMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

    const SC_DRAGMOVE = $F012;

    if Button = mbLeft then
    begin
        ReleaseCapture;
        Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
    end;

end;


procedure TStartupForm.LabelHideMouseEnter(Sender: TObject);
begin
    ShapeHide.Brush.Color:=$00EDE6DD;
    LabelHide.Font.Color:=$00E3B268;
end;


procedure TStartupForm.LabelHideMouseLeave(Sender: TObject);
begin
    ShapeHide.Brush.Color:=$00E3B268;
    LabelHide.Font.Color:=$00EDE6DD;
end;


{$ENDREGION}


end.

