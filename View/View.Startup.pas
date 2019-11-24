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
    Data.Win.ADODB,
    Unity.Records;


type


    TStartupForm {representing main application window} = class(TForm)
        MainText2: TLabel;
        ProgressBar: TGauge;
        TextFooterA: TLabel;
        TextFooterB: TLabel;
        MainText1: TLabel;
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
        var DbConnection: TADOConnection; {for sql server connection check on startup}
        var FIsAppInitialized: boolean;
        var FCurrentSessionLog: string;
        procedure AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);
        procedure ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);
        procedure ExitAppSync();
        procedure ApplicationStart();
        function GetScreenDataSync(): boolean;
        function GetDbConnectionSync(): boolean;
        function GetGeneralTablesSync(): boolean;
        function GetHtmlLayoutsSync(UrlLayoutPak: string; FileLayoutPak: string; LayoutDir: string): boolean;
        procedure GeneralTables_Callback(CallResponse: TCallResponse);
    public
        property IsAppInitialized: boolean read FIsAppInitialized;
        procedure SetSessionLog(SessionEventLog: string);
    end;


    function StartupForm(): TStartupForm;


implementation


{$R *.dfm}


uses
    Unity.Enums,
    Unity.Common,
    Unity.Settings,
    Unity.Helpers,
    Unity.Sql,
    Unity.Chars,
    Unity.EventLogger,
    Unity.SessionService,
    Async.Utilities,
    Async.Queries,
    REST.Types,
    System.Net.HttpClient,
    Handler.Rest,
    Handler.Database,
    View.Main,
    DbModel;


var
    VStartupForm: TStartupForm;
    MainAppForm: View.Main.TMainForm;


function StartupForm(): TStartupForm;
begin
    if not(Assigned(VStartupForm)) then Application.CreateForm(TStartupForm, VStartupForm);
    Result:=VStartupForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


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
        MainAppForm.InitMainWnd(FCurrentSessionLog);

    var NewTask: ITask:=TTask.Create(procedure
    begin

        // -------------------------------
        // Get captions from app settings.
        // -------------------------------
        Sleep(50);
        ChangeProgressBar(5, 'Initializing...', ProgressBar);

        if not GetScreenDataSync() then
        begin
            ThreadFileLog.Log('Critical error has occured [GetScreenDataSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error has occured [GetScreenDataSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.');
            ExitAppSync();
        end
        else
        begin
            ThreadFileLog.Log('Unity has been boot up.');
            ChangeProgressBar(15, 'Initializing... done.', ProgressBar);
        end;

        // -----------------------------------------
        // Establish persistent database connection.  // delete when REST is introduced
        // -----------------------------------------
        Sleep(250);
        ChangeProgressBar(20, 'Connecting to database...', ProgressBar);

        if not GetDbConnectionSync() then
        begin
            ThreadFileLog.Log('Critical error has occured [GetDbConnectionSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error occured [GetDbConnectionSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.');
            ExitAppSync();
        end
        else
        begin
            SessionService.FDbConnect:=DbConnection;
            ThreadFileLog.Log('Persistant connection with SQL database has been established.');
            ChangeProgressBar(33, 'Connecting to database... done.', ProgressBar);
        end;

        // -------------------------------
        // Load all helper general tables.
        // -------------------------------
        Sleep(250);
        ChangeProgressBar(85, 'Loading general tables...', ProgressBar);

        if not GetGeneralTablesSync() then
        begin
            ThreadFileLog.Log('Critical error occured [GetGeneralTablesSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error has occured [GetGeneralTablesSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.');
            ExitAppSync();
        end
        else
        begin
            ThreadFileLog.Log('General tables has been loaded.');
            ChangeProgressBar(95, 'Loading general tables... done.', ProgressBar);
        end;

        // ----------------------------------------
        // Hide startup view and display main view.
        // ----------------------------------------
        Sleep(50);
        ChangeProgressBar(100, 'Finalization...', ProgressBar);

        Sleep(1450);
        TThread.Synchronize(nil, procedure
        begin
            FIsAppInitialized:=True;
            AnimateWindow(StartupForm.Handle, 750, AW_BLEND or AW_HIDE);
            MainAppForm.Show();
        end);

    end);

    NewTask.Start();

end;


function TStartupForm.GetScreenDataSync(): boolean;
begin

    Result:=True;

    var Settings: ISettings:=TSettings.Create();
    try

        // ---------------------
        // Application captions.
        // ---------------------
        MainAppForm.Caption:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'WND_MAIN', TCommon.APPCAPTION);
        MainAppForm.valUpdateStamp.Caption:='';

        // ----------------------------------
        // Icon used in main aging view grid.
        // ----------------------------------
        MainAppForm.FGridPicture:=TImage.Create(MainForm);
        MainAppForm.FGridPicture.SetBounds(0, 0, 16, 16);
        THelpers.LoadImageFromStream(View.Main.MainForm.FGridPicture, Settings.GetPathGridImage);

        // ---------------------------------------------------------------------
        // Setup timers, some of them may be removed after introducing REST API.
        // ---------------------------------------------------------------------
        MainAppForm.TimerFollowUp.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'FOLLOWUP_CHECKER', 1800000{30 minutes});
        MainAppForm.TimerCustOpenItems.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'OI_LOADER', 300000{5 minutes});

        // ---------------------------------------------
        // Setup risk classes with proper number format.
        // ---------------------------------------------
        if FormatSettings.DecimalSeparator = ',' then
        begin

            var getRiskClassA:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', '0');
            var getRiskClassB:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', '0');
            var getRiskClassC:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', '0');

            var RiskClassA:=((getRiskClassA).ToExtended * 100).ToString + '%';
            var RiskClassB:=((getRiskClassB).ToExtended * 100).ToString + '%';
            var RiskClassC:=((getRiskClassC).ToExtended * 100).ToString + '%';

            MainAppForm.valRiskClassA.Caption:=RiskClassA;
            MainAppForm.valRiskClassB.Caption:=RiskClassB;
            MainAppForm.valRiskClassC.Caption:=RiskClassC;

            MainAppForm.FClass_A:=StrToFloat(getRiskClassA);
            MainAppForm.FClass_B:=StrToFloat(getRiskClassB);
            MainAppForm.FClass_C:=StrToFloat(getRiskClassC);

        end else if FormatSettings.DecimalSeparator = '.' then
        begin

            var getRiskClassA:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', '0');
            var getRiskClassB:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', '0');
            var getRiskClassC:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', '0');

            var RiskClassA:=((StringReplace(getRiskClassA, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassB:=((StringReplace(getRiskClassB, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassC:=((StringReplace(getRiskClassC, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';

            MainAppForm.valRiskClassA.Caption:=RiskClassA;
            MainAppForm.valRiskClassB.Caption:=RiskClassB;
            MainAppForm.valRiskClassC.Caption:=RiskClassC;

            MainAppForm.FClass_A:=StrToFloat(getRiskClassA.Replace(',','.'));
            MainAppForm.FClass_B:=StrToFloat(getRiskClassB.Replace(',','.'));
            MainAppForm.FClass_C:=StrToFloat(getRiskClassC.Replace(',','.'));

        end;

        // -----------------------------------
        // Tabsheet captions and aging ranges.
        // -----------------------------------
        MainAppForm.ShapeSelectionCap.ShapeText(10, 1, 'SELECTION', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeRiskClassCap.ShapeText(10, 1, 'RISK CLASS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeDetailsCap.ShapeText(10, 1, 'AGING DETAILS', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeSummaryCap.ShapeText(10, 1, 'SUMMARY', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.ShapeExceedersCap.ShapeText(10, 1, 'CREDIT LIMITS', [fsBold], 'Tahoma', 10, clWhite);

        MainAppForm.Cap10.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap12.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap13.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap43.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap61.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap15.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap21.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap22.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap23.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap62.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);
        MainAppForm.Cap63.ShapeText(10, 1, '', [fsBold], 'Tahoma', 10, clWhite);

        var getRange1A:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE1A','');
        var getRange2A:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE2A','');
        var getRange3A:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE3A','');
        var getRange4A:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE4A','');
        var getRange5A:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE5A','');
        var getRange6A:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE6A','');

        var getRange1B:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE1B','');
        var getRange2B:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE2B','');
        var getRange3B:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE3B','');
        var getRange4B:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE4B','');
        var getRange5B:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE5B','');
        var getRange6B:=Settings.GetStringValue(TConfigSections.AgingRanges,'RANGE6B','');

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


function TStartupForm.GetDbConnectionSync(): boolean;
begin

    Result:=True;

    try

        DbConnection:=TADOConnection.Create(nil);
        var DataBase:=TDataBase.Create(False);
        try

            if DataBase.Check = 0 then
            begin
                DataBase.InitializeConnection(False, DbConnection);
                MainAppForm.FIsConnected:=True;
            end;

        finally
            DataBase.Free();
        end;

    except
        on E: Exception do
        begin
            LastErrorMsg:=E.Message;
            Result:=False;
        end;

    end;

end;


function TStartupForm.GetGeneralTablesSync(): boolean;
begin

    Result:=True;

    var Utilities: IUtilities:=TUtilities.Create();
    try

        Utilities.GeneralTablesAsync(TCompanyData.CompanyData, MainAppForm.sgCoCodes, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TControlStatus.ControlStatus, MainAppForm.sgControlStatus, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TPaidinfo.Paidinfo, MainAppForm.sgPaidInfo, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TPaymentTerms.PaymentTerms, MainAppForm.sgPmtTerms, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TSalesResponsible.SalesResponsible, MainAppForm.sgSalesResp, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TPersonResponsible.PersonResponsible, MainAppForm.sgPersonResp, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TAccountType.AccountType, MainAppForm.sgAccountType, GeneralTables_Callback, '', '', True);
        Utilities.GeneralTablesAsync(TCustomerGroup.CustomerGroup, MainAppForm.sgCustomerGr, GeneralTables_Callback, '', '', True);

    except
        on E: Exception do
        begin
            LastErrorMsg:=E.Message;
            Result:=False;
        end;

    end;

end;


function TStartupForm.GetHtmlLayoutsSync(UrlLayoutPak: string; FileLayoutPak: string; LayoutDir: string): boolean;
begin

    Result:=True;

    var HttpResponse: IHttpResponse;
    var HttpClient:   THttpClient:=THTTPClient.Create();
    var FileStream:   TFileStream:=TFileStream.Create(FileLayoutPak, fmCreate);
    try

        try
            HttpResponse:=HttpClient.Get(UrlLayoutPak);
            FileStream.CopyFrom(HttpResponse.ContentStream, HttpResponse.ContentLength);
        finally
            HttpClient.Free();
            FileStream.Free();
        end;

    except
        on E: Exception do
        begin
            LastErrorMsg:=E.Message;
            Result:=False;
        end;

    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------------- CALLBACKS //


procedure TStartupForm.GeneralTables_Callback(CallResponse: TCallResponse);
begin

    if not CallResponse.IsSucceeded then
    begin
        THelpers.MsgCall(TAppMessage.Error, CallResponse.LastMessage);
        Exit();
    end;

end;


// ----------------------------------------------------------------------------------------------------------------------------------------------- APPERANCE //


procedure TStartupForm.CreateParams(var Params: TCreateParams);
begin

    inherited CreateParams(Params);

    with Params do begin
        Style:=WS_POPUP;
        WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TStartupForm.FormCreate(Sender: TObject);
begin
    {Do nothing}
end;


procedure TStartupForm.FormDestroy(Sender: TObject);
begin
    DestroyThreadFileLog();
    DestroySessionService();
end;


procedure TStartupForm.FormShow(Sender: TObject);
begin
    TextStatus.Caption:='';
    LabelVersion.Caption:='Version ' + THelpers.GetBuildInfoAsString + '.';
end;


procedure TStartupForm.FormActivate(Sender: TObject);
begin
    ApplicationStart();
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- CLICK EVENTS //


procedure TStartupForm.LabelHideClick(Sender: TObject);
begin
    StartupForm.WindowState:=wsMinimized;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


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
    LabelHide.Font.Color:=$FFFFFF;
end;


procedure TStartupForm.LabelHideMouseLeave(Sender: TObject);
begin
    ShapeHide.Brush.Color:=$FFFFFF;
    LabelHide.Font.Color:=$00EDE6DD;
end;


end.

