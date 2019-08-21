unit View.StartupScreen;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views use Lazy Initialization pattern.
// ------------------------------------------------------------------------------

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
    Unity.EventLogger;


type


    TStartupForm = class(TForm)
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
        var LastErrorMsg: string;
        var DbConnection: TADOConnection;
        procedure CreateParams(var Params: TCreateParams); override;
    private
        var FIsAppInitialized: boolean;
        var FCurrentSessionLog: string;
        procedure AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);
        procedure ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);
        procedure ExitAppSync();
        procedure ApplicationStart();
        function GetScreenDataSync(): boolean;
        function GetDbConnectionSync(): boolean;
        function GetUserAccountSync(): boolean;
        function GetGeneralTablesSync(): boolean;
    public
        property IsAppInitialized: boolean read FIsAppInitialized;
        procedure SetSessionLog(SessionEventLog: string);
    end;


    function StartupForm: TStartupForm;


implementation


{$R *.dfm}


uses
    Unity.Enums,
    Unity.Common,
    Unity.Settings,
    Unity.Helpers,
    Unity.RiskClass,
    Unity.UserAccess,
    Unity.Sql,
    Unity.Chars,
    Unity.UserSid,
    Async.Utilities,
    Async.Queries,
    Handler.Database,
    Handler.Account,
    View.Main,
    DbModel;


var
    VStartupForm: TStartupForm;
    MainAppForm: View.Main.TMainForm;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


function StartupForm: TStartupForm;
begin
    if not(Assigned(VStartupForm)) then Application.CreateForm(TStartupForm, VStartupForm);
    Result:=VStartupForm;
end;


procedure TStartupForm.FormCreate(Sender: TObject);
begin
    {Do nothing}
end;


procedure TStartupForm.FormDestroy(Sender: TObject);
begin
    {Do nothing}
end;


procedure TStartupForm.FormShow(Sender: TObject);
begin
    TextStatus.Caption:='';
    LabelVersion.Caption:='Version ' + TCommon.GetBuildInfoAsString + '.';
end;


procedure TStartupForm.FormActivate(Sender: TObject);
begin
    ApplicationStart();
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


/// <summary>
/// Load settings and update MainForm properties and variables before it gets shown to the user.
/// Initialize SQL database with persistent connection (this will be replaced by the REST service).
/// </summary>
/// <remarks>
/// We may operate on visual components from worker thread instead of main thread. Although, VCL is not
/// thread safe we should refrain from doing that, but as long as we do not display visual content/update
/// its visual state, we can safely perform operations on VCLs.
/// </remarks>

procedure TStartupForm.ApplicationStart();
begin

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
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error has occured [GetScreenDataSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error has occured [GetScreenDataSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.');
            ExitAppSync();
        end else
        begin
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'Unity has been boot up.');
            ChangeProgressBar(15, 'Initializing... done.', ProgressBar);
        end;

        // -----------------------------------------
        // Establish persistent database connection.
        // -----------------------------------------

        Sleep(250);
        ChangeProgressBar(20, 'Connecting to database...', ProgressBar);

        if not GetDbConnectionSync() then
        begin
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error has occured [GetDbConnectionSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error occured [GetDbConnectionSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.');
            ExitAppSync();
        end else
        begin
            MainAppForm.FDbConnect:=DbConnection;
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'Persistant connection with SQL database has been established.');
            ChangeProgressBar(33, 'Connecting to database... done.', ProgressBar);
        end;

        // ----------------------
        // Find the user account.
        // ----------------------

        Sleep(250);
        ChangeProgressBar(40, 'Getting user account details...', ProgressBar);

        if not GetUserAccountSync() then
        begin
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error has occured [GetUserAccountSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error occured [GetUserAccountSync]: ' + LastErrorMsg + '. Please contact your administrator. Application will be closed.');
            ExitAppSync();
        end else
        begin
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'User access has been established.');
            ChangeProgressBar(66, 'Getting user account details... done.', ProgressBar);
        end;

        // -------------------------------
        // Load all helper general tables.
        // -------------------------------

        Sleep(250);
        ChangeProgressBar(75, 'Loading general tables...', ProgressBar);

        if not GetGeneralTablesSync() then
        begin
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error occured [GetGeneralTablesSync]: ' + LastErrorMsg);
            THelpers.MsgCall(TAppMessage.Error, 'An error has occured [GetGeneralTablesSync]: ' + LastErrorMsg + '. Please contact IT support. Application will be closed.');
            ExitAppSync();
        end else
        begin
            MainAppForm.FAppEvents.Log(FCurrentSessionLog, 'General tables has been loaded.');
            ChangeProgressBar(90, 'Loading general tables... done.', ProgressBar);
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
        MainAppForm.DataUpdated.Caption:='';

        // ----------------------------------
        // Icon used in main aging view grid.
        // ----------------------------------

        MainAppForm.FGridPicture:=TImage.Create(MainForm);
        MainAppForm.FGridPicture.SetBounds(0, 0, 16, 16);
        THelpers.LoadImageFromStream(View.Main.MainForm.FGridPicture, Settings.GetPathGridImage);

        // ---------------------------------------------------------------------
        // Setup timers, some of them may be removed after introducing REST API.
        // ---------------------------------------------------------------------

        MainAppForm.InvoiceScanTimer.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'INVOICE_SCANNER', 900000{15 minutes});
        MainAppForm.FollowupPopup.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'FOLLOWUP_CHECKER', 1800000{30 minutes});
        MainAppForm.OILoader.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'OI_LOADER', 300000{5 minutes});

        // ---------------------------------------------
        // Setup risk classes with proper number format.
        // ---------------------------------------------

        if FormatSettings.DecimalSeparator = ',' then
        begin

            var getRiskClassA:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', TRiskClass.A);
            var getRiskClassB:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', TRiskClass.B);
            var getRiskClassC:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', TRiskClass.C);

            var RiskClassA:=((getRiskClassA).ToExtended * 100).ToString + '%';
            var RiskClassB:=((getRiskClassB).ToExtended * 100).ToString + '%';
            var RiskClassC:=((getRiskClassC).ToExtended * 100).ToString + '%';

            MainAppForm.procRISKA.Caption:=RiskClassA;
            MainAppForm.procRISKB.Caption:=RiskClassB;
            MainAppForm.procRISKC.Caption:=RiskClassC;

        end else if FormatSettings.DecimalSeparator = '.' then
        begin

            var getRiskClassA:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', TRiskClass.A);
            var getRiskClassB:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', TRiskClass.B);
            var getRiskClassC:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', TRiskClass.C);

            var RiskClassA:=((StringReplace(getRiskClassA, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassB:=((StringReplace(getRiskClassB, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassC:=((StringReplace(getRiskClassC, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';

            MainAppForm.procRISKA.Caption:=RiskClassA;
            MainAppForm.procRISKB.Caption:=RiskClassB;
            MainAppForm.procRISKC.Caption:=RiskClassC;

        end;

        // -----------------------------------
        // Tabsheet captions and aging ranges.
        // -----------------------------------

        MainAppForm.Cap01.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap02.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT02', 'EMPTY'), [fsBold]);
        MainAppForm.Cap03.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT03', 'EMPTY'), [fsBold]);
        MainAppForm.Cap05.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT05', 'EMPTY'), [fsBold]);
        MainAppForm.Cap06.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT06', 'EMPTY'), [fsBold]);
        MainAppForm.Cap07.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT07', 'EMPTY'), [fsBold]);
        MainAppForm.Cap24.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT08', 'EMPTY'), [fsBold]);
        MainAppForm.Cap10.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS2TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap11.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS2TXT02', 'EMPTY'), [fsBold]);
        MainAppForm.Cap12.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS2TXT03', 'EMPTY'), [fsBold]);
        MainAppForm.Cap13.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS3TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap43.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS4TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap61.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS6TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap15.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS7TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap21.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap22.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT02', 'EMPTY'), [fsBold]);
        MainAppForm.Cap23.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT03', 'EMPTY'), [fsBold]);
        MainAppForm.Cap27.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT04', 'EMPTY'), [fsBold]);
        MainAppForm.Cap62.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS9TXT01', 'EMPTY'), [fsBold]);
        MainAppForm.Cap63.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS9TXT02', 'EMPTY'), [fsBold]);

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

        MainAppForm.tR1.Caption:=getRange1A + ' - ' + getRange1B;
        MainAppForm.tR2.Caption:=getRange2A + ' - ' + getRange2B;
        MainAppForm.tR3.Caption:=getRange3A + ' - ' + getRange3B;
        MainAppForm.tR4.Caption:=getRange4A + ' - ' + getRange4B;
        MainAppForm.tR5.Caption:=getRange5A + ' - ' + getRange5B;
        MainAppForm.tR6.Caption:=getRange6A + ' - ' + getRange6B;

        MainAppForm.Text21.Caption:=getRange1A + ' - ' + getRange3B + ':';
        MainAppForm.Text22.Caption:=getRange4A + ' - ' + getRange6B + ':';

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


function TStartupForm.GetUserAccountSync(): boolean;
begin

    {TODO -oTomek -cReplaceWith : new account system}

    Result:=True;

    var UserControl: TUserControl:=TUserControl.Create(MainAppForm.FDbConnect);
    try

        try

            UserControl.UserName:=MainAppForm.WinUserName;
            MainAppForm.FAccessLevel:=UserControl.GetAccessData(TUserAccess.TTypes.AccessLevel);

            // Quit if username is not found
            if MainAppForm.FAccessLevel = '' then
            begin
                LastErrorMsg:='Cannot find account for user alias ' + UpperCase(MainAppForm.WinUserName);
                Result:=False;
            end else
            begin
                MainAppForm.FAccessMode:=UserControl.GetAccessData(TUserAccess.TTypes.AccessMode);
                if MainAppForm.FAccessMode = TUserAccess.AccessFull  then MainAppForm.Action_FullView.Checked:=True;
                if MainAppForm.FAccessMode = TUserAccess.AccessBasic then MainAppForm.Action_BasicView.Checked:=True;
                UserControl.GetGroupList(MainAppForm.FGroupList);
                UserControl.GetAgeDates(MainAppForm.FAgeDateList, MainAppForm.FGroupList[0, 0]);
            end;

        except
            on E: Exception do
            begin
                LastErrorMsg:=E.Message;
                Result:=False;
            end;

        end;

    finally
        UserControl.Free;
    end;

end;


function TStartupForm.GetGeneralTablesSync(): boolean;
begin

    Result:=True;

    var Utilities: IUtilities:=TUtilities.Create();
    try

        Utilities.GeneralTables(TSalesResponsible.SalesResponsible, MainAppForm.sgSalesResp);
        Utilities.GeneralTables(TPersonResponsible.PersonResponsible, MainAppForm.sgPersonResp);
        Utilities.GeneralTables(TAccountType.AccountType, MainAppForm.sgAccountType);
        Utilities.GeneralTables(TCustomerGroup.CustomerGroup, MainAppForm.sgCustomerGr);
        Utilities.GeneralTables(TGroup3.Group3, MainAppForm.sgGroup3);

        Utilities.GeneralTables(TCompanyData.CompanyData, MainAppForm.sgCoCodes, TCompanyData.CoCode + TChars.COMMA + TCompanyData.Branch + TChars.COMMA + TCompanyData.CoName + TChars.COMMA + TCompanyData.CoAddress + TChars.COMMA + TCompanyData.VatNo + TChars.COMMA + TCompanyData.Duns + TChars.COMMA + TCompanyData.Country + TChars.COMMA + TCompanyData.City + TChars.COMMA + TCompanyData.FinManager + TChars.COMMA + TCompanyData.TelephoneNumbers + TChars.COMMA + TCompanyData.CoType + TChars.COMMA + TCompanyData.CoCurrency + TChars.COMMA + TCompanyData.InterestRate + TChars.COMMA + TCompanyData.KpiOverdueTarget + TChars.COMMA + TCompanyData.KpiUnallocatedTarget + TChars.COMMA + TCompanyData.Agents + TChars.COMMA + TCompanyData.Divisions, TSql.ORDER + TCompanyData.CoCode + TSql.ASC);
        Utilities.GeneralTables(TPaymentTerms.PaymentTerms, MainAppForm.sgPmtTerms);
        Utilities.GeneralTables(TPaidinfo.Paidinfo, MainAppForm.sgPaidInfo);
        Utilities.GeneralTables(TPerson.Person, MainAppForm.sgPerson);
        Utilities.GeneralTables(TControlStatus.ControlStatus, MainAppForm.sgControlStatus);

    except
        on E: Exception do
        begin
            LastErrorMsg:=E.Message;
            Result:=False;
        end;

    end;

end;


// -------------------------------------------------------------------------------------------------------------------------------------------------- EVENTS //


procedure TStartupForm.LabelHideClick(Sender: TObject);
begin
    StartupForm.WindowState:=wsMinimized;
end;


end.

