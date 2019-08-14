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
    Unity.EventLogger;


type


    TStartupForm = class(TForm)
        MainText2: TLabel;
        ProgressBar: TGauge;
        TextFooterA: TLabel;
        TextFooter2B: TLabel;
        MainText1: TLabel;
        TextStatus: TLabel;
        SubText: TLabel;
        ShapeProgressBar: TShape;
        ShapeBackground: TShape;
        CentreText: TLabel;
        Flutter: TImage;
        FlutterText: TLabel;
        ShapeHide: TShape;
        LabelHide: TLabel;
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
        type ExecResult = record
            LastError: string;

        end;
    private
        var FIsAppInitialized: boolean;
        var FCurrentSessionLog: string;
        procedure AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);
        procedure ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);
        function GetScreenDataSync(): string;
        function GetDbConnectionSync(): string;
        function GetUserAccountSync(): string;
        function GetGeneralTablesSync(): string;
        function GetFinalizationSync(): string;
    public
        property IsAppInitialized: boolean read FIsAppInitialized;
        procedure ApplicationStart();
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
    Handler.Account,
    View.Main,
    DbModel;


var
    VStartupForm: TStartupForm;


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
    TextStatus.Caption:='Loading, please wait...';
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
    ShapeHide.Brush.Color:=$006433C9;
    LabelHide.Font.Color:=$FFFFFF;
end;


procedure TStartupForm.LabelHideMouseLeave(Sender: TObject);
begin
    ShapeHide.Brush.Color:=$FFFFFF;
    LabelHide.Font.Color:=$006433C9;
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
end;


procedure TStartupForm.SetSessionLog(SessionEventLog: string);
begin
    FCurrentSessionLog:=SessionEventLog;
end;


/// <summary>
/// Load settings and update MainForm properties and variables before it gets shown to the user.
/// Initialize SQL database with persistent connection (this will be replaced by the REST service).
/// </summary>

procedure TStartupForm.ApplicationStart();
begin

    if FIsAppInitialized then Exit();
    if not String.IsNullOrEmpty(FCurrentSessionLog) then
        View.Main.MainForm.InitMainWnd(FCurrentSessionLog);




    FIsAppInitialized:=True;
    AnimateWindow(StartupForm.Handle, 500, AW_BLEND or AW_HIDE);
    MainForm.Show();

end;


function TStartupForm.GetScreenDataSync(): string;
begin

    Result:='';

    var Settings: ISettings:=TSettings.Create();
    try

        // ---------------------
        // Application captions.
        // ---------------------

        View.Main.MainForm.Caption:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'WND_MAIN', TCommon.APPCAPTION);
        View.Main.MainForm.DataUpdated.Caption:='';

        // ----------------------------------
        // Icon used in main aging view grid.
        // ----------------------------------

        View.Main.MainForm.FGridPicture:=TImage.Create(MainForm);
        View.Main.MainForm.FGridPicture.SetBounds(0, 0, 16, 16);
        THelpers.LoadImageFromStream(View.Main.MainForm.FGridPicture, Settings.GetPathGridImage);

        // ---------------------------------------------------------------------
        // Setup timers, some of them may be removed after introducing REST API.
        // ---------------------------------------------------------------------

        View.Main.MainForm.InvoiceScanTimer.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'INVOICE_SCANNER', 900000{15 minutes});
        View.Main.MainForm.FollowupPopup.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'FOLLOWUP_CHECKER', 1800000{30 minutes});
        View.Main.MainForm.OILoader.Interval:=Settings.GetIntegerValue(TConfigSections.TimersSettings, 'OI_LOADER', 300000{5 minutes});

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

            View.Main.MainForm.procRISKA.Caption:=RiskClassA;
            View.Main.MainForm.procRISKB.Caption:=RiskClassB;
            View.Main.MainForm.procRISKC.Caption:=RiskClassC;

        end else if FormatSettings.DecimalSeparator = '.' then
        begin

            var getRiskClassA:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_A_MAX', TRiskClass.A);
            var getRiskClassB:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_B_MAX', TRiskClass.B);
            var getRiskClassC:=Settings.GetStringValue(TConfigSections.RiskClassDetails, 'CLASS_C_MAX', TRiskClass.C);

            var RiskClassA:=((StringReplace(getRiskClassA, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassB:=((StringReplace(getRiskClassB, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';
            var RiskClassC:=((StringReplace(getRiskClassC, ',', '.', [rfReplaceAll])).ToExtended * 100).ToString + '%';

            View.Main.MainForm.procRISKA.Caption:=RiskClassA;
            View.Main.MainForm.procRISKB.Caption:=RiskClassB;
            View.Main.MainForm.procRISKC.Caption:=RiskClassC;

        end;

        // -----------------------------------
        // Tabsheet captions and aging ranges.
        // -----------------------------------

        View.Main.MainForm.Cap01.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap02.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT02', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap03.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT03', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap05.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT05', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap06.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT06', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap07.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT07', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap24.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS1TXT08', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap10.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS2TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap11.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS2TXT02', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap12.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS2TXT03', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap13.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS3TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap43.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS4TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap61.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS6TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap15.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS7TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap21.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap22.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT02', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap23.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT03', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap27.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS8TXT04', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap62.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS9TXT01', 'EMPTY'), [fsBold]);
        View.Main.MainForm.Cap63.ShapeText(10, 1, Settings.GetStringValue(TConfigSections.TabSheetsCaps, 'TS9TXT02', 'EMPTY'), [fsBold]);

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

        View.Main.MainForm.tR1.Caption:=getRange1A + ' - ' + getRange1B;
        View.Main.MainForm.tR2.Caption:=getRange2A + ' - ' + getRange2B;
        View.Main.MainForm.tR3.Caption:=getRange3A + ' - ' + getRange3B;
        View.Main.MainForm.tR4.Caption:=getRange4A + ' - ' + getRange4B;
        View.Main.MainForm.tR5.Caption:=getRange5A + ' - ' + getRange5B;
        View.Main.MainForm.tR6.Caption:=getRange6A + ' - ' + getRange6B;

        View.Main.MainForm.Text21.Caption:=getRange1A + ' - ' + getRange3B + ':';
        View.Main.MainForm.Text22.Caption:=getRange4A + ' - ' + getRange6B + ':';

//        View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Unity has been boot up...');
//        View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Initialization...');
//        ChangeProgressBar(10, 'Initialization...', ProgressBar);

    except
        on E: Exception do Result:=E.Message;
//        begin
//            View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error has occured [ApplicationStart.Captions]: ' + E.Message);
//            THelpers.MsgCall(TAppMessage.Error, 'An error has occured [ApplicationStart.Captions]: ' + E.Message + '. Please contact IT support. Application will be closed.');
//            ExitProcess(0);
//        end;

    end;

end;


function TStartupForm.GetDbConnectionSync(): string;
begin

    Result:='';

    try

        View.Main.MainForm.TryInitConnection;
        //View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Persistant connection with SQL database has been established...');
        //ChangeProgressBar(25, 'Database connectivity...', ProgressBar);

    except
        on E: Exception do Result:=E.Message;
//        begin
//            View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error has occured [ApplicationStart.TryInitConnection]: ' + E.Message);
//            THelpers.MsgCall(TAppMessage.Error, 'An error occured [ApplicationStart.TryInitConnection]: ' + E.Message + '. Please contact IT support. Application will be closed.');
//            ExitProcess(0);
//        end;

    end;

end;


function TStartupForm.GetUserAccountSync(): string;
begin

    Result:='';

    var UserControl: TUserControl:=TUserControl.Create(View.Main.MainForm.FDbConnect);
    try

        try

            UserControl.UserName:=View.Main.MainForm.WinUserName;
            View.Main.MainForm.FAccessLevel:=UserControl.GetAccessData(TUserAccess.TTypes.AccessLevel);

            // Quit if username is not found
            if View.Main.MainForm.FAccessLevel = '' then
            begin
//                THelpers.MsgCall(TAppMessage.Error, 'Cannot find account for user alias: ' + UpperCase(View.Main.MainForm.WinUserName) + '. Please contact your administrator. Application will be closed.');
//                ExitProcess(0);
            end;

            View.Main.MainForm.FAccessMode:=UserControl.GetAccessData(TUserAccess.TTypes.AccessMode);
            if View.Main.MainForm.FAccessMode = TUserAccess.AccessFull  then View.Main.MainForm.Action_FullView.Checked:=True;
            if View.Main.MainForm.FAccessMode = TUserAccess.AccessBasic then View.Main.MainForm.Action_BasicView.Checked:=True;

            UserControl.GetFGroupList(View.Main.MainForm.FGroupList, View.Main.MainForm.GroupListBox);
            UserControl.GetAgeDates(View.Main.MainForm.GroupListDates, View.Main.MainForm.FGroupList[0, 0]);

            {TODO -oTomek -cReplaceWith : ApprovalMatrix}

            // Restricted for "ADMINS"
            if View.Main.MainForm.FAccessLevel <> TUserAccess.Admin then
            begin
                View.Main.MainForm.sgCompanyData.Enabled:=False;
                View.Main.MainForm.ReloadCover.Visible:=True;
                View.Main.MainForm.ReloadCover.Cursor:=crNo;
                View.Main.MainForm.GroupListDates.Enabled:=False;
            end;

        except
            on E: Exception do Result:=E.Message;
//            begin
//                View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error occured [ApplicationStart.TUserControl]: ' + E.Message);
//                THelpers.MsgCall(TAppMessage.Error, 'An error occured [ApplicationStart.TUserControl][' + View.Main.MainForm.WinUserName + ']: ' + E.Message + '. Please contact IT support. Application will be closed.');
//                ExitProcess(0);
//            end;

        end;

//        View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'User access has been established...');
//        ChangeProgressBar(33, 'User access has been established...', ProgressBar);

    finally
        UserControl.Free;
    end;

end;


function TStartupForm.GetGeneralTablesSync(): string;
begin

    Result:='';

    var Utilities: IUtilities:=TUtilities.Create();
    try

        Utilities.GeneralTables(TSalesResponsible.SalesResponsible, View.Main.MainForm.sgSalesResp);
        Utilities.GeneralTables(TPersonResponsible.PersonResponsible, View.Main.MainForm.sgPersonResp);
        Utilities.GeneralTables(TAccountType.AccountType, View.Main.MainForm.sgAccountType);
        Utilities.GeneralTables(TCustomerGroup.CustomerGroup, View.Main.MainForm.sgCustomerGr);
        Utilities.GeneralTables(TGroup3.Group3, View.Main.MainForm.sgGroup3);

        Utilities.GeneralTables(TCompanyData.CompanyData, View.Main.MainForm.sgCoCodes, TCompanyData.CoCode + TChars.COMMA + TCompanyData.Branch + TChars.COMMA + TCompanyData.CoName + TChars.COMMA + TCompanyData.CoAddress + TChars.COMMA + TCompanyData.VatNo + TChars.COMMA + TCompanyData.Duns + TChars.COMMA + TCompanyData.Country + TChars.COMMA + TCompanyData.City + TChars.COMMA + TCompanyData.FinManager + TChars.COMMA + TCompanyData.TelephoneNumbers + TChars.COMMA + TCompanyData.CoType + TChars.COMMA + TCompanyData.CoCurrency + TChars.COMMA + TCompanyData.InterestRate + TChars.COMMA + TCompanyData.KpiOverdueTarget + TChars.COMMA + TCompanyData.KpiUnallocatedTarget + TChars.COMMA + TCompanyData.Agents + TChars.COMMA + TCompanyData.Divisions, TSql.ORDER + TCompanyData.CoCode + TSql.ASC);
        Utilities.GeneralTables(TPaymentTerms.PaymentTerms, View.Main.MainForm.sgPmtTerms);
        Utilities.GeneralTables(TPaidinfo.Paidinfo, View.Main.MainForm.sgPaidInfo);
        Utilities.GeneralTables(TPerson.Person, View.Main.MainForm.sgPerson);
        Utilities.GeneralTables(TControlStatus.ControlStatus, View.Main.MainForm.sgControlStatus);

        //View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'General tables has been loaded...');
        //ChangeProgressBar(75, 'General tables has been loaded...', ProgressBar);

    except
        on E: Exception do Result:=E.Message;
//        begin
//            View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error occured [ApplicationStart.GeneralTables]: ' + E.Message);
//            THelpers.MsgCall(TAppMessage.Error, 'An error has occured [ApplicationStart.GeneralTables]: ' + E.Message + '. Please contact IT support. Application will be closed.');
//            ExitProcess(0);
//        end;

    end;

end;


function TStartupForm.GetFinalizationSync(): string;
begin

    Result:='';

    try

        var NowTime: TTime:=Now;
        View.Main.MainForm.FStartTime:=Now;

        FormatDateTime('hh:mm:ss', NowTime);
        FormatDateTime('hh:mm:ss', View.Main.MainForm.FStartTime);

        View.Main.MainForm.StatBar_TXT3.Caption:=DateToStr(Now);
        View.Main.MainForm.UpTime.Enabled:=True;
        View.Main.MainForm.CurrentTime.Enabled:=True;

        View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Main thread [' + MainThreadID.ToString + ']: Application version = ' + TCommon.GetBuildInfoAsString);
        View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Main thread [' + MainThreadID.ToString + ']: User SID = ' + TUserSid.GetCurrentUserSid);

        //if not(View.Main.MainForm.FirstAgeLoad.Enabled) then View.Main.MainForm.FirstAgeLoad.Enabled:=True;

        //var Queries: IQueries:=TQueries.Create;
        //Queries.InitializeQms;

        //View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Finalization... all has been started.');
        //ChangeProgressBar(100, 'Finalization... all has been started.', ProgressBar);

    except
		on E: Exception do Result:=E.Message;
//        begin
//            View.Main.MainForm.FAppEvents.Log(FCurrentSessionLog, 'Critical error occured [ApplicationStart.Finalization]: ' + E.Message);
//            THelpers.MsgCall(TAppMessage.Error, 'An error occured [ApplicationStart.Finalization]: ' + E.Message + '. Please contact IT support. Application will be closed.');
//            ExitProcess(0);
//        end;

    end;

end;


// -------------------------------------------------------------------------------------------------------------------------------------------------- EVENTS //


procedure TStartupForm.LabelHideClick(Sender: TObject);
begin
    StartupForm.WindowState:=wsMinimized;
end;


end.

