unit View.Luncher;


interface


uses
    System.Classes,
    System.INIFiles,
    System.Zip,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.Samples.Gauges,
    Api.ReturnClientInfo;


type


    TCallResponse = record
        LastMessage:  string;
        ErrorCode:    string;
        IsSucceeded:  boolean;
        ReturnedCode: integer;
    end;

    TGetClientInfo = procedure(PayLoad: TReturnClientInfo) of object;

    TLuncherForm = class(TForm)
        ShapeBackground: TShape;
        MainText: TLabel;
        TextSubtitle: TLabel;
        ShapeFooter: TShape;
        TextFooterA: TLabel;
        TextFooterB: TLabel;
        ProgressBar: TGauge;
        TextStatus: TLabel;
        MainTextA: TLabel;
        ShapeLine: TShape;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure ShapeBackgroundMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    protected
        procedure CreateParams(var Params: TCreateParams); override;
    strict private
        var FAppEnviron:  TMemIniFile;
        var FUpdateInfo:  TMemIniFile;
        var FProgramData: string;
        var FDataFolder:  string;
        var FBinSource:   string;
        var FCfgSource:   string;
        var FSetup:       string;
        var FUpdateUri:   string;
        var FExpandFile:  string;
        var FLastUpdate:  string;
        var FEndpoint:    string;
        procedure AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);
        procedure ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);
        function GetFileFromUrl(const AUri: string; const AFileName: string): integer;
        procedure LunchAndCose(const AProgramFile: string; const Delay: integer);
        procedure GetClientInfoAsync(Callback: TGetClientInfo);
        procedure GetClientInfo_Callback(PayLoad: TReturnClientInfo);
        procedure ExtractToFile(const AZipFileName: string; const AZippedFileIndex: Integer; const AExtractedFileName: string);
        procedure UpdateCheck();
    public
        property ProgramData: string read FProgramData;
        property DataFolder:  string read FDataFolder;
        property BinSource:   string read FBinSource;
        property CfgSource:   string read FCfgSource;
        property Setup:       string read FSetup;
        property UpdateUri:   string read FUpdateUri;
        property ExpandFile:  string read FExpandFile;
        property LastUpdate:  string read FLastUpdate;
        property Endpoint:    string read FEndpoint;
    end;


    function LuncherForm(): TLuncherForm;


implementation


{$R *.dfm}


uses
    System.Variants,
    System.SysUtils,
    System.Threading,
    System.IOUtils,
    Winapi.Windows,
    Winapi.Messages,
    WinApi.UrlMon,
    Winapi.ShellAPI,
    REST.Types,
    REST.Json,
    Unity.Enums,
    Unity.Helpers,
    Unity.RestWrapper,
    Api.ErrorHandler;


var
    VLuncherForm: TLuncherForm;


function LuncherForm(): TLuncherForm;
begin
    if not(Assigned(VLuncherForm)) then Application.CreateForm(TLuncherForm, VLuncherForm);
    Result:=VLuncherForm;
end;


procedure TLuncherForm.CreateParams(var Params: TCreateParams);
begin

    inherited CreateParams(Params);

    with Params do begin
        Style:=WS_POPUP;
        WindowClass.style:=WindowClass.style or CS_DROPSHADOW;
    end;

end;


procedure TLuncherForm.ShapeBackgroundMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

    const SC_DRAGMOVE = $F012;

    if Button = mbLeft then
    begin
        ReleaseCapture;
        Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
    end;

end;


procedure TLuncherForm.AnimateProgressBar(AniFrom: integer; AniTo: integer; ProgressBar: TGauge; Speed: cardinal = 5);//move to lib
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


procedure TLuncherForm.ChangeProgressBar(ProgressTarget: integer; Text: string; var ProgressBar: TGauge);//move to lib
begin
    AnimateProgressBar(ProgressBar.Progress, ProgressTarget, ProgressBar);
    TextStatus.Caption:=Text;
    Update();
end;


function TLuncherForm.GetFileFromUrl(const AUri: string; const AFileName: string): integer;
begin

    Result:=URLDownloadToFile(
        nil,
        PChar(AUri),
        PChar(AFileName),
        0,
        nil
    );

end;


procedure TLuncherForm.LunchAndCose(const AProgramFile: string; const Delay: integer);
begin

    THelpers.ExecWithDelay(Delay, procedure
    begin

        var LParams:='-set {0} {1} {2}';
        LParams:=LParams.Replace('{0}', FBinSource).Replace('{1}', FCfgSource).Replace('{2}', FSetup);

        var ErrorCode:=ShellExecute(LuncherForm.Handle, 'open', PChar(AProgramFile), PChar(LParams), nil, SW_SHOWNORMAL);

        if ErrorCode < 32 then
            THelpers.MsgCall(LuncherForm.Handle, TAppMessage.Error, 'Cannot execute, error code: ' + ErrorCode.ToString() + '.');

        ExitProcess(0);

    end);

end;


procedure TLuncherForm.ExtractToFile(const AZipFileName: string; const AZippedFileIndex: Integer; const AExtractedFileName: string);
begin

    var LDownloadedStream:=TFileStream.Create(AZipFileName, fmOpenRead);
    try

        var LZipFile:=TZipFile.Create();
        try

            var LDecompressionStream: TStream;
            var LLocalHeader: TZipHeader;

            LZipFile.Open(LDownloadedStream, zmRead);
            LZipFile.Read(AZippedFileIndex, LDecompressionStream, LLocalHeader);
            try

                var LOutputStream:=TFileStream.Create(AExtractedFileName, fmCreate);
                try
                    LOutputStream.CopyFrom(LDecompressionStream, LDecompressionStream.Size);
                finally
                    LOutputStream.Free();
                end;

            finally
                LDecompressionStream.Free();
            end;

        finally
            LZipFile.Free();
        end;

    finally
        LDownloadedStream.Free();
    end;

end;


procedure TLuncherForm.GetClientInfoAsync(Callback: TGetClientInfo);
begin

    var NewTask: ITask:=TTask.Create(procedure
    begin

        var Rest: IRESTFul:=TRESTful.Create();
		Rest.AccessToken:='';
        Rest.SelectContentType(TRESTContentType.ctAPPLICATION_JSON);

        Rest.ClientBaseURL:=FEndpoint;
        Rest.RequestMethod:=TRESTRequestMethod.rmGET;

        var LReturnClientInfo: TReturnClientInfo;
        try

            if (Rest.Execute) and (Rest.StatusCode = 200) then
            begin
                LReturnClientInfo:=TJson.JsonToObject<TReturnClientInfo>(Rest.Content);
            end
            else
            begin

                if not Assigned(LReturnClientInfo) then
                begin
                    LReturnClientInfo:=TReturnClientInfo.Create();
                    LReturnClientInfo.Error:=TErrorHandler.Create();
                end;

                if not String.IsNullOrEmpty(Rest.ExecuteError) then
                    LReturnClientInfo.Error.ErrorDesc:='[CheckReleaseAwaited]: Critical error. Please contact IT Support. Description: ' + Rest.ExecuteError
                else
                    if String.IsNullOrEmpty(Rest.Content) then
                        LReturnClientInfo.Error.ErrorDesc:='[CheckReleaseAwaited]: Invalid server response. Please contact IT Support.'
                    else
                        LReturnClientInfo.Error.ErrorDesc:='[CheckReleaseAwaited]: An error has occured. Please contact IT Support. Description: ' + Rest.Content;

                LReturnClientInfo.IsSucceeded:=False;

            end;

        except
            on E: Exception do
            begin

                if not Assigned(LReturnClientInfo) then
                begin
                    LReturnClientInfo:=TReturnClientInfo.Create();
                    LReturnClientInfo.Error:=TErrorHandler.Create();
                end;

                LReturnClientInfo.IsSucceeded:=False;
                LReturnClientInfo.Error.ErrorDesc:='[CheckReleaseAwaited]: Cannot execute. Error has been thrown: ' + E.Message;
            end;

        end;

        TThread.Synchronize(nil, procedure
        begin
            if Assigned(Callback) then Callback(LReturnClientInfo);
            if Assigned(LReturnClientInfo) then LReturnClientInfo.Free();
        end);

    end);

    NewTask.Start();

end;


procedure TLuncherForm.GetClientInfo_Callback(PayLoad: TReturnClientInfo);
begin

    if PayLoad.IsSucceeded then
    begin

        var LProgramFile:=FProgramData + 'Unity.exe';

        if (PayLoad.Status <> 'Production') and (PayLoad.Version.ToInteger() > FLastUpdate.ToInteger()) then
        begin

            const LUrlSource  = FUpdateUri + FExpandFile;
            const LExpandFile = FProgramData + FExpandFile;

            var ReturnCode:=GetFileFromUrl(LUrlSource, LExpandFile);
            if ReturnCode <> 0 then
            begin
                THelpers.MsgCall(LuncherForm.Handle, TAppMessage.Error, 'Error has been thrown, code: ' + ReturnCode.ToString() + '. Please contact IT support.');
                ExitProcess(0);
            end;

            ChangeProgressBar(66, 'Unpacking...', ProgressBar);
            DeleteFile(PChar(LProgramFile));
            ExtractToFile(LExpandFile, 0, LProgramFile);
            DeleteFile(PChar(LExpandFile));

            ChangeProgressBar(90, 'Saving...', ProgressBar);
            FUpdateInfo.WriteString('Version', 'Latest', PayLoad.Version);
            FUpdateInfo.UpdateFile();

        end;

        ChangeProgressBar(100, 'Calling Unity Platform...', ProgressBar);
        LunchAndCose(LProgramFile, 2500);

    end
    else
    begin
        THelpers.MsgCall(LuncherForm.Handle, TAppMessage.Error, PayLoad.Error.ErrorDesc);
    end;

end;


procedure TLuncherForm.UpdateCheck();
begin

    var LProgramFile:=FProgramData + 'Unity.exe';

    if not FileExists(LProgramFile) then
    begin

        ChangeProgressBar(33, 'Downloading package...', ProgressBar);

        const LUrlSource  = FUpdateUri + FExpandFile;
        const LExpandFile = FProgramData + FExpandFile;

        var ReturnCode:=GetFileFromUrl(LUrlSource, LExpandFile);
        if ReturnCode <> 0 then
        begin
            THelpers.MsgCall(LuncherForm.Handle, TAppMessage.Error, 'Error has been thrown, code: ' + ReturnCode.ToString() + '. Please contact IT support.');
            ExitProcess(0);
        end;

        ChangeProgressBar(66, 'Unpacking...', ProgressBar);
        ExtractToFile(LExpandFile, 0, LProgramFile);
        DeleteFile(PChar(LExpandFile));

        ChangeProgressBar(90, 'Calling Unity Platform...', ProgressBar);
        LunchAndCose(LProgramFile, 2500);

    end
    else
    begin
        ChangeProgressBar(33, 'Checking for updates...', ProgressBar);
        GetClientInfoAsync(GetClientInfo_Callback);
    end;

end;


procedure TLuncherForm.FormCreate(Sender: TObject);
begin

    ProgressBar.Progress:=0;
    TextStatus.Caption:='';

    var InfFileName:=ExtractFileDir(Application.ExeName) + TPath.DirectorySeparatorChar + 'Luncher.inf';
    if not Assigned(FAppEnviron) then FAppEnviron:=TMemIniFile.Create(InfFileName);

    FDataFolder:=FAppEnviron.ReadString('Environment', 'DataFolder', '');
    FBinSource :=FAppEnviron.ReadString('Environment', 'BinSource', '');
    FCfgSource :=FAppEnviron.ReadString('Environment', 'CfgSource', '');
    FSetup     :=FAppEnviron.ReadString('Environment', 'Setup', '');
    FUpdateUri :=FAppEnviron.ReadString('Environment', 'UpdateUri', '');
    FExpandFile:=FAppEnviron.ReadString('Environment', 'ExpandFile', '');
    FEndpoint  :=FAppEnviron.ReadString('Environment', 'Endpoint', '');

    FProgramData:=TPath.GetPublicPath()
        + TPath.DirectorySeparatorChar
        + FDataFolder
        + TPath.DirectorySeparatorChar;

    var UpdateInfo:=FProgramData + 'Update.ini';
    if not Assigned(FUpdateInfo) then
    begin

        if FileExists(UpdateInfo) then
        begin
            FUpdateInfo:=TMemIniFile.Create(UpdateInfo);
            FLastUpdate:=FUpdateInfo.ReadString('Version', 'Latest', '');
        end
        else
        begin
            FLastUpdate:='0';
            FUpdateInfo:=TMemIniFile.Create(UpdateInfo);
            FUpdateInfo.WriteString('Version', 'Latest', '0');
            FUpdateInfo.UpdateFile();
        end;

    end;

end;


procedure TLuncherForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FAppEnviron) then FAppEnviron.Free();
    if Assigned(FUpdateInfo) then FUpdateInfo.Free();
end;


procedure TLuncherForm.FormActivate(Sender: TObject);
begin
    ChangeProgressBar(15, 'Initilizing...', ProgressBar);
    THelpers.ExecWithDelay(1500, UpdateCheck);
end;


end.

