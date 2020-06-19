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
    Vcl.Samples.Gauges;


type


    TCallResponse = record
        LastMessage:  string;
        ErrorCode:    string;
        IsSucceeded:  boolean;
        ReturnedCode: integer;
    end;

    TCheckForUpdates = procedure(CallResponse: TCallResponse) of object;

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
        MainTextB: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure ShapeBackgroundMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    protected
        procedure CreateParams(var Params: TCreateParams); override;
    strict private
        var FAppEnviron:  TMemIniFile;
        var FProgramData: string;
        var FDataFolder:  string;
        var FBinSource:   string;
        var FCfgSource:   string;
        var FSetup:       string;
        var FUpdateUri:   string;
        var FExpandFile:  string;
        function GetFileFromUrl(const AUri: string; const AFileName: string): integer;
        procedure CheckForUpdatesAsync(ACallback: TCheckForUpdates);
        procedure CheckForUpdates_Callback(ACallResponse: TCallResponse);
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
    REST.Json;


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


procedure TLuncherForm.CheckForUpdatesAsync(ACallback: TCheckForUpdates);
begin

// perform async REST call

end;


procedure TLuncherForm.CheckForUpdates_Callback(ACallResponse: TCallResponse);
begin

//    - if should update:
//      - download Unity.zip
//      - unpack to Unity.exe
//      - delete Unity.zip
//      - lunch Unity.exe
//      - close Luncher.exe
//    - if should not update, then:
//      - lunch Unity.exe
//      - close Luncher.exe

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


procedure TLuncherForm.UpdateCheck();
begin

    var LProgramFile:=FProgramData + 'Unity.exe';

    if not FileExists(LProgramFile) then
    begin

        TextStatus.Caption:='Downloading package...';

        var LUrlSource :=FUpdateUri + FExpandFile;
        var LExpandFile:=FProgramData + FExpandFile;

        var Code:=GetFileFromUrl(LUrlSource, LExpandFile);

        if Code <> 0 then
        begin

            TextStatus.Caption:='Error: ' + Code.ToString() + ', operation cancelled.';
            Application.MessageBox(
                PCHar('Error has been thrown, code: ' + Code.ToString() + '. Please contact IT support.'),
                PChar('Luncher'), MB_OK + MB_ICONERROR
            );
            ExitProcess(0);

        end;

        TextStatus.Caption:='Unpacking...';

        const Params = ' -set {0} {1} {2}';
        var ExecPath:=LProgramFile + Params.Replace('{0}', FBinSource).Replace('{1}', FCfgSource).Replace('{2}', FSetup);

        ExtractToFile(LExpandFile, 0, LProgramFile);
        DeleteFile(PChar(LExpandFile));

        TextStatus.Caption:='Calling Unity Platform...';
        Sleep(2500);
        ShellExecute(LuncherForm.Handle, 'open', PChar(ExecPath), nil, nil, SW_SHOWNORMAL);
        ExitProcess(0);

    end
    else
    begin
        TextStatus.Caption:='Checking for updates...';
        CheckForUpdatesAsync(CheckForUpdates_Callback);
    end;

end;


procedure TLuncherForm.FormCreate(Sender: TObject);
begin

    ProgressBar.Progress:=0;
    TextStatus.Caption:='Initilizing...';

    var InfFileName:=ExtractFileDir(Application.ExeName) + TPath.DirectorySeparatorChar + 'Luncher.inf';
    if not Assigned(FAppEnviron) then FAppEnviron:=TMemIniFile.Create(InfFileName);

    FDataFolder:=FAppEnviron.ReadString('Environment', 'DataFolder', '');
    FBinSource :=FAppEnviron.ReadString('Environment', 'BinSource', '');
    FCfgSource :=FAppEnviron.ReadString('Environment', 'CfgSource', '');
    FSetup     :=FAppEnviron.ReadString('Environment', 'Setup', '');
    FUpdateUri :=FAppEnviron.ReadString('Environment', 'UpdateUri', '');
    FExpandFile:=FAppEnviron.ReadString('Environment', 'ExpandFile', '');

    FProgramData:=TPath.GetPublicPath()
        + TPath.DirectorySeparatorChar
        + FDataFolder
        + TPath.DirectorySeparatorChar;

end;


procedure TLuncherForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FAppEnviron) then FAppEnviron.Free();
end;


procedure TLuncherForm.FormActivate(Sender: TObject);
begin
    UpdateCheck();
end;


end.
