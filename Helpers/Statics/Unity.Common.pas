unit Unity.Common;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TCommon = class abstract
    private
        class procedure GetBuildInfo(var V1, V2, V3, V4: word); static;
    public
        type  TInputMethod = reference to procedure;
        const SelectionColor: integer   = $00E3B268; // rgb D7E4F2 => bgr F2E4D7
        const FontColor:      integer   = $006433C9; // rgb C93364 => bgr 6433C9
        const AltColor:       integer   = $00FFDBB7; // rgb B7DBFF => bgr FFDBB7
        const DecryptKey:     integer   = 429496;
        const AppCaption:     string    = 'Unity';
        const UnityReader:    string    = 'UnityReader.exe';
        const LicenceFile:    string    = 'Unity.lic';
        const GridImgFile:    string    = 'Unity.img';
        const ReleaseFile:    string    = 'Release.zip';
        const LayoutPak:      string    = 'Layouts.zip';
        const ManifestFile:   string    = 'Unity.manifest';
        const CurrentMutex:   PWideChar = 'UNITY_10255';
        const ConfigFile:     string    = 'Config.cfg';
        class procedure ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod); static;
        class function GetBuildInfoAsString: string; static;
        class function GetOSVer(CheckForOsName: boolean): string; static;
        class function Unpack(ItemID: integer; FileName: string; ShouldFileStay: boolean; var LastErrorMsg: string): boolean; static;
        class function UnzippLayouts(FileName: string; DestDir: string): boolean; static;
    end;


implementation


uses
    Winapi.Windows,
    System.Zip,
    System.SysUtils,
    System.Classes;


class procedure TCommon.GetBuildInfo(var V1, V2, V3, V4: word);
begin

    var VerInfoSize:   DWORD;
    var VerValueSize:  DWORD;
    var Dummy:         DWORD;
    var VerInfo:       Pointer;
    var VerValue:      PVSFixedFileInfo;

    VerInfoSize:=GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);

    if VerInfoSize > 0 then begin
        GetMem(VerInfo, VerInfoSize);
        try
            if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
            begin
                VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
                with VerValue^ do
                begin
                    V1:=dwFileVersionMS shr 16;
                    V2:=dwFileVersionMS and $FFFF;
                    V3:=dwFileVersionLS shr 16;
                    V4:=dwFileVersionLS and $FFFF;
                end;
            end;
        finally
            FreeMem(VerInfo, VerInfoSize);
        end;
    end;

end;


class procedure TCommon.ExecWithDelay(Delay: integer; AnonymousMethod: TInputMethod);
begin

    TThread.CreateAnonymousThread(procedure
    begin

        Sleep(Delay);

        TThread.Synchronize(nil, procedure
        begin

            AnonymousMethod;

        end);

    end).Start;

end;


class function TCommon.GetBuildInfoAsString: string;
begin

    var V1: word;
    var V2: word;
    var V3: word;
    var V4: word;

    GetBuildInfo(V1, V2, V3, V4);
    Result:=IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);

end;


class function TCommon.GetOSVer(CheckForOsName: boolean): string;
begin

    result:='0';

    if not CheckForOsName then
    begin
        case Win32MajorVersion of
        4:
            case Win32MinorVersion of
                0:  result:='40';     { Windows 95    }
                10: result:='41';     { Windows 98    }
                90: result:='49';     { Windows ME    }
            end;
        5:
            case Win32MinorVersion of
                0: result:='50';      { Windows 2000  }
                1: result:='51';      { Windows XP    }
            end;
        6:
            case Win32MinorVersion of
                0: result:='60';      { Windows Vista }
                1: result:='61';      { Windows 7     }
                2: result:='62';      { Windows 8     }
                3: result:='63';      { Windows 8.1   }
            end;
        10:
            case Win32MinorVersion of
                0: result:='100';     { Windows 10    }
            end;
        end;
    end;

    if CheckForOsName then
    begin
        case Win32MajorVersion of
        4:
            case Win32MinorVersion of
                0:  result:='Windows 95';
                10: result:='Windows 98';
                90: result:='Windows ME';
            end;
        5:
            case Win32MinorVersion of
                0: result:='Windows 2000';
                1: result:='Windows XP';
            end;
        6:
            case Win32MinorVersion of
                0: result:='Windows Vista';
                1: result:='Windows 7';
                2: result:='Windows 8';
                3: result:='Windows 8.1';
            end;
        10:
            case Win32MinorVersion of
                0: result:='Windows 10';
            end;
        end;
    end;

end;


/// <summary>
/// Extract given source file by provided ID number.
/// </summary>
/// <param name="ItemID">Resource ID number (integer)</param>
/// <param name="FileName">Resource filename (string)</param>
/// <param name="ShouldStay">Delete before deploy new (boolean)</param>
/// <returns>True if succeed (boolean)</returns>
/// <remarks>
/// 10 RCDATA "Makefile\\config.cfg" default setting file.
/// </remarks>

class function TCommon.Unpack(ItemID: integer; FileName: string; ShouldFileStay: boolean; var LastErrorMsg: string): boolean;
begin

    Result:=False;

    var RS: TResourceStream:=TResourceStream.CreateFromID(hInstance, ItemID, RT_RCDATA);
    try

        RS.Position:=0;
        if not ShouldFileStay then
            DeleteFile(PChar(FileName));

        try
            RS.SaveToFile(FileName);

        except
            on E: Exception do
            begin
                LastErrorMsg:='Cannot extract file from resource container. Exception has been thrown: ' + E.Message;
                Exit;
            end;

        end;

        Result:=True;

    finally
        RS.free;
    end;

end;


class function TCommon.UnzippLayouts(FileName: string; DestDir: string): boolean;
begin

    Result:=False;

    var ZipRead: TZipFile:=TZipFile.Create;
    try

        try
            ZipRead.Open(FileName, zmRead);

            for var iCNT: integer:=0 to ZipRead.FileCount - 1 do
            begin

                var Zipped:   string:=ZipRead.FileName[iCNT];
                var FullPath: string:=DestDir + Zipped;

                // Extract and create any folder if missing
                ZipRead.Extract(iCNT, DestDir, True);
                //LogText.Log(EventLogPath, 'Extracting: ' + Zipped + '.');

            end;

            Result:=True;

        except
            on E: Exception do
                //LogText.Log(EventLogPath, 'Unexpected error has been thrown: ' + E.Message);
        end;

    finally
        ZipRead.Free;
        DeleteFile(PChar(FileName));
    end;

end;


end.

