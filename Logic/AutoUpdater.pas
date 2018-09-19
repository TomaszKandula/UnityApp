
{$I .\Include\Header.inc}

unit AutoUpdater;

interface

uses
    Main, Classes;

    /// <summary>
    ///
    /// </summary>

type

    TUpdater = class
    {$TYPEINFO ON}
    public
        procedure Execute;
    end;


implementation


/// <summary>
///
/// </summary>

procedure TUpdater.Execute;
begin



(*
    Manifest:=Connection.GetResponseText(Settings.GetReleaseManURL);
    if ( GetManifestValue('Status', Manifest) = 'update' ) and ( GetManifestValue('Release', Manifest) > IntToStr(ReleaseNumber) ) then
    begin

        // Update screen
        UpdateForm:=TUpdateForm.Create(nil);
        SystemParametersInfo(SPI_GETWORKAREA, 0, @WndRect, 0);
        UpdateForm.Top :=((WndRect.Bottom - WndRect.Top ) div 2) - (UpdateForm.Height div 2);
        UpdateForm.Left:=((WndRect.Right  - WndRect.Left) div 2) - (UpdateForm.Width  div 2);
        AnimateWindow(UpdateForm.Handle, 500, AW_BLEND or AW_ACTIVATE);
        UpdateForm.Update;

        // Update message for the user
        UpdateForm.txtProgress.Caption:='Downloading...';
        UpdateForm.Update;

        // Get package from website
        if Connection.Download(PathReleasePak, Settings.GetPackageDir + ReleaseFile) then
        begin
            // Unzip the content, update settings file and execute new release
            if UnzippReleaseFile(Settings.GetPackageDir + ReleaseFile, PathAppDir, PathEventLog) then
            begin
                Settings.ReleaseDateTime:=Now;
                Settings.ReleaseNumber:=StrToInt(GetManifestValue('Release', Manifest));
                ShellExecute(Application.Handle, seOpen, PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
            end
            else
            begin
                Application.MessageBox(
                    PChar('Cannot unpack files. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                    PChar(APPCAPTION), MB_OK + MB_ICONERROR
                );
            end;
        end
        else
        begin
            Application.MessageBox(
                PChar('Cannot download new release package. ' + APPCAPTION + ' will be closed. Please contact IT support.'),
                PChar(APPCAPTION), MB_OK + MB_ICONERROR
            );
        end;

        // Exit
        UpdateForm.Free;
        ExitProcess(0);

    end;
*)

end;


end.
