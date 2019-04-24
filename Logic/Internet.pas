
{$I .\Include\Header.inc}

unit Internet;

interface

uses
    Winapi.ActiveX,
    System.Classes,
    System.SysUtils,
    System.Win.ComObj,
    System.StrUtils,
    System.Variants,
    Vcl.AxCtrls,
    WinHttp_TLB;

    /// <remarks>
    /// Do not use WinInet API in service or service-like process as it requires human facing the application. Use WinHTTP instead.
    /// </remarks>

type


    IConnectivity = Interface(IInterface)
    ['{6C9DEA61-7E15-4FC0-8D66-B11E79F051DF}']
        function IsInternetPresent: boolean;
        function GetResponseText(FullURL: string): string;
        function Download(const SourceUrl: string; DestFileName: String): Boolean;
    End;


    TConnectivity = class(TInterfacedObject, IConnectivity)
    {$TYPEINFO ON}
    private
        var ErrorMessage: string;
        function CallServer(CallUrl: string; Mode: string; var HttpResponse: string): integer;
    public
        function IsInternetPresent: boolean;
        function GetResponseText(FullURL: string): string;
        function Download(const SourceUrl: string; DestFileName: String): Boolean;
    end;


implementation


uses
    Main;


function TConnectivity.CallServer(CallUrl: string; Mode: string; var HttpResponse: string): integer;
var
    IsFinished:  boolean;
    NoAttempts:  cardinal;
    ReturnCode:  integer;
    ReturnText:  string;
    Http:        IWinHttpRequest;
begin
    NoAttempts:=0;
    ReturnCode:=0;
    IsFinished:=False;
    Result:=0;

    if string.IsNullOrEmpty(CallUrl) then Exit;

    Http:=CoWinHttpRequest.Create;
    try
        try
            Http.Option[WinHttpRequestOption_EnableRedirects]:=True;
            Http.Open(Mode, CallUrl, False);

            while (not(IsFinished)) and (NoAttempts < MAX_CHECK_ATTEMPTS) do
            begin
                Inc(NoAttempts);
                HttpResponse:=EmptyStr;
                Http.Send(EmptyParam);

                ReturnCode:=Http.Status;
                ReturnText:=Http.StatusText;

                if Mode = ncsiGet then
                    HttpResponse:=Http.ResponseText;

                if Mode = ncsiHead then
                    HttpResponse:=Http.GetAllResponseHeaders;

                case ReturnCode of

                    // OK
                    200:
                    begin
                        IsFinished:=True;
                    end;

                    // Found, redirect
                    302:
                    begin
                        Http.Open(ncsiGet, Http.GetResponseHeader('Location'), False);
                    end;

                    // Call original URL and send credentials
                    401:
                    begin
                        Http.Open(Mode, CallUrl, False);
                        Http.SetAutoLogonPolicy(WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM);
                    end;

                    // Any other response code
                    else
                    begin
                        HttpResponse:=Http.GetAllResponseHeaders;
                        IsFinished:=True;
                    end;

                end;

            end;

        except
            on E: Exception do
                ErrorMessage:=E.Message;
        end;

    finally
        Http:=nil;
        Result:=ReturnCode;
    end;

end;


/// <summary>
/// Check Microsoft NCSI.
/// </summary>
/// <returns>Boolean. True if HTTP response is 200.</returns>

function TConnectivity.IsInternetPresent: boolean;
var
  Return: string;
begin
    if CallServer(ncsiWww + ncsiFile, ncsiGet, Return) = 200 then
        Result:=True
            else
                Result:=False;
end;


/// <summary>
/// Get the response text from given full URL. Used to extract plain text.
/// </summary>

function TConnectivity.GetResponseText(FullURL: string): string;
var
    Return: string;
begin
    if CallServer(FullURL, ncsiGet, Return) = 200 then
        Result:=Return
            else
                Result:='';
end;


/// <summary>
/// Download file from provided URL source.
/// </summary>

function TConnectivity.Download(const SourceUrl: string; DestFileName: String): Boolean;
var
   Http:        IWinHttpRequest;
   wUrl:        WideString;
   FileStream:  TFileStream;
   HttpStream:  IStream;
   OleStream:   TOleStream;
begin

    Result:=False;

    try
        wUrl:=SourceUrl;

        Http:=CoWinHttpRequest.Create;
        Http.open('GET', wUrl, False);
        Http.send(EmptyParam);

        if Http.status = 200 then
        begin
            Result:=True;

            HttpStream:=IUnknown(Http.ResponseStream) as IStream;
            OleStream :=TOleStream.Create(HttpStream);

            try
                FileStream:=TFileStream.Create(DestFileName, fmCreate);
                try
                    OleStream.Position:=0;
                    FileStream.CopyFrom(OleStream, OleStream.Size);
                finally
                    FileStream.Free;
                end;

            finally
                OleStream.Free;
            end;

        end;

    except
        Result:=False;
    end;

end;


end.

