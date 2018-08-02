
{$I .\Include\Header.inc}

unit Internet;

interface

/// <remarks>
///     Do not use WinInet API in service or service-like process as it requires human facing the application. Use WinHTTP instead.
/// </remarks>

uses
    Main, WinHttp_TLB, Classes, SysUtils, ComObj, StrUtils;

type

    /// <summary>
    ///     Base class for checking internet connection.
    /// </summary>

    TInternetConnectivity = class
        {$TYPEINFO ON}
        private
            var ErrorMessage: string;
            function CallNCSIServer(Server: string; FileName: string; Mode: string; var HttpResponse: string): integer;
        public
            function IsInternetPresent: boolean;
    end;

implementation


/// <summary>
///
/// </summary>
/// <param name="Server"></param>
/// <param name="FileName"></param>
/// <param name="Mode"></param>
/// <returns>Integer. Http response code, we expects 200.</returns>

function TInternetConnectivity.CallNCSIServer(Server: string; FileName: string; Mode: string; var HttpResponse: string): integer;
var
    IsFinished:  boolean;
    NoAttempts:  cardinal;
    ReturnCode:  integer;
    ReturnText:  string;
    CallUrl:     string;
    Http:        IWinHttpRequest;
begin
    NoAttempts:=0;
    ReturnCode:=0;
    IsFinished:=False;
    Result:=0;

    if (Server = '') or (FileName = '') then  Exit;

    if Mode = ncsiGet then
        CallUrl:=server + filename;

    if Mode = ncsiHead then
        CallUrl:=server;

    Http:=CoWinHttpRequest.Create;
    try
        try
            Http.Option[WinHttpRequestOption_EnableRedirects]:=True;
            Http.Open(Mode, CallUrl, False);

            while (not(IsFinished)) and (NoAttempts < MAX_CHECK_ATTEMPTS) do
            begin
                Inc(NoAttempts);
                HttpResponse:=EmptyStr;
                Http.Send(EmptyStr);

                ReturnCode:=Http.Status;
                ReturnText:=Http.StatusText;

                if Mode = ncsiGet then
                    HttpResponse:=Http.GetAllResponseHeaders + CRLF + Http.ResponseText;

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
///     Check Microsoft NCSI.
/// </summary>
/// <returns>Boolean. True if HTTP response is 200.</returns>

function TInternetConnectivity.IsInternetPresent: boolean;
var
  Return: string;
begin
    if CallNCSIServer(ncsiWww, ncsiFile, ncsiGet, Return) = 200 then
        Result:=True
            else
                Result:=False;
end;

end.
