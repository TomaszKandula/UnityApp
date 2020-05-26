unit Unity.RestWrapper;

// -------------------------------------------------------------
// Application logic, access layer. Can be referenced by anyone.
// Cannot hold references to the View or to Logic.
// -------------------------------------------------------------

interface


uses
    System.Classes,
    System.Generics.Collections,
    REST.Types,
    REST.Client;


type

    /// <summary>
    /// Exposes properties and methods that allows easy utilisation of WebApi service throught REST.
    /// </summary>
    IRESTFul = Interface(IInterface)
    ['{3A64616D-26BE-44F8-80C8-F69DE813D439}']
        function GetAccessToken(): string;
        function GetExecuteError(): string;
        function GetStatusCode(): integer;
        function GetCustomBody(): string;
        function GetContent(): string;
        function GetHeaders(): string;
        function GetClientAccept(): string;
        function GetClientAcceptCharset(): string;
        function GetClientAcceptEncoding(): string;
        function GetClientAllowCookies(): boolean;
        function GetClientAutoCreateParams(): boolean;
        function GetClientBaseURL(): string;
        function GetClientContentType(): string;
        function GetClientFallbackCharsetEncoding(): string;
        function GetClientHandleRedirects(): boolean;
        function GetClientRaiseExceptionOn500(): boolean;
        function GetClientSynchronizedEvents(): boolean;
        function GetClientUserAgent(): string;
        function GetRequestAccept(): string;
        function GetRequestAcceptCharset(): string;
        function GetRequestAutoCreateParams(): boolean;
        function GetRequestHandleRedirects(): boolean;
        function GetRequestMethod(): TRESTRequestMethod;
        function GetRequestSynchronizedEvents(): boolean;
        function GetRequestTimeout(): integer;
        procedure SetAccessToken(NewValue: string);
        procedure SetCustomBody(NewValue: string);
        procedure SetClientAccept(NewValue: string);
        procedure SetClientAcceptCharset(NewValue: string);
        procedure SetClientAcceptEncoding(NewValue: string);
        procedure SetClientAllowCookies(NewValue: boolean);
        procedure SetClientAutoCreateParams(NewValue: boolean);
        procedure SetClientBaseURL(NewValue: string);
        procedure SetClientContentType(NewValue: string);
        procedure SetClientFallbackCharsetEncoding(NewValue: string);
        procedure SetClientHandleRedirects(NewValue: boolean);
        procedure SetClientRaiseExceptionOn500(NewValue: boolean);
        procedure SetClientSynchronizedEvents(NewValue: boolean);
        procedure SetClientUserAgent(NewValue: string);
        procedure SetRequestAccept(NewValue: string);
        procedure SetRequestAcceptCharset(NewValue: string);
        procedure SetRequestAutoCreateParams(NewValue: boolean);
        procedure SetRequestHandleRedirects(NewValue: boolean);
        procedure SetRequestMethod(NewValue: TRESTRequestMethod);
        procedure SetRequestSynchronizedEvents(NewValue: boolean);
        procedure SetRequestTimeout(NewValue: integer);
        property ExecuteError: string read GetExecuteError;
        property StatusCode: integer read GetStatusCode;
        property Content: string read GetContent;
        property Headers: string read GetHeaders;
        property AccessToken: string read GetAccessToken write SetAccessToken;
        property CustomBody: string read GetCustomBody write SetCustomBody;
        property ClientAccept: string read GetClientAccept write SetClientAccept;
        property ClientAcceptCharset: string read GetClientAcceptCharset write SetClientAcceptCharset;
        property ClientAcceptEncoding: string read GetClientAcceptEncoding write SetClientAcceptEncoding;
        property ClientAllowCookies: boolean read GetClientAllowCookies write SetClientAllowCookies;
        property ClientAutoCreateParams: boolean read GetClientAutoCreateParams write SetClientAutoCreateParams;
        property ClientBaseURL: string read GetClientBaseURL write SetClientBaseURL;
        property ClientContentType: string read GetClientContentType write SetClientContentType;
        property ClientFallbackCharsetEncoding: string read GetClientFallbackCharsetEncoding write SetClientFallbackCharsetEncoding;
        property ClientHandleRedirects: boolean read GetClientHandleRedirects write SetClientHandleRedirects;
        property ClientRaiseExceptionOn500: boolean read GetClientRaiseExceptionOn500 write SetClientRaiseExceptionOn500;
        property ClientSynchronizedEvents: boolean read GetClientSynchronizedEvents write SetClientSynchronizedEvents;
        property ClientUserAgent: string read GetClientUserAgent write SetClientUserAgent;
        property RequestAccept: string read GetRequestAccept write SetRequestAccept;
        property RequestAcceptCharset: string read GetRequestAcceptCharset write SetRequestAcceptCharset;
        property RequestAutoCreateParams: boolean read GetRequestAutoCreateParams write SetRequestAutoCreateParams;
        property RequestHandleRedirects: boolean read GetRequestHandleRedirects write SetRequestHandleRedirects;
        property RequestMethod: TRESTRequestMethod read GetRequestMethod write SetRequestMethod;
        property RequestSynchronizedEvents: boolean read GetRequestSynchronizedEvents write SetRequestSynchronizedEvents;
        property RequestTimeout: integer read GetRequestTimeout write SetRequestTimeout;
        function Execute: boolean;
        procedure AddParameter(QueryName: string; ParamValue: string);
        procedure ClearParameters;
        procedure SelectContentType(LContentType: TRESTContentType);
    end;


    /// <summary>
    /// Implementation of simple wrapper around TRESTClient, TRESTResponse, TRESTRequest classes.
    /// Do not use it directly.
    /// </summary>
    TRESTful = class(TInterfacedObject, IRESTFul)
    strict private
        var FContentType: TRESTContentType;
        var FAccessToken: string;
        var FRestClient: TRESTClient;
        var FRestRequest: TRESTRequest;
        var FRestResponse: TRESTResponse;
        var FQueryList: TList<string>;
        var FParamList: TList<string>;
        var FStatusCode: integer;
        var FResponseContent: string;
        var FCustomBody: string;
        var FExecuteError: string;
        function GetAccessToken(): string;
        function GetExecuteError(): string;
        function GetStatusCode(): integer;
        function GetCustomBody(): string;
        function GetContent(): string;
        function GetHeaders(): string;
        function GetClientAccept(): string;
        function GetClientAcceptCharset(): string;
        function GetClientAcceptEncoding(): string;
        function GetClientAllowCookies(): boolean;
        function GetClientAutoCreateParams(): boolean;
        function GetClientBaseURL(): string;
        function GetClientContentType(): string;
        function GetClientFallbackCharsetEncoding(): string;
        function GetClientHandleRedirects(): boolean;
        function GetClientRaiseExceptionOn500(): boolean;
        function GetClientSynchronizedEvents(): boolean;
        function GetClientUserAgent(): string;
        function GetRequestAccept(): string;
        function GetRequestAcceptCharset(): string;
        function GetRequestAutoCreateParams(): boolean;
        function GetRequestHandleRedirects(): boolean;
        function GetRequestMethod(): TRESTRequestMethod;
        function GetRequestSynchronizedEvents(): boolean;
        function GetRequestTimeout(): integer;
        procedure SetAccessToken(NewValue: string);
        procedure SetCustomBody(NewValue: string);
        procedure SetClientAccept(NewValue: string);
        procedure SetClientAcceptCharset(NewValue: string);
        procedure SetClientAcceptEncoding(NewValue: string);
        procedure SetClientAllowCookies(NewValue: boolean);
        procedure SetClientAutoCreateParams(NewValue: boolean);
        procedure SetClientBaseURL(NewValue: string);
        procedure SetClientContentType(NewValue: string);
        procedure SetClientFallbackCharsetEncoding(NewValue: string);
        procedure SetClientHandleRedirects(NewValue: boolean);
        procedure SetClientRaiseExceptionOn500(NewValue: boolean);
        procedure SetClientSynchronizedEvents(NewValue: boolean);
        procedure SetClientUserAgent(NewValue: string);
        procedure SetRequestAccept(NewValue: string);
        procedure SetRequestAcceptCharset(NewValue: string);
        procedure SetRequestAutoCreateParams(NewValue: boolean);
        procedure SetRequestHandleRedirects(NewValue: boolean);
        procedure SetRequestMethod(NewValue: TRESTRequestMethod);
        procedure SetRequestSynchronizedEvents(NewValue: boolean);
        procedure SetRequestTimeout(NewValue: integer);
        procedure TrimContent(var TextStr: string);
    public
        constructor Create();
        destructor Destroy; override;
        property ExecuteError: string read GetExecuteError;
        property StatusCode: integer read GetStatusCode;
        property Content: string read GetContent;
        property Headers: string read GetHeaders;
        property AccessToken: string read GetAccessToken write SetAccessToken;
        property CustomBody: string read GetCustomBody write SetCustomBody;
        property ClientAccept: string read GetClientAccept write SetClientAccept;
        property ClientAcceptCharset: string read GetClientAcceptCharset write SetClientAcceptCharset;
        property ClientAcceptEncoding: string read GetClientAcceptEncoding write SetClientAcceptEncoding;
        property ClientAllowCookies: boolean read GetClientAllowCookies write SetClientAllowCookies;
        property ClientAutoCreateParams: boolean read GetClientAutoCreateParams write SetClientAutoCreateParams;
        property ClientBaseURL: string read GetClientBaseURL write SetClientBaseURL;
        property ClientContentType: string read GetClientContentType write SetClientContentType;
        property ClientFallbackCharsetEncoding: string read GetClientFallbackCharsetEncoding write SetClientFallbackCharsetEncoding;
        property ClientHandleRedirects: boolean read GetClientHandleRedirects write SetClientHandleRedirects;
        property ClientRaiseExceptionOn500: boolean read GetClientRaiseExceptionOn500 write SetClientRaiseExceptionOn500;
        property ClientSynchronizedEvents: boolean read GetClientSynchronizedEvents write SetClientSynchronizedEvents;
        property ClientUserAgent: string read GetClientUserAgent write SetClientUserAgent;
        property RequestAccept: string read GetRequestAccept write SetRequestAccept;
        property RequestAcceptCharset: string read GetRequestAcceptCharset write SetRequestAcceptCharset;
        property RequestAutoCreateParams: boolean read GetRequestAutoCreateParams write SetRequestAutoCreateParams;
        property RequestHandleRedirects: boolean read GetRequestHandleRedirects write SetRequestHandleRedirects;
        property RequestMethod: TRESTRequestMethod read GetRequestMethod write SetRequestMethod;
        property RequestSynchronizedEvents: boolean read GetRequestSynchronizedEvents write SetRequestSynchronizedEvents;
        property RequestTimeout: integer read GetRequestTimeout write SetRequestTimeout;
        function Execute: boolean;
        procedure AddParameter(QueryName: string; ParamValue: string);
        procedure ClearParameters;
        procedure SelectContentType(ContentType: TRESTContentType);
    end;


implementation


uses
    System.SysUtils;


constructor TRESTful.Create();
begin

    FRestClient  :=TRESTClient.Create('');
    FRestResponse:=TRESTResponse.Create(nil);
    FRestRequest :=TRESTRequest.Create(nil);

    FRestRequest.Client  :=FRestClient;
    FRestRequest.Response:=FRestResponse;

    FQueryList:=TList<string>.Create();
    FParamList:=TList<string>.Create();

    ClientAcceptCharset      :='UTF-8, *;q=0.8';
    ClientAcceptEncoding     :='gzip, deflate';
    ClientAllowCookies       :=True;
    ClientAutoCreateParams   :=True;
    ClientHandleRedirects    :=True;
    ClientRaiseExceptionOn500:=True;
    ClientSynchronizedEvents :=True;
    ClientUserAgent          :='Unity Platform RESTClient/1.0';

    RequestAccept            :=FRestClient.Accept;
    RequestAcceptCharset     :=FRestClient.AcceptCharset;
    RequestAutoCreateParams  :=FRestClient.AutoCreateParams;
    RequestHandleRedirects   :=True;
    RequestSynchronizedEvents:=False;
    RequestTimeout           :=120000;

end;


destructor TRESTful.Destroy();
begin
    FQueryList.Free();
    FParamList.Free();
    FRestClient.Free();
    FRestResponse.Free();
    FRestRequest.Free();
    inherited;
end;


function TRESTful.Execute(): boolean;
begin

    Result:=False;

    if (FRestRequest.Method = TRESTRequestMethod.rmGET)
    or (FRestRequest.Method = TRESTRequestMethod.rmDELETE) then
    begin

        if ((FQueryList.Count > 0) and (FParamList.Count > 0)) and (FQueryList.Count = FParamList.Count) then
        begin

            FRestRequest.Params.Clear;
            for var Index:=0 to FQueryList.Count - 1 do
                FRestRequest.AddParameter(FQueryList.Items[Index], FParamList.Items[Index]);

        end;

    end;

    if (FRestRequest.Method = TRESTRequestMethod.rmPOST)
    or (FRestRequest.Method = TRESTRequestMethod.rmPUT)
    or (FRestRequest.Method = TRESTRequestMethod.rmPATCH) then
    begin

        if (FRestRequest.Method = TRESTRequestMethod.rmPOST) then
        begin

            if ((FQueryList.Count > 0) and (FParamList.Count > 0)) and (FQueryList.Count = FParamList.Count) then
            begin

                FRestRequest.Params.Clear;
                for var Index:=0 to FQueryList.Count - 1 do
                    FRestRequest.AddParameter(FQueryList.Items[Index], FParamList.Items[Index]);

            end;

        end;

        if not(String.IsNullOrEmpty(FCustomBody)) then
        begin
            FRestRequest.Body.ClearBody;
            FRestRequest.Body.Add(FCustomBody, FContentType);
        end;

    end;

    try

        FRestRequest.Execute();

        if FRestResponse.StatusCode > 0 then
        begin

            FStatusCode:=FRestResponse.StatusCode;

            if (not String.IsNullOrEmpty(FRestResponse.Content)) and (not String.IsNullOrWhiteSpace(FRestResponse.Content)) then
            begin

                // -----------------------------------------------------------------------------------------------
                // Remove from recieved string content any \" leaving just single quote. This is necessary in case
                // WebServer returns content as string object instead of raw string or POCO, or by mistake returns
                // content serialized twice (it adds quotes to the start and end of the content), in such case it
                // requires further treatment on quotes inside string.
                // -----------------------------------------------------------------------------------------------
                FResponseContent:=FRestResponse.Content;
                FResponseContent:=FResponseContent.Replace('\"','"');
                TrimContent(FResponseContent);

            end;

            Result:=True;

        end;

    except
        on E: Exception do
            FExecuteError:=E.Message;

    end;

end;


procedure TRESTful.AddParameter(QueryName: string; ParamValue: string);
begin

    if (string.IsNullOrEmpty(QueryName) or string.IsNullOrEmpty(ParamValue)) then Exit();

    if (Assigned(FQueryList) and Assigned(FParamList)) then
    begin
        FQueryList.Add(QueryName);
        FParamList.Add(ParamValue);
    end;

end;


procedure TRESTful.ClearParameters();
begin

    if (Assigned(FQueryList) and Assigned(FParamList)) then
    begin
        FQueryList.Clear();
        FParamList.Clear();
    end;

    FRestRequest.Params.Clear();

end;


procedure TRESTful.TrimContent(var TextStr: string);
begin
    // ---------------------------------------------------------
    // WARNING! Java starts couting string characters from zero,
    // while Delphi and Swift count it from one.
    // ---------------------------------------------------------
    if TextStr[1] = #34 then TextStr[1]:=#32;
    if TextStr[TextStr.Length - 1] = #34 then TextStr[TextStr.Length - 1]:=#32;
end;


procedure TRESTful.SelectContentType(ContentType: TRESTContentType);
begin

    // Ensure all parameters are cleared when Content Type is changed
    ClearParameters();

    FContentType:=ContentType;
    case ContentType of

        TRESTContentType.ctAPPLICATION_JSON:
        begin

            ClientAccept     :='application/json, text/plain; q=0.9, text/html;q=0.8,';
            ClientContentType:='application/json';

            if not String.IsNullOrEmpty(AccessToken) then
                FRestRequest.AddAuthParameter(
                    'Authorization',
                    'Bearer ' + FAccessToken,
                    TREStRequestParameterKind.pkHTTPHEADER,
                    [poDoNotEncode]
                );

        end;

        TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED:
        begin
            ClientAccept     :='*/*';
            ClientContentType:='application/x-www-form-urlencoded';
        end;

    end;

end;


function TRESTful.GetAccessToken(): string;
begin
    Result:=FAccessToken;
end;


function TRESTful.GetExecuteError(): string;
begin
    Result:=FExecuteError;
end;


function TRESTful.GetStatusCode(): integer;
begin
    Result:=FStatusCode;
end;


function TRESTful.GetCustomBody(): string;
begin
    Result:=FCustomBody;
end;


function TRESTful.GetContent(): string;
begin
    Result:=FResponseContent;
end;


function TRESTful.GetHeaders(): string;
begin
    if Assigned(FRestResponse) then Result:=FRestResponse.Headers.Text;
end;


function TRESTful.GetClientAccept(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.Accept;
end;


function TRESTful.GetClientAcceptCharset(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.AcceptCharset;
end;


function TRESTful.GetClientAcceptEncoding(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.AcceptEncoding;
end;


function TRESTful.GetClientAllowCookies(): boolean;
begin
    if Assigned(FRestClient) then Result:=FRestClient.AllowCookies else Result:=False;
end;


function TRESTful.GetClientAutoCreateParams(): boolean;
begin
    if Assigned(FRestClient) then Result:=FRestClient.AutoCreateParams else Result:=False;
end;


function TRESTful.GetClientBaseURL(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.BaseURL;
end;


function TRESTful.GetClientContentType(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.ContentType;
end;


function TRESTful.GetClientFallbackCharsetEncoding(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.FallbackCharsetEncoding;
end;


function TRESTful.GetClientHandleRedirects(): boolean;
begin
    if Assigned(FRestClient) then Result:=FRestClient.HandleRedirects else Result:=False;
end;


function TRESTful.GetClientRaiseExceptionOn500(): boolean;
begin
    if Assigned(FRestClient) then Result:=FRestClient.RaiseExceptionOn500 else Result:=False;
end;


function TRESTful.GetClientSynchronizedEvents(): boolean;
begin
    if Assigned(FRestClient) then Result:=FRestClient.SynchronizedEvents else Result:=False;
end;


function TRESTful.GetClientUserAgent(): string;
begin
    if Assigned(FRestClient) then Result:=FRestClient.UserAgent;
end;


function TRESTful.GetRequestAccept(): string;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.Accept;
end;


function TRESTful.GetRequestAcceptCharset(): string;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.AcceptCharset;
end;


function TRESTful.GetRequestAutoCreateParams(): boolean;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.AutoCreateParams else Result:=False;
end;


function TRESTful.GetRequestHandleRedirects(): boolean;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.HandleRedirects else Result:=False;
end;


function TRESTful.GetRequestMethod(): TRESTRequestMethod;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.Method else Result:=TRESTRequestMethod.rmGET;
end;


function TRESTful.GetRequestSynchronizedEvents(): boolean;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.SynchronizedEvents else Result:=False;
end;


function TRESTful.GetRequestTimeout(): integer;
begin
    if Assigned(FRestRequest) then Result:=FRestRequest.Timeout else Result:=30000 {30 seconds};
end;


procedure TRESTful.SetAccessToken(NewValue: string);
begin
    FAccessToken:=NewValue;
end;


procedure TRESTful.SetCustomBody(NewValue: string);
begin
    FCustomBody:=NewValue;
end;


procedure TRESTful.SetClientAccept(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.Accept:=NewValue;
end;


procedure TRESTful.SetClientAcceptCharset(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.AcceptCharset:=NewValue;
end;


procedure TRESTful.SetClientAcceptEncoding(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.AcceptEncoding:=NewValue;
end;


procedure TRESTful.SetClientAllowCookies(NewValue: boolean);
begin
    if Assigned(FRestClient) then FRestClient.AllowCookies:=NewValue;
end;


procedure TRESTful.SetClientAutoCreateParams(NewValue: boolean);
begin
    if Assigned(FRestClient) then FRestClient.AutoCreateParams:=NewValue;
end;


procedure TRESTful.SetClientBaseURL(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.BaseURL:=NewValue;
end;


procedure TRESTful.SetClientContentType(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.ContentType:=NewValue;
end;


procedure TRESTful.SetClientFallbackCharsetEncoding(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.FallbackCharsetEncoding:=NewValue;
end;


procedure TRESTful.SetClientHandleRedirects(NewValue: boolean);
begin
    if Assigned(FRestClient) then FRestClient.HandleRedirects:=NewValue;
end;


procedure TRESTful.SetClientRaiseExceptionOn500(NewValue: boolean);
begin
    if Assigned(FRestClient) then FRestClient.RaiseExceptionOn500:=NewValue;
end;


procedure TRESTful.SetClientSynchronizedEvents(NewValue: boolean);
begin
    if Assigned(FRestClient) then FRestClient.SynchronizedEvents:=NewValue;
end;


procedure TRESTful.SetClientUserAgent(NewValue: string);
begin
    if Assigned(FRestClient) then FRestClient.UserAgent:=NewValue;
end;


procedure TRESTful.SetRequestAccept(NewValue: string);
begin
    if Assigned(FRestRequest) then FRestRequest.Accept:=NewValue;
end;


procedure TRESTful.SetRequestAcceptCharset(NewValue: string);
begin
    if Assigned(FRestRequest) then FRestRequest.AcceptCharset:=NewValue;
end;


procedure TRESTful.SetRequestAutoCreateParams(NewValue: boolean);
begin
    if Assigned(FRestRequest) then FRestRequest.AutoCreateParams:=NewValue;
end;


procedure TRESTful.SetRequestHandleRedirects(NewValue: boolean);
begin
    if Assigned(FRestRequest) then FRestRequest.HandleRedirects:=NewValue;
end;


procedure TRESTful.SetRequestMethod(NewValue: TRESTRequestMethod);
begin
    if Assigned(FRestRequest) then FRestRequest.Method:=NewValue;
end;


procedure TRESTful.SetRequestSynchronizedEvents(NewValue: boolean);
begin
    if Assigned(FRestRequest) then FRestRequest.SynchronizedEvents:=NewValue;
end;


procedure TRESTful.SetRequestTimeout(NewValue: integer);
begin
    if Assigned(FRestRequest) then FRestRequest.Timeout:=NewValue;
end;


end.

