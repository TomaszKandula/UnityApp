unit Handler.Rest;

// -----------------------------------
// Application logic, access layer.
// Can be referenced by anyone.
// Cannot hold references to the View.
// -----------------------------------

interface


uses
    System.SysUtils,
    System.Types,
    System.UITypes,
    System.Classes,
    System.Variants,
    System.Generics.Collections,
    REST.Types,
    REST.Consts,
    REST.Json,
    REST.Client,
    REST.Authenticator.Basic,
    IPPeerClient;


type


    IRESTFul = Interface(IInterface)
    ['{3A64616D-26BE-44F8-80C8-F69DE813D439}']

        // --------------------------------
        // Undisclosed getters and setters.
        // --------------------------------

        function  GetExecuteError:                  string;
        function  GetStatusCode:                    integer;
        function  GetCustomBody:                    string;
        function  GetContent:                       string;
        function  GetHeaders:                       string;
        function  GethttpAuthUsername:              string;
        function  GethttpAuthPassword:              string;
        function  GetClientAccept:                  string;
        function  GetClientAcceptCharset:           string;
        function  GetClientAllowCookies:            boolean;
        function  GetClientAutoCreateParams:        boolean;
        function  GetClientBaseURL:                 string;
        function  GetClientContentType:             string;
        function  GetClientFallbackCharsetEncoding: string;
        function  GetClientHandleRedirects:         boolean;
        function  GetClientRaiseExceptionOn500:     boolean;
        function  GetClientSynchronizedEvents:      boolean;
        function  GetClientUserAgent:               string;
        function  GetRequestAccept:                 string;
        function  GetRequestAcceptCharset:          string;
        function  GetRequestAutoCreateParams:       boolean;
        function  GetRequestHandleRedirects:        boolean;
        function  GetRequestMethod:                 TRESTRequestMethod;
        function  GetRequestSynchronizedEvents:     boolean;
        function  GetRequestTimeout:                integer;
        procedure SetCustomBody(NewValue: string);
        procedure SetClientAccept(NewValue: string);
        procedure SetClientAcceptCharset(NewValue: string);
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

        // -------------------
        // Exposed properties.
        // -------------------

        property ExecuteError:                  string             read GetExecuteError;
        property StatusCode:                    integer            read GetStatusCode;
        property Content:                       string             read GetContent;
        property Headers:                       string             read GetHeaders;
        property httpAuthUsername:              string             read GethttpAuthUsername;
        property httpAuthPassword:              string             read GethttpAuthPassword;
        property CustomBody:                    string             read GetCustomBody                    write SetCustomBody;
        property ClientAccept:                  string             read GetClientAccept                  write SetClientAccept;
        property ClientAcceptCharset:           string             read GetClientAcceptCharset           write SetClientAcceptCharset;
        property ClientAllowCookies:            boolean            read GetClientAllowCookies            write SetClientAllowCookies;
        property ClientAutoCreateParams:        boolean            read GetClientAutoCreateParams        write SetClientAutoCreateParams;
        property ClientBaseURL:                 string             read GetClientBaseURL                 write SetClientBaseURL;
        property ClientContentType:             string             read GetClientContentType             write SetClientContentType;
        property ClientFallbackCharsetEncoding: string             read GetClientFallbackCharsetEncoding write SetClientFallbackCharsetEncoding;
        property ClientHandleRedirects:         boolean            read GetClientHandleRedirects         write SetClientHandleRedirects;
        property ClientRaiseExceptionOn500:     boolean            read GetClientRaiseExceptionOn500     write SetClientRaiseExceptionOn500;
        property ClientSynchronizedEvents:      boolean            read GetClientSynchronizedEvents      write SetClientSynchronizedEvents;
        property ClientUserAgent:               string             read GetClientUserAgent               write SetClientUserAgent;
        property RequestAccept:                 string             read GetRequestAccept                 write SetRequestAccept;
        property RequestAcceptCharset:          string             read GetRequestAcceptCharset          write SetRequestAcceptCharset;
        property RequestAutoCreateParams:       boolean            read GetRequestAutoCreateParams       write SetRequestAutoCreateParams;
        property RequestHandleRedirects:        boolean            read GetRequestHandleRedirects        write SetRequestHandleRedirects;
        property RequestMethod:                 TRESTRequestMethod read GetRequestMethod                 write SetRequestMethod;
        property RequestSynchronizedEvents:     boolean            read GetRequestSynchronizedEvents     write SetRequestSynchronizedEvents;
        property RequestTimeout:                integer            read GetRequestTimeout                write SetRequestTimeout;

        // ----------------
        // Exposed methods.
        // ----------------

        function  Execute: boolean;
        procedure AddParameter(QueryName: string; ParamValue: string);
        procedure ClearParameters;

    end;


    TRESTful = class(TInterfacedObject, IRESTFul)
    private

        var httpAuth:         THTTPBasicAuthenticator;
        var restClient:       TRESTClient;
        var restRequest:      TRESTRequest;
        var restResponse:     TRESTResponse;
        var queryList:        TList<string>;
        var paramList:        TList<string>;
        var FStatusCode:      integer;
        var FResponseContent: string;
        var FCustomBody:      string;
        var FExecuteError:    string;

        // -------------------
        // Property's getters.
        // -------------------

        function GetExecuteError:                  string;
        function GetStatusCode:                    integer;
        function GetCustomBody:                    string;
        function GetContent:                       string;
        function GetHeaders:                       string;
        function GethttpAuthUsername:              string;
        function GethttpAuthPassword:              string;
        function GetClientAccept:                  string;
        function GetClientAcceptCharset:           string;
        function GetClientAllowCookies:            boolean;
        function GetClientAutoCreateParams:        boolean;
        function GetClientBaseURL:                 string;
        function GetClientContentType:             string;
        function GetClientFallbackCharsetEncoding: string;
        function GetClientHandleRedirects:         boolean;
        function GetClientRaiseExceptionOn500:     boolean;
        function GetClientSynchronizedEvents:      boolean;
        function GetClientUserAgent:               string;
        function GetRequestAccept:                 string;
        function GetRequestAcceptCharset:          string;
        function GetRequestAutoCreateParams:       boolean;
        function GetRequestHandleRedirects:        boolean;
        function GetRequestMethod:                 TRESTRequestMethod;
        function GetRequestSynchronizedEvents:     boolean;
        function GetRequestTimeout:                integer;

        // -------------------
        // Property's setters.
        // -------------------

        procedure SetCustomBody(NewValue:                    string);
        procedure SetClientAccept(NewValue:                  string);
        procedure SetClientAcceptCharset(NewValue:           string);
        procedure SetClientAllowCookies(NewValue:            boolean);
        procedure SetClientAutoCreateParams(NewValue:        boolean);
        procedure SetClientBaseURL(NewValue:                 string);
        procedure SetClientContentType(NewValue:             string);
        procedure SetClientFallbackCharsetEncoding(NewValue: string);
        procedure SetClientHandleRedirects(NewValue:         boolean);
        procedure SetClientRaiseExceptionOn500(NewValue:     boolean);
        procedure SetClientSynchronizedEvents(NewValue:      boolean);
        procedure SetClientUserAgent(NewValue:               string);
        procedure SetRequestAccept(NewValue:                 string);
        procedure SetRequestAcceptCharset(NewValue:          string);
        procedure SetRequestAutoCreateParams(NewValue:       boolean);
        procedure SetRequestHandleRedirects(NewValue:        boolean);
        procedure SetRequestMethod(NewValue:                 TRESTRequestMethod);
        procedure SetRequestSynchronizedEvents(NewValue:     boolean);
        procedure SetRequestTimeout(NewValue:                integer);

        // ----------------------------------
        // Trimming received response string.
        // ----------------------------------

        procedure TrimContent(var TextStr: string);

    public
        constructor Create(UserName: string; Password: string);
        destructor Destroy; override;
        property ExecuteError:                  string             read GetExecuteError;
        property StatusCode:                    integer            read GetStatusCode;
        property Content:                       string             read GetContent;
        property Headers:                       string             read GetHeaders;
        property httpAuthUsername:              string             read GethttpAuthUsername;
        property httpAuthPassword:              string             read GethttpAuthPassword;
        property CustomBody:                    string             read GetCustomBody                    write SetCustomBody;
        property ClientAccept:                  string             read GetClientAccept                  write SetClientAccept;
        property ClientAcceptCharset:           string             read GetClientAcceptCharset           write SetClientAcceptCharset;
        property ClientAllowCookies:            boolean            read GetClientAllowCookies            write SetClientAllowCookies;
        property ClientAutoCreateParams:        boolean            read GetClientAutoCreateParams        write SetClientAutoCreateParams;
        property ClientBaseURL:                 string             read GetClientBaseURL                 write SetClientBaseURL;
        property ClientContentType:             string             read GetClientContentType             write SetClientContentType;
        property ClientFallbackCharsetEncoding: string             read GetClientFallbackCharsetEncoding write SetClientFallbackCharsetEncoding;
        property ClientHandleRedirects:         boolean            read GetClientHandleRedirects         write SetClientHandleRedirects;
        property ClientRaiseExceptionOn500:     boolean            read GetClientRaiseExceptionOn500     write SetClientRaiseExceptionOn500;
        property ClientSynchronizedEvents:      boolean            read GetClientSynchronizedEvents      write SetClientSynchronizedEvents;
        property ClientUserAgent:               string             read GetClientUserAgent               write SetClientUserAgent;
        property RequestAccept:                 string             read GetRequestAccept                 write SetRequestAccept;
        property RequestAcceptCharset:          string             read GetRequestAcceptCharset          write SetRequestAcceptCharset;
        property RequestAutoCreateParams:       boolean            read GetRequestAutoCreateParams       write SetRequestAutoCreateParams;
        property RequestHandleRedirects:        boolean            read GetRequestHandleRedirects        write SetRequestHandleRedirects;
        property RequestMethod:                 TRESTRequestMethod read GetRequestMethod                 write SetRequestMethod;
        property RequestSynchronizedEvents:     boolean            read GetRequestSynchronizedEvents     write SetRequestSynchronizedEvents;
        property RequestTimeout:                integer            read GetRequestTimeout                write SetRequestTimeout;
        function Execute: boolean;
        procedure AddParameter(QueryName: string; ParamValue: string);
        procedure ClearParameters;
    end;


implementation


uses
    Unity.RestAuth;


// ---------------------------------------------------------------------------------------------------------------------------------------- CREATE & RELEASE //


/// <summary>
/// Initialize and assign objects to handle requests/responses with initial settings.
/// </summary>

constructor TRESTful.Create(UserName: string; Password: string);
begin

    httpAuth    :=THTTPBasicAuthenticator.Create(UserName, Password);
    restClient  :=TRESTClient.Create('');
    restResponse:=TRESTResponse.Create(nil);
    restRequest :=TRESTRequest.Create(nil);

    restClient.Authenticator:=httpAuth;
    restRequest.Client:=restClient;
    restRequest.Response:=restResponse;

    queryList:=TList<string>.Create;
    paramList:=TList<string>.Create;

    ClientAccept                 :=TRestAuth.restAccept;
    ClientAcceptCharset          :=TRestAuth.restAcceptCharset;
    ClientAllowCookies           :=True;
    ClientAutoCreateParams       :=True;
    ClientBaseURL                :=TRestAuth.restApiBaseUrl;
    ClientContentType            :=TRestAuth.restContentType;
    ClientFallbackCharsetEncoding:=TRestAuth.restEncoding;
    ClientHandleRedirects        :=True;
    ClientRaiseExceptionOn500    :=True;
    ClientSynchronizedEvents     :=True;
    ClientUserAgent              :=TRestAuth.restUserAgent;

    RequestAccept            :=restClient.Accept;
    RequestAcceptCharset     :=restClient.AcceptCharset;
    RequestAutoCreateParams  :=restClient.AutoCreateParams;
    RequestHandleRedirects   :=True;
    RequestSynchronizedEvents:=False;
    RequestTimeout           :=30000;

end;


destructor TRESTful.Destroy;
begin
    queryList.Free;
    paramList.Free;
    httpAuth.Free;
    restClient.Free;
    restResponse.Free;
    restRequest.Free;
    inherited;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- EXECUTE METHODS //


function TRESTful.Execute: boolean;
begin

    Result:=False;

    // ---------------
    // GET and DELETE.
    // ---------------

    if (restRequest.Method = TRESTRequestMethod.rmGET) or (restRequest.Method = TRESTRequestMethod.rmDELETE) then
    begin

        if ((queryList.Count > 0) and (paramList.Count > 0)) and (queryList.Count = paramList.Count) then
        begin
            restRequest.Params.Clear;
            for var iCNT: integer:=0 to queryList.Count - 1 do
                restRequest.AddParameter(queryList.Items[iCNT], paramList.Items[iCNT]);
        end;

    end;

    // -------------
    // POST and PUT.
    // -------------

    if (restRequest.Method = TRESTRequestMethod.rmPOST) or (restRequest.Method = TRESTRequestMethod.rmPUT) then
    begin

        if not(String.IsNullOrEmpty(CustomBody)) then
        begin

            // -----------------------------------------------------------------------------------------------
            // Replace single quotes into '\"' and add single quote to begin and end of custom body
            // This is necessary because WebApi on .NetCore server defines content as string object instead of
            // raw string or POCO.
            // -----------------------------------------------------------------------------------------------

            CustomBody:=CustomBody.Replace('"', '\"');
            CustomBody:='"' + CustomBody + '"';
            restRequest.Body.ClearBody;
            restRequest.Body.Add(GetCustomBody, TRESTContentType.ctAPPLICATION_JSON);

        end
        else
        begin
            Exit;
        end;

    end;

    try

        restRequest.Execute;

        if restResponse.StatusCode > 0 then
        begin

            FStatusCode:=restResponse.StatusCode;

            if (not String.IsNullOrEmpty(restResponse.Content)) and (not String.IsNullOrWhiteSpace(restResponse.Content)) then
            begin

                // -----------------------------------------------------------------------------------------------
                // Remove from recieved string content any \" leaving just single quote. This is necessary in case
                // WebServer returns content as string object instead of raw string or POCO, or by mistake returns
                // content serialized twice (it adds quotes to the start and end of the content), in such case it
                // requires further treatment on quotes inside string.
                // -----------------------------------------------------------------------------------------------

                FResponseContent:=restResponse.Content;
                FResponseContent:=FResponseContent.Replace('\"','"');

                // -----------------------------------
                // Remove single leading/ending quote.
                // -----------------------------------

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

    if (string.IsNullOrEmpty(QueryName) or string.IsNullOrEmpty(ParamValue)) then Exit;

    if (Assigned(queryList) and Assigned(paramList)) then
    begin
        queryList.Add(QueryName);
        paramList.Add(ParamValue);
    end;

end;


procedure TRESTful.ClearParameters;
begin

    if (Assigned(queryList) and Assigned(paramList)) then
    begin
        queryList.Clear;
        paramList.Clear;
    end;

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- HELPERS //


/// <summary>
/// Replace first and last character (single quote) for empty char.
/// </summary>
/// <remarks>
/// WARNING! JAVA starts counting string characters from zero, while Delphi and Swift count it from one.
/// </remarks>

procedure TRESTful.TrimContent(var TextStr: string);
begin
    if TextStr[1] = #34 then TextStr[1]:=#32;
    if TextStr[TextStr.Length - 1] = #34 then TextStr[TextStr.Length - 1]:=#32;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- GETTERS //


function TRESTful.GetExecuteError: string;
begin
    Result:=FExecuteError;
end;


function TRESTful.GetStatusCode: integer;
begin
    Result:=FStatusCode;
end;


function TRESTful.GetCustomBody: string;
begin
    Result:=FCustomBody;
end;


function TRESTful.GetContent: string;
begin
    Result:=FResponseContent;
end;


function TRESTful.GetHeaders: string;
begin
    if Assigned(restResponse) then Result:=restResponse.Headers.Text;
end;


function TRESTful.GethttpAuthUsername: string;
begin
    if Assigned(httpAuth) then Result:=httpAuth.Username;
end;


function TRESTful.GethttpAuthPassword: string;
begin
    if Assigned(httpAuth) then Result:=httpAuth.Password;
end;


function TRESTful.GetClientAccept: string;
begin
    if Assigned(restClient) then Result:=restClient.Accept;
end;


function TRESTful.GetClientAcceptCharset: string;
begin
    if Assigned(restClient) then Result:=restClient.AcceptCharset;
end;


function TRESTful.GetClientAllowCookies: boolean;
begin
    if Assigned(restClient) then Result:=restClient.AllowCookies else Result:=False;
end;


function TRESTful.GetClientAutoCreateParams: boolean;
begin
    if Assigned(restClient) then Result:=restClient.AutoCreateParams else Result:=False;
end;


function TRESTful.GetClientBaseURL: string;
begin
    if Assigned(restClient) then Result:=restClient.BaseURL;
end;


function TRESTful.GetClientContentType: string;
begin
    if Assigned(restClient) then restClient.ContentType;
end;


function TRESTful.GetClientFallbackCharsetEncoding: string;
begin
    if Assigned(restClient) then Result:=restClient.FallbackCharsetEncoding;
end;


function TRESTful.GetClientHandleRedirects: boolean;
begin
    if Assigned(restClient) then Result:=restClient.HandleRedirects else Result:=False;
end;


function TRESTful.GetClientRaiseExceptionOn500: boolean;
begin
    if Assigned(restClient) then Result:=restClient.RaiseExceptionOn500 else Result:=False;
end;


function TRESTful.GetClientSynchronizedEvents: boolean;
begin
    if Assigned(restClient) then Result:=restClient.SynchronizedEvents else Result:=False;
end;


function TRESTful.GetClientUserAgent: string;
begin
    if Assigned(restClient) then Result:=restClient.UserAgent;
end;


function TRESTful.GetRequestAccept: string;
begin
    if Assigned(restRequest) then Result:=restRequest.Accept;
end;


function TRESTful.GetRequestAcceptCharset: string;
begin
    if Assigned(restRequest) then Result:=restRequest.AcceptCharset;
end;


function TRESTful.GetRequestAutoCreateParams: boolean;
begin
    if Assigned(restRequest) then Result:=restRequest.AutoCreateParams else Result:=False;
end;


function TRESTful.GetRequestHandleRedirects: boolean;
begin
    if Assigned(restRequest) then Result:=restRequest.HandleRedirects else Result:=False;
end;


function TRESTful.GetRequestMethod: TRESTRequestMethod;
begin
    if Assigned(restRequest) then Result:=restRequest.Method else Result:=TRESTRequestMethod.rmGET;
end;


function TRESTful.GetRequestSynchronizedEvents: boolean;
begin
    if Assigned(restRequest) then Result:=restRequest.SynchronizedEvents else Result:=False;
end;


function TRESTful.GetRequestTimeout: integer;
begin
    if Assigned(restRequest) then Result:=restRequest.Timeout else Result:=30000 {30 seconds};
end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- SETTERS //


procedure TRESTful.SetCustomBody(NewValue: string);
begin
    FCustomBody:=NewValue;
end;


procedure TRESTful.SetClientAccept(NewValue: string);
begin
    if Assigned(restClient) then restClient.Accept:=NewValue;
end;


procedure TRESTful.SetClientAcceptCharset(NewValue: string);
begin
    if Assigned(restClient) then restClient.AcceptCharset:=NewValue;
end;


procedure TRESTful.SetClientAllowCookies(NewValue: boolean);
begin
    if Assigned(restClient) then restClient.AllowCookies:=NewValue;
end;


procedure TRESTful.SetClientAutoCreateParams(NewValue: boolean);
begin
    if Assigned(restClient) then restClient.AutoCreateParams:=NewValue;
end;


procedure TRESTful.SetClientBaseURL(NewValue: string);
begin
    if Assigned(restClient) then restClient.BaseURL:=NewValue;
end;


procedure TRESTful.SetClientContentType(NewValue: string);
begin
    if Assigned(restClient) then restClient.ContentType:=NewValue;
end;


procedure TRESTful.SetClientFallbackCharsetEncoding(NewValue: string);
begin
    if Assigned(restClient) then restClient.FallbackCharsetEncoding:=NewValue;
end;


procedure TRESTful.SetClientHandleRedirects(NewValue: boolean);
begin
    if Assigned(restClient) then restClient.HandleRedirects:=NewValue;
end;


procedure TRESTful.SetClientRaiseExceptionOn500(NewValue: boolean);
begin
    if Assigned(restClient) then restClient.RaiseExceptionOn500:=NewValue;
end;


procedure TRESTful.SetClientSynchronizedEvents(NewValue: boolean);
begin
    if Assigned(restClient) then restClient.SynchronizedEvents:=NewValue;
end;


procedure TRESTful.SetClientUserAgent(NewValue: string);
begin
    if Assigned(restClient) then restClient.UserAgent:=NewValue;
end;


procedure TRESTful.SetRequestAccept(NewValue: string);
begin
    if Assigned(restRequest) then restRequest.Accept:=NewValue;
end;


procedure TRESTful.SetRequestAcceptCharset(NewValue: string);
begin
    if Assigned(restRequest) then restRequest.AcceptCharset:=NewValue;
end;


procedure TRESTful.SetRequestAutoCreateParams(NewValue: boolean);
begin
    if Assigned(restRequest) then restRequest.AutoCreateParams:=NewValue;
end;


procedure TRESTful.SetRequestHandleRedirects(NewValue: boolean);
begin
    if Assigned(restRequest) then restRequest.HandleRedirects:=NewValue;
end;


procedure TRESTful.SetRequestMethod(NewValue: TRESTRequestMethod);
begin
    if Assigned(restRequest) then restRequest.Method:=NewValue;
end;


procedure TRESTful.SetRequestSynchronizedEvents(NewValue: boolean);
begin
    if Assigned(restRequest) then restRequest.SynchronizedEvents:=NewValue;
end;


procedure TRESTful.SetRequestTimeout(NewValue: integer);
begin
    if Assigned(restRequest) then restRequest.Timeout:=NewValue;
end;


end.

