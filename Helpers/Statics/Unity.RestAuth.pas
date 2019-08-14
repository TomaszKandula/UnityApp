unit Unity.RestAuth;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TRestAuth = class abstract
    public
        const apiUserName       = 'your_login';
        const apiPassword       = 'your_password';
        const restApiBaseUrl    = 'your_endpoint';
        const restAccept        = 'application/json, text/plain; q=0.9, text/html;q=0.8,';
        const restAcceptCharset = 'UTF-8, *;q=0.8';
        const restContentType   = 'application/json';
        const restEncoding      = 'UTF-8';
        const restUserAgent     = 'Cheers RESTClient/1.0';
    end;


implementation


end.

