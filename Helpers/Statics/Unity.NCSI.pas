unit Unity.NCSI;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TNCSI = class abstract
        const HTTPREQUEST_SETCREDENTIALS_FOR_SERVER   = 0;
        const HTTPREQUEST_SETCREDENTIALS_FOR_PROXY    = 1;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW    = 0;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM = 1;
        const WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH   = 2;
        const MAX_CHECK_ATTEMPTS = 6;
        const ncsiWww:  string = 'http://www.msftncsi.com/';
        const ncsiFile: string = 'ncsi.txt';
        const ncsiGet:  string = 'GET';
        const ncsiHead: string = 'HEAD';
    end;


implementation


end.

