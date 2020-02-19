unit Api.SendEmail;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


	TSendEmail = class
	strict private
        var FUserId:    string;
        var FSessionId: string;
        var FAliasName: string;
        var FFrom:      string;
        var FTo:        TArray<string>;
        var FCc:        TArray<string>;
        var FBcc:       TArray<string>;
        var FSubject:   string;
        var FHtmlBody:  string;
    public
        const _UserId    = 'UserId';
        const _SessionId = 'SessionId';
        const _AliasName = 'AliasName';
        const _From      = 'From';
        const _To        = 'To';
        const _Cc        = 'Cc';
        const _Bcc       = 'Bcc';
        const _Subject   = 'Subject';
        const _HtmlBody  = 'HtmlBody';
        property UserId:    string         read FUserId    write FUserId;
        property SessionId: string         read FSessionId write FSessionId;
        property AliasName: string         read FAliasName write FAliasName;
        property From:      string         read FFrom      write FFrom;
        property &To:       TArray<string> read FTo        write FTo;
        property Cc:        TArray<string> read FCc        write FCc;
        property Bcc:       TArray<string> read FBcc       write FBCc;
        property Subject:   string         read FSubject   write FSubject;
        property HtmlBody:  string         read FHtmlBody  write FHtmlBody;
	end;


implementation


end.

