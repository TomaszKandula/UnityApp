unit Unity.Chars;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TChars = class abstract
        const CrLf:        string = #13#10;
        const Cr:          char   = #13;
        const Lf:          char   = #10;
        const Backspace:   char   = #8;
        const Tab:         char   = #9;
        const Esc:         char   = #27;
        const Space:       char   = #32;
        const Quote:       char   = #39;
        const Comma:       char   = #44;
        const Point:       char   = #46;
        const SingleQuote: string = '''';
        const DoubleQuote: string = '''''';
    end;


implementation


end.

