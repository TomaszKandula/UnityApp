unit Unity.ListView;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Vcl.ComCtrls;


type


    TListView = class(Vcl.ComCtrls.TListView)
    published
        procedure Freeze(PaintWnd: boolean);
    end;


implementation


uses
    Winapi.Windows,
    Winapi.Messages;


procedure TListView.Freeze(PaintWnd: Boolean);
begin

    if PaintWnd then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;

    if not PaintWnd then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Self.Repaint;
    end;

end;


end.

