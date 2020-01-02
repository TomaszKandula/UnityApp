unit Unity.ListView;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    Vcl.ComCtrls;


type


    /// <summary>
    /// Extended version of Vcl.StdCtrls.TListView visual component.
    /// </summary>
    TListView = class(Vcl.ComCtrls.TListView)
    published
        /// <summary>
        /// Allow to disable component painting. It will not process
        /// the events and will not repaint it, so it can be operated
        /// from worker thread safely.
        /// </summary>
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

