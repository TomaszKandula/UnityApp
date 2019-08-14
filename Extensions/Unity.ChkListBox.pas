unit Unity.ChkListBox;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Vcl.CheckLst;


type


    TCheckListBox = class(Vcl.CheckLst.TCheckListBox)
    published
        procedure Freeze(PaintWnd: boolean);
    end;


implementation


uses
    Winapi.Windows,
    Winapi.Messages;


/// <summary>
/// Allow to freeze component during heavy duty task, or when we do not want to show control during updating.
/// </summary>

procedure TCheckListBox.Freeze(PaintWnd: Boolean);
begin

    if (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;

    if not (PaintWnd) then
    begin
        with Self do SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Self.Repaint;
    end;

end;


end.

