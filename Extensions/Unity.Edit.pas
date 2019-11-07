unit Unity.Edit;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    System.Classes,
    Vcl.StdCtrls,
    Vcl.Controls;


type


    TEdit = Class(Vcl.StdCtrls.TEdit)
    public

        var FAlignment: TAlignment;

        property  Alignment: TAlignment read FAlignment write SetAlignment;


        procedure SetAlignment(value: TAlignment);


        procedure CreateParams(var params: TCreateParams); override;

    end;


implementation


uses
    Winapi.Windows,
    Winapi.Messages;


procedure TEdit.CreateParams(var Params: TCreateParams);
begin

    inherited CreateParams(Params);

    case Alignment of
        taLeftJustify:  Params.Style:=Params.Style or ES_LEFT   and not ES_MULTILINE;
        taRightJustify: Params.Style:=Params.Style or ES_RIGHT  and not ES_MULTILINE;
        taCenter:       Params.Style:=Params.Style or ES_CENTER and not ES_MULTILINE;
    end;

end;


procedure TEdit.SetAlignment(value: TAlignment);
begin

    if FAlignment <> value then
    begin
        FAlignment:=value;
        RecreateWnd;
    end;

end;


end.

