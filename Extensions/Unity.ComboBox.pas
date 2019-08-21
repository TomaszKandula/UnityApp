unit Unity.ComboBox;


interface


uses
    Vcl.StdCtrls,
    Unity.Enums,
    Unity.Arrays;


type


    TComboBox = class(Vcl.StdCtrls.TComboBox)
    public
        procedure ListToComboBox(List: TALists; ColumnNo: cardinal; Position: TListSelection);
    end;


implementation


procedure TComboBox.ListToComboBox(List: TALists; ColumnNo: cardinal; Position: TListSelection);
begin

    Self.Clear;
    try

        for var iCNT:=0 to High(List) - 1 do
            Self.Items.Add(List[iCNT, ColumnNo]);

        case Position of
            First: Self.ItemIndex:=0;
            Last:  Self.ItemIndex:=Self.Items.Count - 1;
        end;

    except
        {Exception handler}
    end;

end;


end.

