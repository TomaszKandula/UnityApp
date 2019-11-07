unit Unity.ComboBox;

// ------------------------------------------------------------
// Extension unit for application. Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ------------------------------------------------------------

interface


uses
    Vcl.StdCtrls,
    Unity.Enums,
    Unity.Arrays;


type


    /// <summary>
    /// Extended version of Vcl.StdCtrls.TComboBox visual component.
    /// </summary>
    TComboBox = class(Vcl.StdCtrls.TComboBox)
    public

        /// <summary>
        /// Simple wrapper of method to put two dimensional string array into combo box.
        /// </summary>
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

