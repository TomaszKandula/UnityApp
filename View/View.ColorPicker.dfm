object ColorsForm: TColorsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 337
  ClientWidth = 313
  Color = 15855854
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 303
    Height = 327
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clWhite
    ExplicitTop = -106
    ExplicitWidth = 528
    ExplicitHeight = 459
  end
  object ColorBox_Today: TGroupBox
    Left = 16
    Top = 16
    Width = 281
    Height = 97
    Caption = 'Today Follow-Up'
    Color = clWhite
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    object ColorBox1: TShape
      Left = 136
      Top = 61
      Width = 25
      Height = 20
      Pen.Color = clSilver
    end
    object btnToday: TSpeedButton
      Left = 192
      Top = 24
      Width = 75
      Height = 57
      Cursor = crHandPoint
      Caption = 'Pick'
      Flat = True
      Glyph.Data = {
        360C0000424D360C000000000000360000002800000020000000200000000100
        180000000000000C0000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAAB3EB6B7ADC4355
        D32033DFABD23BA9C957BBD47BD6E6B2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7180DE1127C71E33CA253ACC2A3E
        CD1E30DEACD337A0C3419DC23D9AC03794BB2ABBD57DFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFD6DBF5192FC92136CB2C40CD2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A0C3439CC03A96BD30E7F0D4FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC2C9F10D25C62A3ECD2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A0C34293BB28DCEAC0FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFE2E6F80E25C62C40CD2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A0C34393BB28EB
        F3DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF2339CC293DCD2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A1C4449FC34199
        BF36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF95A0E61C32CA2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A1C444A1C4449A
        C037C8DD96FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF1E34CA2A3FCD2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2C40CD2439
        CD0F23DEA7D12B9EC33BA0C343A1C444A1C444A1C444A1C444A1C444A1C444A0
        C34296BE32FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        CED4F3182DC92D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2C40CD182FCD6E79
        D0B2B2D8D3D6B3B8CB7F9AC133A0C343A1C444A1C444A1C444A1C444A1C444A1
        C44498BE32E2EDC9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        8C98E42035CB2D41CE2D41CE2D41CE2D41CE2D41CE2C40CE1930CDAAACD3EFE7
        D5E1DBD5DDD6DAE1D8E5CED2B59AC132A0C343A1C444A1C444A1C444A1C444A1
        C4449CC039C5DB8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        6471DA2335CA2B3CCC2B3CCC2B3CCC2B3CCC2B3CCC2538CC4E5BCEF2EAD5DBD6
        D5DBD6D5DBD6D5DBD6D5E2D8E7B1CA69A0C63EA3C744A3C744A3C744A3C744A3
        C7449FC53DB5D26DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        5B79DD2E52D33557D53557D53557D53557D53557D52B50D58A97D4E5DDD4DBD6
        D5DBD6D5DBD6D5DBD6D5DFD8DDBEC89790AF3A95B24495B24495B24495B24495
        B24492B03FA0BA5DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        86E7FF61E0FF67E2FF67E2FF67E2FF67E2FF67E2FF60E2FF98DCEDE4D6D1DBD6
        D5DBD6D5DBD6D5DBD6D5E4E0E095877F4A3322523B2C523B2C523B2C523B2C52
        3B2C4D36266C5B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        97E2FC5AD1FB62D4FC62D4FC62D4FC62D4FC62D4FC60D3FC5ED3FDE9D6D0DDD6
        D4DBD6D5DBD6D5DCD8D7EDEAEC59482F57452C59482F59482F59482F59482F59
        482F524026877C6BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        BCECFD56D0FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC5CD3FD75D4F5E4D6
        D2E7D6D0E8E4E5E7E3E473645052402659482F59482F59482F59482F59482F59
        482F4E3B21AEA89DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        F8FDFE4CCEFB61D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC5DD3FD56D3
        FF7DE1FF755F484D3B1F53422859482F59482F59482F59482F59482F59482F59
        482F443014EDEDECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF78D9FC5DD2FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC61D3
        FC5FE4FF53381A54422858472E59482F59482F59482F59482F59482F59482F54
        4329665842FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFE3F7FE4CCEFB61D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF53381A8476654E3C21513F2556452B59482F59482F59482F59482F44
        3014D7D4D0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF91E0FC57D1FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF5337197E705DCFC9C592867762513A4A381C57462C59482F4F3C217F
        7463FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF6AD6FC5AD1FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF573C1F483518C6BFBAE8E4E5EDE9ECBFB8B25442294D3B205B4C34FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF6DD6FC55D0FB61D3FC62D4FC62D4FC62D4FC62D4
        FC63E4FF573C1F503E239C9285E3DFDFDBD6D5E1DDDDE0DBDB71634EFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FE4FD4BCDFB5AD2FB61D3FB62D4FC62D4
        FC63E4FF573C1F57452B57462DECE8EADBD6D5DBD6D5DCD7D6E6E2E3FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4FBFE91E0FC50CFFB53CFFB57D1
        FB59E2FF4E32134D3A1F402C0F827565E1DCDDDBD6D5DBD6D5DAD5D4D8D3D2FE
        FEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBF5FEB9EB
        FDA9EFFFA29586B4ADA3D7D4D0FFFFFFEAE7E7D7D1D0DBD6D5DBD6D5DAD5D4D8
        D3D2FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DBD6D5DBD6D5DA
        D5D4D8D3D2FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DBD6D5DB
        D6D5DAD5D4E3DFE0C7BEB5DCD7D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DB
        D6D5E4E1E2B4AA9F735F45FBFAF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5E1
        DDDEB5AAA084715A554329897E6CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB8
        AEA37662495B4A3156452C4A371C887C6AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA7
        9A8BD6D1CA48361A56452B59482F473418A49A8DFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFF0EFEC47341855432A503F2473644FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFF4F3F163533C5C4B32E8E5E2FFFFFFFFFFFF}
      Spacing = 0
      OnClick = btnTodayClick
    end
    object ColorPreview1: TLabel
      Left = 84
      Top = 63
      Width = 42
      Height = 13
      Caption = 'Preview:'
    end
    object ColorList1: TComboBox
      Left = 16
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Font Color'
      OnSelect = ColorList1Select
      Items.Strings = (
        'Font Color'
        'Background Color')
    end
  end
  object ColorBox_Past: TGroupBox
    Left = 16
    Top = 119
    Width = 281
    Height = 97
    Caption = 'Past Follow-Up'
    Color = clWhite
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    object ColorBox2: TShape
      Left = 136
      Top = 61
      Width = 25
      Height = 20
      Pen.Color = clSilver
    end
    object btnPast: TSpeedButton
      Left = 192
      Top = 24
      Width = 75
      Height = 57
      Cursor = crHandPoint
      Caption = 'Pick'
      Flat = True
      Glyph.Data = {
        360C0000424D360C000000000000360000002800000020000000200000000100
        180000000000000C0000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAAB3EB6B7ADC4355
        D32033DFABD23BA9C957BBD47BD6E6B2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7180DE1127C71E33CA253ACC2A3E
        CD1E30DEACD337A0C3419DC23D9AC03794BB2ABBD57DFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFD6DBF5192FC92136CB2C40CD2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A0C3439CC03A96BD30E7F0D4FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC2C9F10D25C62A3ECD2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A0C34293BB28DCEAC0FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFE2E6F80E25C62C40CD2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A0C34393BB28EB
        F3DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF2339CC293DCD2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A1C4449FC34199
        BF36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF95A0E61C32CA2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A1C444A1C4449A
        C037C8DD96FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF1E34CA2A3FCD2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2C40CD2439
        CD0F23DEA7D12B9EC33BA0C343A1C444A1C444A1C444A1C444A1C444A1C444A0
        C34296BE32FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        CED4F3182DC92D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2C40CD182FCD6E79
        D0B2B2D8D3D6B3B8CB7F9AC133A0C343A1C444A1C444A1C444A1C444A1C444A1
        C44498BE32E2EDC9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        8C98E42035CB2D41CE2D41CE2D41CE2D41CE2D41CE2C40CE1930CDAAACD3EFE7
        D5E1DBD5DDD6DAE1D8E5CED2B59AC132A0C343A1C444A1C444A1C444A1C444A1
        C4449CC039C5DB8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        6471DA2335CA2B3CCC2B3CCC2B3CCC2B3CCC2B3CCC2538CC4E5BCEF2EAD5DBD6
        D5DBD6D5DBD6D5DBD6D5E2D8E7B1CA69A0C63EA3C744A3C744A3C744A3C744A3
        C7449FC53DB5D26DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        5B79DD2E52D33557D53557D53557D53557D53557D52B50D58A97D4E5DDD4DBD6
        D5DBD6D5DBD6D5DBD6D5DFD8DDBEC89790AF3A95B24495B24495B24495B24495
        B24492B03FA0BA5DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        86E7FF61E0FF67E2FF67E2FF67E2FF67E2FF67E2FF60E2FF98DCEDE4D6D1DBD6
        D5DBD6D5DBD6D5DBD6D5E4E0E095877F4A3322523B2C523B2C523B2C523B2C52
        3B2C4D36266C5B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        97E2FC5AD1FB62D4FC62D4FC62D4FC62D4FC62D4FC60D3FC5ED3FDE9D6D0DDD6
        D4DBD6D5DBD6D5DCD8D7EDEAEC59482F57452C59482F59482F59482F59482F59
        482F524026877C6BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        BCECFD56D0FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC5CD3FD75D4F5E4D6
        D2E7D6D0E8E4E5E7E3E473645052402659482F59482F59482F59482F59482F59
        482F4E3B21AEA89DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        F8FDFE4CCEFB61D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC5DD3FD56D3
        FF7DE1FF755F484D3B1F53422859482F59482F59482F59482F59482F59482F59
        482F443014EDEDECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF78D9FC5DD2FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC61D3
        FC5FE4FF53381A54422858472E59482F59482F59482F59482F59482F59482F54
        4329665842FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFE3F7FE4CCEFB61D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF53381A8476654E3C21513F2556452B59482F59482F59482F59482F44
        3014D7D4D0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF91E0FC57D1FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF5337197E705DCFC9C592867762513A4A381C57462C59482F4F3C217F
        7463FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF6AD6FC5AD1FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF573C1F483518C6BFBAE8E4E5EDE9ECBFB8B25442294D3B205B4C34FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF6DD6FC55D0FB61D3FC62D4FC62D4FC62D4FC62D4
        FC63E4FF573C1F503E239C9285E3DFDFDBD6D5E1DDDDE0DBDB71634EFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FE4FD4BCDFB5AD2FB61D3FB62D4FC62D4
        FC63E4FF573C1F57452B57462DECE8EADBD6D5DBD6D5DCD7D6E6E2E3FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4FBFE91E0FC50CFFB53CFFB57D1
        FB59E2FF4E32134D3A1F402C0F827565E1DCDDDBD6D5DBD6D5DAD5D4D8D3D2FE
        FEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBF5FEB9EB
        FDA9EFFFA29586B4ADA3D7D4D0FFFFFFEAE7E7D7D1D0DBD6D5DBD6D5DAD5D4D8
        D3D2FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DBD6D5DBD6D5DA
        D5D4D8D3D2FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DBD6D5DB
        D6D5DAD5D4E3DFE0C7BEB5DCD7D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DB
        D6D5E4E1E2B4AA9F735F45FBFAF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5E1
        DDDEB5AAA084715A554329897E6CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB8
        AEA37662495B4A3156452C4A371C887C6AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA7
        9A8BD6D1CA48361A56452B59482F473418A49A8DFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFF0EFEC47341855432A503F2473644FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFF4F3F163533C5C4B32E8E5E2FFFFFFFFFFFF}
      Spacing = 0
      OnClick = btnPastClick
    end
    object ColorPreview2: TLabel
      Left = 84
      Top = 63
      Width = 42
      Height = 13
      Caption = 'Preview:'
    end
    object ColorList2: TComboBox
      Left = 16
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Font Color'
      OnSelect = ColorList2Select
      Items.Strings = (
        'Font Color'
        'Background Color')
    end
  end
  object ColorBox_Future: TGroupBox
    Left = 16
    Top = 222
    Width = 281
    Height = 97
    Caption = 'Future Follow-Up'
    Color = clWhite
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
    object ColorBox3: TShape
      Left = 136
      Top = 61
      Width = 25
      Height = 20
      Pen.Color = clSilver
    end
    object btnFuture: TSpeedButton
      Left = 192
      Top = 24
      Width = 75
      Height = 57
      Cursor = crHandPoint
      Caption = 'Pick'
      Flat = True
      Glyph.Data = {
        360C0000424D360C000000000000360000002800000020000000200000000100
        180000000000000C0000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAAB3EB6B7ADC4355
        D32033DFABD23BA9C957BBD47BD6E6B2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7180DE1127C71E33CA253ACC2A3E
        CD1E30DEACD337A0C3419DC23D9AC03794BB2ABBD57DFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFD6DBF5192FC92136CB2C40CD2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A0C3439CC03A96BD30E7F0D4FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC2C9F10D25C62A3ECD2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A0C34293BB28DCEAC0FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFE2E6F80E25C62C40CD2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A0C34393BB28EB
        F3DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF2339CC293DCD2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A1C4449FC34199
        BF36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF95A0E61C32CA2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2D41
        CE1E30DEADD337A1C443A1C444A1C444A1C444A1C444A1C444A1C444A1C4449A
        C037C8DD96FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF1E34CA2A3FCD2D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2C40CD2439
        CD0F23DEA7D12B9EC33BA0C343A1C444A1C444A1C444A1C444A1C444A1C444A0
        C34296BE32FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        CED4F3182DC92D41CE2D41CE2D41CE2D41CE2D41CE2D41CE2C40CD182FCD6E79
        D0B2B2D8D3D6B3B8CB7F9AC133A0C343A1C444A1C444A1C444A1C444A1C444A1
        C44498BE32E2EDC9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        8C98E42035CB2D41CE2D41CE2D41CE2D41CE2D41CE2C40CE1930CDAAACD3EFE7
        D5E1DBD5DDD6DAE1D8E5CED2B59AC132A0C343A1C444A1C444A1C444A1C444A1
        C4449CC039C5DB8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        6471DA2335CA2B3CCC2B3CCC2B3CCC2B3CCC2B3CCC2538CC4E5BCEF2EAD5DBD6
        D5DBD6D5DBD6D5DBD6D5E2D8E7B1CA69A0C63EA3C744A3C744A3C744A3C744A3
        C7449FC53DB5D26DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        5B79DD2E52D33557D53557D53557D53557D53557D52B50D58A97D4E5DDD4DBD6
        D5DBD6D5DBD6D5DBD6D5DFD8DDBEC89790AF3A95B24495B24495B24495B24495
        B24492B03FA0BA5DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        86E7FF61E0FF67E2FF67E2FF67E2FF67E2FF67E2FF60E2FF98DCEDE4D6D1DBD6
        D5DBD6D5DBD6D5DBD6D5E4E0E095877F4A3322523B2C523B2C523B2C523B2C52
        3B2C4D36266C5B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        97E2FC5AD1FB62D4FC62D4FC62D4FC62D4FC62D4FC60D3FC5ED3FDE9D6D0DDD6
        D4DBD6D5DBD6D5DCD8D7EDEAEC59482F57452C59482F59482F59482F59482F59
        482F524026877C6BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        BCECFD56D0FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC5CD3FD75D4F5E4D6
        D2E7D6D0E8E4E5E7E3E473645052402659482F59482F59482F59482F59482F59
        482F4E3B21AEA89DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        F8FDFE4CCEFB61D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC5DD3FD56D3
        FF7DE1FF755F484D3B1F53422859482F59482F59482F59482F59482F59482F59
        482F443014EDEDECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF78D9FC5DD2FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC61D3
        FC5FE4FF53381A54422858472E59482F59482F59482F59482F59482F59482F54
        4329665842FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFE3F7FE4CCEFB61D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF53381A8476654E3C21513F2556452B59482F59482F59482F59482F44
        3014D7D4D0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF91E0FC57D1FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF5337197E705DCFC9C592867762513A4A381C57462C59482F4F3C217F
        7463FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF6AD6FC5AD1FB62D4FC62D4FC62D4FC62D4FC62D4FC62D4
        FC63E4FF573C1F483518C6BFBAE8E4E5EDE9ECBFB8B25442294D3B205B4C34FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF6DD6FC55D0FB61D3FC62D4FC62D4FC62D4FC62D4
        FC63E4FF573C1F503E239C9285E3DFDFDBD6D5E1DDDDE0DBDB71634EFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FE4FD4BCDFB5AD2FB61D3FB62D4FC62D4
        FC63E4FF573C1F57452B57462DECE8EADBD6D5DBD6D5DCD7D6E6E2E3FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4FBFE91E0FC50CFFB53CFFB57D1
        FB59E2FF4E32134D3A1F402C0F827565E1DCDDDBD6D5DBD6D5DAD5D4D8D3D2FE
        FEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBF5FEB9EB
        FDA9EFFFA29586B4ADA3D7D4D0FFFFFFEAE7E7D7D1D0DBD6D5DBD6D5DAD5D4D8
        D3D2FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DBD6D5DBD6D5DA
        D5D4D8D3D2FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DBD6D5DB
        D6D5DAD5D4E3DFE0C7BEB5DCD7D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5D7D1D0DB
        D6D5E4E1E2B4AA9F735F45FBFAF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E5E1
        DDDEB5AAA084715A554329897E6CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB8
        AEA37662495B4A3156452C4A371C887C6AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA7
        9A8BD6D1CA48361A56452B59482F473418A49A8DFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFF0EFEC47341855432A503F2473644FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFF4F3F163533C5C4B32E8E5E2FFFFFFFFFFFF}
      Spacing = 0
      OnClick = btnFutureClick
    end
    object ColorPreview3: TLabel
      Left = 84
      Top = 63
      Width = 42
      Height = 13
      Caption = 'Preview:'
    end
    object ColorList3: TComboBox
      Left = 16
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Font Color'
      OnSelect = ColorList3Select
      Items.Strings = (
        'Font Color'
        'Background Color')
    end
  end
  object ColorDialog: TColorDialog
    Color = clCream
    Options = [cdFullOpen, cdAnyColor]
    Left = 200
    Top = 8
  end
end
