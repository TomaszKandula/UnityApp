object ViewSearchForm: TViewSearchForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity'
  ClientHeight = 405
  ClientWidth = 683
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 663
    Height = 336
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object SearchBox: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 643
      Height = 316
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Caption = 'Search Address Book'
      TabOrder = 0
      object Text1: TLabel
        Left = 24
        Top = 59
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Text2: TLabel
        Left = 24
        Top = 32
        Width = 37
        Height = 13
        Caption = 'Number'
      end
      object Text3: TLabel
        Left = 24
        Top = 86
        Width = 24
        Height = 13
        Caption = 'Email'
      end
      object Text4: TLabel
        Left = 24
        Top = 113
        Width = 55
        Height = 13
        Caption = 'Estatement'
      end
      object Text5: TLabel
        Left = 24
        Top = 139
        Width = 35
        Height = 13
        Caption = 'Phones'
      end
      object Splitter: TBevel
        Left = 24
        Top = 176
        Width = 593
        Height = 17
        Shape = bsTopLine
      end
      object Text6: TLabel
        Left = 24
        Top = 199
        Width = 47
        Height = 13
        Caption = 'User Alias'
      end
      object Text7: TLabel
        Left = 24
        Top = 225
        Width = 41
        Height = 13
        Caption = 'Co Code'
      end
      object Text8: TLabel
        Left = 24
        Top = 252
        Width = 29
        Height = 13
        Caption = 'Agent'
      end
      object Text9: TLabel
        Left = 24
        Top = 279
        Width = 36
        Height = 13
        Caption = 'Division'
      end
      object CheckBoxName: TCheckBox
        Left = 457
        Top = 58
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 0
        OnClick = CheckBoxNameClick
      end
      object CheckBoxNumber: TCheckBox
        Left = 457
        Top = 31
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CheckBoxNumberClick
      end
      object CheckBoxEmail: TCheckBox
        Left = 457
        Top = 85
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 2
        OnClick = CheckBoxEmailClick
      end
      object CheckBoxEstatement: TCheckBox
        Left = 457
        Top = 112
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 3
        OnClick = CheckBoxEstatementClick
      end
      object CheckBoxPhones: TCheckBox
        Left = 457
        Top = 138
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 4
        OnClick = CheckBoxPhonesClick
      end
      object PanelEditNumber: TPanel
        Left = 208
        Top = 26
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 5
        object EditNumber: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BorderStyle = bsNone
          Color = clCream
          NumbersOnly = True
          TabOrder = 0
        end
      end
      object PanelEditName: TPanel
        Left = 208
        Top = 53
        Width = 217
        Height = 26
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 6
        object EditName: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 22
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          TabOrder = 0
        end
      end
      object PanelEmail: TPanel
        Left = 208
        Top = 80
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 7
        object EditEmail: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 211
          Height = 19
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          TabOrder = 0
        end
      end
      object PanelPhones: TPanel
        Left = 208
        Top = 134
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 8
        object EditPhones: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          TabOrder = 0
        end
      end
      object PanelEstatement: TPanel
        Left = 208
        Top = 107
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 9
        object EditEstatement: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          TabOrder = 0
        end
      end
      object CheckBoxUserAlias: TCheckBox
        Left = 457
        Top = 197
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 10
        OnClick = CheckBoxUserAliasClick
      end
      object CheckBoxCoCode: TCheckBox
        Left = 457
        Top = 224
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 11
        OnClick = CheckBoxCoCodeClick
      end
      object CheckBoxAgent: TCheckBox
        Left = 457
        Top = 251
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 12
        OnClick = CheckBoxAgentClick
      end
      object CheckBoxDivision: TCheckBox
        Left = 457
        Top = 278
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 13
        OnClick = CheckBoxDivisionClick
      end
      object PanelUserAlias: TPanel
        Left = 210
        Top = 193
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 14
        object EditUserAlias: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          TabOrder = 0
        end
      end
      object PanelCoCode: TPanel
        Left = 210
        Top = 219
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 15
        object EditCoCode: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          NumbersOnly = True
          TabOrder = 0
        end
      end
      object PanelAgent: TPanel
        Left = 210
        Top = 246
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 16
        object EditAgent: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          NumbersOnly = True
          TabOrder = 0
        end
      end
      object PanelDivision: TPanel
        Left = 210
        Top = 273
        Width = 217
        Height = 25
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 17
        object EditDivision: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 213
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWhite
          Enabled = False
          NumbersOnly = True
          TabOrder = 0
        end
      end
      object CheckBoxNameEqual: TCheckBox
        Left = 113
        Top = 58
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Equal'
        Checked = True
        State = cbChecked
        TabOrder = 18
        OnClick = CheckBoxNameEqualClick
      end
      object CheckBoxEmailEqual: TCheckBox
        Left = 113
        Top = 85
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Equal'
        Checked = True
        State = cbChecked
        TabOrder = 19
        OnClick = CheckBoxEmailEqualClick
      end
      object CheckBoxEstatEqual: TCheckBox
        Left = 113
        Top = 112
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Equal'
        Checked = True
        State = cbChecked
        TabOrder = 20
        OnClick = CheckBoxEstatEqualClick
      end
      object CheckBoxNameCase: TCheckBox
        Left = 542
        Top = 57
        Width = 75
        Height = 17
        Cursor = crHandPoint
        Caption = 'Match Case'
        TabOrder = 21
      end
      object CheckBoxEmailCase: TCheckBox
        Left = 542
        Top = 85
        Width = 75
        Height = 17
        Cursor = crHandPoint
        Caption = 'Match Case'
        TabOrder = 22
      end
      object CheckBoxEstatCase: TCheckBox
        Left = 542
        Top = 112
        Width = 75
        Height = 17
        Cursor = crHandPoint
        Caption = 'Match Case'
        TabOrder = 23
      end
      object CheckBoxAliasEqual: TCheckBox
        Left = 113
        Top = 198
        Width = 56
        Height = 17
        Cursor = crHandPoint
        Caption = 'Equal'
        Checked = True
        State = cbChecked
        TabOrder = 24
        OnClick = CheckBoxAliasEqualClick
      end
      object CheckBoxAliasCase: TCheckBox
        Left = 542
        Top = 198
        Width = 75
        Height = 17
        Cursor = crHandPoint
        Caption = 'Match Case'
        TabOrder = 25
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 356
    Width = 683
    Height = 49
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object btnSearch: TSpeedButton
      AlignWithMargins = True
      Left = 553
      Top = 12
      Width = 110
      Height = 25
      Cursor = crHandPoint
      Margins.Left = 15
      Margins.Top = 12
      Margins.Right = 20
      Margins.Bottom = 12
      Align = alRight
      Caption = 'Search'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFD4DEED1A4B9E829CCAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFDEE5F1073B952158A71C54A6FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFF2F80B3E
        962157A70C51A7DBE5F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFCFDFE1243991E55A61154A9E5ECF5FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FCFF18479B1B55A71859
        ABF4F7FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0E9E1E7DDD0E2D6C7E2D6C7EC
        E4D8FFFFFFDDE0E6084BA22168BBFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFBF9F7
        E2D6C8E3CBA7E4BD81E5B977E4BE84E3CEAEE2D6C6D1B392D9DADDFBFCFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFE2D6C8E3BD84E5B365E4B56CE4B66DE4B56CE5
        B263E5C595E2D6C6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0EAE2DFC299
        E5B365E4B66FE5B770E5B770E5B770E5B770E5B263E3CDAEECE4D9FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFE8DED2DFB06BE7BB79E5B771E4B66FE5B770E5B770E5
        B770E5B56CE4BE84E2D7C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6DDD2DDA758
        EBC592E7BC7EE4B66DE5B770E5B770E5B770E5B66DE4B977E2D6C7FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFECE5DFDBA85EEAC289EBC593E4B56CE4B66DE4B66FE5
        B770E5B56CE4BD81E7DDD0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F0EEDAB787
        E3B164ECC899EBC593E7BC7EE5B771E4B66FE5B365E3CBA7F0E9E1FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFE6DDD4D7A45EE3B064EAC28AEBC693E7BC7AE5
        B365E3BD84E2D6C8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFBFB
        E7DED5DAB787DAA75EDDA758DEAF6ADFC197E2D6C7FBF9F7FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F1EFECE5E0E6DDD2E8DED2F0
        EAE3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Spacing = 10
      OnClick = btnSearchClick
      ExplicitLeft = 443
      ExplicitTop = 13
    end
    object btnCancel: TSpeedButton
      AlignWithMargins = True
      Left = 428
      Top = 12
      Width = 110
      Height = 25
      Cursor = crHandPoint
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 12
      Align = alRight
      Caption = 'Cancel'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFF6362FFB4B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB4B4FF6362
        FFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFF5A59FF6160FF5251FFABAAFFFFFFFFFF
        FFFFFFFFFFFFFFFFABAAFF5251FF6160FF5A59FFFCFCFFFFFFFFFFFFFF5A59FF
        6160FF6564FF6564FF5251FFABAAFFFFFFFFFFFFFFABAAFF5251FF6564FF6564
        FF6160FF5A59FFFFFFFF6362FF6160FF6564FF6564FF6564FF6564FF5251FFB5
        B4FFB5B4FF5251FF6564FF6564FF6564FF6564FF6160FF6362FFB4B4FF5251FF
        6564FF6564FF6564FF6564FF6564FF5B5AFF5B5AFF6564FF6564FF6564FF6564
        FF6564FF5251FFB4B4FFFFFFFFABAAFF5251FF6564FF6564FF6564FF6564FF65
        64FF6564FF6564FF6564FF6564FF6564FF5251FFABAAFFFFFFFFFFFFFFFFFFFF
        ABAAFF5251FF6564FF6564FF6564FF6564FF6564FF6564FF6564FF6564FF5251
        FFABAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB5B4FF5B5AFF6564FF6564FF65
        64FF6564FF6564FF6564FF5B5AFFB5B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFB5B4FF5B5AFF6564FF6564FF6564FF6564FF6564FF6564FF5B5AFFB5B4
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFABAAFF5251FF6564FF6564FF6564FF65
        64FF6564FF6564FF6564FF6564FF5251FFABAAFFFFFFFFFFFFFFFFFFFFABAAFF
        5251FF6564FF6564FF6564FF6564FF6564FF6564FF6564FF6564FF6564FF6564
        FF5251FFABAAFFFFFFFFB4B4FF5251FF6564FF6564FF6564FF6564FF6564FF5B
        5AFF5B5AFF6564FF6564FF6564FF6564FF6564FF5251FFB4B4FF6362FF6160FF
        6564FF6564FF6564FF6564FF5251FFB5B4FFB5B4FF5251FF6564FF6564FF6564
        FF6564FF6160FF6362FFFFFFFF5A59FF6160FF6564FF6564FF5251FFABAAFFFF
        FFFFFFFFFFABAAFF5251FF6564FF6564FF6160FF5A59FFFFFFFFFFFFFFFCFCFF
        5A59FF6160FF5251FFABAAFFFFFFFFFFFFFFFFFFFFFFFFFFABAAFF5251FF6160
        FF5A59FFFCFCFFFFFFFFFFFFFFFFFFFFFFFFFF6362FFB4B4FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFB4B4FF6362FFFFFFFFFFFFFFFFFFFF}
      Spacing = 10
      OnClick = btnCancelClick
      ExplicitHeight = 27
    end
  end
end
