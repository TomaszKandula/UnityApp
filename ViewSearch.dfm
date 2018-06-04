object ViewSearchForm: TViewSearchForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity'
  ClientHeight = 271
  ClientWidth = 498
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 478
    Height = 206
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 64
    ExplicitTop = 216
    ExplicitWidth = 665
    ExplicitHeight = 361
    object SearchBox: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 458
      Height = 186
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Caption = 'Search Address Book'
      TabOrder = 0
      ExplicitLeft = 32
      ExplicitTop = 16
      ExplicitWidth = 481
      ExplicitHeight = 233
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
        Top = 140
        Width = 35
        Height = 13
        Caption = 'Phones'
      end
      object EditName: TEdit
        Left = 136
        Top = 56
        Width = 217
        Height = 21
        Enabled = False
        TabOrder = 0
      end
      object EditNumber: TEdit
        Left = 136
        Top = 29
        Width = 217
        Height = 21
        NumbersOnly = True
        TabOrder = 1
      end
      object EditEmail: TEdit
        Left = 136
        Top = 83
        Width = 217
        Height = 21
        Enabled = False
        TabOrder = 2
      end
      object EditEstatement: TEdit
        Left = 136
        Top = 110
        Width = 217
        Height = 21
        Enabled = False
        TabOrder = 3
      end
      object EditPhones: TEdit
        Left = 136
        Top = 137
        Width = 217
        Height = 21
        Enabled = False
        TabOrder = 4
      end
      object CheckBoxName: TCheckBox
        Left = 385
        Top = 58
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 5
        OnClick = CheckBoxNameClick
      end
      object CheckBoxNumber: TCheckBox
        Left = 385
        Top = 31
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = CheckBoxNumberClick
      end
      object CheckBoxEmail: TCheckBox
        Left = 385
        Top = 85
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 7
        OnClick = CheckBoxEmailClick
      end
      object CheckBoxEstatement: TCheckBox
        Left = 385
        Top = 112
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 8
        OnClick = CheckBoxEstatementClick
      end
      object CheckBoxPhones: TCheckBox
        Left = 385
        Top = 139
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Include'
        TabOrder = 9
        OnClick = CheckBoxPhonesClick
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 226
    Width = 498
    Height = 45
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 190
    ExplicitWidth = 433
    object btnSearch: TSpeedButton
      AlignWithMargins = True
      Left = 368
      Top = 12
      Width = 110
      Height = 21
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
      ExplicitHeight = 25
    end
    object btnCancel: TSpeedButton
      AlignWithMargins = True
      Left = 243
      Top = 12
      Width = 110
      Height = 21
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
      ExplicitLeft = 327
      ExplicitTop = 13
      ExplicitHeight = 25
    end
  end
end
