object SearchForm: TSearchForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity - Search'
  ClientHeight = 236
  ClientWidth = 386
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMain: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 376
    Height = 226
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 53
    ExplicitTop = 0
    ExplicitWidth = 627
    ExplicitHeight = 336
    object btnSearch: TSpeedButton
      Left = 293
      Top = 188
      Width = 73
      Height = 25
      Cursor = crHandPoint
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
      Spacing = 7
      OnClick = btnSearchClick
    end
    object btnUnhide: TSpeedButton
      Left = 191
      Top = 188
      Width = 73
      Height = 25
      Cursor = crHandPoint
      Caption = 'Unfold'
      Enabled = False
      Flat = True
      OnClick = btnUnhideClick
    end
    object PanelEditBox: TPanel
      Left = 10
      Top = 9
      Width = 356
      Height = 49
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object TextSearch: TLabel
        Left = 7
        Top = 4
        Width = 119
        Height = 13
        Caption = 'Customer name/number:'
      end
      object PanelEditSearch: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 20
        Width = 346
        Height = 24
        Margins.Left = 5
        Margins.Top = 20
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        ExplicitWidth = 318
        object EditSearch: TEdit
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 342
          Height = 20
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          BorderStyle = bsNone
          TabOrder = 0
          ExplicitWidth = 307
          ExplicitHeight = 26
        end
      end
    end
    object GroupOptions: TGroupBox
      Left = 10
      Top = 64
      Width = 175
      Height = 113
      Caption = 'Options'
      TabOrder = 1
      object TextWarn: TLabel
        Left = 7
        Top = 83
        Width = 149
        Height = 21
        Caption = '*This will replace existing filter.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object CaseSensitive: TCheckBox
        Left = 24
        Top = 31
        Width = 73
        Height = 17
        Cursor = crHandPoint
        Caption = 'Match case'
        TabOrder = 0
      end
      object ShowAll: TCheckBox
        Left = 24
        Top = 54
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Filter on found*'
        TabOrder = 1
      end
    end
    object GroupSearch: TGroupBox
      Left = 191
      Top = 64
      Width = 175
      Height = 113
      Caption = 'Search direction'
      TabOrder = 2
      object CheckUp: TRadioButton
        Left = 25
        Top = 32
        Width = 65
        Height = 17
        Cursor = crHandPoint
        Caption = 'Up'
        TabOrder = 0
      end
      object CheckDown: TRadioButton
        Left = 25
        Top = 55
        Width = 65
        Height = 17
        Cursor = crHandPoint
        Caption = 'Down'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
  end
end
