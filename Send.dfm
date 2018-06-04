object SendForm: TSendForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 524
  ClientWidth = 559
  Color = 15527148
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
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 539
    Height = 462
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 5
    ExplicitTop = 5
    ExplicitWidth = 535
    ExplicitHeight = 467
    object Shape_Banks: TShape
      Left = 16
      Top = 383
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Business: TShape
      Left = 16
      Top = 14
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Customer: TShape
      Left = 16
      Top = 54
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Footer: TShape
      Left = 16
      Top = 423
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Invoices: TShape
      Left = 16
      Top = 303
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Terms: TShape
      Left = 16
      Top = 343
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Text_Banks: TLabel
      Left = 22
      Top = 389
      Width = 151
      Height = 13
      Caption = 'BANK ACCOUNTS (IF GIVEN)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Text_Business: TLabel
      Left = 437
      Top = 20
      Width = 76
      Height = 13
      Caption = 'LBU ADDRESS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Text_Customer: TLabel
      Left = 22
      Top = 60
      Width = 116
      Height = 13
      Caption = 'CUSTOMER ADDRESS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Text_Footer: TLabel
      Left = 22
      Top = 429
      Width = 43
      Height = 13
      Caption = 'FOOTER'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Text_Invoices: TLabel
      Left = 22
      Top = 308
      Width = 73
      Height = 13
      Caption = 'INVOICE LIST'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Text_Terms: TLabel
      Left = 22
      Top = 349
      Width = 93
      Height = 13
      Caption = 'PAYMENT TERMS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Text1: TLabel
      Left = 18
      Top = 92
      Width = 90
      Height = 13
      Caption = 'Custom salutation:'
    end
    object Text2: TLabel
      Left = 18
      Top = 147
      Width = 85
      Height = 13
      Caption = 'Custom message:'
    end
    object cbAddOverdue: TCheckBox
      Left = 424
      Top = 307
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Overdue only'
      TabOrder = 2
    end
    object PanelMessage: TPanel
      Left = 16
      Top = 166
      Width = 505
      Height = 123
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object Text_Message: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 501
        Height = 119
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BorderStyle = bsNone
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxLength = 255
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object PanelSalutation: TPanel
      Left = 16
      Top = 111
      Width = 505
      Height = 25
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Text_Salut: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 501
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BorderStyle = bsNone
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxLength = 255
        ParentFont = False
        TabOrder = 0
        WantReturns = False
        WordWrap = False
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 482
    Width = 559
    Height = 42
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    ExplicitLeft = 5
    ExplicitTop = 477
    ExplicitWidth = 535
    object Text_Warn: TLabel
      Left = 16
      Top = 12
      Width = 274
      Height = 13
      Caption = '*Account Statement is available in English language only.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnCancel: TSpeedButton
      Left = 340
      Top = 8
      Width = 89
      Height = 25
      Cursor = crHandPoint
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
    end
    object btnSendEmail: TSpeedButton
      Left = 440
      Top = 8
      Width = 89
      Height = 25
      Cursor = crHandPoint
      Caption = 'Send'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAB24FFAC24FFAC24FF
        AC24FFAC24FFAC24FFAC24FFAC24FFAC24FFAC24FFAA21FFC15DFFFFFFFFFFFF
        FFFFFFFFEED5FFC464FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFB337FFFFFFFFFFFFFFFFFFFFEFD8FFC260FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB43BFFFFFFFFFFFF
        FFFFFFFFF0D8FFC260FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFB43BFFFFFFFFFFFFFFFFFFFFF5E6FFC260FFFFFFFFFFFFFF
        FFFFFFFFFFFFD48EFFE4B9FFFFFFFFFFFFFFFFFFFFFFFFFFB43BFFFFFFFFFFFF
        FFCE7EFFB53CFFC86FFFFFFFFFFFFFFFFFFFFFB235FFC76BFFB43BFFCB77FFFF
        FFFFFFFFFFFFFFFFB43BFFFFFFFFFFFFFFFFFFFFEDD2FFC364FFFFFFFFFFFFFF
        A514FFD694FFF5E3FFF7E9FFC360FFB742FFFFFFFFFFFFFFB43BFFFFFFFFB642
        FFB235FFAE2BFFCD7CFFFFFFFF9F04FFE4B6FFF2DDFFEED1FFEED1FFF5E4FFD4
        8DFFA81BFFFFFFFFB640FFFFFFFFFFFFFFFFFFFFFAF4FFB43AFFAC24FFF4E1FF
        F7EAFFF4E2FFF4E2FFF4E2FFF4E2FFF9EFFFE8C3FFAE2BFFB030FFAC24FFAC26
        FFAC27FFAD27FF9D00FFAD27FFAB23FFAA1FFFAA1FFFAA1FFFAA1FFFAA1FFFAA
        1FFFAD27FFA615FFC15EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Margin = 10
      ParentShowHint = False
      ShowHint = True
      Spacing = 10
      OnClick = btnSendEmailClick
    end
  end
end
