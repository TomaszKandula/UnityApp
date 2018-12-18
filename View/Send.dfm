object SendForm: TSendForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 531
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
  Scaled = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 539
    Height = 469
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitHeight = 462
    object Shape_Banks: TShape
      Left = 16
      Top = 388
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
      Top = 428
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Invoices: TShape
      Left = 16
      Top = 308
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Terms: TShape
      Left = 16
      Top = 348
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Text_Banks: TLabel
      Left = 22
      Top = 394
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
      Top = 434
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
      Top = 313
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
      Top = 354
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
    object Text_Custom_Message: TLabel
      Left = 18
      Top = 99
      Width = 85
      Height = 13
      Caption = 'Custom message:'
    end
    object cbAddOverdue: TCheckBox
      Left = 424
      Top = 312
      Width = 97
      Height = 17
      Cursor = crHandPoint
      TabStop = False
      Caption = 'Overdue only'
      TabOrder = 1
      OnKeyUp = cbAddOverdueKeyUp
    end
    object PanelMessage: TPanel
      Left = 16
      Top = 118
      Width = 505
      Height = 171
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Text_Message: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 501
        Height = 167
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        TabStop = False
        Align = alClient
        BorderStyle = bsNone
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxLength = 1024
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        OnKeyUp = Text_MessageKeyUp
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 489
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
    ExplicitTop = 482
    object Text_Warn: TLabel
      Left = 16
      Top = 12
      Width = 277
      Height = 13
      Caption = '* Account Statement is available in English language only.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnCancel: TSpeedButton
      Left = 342
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
      Left = 442
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
