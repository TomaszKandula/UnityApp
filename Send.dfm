object SendForm: TSendForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 522
  ClientWidth = 538
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
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 528
    Height = 512
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitWidth = 524
    ExplicitHeight = 514
  end
  object btnCancel: TSpeedButton
    Left = 328
    Top = 474
    Width = 89
    Height = 25
    Cursor = crHandPoint
    Caption = 'Cancel'
    OnClick = btnCancelClick
  end
  object btnSendEmail: TSpeedButton
    Left = 432
    Top = 474
    Width = 89
    Height = 25
    Cursor = crHandPoint
    Caption = 'Send'
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
  object Text1: TLabel
    Left = 16
    Top = 94
    Width = 90
    Height = 13
    Caption = 'Custom salutation:'
  end
  object Text2: TLabel
    Left = 16
    Top = 141
    Width = 85
    Height = 13
    Caption = 'Custom message:'
  end
  object Shape_Customer: TShape
    Left = 16
    Top = 60
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
  object Shape_Business: TShape
    Left = 16
    Top = 20
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
  object Shape_Banks: TShape
    Left = 16
    Top = 383
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
  object Text_Business: TLabel
    Left = 437
    Top = 26
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
    Top = 66
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
  object Text_Warn: TLabel
    Left = 16
    Top = 479
    Width = 227
    Height = 13
    Caption = '*Account Statement is available only in English.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Text_Salut: TMemo
    Left = 16
    Top = 113
    Width = 505
    Height = 22
    Color = clCream
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 0
    WantTabs = True
  end
  object Text_Message: TMemo
    Left = 16
    Top = 160
    Width = 505
    Height = 129
    Color = clCream
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 1
    WantTabs = True
  end
  object cbAddOverdue: TCheckBox
    Left = 424
    Top = 307
    Width = 97
    Height = 17
    Caption = 'Overdue only'
    Enabled = False
    TabOrder = 2
  end
end
