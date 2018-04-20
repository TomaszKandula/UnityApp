object SendForm: TSendForm
  Left = 0
  Top = 0
  Caption = 'Unity'
  ClientHeight = 466
  ClientWidth = 539
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
    Width = 529
    Height = 456
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitWidth = 491
    ExplicitHeight = 292
  end
  object btnCancel: TSpeedButton
    Left = 328
    Top = 424
    Width = 89
    Height = 25
    Cursor = crHandPoint
    Caption = 'Cancel'
    OnClick = btnCancelClick
  end
  object btnSendEmail: TSpeedButton
    Left = 432
    Top = 424
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
    Top = 16
    Width = 66
    Height = 13
    Caption = 'Email subject:'
  end
  object Text2: TLabel
    Left = 16
    Top = 72
    Width = 55
    Height = 13
    Caption = 'Email body:'
  end
  object Text_Subject: TMemo
    Left = 16
    Top = 35
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
  object Text_Body: TMemo
    Left = 16
    Top = 91
    Width = 505
    Height = 318
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
  object StatementAttach: TCheckBox
    Left = 16
    Top = 424
    Width = 161
    Height = 17
    Caption = 'Attach account statement'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
