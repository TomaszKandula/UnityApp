object ViewMailerForm: TViewMailerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 619
  ClientWidth = 1085
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 577
    Width = 1085
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
    TabOrder = 2
    object Text_Warn: TLabel
      Left = 16
      Top = 12
      Width = 258
      Height = 13
      Caption = '* E-mail template is available in English language only.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnCancel: TSpeedButton
      Left = 886
      Top = 9
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
      Left = 986
      Top = 9
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
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 538
    Top = 10
    Width = 537
    Height = 557
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alRight
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Shape_Banks: TShape
      Left = 16
      Top = 461
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
      Top = 501
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Invoices: TShape
      Left = 16
      Top = 381
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Shape_Terms: TShape
      Left = 16
      Top = 421
      Width = 505
      Height = 26
      Pen.Color = clMedGray
      Pen.Style = psDot
    end
    object Text_Banks: TLabel
      Left = 22
      Top = 467
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
      Top = 507
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
      Top = 386
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
      Top = 427
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
    object Text2: TLabel
      Left = 18
      Top = 143
      Width = 85
      Height = 13
      Caption = 'Custom message:'
    end
    object Text3: TLabel
      Left = 18
      Top = 93
      Width = 78
      Height = 13
      Caption = 'Custom subject:'
    end
    object cbAddOverdue: TCheckBox
      Left = 424
      Top = 385
      Width = 97
      Height = 17
      Cursor = crHandPoint
      TabStop = False
      Caption = 'Overdue only'
      TabOrder = 2
      OnKeyUp = cbAddOverdueKeyUp
    end
    object PanelMessage: TPanel
      Left = 16
      Top = 162
      Width = 505
      Height = 201
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object Text_Message: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 501
        Height = 197
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
    object PanelSubject: TPanel
      Left = 16
      Top = 112
      Width = 505
      Height = 25
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Text_Subject: TEdit
        Left = 4
        Top = 4
        Width = 501
        Height = 20
        BorderStyle = bsNone
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyUp = Text_SubjectKeyUp
      end
    end
  end
  object PanelList: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 513
    Height = 557
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 5
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object PanelEmailContainer: TPanel
      AlignWithMargins = True
      Left = 20
      Top = 93
      Width = 473
      Height = 444
      Margins.Left = 20
      Margins.Top = 10
      Margins.Right = 20
      Margins.Bottom = 20
      Align = alClient
      BevelOuter = bvNone
      Caption = 'PanelEmailContainer'
      TabOrder = 1
      object CustomerList: TListView
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 463
        Height = 434
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BorderStyle = bsNone
        Columns = <>
        FlatScrollBars = True
        GridLines = True
        ReadOnly = True
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
      end
    end
    object PanelEmailFrom: TPanel
      AlignWithMargins = True
      Left = 20
      Top = 10
      Width = 473
      Height = 63
      Margins.Left = 20
      Margins.Top = 10
      Margins.Right = 20
      Margins.Bottom = 10
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Text4: TLabel
        Left = 18
        Top = 10
        Width = 178
        Height = 13
        Align = alCustom
        Caption = 'Select email address to be sent from:'
      end
      object EmailList: TComboBox
        Left = 18
        Top = 29
        Width = 178
        Height = 22
        Cursor = crHandPoint
        BevelInner = bvLowered
        BevelKind = bkFlat
        BevelOuter = bvNone
        Style = csOwnerDrawFixed
        TabOrder = 0
        TabStop = False
        OnKeyUp = EmailListKeyUp
      end
    end
  end
end
