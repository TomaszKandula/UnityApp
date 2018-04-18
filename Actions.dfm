object ActionsForm: TActionsForm
  Left = 0
  Top = 0
  ActiveControl = OpenItemsGrid
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 778
  ClientWidth = 940
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 759
    Width = 940
    Height = 19
    Color = 15527148
    Panels = <>
    SimplePanel = True
    SizeGrip = False
    ExplicitTop = 620
  end
  object PanelMiddle: TPanel
    Left = 0
    Top = 177
    Width = 940
    Height = 361
    Margins.Left = 5
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 116
    ExplicitHeight = 283
    object OpenItemsGrid: TStringGrid
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 930
      Height = 351
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 10
      Align = alClient
      ColCount = 13
      DefaultColWidth = 10
      DefaultRowHeight = 17
      DrawingStyle = gdsGradient
      RowCount = 2
      GradientEndColor = 15527148
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goThumbTracking]
      TabOrder = 0
      OnDrawCell = OpenItemsGridDrawCell
      OnKeyDown = OpenItemsGridKeyDown
      OnMouseWheelDown = OpenItemsGridMouseWheelDown
      OnMouseWheelUp = OpenItemsGridMouseWheelUp
      OnSelectCell = OpenItemsGridSelectCell
      ExplicitHeight = 273
      ColWidths = (
        10
        119
        94
        85
        95
        108
        67
        91
        97
        91
        73
        208
        113)
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 538
    Width = 940
    Height = 221
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 399
    object ButtonPanel: TPanel
      Left = 0
      Top = 180
      Width = 940
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object btnCallCustomer: TSpeedButton
        AlignWithMargins = True
        Left = 815
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alCustom
        Anchors = [akRight]
        Caption = 'Call Customer'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E0000000000000000000092E4A500BE25
          00C02C00C12D00C12D00C12D00C12D00C12D00C12D00C12D00C12D00C12C00C1
          2D00C02C00BE2592E4A500BE2500C02C00C02D00C02D00C02D00C02D00C02D00
          C02D00C02D00C02900BC1E00B91400BB1900C02A00C02C00BE2500C02C00C12D
          00C12D00C12D00C12D00C12D00C12D00C02C00BC1C00BF2995E4A7FFFFFFD7F4
          DE00BF2800C02A00C02C00C12D00C12D00C12D00C12D00C12D00C12D00BF2800
          B91488E19DFFFFFFFFFFFFFFFEFFFFFFFFEFFAF200BA1800C12D00C12D00C12D
          00C12D00C12D00C12D00BE2301C12EF7FCF8FFFFFFFFFFFFFEFEFEFEFEFEFFFF
          FFFFFFFF00BA1700C12D00C12D00C12D00C12D00C12D00BE2314C63EFFFFFFFF
          FFFF99E5ABA4E8B4FFFFFFFFFFFFD3F3DB00BD2000C02A00C12D00C12D00C12D
          00C12D00BF2703C12FFFFFFFFFFFFF0CC43700BA1700B9136FDB885AD67700B9
          1200BF2900C12D00C12D00C12D00C12D00C02B00B915FAFDFBFFFFFF07C23200
          BE2300C12D00C02C00BD2000BD2000BE2500C02A00C02B00C12D00C12D00C12D
          00BB1A90E3A3FFFFFF98E5AA00BA1800C12D00C12D00C02B00C02A16C63F32CC
          56D4F4DB00BC1C00C12D00C12D00BF2808C333FFFFFFFFFFFFA4E8B400B81100
          C02C00C02A00B70BE3F7E844D1655CD779E2F7E700BB1A00C12D00C12D00BC1B
          B5ECC2FFFFFFFEFEFEFFFFFF8DE3A100BC1E0AC335D1F3D94FD46ECDF2D64BD3
          6B8DE2A100BD2000C12D00C12D00B914FFFFFFFFFEFFFEFEFEFFFFFF55D57200
          BD2200C02A4FD46EC8F1D22BCB51EFFAF200BC1D00C02B00C12D00C12D00BA17
          F2FBF4FFFFFFFFFFFFC6F0D000B91200BE225ED77A6ADA8439CE5CEBF9EE0FC4
          3900BF2700C12D00C12D00C02C00BF2904C230CDF2D6CDF2D600BB1800C02A00
          C02ACEF2D6DDF6E399E5AA00BD1F00BF2700C12D00C12D00C02C00BE2500C02C
          00BF2900BB1A00BB1B00C02B00C12D00C02B00BC1C00BB1A00BD2000C02B00C1
          2D00C12D00C02C00BE2592E4A500BE2500C02C00C12D00C12D00C12D00C12D00
          C12D00C12D00C12D00C12D00C12D00C12D00C02C00BE2592E4A5}
        OnClick = btnCallCustomerClick
        ExplicitLeft = 805
      end
      object btnNext: TSpeedButton
        Left = 15
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Caption = 'Next'
        OnClick = btnNextClick
      end
      object btnSendStatement: TSpeedButton
        AlignWithMargins = True
        Left = 688
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alCustom
        Anchors = [akRight]
        Caption = 'Send Statement'
        OnClick = btnSendStatementClick
        ExplicitLeft = 678
      end
      object btnFeedback: TSpeedButton
        Left = 145
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Caption = 'Feedback'
        OnClick = btnFeedbackClick
      end
      object btnClearFollowUp: TSpeedButton
        Left = 275
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Caption = 'Clear Follow Up'
        OnClick = btnClearFollowUpClick
      end
      object btnSendEmail: TSpeedButton
        AlignWithMargins = True
        Left = 560
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alCustom
        Anchors = [akRight]
        Caption = 'Send Email'
        Enabled = False
        OnClick = btnSendEmailClick
      end
    end
    object HistoryPanel: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 300
      Height = 175
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object HistoryTitle: TLabel
        Left = 2
        Top = 0
        Width = 134
        Height = 16
        Caption = 'Daily Comment History:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object HistoryGrid: TStringGrid
        Left = 0
        Top = 24
        Width = 300
        Height = 151
        Hint = 'Your daily comments history'
        Margins.Left = 5
        Margins.Top = 0
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alBottom
        ColCount = 9
        DefaultColWidth = 10
        DefaultRowHeight = 17
        DrawingStyle = gdsGradient
        RowCount = 10
        GradientEndColor = 15527148
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goThumbTracking]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnDrawCell = HistoryGridDrawCell
        OnKeyDown = HistoryGridKeyDown
        OnMouseWheelDown = HistoryGridMouseWheelDown
        OnMouseWheelUp = HistoryGridMouseWheelUp
        OnSelectCell = HistoryGridSelectCell
      end
    end
    object DailyPanel: TPanel
      AlignWithMargins = True
      Left = 315
      Top = 0
      Width = 310
      Height = 175
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object DailyTitle: TLabel
        Left = 2
        Top = 0
        Width = 89
        Height = 16
        Caption = 'Daily comment:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object DailyCom: TMemo
        Left = 0
        Top = 24
        Width = 310
        Height = 151
        Hint = 'Press Enter to save'
        Margins.Left = 5
        Margins.Top = 0
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alBottom
        BevelInner = bvNone
        BevelKind = bkFlat
        BorderStyle = bsNone
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 0
        WantReturns = False
        OnKeyDown = DailyComKeyDown
      end
    end
    object GeneralPanel: TPanel
      AlignWithMargins = True
      Left = 635
      Top = 0
      Width = 300
      Height = 175
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 3
      object GeneralTitle: TLabel
        Left = 2
        Top = 0
        Width = 106
        Height = 16
        Caption = 'General comment:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object GeneralCom: TMemo
        Left = 0
        Top = 24
        Width = 300
        Height = 151
        Hint = 'Press Enter to save'
        Margins.Left = 5
        Margins.Top = 0
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alBottom
        BevelInner = bvNone
        BevelKind = bkFlat
        BorderStyle = bsNone
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 0
        WantReturns = False
        OnKeyDown = GeneralComKeyDown
      end
    end
  end
  object PanelTop: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 5
    Width = 940
    Height = 172
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 3
    object InnerBox: TShape
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 930
      Height = 156
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Pen.Color = clMedGray
    end
    object GroupCustomerDetails: TGroupBox
      Left = 19
      Top = 16
      Width = 902
      Height = 129
      Caption = 'Customer details'
      TabOrder = 0
      object Text4: TLabel
        Left = 30
        Top = 32
        Width = 80
        Height = 13
        Caption = 'Customer Name:'
      end
      object Text5: TLabel
        Left = 30
        Top = 59
        Width = 90
        Height = 13
        Caption = 'Customer Number:'
      end
      object Text6: TLabel
        Left = 443
        Top = 32
        Width = 78
        Height = 13
        Caption = 'Contact Person:'
      end
      object Text7: TLabel
        Left = 443
        Top = 59
        Width = 133
        Height = 13
        Caption = 'E-Mail address (statement):'
      end
      object Text8: TLabel
        Left = 30
        Top = 89
        Width = 86
        Height = 13
        Caption = 'Phone number(s):'
      end
      object Cust_Name: TLabel
        Left = 130
        Top = 28
        Width = 260
        Height = 21
        Hint = 'Copy to Clipboard'
        AutoSize = False
        Caption = '{data}'
        Color = 15527148
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        Transparent = False
        Layout = tlCenter
      end
      object Cust_Number: TLabel
        Left = 130
        Top = 55
        Width = 260
        Height = 21
        AutoSize = False
        Caption = '{data}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        Transparent = False
        Layout = tlCenter
      end
      object Copy_CustName: TImage
        Left = 396
        Top = 28
        Width = 16
        Height = 16
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          000010000000010018000000000000030000C30E0000C30E0000000000000000
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF72727272727272727272727272727272727272727272727272
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272727272727272727272727272FFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AFFFFFF727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0
          E0E0FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D
          4AFFFFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFF
          FFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        ShowHint = True
        OnClick = Copy_CustNameClick
      end
      object Copy_CustNumber: TImage
        Left = 396
        Top = 55
        Width = 16
        Height = 16
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          000010000000010018000000000000030000C30E0000C30E0000000000000000
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF72727272727272727272727272727272727272727272727272
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272727272727272727272727272FFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AFFFFFF727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0
          E0E0FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D
          4AFFFFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFF
          FFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        ShowHint = True
        OnClick = Copy_CustNumberClick
      end
      object Copy_Person: TImage
        Left = 857
        Top = 28
        Width = 16
        Height = 16
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          000010000000010018000000000000030000C30E0000C30E0000000000000000
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF72727272727272727272727272727272727272727272727272
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272727272727272727272727272FFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AFFFFFF727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0
          E0E0FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D
          4AFFFFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFF
          FFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        ShowHint = True
        OnClick = Copy_PersonClick
      end
      object Copy_Email: TImage
        Left = 857
        Top = 55
        Width = 16
        Height = 16
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          000010000000010018000000000000030000C30E0000C30E0000000000000000
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF72727272727272727272727272727272727272727272727272
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272727272727272727272727272FFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AB17D4AB17D4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4A
          B17D4AFFFFFF727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0
          E0E0FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFF
          FFFFFFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D
          4AFFFFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFF
          FFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFF
          FFFFFFFF7272727D7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF727272727272727272727272727272727272E0E0E0FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        ShowHint = True
        OnClick = Copy_EmailClick
      end
      object btnSaveCustDetails: TSpeedButton
        AlignWithMargins = True
        Left = 591
        Top = 86
        Width = 110
        Height = 21
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Caption = 'Save Now'
        OnClick = btnSaveCustDetailsClick
      end
      object Text9: TLabel
        Left = 443
        Top = 89
        Width = 120
        Height = 13
        Caption = 'Update customer details:'
      end
      object Cust_MailBack: TShape
        Left = 591
        Top = 55
        Width = 261
        Height = 21
        Brush.Color = 15527148
        Pen.Style = psClear
      end
      object Cust_PersonBack: TShape
        Left = 591
        Top = 28
        Width = 261
        Height = 21
        Brush.Color = 15527148
        Pen.Style = psClear
      end
      object Cust_Mail: TEdit
        Left = 591
        Top = 59
        Width = 260
        Height = 13
        BorderStyle = bsNone
        Color = 15527148
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '{data}'
      end
      object Cust_Person: TEdit
        Left = 591
        Top = 32
        Width = 260
        Height = 13
        BorderStyle = bsNone
        Color = 15527148
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '{data}'
      end
      object Cust_Phone: TComboBox
        Left = 130
        Top = 86
        Width = 156
        Height = 21
        Cursor = crHandPoint
        Style = csDropDownList
        Color = clWhite
        ItemIndex = 0
        Sorted = True
        TabOrder = 2
        Text = '{data}'
        Items.Strings = (
          '{data}')
      end
    end
  end
end
