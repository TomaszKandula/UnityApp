object ActionsForm: TActionsForm
  Left = 0
  Top = 0
  ActiveControl = OpenItemsGrid
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 708
  ClientWidth = 940
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMiddle: TPanel
    Left = 0
    Top = 168
    Width = 940
    Height = 281
    Margins.Left = 5
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 0
    object OpenItemsGrid: TStringGrid
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 930
      Height = 271
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 10
      Align = alClient
      ColCount = 13
      DefaultColWidth = 10
      DefaultRowHeight = 17
      RowCount = 2
      GradientEndColor = 15527148
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goThumbTracking]
      TabOrder = 0
      OnDrawCell = OpenItemsGridDrawCell
      OnKeyDown = OpenItemsGridKeyDown
      OnMouseWheelDown = OpenItemsGridMouseWheelDown
      OnMouseWheelUp = OpenItemsGridMouseWheelUp
      OnSelectCell = OpenItemsGridSelectCell
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
    Top = 449
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
    TabOrder = 1
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
        Left = 81
        Top = 7
        Width = 42
        Height = 25
        Cursor = crHandPoint
        Caption = '>>'
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
        Caption = 'Set Follow-Up'
        OnClick = btnFeedbackClick
      end
      object btnClearFollowUp: TSpeedButton
        Left = 275
        Top = 7
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Caption = 'Clear Follow-Up'
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
        OnClick = btnSendEmailClick
      end
      object btnBack: TSpeedButton
        Left = 18
        Top = 7
        Width = 42
        Height = 25
        Cursor = crHandPoint
        Caption = '<<'
        OnClick = btnBackClick
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
        ColWidths = (
          10
          96
          89
          81
          75
          72
          64
          53
          42)
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
    Top = 0
    Width = 940
    Height = 153
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 15
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object GroupCustomerDetails: TGroupBox
      Left = 18
      Top = 12
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
      object btnSaveCustDetails: TSpeedButton
        AlignWithMargins = True
        Left = 596
        Top = 86
        Width = 25
        Height = 22
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000C0CB77B4C25C
          CDD59AF7F6FFF0F0EFF0F0EFF0F0EFF0F0EFF0F0EFF0F0EFF0F0EFF0F0EFF7F5
          FECDD59AB4C25CC0CB77B7C463B5C25DCDD59AF8F9FFE4DDD7E2DAD5E2DBD5E2
          DBD5E2DBD5E2DBD5E2DAD5E4DCD7F8F8FFCDD59AB5C25DB7C463B8C565B5C25D
          CDD59AF8F7FFEDEAE8EDEAE7EDEAE8EDEAE8EDEAE8EDEAE8EDEAE7EDEBE8F8F6
          FFCDD59AB5C25DB8C565B8C565B5C25DCDD59AF8F7FFEAE5E2E9E4E0E9E4E0E9
          E4E0E9E4E0E9E4E0E9E4E0EAE5E1F8F7FFCDD59AB5C25DB8C565B8C565B5C25D
          CDD59AF8F9FFE7E2DDE6E0DBE6E0DCE6E0DCE6E0DCE6E0DCE6E0DBE7E1DCF8F7
          FFCDD59AB5C25DB8C565B8C565B5C25DCED69DFBFAFFF1EEF2F1EDF1F1EEF2F1
          EEF2F1EEF2F1EEF2F1EDF1F1EEF2FBF9FFCED69EB5C25DB8C565B8C565B6C361
          C2CD7FD8DDB0D6DCACD6DCACD6DCACD6DCACD6DCACD6DCACD6DCACD6DCACD8DD
          B0C2CD80B6C361B8C565B8C565B8C565B6C361B3C15BB3C15BB3C15BB3C15BB3
          C15BB3C15BB3C15BB3C15BB3C15BB3C15BB6C361B8C565B8C565B8C565B8C565
          B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C5
          65B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8
          C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565B8C565
          B8C565B5C25EB3C159B3C159B3C159B3C159B3C159B3C059B2C059B3C159B5C2
          5EB8C565B8C565B8C565B8C565B8C565B5C35FCAD38DDBE0BBD9DEB6D9DEB6D9
          DEB6D9DEB6E3EBC0EBF4C8E2E8C2CAD38DB5C35FB8C565B7C464B8C565B8C565
          B3C159DBE0B9F8F7FFF5F4F9F5F4F9F5F4F9FEFEFFAEA49C6D5C4ACFC7CAE0E5
          C0B3C159B7C464B3C05BB8C565B8C565B3C15ADADEB5F5F4F9F2F2F2F2F2F2F1
          F1F1FDFEFF9F9485543F24C5BEB8E0E5BDB4C25AA9B946FAFBF4B7C463B8C565
          B3C15ADADEB5F5F4F9F2F2F2F2F2F2F1F1F1FDFEFF958978412A0BBEB6AFE0E6
          BDA5B53BF8FAF1FFFFFFC0CB77B7C463B3C15ADADEB5F5F4F9F2F2F2F2F2F2F2
          F1F1F3F4F4ECECEBE6E4E2F2F2F7D6DDAEF5F7EAFFFFFFFFFFFF}
        Spacing = 15
        OnClick = btnSaveCustDetailsClick
      end
      object Cust_MailBack: TShape
        Left = 591
        Top = 55
        Width = 261
        Height = 21
        Pen.Color = 15527148
      end
      object Cust_PersonBack: TShape
        Left = 591
        Top = 28
        Width = 261
        Height = 21
        Pen.Color = 15527148
      end
      object Cust_NumberBack: TShape
        Left = 130
        Top = 55
        Width = 261
        Height = 21
        Pen.Color = 15527148
      end
      object Cust_Number: TLabel
        Left = 136
        Top = 59
        Width = 250
        Height = 13
        AutoSize = False
        Caption = '{data}'
        Color = clWhite
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
      object Cust_NameBack: TShape
        Left = 130
        Top = 28
        Width = 261
        Height = 21
        Pen.Color = 15527148
      end
      object Cust_Name: TLabel
        Left = 136
        Top = 32
        Width = 250
        Height = 13
        Hint = 'Copy to Clipboard'
        AutoSize = False
        Caption = '{data}'
        Color = clWhite
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
      object btnEdit: TSpeedButton
        AlignWithMargins = True
        Left = 299
        Top = 86
        Width = 25
        Height = 22
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E000000000000000000006F655AC5E4FB
          D9ECFEFCFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFC5E4FB8FC0ED99CBFBA3C8FD1EB3ECFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD9ECFF99CBFC
          78C7F666CDF327C6EC1074EBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFCFEFEA4C8FD66CDF30786EB1496EC10B4EA1278ECFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1EB2EC
          27C6EC1597EB1981ED1993EC10B4EA1278ECFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1073EB10B4EA1993EC1982ED1993EC10
          B4EA1278ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF1278EC10B4EA1993EC1982ED1993EC10B4EA1278ECFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1278EC10B4EA1993EC19
          82ED1993EC10B4EA1278ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF1278EC10B4EA1993EC1982ED1993EC10B4EA1278ECFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1278EC10
          B4EA1993EC1982ED1993EC10B4EA0F77EBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1278EC10B4EA1993EC1982ED1390EC0FAE
          EECAE1E3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF1278EC10B4EA1290EC1386F0E3F1E7C3D5D5C5DFDAFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F77EB0FAEEEE4F1E7C4D6
          D6CDE7DEACABC16977E3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFCAE1E3C3D5D5CDE7DEB4B2C32D40DA4152D4FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC5DEDAACAC
          C12D40DA4253D56471DCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF6A77E34152D46471DCFBFCFE}
        Spacing = 15
        OnClick = btnEditClick
      end
      object btnCopyCustName: TSpeedButton
        Left = 397
        Top = 28
        Width = 21
        Height = 21
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272727272727272727272727272727272727272727272727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFF7272
          72727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0E0E0FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272
          727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFFFFFFFF72
          7272727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272727D
          7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272
          727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnCopyCustNameClick
      end
      object btnCopyCustNumber: TSpeedButton
        Left = 397
        Top = 55
        Width = 21
        Height = 21
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272727272727272727272727272727272727272727272727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFF7272
          72727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0E0E0FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272
          727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFFFFFFFF72
          7272727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272727D
          7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272
          727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnCopyCustNumberClick
      end
      object btnCopyPerson: TSpeedButton
        Left = 858
        Top = 28
        Width = 21
        Height = 21
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272727272727272727272727272727272727272727272727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFF7272
          72727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0E0E0FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272
          727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFFFFFFFF72
          7272727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272727D
          7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272
          727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnCopyPersonClick
      end
      object btnCopyEmail: TSpeedButton
        Left = 858
        Top = 55
        Width = 21
        Height = 21
        Cursor = crHandPoint
        Hint = 'Copy to Clipboard'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272727272727272727272727272727272727272727272727272FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          727272727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AB17D4AB17D
          4AB17D4AFFFFFF727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFF7272
          72727272727272727272FFFFFF727272FFFFFFB17D4AB17D4AB17D4AFFFFFF72
          7272FFFFFFFFFFFFFFFFFFFFFFFF727272D6D6D67D7D7DE0E0E0FFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272
          727D7D7DE0E0E0FFFFFFFFFFFF727272FFFFFFB17D4AB17D4AFFFFFFFFFFFF72
          7272727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFF727272
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFFFFFFFFFFFFFFFFFFF7272727D
          7D7DE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272
          727272727272727272727272727272E0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnCopyEmailClick
      end
      object Label1: TLabel
        Left = 443
        Top = 89
        Width = 84
        Height = 13
        Caption = 'Save all changes:'
      end
      object Cust_Mail: TEdit
        Left = 596
        Top = 59
        Width = 250
        Height = 13
        BorderStyle = bsNone
        Color = clWhite
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
        Left = 596
        Top = 32
        Width = 250
        Height = 13
        BorderStyle = bsNone
        Color = clWhite
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
        Height = 22
        Cursor = crHandPoint
        Style = csOwnerDrawFixed
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
  object PanelStatusBar: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 680
    Width = 940
    Height = 28
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    object SimpleText: TLabel
      Left = 7
      Top = 8
      Width = 34
      Height = 13
      Caption = '{TEXT}'
    end
  end
end
