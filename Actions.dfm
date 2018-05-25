object ActionsForm: TActionsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 771
  ClientWidth = 940
  Color = clWhite
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
  object MasterPanel: TPanel
    Left = 0
    Top = 0
    Width = 940
    Height = 771
    Align = alClient
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 88
    ExplicitTop = 136
    ExplicitWidth = 577
    ExplicitHeight = 193
    object PanelBottom: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 466
      Width = 920
      Height = 213
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      Color = 15527148
      ParentBackground = False
      TabOrder = 0
      ExplicitTop = 468
      object HistoryPanel: TPanel
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 321
        Height = 213
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alLeft
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        object HistoryTitle: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 5
          Width = 306
          Height = 16
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          Caption = 'Daily Comment History:'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          ParentColor = False
          ParentFont = False
          ExplicitWidth = 134
        end
        object HistoryGrid: TStringGrid
          AlignWithMargins = True
          Left = 10
          Top = 36
          Width = 301
          Height = 167
          Hint = 'Your daily comments history'
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alClient
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
          ExplicitLeft = 18
          ExplicitTop = 56
          ExplicitWidth = 242
          ExplicitHeight = 169
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
        Left = 321
        Top = 0
        Width = 299
        Height = 213
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 1
        object DailyTitle: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 5
          Width = 285
          Height = 16
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          Caption = 'Daily Comment:'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          ParentColor = False
          ParentFont = False
          ExplicitWidth = 91
        end
        object DailyCom: TMemo
          AlignWithMargins = True
          Left = 10
          Top = 36
          Width = 280
          Height = 167
          Hint = 'Press Enter to save'
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alClient
          BevelInner = bvNone
          BevelKind = bkFlat
          BevelOuter = bvNone
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
          ExplicitLeft = 5
          ExplicitTop = 26
          ExplicitWidth = 300
          ExplicitHeight = 261
        end
      end
      object GeneralPanel: TPanel
        AlignWithMargins = True
        Left = 620
        Top = 0
        Width = 300
        Height = 213
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 2
        ExplicitLeft = 1042
        ExplicitHeight = 175
        object GeneralTitle: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 5
          Width = 285
          Height = 16
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          Caption = 'General Comment:'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          ParentColor = False
          ParentFont = False
          ExplicitWidth = 108
        end
        object GeneralCom: TMemo
          AlignWithMargins = True
          Left = 10
          Top = 36
          Width = 280
          Height = 167
          Hint = 'Press Enter to save'
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alClient
          BevelInner = bvNone
          BevelKind = bkFlat
          BevelOuter = bvNone
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
          ExplicitHeight = 246
        end
      end
    end
    object PanelMiddle: TPanel
      Left = 0
      Top = 168
      Width = 940
      Height = 298
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      Color = 15527148
      ParentBackground = False
      TabOrder = 1
      ExplicitHeight = 335
      object OpenItemsGrid: TStringGrid
        AlignWithMargins = True
        Left = 10
        Top = 0
        Width = 920
        Height = 288
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 10
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
        ExplicitTop = 40
        ExplicitHeight = 285
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
    object PanelStatusBar: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 739
      Width = 940
      Height = 32
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 2
      ExplicitTop = 676
      object SimpleText1: TLabel
        Left = 120
        Top = 9
        Width = 41
        Height = 13
        Caption = '{TEXT}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Text1: TLabel
        Left = 10
        Top = 9
        Width = 88
        Height = 13
        Caption = 'Current follow-up:'
      end
      object Splitter1: TBevel
        Left = 208
        Top = 8
        Width = 17
        Height = 17
        Shape = bsLeftLine
      end
      object Text2: TLabel
        Left = 231
        Top = 9
        Width = 116
        Height = 13
        Caption = 'Open items date && time:'
      end
      object SimpleText2: TLabel
        Left = 368
        Top = 9
        Width = 41
        Height = 13
        Caption = '{TEXT}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
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
      TabOrder = 3
      ExplicitWidth = 1347
      object GroupCustomerDetails: TGroupBox
        Left = 10
        Top = 12
        Width = 920
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
          Width = 68
          Height = 22
          Cursor = crHandPoint
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Caption = 'Edit'
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C40E0000C40E00000000000000000000C9C8C6EEEDEB
            F4F4F3FFFDFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFEFEEECDDDAD7E6DCD6C5D8DEA6E1F8FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F4F3E8DDD6
            BCD8E04DC4F287D5F3ABE1F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFDB0CED74BC3F15CC6EF5CC6EE87D5F3ABE1F6FF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF80CEEB
            49B7E05EC7F05CC6EF5CC6EE87D5F3ABE1F6FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF88CFEA4AB7E15EC8F05CC6EE5CC6EE87
            D5F3ABE1F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF86CEEA4BB8E15EC8F05CC6EE5CC6EE87D5F3ABE1F6FFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF85CEE94BB8E25EC8F05C
            C6EE5CC6EE87D5F3ABE1F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF84CDE94BB9E35EC8F05CC6EE5CC6EE87D5F3ABE1F6FFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF83CDE94C
            BAE35EC8F05CC6EE5CC6EE79D1F3DBECF2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF82CCE94CBAE45EC8F048C1EFBFE1ECF6EF
            EAF9F7F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF81CCE83CB6E5BEE1EDFBF1EBFEFBED92A5E298ABE8FFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6E3EDF9F0EAFFFBEE99AB
            E46D88E06B87DEAAB9ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF8F7F18FA2DC6E89E17891E1748EE07891E1FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E9ED56882
            D6758FE26C88DEFCFDFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF9FADDB7088D4FCFDFFFFFFFF}
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
    object ButtonPanel: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 679
      Width = 920
      Height = 50
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      DoubleBuffered = True
      ParentBackground = False
      ParentDoubleBuffered = False
      TabOrder = 4
      ExplicitLeft = 0
      ExplicitTop = 775
      ExplicitWidth = 940
      object btnCallCustomer: TSpeedButton
        AlignWithMargins = True
        Left = 795
        Top = 12
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
        Flat = True
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
        Spacing = 7
        OnClick = btnCallCustomerClick
        ExplicitLeft = 815
      end
      object btnNext: TSpeedButton
        Left = 120
        Top = 13
        Width = 77
        Height = 25
        Cursor = crHandPoint
        Caption = 'Next'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFDEFDEF2941EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCE8CFF18805F2931BFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFCE9D1F18C0CF29219F2931BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCE8CFF18C0CF39621F29219F2
          931BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFDEEDBF08400F29520F39621F29219F2931BFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAD6A8F18701F29520F3
          9621F29219F2931BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFBDDB8F18701F2951FF39621F29219F2931BFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCE9D1F2
          9015F29520F39621F29219F29117FEFDFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFCE9D1F29015F29520F39621F29219F29117FEFD
          FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBDDB9F18701F2
          951FF39621F29219F2931BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFAD6A8F18701F29520F39621F29219F2931BFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDEEDBF08400F29520F39621F2
          9219F2931BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFCE8CFF18C0CF39621F29219F2931BFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCE9D1F18C0CF29219F2931BFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFCE8CFF18804F2931BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDEFDEF2951FFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Layout = blGlyphRight
        Spacing = 7
        OnClick = btnNextClick
      end
      object btnSendStatement: TSpeedButton
        AlignWithMargins = True
        Left = 632
        Top = 12
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alCustom
        Anchors = [akRight]
        Caption = 'Auto Statement'
        Flat = True
        Glyph.Data = {
          F6030000424DF603000000000000360000002800000014000000100000000100
          180000000000C0030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D6FF95
          38FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF
          9C45FF9C45FF9C46FF9940FFD3ACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFA04EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFF9437
          FFFFFFFFA75AFFFFFFFFFFFFFFF3E9FFAB63FFB16EFFB16EFFAB63FFF3E9FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFF973DFFEFE1FFFFFFFF
          A658FFFFFFFFFFFFFFFFFFFFF6EFFFF7F1FFF7F0FFF1E5FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFF8F2FF973DFFEFE1FFFFFFFFFFFFFFA658FFFF
          FFFFE6D1FFD0A7FFD4AFFFD3ADFFD9B9FFFFFFFFFFFFFFFFFFFFFFFFFFD8B6FF
          A250FFAC64FFF8F3FFEFE2FFEAD9FFFFFFFFFFFFFFFFFFFFA658FFFFFFFFE8D5
          FFD2ABFFD4AFFFD2ABFFD7B5FFFFFFFFFFFFFFFFFFFFB26FFFB575FFFFFFFFEE
          E0FF983FFFDEC1FFFFFFFFFFFFFFFFFFFFFFFFFFA658FFF6EFFFF5ECFFF8F1FF
          F6EFFFFFFFFFFFFFFFFFFFFFF7F0FF9A43FFD7B5FFFFFFFFFFFFFFFFFFFFFFFF
          FFAB63FFB97DFFFFFFFFFFFFFFFFFFFFA658FFB779FFB16DFFB16EFFAD66FFDA
          BBFFFFFFFFD9B9FF9940FFF5ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          CCA1FF9E4AFFFCFAFFFFFFFFA658FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCC9F
          FFAC63FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEC
          DDFF9335FFFFFFFFA75AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFA04EFFFFFFFFFFFFFFFFFFFFE9D6FF9538FF9C46FF9C45FF9C45FF9C
          45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C45FF9C46FF9940FF
          D3ACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Spacing = 7
        OnClick = btnSendStatementClick
        ExplicitLeft = 652
      end
      object btnFeedback: TSpeedButton
        Left = 231
        Top = 12
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Caption = 'Set Follow-Up'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF807F7EFDFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFF4F9FFA6BFD63090DFFFFFFFFFFFFFFFFFFFFFFFFFF6F5F4F0F0EB
          F1F0ECF1F0ECF1F0ECF2F2EEF1F0ECF1F0ECF2F0ECFFFAEF169CF91488E42B8D
          DFFFF8EDF4F1EBFCFCFAF6F5F3F0EFEAF0EFEBF0F0EBF1F0ECE6E2DEF2F1EDF0
          EFEBF6F6F2EDE6DFC4C2C117A2FF188AE42B8EE0FEFBEFFFFFFBF6F5F3F0EFEB
          F1F0ECF2F1EEE6E3DEB09E96F7F7F4F4F4F0D2C9C3A28B83D5C4BA907E7916A2
          FF1690E64056ABFFFFFDF6F5F3F0EFEBF1F0ECF2F1EEE8E5E0B5A49CF7F7F4F7
          F7F4AA968EFEFFFDFCFEFBBBA296DDEAE7365DE02D32A58B8FC6F6F5F3F0EFEB
          F1F0ECF2F1EEE8E5E0B5A49CF7F7F4F1F0ECF7F7F4F6F6F2FEFFFDB7A59EF1EB
          E1E4E3EB6D7ADDFFFFFDF6F5F3F0EFEBF1F0ECF2F1EEE8E5E0B5A49CF7F7F4F1
          F0ECFBFDF9BFB1AAAB978EAD9991EAE8E3F6F5EEFCFAECFDFDFBF6F5F3F0EFEB
          F1F0ECF8F9F6EAE8E3B5A49CF7F7F4F6F7F3B8A8A0CCC1BBFBFDFA816257EDEB
          E6F2F1EDEFEEEAFCFCFAF6F5F3F0EFEBF7F7F4AC9890D5CDC7B7A7A0F7F7F4F8
          F9F69D857CFFFFFFFEFFFDB2A098EDEAE6F1F1EDEFEEEAFCFCFAF6F5F3F0EFEB
          F1F0ECFDFFFC967D73B7A69FF7F7F4F4F3F0DBD4CF9B8278BEB0A8A18A81F9FB
          F7F1F0ECEFEEEAFCFCFAFCF8FBF8F3F6F9F3F7F9F4F8FFFDFFF4EDF0FAF5F9F9
          F3F7FDF8FDF8F2F6E6DBDDFFFFFFF9F3F7F9F3F7F8F2F6FEFDFECEE3BCA9CF89
          ADD18EADD18EADD18EAED28FADD18EADD18EADD18EAED28FAFD491ADD18EADD1
          8EADD18EA6CD84EEF5E698CB724CA50954A91354A91354A91354A91354A91354
          A91354A91354A91354A91354A91354A91354A91346A200DAECCB9FCE7B56AA17
          5EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE
          215EAE214FA60DDDEECF9FCE7B56AA175EAE215EAE215EAE215EAE215EAE215E
          AE215EAE215EAE215EAE215EAE215EAE215EAE214FA60DDDEECF}
        Spacing = 7
        OnClick = btnFeedbackClick
      end
      object btnClearFollowUp: TSpeedButton
        Left = 353
        Top = 12
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Caption = 'Clear Follow-Up'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8EBFC7987EF5D6D
          EC7583EFE0E3FBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFF3F5FE4659EA6574ED5566EB6574ED4558E9EDEEFCF5F4F2F1F0EC
          F1F0ECF1F0ECF2F1EDF2F1EEF1F0ECF1F0ECFCF9EC969FED4F61EBC5CBF8A2AC
          F4CFD3F94F61EA9AA4F3F5F4F1F0EFEBF0EFEBF1F0ECEEECE8E9E6E2F1F1EDF0
          EFEBFFFFF28B97EF5667EC7280EFFFFFFF7D8AF05667EB8F9AF2F5F4F1F0EFEB
          F1F0ECF3F3EFD6CEC9C0B2ACF5F6F2F5F6F2C9BBAE8F88AE4C60F2B0B8F64557
          E9B0B8F6495BEAC2C8F8F5F4F1F0EFEBF1F0ECF3F3EFD9D1CCC4B7B1F5F6F2F7
          F7F4AB978EFFFFF98B96EC4A5FF35567ED4659EC7E8AEAFFFFFDF5F4F1F0EFEB
          F1F0ECF3F3EFD9D1CCC4B7B1F5F6F2F1F0ECF7F8F4F6F7F3FFFFFBA597A2BDC4
          F3D7D9ECFFFCEAFDFDFCF5F4F1F0EFEBF1F0ECF3F3EFD9D1CCC4B7B1F5F6F2F1
          F0ECFBFCF9B8A79FB09D95A58D81FFFFF7F5F3EBEFEEEAFDFDFCF5F4F1F0EFEB
          F1F0ECFAFCF8DAD4CFC4B7B1F5F6F2F7F8F4AC988FDAD2CDF6F7F3765448FCFE
          FBF1F0ECEFEEEAFDFDFCF5F4F1F0EFEBF7F8F5A48E85D0C6C1C6B9B3F5F6F2F8
          F9F69E877DFFFFFFFFFFFFA58F87F8F9F6F1F0ECEFEEEAFDFDFCF5F4F1F0EFEB
          F1F0ECFCFEFB896B61C7BAB4F5F6F2F5F5F1CEC3BDA38D84B9A8A1AC9991F7F8
          F5F1F0ECEFEEEAFDFDFCFBF7FAF9F3F7F9F3F7FAF4F8FFFAFFF5EFF2FAF4F8F9
          F3F7FEFAFFF4EDF0E9DFE2FFFFFFF9F3F7F9F3F7F8F2F5FFFEFFC6DFB0AAD08B
          ADD18EADD18EAED28FAED28FADD18EADD18EADD18EAED390AFD491ADD18EADD1
          8EADD18EA5CD83F6FAF487C3594EA60C54A91354A91354A91354A91354A91354
          A91354A91354A91354A91354A91354A91354A91344A100EDF6E68EC66358AB19
          5EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE215EAE
          215EAE214DA50AEFF7E98EC66358AB195EAE215EAE215EAE215EAE215EAE215E
          AE215EAE215EAE215EAE215EAE215EAE215EAE214DA50AEFF7E9}
        Spacing = 7
        OnClick = btnClearFollowUpClick
      end
      object btnSendEmail: TSpeedButton
        AlignWithMargins = True
        Left = 498
        Top = 12
        Width = 110
        Height = 25
        Cursor = crHandPoint
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alCustom
        Anchors = [akRight]
        Caption = 'Custom Statement'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000D8D8D8C5C5C5
          C5C5C5C5C5C5C5C5C5C5C5C5C6C6C6C7C7C7C7C7C7C7C7C7C6C6C6CCCCCCFFFF
          FFFFFFFFFFFFFFFFFFFFC5C5C5E7E7E7F6F6F6F6F6F6F6F6F6F8F8F8F6F1EEE6
          E4E3E4E4E4E5E5E5E5E5E5E3E3E2E4E4E4FFFFFFFFFFFFFFFFFFC5C5C5F6F6F6
          ECECEDE2E2E3E4E4E5DDD9D777AAC3C4E2EFE5E5EAD3D3D4D3D3D4D8D8D9E0E0
          E0FDFDFDFFFFFFFFFFFFC5C5C5F5F5F5EBEBECE1E1E2E1E1E2FBF3F057C8F5D5
          9433DCC19CD3D9E4D2D2D4D7D7D8E1E1E1FDFDFDFFFFFFFFFFFFC5C5C5F4F4F4
          F5F5F5F8F8F8F8F8F8F9FBFFF5D5ADD79432D89937DFC5A1E7EEF7E6E6E6E0E0
          E0FDFDFDFFFFFFFFFFFFC5C5C5F6F6F5E2E2E3CDCDCFCFCFD1CFCFD1D0D5E0D1
          BCA1E2A639E5AB3DC8AE8DCAD0DCE2E2E2FDFDFDFFFFFFFFFFFFC5C5C5F4F4F4
          F5F5F5F8F8F7F8F8F7F8F8F7F8F8F7F9FFFFE9D4B7D89935E4AA3CDEC4A1E1E7
          F1FDFDFDFFFFFFFFFFFFC5C5C5F5F5F5EBEBECE2E2E3E3E3E4E3E3E4E3E3E4E3
          E3E4E5EBF4D9C3A8E3A83AE4AB3BD9BE9CFFFFFFFFFFFFFFFFFFC5C5C5F5F5F5
          EBEBECE1E1E2E2E2E3E2E2E3E2E2E3E2E2E3E2E2E3E3E9F3E0CBAFE1A738E3A9
          3AECD3AFFFFFFFFFFFFFC5C5C5F4F4F4F5F5F5F8F8F8F8F8F8F8F8F8F8F8F7F4
          F4F4F4F4F4F4F4F4F6FCFFE7D1B5DEA237D29130F0D4A6FFFFFFC5C5C5F6F6F5
          E2E2E3CDCDCFCFCFD1CECED1CECED1F5F5F5F3F3F3F3F3F3F3F3F3F5FBFFE8D2
          B4D8942CD7D0DAB4B5F9C5C5C5F4F4F4F5F5F5F8F8F7F8F8F7F8F8F7F7F7F7F3
          F3F3F3F3F3F3F3F3F5F5F5F5F5F5F7FBFEFAFAFA3234EA4141E6C5C5C5F5F5F5
          EBEBECE2E2E3E3E3E4E2E2E4E2E2E3F4F4F4F3F3F3F4F4F4DDDDDDDDDDDDD9D9
          D9FFFFFFCECEF9D8D8FAC3C3C3F5F5F5EBEBECE1E1E2E2E2E3E2E2E3E2E2E3F4
          F4F4F4F4F4E3E3E3CBCBCBC6C6C6F9F9F9FFFFFFFFFFFFFFFFFFD3D3D3F2F2F2
          F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F3F3F3F4F4F4E4E4E4C4C4C4FBFBFBFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFF6F6F6F2F2F2F3F3F3F3F3F3F3F3F3F3F3F3F3
          F3F3F4F4F4E4E4E4F9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        OnClick = btnSendEmailClick
        ExplicitLeft = 518
      end
      object btnBack: TSpeedButton
        Left = 20
        Top = 12
        Width = 77
        Height = 25
        Cursor = crHandPoint
        Caption = 'Previous'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2951FFDEF
          DEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF2931BF18804FCE8CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2931BF29219F18C0CFCE9
          D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFF2931BF29219F39621F18C0CFCE8CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2931BF29219F39621F29520F08400FDEE
          DBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2931BF2
          9219F39621F29520F18701FAD6A8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFF2931BF29219F39621F2951FF18701FBDDB9FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFCF29117F29219F39621F2
          9520F29015FCE9D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFEFDFCF29117F29219F39621F29520F29015FCE9D1FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2931BF29219F3
          9621F2951FF18701FBDDB8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFF2931BF29219F39621F29520F18701FAD6A8FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2
          931BF29219F39621F29520F08400FDEEDBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2931BF29219F39621F18C0CFCE8
          CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFF2931BF29219F18C0CFCE9D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2931BF18805FCE8
          CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFF2941EFDEFDEFFFFFFFFFFFFFFFFFF}
        Spacing = 7
        OnClick = btnBackClick
      end
      object Bevel1: TBevel
        Left = 208
        Top = 6
        Width = 17
        Height = 39
        Shape = bsLeftLine
      end
      object Bevel2: TBevel
        Left = 487
        Top = 6
        Width = 17
        Height = 39
        Shape = bsLeftLine
      end
      object Bevel3: TBevel
        Left = 791
        Top = 6
        Width = 17
        Height = 39
        Shape = bsLeftLine
      end
    end
  end
end
