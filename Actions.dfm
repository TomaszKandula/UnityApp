object ActionsForm: TActionsForm
  Left = 0
  Top = 0
  ActiveControl = OpenItemsGrid
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 639
  ClientWidth = 940
  Color = 15527148
  Constraints.MaxHeight = 668
  Constraints.MaxWidth = 946
  Constraints.MinHeight = 668
  Constraints.MinWidth = 946
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
    Top = 620
    Width = 940
    Height = 19
    Color = 15527148
    Panels = <>
    SimplePanel = True
    SizeGrip = False
  end
  object PanelMiddle: TPanel
    Left = 0
    Top = 116
    Width = 940
    Height = 283
    Margins.Left = 5
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 113
    ExplicitHeight = 286
    object OpenItemsGrid: TStringGrid
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 930
      Height = 273
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
      ExplicitTop = 10
      ExplicitHeight = 279
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
    Top = 399
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
    Height = 111
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 3
    ExplicitTop = 2
    object InnerBox: TShape
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 930
      Height = 96
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Pen.Color = clMedGray
    end
    object Text4: TLabel
      Left = 15
      Top = 22
      Width = 80
      Height = 13
      Caption = 'Customer Name:'
    end
    object Text5: TLabel
      Left = 15
      Top = 43
      Width = 90
      Height = 13
      Caption = 'Customer Number:'
    end
    object Text6: TLabel
      Left = 15
      Top = 65
      Width = 78
      Height = 13
      Caption = 'Contact Person:'
    end
    object Text7: TLabel
      Left = 392
      Top = 43
      Width = 133
      Height = 13
      Caption = 'E-Mail address (statement):'
    end
    object Text8: TLabel
      Left = 392
      Top = 65
      Width = 86
      Height = 13
      Caption = 'Phone number(s):'
    end
    object Cust_Name: TLabel
      Left = 128
      Top = 22
      Width = 657
      Height = 13
      Cursor = crArrow
      Hint = 'Copy to Clipboard'
      Caption = '{data}'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = Cust_NameClick
    end
    object Cust_Number: TLabel
      Left = 128
      Top = 43
      Width = 241
      Height = 13
      Cursor = crArrow
      Hint = 'Copy to Clipboard'
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
      OnClick = Cust_NumberClick
    end
    object imgEditDetails: TImage
      AlignWithMargins = True
      Left = 893
      Top = 38
      Width = 32
      Height = 32
      Cursor = crHandPoint
      Hint = 'Edit customer details'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alCustom
      Anchors = [akRight]
      AutoSize = True
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        0020080300000044A48AC60000000373424954080808DBE14FE0000000097048
        59730000078B0000078B01868D9B340000001974455874536F66747761726500
        7777772E696E6B73636170652E6F72679BEE3C1A00000141504C5445FFFFFFED
        EDDBF2E6D9D65C47D95E4CEFE7DFA34740E2DDCAF1C587DFDFC7E4DFCDE0E0CA
        A24941EFEBE0A14740AA4E3BE18C25E28A24E28C24E6A554EEEBDFD6594BA547
        41EFEADED75B4ADCD9CDCDC9C0DC6741D16140E08B26D75A4AAE543BCD5748F2
        9E25EF9921E18B25EF9B25E18D25A44741F29B22E4DFCFE28D2771716FE1DDCC
        A34640A34740E18C25E18C26D8D3C0D7D3BF837E7994918CA34740D5D0BBD6C4
        B0D75A4AD7D2BED7D3BED9C8B6D9D4C0D9D4C1DBD6C3DCCFBDDCD7C5DFDAC8E0
        DCCAE18C25E18F2CE1DDCBE29536E2D6C6E2DDCCE3AF6FE3B172E3DECDE3DECE
        E4E0D0E6E2D2E8E1D2E8E4D5E9E4D5E9E5D6EADAC6EAE6D7EAE6D8EBD2ADEBD2
        AEEBE7D8EBE7D9ECE8D9ECE8DAEDE8DBEEE0CDEEE2C9EEE4D3EEEADCEEEADDEF
        A439EFE3CBEFE5D0EFEBDEF0A339F0BA6BF29C21F2A73CF2A83DF2A83E10990F
        FF0000003274524E53000E14191B2024353537383A3F4144454546475B686A7A
        A0A6A8AFBCC6D7DADDDDDDDEE2E2E8E9E9EAEBECEFF6F7F7F7FAFB5FEF816A00
        0001274944415478DAADD1414F83301400E0761646501B456224CE8BC6FFE35F
        34F107EC6E8C170F1E4D4CD47119440F32C03117608CD23AC069BBA5351A7B78
        A4BC2F7DAFAFD03E03E2BA1916FC16AE012F7A2CD460520E723500A59BA90120
        834C0D00715335F8162D8802B688DDD3FA4F306D12E103E34F481A60F2279D97
        92127F03E92B9FD21DB40AB29100F6D1AF4BFC0C12C6A5B4AD35D00E6AB9D089
        F64F3D20BB0A55801E3A41366A412CCCA1735CF770B1D70B74FCD40222DC02ED
        2E02BDD65313CF5C59096A54976507FBEF1240BBF40A1EF9D1E4B3C93CE1B3F0
        80EA6C7EEB442F6FCB6B4E0301F43619F5AAE8792C9903D3EA7CEC27B241CD0D
        E655F02E964E7296E505B4FAD2519743422C8CA56F41EE996D58E8EBB1B6CD15
        B0B133A6F5376C22F800F4C99E3371AB48150000000049454E44AE426082}
      ShowHint = True
      Stretch = True
      OnClick = imgEditDetailsClick
    end
    object imgSaveDetails: TImage
      AlignWithMargins = True
      Left = 847
      Top = 38
      Width = 32
      Height = 32
      Cursor = crHandPoint
      Hint = 'Save customer details'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alCustom
      Anchors = [akRight]
      AutoSize = True
      Center = True
      Enabled = False
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF40000000373424954080808DBE14FE0000000017352
        474200AECE1CE90000000467414D410000B18F0BFC6105000000097048597300
        000EC400000EC401952B0E1B0000001974455874536F66747761726500777777
        2E696E6B73636170652E6F72679BEE3C1A000003624944415478DADD97CB6B13
        5114C6BFC9CC3495B4A66D6C6C48B5168B0BB1427D75A5AE5C8808BAD28D7F80
        1BDDF80FE84291AA0BA1E02615175D14448A8A0541A4556851AAAD0FB48FB4F1
        D59834D3A679CC4CE671C733092ED4A4EDD811D1C30C84CCBDB9BFFB7DE79CDC
        E154256329B914D6128A924720D88A2AAFCFF15CCE0D804C4682AAC8D8D2B6DB
        31846B00B96C1A82508596B65DF07A6BFE0E801D368413255C07B0831744B4B6
        ED5915C41F01F80E612BB1921D7F0C60B5106501B4828285C50460FD385814BD
        086C08FDF2238A92434195CB2E20D09C70CB4E709CC79902BAAEE167028F8707
        CF0B8E155A5FBF099E0AF35CB1E0FF03304D1DB9DC92E3853CE4734D6D3DF9CD
        AD15C0403EFF7B00BE9ABAB503B81DFF2640B9325C29EC5A17A8F9AC19A05223
        5A29EC3ED1180CFFD274FE4D0B9CC69B4113C929D3D19C0969D83D80BE9E9748
        CC2CA0D6DF88D3170EC330190C8B51493330F2D276532F18C5B1F77BC690988D
        02B56A851CD054A41793CE76331440B853C360DF279CB974947A42E97BAB9847
        0440307481F13C6E760D62EF213FEE47C6CB0358344B270827393872DB82E837
        30F12E8613E70E2C3BF61E29E05D68C05472C43D0B1EDD92515DA76172F223CE
        5C24053CA58D946E1415B03F70BC88C8954108493FA6E79FB907F03092437583
        86E8F4E7552980B80FD1D4F3F200CC3421CB59470043BD1EAC0BE888C5BE1473
        C036BCB473DB48528195529117AB10E91A82F9A9BA32806168658F58CBC5D35E
        2F44B2602E1EC7FE63EDE8D8B79916B58A8BB21249F17EFD328ED1A10F28CC08
        98591C75CF8281EE2564E53C042F433EAB5019D2718CA7E39A6C8219ACA88449
        2A807283E73998190112F7DE3D80BBD717A1AB645D46C1C61D802A2BF81295B0
        BDB305DE751E64B279A8AA8AD97709A4A65534D2D95262B3EE01F45F93088021
        9160387BA31D8FFBDE636C601EC7CF6F419DDF47B9F111E9741AA32FC62151CF
        08859A102F4CBA0770A72B85826242928093E78318EE8F61EA898CF6532695A4
        85544A824CEF8FD1680CDCDBAD680A87319777C902DBDFFECB19FA173530BF60
        E0E06986B1074924C639848E24E9B949EF0E194A6E03F1AFF31027B6A1B97933
        66D3AFC0E99A6269AAB3922B17DD57FB81B966E4B8349A3AB2E4B786F4122525
        6FD1C23A183351258A88C79350A70208368490D263F80642578D20CF69876D00
        00000049454E44AE426082}
      ShowHint = True
      Stretch = True
      OnClick = imgSaveDetailsClick
    end
    object Cust_Person: TEdit
      Left = 128
      Top = 65
      Width = 241
      Height = 13
      Cursor = crArrow
      Hint = 'Copy to Clipboard'
      BorderStyle = bsNone
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 0
      Text = '{data}'
      OnClick = Cust_PersonClick
    end
    object Cust_Mail: TEdit
      Left = 542
      Top = 43
      Width = 243
      Height = 13
      Cursor = crArrow
      Hint = 'Copy to Clipboard'
      BorderStyle = bsNone
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 1
      Text = '{data}'
      OnClick = Cust_MailClick
    end
    object Cust_Phone: TComboBox
      Left = 542
      Top = 62
      Width = 179
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
