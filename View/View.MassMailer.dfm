object MassMailerForm: TMassMailerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 755
  ClientWidth = 1257
  Color = 15855854
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 713
    Width = 1257
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
      Left = 1042
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
      Left = 1142
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
    Left = 710
    Top = 10
    Width = 537
    Height = 693
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
        Left = 2
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
    object PanelOption: TPanel
      Left = 6
      Top = 533
      Width = 525
      Height = 157
      BevelOuter = bvNone
      TabOrder = 2
      object Shape_Options: TShape
        AlignWithMargins = True
        Left = 10
        Top = 10
        Width = 505
        Height = 137
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alClient
        Brush.Style = bsClear
        Pen.Color = clMedGray
        Pen.Style = psDot
        ExplicitHeight = 127
      end
      object Shape_Dates: TShape
        AlignWithMargins = True
        Left = 240
        Top = 48
        Width = 216
        Height = 73
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Brush.Style = bsClear
        Pen.Color = clMedGray
        Pen.Style = psDot
      end
      object btnDelBegin: TSpeedButton
        Left = 423
        Top = 60
        Width = 25
        Height = 25
        Cursor = crHandPoint
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7EAFC6B7BEE5C6C
          EB6B7AEEDBDEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFF3F4FE3045E86776EC5061EC6373ED2D42E6ECEEFCF4F3EFEFEFEB
          EFEFEBEFEFEBF0EFEBF0F0ECEFEFEBEFEEEBFFFEEB8D97EA4659EBD2D5F98693
          F0D8DBFA4A5CEA8995F1F4F3F0F0EFEBF0EFEBF1F0ECF1F0EDEEEDE9F2F1EDEF
          EEEAFFFFF88392F35869ED5466ECFFFFFF6675ED5C6DEC818EF0F4F3F0F0EFEB
          F1F0ECF4F4F0CEC3BDBDACA6F4F4F0F9FAF7C8B8AC887C9B3D54F5CCD0F72036
          E5C5CAF8354AE8BCC2F8F4F3F0F0EFEBF1F0ECF4F4F0D5CCC5C6B7B4F3F4EFFF
          FFFF8A6D63FFFFFF7F8BE83851F95668EC3247EB7180E8FFFFFDF4F3F0F0EFEB
          F1F0ECF4F4F0D5CBC5C6B7B3F4F4F0EFEEEAFFFFFFF4F4F2FFFFFFA69392BBC2
          F6E0E2EBFFFFE8FCFCFDF4F3F0F0EFEBF1F0ECF3F3EFD5CBC5C6B7B3F4F4F0EF
          EEE9FFFFFFB2A098AE9B94A28B81FFFFF7F4F2ECEDECE8FDFDFDF4F3F0F0EFEB
          EFEEEAFFFFFFD5CCC6C6B7B3F3F4F0FFFFFFA08880D5CBC8F6F5F3694438FFFF
          FFF0EFEBEEEDE8FDFDFDF4F3F0F0EEEAFFFFFF85665CD5CBC5C7B9B5F3F3EFFF
          FFFF826357FFFFFFFFFFFFA0877FFCFDFBF0EFEBEEEDE8FDFDFDF4F3F0F0EFEB
          EFEEE9FFFFFF684134CDC0BBF3F4F0F8F8F4D1C6C291756CB5A39E9D847BFFFF
          FEF0EFEBEEEDE8FDFDFDFBF6FAF9F3F8F9F3F8F8F2F6FFFFFFF7F0F6FAF5F9F9
          F3F7FFFFFFF7F0F4E4D8DBFFFFFFF7F2F6F9F3F8F7F1F5FFFEFFC5DEB5B1D292
          B2D395B2D395B2D294B3D396B2D395B2D395B2D294B4D496B5D598B2D294B2D3
          95B3D395A3CB88F5F9F177BB4849A4044BA5084BA5084BA5084BA5084BA5084B
          A5084BA5084BA5084BA5084BA5084BA5084DA6092B9500E6F2DB85C2575BAD1E
          5EAE215EAE225EAE225EAE225EAE225EAE225EAE225EAE225EAE225EAE225EAE
          2260AF24409F00EAF4E185C2565AAD1D5DAD205EAE215EAE215EAE215EAE215E
          AE215EAE215EAE215EAE215EAE215EAE215FAE233F9E00E9F4E1}
        OnClick = btnDelBeginClick
      end
      object btnDelEnd: TSpeedButton
        Left = 423
        Top = 85
        Width = 25
        Height = 25
        Cursor = crHandPoint
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7EAFC6B7BEE5C6C
          EB6B7AEEDBDEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFF3F4FE3045E86776EC5061EC6373ED2D42E6ECEEFCF4F3EFEFEFEB
          EFEFEBEFEFEBF0EFEBF0F0ECEFEFEBEFEEEBFFFEEB8D97EA4659EBD2D5F98693
          F0D8DBFA4A5CEA8995F1F4F3F0F0EFEBF0EFEBF1F0ECF1F0EDEEEDE9F2F1EDEF
          EEEAFFFFF88392F35869ED5466ECFFFFFF6675ED5C6DEC818EF0F4F3F0F0EFEB
          F1F0ECF4F4F0CEC3BDBDACA6F4F4F0F9FAF7C8B8AC887C9B3D54F5CCD0F72036
          E5C5CAF8354AE8BCC2F8F4F3F0F0EFEBF1F0ECF4F4F0D5CCC5C6B7B4F3F4EFFF
          FFFF8A6D63FFFFFF7F8BE83851F95668EC3247EB7180E8FFFFFDF4F3F0F0EFEB
          F1F0ECF4F4F0D5CBC5C6B7B3F4F4F0EFEEEAFFFFFFF4F4F2FFFFFFA69392BBC2
          F6E0E2EBFFFFE8FCFCFDF4F3F0F0EFEBF1F0ECF3F3EFD5CBC5C6B7B3F4F4F0EF
          EEE9FFFFFFB2A098AE9B94A28B81FFFFF7F4F2ECEDECE8FDFDFDF4F3F0F0EFEB
          EFEEEAFFFFFFD5CCC6C6B7B3F3F4F0FFFFFFA08880D5CBC8F6F5F3694438FFFF
          FFF0EFEBEEEDE8FDFDFDF4F3F0F0EEEAFFFFFF85665CD5CBC5C7B9B5F3F3EFFF
          FFFF826357FFFFFFFFFFFFA0877FFCFDFBF0EFEBEEEDE8FDFDFDF4F3F0F0EFEB
          EFEEE9FFFFFF684134CDC0BBF3F4F0F8F8F4D1C6C291756CB5A39E9D847BFFFF
          FEF0EFEBEEEDE8FDFDFDFBF6FAF9F3F8F9F3F8F8F2F6FFFFFFF7F0F6FAF5F9F9
          F3F7FFFFFFF7F0F4E4D8DBFFFFFFF7F2F6F9F3F8F7F1F5FFFEFFC5DEB5B1D292
          B2D395B2D395B2D294B3D396B2D395B2D395B2D294B4D496B5D598B2D294B2D3
          95B3D395A3CB88F5F9F177BB4849A4044BA5084BA5084BA5084BA5084BA5084B
          A5084BA5084BA5084BA5084BA5084BA5084DA6092B9500E6F2DB85C2575BAD1E
          5EAE215EAE225EAE225EAE225EAE225EAE225EAE225EAE225EAE225EAE225EAE
          2260AF24409F00EAF4E185C2565AAD1D5DAD205EAE215EAE215EAE215EAE215E
          AE215EAE215EAE215EAE215EAE215EAE215FAE233F9E00E9F4E1}
        OnClick = btnDelEndClick
      end
      object Text_Begin: TLabel
        Left = 253
        Top = 66
        Width = 58
        Height = 13
        Caption = 'Begins with:'
      end
      object Text_End: TLabel
        Left = 253
        Top = 88
        Width = 50
        Height = 13
        Caption = 'Ends with:'
      end
      object Text_Options: TLabel
        Left = 16
        Top = 18
        Width = 97
        Height = 13
        Caption = 'INVOICE OPTIONS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ValBeginDate: TLabel
        Left = 322
        Top = 66
        Width = 60
        Height = 13
        Caption = 'yyyy-mm-dd'
      end
      object ValEndDate: TLabel
        Left = 322
        Top = 88
        Width = 60
        Height = 13
        Caption = 'yyyy-mm-dd'
      end
      object btnBeginDate: TSpeedButton
        Left = 391
        Top = 60
        Width = 25
        Height = 25
        Cursor = crHandPoint
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
        OnClick = btnBeginDateClick
      end
      object btnEndDate: TSpeedButton
        Left = 391
        Top = 85
        Width = 25
        Height = 25
        Cursor = crHandPoint
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
        OnClick = btnEndDateClick
      end
      object DueDateLabel: TLabel
        Left = 240
        Top = 33
        Width = 122
        Height = 13
        Caption = 'Due date filter (optional):'
      end
      object ImgCover: TImage
        Left = 232
        Top = 24
        Width = 233
        Height = 105
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000001000000
          010008060000005C72A8660000001974455874536F6674776172650041646F62
          6520496D616765526561647971C9653C0000032269545874584D4C3A636F6D2E
          61646F62652E786D7000000000003C3F787061636B657420626567696E3D22EF
          BBBF222069643D2257354D304D7043656869487A7265537A4E54637A6B633964
          223F3E203C783A786D706D65746120786D6C6E733A783D2261646F62653A6E73
          3A6D6574612F2220783A786D70746B3D2241646F626520584D5020436F726520
          352E332D633031312036362E3134353636312C20323031322F30322F30362D31
          343A35363A32372020202020202020223E203C7264663A52444620786D6C6E73
          3A7264663D22687474703A2F2F7777772E77332E6F72672F313939392F30322F
          32322D7264662D73796E7461782D6E7323223E203C7264663A44657363726970
          74696F6E207264663A61626F75743D222220786D6C6E733A786D703D22687474
          703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F2220786D6C6E73
          3A786D704D4D3D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F
          312E302F6D6D2F2220786D6C6E733A73745265663D22687474703A2F2F6E732E
          61646F62652E636F6D2F7861702F312E302F73547970652F5265736F75726365
          526566232220786D703A43726561746F72546F6F6C3D2241646F62652050686F
          746F73686F7020435336202857696E646F7773292220786D704D4D3A496E7374
          616E636549443D22786D702E6969643A35414244443430413933384131314536
          393044433837393245303645334533442220786D704D4D3A446F63756D656E74
          49443D22786D702E6469643A3541424444343042393338413131453639304443
          383739324530364533453344223E203C786D704D4D3A4465726976656446726F
          6D2073745265663A696E7374616E636549443D22786D702E6969643A35414244
          4434303839333841313145363930444338373932453036453345334422207374
          5265663A646F63756D656E7449443D22786D702E6469643A3541424444343039
          393338413131453639304443383739324530364533453344222F3E203C2F7264
          663A4465736372697074696F6E3E203C2F7264663A5244463E203C2F783A786D
          706D6574613E203C3F787061636B657420656E643D2272223F3E45BB4F010000
          02FA4944415478DAEDD40101003008C0A0DBFCCD35C82004B3BBFF01492300E8
          1200840900C204006102803001409800204C00102600081300840900C2040061
          02803001409800204C00102600081300840900C204006102803001409800204C
          00102600081300840900C204006102803001409800204C001026000813008409
          00C204006102803001409800204C00102600081300840900C204006102803001
          409800204C00102600081300840900C204006102803001409800204C00102600
          081300840900C204006102803001409800204C00102600081300840900C20400
          6102803001409800204C00102600081300840900C20400610280300140980020
          4C00102600081300840900C204006102803001409800204C0010260008130084
          0900C204006102803001409800204C00102600081300840900C2040061028030
          01409800204C00102600081300840900C204006102803001409800204C001026
          00081300840900C204006102803001409800204C00102600081300840900C204
          006102803001409800204C00102600081300840900C204006102803001409800
          204C00102600081300840900C204006102803001409800204C00102600081300
          840900C204006102803001409800204C00102600081300840900C20400610280
          3001409800204C00102600081300840900C204006102803001409800204C0010
          2600081300840900C204006102803001409800204C00102600081300840900C2
          04006102803001409800204C00102600081300840900C2040061028030014098
          00204C00102600081300840900C204006102803001409800204C001026000813
          00840900C204006102803001409800204C00102600081300840900C204006102
          803001409800204C00102600081300840900C204006102803001409800204C00
          102600081300840900C204006102803001409800204C00102600081300840900
          C204006102803001409800204C00102600081300840900C20400610280300140
          9800204C00102600081300840900C20400610280300140980020EC0012FE7E2E
          04C418B60000000049454E44AE426082}
      end
      object cbNonOverdue: TCheckBox
        Left = 38
        Top = 100
        Width = 163
        Height = 17
        Cursor = crHandPoint
        TabStop = False
        Caption = 'Reminder with cut-off date'
        TabOrder = 0
        OnClick = cbNonOverdueClick
        OnKeyUp = cbNonOverdueKeyUp
      end
      object cbOverdueOnly: TCheckBox
        Left = 38
        Top = 77
        Width = 163
        Height = 17
        Cursor = crHandPoint
        TabStop = False
        Caption = 'Overdue only (reminder)'
        TabOrder = 1
        OnClick = cbOverdueOnlyClick
        OnKeyUp = cbOverdueOnlyKeyUp
      end
      object cbShowAll: TCheckBox
        Left = 39
        Top = 52
        Width = 163
        Height = 17
        Cursor = crHandPoint
        TabStop = False
        Caption = 'Show all items (statement)'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbShowAllClick
        OnKeyUp = cbShowAllKeyUp
      end
    end
  end
  object PanelList: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 685
    Height = 693
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
      Top = 20
      Width = 645
      Height = 653
      Margins.Left = 20
      Margins.Top = 20
      Margins.Right = 20
      Margins.Bottom = 20
      Align = alClient
      BevelOuter = bvNone
      Caption = 'PanelEmailContainer'
      TabOrder = 0
      object CustomerList: TListView
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 635
        Height = 643
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
        OnKeyUp = CustomerListKeyUp
      end
    end
  end
end
