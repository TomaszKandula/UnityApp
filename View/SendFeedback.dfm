object ReportForm: TReportForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 456
  ClientWidth = 506
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 486
    Height = 396
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 491
    ExplicitHeight = 395
    object PanelArea: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 476
      Height = 386
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 481
      ExplicitHeight = 385
      object Caption: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 466
        Height = 13
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        Caption = 'User feedback:'
        Color = clWhite
        ParentColor = False
        ExplicitWidth = 73
      end
      object PanelReportMemo: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 28
        Width = 466
        Height = 353
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        ExplicitWidth = 471
        ExplicitHeight = 352
        object ReportMemo: TMemo
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 462
          Height = 349
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
          OnKeyUp = ReportMemoKeyUp
          ExplicitWidth = 467
          ExplicitHeight = 348
        end
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 416
    Width = 506
    Height = 40
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 415
    ExplicitWidth = 511
    object Text2: TLabel
      Left = 12
      Top = 14
      Width = 60
      Height = 13
      Caption = 'Word count:'
    end
    object TotalWords: TLabel
      Left = 102
      Top = 14
      Width = 40
      Height = 13
      Caption = '0 / 1024'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnCancel: TSpeedButton
      Left = 302
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
      OnClick = btnCancelClick
    end
    object btnSendReport: TSpeedButton
      Left = 402
      Top = 8
      Width = 89
      Height = 25
      Cursor = crHandPoint
      Caption = 'Send'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFF808080
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF747474FFFFFFFFFFFF1C1C1CE6E6E6FFFFFFFFFFFFFFFFFF91D7FF4F
        C3FF4DA0FF90C7FFFFFFFFFFFFFFFFFFFFE3E3E3020202FFFFFFFFFFFF939393
        494949FFFFFFFFFFFF01A0FF009FFF00ACFF007CFF007CFF007BFFFFFFFFFFFF
        FF333333878787FFFFFFFFFFFFFFFFFF66676800000017A8F500A8FF00A8FF00
        AFFF0081FF0088FF0086FF1589F5000000555555FFFFFFFFFFFFDDDDDDFFFFFF
        FFFFFFFFFFFF009EFF00A9FF00A8FF00AFFF0081FF0088FF0088FF0077FFFFFF
        FFFFFFFFFFFFFFD8D8D83F3F3F626262FFFFFFB6F2FF009FFF00A9FF00A8FF00
        B0FF0081FF0088FF0088FF007AFFB5E7FFFFFFFF5050502A2A2AFFFFFF333333
        10090515455D00B4FF00A8FF00AAFF00ACFF0080FA0089FF0087FF0091FF042E
        55000000191919FFFFFFFFFFFFFFFFFFFFFFFFB7ECFF00A0FF00ADFF00A3FF00
        84FF006FD80085FA008AFF007DFFB5E1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF5F94AD00B1FF0098FF0080FF008BFF0073DC0072DD007FEF008BFF5780
        A4FFFFFFFFFFFFFFFFFFFFFFFFD5D5D50000004987A00078FF0092FF0094FF00
        97FF0081FF0085FF0085FF005EEE3D75A1000000D0D0D0FFFFFFFFFFFF636363
        ABABABFFFFFFFFFFFF391D075C412B62463036200C3D2712140000FFFFFFFFFF
        FFA0A0A0535353FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF2223244D4E4F52
        53542C2D2D343535020303FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF6E6E6E4343435353532E2E2E282828575757FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF58585835
        35350B0B0B404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF020202FEFEFEFAFAFA000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5E5E5A9A9A9FF
        FFFFFFFFFFA6A6A6E1E1E1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Margin = 10
      ParentShowHint = False
      ShowHint = True
      Spacing = 10
      OnClick = btnSendReportClick
    end
  end
end
