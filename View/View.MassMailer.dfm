object MassMailerForm: TMassMailerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 648
  ClientWidth = 1187
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
    Top = 598
    Width = 1187
    Height = 50
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
    object btnCancel: TSpeedButton
      Left = 974
      Top = 13
      Width = 89
      Height = 27
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
      Left = 1082
      Top = 13
      Width = 89
      Height = 27
      Cursor = crHandPoint
      Caption = 'Send'
      Flat = True
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C006000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF80808080808080
        8080808080808080808080808080808080808080808080808080808080808080
        808080808080808080808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFF
        FFFFFFFFFF808080808080808080808080808080808080808080FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF808080808080FFFF
        FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF8080808080808080808080
        80808080808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF808080FFFFFFFFFFFF808080808080808080FFFFFF808080FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFB8824DB8824DFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFB8824DB8824DFFFFFF808080FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FF
        FFFF808080808080808080808080FFFFFF808080808080808080808080808080
        8080808080808080808080808080808080808080808080808080808080808080
        80808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
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
    object btnApply: TSpeedButton
      Left = 35
      Top = 13
      Width = 118
      Height = 27
      Cursor = crHandPoint
      Caption = 'Apply settings'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFEEF6EB8CC17752A33437941437941452A2338BC177EEF6EBFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF78B7611B85002E8F0934931137
        94143794143493112E8F091B850077B65FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        4DA02D288C03379414379414288C03349311389515389515389515379414298D
        034B9F2BFFFFFFFFFFFFFFFFFF79B761288C03389515379414208800D1E6CB3F
        991F349311389515389515389515389515298D0377B65FFFFFFFEFF6EC1B8500
        3794143895152B8E066BB053FFFFFFA6D098298D033895153895153895153895
        153794141B8500EEF5EB8DC2792E8F093895152F900B3B9619FFFFFFFFFFFFFF
        FFFF268B003694133895153895153895153895152E8F098BC17753A335349310
        2E9009399518FFFFFFFFFFFFF6FAF5FFFFFF98C887298D033895153895153895
        1538951534931152A23338951532910E5DA942FFFFFFFFFFFFEDF5EB479D29FF
        FFFFFFFFFF2B8E07359311389515389515389515379414379414389515349311
        3D981CFFFFFFFFFFFF3C971B198400ECF5EAFFFFFFD2E7CC1A85003794143895
        1538951537941437941454A33634931031910D4B9F2D3C971B31910D31910C5D
        A841FFFFFFFFFFFFA5CF971C860037941438951534931152A2348DC2792D8F09
        389515349310359311389515379514218800C7E1BFFFFFFFFFFFFFB7D9AC1C86
        003493112E8F098CC177F0F7ED1B850037941438951538951538951538951536
        9413238900EFF6EDFFFFFFFFFFFFF4F9F3419A20198400EEF6EBFFFFFF7AB863
        288C02389515389515389515389515389515359311228900C9E2C1FFFFFFEEF6
        ED23890077B65FFFFFFFFFFFFFFFFFFF4EA12F288C0237941438951538951538
        951538951536941320880078B7621D86004A9E2AFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF7AB8631B85002D8F093493103794143794143493102E8F0914810078B7
        60FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F7ED8DC27954A33638
        951538951553A3358DC279EFF6ECFFFFFFFFFFFFFFFFFFFFFFFF}
      Spacing = 10
      OnClick = btnApplyClick
    end
  end
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 640
    Top = 10
    Width = 537
    Height = 578
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alRight
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object TextMessage: TLabel
      AlignWithMargins = True
      Left = 20
      Top = 68
      Width = 497
      Height = 13
      Margins.Left = 20
      Margins.Top = 0
      Margins.Right = 20
      Margins.Bottom = 10
      Align = alTop
      Caption = 'Custom message:'
      ExplicitWidth = 85
    end
    object TextSubject: TLabel
      AlignWithMargins = True
      Left = 20
      Top = 10
      Width = 497
      Height = 13
      Margins.Left = 20
      Margins.Top = 10
      Margins.Right = 20
      Margins.Bottom = 10
      Align = alTop
      Caption = 'Custom subject:'
      ExplicitWidth = 78
    end
    object PanelMessage: TPanel
      AlignWithMargins = True
      Left = 15
      Top = 91
      Width = 507
      Height = 315
      Margins.Left = 15
      Margins.Top = 0
      Margins.Right = 15
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object Text_Message: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 503
        Height = 311
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
      AlignWithMargins = True
      Left = 15
      Top = 33
      Width = 507
      Height = 25
      Margins.Left = 15
      Margins.Top = 0
      Margins.Right = 15
      Margins.Bottom = 10
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Text_Subject: TEdit
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 503
        Height = 21
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
        ParentFont = False
        TabOrder = 0
        OnKeyUp = Text_SubjectKeyUp
      end
    end
    object PanelOption: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 416
      Width = 527
      Height = 157
      Margins.Left = 5
      Margins.Top = 10
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object GroupOptions: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 0
        Width = 507
        Height = 147
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alClient
        Caption = 'Invoice options'
        TabOrder = 0
        object GroupFiltering: TGroupBox
          Left = 232
          Top = 31
          Width = 233
          Height = 97
          Caption = 'Due date filter (optional)'
          TabOrder = 0
          object Text_Begin: TLabel
            Left = 23
            Top = 34
            Width = 58
            Height = 13
            Caption = 'Begins with:'
          end
          object Text_End: TLabel
            Left = 23
            Top = 60
            Width = 50
            Height = 13
            Caption = 'Ends with:'
          end
          object ValBeginDate: TLabel
            Left = 101
            Top = 34
            Width = 60
            Height = 13
            Caption = 'yyyy-mm-dd'
          end
          object ValEndDate: TLabel
            Left = 101
            Top = 60
            Width = 60
            Height = 13
            Caption = 'yyyy-mm-dd'
          end
          object btnBeginDate: TSpeedButton
            Left = 173
            Top = 32
            Width = 20
            Height = 20
            Cursor = crHandPoint
            Flat = True
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F003000000000000000000000000000000000000FCFBF9F6F0ED
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000F4EDE9C29979D0AF8FF2EDECFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFD0AE8EAE7033B98553D1B194FFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFF4EFEFB98553C09369EADED8D7BDA9F8F3F2FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              D7BAA0E8DCD5D4B8A3B47A42C19773F6F0EEFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFDCC7B6B278
              3EB67F48B57D45C29874FBFAFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFDFDFECBA98DB57C44B7814BB4
              7B42B68559FBF9FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB2783DB8824DB47B42C29874
              F6F0EEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFC29876B2783DB7814BB57D45C29874FBFAFAFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFCCAA8FB57C44B7814BB47B42B68559FBF9FAFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB
              FAFBCCAA8DB2783DB8824DB47B42C29874F6F0EEFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC29876
              B2783DB7814BB57D45C29874FAF8F8FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCAA8FB57C44B781
              4BB3783DBA8C66FFFFFFFDFBFBFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB17438BA8859DDC7B7E6
              D8CFF3EBE5FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFC5A084DCC6B6E5D8D3B57E4ABE916AF8F4F2
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFE7D9D1B67F4AB3783EC59D7BFBF9F80000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFF9F6F4C9A485BF926BF5EFECFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFAF8
              F4F1FFFFFFFFFFFF0000}
            OnClick = btnBeginDateClick
          end
          object btnDelBegin: TSpeedButton
            Left = 199
            Top = 32
            Width = 20
            Height = 20
            Cursor = crHandPoint
            Flat = True
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F003000000000000000000000000000000000000FCFBF9F6F0ED
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6DBF45E73
              D83456D53053D48A97E0E4E7F8FFFFFF0000F4EDE9C29979D0AF8FF2EDECFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBDC5EE2C4FD38092E1A0AFEAACBAEC3A
              59D54C69D8E4E7F70000FFFFFFD0AE8EAE7033B98553D1B194FFFFFFFFFFFFFF
              FFFFFFFFFFF2F2FA5A71D9778ADEFFFFFFFFFFFFC2C6ED3555D43A5AD58A97E0
              0000FFFFFFF4EFEFB98553C09369EADED8D7BDA9F8F3F2FFFFFFFFFFFFD4DBF5
              1C42D0E0E5F8FEFEFEA3AFE83051D3C2C6EDACBAED3153D40000FFFFFFFFFFFF
              D7BAA0E8DCD5D4B8A3B47A42C19773F6F0EEFFFFFFCFD6F41F45D1E5E8F8A7B1
              E93F5CD3A3AEE8FFFFFFA0AFEB3456D50000FFFFFFFFFFFFFFFFFFDCC7B6B278
              3EB67F48B57D45C29874FFFEFBEAEEFE4460D57487DF3F5CD5A8B2E9FEFEFEFF
              FFFF8092E15E73D80000FFFFFFFFFFFFFFFFFFFDFDFECBA98DB57C44B7814BB4
              7B42B78559FFFEFBA6B4EF2B4FD37387DFE5E8F8E0E5F8778ADE2C4FD3D6DBF4
              0000FFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB2783DB8824DB47B42C59B75
              F5EEECA5B4EF435FD41F44D01C42D05A71D8BCC5EEFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFC29876B2783DB7814BB57D45C59A75FFFEFAE9ED
              FEDAE0F7D9DFF7F1F2FAFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFCCAA8FB57C44B7814BB47B42B78559FFFDFBFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB
              FAFBCCAA8DB2783DB8824DB47B42C29874F6F0EEFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC29876
              B2783DB7814BB57D45C29874FAF8F8FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCAA8FB57C44B781
              4BB3783DBA8C66FFFFFFFDFBFBFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB17438BA8859DDC7B7E6
              D8CFF3EBE5FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFC5A084DCC6B6E5D8D3B57E4ABE916AF8F4F2
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFE7D9D1B67F4AB3783EC59D7BFBF9F80000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFF9F6F4C9A485BF926BF5EFECFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFAF8
              F4F1FFFFFFFFFFFF0000}
            OnClick = btnDelBeginClick
          end
          object btnEndDate: TSpeedButton
            Left = 173
            Top = 58
            Width = 20
            Height = 20
            Cursor = crHandPoint
            Flat = True
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F003000000000000000000000000000000000000FCFBF9F6F0ED
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000F4EDE9C29979D0AF8FF2EDECFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFD0AE8EAE7033B98553D1B194FFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFF4EFEFB98553C09369EADED8D7BDA9F8F3F2FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              D7BAA0E8DCD5D4B8A3B47A42C19773F6F0EEFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFDCC7B6B278
              3EB67F48B57D45C29874FBFAFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFDFDFECBA98DB57C44B7814BB4
              7B42B68559FBF9FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB2783DB8824DB47B42C29874
              F6F0EEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFC29876B2783DB7814BB57D45C29874FBFAFAFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFCCAA8FB57C44B7814BB47B42B68559FBF9FAFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB
              FAFBCCAA8DB2783DB8824DB47B42C29874F6F0EEFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC29876
              B2783DB7814BB57D45C29874FAF8F8FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCAA8FB57C44B781
              4BB3783DBA8C66FFFFFFFDFBFBFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB17438BA8859DDC7B7E6
              D8CFF3EBE5FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFC5A084DCC6B6E5D8D3B57E4ABE916AF8F4F2
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFE7D9D1B67F4AB3783EC59D7BFBF9F80000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFF9F6F4C9A485BF926BF5EFECFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFAF8
              F4F1FFFFFFFFFFFF0000}
            OnClick = btnEndDateClick
          end
          object btnDelEnd: TSpeedButton
            Left = 199
            Top = 58
            Width = 20
            Height = 20
            Cursor = crHandPoint
            Flat = True
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F003000000000000000000000000000000000000FCFBF9F6F0ED
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6DBF45E73
              D83456D53053D48A97E0E4E7F8FFFFFF0000F4EDE9C29979D0AF8FF2EDECFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBDC5EE2C4FD38092E1A0AFEAACBAEC3A
              59D54C69D8E4E7F70000FFFFFFD0AE8EAE7033B98553D1B194FFFFFFFFFFFFFF
              FFFFFFFFFFF2F2FA5A71D9778ADEFFFFFFFFFFFFC2C6ED3555D43A5AD58A97E0
              0000FFFFFFF4EFEFB98553C09369EADED8D7BDA9F8F3F2FFFFFFFFFFFFD4DBF5
              1C42D0E0E5F8FEFEFEA3AFE83051D3C2C6EDACBAED3153D40000FFFFFFFFFFFF
              D7BAA0E8DCD5D4B8A3B47A42C19773F6F0EEFFFFFFCFD6F41F45D1E5E8F8A7B1
              E93F5CD3A3AEE8FFFFFFA0AFEB3456D50000FFFFFFFFFFFFFFFFFFDCC7B6B278
              3EB67F48B57D45C29874FFFEFBEAEEFE4460D57487DF3F5CD5A8B2E9FEFEFEFF
              FFFF8092E15E73D80000FFFFFFFFFFFFFFFFFFFDFDFECBA98DB57C44B7814BB4
              7B42B78559FFFEFBA6B4EF2B4FD37387DFE5E8F8E0E5F8778ADE2C4FD3D6DBF4
              0000FFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB2783DB8824DB47B42C59B75
              F5EEECA5B4EF435FD41F44D01C42D05A71D8BCC5EEFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFC29876B2783DB7814BB57D45C59A75FFFEFAE9ED
              FEDAE0F7D9DFF7F1F2FAFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFCCAA8FB57C44B7814BB47B42B78559FFFDFBFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB
              FAFBCCAA8DB2783DB8824DB47B42C29874F6F0EEFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC29876
              B2783DB7814BB57D45C29874FAF8F8FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCAA8FB57C44B781
              4BB3783DBA8C66FFFFFFFDFBFBFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFBCCAA8DB17438BA8859DDC7B7E6
              D8CFF3EBE5FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFC5A084DCC6B6E5D8D3B57E4ABE916AF8F4F2
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFE7D9D1B67F4AB3783EC59D7BFBF9F80000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFF9F6F4C9A485BF926BF5EFECFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFAF8
              F4F1FFFFFFFFFFFF0000}
            OnClick = btnDelEndClick
          end
          object ImgCover: TImage
            Left = 16
            Top = 22
            Width = 209
            Height = 65
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
        end
        object cbShowAll: TRadioButton
          Left = 24
          Top = 38
          Width = 154
          Height = 17
          Cursor = crHandPoint
          Caption = 'Show all items (statement)'
          TabOrder = 1
          OnClick = cbShowAllClick
          OnKeyUp = cbShowAllKeyUp
        end
        object cbOverdueOnly: TRadioButton
          Left = 24
          Top = 61
          Width = 154
          Height = 17
          Cursor = crHandPoint
          Caption = 'Overdue only (reminder)'
          TabOrder = 2
          OnClick = cbOverdueOnlyClick
          OnKeyUp = cbOverdueOnlyKeyUp
        end
        object cbNonOverdue: TRadioButton
          Left = 24
          Top = 84
          Width = 154
          Height = 17
          Cursor = crHandPoint
          Caption = 'Reminder with cut-off date'
          TabOrder = 3
          OnClick = cbNonOverdueClick
          OnKeyUp = cbNonOverdueKeyUp
        end
        object cbNotDueOnly: TRadioButton
          Left = 24
          Top = 107
          Width = 154
          Height = 17
          Cursor = crHandPoint
          Caption = 'Show not due items only'
          TabOrder = 4
          OnClick = cbNotDueOnlyClick
          OnKeyUp = cbNotDueOnlyKeyUp
        end
      end
    end
  end
  object PanelList: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 615
    Height = 578
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
      Top = 221
      Width = 575
      Height = 337
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
        Width = 565
        Height = 327
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BorderStyle = bsNone
        Checkboxes = True
        Columns = <>
        ColumnClick = False
        FlatScrollBars = True
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
        OnKeyUp = CustomerListKeyUp
        OnItemChecked = CustomerListItemChecked
      end
    end
    object GroupEmails: TGroupBox
      AlignWithMargins = True
      Left = 20
      Top = 20
      Width = 575
      Height = 181
      Margins.Left = 20
      Margins.Top = 20
      Margins.Right = 20
      Margins.Bottom = 0
      Align = alTop
      Caption = 'E-mail settings'
      TabOrder = 1
      object txtCompany: TLabel
        Left = 27
        Top = 40
        Width = 82
        Height = 13
        Caption = 'Source DB name:'
      end
      object shapeLbuEmails: TShape
        Left = 24
        Top = 70
        Width = 220
        Height = 75
        Pen.Color = 14474460
      end
      object Text_Warn: TLabel
        Left = 24
        Top = 151
        Width = 211
        Height = 13
        Caption = '* E-mail template is available only in English.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object selCompany: TComboBox
        Left = 128
        Top = 37
        Width = 116
        Height = 22
        Cursor = crHandPoint
        Style = csOwnerDrawFixed
        TabOrder = 0
        OnSelect = selCompanySelect
      end
      object lstLbuEmails: TListBox
        Left = 27
        Top = 74
        Width = 214
        Height = 67
        Style = lbOwnerDrawFixed
        BorderStyle = bsNone
        Items.Strings = (
          'A'
          'B'
          'C'
          'D'
          'E'
          'F')
        Sorted = True
        TabOrder = 1
      end
      object grSettings: TGroupBox
        Left = 272
        Top = 24
        Width = 281
        Height = 137
        Caption = 'Global settings'
        TabOrder = 2
        object cbCtrlStatusOff: TCheckBox
          Left = 24
          Top = 77
          Width = 145
          Height = 17
          Cursor = crHandPoint
          Caption = 'Hide control status column'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 0
        end
        object cbUserInCopy: TCheckBox
          Left = 24
          Top = 31
          Width = 145
          Height = 17
          Cursor = crHandPoint
          Caption = 'Include me in the copy'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5592405
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 1
        end
        object cbMergeList: TCheckBox
          Left = 24
          Top = 100
          Width = 145
          Height = 17
          Cursor = crHandPoint
          Caption = 'Merge emails into one'
          Color = clWhite
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 2
        end
        object cbIncludeSource: TCheckBox
          Left = 24
          Top = 54
          Width = 145
          Height = 17
          Cursor = crHandPoint
          Caption = 'Include source address'
          Checked = True
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          State = cbChecked
          TabOrder = 3
        end
      end
    end
  end
end
