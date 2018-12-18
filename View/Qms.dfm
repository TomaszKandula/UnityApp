object QmsForm: TQmsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 650
  ClientWidth = 551
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPanel: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 601
    Width = 551
    Height = 49
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object btnLog: TSpeedButton
      Left = 431
      Top = 12
      Width = 90
      Height = 27
      Cursor = crHandPoint
      Caption = 'Log'
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
      OnClick = btnLogClick
    end
    object btnCancel: TSpeedButton
      Left = 316
      Top = 12
      Width = 90
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
      OnClick = btnCancelClick
    end
  end
  object MainPanel: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 531
    Height = 581
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object MainFrame: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 511
      Height = 561
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Caption = 'QMS'
      TabOrder = 0
      object MissingInvoiceBox: TGroupBox
        AlignWithMargins = True
        Left = 22
        Top = 35
        Width = 467
        Height = 334
        Margins.Left = 20
        Margins.Top = 20
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alClient
        Caption = 'Missing invoice'
        TabOrder = 0
        object Currency: TLabel
          Left = 49
          Top = 270
          Width = 44
          Height = 13
          Caption = 'Currency'
        end
        object bgEdit1: TShape
          Left = 47
          Top = 46
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit2: TShape
          Left = 47
          Top = 94
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit3: TShape
          Left = 47
          Top = 139
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit4: TShape
          Left = 47
          Top = 190
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit5: TShape
          Left = 47
          Top = 238
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit6: TShape
          Left = 263
          Top = 46
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit7: TShape
          Left = 263
          Top = 94
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit8: TShape
          Left = 263
          Top = 139
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit9: TShape
          Left = 263
          Top = 190
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit10: TShape
          Left = 263
          Top = 238
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object bgEdit11: TShape
          Left = 263
          Top = 284
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object btnAddDueDate: TSpeedButton
          Left = 424
          Top = 46
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
          OnClick = btnAddDueDateClick
        end
        object btnAddValDate: TSpeedButton
          Left = 424
          Top = 94
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
          OnClick = btnAddValDateClick
        end
        object EditInvoiceNo: TLabeledEdit
          Left = 48
          Top = 48
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 50
          EditLabel.Height = 13
          EditLabel.Caption = 'Invoice no'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          NumbersOnly = True
          ParentFont = False
          TabOrder = 0
        end
        object EditOpenAmount: TLabeledEdit
          Left = 48
          Top = 96
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 66
          EditLabel.Height = 13
          EditLabel.Caption = 'Open Amount'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnKeyPress = EditOpenAmountKeyPress
        end
        object EditAmount: TLabeledEdit
          Left = 48
          Top = 141
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 37
          EditLabel.Height = 13
          EditLabel.Caption = 'Amount'
          EditLabel.Font.Charset = DEFAULT_CHARSET
          EditLabel.Font.Color = clBlack
          EditLabel.Font.Height = -11
          EditLabel.Font.Name = 'Tahoma'
          EditLabel.Font.Style = []
          EditLabel.ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnKeyPress = EditAmountKeyPress
        end
        object EditOpenCurrAm: TLabeledEdit
          Left = 48
          Top = 192
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 113
          EditLabel.Height = 13
          EditLabel.Caption = 'Open Currency Amount'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnKeyPress = EditOpenCurrAmKeyPress
        end
        object EditDueDate: TLabeledEdit
          Left = 264
          Top = 48
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 115
          EditLabel.Height = 13
          EditLabel.Caption = 'Due date (yyyy-mm-dd)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 6
        end
        object EditValDate: TLabeledEdit
          Left = 264
          Top = 96
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 122
          EditLabel.Height = 13
          EditLabel.Caption = 'Value date (yyyy-mm-dd)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 7
        end
        object EditQueryReason: TLabeledEdit
          Left = 264
          Top = 141
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clWhite
          EditLabel.Width = 73
          EditLabel.Height = 13
          EditLabel.Caption = 'Query Reason:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 8
        end
        object EditLogType: TLabeledEdit
          Left = 264
          Top = 192
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clWhite
          EditLabel.Width = 42
          EditLabel.Height = 13
          EditLabel.Caption = 'Log type'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 9
        end
        object EditCurrAmount: TLabeledEdit
          Left = 48
          Top = 240
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 84
          EditLabel.Height = 13
          EditLabel.Caption = 'Currency Amount'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnKeyPress = EditCurrAmountKeyPress
        end
        object EditUserAlias: TLabeledEdit
          Left = 264
          Top = 240
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clWhite
          EditLabel.Width = 46
          EditLabel.Height = 13
          EditLabel.Caption = 'User alias'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 10
        end
        object EditStamp: TLabeledEdit
          Left = 264
          Top = 286
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clWhite
          EditLabel.Width = 30
          EditLabel.Height = 13
          EditLabel.Caption = 'Stamp'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 11
        end
        object CurrencyList: TComboBox
          Left = 48
          Top = 286
          Width = 153
          Height = 22
          BevelInner = bvLowered
          BevelKind = bkFlat
          BevelOuter = bvNone
          Style = csOwnerDrawFixed
          TabOrder = 5
        end
      end
      object StatusPanel: TPanel
        AlignWithMargins = True
        Left = 12
        Top = 379
        Width = 487
        Height = 170
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alBottom
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 1
        object StatusLabel: TLabel
          Left = 10
          Top = 7
          Width = 70
          Height = 13
          Caption = 'Query reason:'
        end
        object QueryDescBorders: TShape
          Left = 10
          Top = 86
          Width = 467
          Height = 75
          Pen.Color = 15527148
        end
        object QueryDescLabel: TLabel
          Left = 11
          Top = 67
          Width = 318
          Height = 13
          Caption = 
            'Provide explanation (you may incl. booking and other references)' +
            ':'
        end
        object bgEdit: TShape
          Left = 273
          Top = 24
          Width = 155
          Height = 25
          Pen.Color = 15527148
        end
        object btnAddAttcahement: TSpeedButton
          Left = 434
          Top = 24
          Width = 25
          Height = 25
          Cursor = crHandPoint
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF6565656B6B6B6B6B6B656565FFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF565656F2F2F2FF
            FFFFFFFFFFF2F2F2555555FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF5A5A5AFFFFFFFFFFFFFFFFFFFFFFFF5A5A5AFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF656565F7F7F73B
            3B3B3B3B3BF7F7F7656565FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF6F6F6F6A6A6AFFFFFFFFFFFF6B6B6B6F6F6FFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7070706F6F6FFF
            FFFFFFFFFF6F6F6F707070FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF707070707070FFFFFFFFFFFF707070707070FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF707070707070FF
            FFFFFFFFFF707070707070FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF707070707070FFFFFFFFFFFF707070707070FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF707070707070FF
            FFFFFFFFFF707070707070FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF707070707070FFFFFFFFFFFF707070707070FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7070706F6F6FFF
            FFFFFFFFFF7070706F6F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF6F6F6F6A6A6AFFFFFFFFFFFF6F6F6F6A6A6AFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF616161FFFFFFFF
            FFFFFFFFFF5B5B5BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF424242FFFFFFFFFFFFCBCBCB909090FFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6D6D65454546D
            6D6D676767FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = btnAddAttcahementClick
        end
        object StatusList: TComboBox
          Left = 10
          Top = 26
          Width = 161
          Height = 22
          Cursor = crHandPoint
          AutoComplete = False
          BevelInner = bvLowered
          BevelKind = bkFlat
          BevelOuter = bvNone
          Style = csOwnerDrawFixed
          Color = clWhite
          Sorted = True
          TabOrder = 0
          TabStop = False
          OnSelect = StatusListSelect
        end
        object QueryDesc: TMemo
          Left = 11
          Top = 87
          Width = 465
          Height = 73
          BorderStyle = bsNone
          Color = clCream
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 1024
          ParentFont = False
          TabOrder = 2
        end
        object LbuEmailAddress: TLabeledEdit
          Left = 274
          Top = 26
          Width = 153
          Height = 21
          BorderStyle = bsNone
          Color = clCream
          EditLabel.Width = 63
          EditLabel.Height = 13
          EditLabel.Caption = 'LBU address:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 255
          ParentFont = False
          TabOrder = 1
        end
      end
    end
  end
  object OpenDlgBox: TOpenDialog
    Filter = 'All files|*.*'
    Options = [ofReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Unity - attach file'
    Left = 244
    Top = 404
  end
end
