object TrackerForm: TTrackerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 664
  ClientWidth = 966
  Color = 15855854
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  PopupMode = pmExplicit
  PopupParent = MainForm.Owner
  Position = poMainFormCenter
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 614
    Width = 966
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object btnOK: TSpeedButton
      Left = 776
      Top = 10
      Width = 75
      Height = 27
      Cursor = crHandPoint
      Caption = 'Save'
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
      OnClick = btnOKClick
    end
    object btnCancel: TSpeedButton
      Left = 864
      Top = 10
      Width = 75
      Height = 27
      Cursor = crHandPoint
      Caption = 'Close'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFF0F0F58E8EDE5656D33A3ACE3A3ACE5656D38E8EDEF0F0F5FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7C7CD92020D13333DA3535DA34
        34DA3434DA3535DA3333DA2020D17C7CD9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        5252D32F2FDC3B3BDC2929D84A4ADE7D7DE77D7DE74F4FE03A3ADB3E3EDC2F2F
        DC5252D3FFFFFFFFFFFFFFFFFF7F7FDE3131DD3939DC4141DEE3E3FAFFFFFFFF
        FFFFFFFFFFFFFFF93333DB3F3FDD4040DD3131DD7F7FDEFFFFFFF0F0F82626DA
        3E3EDF4242DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3434D63F3FDF4040DF3434
        DD4141DF2626DAF0F0F89393E93A3AE02E2EDDE3E3FAFFFFFFFFFFFFFFFFFFFF
        FFFF4C4CD83C3CE14242E03232DEC6C6F53232DE3A3AE09393E95F5FE33E3EE1
        5050E4FFFFFFFFFFFFFFFFFFFFFFFF6E6EDC3939E24444E22A2ADED1D1F8FFFF
        FF5050E43E3EE15E5EE34747E23E3EE38383EDFFFFFFFFFFFFFFFFFF9A9AE434
        34E34646E43232E1A6A6F2FFFFFFFFFFFF8383ED3E3EE34747E24848E53F3FE4
        8484ECFFFFFFFFFFFFC7C7ED2F2FE14848E53B3BE37B7BECFFFFFFFFFFFFFFFF
        FF8484EC3F3FE44848E56363EA4242E65454E4FFFFFFEBEBF53030DF4848E742
        42E65A5AE9FFFFFFFFFFFFFFFFFFFFFFFF5454E44242E66363EA9898F24242E7
        3939E7D9D9EE4242E14848E94747E84343E7FFFFFFFFFFFFFFFFFFFFFFFFE3E3
        F53737E64242E79898F2F2F2FD3333E74C4CEA3E3EE54B4BEA4B4BEA3737E7F5
        F5FEFFFFFFFFFFFFFFFFFFFFFFFE4B4BE24949EA3333E7F2F2FDFFFFFF8989F2
        4040EA4E4EEB4E4EEB4040EAE9E9FDFFFFFFFFFFFFFFFFFFE3E3F24D4DE14848
        EC4040EA8989F2FFFFFFFFFFFFFFFFFF6363EF4242EB4F4FED4C4CED5F5FE087
        87E58787E55858DF3B3BE84D4DEE4242EB6363EFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF8B8BF43838EC4848ED4B4BEF4949EF4949EF4B4BEF4848EE3838EC8B8B
        F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2F2FE9D9DF66B6BF253
        53F05353F06B6BF29D9DF6F2F2FEFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = btnCancelClick
    end
    object btnApply: TSpeedButton
      Left = 26
      Top = 10
      Width = 111
      Height = 27
      Cursor = crHandPoint
      Caption = 'Apply timings'
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
      OnClick = btnApplyClick
    end
    object btnSelection: TSpeedButton
      Left = 180
      Top = 10
      Width = 111
      Height = 27
      Cursor = crHandPoint
      Caption = 'Unselect all'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000000000FFFFFF
        606060606060FFFFFF000000FFFFFF606060606060FFFFFF000000FFFFFF6060
        60606060FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5FFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFF5F5F5F5F5F5FFFFFFFFFFFFF60606060606060606060606060
        6060606060606060606060606060606060FFFFFFFFFFFF5F5F5FFFFFFFFFFFFF
        FFFFFF6060606060606060606060606060606060606060606060606060606060
        60FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF606060606060FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF606060606060FFFFFFFFFFFF000000FFFFFFFFFFFF
        FFFFFF606060606060FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6060606060
        60FFFFFFFFFFFFFFFFFF5F5F5FFFFFFFFFFFFF606060606060FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF606060606060FFFFFFFFFFFF5F5F5F5F5F5FFFFFFF
        FFFFFF606060606060FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6060606060
        60FFFFFFFFFFFF5F5F5FFFFFFFFFFFFFFFFFFF606060606060FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF606060606060FFFFFFFFFFFFFFFFFF000000FFFFFF
        FFFFFF606060606060FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6060606060
        60FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF60606060606060606060606060
        6060606060606060606060606060606060FFFFFFFFFFFFFFFFFF5F5F5FFFFFFF
        FFFFFF6060606060606060606060606060606060606060606060606060606060
        60FFFFFFFFFFFF5F5F5F5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5FFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FFFFFF606060606060FFFFFF000000FFFFFF60
        6060606060FFFFFF000000FFFFFF606060606060FFFFFF000000}
      OnClick = btnSelectionClick
    end
  end
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 946
    Height = 594
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object GroupBoxClient: TGroupBox
      Left = 16
      Top = 13
      Width = 913
      Height = 388
      Caption = 'Selected Customers'
      TabOrder = 0
      TabStop = True
      object PanelCustomerList: TPanel
        AlignWithMargins = True
        Left = 12
        Top = 25
        Width = 889
        Height = 351
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object CustomerList: TListView
          AlignWithMargins = True
          Left = 15
          Top = 15
          Width = 859
          Height = 321
          Margins.Left = 15
          Margins.Top = 15
          Margins.Right = 15
          Margins.Bottom = 15
          Align = alClient
          BorderStyle = bsNone
          Columns = <>
          FlatScrollBars = True
          GridLines = True
          HideSelection = False
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnKeyDown = CustomerListKeyDown
          OnKeyUp = CustomerListKeyUp
          OnSelectItem = CustomerListSelectItem
        end
      end
    end
    object GroupBoxLeft: TGroupBox
      Left = 16
      Top = 407
      Width = 265
      Height = 170
      Caption = 'Layouts'
      TabOrder = 1
      TabStop = True
      object ErrorEmailFrom: TLabel
        Left = 144
        Top = 134
        Width = 57
        Height = 13
        Caption = 'Empty field!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 29
        Top = 32
        Width = 66
        Height = 13
        Caption = 'Layout name:'
      end
      object Label6: TLabel
        Left = 29
        Top = 87
        Width = 150
        Height = 13
        Caption = 'E-mail address to be sent from:'
      end
      object ListEmailFrom: TComboBox
        Left = 29
        Top = 106
        Width = 172
        Height = 22
        Cursor = crHandPoint
        BevelInner = bvLowered
        BevelKind = bkFlat
        BevelOuter = bvNone
        Style = csOwnerDrawFixed
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object ListLayout: TComboBox
        Left = 29
        Top = 51
        Width = 172
        Height = 22
        Cursor = crHandPoint
        BevelInner = bvLowered
        BevelKind = bkFlat
        BevelOuter = bvNone
        Style = csOwnerDrawFixed
        Color = clCream
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object GroupBoxMiddle: TGroupBox
      Left = 296
      Top = 407
      Width = 305
      Height = 170
      Caption = 'Timings'
      TabOrder = 2
      TabStop = True
      object TextReminder1: TLabeledEdit
        Left = 37
        Top = 106
        Width = 41
        Height = 21
        Color = clCream
        EditLabel.Width = 58
        EditLabel.Height = 13
        EditLabel.Caption = 'Reminder 1:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 2
        Text = '0'
      end
      object TextReminder2: TLabeledEdit
        Left = 133
        Top = 106
        Width = 41
        Height = 21
        Color = clCream
        EditLabel.Width = 58
        EditLabel.Height = 13
        EditLabel.Caption = 'Reminder 2:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 3
        Text = '0'
      end
      object TextReminder3: TLabeledEdit
        Left = 221
        Top = 107
        Width = 41
        Height = 21
        Color = clCream
        EditLabel.Width = 58
        EditLabel.Height = 13
        EditLabel.Caption = 'Reminder 3:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 4
        Text = '0'
      end
      object TextReminder4: TLabeledEdit
        Left = 133
        Top = 51
        Width = 41
        Height = 21
        Color = clCream
        EditLabel.Width = 62
        EditLabel.Height = 13
        EditLabel.Caption = 'Legal Action:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 1
        Text = '0'
      end
      object Exp_Rem3_Switch: TCheckBox
        Left = 221
        Top = 134
        Width = 68
        Height = 17
        Cursor = crHandPoint
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object Exp_Rem2_Switch: TCheckBox
        Left = 136
        Top = 133
        Width = 68
        Height = 17
        Cursor = crHandPoint
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object TextReminder0: TLabeledEdit
        Left = 37
        Top = 51
        Width = 41
        Height = 21
        Color = clCream
        EditLabel.Width = 79
        EditLabel.Height = 13
        EditLabel.Caption = 'Pre-statement*:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 0
        Text = '0'
      end
    end
    object Help: TGroupBox
      Left = 616
      Top = 407
      Width = 313
      Height = 170
      Caption = 'Help'
      TabOrder = 3
      object Memo: TMemo
        AlignWithMargins = True
        Left = 14
        Top = 27
        Width = 285
        Height = 129
        Margins.Left = 12
        Margins.Top = 12
        Margins.Right = 12
        Margins.Bottom = 12
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'Please select customer, provide desired timings and '
          'click "Apply timings" button. You may select more '
          'than one client and apply the same rules.'
          ''
          '*Pre-statement - assign if statement should be sent '
          'before any reminder goes out.'
          ''
          'Reminder 2 and 3 may be switched off.')
        ParentFont = False
        TabOrder = 0
      end
    end
  end
end
