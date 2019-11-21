object CompanyListForm: TCompanyListForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 403
  ClientWidth = 257
  Color = 15855854
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBackground: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 247
    Height = 393
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object btnSelect: TSpeedButton
      AlignWithMargins = True
      Left = 20
      Top = 356
      Width = 207
      Height = 27
      Cursor = crHandPoint
      Margins.Left = 20
      Margins.Top = 10
      Margins.Right = 20
      Margins.Bottom = 10
      Align = alBottom
      Caption = 'Open selected'
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
      Spacing = 15
      OnClick = btnSelectClick
      ExplicitLeft = 10
      ExplicitTop = 358
      ExplicitWidth = 227
    end
    object PanelListItems: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 35
      Width = 237
      Height = 306
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object FilterList: TCheckListBox
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 233
        Height = 302
        Cursor = crHandPoint
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        TabStop = False
        OnClickCheck = FilterListClickCheck
        Align = alClient
        BorderStyle = bsNone
        ItemHeight = 13
        Items.Strings = (
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 1'
          'Test item 14'
          'Test item 14'
          'Test item 16'
          'Test item 16'
          'Test item 2'
          'Test item 2'
          'Test item 21'
          'Test item 21'
          'Test item 21'
          'Test item 21'
          'Test item 4'
          'Test item 4'
          'Test item 45'
          'Test item 45'
          'Test item 5'
          'Test item 5'
          'Test item 7'
          'Test item 7'
          'Test item 8'
          'Test item 8'
          'Test item 81'
          'Test item 81')
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        OnClick = FilterListClick
      end
    end
    object cbSelectAll: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 237
      Height = 20
      Cursor = crHandPoint
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Select All'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbSelectAllClick
    end
  end
end
