object PhoneListForm: TPhoneListForm
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderIcons = [biSystemMenu]
  Caption = 'Unity'
  ClientHeight = 406
  ClientWidth = 253
  Color = 15855854
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ParentBiDiMode = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelPhoneList: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 243
    Height = 346
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object PanelMemo: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 223
      Height = 326
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      BevelOuter = bvNone
      Caption = 'PanelMemo'
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object PhoneList: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 219
        Height = 322
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
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object PanelButtons: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 356
    Width = 253
    Height = 50
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object btnSubmit: TSpeedButton
      Left = 15
      Top = 13
      Width = 85
      Height = 27
      Cursor = crHandPoint
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Caption = 'Submit'
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
      OnClick = btnSubmitClick
    end
    object ImageGrip: TImage
      AlignWithMargins = True
      Left = 237
      Top = 34
      Width = 16
      Height = 16
      Margins.Left = 0
      Margins.Top = 34
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
        001008060000001FF3FF61000000017352474200AECE1CE90000000467414D41
        0000B18F0BFC6105000000097048597300000EC200000EC20115284A80000000
        324944415478DA63FC0F040C1400C6510306D08023478E0CB001F40F0398936D
        6C6CC8F302C506D03F0C703999682F106B000083615F99D1940F550000000049
        454E44AE426082}
      ExplicitLeft = 1046
      ExplicitTop = 0
    end
  end
end
