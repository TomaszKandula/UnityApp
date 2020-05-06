object FilterForm: TFilterForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Unity'
  ClientHeight = 426
  ClientWidth = 238
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
  PopupParent = MainForm.Owner
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBackground: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 228
    Height = 366
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object PanelListItems: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 45
      Width = 208
      Height = 306
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 15
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object FilterList: TCheckListBox
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 204
        Height = 302
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
    object PanelHeader: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 228
      Height = 40
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object btnRemove: TSpeedButton
        Left = 189
        Top = 8
        Width = 27
        Height = 27
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 10
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF758CE3B6
          C2F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDAE0F7889CE6FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF728AE23658D6D3DAF5FFFFFFFFFFFFFFFFFFDAE0
          F74E6CDBE6EAF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEDF0FB4E
          6CDB3D5ED7D7DEF6FFFFFFD7DEF64262D8AEBCEEFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8ECFA617CDF3F60D8A5B4EC4262D8728A
          E2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272727272727272FF
          FFFFFCFCFE5F7ADE3255D64C6ADAF4F6FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF727272B8B8B8FFFFFFEDF0FB758CE33255D65874DD6680E0859A
          E6F7F8FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272FFFFFFBFCAF13F
          60D83255D66B84E1F4F6FCFFFFFFC7D0F37990E3C9D2F3FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF727272FFFFFFA0B0EB4C6ADAB6C2F0FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFDFE4F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272B7B7B7FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFC2C2C2727272727272727272BCBCBCFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD3D3D373737372727272727272727272
          7272CCCCCCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0E0E0
          767676727272727272727272727272727272747474DADADAFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFEAEAEA7D7D7D72727272727272727272727272727272
          7272727272797979E5E5E5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8A8A8A727272
          727272727272727272727272727272727272727272727272808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF79797972727272727272727272727272727272727272
          7272727272727272727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Spacing = 15
      end
      object cbSelectAll: TCheckBox
        AlignWithMargins = True
        Left = 10
        Top = 10
        Width = 149
        Height = 25
        Cursor = crHandPoint
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 5
        Align = alLeft
        Caption = 'Select all items'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbSelectAllClick
      end
    end
  end
  object PanelButtons: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 376
    Width = 238
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
    object btnFilter: TSpeedButton
      Left = 15
      Top = 12
      Width = 85
      Height = 27
      Cursor = crHandPoint
      Margins.Left = 20
      Margins.Top = 10
      Margins.Right = 20
      Margins.Bottom = 10
      Caption = 'Filter'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72727272
        7272727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF727272727272727272FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72727272
        7272727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF727272727272727272FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF72727272
        7272727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF727272727272727272FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB7B7B772727272
        7272727272B7B7B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFB7B7B7727272727272727272727272727272B7B7B7FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB7B7B772727272727272727272
        7272727272727272727272B7B7B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        B7B7B7727272727272727272727272727272727272727272727272727272B7B7
        B7FFFFFFFFFFFFFFFFFFFFFFFFB7B7B772727272727272727272727272727272
        7272727272727272727272727272727272B7B7B7FFFFFFFFFFFFB7B7B7727272
        7272727272727272727272727272727272727272727272727272727272727272
        72727272B7B7B7FFFFFF72727272727272727272727272727272727272727272
        7272727272727272727272727272727272727272727272FFFFFF727272727272
        7272727272727272727272727272727272727272727272727272727272727272
        72727272727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Spacing = 15
      OnClick = btnFilterClick
    end
    object ImageGrip: TImage
      AlignWithMargins = True
      Left = 222
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
