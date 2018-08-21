object FilterForm: TFilterForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Unity'
  ClientHeight = 408
  ClientWidth = 249
  Color = 15527148
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
  Scaled = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBackground: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 239
    Height = 398
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object btnFilter: TSpeedButton
      AlignWithMargins = True
      Left = 10
      Top = 333
      Width = 219
      Height = 25
      Cursor = crHandPoint
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alBottom
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
      ExplicitLeft = 0
      ExplicitTop = 376
      ExplicitWidth = 1039
    end
    object btnRemove: TSpeedButton
      AlignWithMargins = True
      Left = 10
      Top = 368
      Width = 219
      Height = 25
      Cursor = crHandPoint
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alBottom
      Caption = 'Remove'
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
      OnClick = btnRemoveClick
      ExplicitLeft = 0
      ExplicitTop = 381
      ExplicitWidth = 229
    end
    object PanelListItems: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 35
      Width = 229
      Height = 283
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
        Width = 225
        Height = 279
        Cursor = crHandPoint
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        TabStop = False
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
      Width = 229
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
