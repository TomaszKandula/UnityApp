object FilterForm: TFilterForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Unity'
  ClientHeight = 378
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
    Height = 368
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 33
    ExplicitWidth = 1039
    ExplicitHeight = 407
    object btnFilter: TSpeedButton
      AlignWithMargins = True
      Left = 5
      Top = 338
      Width = 229
      Height = 25
      Cursor = crHandPoint
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
    object PanelListItems: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 35
      Width = 229
      Height = 293
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitTop = 5
      ExplicitWidth = 1029
      ExplicitHeight = 228
      object FilterList: TCheckListBox
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 225
        Height = 289
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
        ExplicitWidth = 330
        ExplicitHeight = 357
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
      ExplicitTop = 3
      ExplicitWidth = 1049
    end
  end
end
