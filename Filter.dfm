object FilterForm: TFilterForm
  AlignWithMargins = True
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 355
  ClientWidth = 233
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
  object FilterList: TCheckListBox
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 223
    Height = 312
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 0
    TabStop = False
    Align = alClient
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
    ExplicitLeft = 13
    ExplicitTop = 34
    ExplicitWidth = 252
    ExplicitHeight = 376
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 322
    Width = 223
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 423
    ExplicitWidth = 270
    object btnFilter: TSpeedButton
      Left = 150
      Top = 2
      Width = 73
      Height = 25
      Cursor = crHandPoint
      Caption = 'Filter'
      OnClick = btnFilterClick
    end
    object cbSelectAll: TCheckBox
      Left = 0
      Top = 6
      Width = 77
      Height = 17
      Caption = 'Select All'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbSelectAllClick
    end
  end
end
