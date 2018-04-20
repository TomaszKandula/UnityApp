object SearchForm: TSearchForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity - Search'
  ClientHeight = 229
  ClientWidth = 377
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 367
    Height = 219
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitTop = -269
    ExplicitWidth = 270
    ExplicitHeight = 413
  end
  object btnSearch: TSpeedButton
    Left = 279
    Top = 183
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Search'
    OnClick = btnSearchClick
  end
  object btnUnhide: TSpeedButton
    Left = 191
    Top = 183
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Unfold'
    Enabled = False
    OnClick = btnUnhideClick
  end
  object GroupOptions: TGroupBox
    Left = 24
    Top = 73
    Width = 161
    Height = 96
    Caption = 'Options'
    TabOrder = 2
    object CaseSensitive: TCheckBox
      Left = 24
      Top = 31
      Width = 73
      Height = 17
      Caption = 'Match case'
      TabOrder = 0
    end
    object ShowAll: TCheckBox
      Left = 24
      Top = 54
      Width = 97
      Height = 17
      Caption = 'Filter on found'
      TabOrder = 1
    end
  end
  object EditSearch: TLabeledEdit
    Left = 24
    Top = 40
    Width = 328
    Height = 21
    Color = clCream
    EditLabel.Width = 119
    EditLabel.Height = 13
    EditLabel.Caption = 'Customer name/number:'
    TabOrder = 0
  end
  object GroupSearch: TGroupBox
    Left = 191
    Top = 74
    Width = 161
    Height = 95
    Caption = 'Search direction'
    TabOrder = 1
    object CheckUp: TRadioButton
      Left = 16
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Up'
      TabOrder = 0
    end
    object CheckDown: TRadioButton
      Left = 16
      Top = 55
      Width = 113
      Height = 17
      Caption = 'Down'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
end
