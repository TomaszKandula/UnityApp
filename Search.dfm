object SearchForm: TSearchForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity - Search'
  ClientHeight = 137
  ClientWidth = 369
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
  PixelsPerInch = 96
  TextHeight = 13
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 359
    Height = 127
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
    Left = 272
    Top = 95
    Width = 73
    Height = 25
    Caption = 'Search'
    OnClick = btnSearchClick
  end
  object btnUnhide: TSpeedButton
    Left = 176
    Top = 95
    Width = 73
    Height = 25
    Caption = 'Unfold'
    Enabled = False
    OnClick = btnUnhideClick
  end
  object EditSearch: TLabeledEdit
    Left = 24
    Top = 40
    Width = 321
    Height = 21
    Color = clCream
    EditLabel.Width = 119
    EditLabel.Height = 13
    EditLabel.Caption = 'Customer name/number:'
    TabOrder = 0
  end
  object CaseSensitive: TCheckBox
    Left = 24
    Top = 80
    Width = 73
    Height = 17
    Caption = 'Match case'
    TabOrder = 1
  end
  object ShowAll: TCheckBox
    Left = 24
    Top = 103
    Width = 97
    Height = 17
    Caption = 'Filter on found'
    TabOrder = 2
  end
end
