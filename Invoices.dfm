object InvoicesForm: TInvoicesForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Unity'
  ClientHeight = 448
  ClientWidth = 588
  Color = 15527148
  Constraints.MaxHeight = 1050
  Constraints.MaxWidth = 1280
  Constraints.MinHeight = 320
  Constraints.MinWidth = 580
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InvoicesGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 588
    Height = 429
    Align = alClient
    BorderStyle = bsNone
    Color = clWhite
    ColCount = 4
    DefaultColWidth = 10
    DefaultRowHeight = 17
    DrawingStyle = gdsGradient
    FixedColor = clSilver
    RowCount = 2
    GradientEndColor = 15527148
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goThumbTracking]
    TabOrder = 0
    OnDrawCell = InvoicesGridDrawCell
    OnKeyDown = InvoicesGridKeyDown
    ColWidths = (
      10
      183
      163
      179)
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 429
    Width = 588
    Height = 19
    Panels = <>
    ParentColor = True
  end
end
