object TicketForm: TTicketForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 465
  ClientWidth = 531
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 521
    Height = 455
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitLeft = -58
    ExplicitWidth = 528
    ExplicitHeight = 459
  end
  object btnSelect: TSpeedButton
    Left = 440
    Top = 419
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Select'
    OnClick = btnSelectClick
  end
  object btnCancel: TSpeedButton
    Left = 348
    Top = 419
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Cancel'
    OnClick = btnCancelClick
  end
  object sgTicketList: TStringGrid
    Left = 17
    Top = 16
    Width = 496
    Height = 385
    ColCount = 6
    DefaultColWidth = 10
    FixedColor = clWhite
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    TabOrder = 0
    ColWidths = (
      10
      10
      10
      10
      10
      10)
  end
end
