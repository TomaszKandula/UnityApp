object LuncherForm: TLuncherForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Unity Luncher'
  ClientHeight = 372
  ClientWidth = 605
  Color = 14922344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ShapeBackground: TShape
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 599
    Height = 298
    Margins.Bottom = 0
    Align = alClient
    Brush.Color = 14922344
    Pen.Color = 14922344
    Pen.Width = 3
    OnMouseDown = ShapeBackgroundMouseDown
    ExplicitLeft = -358
    ExplicitTop = -91
    ExplicitWidth = 956
    ExplicitHeight = 579
  end
  object MainText: TLabel
    Left = 147
    Top = 23
    Width = 303
    Height = 45
    Caption = 'Unity Platform'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -37
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object TextSubtitle: TLabel
    Left = 188
    Top = 74
    Width = 231
    Height = 18
    Caption = 'Helping you along the way'
    Color = 15848108
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 15848108
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentColor = False
    ParentFont = False
  end
  object ShapeFooter: TShape
    AlignWithMargins = True
    Left = 3
    Top = 306
    Width = 599
    Height = 63
    Margins.Top = 0
    Align = alBottom
    Brush.Color = 15987699
    Pen.Color = 15987699
    ExplicitTop = 384
  end
  object TextFooterA: TLabel
    AlignWithMargins = True
    Left = 20
    Top = 321
    Width = 393
    Height = 13
    Margins.Left = 15
    Margins.Top = 5
    Margins.Right = 15
    Margins.Bottom = 5
    Caption = 
      'Copyright '#169' 2017 - 2020 DFDS Polska Sp. z o.o. All rights reserv' +
      'ed.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object TextFooterB: TLabel
    AlignWithMargins = True
    Left = 20
    Top = 339
    Width = 529
    Height = 13
    Margins.Left = 15
    Margins.Top = 0
    Margins.Right = 15
    Margins.Bottom = 5
    Caption = 
      'This product is protected by Polish and international copyright ' +
      'and intellectual property laws.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object ProgressBar: TGauge
    AlignWithMargins = True
    Left = 3
    Top = 301
    Width = 599
    Height = 5
    Margins.Top = 0
    Margins.Bottom = 0
    Align = alBottom
    BackColor = 15987699
    BorderStyle = bsNone
    Color = clWhite
    ForeColor = 35565
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Progress = 25
    ShowText = False
    ExplicitTop = 359
  end
  object TextStatus: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 283
    Width = 58
    Height = 13
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 5
    Caption = '{STATUS}'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object MainTextA: TLabel
    AlignWithMargins = True
    Left = 202
    Top = 195
    Width = 211
    Height = 18
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 5
    Caption = 'Lunching Unity Platform...'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object ShapeLine: TShape
    Left = 8
    Top = 120
    Width = 589
    Height = 2
    Brush.Color = 15987699
    Pen.Style = psClear
  end
end
