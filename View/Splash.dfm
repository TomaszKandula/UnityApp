object SplashForm: TSplashForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Unity'
  ClientHeight = 428
  ClientWidth = 843
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ShapeBackground: TShape
    Left = 0
    Top = 0
    Width = 843
    Height = 428
    Align = alClient
    Pen.Color = clSkyBlue
    Pen.Width = 2
    ExplicitLeft = 28
    ExplicitTop = 78
    ExplicitWidth = 449
    ExplicitHeight = 29
  end
  object ShapeProgressBar: TShape
    Left = 195
    Top = 226
    Width = 449
    Height = 29
    Pen.Color = clSkyBlue
  end
  object MainText1: TLabel
    Left = 197
    Top = 72
    Width = 24
    Height = 35
    Cursor = crAppStart
    Caption = 'U'
    Color = 6566857
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -29
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object MainText2: TLabel
    Left = 223
    Top = 72
    Width = 424
    Height = 35
    Cursor = crAppStart
    Caption = 'nity for Debt Management'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6566857
    Font.Height = -29
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentFont = False
    Transparent = True
  end
  object SubText: TLabel
    Left = 494
    Top = 113
    Width = 153
    Height = 16
    Cursor = crAppStart
    Caption = 'Desktop Edition 2018'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqClearTypeNatural
    ParentFont = False
  end
  object ProgressBar: TGauge
    AlignWithMargins = True
    Left = 197
    Top = 228
    Width = 445
    Height = 25
    Cursor = crAppStart
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    BorderStyle = bsNone
    Color = clWhite
    ForeColor = 15918295
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Progress = 0
    ShowText = False
  end
  object TextFooter2B: TLabel
    AlignWithMargins = True
    Left = 160
    Top = 383
    Width = 529
    Height = 13
    Cursor = crAppStart
    Margins.Left = 15
    Margins.Top = 0
    Margins.Right = 15
    Margins.Bottom = 5
    Caption = 
      'This product is protected by Polish and international copyright ' +
      'and intellectual property laws.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object TextFooterA: TLabel
    AlignWithMargins = True
    Left = 223
    Top = 365
    Width = 393
    Height = 13
    Cursor = crAppStart
    Margins.Left = 15
    Margins.Top = 5
    Margins.Right = 15
    Margins.Bottom = 5
    Caption = 
      'Copyright '#169' 2016 - 2018 DFDS Polska Sp. z o.o. All rights reserv' +
      'ed.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object TextProgress: TLabel
    Left = 412
    Top = 234
    Width = 17
    Height = 13
    Alignment = taCenter
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TextStatus: TLabel
    AlignWithMargins = True
    Left = 197
    Top = 207
    Width = 4
    Height = 13
    Cursor = crAppStart
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 5
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
end
