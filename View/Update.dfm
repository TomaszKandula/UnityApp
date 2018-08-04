object UpdateForm: TUpdateForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Unity'
  ClientHeight = 373
  ClientWidth = 704
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Background: TShape
    Left = 0
    Top = 0
    Width = 704
    Height = 373
    Align = alClient
    Pen.Color = clSkyBlue
    Pen.Width = 2
    ExplicitLeft = 8
    ExplicitWidth = 636
    ExplicitHeight = 300
  end
  object MainText1: TLabel
    Left = 127
    Top = 43
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
    Left = 151
    Top = 43
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
    Transparent = False
  end
  object Text1: TLabel
    Left = 177
    Top = 128
    Width = 342
    Height = 18
    Caption = 'Running automatic update, please wait...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object Text4: TLabel
    Left = 24
    Top = 336
    Width = 444
    Height = 13
    Cursor = crAppStart
    Caption = 
      'This product is protected by Polish and international copyright ' +
      'and intellectual property laws.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object BottomLine: TBevel
    Left = 24
    Top = 291
    Width = 657
    Height = 19
    Cursor = crAppStart
    Shape = bsBottomLine
  end
  object Text3: TLabel
    Left = 24
    Top = 316
    Width = 327
    Height = 13
    Cursor = crAppStart
    Caption = 
      'Copyright '#169' 2016 - 2018 DFDS Polska Sp. z o.o. All rights reserv' +
      'ed.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ShapeProgressBar: TShape
    Left = 175
    Top = 182
    Width = 346
    Height = 21
    Pen.Color = clSkyBlue
  end
  object Progress: TGauge
    Left = 177
    Top = 184
    Width = 342
    Height = 17
    Cursor = crAppStart
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
  object Text2: TLabel
    Left = 233
    Top = 240
    Width = 249
    Height = 18
    Caption = 'Do not turn off your computer.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
end
