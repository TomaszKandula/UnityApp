object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 455
  ClientWidth = 603
  Color = 15855854
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = MainForm.PopupMenu
  PopupMode = pmExplicit
  PopupParent = MainForm.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 593
    Height = 445
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = 14922344
    ExplicitLeft = 0
    ExplicitTop = 35
    ExplicitWidth = 599
    ExplicitHeight = 417
  end
  object Shape2: TShape
    Left = 15
    Top = 388
    Width = 570
    Height = 50
    Pen.Color = 15855854
  end
  object Shape3: TShape
    Left = 15
    Top = 15
    Width = 570
    Height = 49
    Pen.Color = 15855854
  end
  object Shape4: TShape
    Left = 15
    Top = 70
    Width = 570
    Height = 312
    Pen.Color = 15855854
  end
  object Text12: TLabel
    Left = 30
    Top = 396
    Width = 327
    Height = 13
    Caption = 
      'Copyright '#169' 2016 - 2019 DFDS Polska Sp. z o.o. All rights reserv' +
      'ed.'
  end
  object Text13: TLabel
    Left = 30
    Top = 415
    Width = 444
    Height = 13
    Caption = 
      'This product is protected by Polish and international copyright ' +
      'and intellectual property laws.'
  end
  object Text03: TLabel
    Left = 30
    Top = 87
    Width = 39
    Height = 13
    Caption = 'Version:'
  end
  object Text05: TLabel
    Left = 30
    Top = 230
    Width = 45
    Height = 13
    Caption = 'Inquiries:'
  end
  object Text07: TLabel
    Left = 30
    Top = 249
    Width = 81
    Height = 13
    Caption = 'Local IT support:'
  end
  object Text08: TLabel
    Left = 30
    Top = 311
    Width = 39
    Height = 13
    Caption = 'System:'
  end
  object Text10: TLabel
    Left = 30
    Top = 330
    Width = 69
    Height = 13
    Caption = 'Total memory:'
  end
  object Text11: TLabel
    Left = 30
    Top = 349
    Width = 74
    Height = 13
    Caption = 'Memory usage:'
  end
  object Text06: TLabel
    Left = 30
    Top = 125
    Width = 39
    Height = 13
    Caption = 'Licence:'
  end
  object Text01: TLabel
    Left = 30
    Top = 25
    Width = 63
    Height = 25
    Cursor = crAppStart
    Caption = 'Unity'
    Color = 35565
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Text02: TLabel
    Left = 103
    Top = 25
    Width = 256
    Height = 25
    Cursor = crAppStart
    Caption = 'for Debt Management'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 11905689
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object Text04: TLabel
    Left = 30
    Top = 106
    Width = 36
    Height = 13
    Caption = 'Edition:'
  end
  object txt_VER: TLabel
    Left = 140
    Top = 87
    Width = 29
    Height = 13
    Caption = '{VER}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object txt_EDT: TLabel
    Left = 140
    Top = 106
    Width = 29
    Height = 13
    Caption = '{EDT}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object txt_INQ: TLabel
    Left = 141
    Top = 230
    Width = 29
    Height = 13
    Cursor = crHandPoint
    Caption = '{INQ}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3938304
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = txtINQClick
  end
  object txt_LIC: TLabel
    Left = 140
    Top = 125
    Width = 26
    Height = 13
    Caption = '{LIC}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object txt_ITS: TLabel
    Left = 140
    Top = 249
    Width = 26
    Height = 13
    Cursor = crHandPoint
    Caption = '{ITS}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3938304
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = txtITSClick
  end
  object txt_SYS: TLabel
    Left = 141
    Top = 311
    Width = 28
    Height = 13
    Caption = '{SYS}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object txt_MEM: TLabel
    Left = 140
    Top = 330
    Width = 32
    Height = 13
    Caption = '{MEM}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object txt_USG: TLabel
    Left = 140
    Top = 349
    Width = 30
    Height = 13
    Caption = '{USG}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Text14: TLabel
    Left = 30
    Top = 144
    Width = 35
    Height = 13
    Caption = 'Status:'
  end
  object txt_STA: TLabel
    Left = 140
    Top = 144
    Width = 29
    Height = 13
    Caption = '{STA}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Text15: TLabel
    Left = 30
    Top = 268
    Width = 43
    Height = 13
    Caption = 'Website:'
  end
  object txt_WEB: TLabel
    Left = 141
    Top = 268
    Width = 32
    Height = 13
    Cursor = crHandPoint
    Caption = '{WEB}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3938304
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = txtWEBClick
  end
  object Text16: TLabel
    Left = 30
    Top = 187
    Width = 66
    Height = 13
    Caption = 'Programming:'
  end
  object txt_PRO: TLabel
    Left = 141
    Top = 187
    Width = 31
    Height = 13
    Caption = '{PRO}'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnClose: TSpeedButton
    Left = 495
    Top = 402
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Close'
    Flat = True
    OnClick = btnCloseClick
  end
  object Shape1: TShape
    Left = 30
    Top = 172
    Width = 538
    Height = 3
    Brush.Color = 15855854
    Pen.Style = psClear
  end
  object Shape5: TShape
    Left = 30
    Top = 215
    Width = 538
    Height = 3
    Brush.Color = 15855854
    Pen.Style = psClear
  end
  object Shape6: TShape
    Left = 30
    Top = 296
    Width = 538
    Height = 3
    Brush.Color = 15855854
    Pen.Style = psClear
  end
end
