object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 459
  ClientWidth = 588
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
    Width = 578
    Height = 449
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clWhite
    ExplicitLeft = 0
    ExplicitTop = 35
    ExplicitWidth = 599
    ExplicitHeight = 417
  end
  object PanelHeader: TPanel
    Left = 16
    Top = 16
    Width = 555
    Height = 78
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object txtName1: TLabel
      Left = 10
      Top = 15
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
    object txtName2: TLabel
      Left = 79
      Top = 15
      Width = 101
      Height = 25
      Cursor = crAppStart
      Caption = 'Platform'
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
    object txtSubtitle: TLabel
      Left = 10
      Top = 46
      Width = 295
      Height = 18
      Caption = 'Debtors && Creditors Management'
      Color = 11905689
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11905689
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      Font.Quality = fqAntialiased
      ParentColor = False
      ParentFont = False
    end
  end
  object PanelContent: TPanel
    Left = 16
    Top = 100
    Width = 555
    Height = 282
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object txtVersion: TLabel
      Left = 10
      Top = 9
      Width = 39
      Height = 13
      Caption = 'Version:'
    end
    object txtEmail: TLabel
      Left = 10
      Top = 136
      Width = 45
      Height = 13
      Caption = 'Inquiries:'
    end
    object txtLicence: TLabel
      Left = 10
      Top = 28
      Width = 39
      Height = 13
      Caption = 'Licence:'
    end
    object txtSupport: TLabel
      Left = 10
      Top = 155
      Width = 81
      Height = 13
      Caption = 'Local IT support:'
    end
    object shLine2: TShape
      Left = 10
      Top = 121
      Width = 538
      Height = 5
      Brush.Color = 15855854
      Pen.Style = psClear
    end
    object shLine3: TShape
      Left = 10
      Top = 202
      Width = 538
      Height = 5
      Brush.Color = 15855854
      Pen.Style = psClear
    end
    object txtSystem: TLabel
      Left = 10
      Top = 217
      Width = 39
      Height = 13
      Caption = 'System:'
    end
    object txtTotMem: TLabel
      Left = 10
      Top = 236
      Width = 69
      Height = 13
      Caption = 'Total memory:'
    end
    object txtMemUse: TLabel
      Left = 10
      Top = 255
      Width = 74
      Height = 13
      Caption = 'Memory usage:'
    end
    object txtStatus: TLabel
      Left = 10
      Top = 47
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object txtWebsite: TLabel
      Left = 10
      Top = 174
      Width = 43
      Height = 13
      Caption = 'Website:'
    end
    object txtDevs: TLabel
      Left = 10
      Top = 93
      Width = 66
      Height = 13
      Caption = 'Programming:'
    end
    object txt_INQ: TLabel
      Left = 121
      Top = 136
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
    object txt_ITS: TLabel
      Left = 120
      Top = 155
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
    object txt_LIC: TLabel
      Left = 120
      Top = 28
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
    object txt_MEM: TLabel
      Left = 120
      Top = 236
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
    object txt_PRO: TLabel
      Left = 121
      Top = 93
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
    object txt_STA: TLabel
      Left = 120
      Top = 47
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
    object txt_SYS: TLabel
      Left = 121
      Top = 217
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
    object txt_USG: TLabel
      Left = 120
      Top = 255
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
    object txt_VER: TLabel
      Left = 120
      Top = 9
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
    object txt_WEB: TLabel
      Left = 121
      Top = 174
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
    object shLine1: TShape
      Left = 10
      Top = 75
      Width = 538
      Height = 5
      Brush.Color = 15855854
      Pen.Style = psClear
    end
  end
  object PanelFooter: TPanel
    Left = 16
    Top = 388
    Width = 555
    Height = 55
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object txtCopyright: TLabel
      Left = 10
      Top = 11
      Width = 327
      Height = 13
      Caption = 
        'Copyright '#169' 2016 - 2019 DFDS Polska Sp. z o.o. All rights reserv' +
        'ed.'
    end
    object txtLegalNote: TLabel
      Left = 10
      Top = 30
      Width = 444
      Height = 13
      Caption = 
        'This product is protected by Polish and international copyright ' +
        'and intellectual property laws.'
    end
  end
end
