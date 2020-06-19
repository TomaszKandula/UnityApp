object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 449
  ClientWidth = 802
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
  object PanelHeader: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 802
    Height = 57
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 10
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object txtSubtitle: TLabel
      Left = 10
      Top = 18
      Width = 249
      Height = 18
      Caption = 'Helping you along the way...'
      Color = 14922344
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMedGray
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      Font.Quality = fqAntialiased
      ParentColor = False
      ParentFont = False
    end
  end
  object PanelContent: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 67
    Width = 782
    Height = 317
    Margins.Left = 10
    Margins.Top = 0
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object versionGroupBox: TGroupBox
      Left = 14
      Top = 9
      Width = 331
      Height = 104
      Caption = 'Version'
      TabOrder = 0
      object valLicence: TLabel
        Left = 80
        Top = 52
        Width = 36
        Height = 13
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object valStatus: TLabel
        Left = 80
        Top = 71
        Width = 36
        Height = 13
        Caption = '{value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object valVersion: TLabel
        Left = 80
        Top = 33
        Width = 36
        Height = 13
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object txtLicence: TLabel
        Left = 24
        Top = 52
        Width = 39
        Height = 13
        Caption = 'Licence:'
      end
      object txtStatus: TLabel
        Left = 24
        Top = 71
        Width = 35
        Height = 13
        Caption = 'Status:'
      end
      object txtVersion: TLabel
        Left = 24
        Top = 33
        Width = 39
        Height = 13
        Caption = 'Version:'
      end
    end
    object developersGroupBox: TGroupBox
      Left = 14
      Top = 119
      Width = 331
      Height = 74
      Caption = 'Programming'
      TabOrder = 1
      object valDeveloper: TLabel
        Left = 80
        Top = 37
        Width = 36
        Height = 13
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object txtDeveloper: TLabel
        Left = 24
        Top = 37
        Width = 37
        Height = 13
        Caption = 'Author:'
      end
    end
    object helpGroupBox: TGroupBox
      Left = 14
      Top = 199
      Width = 331
      Height = 105
      Caption = 'Assistance'
      TabOrder = 2
      object valEmail: TLabel
        Left = 120
        Top = 57
        Width = 36
        Height = 13
        Cursor = crHandPoint
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 3938304
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = txtINQClick
      end
      object valSupport: TLabel
        Left = 120
        Top = 76
        Width = 36
        Height = 13
        Cursor = crHandPoint
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 3938304
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = txtITSClick
      end
      object valWebsite: TLabel
        Left = 120
        Top = 38
        Width = 36
        Height = 13
        Cursor = crHandPoint
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 3938304
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = txtWEBClick
      end
      object txtEmail: TLabel
        Left = 26
        Top = 57
        Width = 45
        Height = 13
        Caption = 'Inquiries:'
      end
      object txtSupport: TLabel
        Left = 26
        Top = 76
        Width = 81
        Height = 13
        Caption = 'Local IT support:'
      end
      object txtWebsite: TLabel
        Left = 26
        Top = 38
        Width = 43
        Height = 13
        Caption = 'Website:'
      end
    end
    object infoGroupBox: TGroupBox
      Left = 358
      Top = 199
      Width = 411
      Height = 105
      Caption = 'System'
      TabOrder = 3
      object valTotMem: TLabel
        Left = 118
        Top = 37
        Width = 36
        Height = 13
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object valSystem: TLabel
        Left = 118
        Top = 75
        Width = 36
        Height = 13
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object valMemUse: TLabel
        Left = 118
        Top = 56
        Width = 36
        Height = 13
        Caption = '{Value}'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object txtMemUse: TLabel
        Left = 26
        Top = 56
        Width = 74
        Height = 13
        Caption = 'Memory usage:'
      end
      object txtSystem: TLabel
        Left = 26
        Top = 75
        Width = 39
        Height = 13
        Caption = 'System:'
      end
      object txtTotMem: TLabel
        Left = 26
        Top = 37
        Width = 69
        Height = 13
        Caption = 'Total memory:'
      end
    end
    object userGroupBox: TGroupBox
      Left = 358
      Top = 9
      Width = 411
      Height = 184
      Caption = 'User info'
      TabOrder = 4
      object txtDisplayName: TLabel
        Left = 26
        Top = 33
        Width = 67
        Height = 13
        Caption = 'Display name:'
      end
      object txtDepartment: TLabel
        Left = 26
        Top = 52
        Width = 61
        Height = 13
        Caption = 'Department:'
      end
      object txtUserEmail: TLabel
        Left = 26
        Top = 71
        Width = 69
        Height = 13
        Caption = 'Email address:'
      end
      object valDisplayName: TLabel
        Left = 110
        Top = 33
        Width = 36
        Height = 13
        Caption = '{Value}'
      end
      object valUserEmail: TLabel
        Left = 110
        Top = 71
        Width = 36
        Height = 13
        Caption = '{Value}'
      end
      object valDepartment: TLabel
        Left = 110
        Top = 52
        Width = 36
        Height = 13
        Caption = '{Value}'
      end
      object txtUserNumber: TLabel
        Left = 26
        Top = 90
        Width = 65
        Height = 13
        Caption = 'User number:'
      end
      object valUserNumber: TLabel
        Left = 110
        Top = 90
        Width = 36
        Height = 13
        Caption = '{Value}'
      end
      object txtAliasName: TLabel
        Left = 26
        Top = 109
        Width = 55
        Height = 13
        Caption = 'Alias name:'
      end
      object valAliasName: TLabel
        Left = 110
        Top = 109
        Width = 36
        Height = 13
        Caption = '{Value}'
      end
    end
  end
  object PanelFooter: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 394
    Width = 802
    Height = 55
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
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
        'Copyright '#169' 2017 - 2020 DFDS Polska Sp. z o.o. All rights reserv' +
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
