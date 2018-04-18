object TrackerForm: TTrackerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 481
  ClientWidth = 977
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PopupMode = pmExplicit
  PopupParent = MainForm.Owner
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AppMain: TShape
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 967
    Height = 471
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitLeft = 10
    ExplicitTop = 0
    ExplicitWidth = 1398
    ExplicitHeight = 974
  end
  object btnOK: TSpeedButton
    Left = 781
    Top = 437
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'OK'
    OnClick = btnOKClick
  end
  object btnCancel: TSpeedButton
    Left = 880
    Top = 437
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Cancel'
    OnClick = btnCancelClick
  end
  object GroupBoxLeft: TGroupBox
    Left = 19
    Top = 18
    Width = 225
    Height = 279
    Caption = 'Reminders'
    TabOrder = 0
    TabStop = True
    object Label1: TLabel
      Left = 95
      Top = 46
      Width = 98
      Height = 13
      Caption = 'days from due date.'
    end
    object Label2: TLabel
      Left = 95
      Top = 102
      Width = 98
      Height = 13
      Caption = 'days from due date.'
    end
    object Label3: TLabel
      Left = 95
      Top = 166
      Width = 98
      Height = 13
      Caption = 'days from due date.'
    end
    object Label4: TLabel
      Left = 95
      Top = 230
      Width = 98
      Height = 13
      Caption = 'days from due date.'
    end
    object TextReminder1: TLabeledEdit
      Left = 29
      Top = 43
      Width = 41
      Height = 21
      Color = clCream
      EditLabel.Width = 58
      EditLabel.Height = 13
      EditLabel.Caption = 'Reminder 1:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      NumbersOnly = True
      ParentFont = False
      TabOrder = 0
      Text = '0'
    end
    object TextReminder2: TLabeledEdit
      Left = 29
      Top = 99
      Width = 41
      Height = 21
      Color = clCream
      EditLabel.Width = 58
      EditLabel.Height = 13
      EditLabel.Caption = 'Reminder 2:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      NumbersOnly = True
      ParentFont = False
      TabOrder = 1
      Text = '0'
    end
    object TextReminder3: TLabeledEdit
      Left = 29
      Top = 163
      Width = 41
      Height = 21
      Color = clCream
      EditLabel.Width = 58
      EditLabel.Height = 13
      EditLabel.Caption = 'Reminder 3:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      NumbersOnly = True
      ParentFont = False
      TabOrder = 3
      Text = '0'
    end
    object TextReminder4: TLabeledEdit
      Left = 29
      Top = 227
      Width = 41
      Height = 21
      Color = clCream
      EditLabel.Width = 62
      EditLabel.Height = 13
      EditLabel.Caption = 'Legal Action:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      NumbersOnly = True
      ParentFont = False
      TabOrder = 5
      Text = '0'
    end
    object Exp_Rem2_Switch: TCheckBox
      Left = 29
      Top = 126
      Width = 97
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object Exp_Rem3_Switch: TCheckBox
      Left = 29
      Top = 190
      Width = 97
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object GroupBoxBottom: TGroupBox
    Left = 19
    Top = 303
    Width = 529
    Height = 122
    Caption = 'Settings'
    TabOrder = 2
    TabStop = True
    object ErrorEmailFrom: TLabel
      Left = 440
      Top = 79
      Width = 57
      Height = 13
      Caption = 'Empty field!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 29
      Top = 32
      Width = 82
      Height = 13
      Caption = 'Reminder layout:'
    end
    object Label6: TLabel
      Left = 347
      Top = 32
      Width = 150
      Height = 13
      Caption = 'E-mail address to be sent from:'
    end
    object Label7: TLabel
      Left = 29
      Top = 79
      Width = 191
      Height = 13
      Caption = 'Statement layout is fixed (English only).'
    end
    object EmailFromList: TComboBox
      Left = 312
      Top = 51
      Width = 185
      Height = 22
      Style = csOwnerDrawFixed
      Color = clWhite
      TabOrder = 1
    end
    object LayoutList: TComboBox
      Left = 29
      Top = 51
      Width = 164
      Height = 22
      Style = csOwnerDrawFixed
      Color = clCream
      TabOrder = 0
    end
  end
  object GroupBoxMid: TGroupBox
    Left = 256
    Top = 18
    Width = 292
    Height = 279
    Caption = 'Emails'
    TabOrder = 1
    TabStop = True
    object ErrorLegalTo: TLabel
      Left = 204
      Top = 217
      Width = 57
      Height = 13
      Caption = 'Empty field!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ErrorMailTo: TLabel
      Left = 204
      Top = 89
      Width = 57
      Height = 13
      Caption = 'Empty field!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ErrorStatTo: TLabel
      Left = 204
      Top = 153
      Width = 57
      Height = 13
      Caption = 'Empty field!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object TextLegalTo: TLabeledEdit
      Left = 36
      Top = 190
      Width = 225
      Height = 21
      Cursor = crArrow
      Color = clWhite
      EditLabel.Width = 131
      EditLabel.Height = 13
      EditLabel.Caption = 'Legal Action notification to:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxLength = 2048
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
    end
    object TextMailTo: TLabeledEdit
      Left = 36
      Top = 62
      Width = 225
      Height = 21
      Cursor = crArrow
      Color = clWhite
      EditLabel.Width = 99
      EditLabel.Height = 13
      EditLabel.Caption = 'Remainders send to:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxLength = 2048
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object TextStatTo: TLabeledEdit
      Left = 36
      Top = 126
      Width = 225
      Height = 21
      Cursor = crArrow
      Color = clWhite
      EditLabel.Width = 98
      EditLabel.Height = 13
      EditLabel.Caption = 'Statements send to:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxLength = 2048
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
    end
  end
  object GroupBoxRight: TGroupBox
    Left = 560
    Top = 18
    Width = 393
    Height = 407
    Caption = 'Selected Customers'
    TabOrder = 3
    TabStop = True
    object CustomerList: TListView
      Left = 16
      Top = 29
      Width = 361
      Height = 356
      Columns = <>
      FlatScrollBars = True
      GridLines = True
      ReadOnly = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
end
