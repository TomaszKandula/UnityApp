object TrackerForm: TTrackerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unity'
  ClientHeight = 404
  ClientWidth = 501
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
    Width = 491
    Height = 394
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitHeight = 292
  end
  object btnOK: TSpeedButton
    Left = 325
    Top = 360
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'OK'
    OnClick = btnOKClick
  end
  object btnCancel: TSpeedButton
    Left = 413
    Top = 360
    Width = 73
    Height = 25
    Cursor = crHandPoint
    Caption = 'Cancel'
    OnClick = btnCancelClick
  end
  object FrameTop: TShape
    Left = 15
    Top = 15
    Width = 471
    Height = 330
    Pen.Color = clMedGray
  end
  object SplitLine: TBevel
    Left = 206
    Top = 31
    Width = 25
    Height = 181
    Shape = bsLeftLine
  end
  object Label1: TLabel
    Left = 88
    Top = 50
    Width = 98
    Height = 13
    Caption = 'days from due date.'
  end
  object Label2: TLabel
    Left = 88
    Top = 98
    Width = 98
    Height = 13
    Caption = 'days from due date.'
  end
  object Label3: TLabel
    Left = 88
    Top = 146
    Width = 98
    Height = 13
    Caption = 'days from due date.'
  end
  object ErrorLegalTo: TLabel
    Left = 402
    Top = 202
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
    Left = 402
    Top = 74
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
  object Label4: TLabel
    Left = 88
    Top = 194
    Width = 98
    Height = 13
    Caption = 'days from due date.'
  end
  object FrameBottom: TShape
    Left = 37
    Top = 233
    Width = 425
    Height = 96
    Pen.Color = clMedGray
  end
  object Label6: TLabel
    Left = 277
    Top = 243
    Width = 150
    Height = 13
    Caption = 'E-mail address to be sent from:'
  end
  object Label5: TLabel
    Left = 59
    Top = 243
    Width = 82
    Height = 13
    Caption = 'Reminder layout:'
  end
  object ErrorStatTo: TLabel
    Left = 402
    Top = 138
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
  object Label7: TLabel
    Left = 59
    Top = 299
    Width = 191
    Height = 13
    Caption = 'Statement layout is fixed (English only).'
  end
  object ErrorEmailFrom: TLabel
    Left = 370
    Top = 290
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
  object TextReminder1: TLabeledEdit
    Left = 37
    Top = 47
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
    Left = 37
    Top = 95
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
    Left = 37
    Top = 143
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
    TabOrder = 2
    Text = '0'
  end
  object LayoutList: TComboBox
    Left = 59
    Top = 262
    Width = 150
    Height = 22
    Style = csOwnerDrawFixed
    Color = clCream
    TabOrder = 6
  end
  object TextLegalTo: TLabeledEdit
    Left = 237
    Top = 175
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
    TabOrder = 5
  end
  object TextMailTo: TLabeledEdit
    Left = 237
    Top = 47
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
    TabOrder = 4
  end
  object TextReminder4: TLabeledEdit
    Left = 37
    Top = 191
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
    TabOrder = 3
    Text = '0'
  end
  object EmailFromList: TComboBox
    Left = 277
    Top = 262
    Width = 150
    Height = 22
    Style = csOwnerDrawFixed
    Color = clWhite
    TabOrder = 7
  end
  object TextStatTo: TLabeledEdit
    Left = 237
    Top = 111
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
    TabOrder = 8
  end
end
