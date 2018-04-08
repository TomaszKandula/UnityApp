object CalendarForm: TCalendarForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity'
  ClientHeight = 334
  ClientWidth = 313
  Color = clBtnFace
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
    Width = 303
    Height = 324
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Pen.Color = clMedGray
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 248
    ExplicitHeight = 305
  end
  object MainPanel: TPanel
    Left = 13
    Top = 199
    Width = 287
    Height = 122
    BevelKind = bkFlat
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 13
      Width = 75
      Height = 13
      Caption = 'Quick selection:'
    end
    object DaysOne: TRadioButton
      Left = 26
      Top = 40
      Width = 169
      Height = 17
      Cursor = crHandPoint
      Caption = 'One day from now'
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      OnClick = DaysOneClick
    end
    object DaysThreen: TRadioButton
      Left = 26
      Top = 63
      Width = 169
      Height = 17
      Cursor = crHandPoint
      Caption = 'Three days from now'
      TabOrder = 1
      OnClick = DaysThreenClick
    end
    object DaysSeven: TRadioButton
      Left = 26
      Top = 86
      Width = 169
      Height = 17
      Cursor = crHandPoint
      Caption = 'Seven days from now'
      TabOrder = 2
      OnClick = DaysSevenClick
    end
  end
  object CalendarPanel: TPanel
    Left = 13
    Top = 13
    Width = 287
    Height = 180
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 1
    object MyCalendar: TMonthCalendar
      Left = 17
      Top = 8
      Width = 248
      Height = 160
      Hint = 'Double click to select'
      CalColors.BackColor = clWhite
      CalColors.TextColor = clBlack
      CalColors.TitleBackColor = clSilver
      CalColors.TitleTextColor = clBlack
      CalColors.TrailingTextColor = clMaroon
      Date = 43147.721848703710000000
      FirstDayOfWeek = dowMonday
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      WeekNumbers = True
      OnClick = MyCalendarClick
      OnDblClick = MyCalendarDblClick
    end
  end
end
