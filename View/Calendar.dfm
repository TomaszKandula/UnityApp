object CalendarForm: TCalendarForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Unity'
  ClientHeight = 354
  ClientWidth = 313
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 293
    Height = 334
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object PanelCalendar: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 273
      Height = 182
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object MyCalendar: TMonthCalendar
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 269
        Height = 178
        Hint = 'Double click to select'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        CalColors.BackColor = clWhite
        CalColors.TextColor = clBlack
        CalColors.TitleBackColor = clSilver
        CalColors.TitleTextColor = clBlack
        CalColors.TrailingTextColor = clMaroon
        Date = 43147.754980740740000000
        FirstDayOfWeek = dowMonday
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        WeekNumbers = True
        OnClick = MyCalendarClick
        OnDblClick = MyCalendarDblClick
      end
    end
    object PanelActions: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 202
      Width = 273
      Height = 122
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object Text: TLabel
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
  end
end
