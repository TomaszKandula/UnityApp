object EventForm: TEventForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Unity'
  ClientHeight = 611
  ClientWidth = 816
  Color = 15855854
  Constraints.MinHeight = 400
  Constraints.MinWidth = 400
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
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 796
    Height = 559
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object PanelEventMemo: TPanel
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 776
      Height = 539
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object EventMemo: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 772
        Height = 535
        Hint = 'Press F5 to reload'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BorderStyle = bsNone
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssBoth
        ShowHint = True
        TabOrder = 0
        WantTabs = True
        WordWrap = False
        OnKeyUp = FormKeyUp
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 579
    Width = 816
    Height = 32
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object ImageGrip: TImage
      AlignWithMargins = True
      Left = 800
      Top = 16
      Width = 16
      Height = 16
      Margins.Left = 0
      Margins.Top = 16
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
        001008060000001FF3FF61000000017352474200AECE1CE90000000467414D41
        0000B18F0BFC6105000000097048597300000EC200000EC20115284A80000000
        324944415478DA63FC0F040C1400C6510306D08023478E0CB001F40F0398936D
        6C6CC8F302C506D03F0C703999682F106B000083615F99D1940F550000000049
        454E44AE426082}
      ExplicitLeft = 1046
      ExplicitTop = 0
    end
  end
end
