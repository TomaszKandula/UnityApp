object EventForm: TEventForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Unity'
  ClientHeight = 478
  ClientWidth = 528
  Color = 15527148
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
    Width = 508
    Height = 442
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
      Width = 488
      Height = 422
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 498
      ExplicitHeight = 432
      object EventMemo: TMemo
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 484
        Height = 418
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
        ExplicitWidth = 494
        ExplicitHeight = 428
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 462
    Width = 528
    Height = 16
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
      Left = 512
      Top = 0
      Width = 16
      Height = 16
      Align = alRight
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
        0010080200000090916836000000017352474200AECE1CE90000000467414D41
        0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000000
        314944415478DA63FCFFFF3F0329807154035E909E9E4E630DB4F103C4193367
        CE24D649246BA08D1F309D41C049F83500007846491F93B186F0000000004945
        4E44AE426082}
      ExplicitLeft = 675
    end
  end
end
