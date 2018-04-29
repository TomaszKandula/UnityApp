object FormReader: TFormReader
  Left = 0
  Top = 0
  Caption = 'Unity Reader'
  ClientHeight = 727
  ClientWidth = 1030
  Color = 15527148
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ChromiumWindow: TChromiumWindow
    Left = 0
    Top = 0
    Width = 1030
    Height = 708
    Align = alClient
    TabOrder = 0
    OnClose = ChromiumWindowClose
    OnBeforeClose = ChromiumWindowBeforeClose
    OnAfterCreated = ChromiumWindowAfterCreated
    ExplicitLeft = 280
    ExplicitTop = 224
    ExplicitWidth = 100
    ExplicitHeight = 41
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 708
    Width = 1030
    Height = 19
    Panels = <
      item
        Text = 'Status:'
        Width = 150
      end
      item
        Text = 'URL: '
        Width = 150
      end>
    ExplicitLeft = 736
    ExplicitTop = 592
    ExplicitWidth = 0
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 16
    Top = 16
  end
end
