object Form1: TForm1
  Left = 134
  Top = 333
  Width = 873
  Height = 231
  Caption = 'hxgrid coordinator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 865
    Height = 204
    Align = alClient
    ColCount = 7
    DefaultColWidth = 110
    DefaultRowHeight = 14
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
    TabOrder = 0
  end
  object JvTrayIcon1: TJvTrayIcon
    Active = True
    IconIndex = 0
    Visibility = [tvVisibleTaskList, tvAutoHide, tvRestoreDbClick]
    Left = 36
    Top = 388
  end
  object Timer2: TTimer
    Enabled = False
    OnTimer = Timer2Timer
    Left = 240
    Top = 380
  end
end
