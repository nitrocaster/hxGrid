object Form1: TForm1
  Left = 501
  Top = 298
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'hxgrid agent'
  ClientHeight = 112
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 321
    Height = 109
    Caption = ' Status '
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 36
      Width = 272
      Height = 13
      Caption = 
        'CPU used by agent: .............................................' +
        '.............'
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 274
      Height = 13
      Caption = 
        'CPU available: .................................................' +
        '..................'
    end
    object Label3: TLabel
      Left = 8
      Top = 76
      Width = 305
      Height = 29
      AutoSize = False
      Caption = 'State: Unassigned'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 284
      Top = 16
      Width = 26
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '100%'
    end
    object Label5: TLabel
      Left = 284
      Top = 36
      Width = 26
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '100%'
    end
    object Label6: TLabel
      Left = 8
      Top = 56
      Width = 273
      Height = 13
      Caption = 
        'CPU count (allowed to use):  ...................................' +
        '..........'
    end
    object Label7: TLabel
      Left = 284
      Top = 56
      Width = 25
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '1'
    end
  end
  object JvTrayIcon1: TJvTrayIcon
    Active = True
    IconIndex = 0
    PopupMenu = PopupMenu1
    Visibility = [tvVisibleTaskList, tvAutoHide, tvRestoreDbClick]
    Left = 152
    Top = 65528
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 192
    Top = 65532
  end
  object PopupMenu1: TPopupMenu
    Left = 116
    Top = 65528
    object Status1: TMenuItem
      Caption = '&Status'
      OnClick = Status1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Caption = 'E&xit'
      OnClick = Exit1Click
    end
  end
  object SWbemLocator1: TSWbemLocator
    AutoConnect = False
    ConnectKind = ckRunningOrNew
    Left = 236
    Top = 8
  end
end
