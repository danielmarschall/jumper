object MainForm: TMainForm
  Left = 242
  Top = 142
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ViaThinkSoft Peg Solit'#228'r'
  ClientHeight = 277
  ClientWidth = 340
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Playground: TPanel
    Left = 8
    Top = 8
    Width = 313
    Height = 289
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Statistics: TStatusBar
    Left = 0
    Top = 258
    Width = 340
    Height = 19
    Panels = <
      item
        Width = 85
      end
      item
        Alignment = taCenter
        Width = 140
      end
      item
        Alignment = taRightJustify
        Width = 50
      end>
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object Help1: TMenuItem
      Caption = 'Spiel'
      object MNewGame: TMenuItem
        Caption = 'Neues Spiel...'
        ShortCut = 16462
        OnClick = MNewGameClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MRestartGame: TMenuItem
        Caption = 'Neu starten'
        ShortCut = 16466
        OnClick = MRestartGameClick
      end
      object MPauseTime: TMenuItem
        Caption = 'Zeit anhalten'
        ShortCut = 16464
        OnClick = MPauseTimeClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MUndo: TMenuItem
        Caption = 'R'#252'ckg'#228'ngig'
        Enabled = False
        ShortCut = 16474
        OnClick = MUndoClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MJumpHistory: TMenuItem
        Caption = 'Sprungverlauf...'
        ShortCut = 16458
        OnClick = MJumpHistoryClick
      end
      object MHighScores: TMenuItem
        Caption = 'Highscores von diesem Level...'
        ShortCut = 16456
        OnClick = MHighScoresClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MExit: TMenuItem
        Caption = 'Beenden'
        ShortCut = 32883
        OnClick = MExitClick
      end
    end
    object MSettings: TMenuItem
      Caption = 'Einstellungen'
      object MEnableSound: TMenuItem
        Caption = 'Ton'
        Checked = True
        ShortCut = 32851
        OnClick = MEnableSoundClick
      end
    end
    object Help2: TMenuItem
      Caption = 'Hilfe'
      object MHelp: TMenuItem
        Caption = 'Hilfe anzeigen...'
        ShortCut = 112
        OnClick = MHelpClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MAbout: TMenuItem
        Caption = #220'ber...'
        OnClick = MAboutClick
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 40
    Top = 8
  end
end
