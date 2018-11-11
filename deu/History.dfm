object HistoryForm: THistoryForm
  Left = 192
  Top = 103
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Sprungverlauf'
  ClientHeight = 353
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object JumpMemo: TMemo
    Left = 8
    Top = 8
    Width = 265
    Height = 305
    TabStop = False
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object SaveBtn: TButton
    Left = 8
    Top = 320
    Width = 105
    Height = 25
    Caption = 'In Datei speichern'
    TabOrder = 1
    OnClick = SaveBtnClick
  end
  object CloseBtn: TButton
    Left = 168
    Top = 320
    Width = 105
    Height = 25
    Cancel = True
    Caption = 'Schlie'#223'en'
    Default = True
    TabOrder = 0
    OnClick = CloseBtnClick
  end
  object JumpSaveDialog: TSaveDialog
    DefaultExt = '*.txt'
    Filter = 'Textdateien (*.txt)|*.txt|Alle Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 16
    Top = 16
  end
end
