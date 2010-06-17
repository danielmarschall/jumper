object HelpForm: THelpForm
  Left = 192
  Top = 103
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Help'
  ClientHeight = 276
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object HelpMemo: TMemo
    Left = 8
    Top = 8
    Width = 409
    Height = 217
    TabStop = False
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object CloseBtn: TButton
    Left = 160
    Top = 240
    Width = 105
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    OnClick = CloseBtnClick
  end
end
