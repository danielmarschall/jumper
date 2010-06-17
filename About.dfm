object AboutBox: TAboutBox
  Left = 311
  Top = 124
  BorderStyle = bsDialog
  Caption = 'Info'
  ClientHeight = 249
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object AboutPanel: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 193
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 8
      Top = 8
      Width = 65
      Height = 57
      Picture.Data = {
        07544269746D617076020000424D760200000000000076000000280000002000
        0000200000000100040000000000000200000000000000000000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00000000000000000000000000000000000EE8787878EEEEEEE03F30878EEE
        EEE00EE8787878EEEEEEE03F30878EEEEEE00EE8787878EEEEEEE03F30878EEE
        EEE00EE8787878EEEEEEE03F30878EEEEEE00887787877788888803F3088787E
        EEE00788787878878887803F3088887EEEE00788887888878887803F3088887E
        EEE00877888887788888703F308887EEEEE00888777778888888037883088888
        8EE007777777777777703787883087777EE00888888888888803787FF8830888
        888008888888888880378777778830888880077777777788037873F3F3F87808
        88E00888888888803787FFFFFFFF8830EEE00887777778800001111111111100
        EEE00888888888888899B999B99999EEEEE00888888888888899B9B99BB9B9EE
        EEE0088888888888899BB9BB99BB99EEEEE0078888888888899B999B999999EE
        EEE0087788888778899B9B9BB9BB99EEEEE00888778778888E9B9B9BB9999EEE
        EEE0088888788888EE9B99B9BB9BEEEEEEE00EE8888888EEEEE999B9999EEEEE
        EEE00EEEE888EEEEEEEE99BB999EEEEEEEE00EEEEE8EEEEEEEEEE999B9EEEEEE
        EEE00EEEEE8EEEEEEEEEEEE999EEEEEEEEE00EEEEE8EEEEEEEEEEEEE99EEEEEE
        EEE00EEEEE8EEEEEEEEEEEEE9EEEEEEEEEE00EEEEE8EEEEEEEEEEEEEEEEEEEEE
        EEE00EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE00000000000000000000000000000
        0000}
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 88
      Top = 16
      Width = 123
      Height = 13
      Caption = 'ViaThinkSoft Peg Solitaire'
      Transparent = True
      IsControl = True
    end
    object Version: TLabel
      Left = 88
      Top = 40
      Width = 53
      Height = 13
      Caption = 'Version 1.0'
      Transparent = True
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 80
      Width = 147
      Height = 13
      Caption = 'Copyright '#169' 2009 ViaThinkSoft'
      Transparent = True
      OnClick = LinkClick
      IsControl = True
    end
    object Comments: TLabel
      Left = 8
      Top = 136
      Width = 265
      Height = 49
      AutoSize = False
      Caption = 
        'This is a remake of the peg solitaire game "Drag-n-Drop Solitair' +
        'e" from Intuitive Software Inc. 1995 with several new features a' +
        'nd in 32-Bit.'
      Transparent = True
      WordWrap = True
      IsControl = True
    end
    object ProjectLeader: TLabel
      Left = 8
      Top = 104
      Width = 149
      Height = 13
      Caption = 'Project leader: Daniel Marschall'
      Transparent = True
      IsControl = True
    end
    object CompanyHomepage: TLabel
      Left = 168
      Top = 80
      Width = 52
      Height = 13
      Cursor = crHandPoint
      Hint = 'www.viathinksoft.com'
      Caption = 'Homepage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
      OnClick = LinkClick
      IsControl = True
    end
    object LeaderHomepage: TLabel
      Left = 168
      Top = 104
      Width = 52
      Height = 13
      Cursor = crHandPoint
      Hint = 'www.daniel-marschall.de'
      Caption = 'Homepage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
      OnClick = LinkClick
      IsControl = True
    end
  end
  object OKButton: TButton
    Left = 96
    Top = 212
    Width = 105
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKButtonClick
  end
end
