object HelpForm: THelpForm
  Left = 192
  Top = 103
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Help'
  ClientHeight = 276
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object HelpMemo: TMemo
    Left = 8
    Top = 8
    Width = 409
    Height = 217
    TabStop = False
    Color = clBtnFace
    Lines.Strings = (
      
        'The rules of Peg Solitaire are simple. You have to jump over the' +
        ' neighboring '
      
        'stones in a similar way to the drafts to remove them. The game i' +
        's over when there '
      'are no more moves.'
      ''
      
        'ViaThinkSoft Peg Solit'#228'r offers you freely variable and extensib' +
        'le board files that '
      
        'provide you with the template for playing. In the next versions,' +
        ' a comfortable level '
      
        'editor is also planned that will make editing via the editor sup' +
        'erfluous. However, '
      
        'the boards already supplied correspond to the usual standard boa' +
        'rds.'
      ''
      
        'This variant of Peg Solitaire has different colored stones, whic' +
        'h are weighted '
      
        'differently in the number of points. A distant red stone will gi' +
        've you 30 points, a '
      'yellow 20 and a distant green 10.'
      ''
      
        'However, playing for points is not absolutely necessary. You can' +
        ' also play on '
      
        'time, on the fewest remaining stones or on the target field assi' +
        'gnment. Choose the '
      'destination you prefer.'
      ''
      
        'As a rule, diagonal moves are not allowed. The decision as to wh' +
        'ether the '
      
        'diagonal moves are still allowed is determined by the developer ' +
        'of the level file. It '
      
        'is possible that certain boards require diagonal pulls to be loo' +
        'sened correctly. '
      
        'However, this is mathematically very complex to prove. How have ' +
        'the standard '
      
        'boards delivered in two versions as comfort: with and without di' +
        'agonal pull.'
      ''
      
        'Some levels have target fields. These are characterized by a dee' +
        'per imprint on '
      
        'the field or in the preview by a black border. The Peg Solitaire' +
        ' is finished correctly, '
      
        'if there are stones on the target fields (color does not matter)' +
        ', no further moves are '
      
        'possible and there are no other stones on the board. You will re' +
        'ceive a special '
      
        'award in the high score lists. But as I said, you do not have to' +
        ' pursue this primary '
      'goal. You can also play for time or points instead.'
      ''
      
        'Further information about the game can be found on the internet ' +
        'under the '
      'keyword "Solitaire" or "Peg Solitaire".')
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
