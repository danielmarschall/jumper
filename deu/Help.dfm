object HelpForm: THelpForm
  Left = 192
  Top = 103
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Hilfe'
  ClientHeight = 276
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object HelpMemo: TMemo
    Left = 8
    Top = 8
    Width = 409
    Height = 217
    TabStop = False
    Color = clBtnFace
    Lines.Strings = (
      
        'Die Regeln von Peg Solit'#228'r sind einfach. Sie m'#252'ssen '#228'hnlich wie ' +
        'beim Damespiel '#252'ber die benachbarten Steine h'#252'pfen,'
      'um diese zu entfernen. Das Spiel ist beendet,'
      'wenn keine weiteren Z'#252'ge mehr vorhanden sind.'
      ''
      
        'ViaThinkSoft Peg Solit'#228'r bietet Ihnen beliebig variierbare und e' +
        'rweiterbare Brettdateien, die Ihnen die Vorlage zum Spielen biet' +
        'en.'
      
        'In den n'#228'chsten Versionen ist auch ein komfortabler Leveleditor ' +
        'geplant, '
      'der das Editieren per Editor '#252'berfl'#252'ssig machen soll. '
      
        'Die bereits mitgelieferten Bretter entsprechen jedoch den '#252'blich' +
        'en Standardbrettern.'
      ''
      
        'Diese Variante von Peg Solit'#228'r hat verschiedenfarbige Steine, di' +
        'e unterschiedlich in der Punktezahl gewichtet wird. Ein entfernt' +
        'er roter Stein bringt Ihnen 30 Punkte,'
      'ein gelber 20 und ein entfernter Gr'#252'ner 10. '
      ''
      ''
      
        'Das Spielen nach Punktezahl ist jedoch nicht zwingend notwendig.' +
        ' Sie k'#246'nnen auch auf Zeit spielen, auf die wenigsten verbleibend' +
        'en Steine oder auf die Zielfeldbelegung. W'#228'hlen Sie das Ziel, da' +
        's Sie bevorzugen.'
      ' '
      ''
      ''
      
        'In der Regel sind diagonale Spielz'#252'ge nicht erlaubt. Die Entsche' +
        'idung, ob die diagonalen Z'#252'ge trotzdem erlaubt sind, wird vom En' +
        'twickler der Leveldatei festgelegt. Es ist m'#246'glich, dass gewisse' +
        ' Bretter diagonale z'#252'ge erfordern, um korrekt gel'#246'st zu werden. ' +
        'Dies ist Mathematisch jedoch sehr komplex nachzuweisen. Wie habe' +
        'n als Komfort die Standardbretter in zwei Varianten ausgeliefert' +
        ': Mit und ohne Diagonalzug.'
      ''
      ''
      ''
      ''
      ''
      
        'Einige Levels besitzen Zielfelder. Diese sind durch eine tiefere' +
        ' Pr'#228'gung auf dem Spielfeld  bzw. in der Vorschau durch einen sch' +
        'warzen Rand gekennzeichnet. Das Peg Solit'#228'r ist korrekt beendet,' +
        ' wenn auf den Zielfeldern Steine (Farbe egal) liegen, keine weit' +
        'eren Z'#252'ge mehr m'#246'glich sind und sonst keine Steine auf dem Brett' +
        ' liegen. Sie erhalten hierf'#252'r eine besondere Auszeichnung in den' +
        ' Highscorelisten. Sie m'#252'ssen aber wie gesagt, dieses Prim'#228'rziel ' +
        'nicht verfolgen. Sie k'#246'nnen stattdessen auch auf Zeit oder Punkt' +
        'ezahlen spielen.'
      ''
      ''
      ''
      ''
      ''
      ''
      
        'Weitere Informationen zu dem Spiel finden Sie im Internet unter ' +
        'dem Stichwort "Solit'#228'r" bzw. "Peg Solitaire".'
      '')
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
    Caption = 'Schlie'#223'en'
    Default = True
    TabOrder = 0
    OnClick = CloseBtnClick
  end
end
