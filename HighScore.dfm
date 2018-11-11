object HighScoreForm: THighScoreForm
  Left = 258
  Top = 131
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'High scores'
  ClientHeight = 305
  ClientWidth = 441
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
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 425
    Height = 249
    ActivePage = PPMTab
    TabOrder = 1
    object PPMTab: TTabSheet
      Caption = 'Highest Points per Minute'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PPMList: TListView
        Left = 8
        Top = 8
        Width = 401
        Height = 201
        Columns = <
          item
            Caption = 'Name'
            Width = 123
          end
          item
            Caption = 'Date'
            Width = 123
          end
          item
            Caption = 'Points per Minute'
            Width = 123
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
        OnCompare = CompareAsc
      end
    end
    object RemainingTab: TTabSheet
      Caption = 'Lowest remaining stones'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RemainingList: TListView
        Left = 8
        Top = 8
        Width = 401
        Height = 201
        Columns = <
          item
            Caption = 'Name'
            Width = 123
          end
          item
            Caption = 'Date'
            Width = 123
          end
          item
            Caption = 'Remaining stones'
            Width = 123
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
        OnCompare = CompareDesc
      end
    end
    object TimeTab: TTabSheet
      Caption = 'Shortest time'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TimeList: TListView
        Left = 8
        Top = 8
        Width = 401
        Height = 201
        Columns = <
          item
            Caption = 'Name'
            Width = 123
          end
          item
            Caption = 'Date'
            Width = 123
          end
          item
            Caption = 'Time [Seconds]'
            Width = 123
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
        OnCompare = CompareDesc
      end
    end
    object ScoreTab: TTabSheet
      Caption = 'Highest score'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ScoreList: TListView
        Left = 8
        Top = 8
        Width = 401
        Height = 201
        Columns = <
          item
            Caption = 'Name'
            Width = 123
          end
          item
            Caption = 'Date'
            Width = 123
          end
          item
            Caption = 'Score'
            Width = 123
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
        OnCompare = CompareAsc
      end
    end
  end
  object CloseBtn: TButton
    Left = 312
    Top = 272
    Width = 123
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    OnClick = CloseBtnClick
  end
  object ClearBtn: TButton
    Left = 8
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Clear list'
    TabOrder = 2
    OnClick = ClearBtnClick
  end
end
