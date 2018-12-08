unit Choice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ComCtrls, Menus, ExtCtrls, ImageList,
  LevelFunctions;

type
  TLevelChoice = class(TForm)
    PlayBtn: TButton;
    CancelBtn: TButton;
    LevelImageList: TImageList;
    LevelPopupMenu: TPopupMenu;
    PLoadLevel: TMenuItem;
    PRefreshList: TMenuItem;
    PreviewGrp: TGroupBox;
    PreviewImage: TImage;
    LevelGrp: TGroupBox;
    LevelList: TListView;
    procedure PlayBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LevelListClick(Sender: TObject);
    procedure LevelListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure PRefreshListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure RefreshList;
    procedure DrawLevelPreview(Level: TLevel);
  public
    function SelectedLevel: string;
  end;

var
  LevelChoice: TLevelChoice;

implementation

{$R *.dfm}

uses
  Functions, Constants;

procedure TLevelChoice.DrawLevelPreview(Level: TLevel);
var
  PlaygroundMatrix: TPlayGroundMatrix;
  y, x: integer;
  indent: Integer;
  Image: TImage;
  BackgroundColor: TColor;
const
  PREVIEW_BLOCK_SIZE = 10; // Enthält Field und Abstand
  PREVIEW_TAB_SIZE = PREVIEW_BLOCK_SIZE div 2; // 5
begin
  Image := PreviewImage;
  BackgroundColor := Self.Color;

  ClearImage(Image, BackgroundColor);

  Level.FillPlaygroundMatrix(PlaygroundMatrix, false);
  try
    for x := Low(PlaygroundMatrix.Fields) to High(PlaygroundMatrix.Fields) do
    begin
      for y := Low(PlaygroundMatrix.Fields[x]) to High(PlaygroundMatrix.Fields[x]) do
      begin
        // Rectange filling
        case PlaygroundMatrix.Fields[x,y].FieldType of
          ftFullSpace: Image.Canvas.Brush.Color := BackgroundColor; // invisible
          ftEmpty:     Image.Canvas.Brush.Color := clWhite;
          ftGreen:     Image.Canvas.Brush.Color := clLime;
          ftYellow:    Image.Canvas.Brush.Color := clYellow;
          ftRed:       Image.Canvas.Brush.Color := clRed;
        end;

        // Rectangle border
        if PlaygroundMatrix.Fields[x,y].Goal then
          Image.Canvas.Pen.Color := clBlack
        else
        begin
          if PlaygroundMatrix.Fields[x,y].FieldType = ftFullSpace then
            Image.Canvas.Pen.Color := BackgroundColor // invisible
          else
            Image.Canvas.Pen.Color := clLtGray;
        end;

        // Draw the rectangle
        indent := PlaygroundMatrix.Fields[x,y].Indent;
        Image.Canvas.Rectangle(x*PREVIEW_BLOCK_SIZE + indent*PREVIEW_TAB_SIZE,
                               y*PREVIEW_BLOCK_SIZE,
                               x*PREVIEW_BLOCK_SIZE + indent*PREVIEW_TAB_SIZE + PREVIEW_BLOCK_SIZE,
                               y*PREVIEW_BLOCK_SIZE                           + PREVIEW_BLOCK_SIZE);
      end;
    end;
  finally
    PlaygroundMatrix.ClearMatrix(true);
  end;
end;

function TLevelChoice.SelectedLevel: string;
begin
  result := Format(LVL_FILE, [LevelList.Selected.Caption]);
end;

procedure TLevelChoice.PlayBtnClick(Sender: TObject);
var
  Level: TLevel;
begin
  if Assigned(LevelList.Selected) then
  begin
    if LevelList.Selected.ImageIndex = 2 then
    begin
      Level := TLevel.Create(Format(LVL_FILE, [LevelList.Selected.Caption]));
      try
        if Level.CheckLevelIntegrity(true) <> leNone then
        begin
          exit;
        end;
      finally
        FreeAndNil(Level);
      end;
    end;
    ModalResult := mrOk;
  end;
end;

procedure TLevelChoice.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TLevelChoice.FormShow(Sender: TObject);
begin
  RefreshList;
end;

procedure TLevelChoice.LevelListClick(Sender: TObject);
var
  LevelFile: string;
  Level: TLevel;
begin
  PlayBtn.Enabled := Assigned(LevelList.Selected);
  PLoadLevel.Enabled := Assigned(LevelList.Selected);

  if Assigned(LevelList.Selected) then
  begin
    LevelFile := Format(LVL_FILE, [LevelList.Selected.Caption]);
    Level := TLevel.Create(LevelFile);
    try
      DrawLevelPreview(Level);
    finally
      FreeAndNil(Level);
    end;
  end
  else
  begin
    ClearImage(PreviewImage, Color);
  end;
end;

procedure TLevelChoice.LevelListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctState then LevelListClick(self);
end;

procedure TLevelChoice.PRefreshListClick(Sender: TObject);
begin
  RefreshList;
end;

procedure TLevelChoice.RefreshList;
var
  s: TSearchRec;
  Level: TLevel;
begin
  LevelList.Clear;

  // Levels auflisten
  if FindFirst(Format(LVL_FILE, ['*']), faAnyFile, s) = 0 then
  begin
    repeat
      with LevelList.Items.Add do
      begin
        Caption := Copy(s.Name, 1, Length(s.Name)-Length(LVL_EXT));
        Level := TLevel.Create(LVL_PATH + s.Name);

        if Level.CheckLevelIntegrity <> leNone then
          ImageIndex := 2{Error}
        else case Level.GameMode of
          gmNormal: ImageIndex := 0{Normal};
          gmDiagonal: ImageIndex := 1{Diagonal};
          gmUndefined: ImageIndex := 2{Error};
        end;
      end;
    until FindNext(s) <> 0;
    FindClose(s);
  end;
end;

procedure TLevelChoice.FormCreate(Sender: TObject);
begin
  if not ForceDirectories(ExtractFilePath(Application.ExeName) + LVL_PATH) then
  begin
    MessageDlg(Format(LNG_COULD_NOT_CREATE_DIR, [LVL_PATH]), mtError, [mbOK], 0);
  end;
end;

end.
