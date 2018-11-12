unit Choice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ComCtrls, Menus, ExtCtrls, System.ImageList;

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
    procedure LevelListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PRefreshListClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure RefreshList;
  public
    function SelectedLevel: string;
  end;

var
  LevelChoice: TLevelChoice;

implementation

{$R *.dfm}

uses
  Functions, LevelFunctions, Constants;

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
      DrawLevelPreview(Level, PreviewImage, Color);
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
        else case Level.GetGameMode of
          gmNormal: ImageIndex := 0{Normal};
          gmDiagonal: ImageIndex := 1{Diagonal};
          gmUndefined: ImageIndex := 2{Error};
        end;
      end;
    until FindNext(s) <> 0;
    FindClose(s);
  end;
end;

procedure TLevelChoice.FormResize(Sender: TObject);
var
  p: integer;
begin
  // WIDTH
  p := ClientWidth - 3*LevelGrp.Left; // 100% useable
  LevelGrp.Width := Round((1-MET_PREVIEW_SIZE_RATIO) * p);
  PreviewGrp.Width := Round(MET_PREVIEW_SIZE_RATIO * p);
  PreviewGrp.Left := 2*LevelGrp.Left + LevelGrp.Width;
  LevelList.Width := LevelGrp.Width - 2*LevelList.Left;
  PreviewImage.Width := PreviewGrp.Width - 2*PreviewImage.Left;
  PlayBtn.Left := (LevelGrp.Left + LevelGrp.Width) - PlayBtn.Width;

  // HEIGHT
  LevelGrp.Height := ClientHeight - 3*LevelGrp.Top - PlayBtn.Height;
  PreviewGrp.Height := LevelGrp.Height;
  PlayBtn.Top := 2*LevelGrp.Top + LevelGrp.Height;
  CancelBtn.Top := PlayBtn.Top;
  LevelList.Height := LevelGrp.Height - 2*LevelList.Top;
  PreviewImage.Height := PreviewGrp.Height - 2*PreviewImage.Top;

  // TODO: Icons rearrangieren
end;

procedure TLevelChoice.FormCreate(Sender: TObject);
begin
  if not ForceDirectories(ExtractFilePath(Application.ExeName) + LVL_PATH) then
  begin
    MessageDlg(Format(LNG_COULD_NOT_CREATE_DIR, [LVL_PATH]), mtError, [mbOK], 0);
  end;
end;

end.
