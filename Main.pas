unit Main;

interface

uses
  Windows, SysUtils, Classes, Graphics, Dialogs, StdCtrls, Menus, Controls,
  ComCtrls, ExtCtrls, Forms, MMSystem, LevelFunctions, Registry;

type
  TField = record
    FieldType: TFieldType;
    Goal: Boolean;
    Panel: TPanel;
    Stone: TImage;
  end;

  TFieldState = (fsError, fsLocked, fsAvailable, fsStone);

  TPlayGroundMatrix = array of array of TField;

  TMainForm = class(TForm)
    Playground: TPanel;
    MainMenu: TMainMenu;
    Help1: TMenuItem;
    MExit: TMenuItem;
    Statistics: TStatusBar;
    Timer: TTimer;
    MNewGame: TMenuItem;
    Help2: TMenuItem;
    MAbout: TMenuItem;
    MHelp: TMenuItem;
    N5: TMenuItem;
    MJumpHistory: TMenuItem;
    N2: TMenuItem;
    N4: TMenuItem;
    MHighScores: TMenuItem;
    MRestartGame: TMenuItem;
    MSettings: TMenuItem;
    MEnableSound: TMenuItem;
    MPauseTime: TMenuItem;
    N1: TMenuItem;
    MUndo: TMenuItem;
    N3: TMenuItem;
    procedure MExitClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure MNewGameClick(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MJumpHistoryClick(Sender: TObject);
    procedure MRestartGameClick(Sender: TObject);
    procedure MHighScoresClick(Sender: TObject);
    procedure MPauseTimeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MHelpClick(Sender: TObject);
    procedure MEnableSoundClick(Sender: TObject);
    procedure MUndoClick(Sender: TObject);
  private
    CountedSeconds: Integer;
    LevelFile: String;
    LookupFieldCoordinateArray: array of TPoint;
    OriginalPlayGroundMatrix: TPlayGroundMatrix;
    PrevPlaygroundMatrixes: array of TPlayGroundMatrix;
    PlaygroundMatrix: TPlayGroundMatrix;
    Points: Integer;
    LevelTotalStones: Integer;
    LevelRemovedStones: Integer;
    JumpHistory: TStringList;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure RestartLevel;
    procedure SetNewPlayGroundMatrix(Matrix: TPlayGroundMatrix);
    procedure RedrawStonesFromMatrix(Matrix: TPlayGroundMatrix);
    function AskForLevel: String;
    function AreJumpsPossible: boolean;
    procedure StoneDraggingAllow(Stone: TImage; Allow: boolean);
    procedure NewGame(Filename: string);
    function LevelTime: String;
    procedure DestroyLevel;
    procedure RefreshTime;
    procedure RefreshPoints;
    procedure RefreshStonesRemoved;
    procedure CountPoints(t: TFieldType);
    procedure RemoveStone(x, y: integer; count_points: boolean);
    procedure DoJump(SourceTag, DestTag: integer);
    function CanJump(x, y: integer): boolean;
    function MayJump(SourceX, SourceY, DestX, DestY: integer): boolean; overload;
    function MayJump(SourceTag, DestTag: integer): boolean; overload;
    procedure StoneDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure StoneDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawField(x, y: integer; t: TFieldProperties; halftabs: integer);
    function DrawStone(fieldtype: TFieldType; panel: TPanel): TImage;
    function DrawStoneBox(x, y, tag, halftabs: integer; isGoal: boolean): TPanel;
    procedure BuildPlayground(LevelArray: TLevelArray);
    function FieldState(t: TFieldType): TFieldState; overload;
    function FieldState(f: TField): TFieldState; overload;
    function FieldState(x, y: integer): TFieldState; overload;
    procedure ClearMatrix(Matrix: TPlayGroundMatrix; FreeVCL: boolean);
    function CloneMatrix(Source: TPlayGroundMatrix): TPlayGroundMatrix;
    procedure LoadPictureForType(FieldType: TFieldType; Picture: TPicture);
    function MatrixWorth(Matrix: TPlayGroundMatrix): integer;
    function FieldTypeWorth(t: TFieldType): integer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  About, Finish, Choice, Functions, History, HighScore, Help, Constants;

{$R *.dfm}

function TMainForm.MatrixWorth(Matrix: TPlayGroundMatrix): integer;
var
  i, j: integer;
begin
  result := 0;
  for i := Low(Matrix) to High(Matrix) do
  begin
    for j := Low(Matrix[i]) to High(Matrix[i]) do
    begin
      Inc(result, FieldTypeWorth(Matrix[i][j].FieldType));
    end;
  end;
end;

procedure TMainForm.ClearMatrix(Matrix: TPlayGroundMatrix; FreeVCL: boolean);
var
  i, j: integer;
begin
  for i := Low(Matrix) to High(Matrix) do
  begin
    for j := Low(Matrix[i]) to High(Matrix[i]) do
    begin
      if FreeVCL then
      begin
        if Assigned(Matrix[i][j].Stone) then Matrix[i][j].Stone.Free;
        if Assigned(Matrix[i][j].Panel) then Matrix[i][j].Panel.Free;
      end;
    end;
    SetLength(Matrix[i], 0);
  end;
  SetLength(Matrix, 0);
end;

procedure TMainForm.RedrawStonesFromMatrix(Matrix: TPlayGroundMatrix);
var
  i, j: integer;
begin
  for i := Low(Matrix) to High(Matrix) do
  begin
    for j := Low(Matrix[i]) to High(Matrix[i]) do
    begin
      if Assigned(Matrix[i][j].Stone) then
      begin
        LoadPictureForType(Matrix[i][j].FieldType, Matrix[i][j].Stone.Picture);
        StoneDraggingAllow(Matrix[i][j].Stone, FieldState(Matrix[i][j].FieldType) <> fsAvailable);
      end;
    end;
  end;
end;

procedure TMainForm.DestroyLevel;
var
  i: Integer;
begin
  MPauseTime.Enabled := false;
  Timer.Enabled := false;

  MRestartGame.Enabled := false;

  LevelFile := '';

  CountedSeconds := 0;
  RefreshTime;

  Points := 0;
  RefreshPoints;

  LevelRemovedStones := 0;
  LevelTotalStones := 0;
  RefreshStonesRemoved;

  JumpHistory.Clear;

  ClearMatrix(PlayGroundMatrix, true);
  ClearMatrix(OriginalPlayGroundMatrix, false);
  for i := 0 to Length(PrevPlaygroundMatrixes)-1 do
    ClearMatrix(PrevPlaygroundMatrixes[i], false);
  SetLength(PrevPlaygroundMatrixes, 0);
  MUndo.Enabled := false;

  SetLength(LookupFieldCoordinateArray, 0);
end;

procedure TMainForm.LoadPictureForType(FieldType: TFieldType; Picture: TPicture);
begin
  case FieldType of
    ftEmpty:  Picture.Bitmap.LoadFromResourceName(HInstance, RES_EMPTY);
    ftGreen:  Picture.Bitmap.LoadFromResourceName(HInstance, RES_GREEN);
    ftYellow: Picture.Bitmap.LoadFromResourceName(HInstance, RES_YELLOW);
    ftRed:    Picture.Bitmap.LoadFromResourceName(HInstance, RES_RED);
  end;
end;

function TMainForm.DrawStone(fieldtype: TFieldType; panel: TPanel): TImage;
begin
  result := TImage.Create(panel);
  result.Parent := panel;
  LoadPictureForType(fieldtype, result.Picture);
  result.Width := panel.Width - 2*MET_SHAPE_MARGIN;
  result.Height := panel.Height - 2*MET_SHAPE_MARGIN;
  result.Left := MET_SHAPE_MARGIN;
  result.Top := MET_SHAPE_MARGIN;
  result.Center := true;
  result.Transparent := true;

  result.Tag := panel.Tag;
  result.OnDragOver := panel.OnDragOver;
  result.OnDragDrop := panel.OnDragDrop;

  StoneDraggingAllow(result, FieldState(fieldtype) <> fsAvailable);
end;

procedure TMainForm.StoneDraggingAllow(Stone: TImage; Allow: boolean);
begin
  if Allow then
  begin
    Stone.DragMode := dmAutomatic;
    (Stone.Parent as TPanel).DragMode := dmAutomatic;
  end
  else
  begin
    Stone.DragMode := dmManual;
    (Stone.Parent as TPanel).DragMode := dmManual;
  end;
end;

function TMainForm.DrawStoneBox(x, y, tag, halftabs: integer; isGoal: boolean): TPanel;
begin
  result := TPanel.Create(Playground);
  result.Parent := Playground;
  if isGoal then
  begin
    result.BevelInner := bvLowered;
  end;
  result.Color := Playground.Color;
  result.BevelOuter := bvLowered;
  result.Width := MET_FIELD_SIZE;
  result.Height := MET_FIELD_SIZE;
  result.Left := x * (MET_FIELD_SIZE+MET_FIELD_SPACE) + MET_FIELD_SPACE - (halftabs*MET_HALFTAB_SIZE);
  result.Top := y * (MET_FIELD_SIZE+MET_FIELD_SPACE) + MET_FIELD_SPACE;

  result.Tag := tag;
  result.OnDragOver := StoneDragOver;
  result.OnDragDrop := StoneDragDrop;
end;

procedure TMainForm.MExitClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.FieldState(t: TFieldType): TFieldState;
begin
  result := fsError;
  case t of
    ftLocked:        result := fsLocked;
    ftLockedWithTab: result := fsLocked;
    ftEmpty:         result := fsAvailable;
    ftGreen:         result := fsStone;
    ftYellow:        result := fsStone;
    ftRed:           result := fsStone;
  end;
end;

function TMainForm.FieldState(f: TField): TFieldState;
begin
  result := FieldState(f.FieldType);
end;

function TMainForm.FieldState(x, y: integer): TFieldState;
begin
  result := fsError;
  if (x < Low(PlayGroundMatrix)) or (x > High(PlayGroundMatrix)) then exit;
  if (y < Low(PlayGroundMatrix[x])) or (y > High(PlayGroundMatrix[x])) then exit;

  result := FieldState(PlayGroundMatrix[x][y]);
end;

procedure TMainForm.RefreshTime;
begin
  Statistics.Panels.Items[0].Text := Format(LNG_TIME, [LevelTime]);
end;

procedure TMainForm.RefreshStonesRemoved;
begin
  Statistics.Panels.Items[1].Text := Format(LNG_STONES_REMOVED, [LevelRemovedStones, LevelTotalStones]);
end;

procedure TMainForm.RefreshPoints;
begin
  Statistics.Panels.Items[2].Text := Format(LNG_POINTS, [Points]);
end;

function TMainForm.FieldTypeWorth(t: TFieldType): integer;
begin
  if t = ftGreen then result := 10
  else if t = ftYellow then result := 20
  else if t = ftRed then result := 30
  else result := 0;
end;

procedure TMainForm.CountPoints(t: TFieldType);
begin
  inc(Points, FieldTypeWorth(t));
  RefreshPoints;
end;

procedure TMainForm.RemoveStone(x, y: integer; count_points: boolean);
begin
  if count_points then
  begin
    CountPoints(PlayGroundMatrix[x, y].FieldType);
    Inc(LevelRemovedStones);
    RefreshStonesRemoved;
  end;
  PlayGroundMatrix[x, y].FieldType := ftEmpty;
  LoadPictureForType(PlayGroundMatrix[x, y].FieldType, PlayGroundMatrix[x, y].Stone.Picture);
  StoneDraggingAllow(PlayGroundMatrix[x, y].Stone, false);
end;

function TMainForm.CanJump(x, y: integer): boolean;
begin
  if FieldState(x, y) <> fsStone then
  begin
    result := false;
    exit;
  end;

  result := true;

  if MayJump(x, y, x+2, y) then exit;
  if MayJump(x, y, x-2, y) then exit;
  if MayJump(x, y, x, y+2) then exit;
  if MayJump(x, y, x, y-2) then exit;

  if AllowDiagonalMoves then
  begin
    if MayJump(x, y, x-2, y-2) then exit;
    if MayJump(x, y, x+2, y-2) then exit;
    if MayJump(x, y, x-2, y+2) then exit;
    if MayJump(x, y, x+2, y+2) then exit;
  end;

  result := false;
end;

function TMainForm.AreJumpsPossible: boolean;
var
  i, j: integer;
begin
  result := false;
  for i := Low(PlayGroundMatrix) to High(PlayGroundMatrix) do
  begin
    for j := Low(PlayGroundMatrix[i]) to High(PlayGroundMatrix[i]) do
    begin
      if CanJump(i, j) then
      begin
        result := true;
        break;
      end;
      if result then break;
    end;
  end;
end;

procedure TMainForm.DoJump(SourceTag, DestTag: integer);
var
  d, s: TPoint;
  old_fieldtype: TFieldType;
  res: Integer;
begin
  if not MayJump(SourceTag, DestTag) then exit;

  d := LookupFieldCoordinateArray[DestTag];
  s := LookupFieldCoordinateArray[SourceTag];

  JumpHistory.Add(Format(LNG_JUMP_LOG, [SourceTag+1, s.x+1, s.y+1, DestTag+1, d.x+1, d.y+1]));

  {$REGION 'Stein entfernen und Punkte vergeben'}
  if AllowDiagonalMoves then
  begin
    if (s.X-2 = d.X) and (s.Y-2 = d.Y) and (FieldState(s.X-1, s.Y-1) = fsStone) then RemoveStone(s.X-1, s.Y-1, true);
    if (s.X-2 = d.X) and (s.Y+2 = d.Y) and (FieldState(s.X-1, s.Y+1) = fsStone) then RemoveStone(s.X-1, s.Y+1, true);
    if (s.X+2 = d.X) and (s.Y-2 = d.Y) and (FieldState(s.X+1, s.Y-1) = fsStone) then RemoveStone(s.X+1, s.Y-1, true);
    if (s.X+2 = d.X) and (s.Y+2 = d.Y) and (FieldState(s.X+1, s.Y+1) = fsStone) then RemoveStone(s.X+1, s.Y+1, true);
  end;

  if (s.X+2 = d.X) and (s.Y = d.Y) and (FieldState(s.X+1, s.Y  ) = fsStone) then RemoveStone(s.X+1, s.Y, true);
  if (s.X-2 = d.X) and (s.Y = d.Y) and (FieldState(s.X-1, s.Y  ) = fsStone) then RemoveStone(s.X-1, s.Y, true);
  if (s.X = d.X) and (s.Y+2 = d.Y) and (FieldState(s.X  , s.Y+1) = fsStone) then RemoveStone(s.X, s.Y+1, true);
  if (s.X = d.X) and (s.Y-2 = d.Y) and (FieldState(s.X  , s.Y-1) = fsStone) then RemoveStone(s.X, s.Y-1, true);
  {$ENDREGION}

  // Den Timer erst nach dem ersten Zug starten
  // oder nach einer Pause neustarten
  if not Timer.Enabled then
  begin
    MPauseTime.Enabled := true;
    Timer.Enabled := true;
  end;

  MRestartGame.Enabled := true;

  // Sound abspielen
  if MEnableSound.Checked then PlaySound(RES_JUMP, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE);

  {$REGION 'Nun den Stein springen lassen'}
  old_fieldtype := PlayGroundMatrix[s.X, s.Y].FieldType; // Steinfarbe merken
  RemoveStone(s.X, s.Y, false); // Eigenen Stein entfernen. Keine Punkte zählen, da das unser eigener Stein ist, der springt
  PlayGroundMatrix[d.X, d.Y].FieldType := old_fieldtype; // Farbe wiederherstellen
  LoadPictureForType(PlayGroundMatrix[d.X, d.Y].FieldType, PlayGroundMatrix[d.X, d.Y].Stone.Picture); // Stein an neue Position malen
  StoneDraggingAllow(PlayGroundMatrix[d.X, d.Y].Stone, true); // Und die Drag-Eigenschaft erneuern
  {$ENDREGION}

  {$REGION 'Sind weitere Sprünge möglich oder ist das Spiel vorbei?'}
  if not AreJumpsPossible then
  begin
    MPauseTime.Enabled := false;
    Timer.Enabled := false;
    RefreshTime;
    if MEnableSound.Checked then
    begin
      if LevelRemovedStones = LevelTotalStones then
        PlaySound(RES_FINISH, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE)
      else
        PlaySound(RES_LOSE, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE);
    end;
    res := FinishForm.Execute(ExtractFileNameWithoutExt(LevelFile), Points, LevelTotalStones, LevelRemovedStones, CountedSeconds, JumpHistory);
    if (res = mrOK) and FinishForm.ReplayCheckbox.Checked then RestartLevel;
  end;
  {$ENDREGION}

  SetLength(PrevPlaygroundMatrixes, Length(PrevPlaygroundMatrixes)+1);
  PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1] := CloneMatrix(PlaygroundMatrix);
  MUndo.Enabled := true;
end;

function TMainForm.MayJump(SourceX, SourceY, DestX, DestY: integer): boolean;
begin
  result := false;

  // Check 1: Ist das Zielfeld überhaupt leer?
  if FieldState(DestX, DestY) <> fsAvailable then exit;

  // Check 2: Befindet sich ein Stein zwischen Source und Destination und ist der Abstand 2?
  if AllowDiagonalMoves then
  begin
    if (SourceX-2 = DestX) and (SourceY-2 = DestY) and (FieldState(SourceX-1, SourceY-1) = fsStone) then result := true;
    if (SourceX-2 = DestX) and (SourceY+2 = DestY) and (FieldState(SourceX-1, SourceY+1) = fsStone) then result := true;
    if (SourceX+2 = DestX) and (SourceY-2 = DestY) and (FieldState(SourceX+1, SourceY-1) = fsStone) then result := true;
    if (SourceX+2 = DestX) and (SourceY+2 = DestY) and (FieldState(SourceX+1, SourceY+1) = fsStone) then result := true;
  end;

  if (SourceX+2 = DestX) and (SourceY   = DestY) and (FieldState(SourceX+1, SourceY  ) = fsStone) then result := true;
  if (SourceX-2 = DestX) and (SourceY   = DestY) and (FieldState(SourceX-1, SourceY  ) = fsStone) then result := true;
  if (SourceX   = DestX) and (SourceY+2 = DestY) and (FieldState(SourceX  , SourceY+1) = fsStone) then result := true;
  if (SourceX   = DestX) and (SourceY-2 = DestY) and (FieldState(SourceX  , SourceY-1) = fsStone) then result := true;
end;

function TMainForm.MayJump(SourceTag, DestTag: integer): boolean;
var
  s, d: TPoint;
begin
  d := LookupFieldCoordinateArray[DestTag];
  s := LookupFieldCoordinateArray[SourceTag];

  result := MayJump(s.X, s.Y, d.X, d.Y);
end;

procedure TMainForm.StoneDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  DoJump(TComponent(Source).Tag, TComponent(Sender).Tag);
end;

procedure TMainForm.StoneDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := MayJump(TComponent(Source).Tag, TComponent(Sender).Tag);
end;

procedure TMainForm.DrawField(x, y: integer; t: TFieldProperties; halftabs: integer);
var
  newField: TField;
  index: integer;
begin
  if (t.Typ = ftLocked) or (t.Typ = ftLockedWithTab) then exit;

  index := Length(LookupFieldCoordinateArray);

  newField.FieldType := t.Typ;
  newField.Goal := t.Goal;
  newField.Panel := DrawStoneBox(x, y, index, halftabs, t.Goal);
  newField.Stone := DrawStone(t.Typ, newField.Panel);
  if FieldState(t.Typ) = fsStone then Inc(LevelTotalStones);

  SetLength(LookupFieldCoordinateArray, index + 1);
  LookupFieldCoordinateArray[index].X := x;
  LookupFieldCoordinateArray[index].Y := y;

  if Length(PlayGroundMatrix) < x+1 then SetLength(PlayGroundMatrix, x+1);
  if Length(PlayGroundMatrix[x]) < y+1 then SetLength(PlayGroundMatrix[x], y+1);
  PlaygroundMatrix[x, y] := newField;
end;

function TMainForm.CloneMatrix(Source: TPlayGroundMatrix): TPlayGroundMatrix;
var
  i, j: integer;
begin
  SetLength(result, Length(Source));
  for i := Low(Source) to High(Source) do
  begin
    SetLength(result[i], Length(Source[i]));
    for j := Low(Source[i]) to High(Source[i]) do
    begin
      result[i][j].FieldType := Source[i][j].FieldType;
      result[i][j].Goal      := Source[i][j].Goal;
      result[i][j].Panel     := Source[i][j].Panel;
      result[i][j].Stone     := Source[i][j].Stone;
    end;
  end;
end;

procedure TMainForm.BuildPlayground(LevelArray: TLevelArray);
var
  i, j, halftabs, cur_x: integer;
  max_x, max_y, old_cw, old_ch: integer;
begin
  PlayGround.Visible := false;

  // Die Dimensionen ermitteln
  max_x := 0;
  for i := Low(LevelArray) to High(LevelArray) do
  begin
    halftabs := 0;
    for j := Low(LevelArray[i]) to High(LevelArray[i]) do
    begin
      if LevelArray[i][j].Typ = ftLockedWithTab then inc(halftabs);
      DrawField(j, i, LevelArray[i][j], halftabs);
    end;
    cur_x := High(LevelArray[i]) + 1;
    if cur_x > max_x then max_x := cur_x;
  end;
  max_y := High(LevelArray) + 1;

  PlayGround.Visible := true;

  // Die aktuellen Dimensionen merken
  old_cw := ClientWidth;
  old_ch := ClientHeight;

  // Das Form an das Level anpassen
  PlayGround.Width := MET_FIELD_SPACE + max_x * (MET_FIELD_SPACE + MET_FIELD_SIZE);
  PlayGround.Height := MET_FIELD_SPACE + max_y * (MET_FIELD_SPACE + MET_FIELD_SIZE);
  ClientWidth := 2 * MET_OUTER_MARGIN + PlayGround.Width;
  ClientHeight := 2 * MET_OUTER_MARGIN + PlayGround.Height + Statistics.Height;

  Statistics.Panels.Items[0].Width := Round(ClientWidth*MET_PERCENT_PNL_TIME);
  Statistics.Panels.Items[1].Width := Round(ClientWidth*MET_PERCENT_PNL_STONES);

  // Wenn sich das Form vergrößert oder verkleinert hat, neu justieren
  if (old_cw <> ClientWidth) or (old_ch <> ClientHeight) then
  begin
    Left := Screen.Width div 2 - Width div 2;
    Top := Screen.Height div 2 - Height div 2;
    
    // Playground mittig setzen, falls die Mindestgröße für die
    // Punkteanzeige unterschritten wurde,
    PlayGround.Left := ClientWidth div 2 - PlayGround.Width div 2;
    PlayGround.Top := ClientHeight div 2 - PlayGround.Height div 2;
  end;

  OriginalPlayGroundMatrix := CloneMatrix(PlayGroundMatrix);
  SetLength(PrevPlaygroundMatrixes,1);
  PrevPlaygroundMatrixes[0] := CloneMatrix(PlayGroundMatrix);
  MUndo.Enabled := false;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if mainform.Focused then Inc(CountedSeconds);
  RefreshTime;
end;

function TMainForm.LevelTime: String;
begin
  result := SecondsToTimeString(CountedSeconds);
end;

procedure TMainForm.NewGame(Filename: string);
var
  LevelString: String;
  LevelArray: TLevelArray;
begin                           
  DestroyLevel;
  LevelFile := Filename;
  LevelString := ReadFile(LevelFile);
  LevelArray := LevelStringToLevelArray(LevelString, true);
  if Length(LevelArray) = 0 then Exit;
  BuildPlayground(LevelArray);
  if not AreJumpsPossible then
  begin
    ShowMessage(LNG_LVL_INVALID_NO_JUMP);
  end;
  RefreshTime;
  RefreshStonesRemoved;
  RefreshPoints;
end;

procedure TMainForm.MNewGameClick(Sender: TObject);
begin
  LevelFile := AskForLevel;
  if LevelFile <> '' then
  begin
    NewGame(LevelFile);
  end;
end;

procedure TMainForm.MAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

function TMainForm.AskForLevel: String;
begin
  LevelChoice.ShowModal;

  if LevelChoice.ModalResult <> mrOK then
  begin
    result := '';
    exit;
  end;

  result := LevelChoice.SelectedLevel;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LevelFile := AskForLevel;
  if LevelFile <> '' then
  begin
    NewGame(LevelFile);
  end
  else Close();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JumpHistory := TStringList.Create;
  LoadSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JumpHistory.Free;
end;

procedure TMainForm.MJumpHistoryClick(Sender: TObject);
begin
  HistoryForm.JumpMemo.Lines.Assign(JumpHistory);
  HistoryForm.ShowModal;
end;

procedure TMainForm.RestartLevel;
begin
  MPauseTime.Enabled := false;
  Timer.Enabled := false;

  MRestartGame.Enabled := false;

  CountedSeconds := 0;
  RefreshTime;

  Points := 0;
  RefreshPoints;

  LevelRemovedStones := 0;
  RefreshStonesRemoved;

  JumpHistory.Clear;

  RedrawStonesFromMatrix(OriginalPlayGroundMatrix);
  SetNewPlayGroundMatrix(OriginalPlayGroundMatrix);
  SetLength(PrevPlaygroundMatrixes,1);
  PrevPlaygroundMatrixes[0] := CloneMatrix(OriginalPlayGroundMatrix);
  MUndo.Enabled := false;
end;

procedure TMainForm.SetNewPlayGroundMatrix(Matrix: TPlayGroundMatrix);
begin
  ClearMatrix(PlayGroundMatrix, false); // Memory Leak verhindern
  PlayGroundMatrix := CloneMatrix(Matrix);
end;

procedure TMainForm.MRestartGameClick(Sender: TObject);
begin
  RestartLevel;
end;

procedure TMainForm.MUndoClick(Sender: TObject);
var
  PrevWorth: integer;
  NewWorth: integer;
begin
  if Length(PrevPlaygroundMatrixes) > 1 then
  begin
    PrevWorth := MatrixWorth(PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1]);

    ClearMatrix(PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1], false);
    SetLength(PrevPlaygroundMatrixes, Length(PrevPlaygroundMatrixes)-1);

    NewWorth := MatrixWorth(PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1]);
    RedrawStonesFromMatrix(PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1]);
    SetNewPlayGroundMatrix(PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1]);

    JumpHistory.Delete(JumpHistory.Count-1);

    Dec(LevelRemovedStones);
    RefreshStonesRemoved;

    Dec(Points, NewWorth-PrevWorth);
    RefreshPoints;

    // Sound abspielen
    if MEnableSound.Checked then PlaySound(RES_UNDO, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE);
  end;

  MUndo.Enabled := Length(PrevPlaygroundMatrixes) > 1;
end;

procedure TMainForm.MHighScoresClick(Sender: TObject);
begin
  HighScoreForm.Execute(ExtractFileNameWithoutExt(LevelFile));
end;

procedure TMainForm.MPauseTimeClick(Sender: TObject);
begin
  MPauseTime.Enabled := false;
  Timer.Enabled := false;
end;

procedure TMainForm.LoadSettings;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(REG_KEY) then
    begin
      if reg.ValueExists(REG_SOUND) then
        MEnableSound.Checked := reg.ReadBool(REG_SOUND);
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TMainForm.SaveSettings;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(REG_KEY, true) then
    begin
      reg.WriteBool(REG_SOUND, MEnableSound.Checked);
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  if FinishForm.NameEdit.Text <> '' then
  begin
    FinishForm.SaveSettings;
  end;
end;

procedure TMainForm.MHelpClick(Sender: TObject);
begin
  HelpForm.ShowModal;
end;

procedure TMainForm.MEnableSoundClick(Sender: TObject);
begin
  MEnableSound.Checked := not MEnableSound.Checked; 
end;

end.
