unit Main;

interface

uses
  Windows, SysUtils, Classes, Graphics, Dialogs, StdCtrls, Menus, Controls,
  ComCtrls, ExtCtrls, Forms, MMSystem, LevelFunctions, Registry;

type
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
    Aboutthislevel1: TMenuItem;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MHelpClick(Sender: TObject);
    procedure MUndoClick(Sender: TObject);
    procedure Aboutthislevel1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    NoCloseQuery: boolean;
    CountedSeconds: Integer;
    LevelFile: String;
    PrevPlaygroundMatrixes: array of TPlayGroundMatrix;
    PlaygroundMatrix: TPlayGroundMatrix;
    Points: Integer;
    LevelTotalStones: Integer;
    LevelRemovedStones: Integer;
    JumpHistory: TStringList;
    Level: TLevel;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure RestartLevel;
    procedure SetNewPlayGroundMatrix(Matrix: TPlayGroundMatrix);
    procedure RedrawStonesFromMatrix(Matrix: TPlayGroundMatrix);
    function AskForLevel: String;
    procedure StoneDraggingAllow(Stone: TImage; Allow: boolean);
    procedure NewGame(Filename: string);
    function LevelTime: String;
    procedure DestroyLevel;
    procedure RefreshTime;
    procedure RefreshPoints;
    procedure RefreshStonesRemoved;
    procedure RemoveStone(x, y: integer; count_points: boolean);
    procedure DoJump(SourceTag, DestTag: integer);
    function MayJump(SourceTag, DestTag: integer): boolean; overload;
    procedure StoneDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure StoneDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawField(x, y: integer; var f: TField);
    function DrawStone(f: TField): TImage;
    function DrawStoneBox(x, y, tag: integer; f: TField): TPanel;
    procedure LoadPictureForType(FieldType: TFieldType; Picture: TPicture);
    function GoalStatus: TGoalStatus;
  end;

var
  MainForm: TMainForm;

implementation

uses
  About, Finish, Choice, Functions, History, HighScore, Help, Constants, Math;

{$R *.dfm}

type
  TFieldVclData = class(TObject)
  public
    Panel: TPanel;
    Stone: TImage;
    destructor Destroy; override;
  end;

{ TMainForm }

procedure TMainForm.RedrawStonesFromMatrix(Matrix: TPlayGroundMatrix);
var
  x, y: integer;
begin
  for x := Low(Matrix.Fields) to High(Matrix.Fields) do
  begin
    for y := Low(Matrix.Fields[x]) to High(Matrix.Fields[x]) do
    begin
      if Assigned(Matrix.Fields[x,y].Data) and
         Assigned(TFieldVclData(Matrix.Fields[x,y].Data).Stone) then
      begin
        LoadPictureForType(Matrix.Fields[x,y].FieldType,
                           TFieldVclData(Matrix.Fields[x,y].Data).Stone.Picture);
        StoneDraggingAllow(TFieldVclData(Matrix.Fields[x,y].Data).Stone,
                           Matrix.Fields[x,y].FieldState <> fsAvailable);
      end;
    end;
  end;
end;

procedure TMainForm.DestroyLevel;
var
  i: Integer;
begin
  MPauseTime.Checked := false;
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

  PlayGroundMatrix.ClearMatrix(true);
  for i := 0 to Length(PrevPlaygroundMatrixes)-1 do
    PrevPlaygroundMatrixes[i].ClearMatrix(false);
  SetLength(PrevPlaygroundMatrixes, 0);
  MUndo.Enabled := false;

  if Assigned(Level) then FreeAndNil(Level);
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

function TMainForm.DrawStone(f: TField): TImage;
var
  panel: TPanel;
begin
  panel := TFieldVclData(f.Data).Panel;

  result := TImage.Create(panel);
  result.Parent := panel;
  LoadPictureForType(f.FieldType, result.Picture);
  result.Width := panel.Width - 2*MET_SHAPE_MARGIN;
  result.Height := panel.Height - 2*MET_SHAPE_MARGIN;
  result.Left := MET_SHAPE_MARGIN;
  result.Top := MET_SHAPE_MARGIN;
  result.Center := true;
  result.Transparent := true;

  result.Tag := panel.Tag;
  result.OnDragOver := panel.OnDragOver;
  result.OnDragDrop := panel.OnDragDrop;

  StoneDraggingAllow(result, f.FieldState <> fsAvailable);
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

function TMainForm.DrawStoneBox(x, y, tag: integer; f: TField): TPanel;
begin
  result := TPanel.Create(Playground);
  result.Parent := Playground;
  if f.Goal then
  begin
    result.BevelInner := bvLowered;
  end;
  result.Color := Playground.Color;
  result.BevelOuter := bvLowered;
  result.Width := MET_FIELD_SIZE;
  result.Height := MET_FIELD_SIZE;
  result.Left := x * (MET_FIELD_SIZE+MET_FIELD_SPACE) + MET_FIELD_SPACE + (f.Indent*MET_HALFTAB_SIZE);
  result.Top := y * (MET_FIELD_SIZE+MET_FIELD_SPACE) + MET_FIELD_SPACE;

  result.Tag := tag;
  result.OnDragOver := StoneDragOver;
  result.OnDragDrop := StoneDragDrop;
end;

procedure TMainForm.MExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.RefreshTime;
begin
  Statistics.Panels.Items[0].Text := Format(LNG_TIME, [LevelTime]);
end;

procedure TMainForm.RefreshStonesRemoved;
resourcestring
  LNG_STONES_REMOVED = '%d of %d stones removed'; // Jumping stone not counted
begin
  Statistics.Panels.Items[1].Text := Format(LNG_STONES_REMOVED, [LevelRemovedStones, LevelTotalStones-1]);
end;

procedure TMainForm.RefreshPoints;
begin
  Statistics.Panels.Items[2].Text := Format(LNG_POINTS, [Points]);
end;

procedure TMainForm.RemoveStone(x, y: integer; count_points: boolean);
begin
  if count_points then
  begin
    Inc(Points, FieldTypeWorth(PlayGroundMatrix.Fields[x,y].FieldType));
    RefreshPoints;

    Inc(LevelRemovedStones);
    RefreshStonesRemoved;
  end;

  PlayGroundMatrix.Fields[x,y].FieldType := ftEmpty;
  LoadPictureForType(PlayGroundMatrix.Fields[x,y].FieldType,
                     TFieldVclData(PlayGroundMatrix.Fields[x,y].Data).Stone.Picture);
  StoneDraggingAllow(TFieldVclData(PlayGroundMatrix.Fields[x,y].Data).Stone, false);
end;

procedure TMainForm.Aboutthislevel1Click(Sender: TObject);
var
  mode: string;
  goalYeSNo: string;
resourcestring
  LNG_BOARD = 'Board: %s';
  LNG_MODE = 'Mode: %s';
  LNG_STONES_TOTAL = 'Stones: %d';
  LNG_GOAL_AVAILABLE = 'Target field defined';
  LNG_NO_GOAL = 'No target field';
begin
  case Level.GameMode of
    gmNormal:    mode := 'Diagonal';
    gmDiagonal:  mode := 'Normal';
    gmUndefined: mode := '?';
  end;

  if GoalStatus = gsNoGoal then
    goalYeSNo := LNG_NO_GOAL
  else
    goalYeSNo := LNG_GOAL_AVAILABLE;

  ShowMessage(Format(LNG_BOARD, [ExtractFileNameWithoutExt(LevelFile)]) + #13#10 +
              #13#10 +
              Format(LNG_MODE, [mode]) + #13#10 +
              Format(LNG_STONES_TOTAL, [LevelTotalStones]) + #13#10 +
              goalYesNo);
end;

procedure TMainForm.DoJump(SourceTag, DestTag: integer);
resourcestring
  LNG_JUMP_LOG = '[%d, %d] -> [%d, %d];';
var
  d, s: TCoord;
  old_fieldtype: TFieldType;
  res: Integer;
begin
  if not MayJump(SourceTag, DestTag) then exit;

  s := PlaygroundMatrix.IndexToCoord(SourceTag);
  d := PlaygroundMatrix.IndexToCoord(DestTag);

  JumpHistory.Add(Format(LNG_JUMP_LOG, [s.x+1, s.y+1, d.x+1, d.y+1]));

  {$REGION 'Stein entfernen und Punkte vergeben'}
  if Level.GameMode = gmDiagonal then
  begin
    if (s.X-2 = d.X) and (s.Y-2 = d.Y) and (PlayGroundMatrix.FieldState(s.X-1, s.Y-1) = fsOccupied) then RemoveStone(s.X-1, s.Y-1, true);
    if (s.X-2 = d.X) and (s.Y+2 = d.Y) and (PlayGroundMatrix.FieldState(s.X-1, s.Y+1) = fsOccupied) then RemoveStone(s.X-1, s.Y+1, true);
    if (s.X+2 = d.X) and (s.Y-2 = d.Y) and (PlayGroundMatrix.FieldState(s.X+1, s.Y-1) = fsOccupied) then RemoveStone(s.X+1, s.Y-1, true);
    if (s.X+2 = d.X) and (s.Y+2 = d.Y) and (PlayGroundMatrix.FieldState(s.X+1, s.Y+1) = fsOccupied) then RemoveStone(s.X+1, s.Y+1, true);
  end;

  if (s.X+2 = d.X) and (s.Y = d.Y) and (PlayGroundMatrix.FieldState(s.X+1, s.Y  ) = fsOccupied) then RemoveStone(s.X+1, s.Y, true);
  if (s.X-2 = d.X) and (s.Y = d.Y) and (PlayGroundMatrix.FieldState(s.X-1, s.Y  ) = fsOccupied) then RemoveStone(s.X-1, s.Y, true);
  if (s.X = d.X) and (s.Y+2 = d.Y) and (PlayGroundMatrix.FieldState(s.X  , s.Y+1) = fsOccupied) then RemoveStone(s.X, s.Y+1, true);
  if (s.X = d.X) and (s.Y-2 = d.Y) and (PlayGroundMatrix.FieldState(s.X  , s.Y-1) = fsOccupied) then RemoveStone(s.X, s.Y-1, true);
  {$ENDREGION}

  // Den Timer erst nach dem ersten Zug starten
  // oder nach einer Pause neustarten
  MPauseTime.Checked := false;
  MPauseTime.Enabled := true;
  Timer.Enabled := true;

  // Sound abspielen
  if MEnableSound.Checked then PlaySound(RES_JUMP, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE);

  {$REGION 'Nun den Stein springen lassen'}
  old_fieldtype := PlayGroundMatrix.Fields[s.X, s.Y].FieldType; // Steinfarbe merken
  RemoveStone(s.X, s.Y, false); // Eigenen Stein entfernen. Keine Punkte zählen, da das unser eigener Stein ist, der springt
  PlayGroundMatrix.Fields[d.X, d.Y].FieldType := old_fieldtype; // Farbe wiederherstellen
  LoadPictureForType(PlayGroundMatrix.Fields[d.X, d.Y].FieldType,
                     TFieldVclData(PlayGroundMatrix.Fields[d.X, d.Y].Data).Stone.Picture); // Stein an neue Position malen
  StoneDraggingAllow(TFieldVclData(PlayGroundMatrix.Fields[d.X, d.Y].Data).Stone, true); // Und die Drag-Eigenschaft erneuern
  {$ENDREGION}

  {$REGION 'Sind weitere Sprünge möglich oder ist das Spiel vorbei?'}
  if not PlayGroundMatrix.CanJump(Level.GameMode = gmDiagonal) then
  begin
    MPauseTime.Checked := false;
    MPauseTime.Enabled := false;
    Timer.Enabled := false;
    RefreshTime;
    if MEnableSound.Checked then
    begin
      if LevelRemovedStones = LevelTotalStones-1 then
      begin
        if GoalStatus in [gsLastStoneInGoalRed, gsLastStoneInGoalYellow, gsLastStoneInGoalGreen] then
          PlaySound(RES_WIN2, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE)
        else
          PlaySound(RES_WIN1, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE)
      end
      else
        PlaySound(RES_LOSE, HInstance, SND_ASYNC or SND_NOWAIT or SND_RESOURCE);
    end;
    res := FinishForm.Execute(ExtractFileNameWithoutExt(LevelFile), Points, LevelTotalStones, LevelRemovedStones, CountedSeconds, GoalStatus, JumpHistory);
    if (res = mrOK) and FinishForm.ReplayCheckbox.Checked then RestartLevel;
  end;
  {$ENDREGION}

  SetLength(PrevPlaygroundMatrixes, Length(PrevPlaygroundMatrixes)+1);
  PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1] := PlaygroundMatrix.CloneMatrix;
  MUndo.Enabled := true;
end;

function TMainForm.MayJump(SourceTag, DestTag: integer): boolean;
var
  s, d: TCoord;
begin
  s := PlayGroundMatrix.IndexToCoord(SourceTag);
  d := PlayGroundMatrix.IndexToCoord(DestTag);

  result := PlaygroundMatrix.CanJump(s, d, Level.GameMode = gmDiagonal);
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

procedure TMainForm.DrawField(x, y: integer; var f: TField);
var
  index: integer;
begin
  if f.FieldType = ftFullSpace then exit;

  index := PlaygroundMatrix.CoordToIndex(x, y);

  if not Assigned(f.Data) then f.Data := TFieldVclData.Create;
  TFieldVclData(f.Data).Panel := DrawStoneBox(x, y, index, f);
  TFieldVclData(f.Data).Stone := DrawStone(f);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if MPauseTime.Checked then exit;
  if mainform.Focused then Inc(CountedSeconds);
  RefreshTime;
end;

function TMainForm.LevelTime: String;
begin
  result := FormatDateTime('hh:nn:ss', CountedSeconds / SecsPerDay)
end;

procedure TMainForm.NewGame(Filename: string);
resourcestring
  LNG_LVL_INVALID_NO_JUMP = 'Warning! The level is not playable. There are no jumps possible.';
var
  y, x: integer;
  max_x, max_y: integer;
  p: TPanel;
begin
  DestroyLevel;

  MPauseTime.Checked := true;
  MPauseTime.Enabled := true;
  Timer.Enabled := true;
  MRestartGame.Enabled := true;

  LevelFile := Filename;
  Level := TLevel.Create(LevelFile);

  Level.FillPlaygroundMatrix(PlaygroundMatrix, true);
  if Length(PlaygroundMatrix.Fields) = 0 then Exit;

  PlayGround.Visible := false;

  max_x := 0;
  max_y := 0;
  for x := Low(PlaygroundMatrix.Fields) to High(PlaygroundMatrix.Fields) do
  begin
    for y := Low(PlaygroundMatrix.Fields[x]) to High(PlaygroundMatrix.Fields[x]) do
    begin
      if PlaygroundMatrix.Fields[x,y].FieldState = fsOccupied then
        Inc(LevelTotalStones);
      DrawField(x, y, PlaygroundMatrix.Fields[x,y]);
      p := TFieldVclData(PlaygroundMatrix.Fields[x,y].Data).Panel;
      if Assigned(p) then
      begin
        max_x := Max(max_x, p.Left + p.Width);
        max_y := Max(max_y, p.Top  + p.Height);
      end;
    end;
  end;

  PlayGround.Visible := true;

  // Das Form an das Level anpassen
  PlayGround.Top    := MET_OUTER_MARGIN;
  PlayGround.Left   := MET_OUTER_MARGIN;
  PlayGround.Width  := max_x;
  PlayGround.Height := max_y;
  ClientWidth       := 2 * MET_OUTER_MARGIN + PlayGround.Width;
  ClientHeight      := 2 * MET_OUTER_MARGIN + PlayGround.Height + Statistics.Height;

  // If the board is too small, ClientWidth/ClientHeight will stop at a minimum value
  // in this case, we make sure that the Playground is centered
  PlayGround.Left := ClientWidth div 2 - Playground.Width div 2;
  PlayGround.Top := (ClientHeight - Statistics.Height) div 2 - Playground.Height div 2;

  Statistics.Panels.Items[0].Width := Round(ClientWidth * MET_PERCENT_PNL_TIME);
  Statistics.Panels.Items[1].Width := Round(ClientWidth * MET_PERCENT_PNL_STONES);

  SetLength(PrevPlaygroundMatrixes,1);
  PrevPlaygroundMatrixes[0] := PlayGroundMatrix.CloneMatrix;
  MUndo.Enabled := false;

  if not PlayGroundMatrix.CanJump(Level.GameMode = gmDiagonal) then
  begin
    MessageDlg(LNG_LVL_INVALID_NO_JUMP, mtError, [mbOk], 0);
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
  else
  begin
    NoCloseQuery := true;
    Close;
  end;
end;

function TMainForm.GoalStatus: TGoalStatus;
begin
  result := PlaygroundMatrix.GoalStatus(LevelTotalStones - LevelRemovedStones);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JumpHistory := TStringList.Create;
  LoadSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DestroyLevel;
  JumpHistory.Free;
end;

procedure TMainForm.MJumpHistoryClick(Sender: TObject);
begin
  HistoryForm.JumpMemo.Lines.Assign(JumpHistory);
  HistoryForm.ShowModal;
end;

procedure TMainForm.RestartLevel;
var
  i: Integer;
begin
  MPauseTime.Checked := true;
  MPauseTime.Enabled := true;
  Timer.Enabled := true;

  CountedSeconds := 0;
  RefreshTime;

  Points := 0;
  RefreshPoints;

  LevelRemovedStones := 0;
  RefreshStonesRemoved;

  JumpHistory.Clear;

  RedrawStonesFromMatrix(PrevPlaygroundMatrixes[0]);
  SetNewPlayGroundMatrix(PrevPlaygroundMatrixes[0]);
  for i := 1 to Length(PrevPlaygroundMatrixes)-1 do
    PrevPlaygroundMatrixes[i].ClearMatrix(false);
  SetLength(PrevPlaygroundMatrixes, 1);

  MUndo.Enabled := false;
end;

procedure TMainForm.SetNewPlayGroundMatrix(Matrix: TPlayGroundMatrix);
begin
  PlayGroundMatrix.ClearMatrix(false); // Memory Leak verhindern
  PlayGroundMatrix := Matrix.CloneMatrix;
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
    PrevWorth := PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1].MatrixWorth;

    PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1].ClearMatrix(false);
    SetLength(PrevPlaygroundMatrixes, Length(PrevPlaygroundMatrixes)-1);

    NewWorth := PrevPlaygroundMatrixes[Length(PrevPlaygroundMatrixes)-1].MatrixWorth;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  LNG_REALLY_QUIT = 'Do you really want to quit?';
begin
  CanClose := NoCloseQuery or (MessageDlg(LNG_REALLY_QUIT, mtConfirmation, mbYesNoCancel, 0) = mrYes);
end;

procedure TMainForm.MHelpClick(Sender: TObject);
begin
  HelpForm.ShowModal;
end;

{ TFieldVclData }

destructor TFieldVclData.Destroy;
begin
  if Assigned(Stone) then Stone.Free;
  if Assigned(Panel) then Panel.Free;
  inherited;
end;

end.
