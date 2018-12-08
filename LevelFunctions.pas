unit LevelFunctions;

interface

uses
  SysUtils, Dialogs, Functions, ExtCtrls, Classes, Math;

type
  TFieldType = (ftUndefined, ftFullSpace, ftEmpty, ftRed, ftYellow, ftGreen);

  TFieldProperties = record
    Typ: TFieldType;
    Goal: Boolean;
  end;

  TGameMode = (gmUndefined, gmNormal, gmDiagonal);

  TRow = record
    Indent: integer;
    Fields: array of TFieldProperties;
  end;
  TLevelArray = array of TRow;

  TLevelError = (leUndefined, leNone, leInvalidElement, leEmptyBoard, leRowInvalidLength,
                 leUnsupportedVersion, leUnsupportedMode);

  TLevel = class(TObject)
  private
    FStringList: TStringList;
    procedure Load(ABoardFile: string);
    function GetGameMode: TGameMode;
  public
    constructor Create(ABoardFile: string);
    destructor Destroy; override;
    function LevelStringToLevelArray(ShowErrors: boolean): TLevelArray;
    function CheckLevelIntegrity: TLevelError; overload;
    function CheckLevelIntegrity(ShowErrors: boolean): TLevelError; overload;
    property GameMode: TGameMode read GetGameMode;
  end;

  TField = record
    FieldType: TFieldType;
    Goal: Boolean;
    Panel: TPanel;
    Stone: TImage;
  end;

  TGoalStatus = (gsUndefined, gsNoGoal, gsMultipleStonesRemaining, gsLastStoneInGoalRed, gsLastStoneInGoalYellow, gsLastStoneInGoalGreen, gsLastStoneOutsideGoal);

  TFieldState = (fsUndefined, fsError, fsLocked, fsAvailable, fsStone);

  TPlayGroundMatrix = record
    Fields: array of array of TField;
  public
    function MatrixHasGoal: boolean;
    function GoalFieldType: TFieldType;
    function MatrixWorth: integer;
    procedure ClearMatrix(FreeVCL: boolean);
    function CloneMatrix: TPlayGroundMatrix;
    function FieldState(t: TFieldType): TFieldState; overload;
    function FieldState(f: TField): TFieldState; overload;
    function FieldState(x, y: integer): TFieldState; overload;
    function CanJump(SourceX, SourceY, DestX, DestY: integer; DiagonalOK: boolean): boolean; overload;
    function CanJump(SourceX, SourceY: integer; DiagonalOK: boolean): boolean; overload;
    function CanJump(DiagonalOK: boolean): boolean; overload;
  end;

function FieldTypeWorth(t: TFieldType): integer;

implementation

function FieldTypeWorth(t: TFieldType): integer;
begin
  if t = ftGreen then result := 10
  else if t = ftYellow then result := 20
  else if t = ftRed then result := 30
  else result := 0;
end;

{ TPlayGroundMatrix }

function TPlayGroundMatrix.MatrixHasGoal: boolean;
var
  x, y: integer;
begin
  result := false;
  for x := Low(Fields) to High(Fields) do
  begin
    for y := Low(Fields[x]) to High(Fields[x]) do
    begin
      result := result or Fields[x][y].Goal;
    end;
  end;
end;

function TPlayGroundMatrix.GoalFieldType: TFieldType;
var
  x, y: integer;
begin
  result := ftEmpty; // Damit der Compiler nicht meckert
  for x := Low(Fields) to High(Fields) do
  begin
    for y := Low(Fields[x]) to High(Fields[x]) do
    begin
      if Fields[x][y].Goal then result := Fields[x][y].FieldType
    end;
  end;
end;

function TPlayGroundMatrix.MatrixWorth: integer;
var
  x, y: integer;
begin
  result := 0;
  for x := Low(Fields) to High(Fields) do
  begin
    for y := Low(Fields[x]) to High(Fields[x]) do
    begin
      Inc(result, FieldTypeWorth(Fields[x][y].FieldType));
    end;
  end;
end;

procedure TPlayGroundMatrix.ClearMatrix(FreeVCL: boolean);
var
  x, y: integer;
begin
  for x := Low(Fields) to High(Fields) do
  begin
    for y := Low(Fields[x]) to High(Fields[x]) do
    begin
      if FreeVCL then
      begin
        if Assigned(Fields[x][y].Stone) then Fields[x][y].Stone.Free;
        if Assigned(Fields[x][y].Panel) then Fields[x][y].Panel.Free;
      end;
    end;
    SetLength(Fields[x], 0);
  end;
  SetLength(Fields, 0);
end;

function TPlayGroundMatrix.CloneMatrix: TPlayGroundMatrix;
var
  x, y: integer;
begin
  SetLength(result.Fields, Length(Fields));
  for x := Low(Fields) to High(Fields) do
  begin
    SetLength(result.Fields[x], Length(Fields[x]));
    for y := Low(Fields[x]) to High(Fields[x]) do
    begin
      result.Fields[x][y].FieldType := Fields[x][y].FieldType;
      result.Fields[x][y].Goal      := Fields[x][y].Goal;
      result.Fields[x][y].Panel     := Fields[x][y].Panel;
      result.Fields[x][y].Stone     := Fields[x][y].Stone;
    end;
  end;
end;

function TPlayGroundMatrix.FieldState(t: TFieldType): TFieldState;
begin
  result := fsError;
  case t of
    ftFullSpace: result := fsLocked;
    ftEmpty:     result := fsAvailable;
    ftGreen:     result := fsStone;
    ftYellow:    result := fsStone;
    ftRed:       result := fsStone;
  end;
end;

function TPlayGroundMatrix.FieldState(f: TField): TFieldState;
begin
  result := FieldState(f.FieldType);
end;

function TPlayGroundMatrix.FieldState(x, y: integer): TFieldState;
begin
  result := fsError;
  if (x < Low(Fields)) or (x > High(Fields)) then exit;
  if (y < Low(Fields[x])) or (y > High(Fields[x])) then exit;

  result := FieldState(Fields[x][y]);
end;

function TPlayGroundMatrix.CanJump(SourceX, SourceY, DestX, DestY: integer; DiagonalOK: boolean): boolean;
begin
  result := false;

  // Check 1: Ist das Zielfeld überhaupt leer?
  if FieldState(DestX, DestY) <> fsAvailable then exit;

  // Check 2: Befindet sich ein Stein zwischen Source und Destination und ist der Abstand 2?
  if DiagonalOK then
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

function TPlayGroundMatrix.CanJump(SourceX, SourceY: integer; DiagonalOK: boolean): boolean;
begin
  if FieldState(SourceX, SourceY) <> fsStone then
  begin
    result := false;
    exit;
  end;

  result := true;

  if CanJump(SourceX, SourceY, SourceX+2, SourceY, DiagonalOK) then exit;
  if CanJump(SourceX, SourceY, SourceX-2, SourceY, DiagonalOK) then exit;
  if CanJump(SourceX, SourceY, SourceX, SourceY+2, DiagonalOK) then exit;
  if CanJump(SourceX, SourceY, SourceX, SourceY-2, DiagonalOK) then exit;

  if DiagonalOK then
  begin
    if CanJump(SourceX, SourceY, SourceX-2, SourceY-2, DiagonalOK) then exit;
    if CanJump(SourceX, SourceY, SourceX+2, SourceY-2, DiagonalOK) then exit;
    if CanJump(SourceX, SourceY, SourceX-2, SourceY+2, DiagonalOK) then exit;
    if CanJump(SourceX, SourceY, SourceX+2, SourceY+2, DiagonalOK) then exit;
  end;

  result := false;
end;

function TPlayGroundMatrix.CanJump(DiagonalOK: boolean): boolean;
var
  x, y: integer;
begin
  result := false;
  for x := Low(Fields) to High(Fields) do
  begin
    for y := Low(Fields[x]) to High(Fields[x]) do
    begin
      if CanJump(x, y, DiagonalOK) then
      begin
        result := true;
        break;
      end;
      if result then break;
    end;
  end;
end;

{ TLevel }

const NUM_HEADERS = 2;

constructor TLevel.Create(ABoardFile: string);
begin
  inherited Create;
  FStringList := TStringList.Create;
  Load(ABoardFile);
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FStringList);

  inherited;
end;

function TLevel.GetGameMode: TGameMode;
begin
  if LowerCase(FStringList.Strings[1]) = 'mode: normal' then
    result := gmNormal
  else if LowerCase(FStringList.Strings[1]) = 'mode: diagonal' then
    result := gmDiagonal
  else
    result := gmUndefined;
end;

procedure TLevel.Load(ABoardFile: string);
var
  i: Integer;
begin
  FStringList.Clear;
  FStringList.LoadFromFile(ABoardFile);

  // Remove whitespaces and empty lines
  for i := FStringList.Count-1 downto NUM_HEADERS do
  begin
    FStringList.Strings[i] := StringReplace(FStringList.Strings[i], ' ', '', [rfReplaceAll]);
    if FStringList.Strings[i] = '' then FStringList.Delete(i);
  end;
end;

function TLevel.LevelStringToLevelArray(ShowErrors: boolean): TLevelArray;
var
  i: integer;
  t: TFieldType;
  err: TLevelError;
  y: Integer;
  x: Integer;
  Line: string;
  lch, uch: char;
  ch: char;
begin
  // Zuerst nach Fehlern suchen
  err := CheckLevelIntegrity(ShowErrors);
  if err <> leNone then exit;

  // Nun Matrix aufbauen
  SetLength(result, 0);
  for i := NUM_HEADERS to FStringList.Count-1 do
  begin
    y := i - NUM_HEADERS;

    SetLength(result, Length(result)+1); // add line to matrix

    Line := FStringList.Strings[i];
    result[y].Indent := DotsAtBeginning(Line) - DotsAtEnd(Line);
    Line := StringReplace(Line, '.', '', [rfReplaceAll]);
    SetLength(result[y].Fields, Length(Line));

    for x := 0 to Length(Line)-1 do
    begin
      ch := Line[x+1];
      lch := LowerCase(ch)[1];
      uch := UpperCase(ch)[1];

      t := ftUndefined;
      case lch of
        '*': t := ftFullSpace;
        'e': t := ftEmpty;
        'r': t := ftRed;
        'y': t := ftYellow;
        'g': t := ftGreen;
      end;

      result[y].Fields[x].Typ := t;
      result[y].Fields[x].Goal := (ch = uch) and (ch <> lch);
    end;
  end;
end;

function TLevel.CheckLevelIntegrity(ShowErrors: boolean): TLevelError;
resourcestring
  LNG_LVL_INVALID_ELEMENT = 'Level invalid: There are invalid elements in the file.'+#13#10#13#10+'Valid elements are r/R, y/Y, g/G, e/E, . and *.';
  LNG_LVL_UNSUPPORTED_VERSION = 'Level format invalid: Version not supported.';
  LNG_LVL_UNSUPPORTED_MODE = 'Level format invalid: Mode not supported.';
  LNG_LVL_EMPTY_BOARD = 'Level invalid: Board is empty.';
  LNG_LVL_INVALID_LENGTH = 'Level invalid: Lines don''t have an equal amount of elements.';
begin
  result := CheckLevelIntegrity;
  if ShowErrors then
  begin
    case result of
      leNone: ;
      leInvalidElement: MessageDlg(LNG_LVL_INVALID_ELEMENT, mtError, [mbOk], 0);
      leUnsupportedVersion: MessageDlg(LNG_LVL_UNSUPPORTED_VERSION, mtError, [mbOk], 0);
      leUnsupportedMode: MessageDlg(LNG_LVL_UNSUPPORTED_MODE, mtError, [mbOk], 0);
      leEmptyBoard: MessageDlg(LNG_LVL_EMPTY_BOARD, mtError, [mbOk], 0);
      leRowInvalidLength: MessageDlg(LNG_LVL_INVALID_LENGTH, mtError, [mbOk], 0);
    end;
  end;
end;

function TLevel.CheckLevelIntegrity: TLevelError;
var
  tmp: string;
  i: Integer;
  Line: string;
  firstLine: string;
  thisLine: string;
begin
  result := leNone;

  // Check 1: Ist der Header OK?

  if LowerCase(FStringList.Strings[0]) <> 'version 2' then
  begin
    result := leUnsupportedVersion;
    exit;
  end;

  if ((LowerCase(FStringList.Strings[1]) <> 'mode: normal') and (LowerCase(FStringList.Strings[1]) <> 'mode: diagonal')) then
  begin
    result := leUnsupportedMode;
    exit;
  end;

  // Check 2: Ist das Brett leer?

  tmp := '';
  for i := NUM_HEADERS to FStringList.Count-1 do tmp := tmp + FStringList.Strings[i];
  if Trim(StringReplace(tmp, '.', '', [rfReplaceAll])) = '' then
  begin
    result := leEmptyBoard;
    exit;
  end;

  // Check 3: Geht das Level nicht in einem Quadrat oder Rechteck auf?

  firstLine := StringReplace(FStringList.Strings[NUM_HEADERS], '.', '', [rfReplaceAll]);
  for i := NUM_HEADERS to FStringList.Count-1 do
  begin
    thisLine := StringReplace(FStringList.Strings[i], '.', '', [rfReplaceAll]);
    if Length(thisLine) <> Length(firstLine) then
    begin
      result := leRowInvalidLength; // at row y-NUM_HEADERS
      exit;
    end;
  end;

  // Check 4: Gibt es ungültige Elemente in den Zeilen?

  for i := NUM_HEADERS to FStringList.Count-1 do
  begin
    Line := FStringList.Strings[i];

    Line := StringReplace(Line, '.', '', [rfReplaceAll]);
    Line := StringReplace(Line, '*', '', [rfReplaceAll]);
    Line := StringReplace(Line, 'r', '', [rfReplaceAll, rfIgnoreCase]);
    Line := StringReplace(Line, 'y', '', [rfReplaceAll, rfIgnoreCase]);
    Line := StringReplace(Line, 'g', '', [rfReplaceAll, rfIgnoreCase]);
    Line := StringReplace(Line, 'e', '', [rfReplaceAll, rfIgnoreCase]);

    if Length(Line) > 0 then
    begin
      result := leInvalidElement; // at row y-NUM_HEADERS
      Exit;
    end;
  end;

  // Check 5: Kann im Level gesprungen werden?

  { Wird hier nicht abgeprüft, da dafür zuerst der PlayGround gebaut sein muss.
    Es ist außerdem eher ein logischer Fehler, kein Fehler in der Levelstruktur! }
end;

end.
