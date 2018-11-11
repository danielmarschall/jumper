unit LevelFunctions;

interface

uses
  SysUtils, Dialogs, Functions, ExtCtrls, Graphics, Classes, Math;

type
  TFieldType = (ftFullSpace, ftHalfSpace, ftEmpty, ftRed, ftYellow, ftGreen);

  TFieldProperties = record
    Typ: TFieldType;
    Goal: Boolean;
  end;

  TGameMode = (gmUndefined, gmNormal, gmDiagonal);

  TLevelArray = array of array of TFieldProperties;

  TLevelError = (leNone, leInvalidElement, leEmptyBoard, leRowInvalidLength,
                 leUnsupportedVersion, leUnsupportedMode);

  TLevel = class(TObject)
  private
    FStringList: TStringList;
    procedure Load(ABoardFile: string);
  public
    constructor Create(ABoardFile: string);
    destructor Destroy; override;
    function LevelStringToLevelArray(ShowErrors: boolean): TLevelArray;
    function CheckLevelIntegrity: TLevelError; overload;
    function CheckLevelIntegrity(ShowErrors: boolean): TLevelError; overload;
    function GetGameMode: TGameMode;
  end;

procedure DrawLevelPreview(Level: TLevel; Image: TImage; BackgroundColor: TColor);
function FieldTypeWorth(t: TFieldType): integer;

implementation

procedure DrawLevelPreview(Level: TLevel; Image: TImage; BackgroundColor: TColor);
var
  LevelArray: TLevelArray;
  y, x: integer;
  t: TFieldType;
  halftabs: integer;
const
  PREVIEW_BLOCK_SIZE = 10; // Enthält Field und Abstand
  PREVIEW_TAB_SIZE = PREVIEW_BLOCK_SIZE div 2; // 5
begin
  LevelArray := nil;

  ClearImage(Image, BackgroundColor);

  LevelArray := Level.LevelStringToLevelArray(false);

  for y := Low(LevelArray) to High(LevelArray) do
  begin
    halftabs := 0;
    for x := Low(LevelArray[y]) to High(LevelArray[y]) do
    begin
      t := LevelArray[y][x].Typ;

      case t of
        ftFullSpace: Image.Canvas.Brush.Color := BackgroundColor;
        ftHalfSpace: begin
          Image.Canvas.Brush.Color := BackgroundColor;
          inc(halftabs);
        end;
        ftEmpty: Image.Canvas.Brush.Color := clWhite;
        ftGreen: Image.Canvas.Brush.Color := clLime;
        ftYellow: Image.Canvas.Brush.Color := clYellow;
        ftRed: Image.Canvas.Brush.Color := clRed;
      end;

      if LevelArray[y][x].Goal then
        Image.Canvas.Pen.Color := clBlack
      else
        Image.Canvas.Pen.Color := BackgroundColor;

      Image.Canvas.Rectangle((x-halftabs)*PREVIEW_BLOCK_SIZE + halftabs*PREVIEW_TAB_SIZE,
                             y*PREVIEW_BLOCK_SIZE,
                             (x-halftabs)*PREVIEW_BLOCK_SIZE + halftabs*PREVIEW_TAB_SIZE + PREVIEW_BLOCK_SIZE,
                             y*PREVIEW_BLOCK_SIZE                                        + PREVIEW_BLOCK_SIZE);
    end;
  end;
end;

function FieldTypeWorth(t: TFieldType): integer;
begin
  if t = ftGreen then result := 10
  else if t = ftYellow then result := 20
  else if t = ftRed then result := 30
  else result := 0;
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
  longestLine: Integer;
  thisLine: Integer;
  y: Integer;
  x: Integer;
  Line: string;
  lch, uch: char;
  ch: char;
begin
  // Zuerst nach Fehlern suchen
  err := CheckLevelIntegrity(ShowErrors);
  if err <> leNone then exit;

  // Längste Zeile finden
  longestLine := 0;
  for i := NUM_HEADERS to FStringList.Count-1 do
  begin
    longestLine := Max(longestLine, Length(FStringList.Strings[i]));
  end;

  // Nun Matrix aufbauen
  SetLength(result, 0);
  for i := NUM_HEADERS to FStringList.Count-1 do
  begin
    y := i - NUM_HEADERS;

    SetLength(result, Length(result)+1); // add line to matrix
    SetLength(result[y], longestLine);

    Line := FStringList.Strings[i];

    for x := 0 to LongestLine-1 do
    begin
      ch := Copy(Line,x+1,1)[1];
      lch := LowerCase(ch)[1];
      uch := UpperCase(ch)[1];
      case lch of
        '*': t := ftFullSpace;
        '.': t := ftHalfSpace;
        'e': t := ftEmpty;
        'r': t := ftRed;
        'y': t := ftYellow;
        'g': t := ftGreen;
      end;

      result[y][x].Typ := t;
      result[y][x].Goal := (ch = uch) and (ch <> lch);
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
  W: integer;
  H: extended;
  header, h_ver, h_dia, h_del, tmp: string;
  p: integer;
  i: Integer;
  Line: string;
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

  if FStringList.Count - NUM_HEADERS = 0 then
  begin
    result := leEmptyBoard;
    exit;
  end;

  // Check 3: Geht das Level nicht in einem Quadrat oder Rechteck auf?

  for i := NUM_HEADERS to FStringList.Count-1 do
  begin
    if Length(FStringList.Strings[i]) <> Length(FStringList.Strings[NUM_HEADERS]) then
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

  // Check 5: Kann im Level gesprungen werden

  { Wird hier nicht abgeprüft, da dafür zuerst der PlayGround gebaut sein muss.
    Es ist außerdem eher ein logischer Fehler, kein Fehler in der Levelstruktur! }
end;

end.
