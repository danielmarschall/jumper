unit LevelFunctions;

interface

uses
  SysUtils, Dialogs, Functions, ExtCtrls, Graphics;

type
  TFieldType = (ftLocked, ftLockedWithTab, ftEmpty, ftGreen, ftYellow, ftRed);

  TFieldProperties = record
    Typ: TFieldType;
    Goal: Boolean;
  end;

  TLevelType = (ltStandard, ltDiagonal, ltError);

  TLevelArray = array of array of TFieldProperties;

  TLevelError = (leNone, leInvalidElement, leNoIndicator, leMultipleIndicator,
                 leLevelIncomplete, leHeaderError, leInvalidGoal);

procedure DrawLevelPreview(LevelString: string; Image: TImage; BackgroundColor: TColor);
function GetLevelType(LevelString: string): TLevelType;
function CheckLevelIntegrity(LevelString: string; ShowErrors: boolean): TLevelError; overload;
function CheckLevelIntegrity(LevelString: string): TLevelError; overload;
function LevelStringToLevelArray(LevelString: string; ShowErrors: boolean): TLevelArray;

var
  AllowDiagonalMoves: boolean;

implementation

const
  PREVIEW_BLOCK_SIZE = 10; // Enthält Field und Abstand
  PREVIEW_TAB_SIZE = PREVIEW_BLOCK_SIZE div 2; // 5
  HEADER_SIZE = 3;
  ERED = '3';
  EYEL = '2';
  EGRE = '1';
  ELOC = 'X';
  EEMP = 'E';
  EIND = '!';
  ETAR = '>';
  ESPE = '*';
  TY_DIA = 'D';
  TY_NOR = 'N';

resourcestring
  LNG_LVL_INVALID_ELEMENT = 'Level invalid: There are invalid elements in the file.'+#13#10#13#10+'Valid elements are "1", "2", "3", "X", "*", "E" and ">" as goal prefix.';
  LNG_LVL_INVALID_WIDTH = 'Level invalid: No width indicator ("!") found.';
  LNG_LVL_INVALID_HEIGHT_MUTLIPLE = 'Level invalid: The level''s actual length is not a multiple of the width.';
  LNG_LVL_INVALID_MULTIPLE_WIND = 'Level invalid: There are multiple width indicators ("!").';
  LNG_LVL_INVALID_HEADER = 'Level invalid: The header is invalid. It does not match the structure "1(D|N)~".';
  LNG_INVALID_GOAL = 'Level invalid: A goal does not point to a valid accessable element ("3", "2", "1" or "E").';

procedure DrawLevelPreview(LevelString: string; Image: TImage; BackgroundColor: TColor);
var
  LevelArray: TLevelArray;
  i, j: integer;
  t: TFieldType;
  halftabs: integer;
begin
  LevelArray := nil;

  ClearImage(Image, BackgroundColor);

  LevelArray := LevelStringToLevelArray(LevelString, false);

  for i := Low(LevelArray) to High(LevelArray) do
  begin
    halftabs := 0;
    for j := Low(LevelArray[i]) to High(LevelArray[i]) do
    begin
      t := LevelArray[i][j].Typ;

      case t of
        ftLocked: Image.Canvas.Brush.Color := BackgroundColor;
        ftLockedWithTab: begin
          Image.Canvas.Brush.Color := BackgroundColor;
          inc(halftabs);
        end;
        ftEmpty: Image.Canvas.Brush.Color := clWhite;
        ftGreen: Image.Canvas.Brush.Color := clLime;
        ftYellow: Image.Canvas.Brush.Color := clYellow;
        ftRed: Image.Canvas.Brush.Color := clRed;
      end;

      if LevelArray[i][j].Goal then
        Image.Canvas.Pen.Color := clBlack
      else
        Image.Canvas.Pen.Color := BackgroundColor;

      Image.Canvas.Rectangle(j*PREVIEW_BLOCK_SIZE - halftabs*PREVIEW_TAB_SIZE, i*PREVIEW_BLOCK_SIZE, j*PREVIEW_BLOCK_SIZE - halftabs*PREVIEW_TAB_SIZE+PREVIEW_BLOCK_SIZE, i*PREVIEW_BLOCK_SIZE+PREVIEW_BLOCK_SIZE);
    end;
  end;
end;

function GetLevelType(LevelString: string): TLevelType;
begin
  if CheckLevelIntegrity(LevelString) = leNone then
  begin
    if Copy(LevelString, 2, 1) = TY_DIA then
    begin
      result := ltDiagonal;
    end
    else // if Copy(LevelString, 2, 1) = TY_NOR
    begin
      result := ltStandard;
    end;
  end
  else
  begin
    result := ltError;
  end;
end;

procedure ShowErrorMessage(error: TLevelError);
begin
  case error of
    leNone: ;
    leInvalidElement: ShowMessage(LNG_LVL_INVALID_ELEMENT);
    leNoIndicator: ShowMessage(LNG_LVL_INVALID_WIDTH);
    leMultipleIndicator: ShowMessage(LNG_LVL_INVALID_MULTIPLE_WIND);
    leLevelIncomplete: ShowMessage(LNG_LVL_INVALID_HEIGHT_MUTLIPLE);
    leHeaderError: ShowMessage(LNG_LVL_INVALID_HEADER);
    leInvalidGoal: ShowMessage(LNG_INVALID_GOAL);
  end;
end;

function CheckLevelIntegrity(LevelString: string; ShowErrors: boolean): TLevelError;
begin
  result := CheckLevelIntegrity(LevelString);
  if ShowErrors then ShowErrorMessage(result);
end;

function CheckLevelIntegrity(LevelString: string): TLevelError;
var
  W: integer;
  H: extended;
  header, h_ver, h_dia, h_del, tmp: string;
  p: integer;
begin
  result := leNone;

  // Entfernt die Zeilenumbrüche

  LevelString := RemoveLineBreaks(LevelString);

  // Check 1: Ist der Header OK?

  header := copy(LevelString, 1, HEADER_SIZE);

  h_ver := copy(header, 1, 1);
  if h_ver <> '1' then
  begin
    result := leHeaderError;
    Exit;
  end;

  h_dia := copy(header, 2, 1);
  if (h_dia <> TY_DIA) and (h_dia <> TY_NOR) then
  begin
    result := leHeaderError;
    Exit;
  end;

  h_del := copy(header, 3, 1);
  if h_del <> '~' then
  begin
    result := leHeaderError;
    Exit;
  end;

  LevelString := copy(LevelString, HEADER_SIZE+1, Length(LevelString)-HEADER_SIZE);

  // Check 2: Steht das ggf. vorhandenen ">" vor einem gültigen Feld 1, 2, 3, E?

  p := Position(LevelString, ETAR);

  while (p <> -1) do
  begin
    tmp := copy(LevelString, p+1, 1);

    if (tmp <> EEMP) and (tmp <> EGRE) and (tmp <> EYEL) and (tmp <> ERED) then
    begin
      result := leInvalidGoal;
      Exit;
    end;

    LevelString := StringReplace(LevelString, ETAR, '', []); // Dieses Ziel entfernen

    p := Position(LevelString, ETAR);
  end;

  // Check 3: Kommt überhaupt ein "!" vor?

  W := Position(LevelString, EIND);

  if W = -1 then
  begin
    result := leNoIndicator;
    Exit;
  end;

  // Check 4: Kam das "!" mehrmals vor?

  LevelString := StringReplace(LevelString, EIND, '', []); // Das Erste entfernen

  if Position(LevelString, EIND) <> -1 then // gibt es ein Zweites?
  begin
    result := leMultipleIndicator;
    Exit;
  end;

  // Check 5: Geht das Level nicht in einem Quadrat oder Rechteck auf?

  H := (Length(LevelString) - 1) / W;

  if not Ganzzahlig(H) then
  begin
    result := leLevelIncomplete;
    Exit;
  end;

  // Check 6: Gibt es ungültige Elemente im LevelString?

  LevelString := StringReplace(LevelString, ESPE, '', [rfReplaceAll]);
  LevelString := StringReplace(LevelString, ELOC, '', [rfReplaceAll]);
  LevelString := StringReplace(LevelString, EEMP, '', [rfReplaceAll]);
  LevelString := StringReplace(LevelString, EGRE, '', [rfReplaceAll]);
  LevelString := StringReplace(LevelString, EYEL, '', [rfReplaceAll]);
  LevelString := StringReplace(LevelString, ERED, '', [rfReplaceAll]);

  if Length(LevelString) > 0 then
  begin
    result := leInvalidElement;
    Exit;
  end;

  // Check 7: Kann im Level gesprungen werden

  { Wird hier nicht abgeprüft, da dafür zuerst der PlayGround gebaut sein muss.
    Es ist außerdem eher ein logischer Fehler, kein Fehler in der Levelstruktur! }
end;

function LevelStringToLevelArray(LevelString: string; ShowErrors: boolean): TLevelArray;
var
  i, j, j_dec, c: integer;
  m: string;
  t: TFieldType;
  W, H: integer;
  err: TLevelError;
  NextIsGoal: boolean;
begin
  // Zuerst nach Fehlern suchen
  err := CheckLevelIntegrity(LevelString, ShowErrors);
  if err <> leNone then exit;

  // Headerinformationen auslesen
  AllowDiagonalMoves := copy(LevelString, 2, 1) = TY_DIA;

  // Header entfernen
  LevelString := copy(LevelString, HEADER_SIZE+1, Length(LevelString)-HEADER_SIZE);

  // Entfernt die Zeilenumbrüche
  LevelString := RemoveLineBreaks(LevelString);

  // Dimensionen abmessen
  W := Position(StringReplace(LevelString, ETAR, '', [rfReplaceAll]), EIND) - 1;
  LevelString := StringReplace(LevelString, EIND, '', [rfReplaceAll]);
  H := Length(LevelString) div W;

  c := 1;
  NextIsGoal := false;

  SetLength(result, round(H));
  for i := Low(result) to High(result) do
  begin
    j_dec := 0;
    SetLength(result[i], round(W));
    for j := Low(result[i]) to High(result[i])+1 do  // +1 wegen dem möglichen zusätzlichem ">"
    begin
      if (j = High(result[i])+1) and (j_dec = 0) then break;
      m := Copy(LevelString, c, 1);
      if m = ETAR then
      begin
        NextIsGoal := true;
        inc(j_dec);
      end
      else
      begin
             if m = EEMP then t := ftEmpty
        else if m = EGRE then t := ftGreen
        else if m = EYEL then t := ftYellow
        else if m = ERED then t := ftRed
        else if m = ESPE then t := ftLockedWithTab
        else t := ftLocked;
        result[i][j-j_dec].Typ := t;
        result[i][j-j_dec].Goal := NextIsGoal;
        if NextIsGoal then NextIsGoal := false;
      end;
      inc(c);
    end;
  end;
end;

end.
