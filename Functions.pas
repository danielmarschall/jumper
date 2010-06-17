unit Functions;

interface

uses
  SysUtils, Dialogs, Graphics, Classes, ExtCtrls;

function ExtractFileNameWithoutExt(filename: string): string;
function SecondsToTimeString(Seconds: Integer): string;
procedure ClearImage(Image: TImage; BackgroundColor: TColor);
function Ganzzahlig(num: extended): boolean;
function Explode(Separator, Text: String): TStringList;
function Position(FullString, Search: String): Integer;
function ReadFile(InputFile: string): string;
function RemoveLineBreaks(inp: string): string;

implementation

resourcestring
  LNG_COULD_NOT_OPEN_FILE = 'Could not open file "%s".';

function ExtractFileNameWithoutExt(filename: string): string;
begin
  result := ExtractFileName(filename);
  result := copy(result, 1, Length(result)-Length(ExtractFileExt(result)));
end;

function SecondsToTimeString(Seconds: Integer): string;
var
  h, m, s: integer;
  tim: TDateTime;
begin
  h := 0;
  m := 0;
  s := Seconds;

  while s - 60*60 >= 0 do
  begin
    dec(s, 60*60);
    inc(h);
  end;

  while s - 60 >= 0 do
  begin
    dec(s, 60);
    inc(m);
  end;

  tim := EncodeTime(h, m, s, 0);

  result := TimeToStr(tim);
end;

procedure ClearImage(Image: TImage; BackgroundColor: TColor);
var
  OldPenColor, OldBrushColor: TColor;
begin
  OldPenColor := Image.Canvas.Pen.Color;
  OldBrushColor := Image.Canvas.Brush.Color;
  Image.Canvas.Pen.Color := BackgroundColor;
  Image.Canvas.Brush.Color := BackgroundColor;
  Image.Canvas.Rectangle(0, 0, Image.Width, Image.Height);
  Image.Canvas.Pen.Color := OldPenColor;
  Image.Canvas.Brush.Color := OldBrushColor;
end;

function Ganzzahlig(num: extended): boolean;
begin
  result := num = round(num);
end;

function Explode(Separator, Text: String): TStringList;
var
  pos: integer;
  tmp: string;
begin
  result := TStringList.Create;
  
  while Length(Text) > 0 do
  begin
    pos := Functions.Position(Text, Separator);

    if pos = -1 then
    begin
      tmp := Text;
      Text := '';
    end
    else
    begin
      tmp := copy(Text, 1, pos-1);
      Text := copy(Text, pos+1, Length(Text)-pos);
    end;

    result.Add(tmp);
  end;
end;

function Position(FullString, Search: String): Integer;
var
  x: Integer;
begin
  x := Length(StrPos(PChar(FullString), PChar(Search)));
  if x = 0 then
    result := -1
  else
    result := Length(FullString) - x + 1;
end;

function ReadFile(InputFile: string): string;
var
  f: textfile;
  tmp: string;
begin
  result := '';

  if not FileExists(InputFile) then
  begin
    ShowMessage(Format(LNG_COULD_NOT_OPEN_FILE, [InputFile]));
    Exit;
  end;

  AssignFile(f, InputFile);
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, tmp);
    result := result + tmp + #13#10;
  end;
  CloseFile(f);
end;

function RemoveLineBreaks(inp: string): string;
begin
  inp := StringReplace(inp, #13, '', [rfReplaceAll]);
  inp := StringReplace(inp, #10, '', [rfReplaceAll]);
  result := inp;
end;

end.
 