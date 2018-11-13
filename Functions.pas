unit Functions;

interface

uses
  SysUtils, Dialogs, Graphics, Classes, ExtCtrls;

function ExtractFileNameWithoutExt(filename: string): string;
procedure ClearImage(Image: TImage; BackgroundColor: TColor);
function Explode(Separator, Text: String): TStringList;
function Position(FullString, Search: String): Integer;
function DotsAtBeginning(s: string): integer;
function DotsAtEnd(s: string): integer;

implementation

function ExtractFileNameWithoutExt(filename: string): string;
begin
  result := ExtractFileName(filename);
  result := copy(result, 1, Length(result)-Length(ExtractFileExt(result)));
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

function DotsAtBeginning(s: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(s) do
  begin
    if s[i] = '.' then
      Inc(result)
    else
      Exit;
  end;
end;

function DotsAtEnd(s: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := Length(s) downto 1 do
  begin
    if s[i] = '.' then
      Inc(result)
    else
      Exit;
  end;
end;

end.
 