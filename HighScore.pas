unit HighScore;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, System.UITypes;

type
  THighScoreForm = class(TForm)
    PageControl: TPageControl;
    RemainingTab: TTabSheet;
    TimeTab: TTabSheet;
    RemainingList: TListView;
    ScoreTab: TTabSheet;
    PPMTab: TTabSheet;
    TimeList: TListView;
    ScoreList: TListView;
    PPMList: TListView;
    CloseBtn: TButton;
    ClearBtn: TButton;
    procedure CompareDesc(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure CompareAsc(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure CloseBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    FLevelName: String;
    procedure Parse(JournalEntry: String);
    procedure ReadOutJournal;
    procedure ClearLists;
  public
    procedure Execute(LevelName: String);
  end;

var
  HighScoreForm: THighScoreForm;

implementation

{$R *.dfm}

uses
  Functions, Constants;

procedure THighScoreForm.CompareDesc(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  n1, n2: integer;
begin
  if item1.SubItems.Count < 1 then exit;
  if item2.SubItems.Count < 1 then exit;

  n1 := StrToIntDef(Item1.SubItems[1], 0);
  n2 := StrToIntDef(Item2.SubItems[1], 0);

  if (n1 = n2)
    then Compare := 0 else
  if (n1 > n2)
    then Compare := 1
    else Compare := -1;
end;

procedure THighScoreForm.Parse(JournalEntry: String);
var
  res: TStringList;
  Date, Name: String;
  Score, Seconds, Removed, Total: Integer;
  Remaining, PPM: integer;
begin
  res := TStringList.Create;
  try
    Explode(JNL_SEP, JournalEntry, res);

    // VARIABLES

    Date := res.Strings[0];
    Name := res.Strings[1];
    Score := StrToIntDef(res.Strings[2], 0);
    Seconds := StrToIntDef(res.Strings[3], 0);
    Removed := StrToIntDef(res.Strings[4], 0);
    Total := StrToIntDef(res.Strings[5], 0);

    Remaining := Total - Removed;
    PPM := Round(Score / Seconds * 60);

    // CATEGORY A - LOWEST REMAINING STONES

    RemainingList.SortType := stNone;

    with RemainingList.Items.Add do
    begin
      Caption := Name;
      SubItems.Add(Date);
      SubItems.Add(IntToStr(Remaining));
    end;

    RemainingList.SortType := stText;

    // CATEGORY B - HIGHEST SCORE

    ScoreList.SortType := stNone;

    with ScoreList.Items.Add do
    begin
      Caption := Name;
      SubItems.Add(Date);
      SubItems.Add(IntToStr(Score));
    end;

    ScoreList.SortType := stText;

    // CATEGORY C - LOWEST TIME

    TimeList.SortType := stNone;

    with TimeList.Items.Add do
    begin
      Caption := Name;
      SubItems.Add(Date);
      SubItems.Add(IntToStr(Seconds));
    end;

    TimeList.SortType := stText;

    // CATEGORY D - HIGHEST POINTS PER MINUTE

    PPMList.SortType := stNone;

    with PPMList.Items.Add do
    begin
      Caption := Name;
      SubItems.Add(Date);
      SubItems.Add(IntToStr(PPM));
    end;

    PPMList.SortType := stText;
  finally
    FreeAndNil(res);
  end;
end;

procedure THighScoreForm.ReadOutJournal;
var
  f: textfile;
  tmp: string;
begin
  ClearLists;

  tmp := Format(JNL_FILE, [FLevelName]);
  if FileExists(tmp) then
  begin
    AssignFile(f, tmp);
    Reset(f);
    while not eof(f) do
    begin
      ReadLn(f, tmp);
      Parse(tmp);
    end;
    CloseFile(f);
  end;
end;

procedure THighScoreForm.Execute(LevelName: String);
begin
  FLevelName := LevelName;

  ReadOutJournal;
  ShowModal;
end;

procedure THighScoreForm.CompareAsc(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  n1, n2: integer;
begin
  if item1.SubItems.Count < 1 then exit;
  if item2.SubItems.Count < 1 then exit;

  n1 := StrToIntDef(Item1.SubItems[1], 0);
  n2 := StrToIntDef(Item2.SubItems[1], 0);

  if (n1 = n2)
    then Compare := 0 else
  if (n1 > n2)
    then Compare := -1
    else Compare := 1;
end;

procedure THighScoreForm.CloseBtnClick(Sender: TObject);
begin
  Close();
end;

procedure THighScoreForm.ClearBtnClick(Sender: TObject);
resourcestring
  LNG_ARE_YOU_SURE = 'Are you really sure you want to clear the high score list?';
begin
  if MessageDlg(LNG_ARE_YOU_SURE, mtConfirmation, mbYesNoCancel, 0) = mrYes then
  begin
    DeleteFile(Format(JNL_FILE, [FLevelName]));
    ClearLists;
  end;
end;

procedure THighScoreForm.ClearLists;
begin
  RemainingList.Clear;
  ScoreList.Clear;
  TimeList.Clear;
  PPMList.Clear;
end;

end.
