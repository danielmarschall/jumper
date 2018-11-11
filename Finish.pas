unit Finish;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MMSystem, Math, ExtCtrls, Registry, Main;

type
  TFinishForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    SaveBtn: TButton;
    CancelBtn: TButton;
    NameEdit: TEdit;
    PerformanceMemo: TMemo;
    Label3: TLabel;
    CupImage: TImage;
    ReplayCheckBox: TCheckBox;
    JumpHistoryLink: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure JumpHistoryLinkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLevel: String;
    FScore: integer;
    FStonesTotal: integer;
    FStonesRemoved: integer;
    FSeconds: integer;
    FHistory: TStringList;
    FGoalStatus: TGoalStatus;
    procedure SaveToJournal(PlayerName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer);
    procedure LoadSettings;
  public
    procedure SaveSettings;
    function Execute(LevelName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer; GoalStatus: TGoalStatus; JumpHistory: TStringList): Integer;
  end;

var
  FinishForm: TFinishForm;

implementation

uses
  History, Constants, LevelFunctions;

{$R *.dfm}

procedure TFinishForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

function TFinishForm.Execute(LevelName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer; GoalStatus: TGoalStatus; JumpHistory: TStringList): Integer;
var
  ExtraPoints: Integer;
begin
  FLevel := LevelName;
  FScore := Score;
  FStonesTotal := StonesTotal;
  FStonesRemoved := StonesRemoved;
  FSeconds := Seconds;
  FGoalStatus := GoalStatus;
  FHistory := JumpHistory;

  PerformanceMemo.Lines.Clear;
  PerformanceMemo.Lines.Add('');
  PerformanceMemo.Lines.Add(Format(LNG_REMAINING, [FStonesTotal - 1 - FStonesRemoved,
                            RoundTo(((FStonesTotal - 1 - FStonesRemoved) / FStonesTotal * 100), -2)]));
  PerformanceMemo.Lines.Add(Format(LNG_TIME_SECONDS, [FSeconds]));
  PerformanceMemo.Lines.Add(Format(LNG_POINTS_PER_MINUTE, [Round(FScore / FSeconds * 60)]));

  if FGoalStatus = gsLastStoneInGoalRed then
  begin
    ExtraPoints := FieldTypeWorth(ftRed) * 100;
    PerformanceMemo.Lines.Add(Format(LNG_GOAL_RED, [ExtraPoints]))
  end
  else if FGoalStatus = gsLastStoneInGoalYellow then
  begin
    ExtraPoints := FieldTypeWorth(ftYellow) * 100;
    PerformanceMemo.Lines.Add(Format(LNG_GOAL_YELLOW, [ExtraPoints]))
  end
  else if FGoalStatus = gsLastStoneInGoalGreen then
  begin
    ExtraPoints := FieldTypeWorth(ftGreen) * 100;
    PerformanceMemo.Lines.Add(Format(LNG_GOAL_GREEN, [ExtraPoints]))
  end
  else if FGoalStatus = gsLastStoneOutsideGoal then
  begin
    ExtraPoints := 0;
    PerformanceMemo.Lines.Add(Format(LNG_GOAL_MISSED, [ExtraPoints]))
  end;

  Inc(FScore, ExtraPoints);
  PerformanceMemo.Lines.Strings[0] := Format(LNG_SCORE, [FScore]);

  result := ShowModal;
end;

procedure TFinishForm.SaveToJournal(PlayerName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer);
var
  f: textfile;
  tmp: string;
begin
  tmp := Format(JNL_FILE, [FLevel]);

  AssignFile(f, tmp);
  if FileExists(tmp) then
    Append(f)
  else
    ReWrite(f);

  // TODO: Maybe we should do much more details, like, how many green stones were removed, how many yellows etc., and which stone was in the goal?
  WriteLn(f, Format(JNL_ENTRY, [DateTimeToStr(now()), NameEdit.Text, FScore, FSeconds, FStonesRemoved, FStonesTotal-1]));

  CloseFile(f);
end;

procedure TFinishForm.SaveSettings;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(REG_KEY, true) then
    begin
      reg.WriteString(REG_PLAYERNAME, NameEdit.Text);
      reg.WriteBool(REG_REPLAY, ReplayCheckbox.Checked);
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TFinishForm.LoadSettings;
var
  reg: TRegistry;
begin
  NameEdit.Text := '';
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(REG_KEY) then
    begin
      if reg.ValueExists(REG_PLAYERNAME) then
        NameEdit.Text := reg.ReadString(REG_PLAYERNAME);
      if reg.ValueExists(REG_REPLAY) then
        ReplayCheckBox.Checked := reg.ReadBool(REG_REPLAY);
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TFinishForm.SaveBtnClick(Sender: TObject);
begin
  if NameEdit.Text = '' then
  begin
    showmessage(LNG_ENTER_NAME);
    NameEdit.SetFocus;
    Exit;
  end;

  SaveToJournal(NameEdit.Text, FScore, FStonesTotal, FStonesRemoved, FSeconds);
  ModalResult := mrOK;
end;

procedure TFinishForm.JumpHistoryLinkClick(Sender: TObject);
begin
  HistoryForm.JumpMemo.Lines.Assign(FHistory);
  HistoryForm.ShowModal();
end;

procedure TFinishForm.FormCreate(Sender: TObject);
begin
  if not ForceDirectories(ExtractFilePath(Application.ExeName) + JNL_PATH) then
  begin
    ShowMessage(Format(LNG_COULD_NOT_CREATE_DIR, [JNL_PATH]));
  end;

  LoadSettings;
end;

end.
