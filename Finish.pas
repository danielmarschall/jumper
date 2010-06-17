unit Finish;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MMSystem, Math, ExtCtrls, Registry;

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
    procedure Calculate;
    procedure SaveToJournal(PlayerName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer);
    procedure LoadSettings;
  public
    procedure SaveSettings;
    function Execute(LevelName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer; JumpHistory: TStringList): Integer;
  end;

var
  FinishForm: TFinishForm;

implementation

uses
  History, Constants;

{$R *.dfm}

procedure TFinishForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFinishForm.Calculate;
begin
  PerformanceMemo.Lines.Clear;
  PerformanceMemo.Lines.Add(Format(LNG_SCORE, [FScore]));
  PerformanceMemo.Lines.Add(Format(LNG_REMAINING, [FStonesTotal - FStonesRemoved,
   RoundTo(((FStonesTotal - FStonesRemoved) / FStonesTotal * 100), -2)]));
  PerformanceMemo.Lines.Add(Format(LNG_TIME_SECONDS, [FSeconds]));
  PerformanceMemo.Lines.Add(Format(LNG_POINTS_PER_MINUTE, [Round(FScore / FSeconds * 60)]));
end;

function TFinishForm.Execute(LevelName: String; Score, StonesTotal, StonesRemoved, Seconds: Integer; JumpHistory: TStringList): Integer;
begin
  FLevel := LevelName;
  FScore := Score;
  FStonesTotal := StonesTotal;
  FStonesRemoved := StonesRemoved;
  FSeconds := Seconds;
  FHistory := JumpHistory;

  Calculate;
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
  WriteLn(f, Format(JNL_ENTRY, [DateTimeToStr(now()), NameEdit.Text, FScore, FSeconds, FStonesRemoved, FStonesTotal]));
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
  end
  else
  begin
    SaveToJournal(NameEdit.Text, FScore, FStonesTotal, FStonesRemoved, FSeconds);
    Close;
  end;

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
