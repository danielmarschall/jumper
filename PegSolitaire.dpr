program PegSolitaire;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  About in 'About.pas' {AboutBox},
  Finish in 'Finish.pas' {FinishForm},
  Choice in 'Choice.pas' {LevelChoice},
  LevelFunctions in 'LevelFunctions.pas',
  Functions in 'Functions.pas',
  History in 'History.pas' {HistoryForm},
  HighScore in 'HighScore.pas' {HighScoreForm},
  Help in 'Help.pas' {HelpForm},
  Constants in 'Constants.pas';

{$R *.res}
{$R ExtraResources.res}

begin
  Application.Initialize;
  Application.Title := 'ViaThinkSoft Peg Solitaire';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TFinishForm, FinishForm);
  Application.CreateForm(TLevelChoice, LevelChoice);
  Application.CreateForm(THistoryForm, HistoryForm);
  Application.CreateForm(THighScoreForm, HighScoreForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.
