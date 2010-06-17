unit About;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellAPI;

type
  TAboutBox = class(TForm)
    AboutPanel: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    ProjectLeader: TLabel;
    CompanyHomepage: TLabel;
    LeaderHomepage: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure LinkClick(Sender: TObject);
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.LinkClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar('http://'+TLabel(Sender).Hint+'/'), '', '', SW_NORMAL);
end;

end.

