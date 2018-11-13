unit History;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  THistoryForm = class(TForm)
    JumpMemo: TMemo;
    SaveBtn: TButton;
    CloseBtn: TButton;
    JumpSaveDialog: TSaveDialog;
    procedure CloseBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  end;

var
  HistoryForm: THistoryForm;

implementation

{$R *.dfm}

uses
  Constants;

procedure THistoryForm.CloseBtnClick(Sender: TObject);
begin
  Close();
end;

procedure THistoryForm.SaveBtnClick(Sender: TObject);
resourcestring
  LNG_SAVED = 'History successfully saved!';
begin
  if JumpSaveDialog.Execute then
  begin
    JumpMemo.Lines.SaveToFile(JumpSaveDialog.FileName);
    MessageDlg(LNG_SAVED, mtInformation, [mbOk], 0);
  end;
end;

end.
