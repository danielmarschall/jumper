unit Help;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  THelpForm = class(TForm)
    HelpMemo: TMemo;
    CloseBtn: TButton;
    procedure CloseBtnClick(Sender: TObject);
  end;

var
  HelpForm: THelpForm;

implementation

{$R *.dfm}

procedure THelpForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.
